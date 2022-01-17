use crate::{
    instruction::Instruction,
    operator::BinaryOperator,
    vm::{BulitinVariable, VariableInfo},
};

use self::parser::{ErbParser, Rule, PREC_CLIMBER};
use anyhow::{anyhow, Result};
use arrayvec::ArrayVec;
use hashbrown::HashMap;
use itertools::Itertools;
use pest::{
    iterators::{Pair, Pairs},
    Parser,
};
use serde::{Deserialize, Serialize};

mod parser;

bitflags::bitflags! {
    #[derive(Serialize, Deserialize)]
    pub struct PrintFlags: u32 {
        const NEWLINE = 0x1;
        const WAIT = 0x2;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

impl Default for Alignment {
    fn default() -> Self {
        Self::Left
    }
}

struct Compiler {
    label: HashMap<String, u32>,
    out: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            label: HashMap::new(),
            out: Vec::new(),
        }
    }

    fn get_var(&mut self, p: Pair<Rule>) -> Result<()> {
        let mut pairs = p.into_inner();
        let var = pairs.next().unwrap().as_str();
        self.push_args(pairs)?;
        self.push_str(var)?;

        Ok(())
    }

    fn store_var(&mut self, p: Pair<Rule>) -> Result<()> {
        self.get_var(p)?;
        self.out.push(Instruction::StoreVar);

        Ok(())
    }

    fn push_var(&mut self, p: Pair<Rule>) -> Result<()> {
        self.get_var(p)?;
        self.out.push(Instruction::LoadVar);

        Ok(())
    }

    fn push_int(&mut self, i: i64) -> Result<()> {
        self.out.push(Instruction::LoadInt(i));
        Ok(())
    }

    fn push_str(&mut self, s: impl Into<String>) -> Result<()> {
        self.out.push(Instruction::LoadStr(s.into()));
        Ok(())
    }

    fn push_expr(&mut self, p: Pair<Rule>) -> Result<()> {
        match p.as_rule() {
            Rule::binop_expr => {
                let f = PREC_CLIMBER.climb(
                    p.into_inner(),
                    |p| {
                        Box::new(move |this: &mut Compiler| this.push_expr(p))
                            as Box<dyn FnOnce(&mut Compiler) -> Result<()>>
                    },
                    |lhs, op, rhs| {
                        Box::new(move |this: &mut Compiler| {
                            lhs(this)?;
                            rhs(this)?;

                            let op = match op.as_rule() {
                                Rule::add => BinaryOperator::Add,
                                Rule::sub => BinaryOperator::Sub,
                                Rule::mul => BinaryOperator::Mul,
                                Rule::div => BinaryOperator::Div,
                                Rule::rem => BinaryOperator::Rem,
                                _ => todo!(),
                            };

                            this.out.push(Instruction::BinaryOperator(op));

                            Ok(())
                        })
                    },
                );

                f(self)?;

                Ok(())
            }
            Rule::string => self.push_str(p.into_inner().next().unwrap().as_str()),
            Rule::string_inner => self.push_str(p.as_str()),
            Rule::num => self.push_int(p.as_str().parse()?),
            Rule::var_expr => self.push_var(p),
            Rule::method_expr => {
                let mut pairs = p.into_inner();
                let name = pairs.next().unwrap().as_str();
                self.push_args(pairs)?;
                self.push_str(name)?;
                self.out.push(Instruction::CallMethod);
                Ok(())
            }
            Rule::formstring_expr => self.push_formtext(p),
            _ => unreachable!("{:?}", p),
        }
    }

    fn push_list_begin(&mut self) {
        self.out.push(Instruction::ListBegin);
    }

    fn push_list_end(&mut self) {
        self.out.push(Instruction::ListEnd);
    }

    fn push_args(&mut self, pairs: Pairs<Rule>) -> Result<()> {
        self.push_list_begin();

        for p in pairs {
            self.push_expr(p)?;
        }

        self.push_list_end();

        Ok(())
    }

    fn push_formtext(&mut self, mut p: Pair<Rule>) -> Result<()> {
        if p.as_rule() == Rule::formstring_expr {
            p = p.into_inner().next().unwrap();
        }

        let mut pairs = p.into_inner();

        self.push_list_begin();

        self.push_str(pairs.next().unwrap().as_str())?;

        for (expr, text) in pairs.tuples() {
            self.push_expr(expr)?;
            self.push_str(text.as_str())?;
        }

        self.push_list_end();

        self.out.push(Instruction::ConcatString);

        Ok(())
    }

    fn mark(&mut self) -> u32 {
        let ret = self.current_no();
        self.out.push(Instruction::Nop);
        ret
    }

    fn current_no(&self) -> u32 {
        self.out.len() as u32
    }

    fn insert(&mut self, mark: u32, inst: Instruction) -> Result<()> {
        *self
            .out
            .get_mut(mark as usize)
            .ok_or_else(|| anyhow!("Invalid mark {}", mark))? = inst;
        Ok(())
    }

    fn push_block(&mut self, pairs: Pairs<Rule>) -> Result<()> {
        for p in pairs {
            self.push_line(p)?;
        }

        Ok(())
    }

    fn push_if(&mut self, p: Pair<Rule>) -> Result<()> {
        let mut pairs = p.into_inner();

        // cond
        self.push_expr(pairs.next().unwrap())?;
        let begin = self.mark();

        // body
        self.push_block(pairs)?;
        self.insert(begin, Instruction::GotoIfNot(self.current_no() + 1))?;

        Ok(())
    }

    fn push_line(&mut self, p: Pair<Rule>) -> Result<()> {
        let rule = p.as_rule();
        let mut pairs = p.into_inner();

        match rule {
            Rule::begin_com => {
                let begin = pairs.next().unwrap().as_str().parse()?;
                self.out.push(Instruction::Begin(begin));
            }
            Rule::sif_com => {
                self.push_expr(pairs.next().unwrap())?;
                let begin = self.mark();
                self.push_block(pairs)?;
                self.insert(begin, Instruction::GotoIfNot(self.current_no()))?;
            }
            Rule::if_com => {
                let mut end_stack = ArrayVec::<u32, 24>::new();

                self.push_if(pairs.next().unwrap())?;
                end_stack.push(self.mark());

                for p in pairs {
                    match p.as_rule() {
                        Rule::elseif_part => {
                            self.push_if(p)?;
                            end_stack.push(self.mark());
                        }
                        Rule::else_part => {
                            self.push_block(p.into_inner())?;
                        }
                        _ => unreachable!(),
                    }
                }

                for end in end_stack {
                    self.insert(end, Instruction::Goto(self.current_no()))?;
                }
            }
            Rule::print_com | Rule::printform_com => {
                let mut flags = PrintFlags::empty();

                if pairs.peek().unwrap().as_rule() == Rule::print_flag {
                    flags = match pairs.next().unwrap().as_str() {
                        "l" | "L" => PrintFlags::NEWLINE,
                        "w" | "W" => PrintFlags::NEWLINE | PrintFlags::WAIT,
                        _ => unreachable!(),
                    };
                }

                let text = pairs.next().unwrap();

                if text.as_rule() == Rule::print_text {
                    self.push_str(text.as_str())?;
                } else {
                    self.push_formtext(text)?;
                };

                self.out.push(Instruction::Print(flags));
            }
            Rule::reuselastline_com => {
                self.push_str(pairs.next().unwrap().as_str())?;
                self.out.push(Instruction::ReuseLastLine);
            }
            Rule::assign_line => {
                let var = pairs.next().unwrap();
                let rhs = pairs.next().unwrap();

                self.push_expr(rhs)?;
                self.store_var(var)?;
            }
            Rule::alignment_com => {
                let alignment = pairs.next().unwrap().as_str();

                let alignment = match alignment {
                    "LEFT" => Alignment::Left,
                    "CENTER" => Alignment::Center,
                    "RIGHT" => Alignment::Right,
                    _ => unreachable!(),
                };

                self.out.push(Instruction::SetAlignment(alignment));
            }
            Rule::call_com => {
                let name = pairs.next().unwrap().as_str();
                self.push_args(pairs)?;
                self.push_str(name)?;

                self.out.push(Instruction::Call);
            }
            Rule::goto_com => {
                let mark = self
                    .label
                    .get(pairs.next().unwrap().as_str())
                    .ok_or_else(|| {
                        anyhow!("Unknown goto label ${}", pairs.next().unwrap().as_str())
                    })?;
                self.out.push(Instruction::Goto(*mark));
            }
            Rule::goto_label => {
                let mark = self.mark();
                self.label
                    .insert(pairs.next().unwrap().as_str().into(), mark);
            }
            Rule::other_com => {
                let name = pairs.next().unwrap().as_str();
                self.push_args(pairs)?;
                self.push_str(name)?;

                self.out.push(Instruction::Command);
            }
            _ => unreachable!("{:?}", rule),
        };

        Ok(())
    }
}

fn parse_function(p: Pair<Rule>) -> Result<(String, Vec<Instruction>)> {
    let mut pairs = p.into_inner();

    let label = pairs
        .next()
        .unwrap()
        .as_str()
        .trim_start_matches('@')
        .to_string();

    let mut compiler = Compiler::new();

    compiler.push_block(pairs)?;

    Ok((label, compiler.out))
}

pub fn compile(s: &str) -> Result<Vec<(String, Vec<Instruction>)>> {
    let r = ErbParser::parse(Rule::program, s).unwrap();

    r.map_while(|p| {
        if p.as_rule() == Rule::EOI {
            None
        } else {
            debug_assert_eq!(p.as_rule(), Rule::function);
            Some(parse_function(p))
        }
    })
    .collect()
}
