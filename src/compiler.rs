use crate::{
    event::{Event, EventFlags, EventType},
    function::{FunctionBody, FunctionDic},
    instruction::Instruction,
    operator::BinaryOperator,
};

use self::parser::{ErbParser, Rule, PREC_CLIMBER};
use anyhow::{anyhow, bail, Result};
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
        const LEFT_ALIGN = 0x4;
        const RIGHT_ALIGN = 0x8;
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

struct Compiler<'s> {
    name: &'s str,
    out: Vec<Instruction>,
    marks: HashMap<String, u32>,
    goto_marks: HashMap<String, Vec<u32>>,
}

impl<'s> Compiler<'s> {
    pub fn new(name: &'s str) -> Self {
        Self {
            name,
            out: Vec::new(),
            marks: HashMap::new(),
            goto_marks: HashMap::new(),
        }
    }

    pub fn finish(mut self) -> Result<Vec<Instruction>> {
        for (label, marks) in self.goto_marks {
            match self.marks.get(&label) {
                Some(label_pos) => {
                    for goto_pos in marks {
                        self.out[goto_pos as usize] = Instruction::Goto(*label_pos);
                    }
                }
                None => bail!("Unknown goto label ${}", label),
            }
        }

        Ok(self.out)
    }

    fn get_var(&mut self, p: Pair<Rule>) -> Result<()> {
        let mut pairs = p.into_inner();
        let var = pairs.next().unwrap().as_str();

        self.push_args(pairs)?;

        match var {
            // local variables
            "ARG" | "ARGS" | "LOCAL" | "LOCALS" => {
                self.push_str(format!("{}@{}", var, self.name))?
            }
            _ => self.push_str(var)?,
        }

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
        use std::cell::UnsafeCell;

        match p.as_rule() {
            Rule::binop_expr => {
                let s = UnsafeCell::new(self);

                PREC_CLIMBER.climb(
                    p.into_inner(),
                    |p| unsafe { s.get().as_mut().unwrap().push_expr(p) },
                    |lhs, op, rhs| {
                        lhs?;
                        rhs?;

                        unsafe {
                            s.get()
                                .as_mut()
                                .unwrap()
                                .out
                                .push(Instruction::BinaryOperator(to_binop(op)?))
                        };

                        Ok(())
                    },
                )?;

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
                self.out.push(Instruction::Call);
                Ok(())
            }
            // Rule::print_expr | Rule::print_term => {
            //     // These rules should be slient but there is no slient non-atomic rule in pest
            //     // so we have to unwrap inner expr
            //     self.push_expr(p.into_inner().next().unwrap())
            // }
            Rule::print_form_var_text => {
                let mut pairs = p.into_inner();
                self.push_expr(pairs.next().unwrap())?;

                if let Some(align) = pairs.next() {
                    self.push_expr(align)?;
                    self.out.push(Instruction::AlignString);
                }

                Ok(())
            }
            Rule::conditionalop_expr | Rule::print_form_cond_text => {
                let mut pairs = p.into_inner();
                let cond = pairs.next().unwrap();
                let if_true = pairs.next().unwrap();
                let or_false = pairs.next().unwrap();
                self.push_expr(cond)?;
                let begin = self.mark();
                self.push_expr(if_true)?;
                let true_end = self.mark();
                self.insert(begin, Instruction::GotoIfNot(true_end + 1))?;
                self.push_expr(or_false)?;
                self.insert(true_end, Instruction::Goto(self.current_no()))?;
                Ok(())
            }
            Rule::print_form_cond_inner_text_first
            | Rule::print_form_cond_inner_text_second
            | Rule::print_form_text
            | Rule::print_form_text_space
            | Rule::formstring_expr => self.push_formtext(p),
            Rule::assign_fallback => {
                self.push_str(p.as_str())
            }
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
                let ty = pairs.next().unwrap().as_str();
                let begin = ty
                    .parse()
                    .map_err(|_| anyhow!("Unknown begin type {}", ty))?;
                self.out.push(Instruction::Begin(begin));
            }
            Rule::return_com => {
                self.push_list_begin();
                for expr in pairs {
                    self.push_expr(expr)?;
                }
                self.push_list_end();
                self.out.push(Instruction::Return);
            }
            Rule::returnf_com => {
                self.push_expr(pairs.next().unwrap())?;
                self.out.push(Instruction::ReturnF);
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
                let flags = parse_print_attributes(pairs.next().unwrap());

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
            Rule::times_com => {
                let var = pairs.next().unwrap();
                let times = pairs.next().unwrap();

                self.push_expr(var.clone())?;
                self.out.push(Instruction::Times(
                    ordered_float::NotNan::new(
                        times
                            .as_str()
                            .parse()
                            .map_err(|_| anyhow!("Literal is not float number"))?,
                    )
                    .unwrap(),
                ));
                self.store_var(var)?;
            }
            Rule::assign_line => {
                let var = pairs.next().unwrap();
                let mut rhs = pairs.next().unwrap();

                if rhs.as_rule() == Rule::assign_addop {
                    let op = rhs.into_inner().next().unwrap();
                    rhs = pairs.next().unwrap();
                    self.push_expr(var.clone())?;
                    self.push_expr(rhs)?;
                    self.out.push(Instruction::BinaryOperator(to_binop(op)?));
                } else {
                    self.push_expr(rhs)?;
                }

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
                let label = pairs.next().unwrap().as_str();
                let mark = self.mark();
                self.goto_marks.entry(label.into()).or_default().push(mark);
            }
            Rule::goto_label => {
                let label = pairs.next().unwrap().as_str();
                self.marks.insert(label.into(), self.current_no());
            }
            Rule::other_com => {
                let name = pairs.next().unwrap().as_str();
                self.push_args(pairs)?;
                self.push_str(name)?;

                self.out.push(Instruction::Command);
            }
            Rule::customdrawline_com => {
                self.push_str(pairs.next().unwrap().as_str())?;
                self.push_str("CUSTOMDRAWLINE")?;
                self.out.push(Instruction::Command);
            }
            Rule::callform_com => {
                let _header = pairs.next().unwrap();
                self.push_expr(pairs.next().unwrap())?;
                self.out.push(Instruction::Call);
            }
            _ => unreachable!("{:?}", rule),
        };

        Ok(())
    }
}

fn parse_print_attributes(p: Pair<Rule>) -> PrintFlags {
    let mut ret = PrintFlags::empty();

    debug_assert_eq!(p.as_rule(), Rule::print_attributes);

    for p in p.into_inner() {
        let flag = match p.as_str() {
            "l" | "L" => PrintFlags::NEWLINE,
            "w" | "W" => PrintFlags::NEWLINE | PrintFlags::WAIT,
            "c" | "C" => PrintFlags::RIGHT_ALIGN,
            "lc" | "LC" => PrintFlags::LEFT_ALIGN,
            _ => unreachable!(),
        };

        ret.insert(flag);
    }

    ret
}

fn parse_function(p: Pair<Rule>, dic: &mut FunctionDic) -> Result<()> {
    let mut pairs = p.into_inner();

    let mut label_pair = pairs.next().unwrap().into_inner();
    let label = label_pair.next().unwrap().as_str();
    let header = pairs.next().unwrap().into_inner();

    let mut flags = EventFlags::None;
    let mut local_size = 1000;
    let mut locals_size = 100;

    for info in header {
        match info.as_rule() {
            Rule::function_event_info => match info.as_str() {
                "PRI" => flags = EventFlags::Pre,
                "LATER" => flags = EventFlags::Later,
                "SINGLE" => flags = EventFlags::Single,
                _ => bail!("Unknown event info {}", info.as_str()),
            },
            Rule::function_local_info => {
                local_size = info
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .parse::<usize>()?;
            }
            Rule::function_locals_info => {
                locals_size = info
                    .into_inner()
                    .next()
                    .unwrap()
                    .as_str()
                    .parse::<usize>()?;
            }
            Rule::function_ignored_info => {}
            _ => todo!("Other header"),
        }
    }

    let mut compiler = Compiler::new(label);

    compiler.push_block(pairs)?;

    let mut body = FunctionBody::new(local_size, locals_size, compiler.finish()?);

    if let Some(args) = label_pair.next().map(|p| p.into_inner()) {
        for arg in args {
            debug_assert_eq!(arg.as_rule(), Rule::var_expr);
            let mut pairs = arg.into_inner();
            let name = pairs.next().unwrap().as_str();

            let name = if matches!(name, "LOCAL" | "LOCALS" | "ARG" | "ARGS") {
                format!("{}@{}", name, label)
            } else {
                name.into()
            };
            let mut indices = ArrayVec::new();
            for arg_pair in pairs {
                indices.push(eval_pair(arg_pair)?);
            }
            body.push_arg(name, indices);
        }
    }

    match label.parse::<EventType>() {
        Ok(ty) => dic.insert_event(Event { flags, ty }, body),
        _ => dic.insert_func(label.into(), body),
    };

    Ok(())
}

fn eval_pair(p: Pair<Rule>) -> Result<usize> {
    match p.as_rule() {
        Rule::num => p.as_str().parse().map_err(Into::into),
        _ => bail!("Can't eval {} as number", p),
    }
}

pub fn compile(s: &str, dic: &mut FunctionDic) -> Result<()> {
    let r = ErbParser::parse(Rule::program, s)?;

    for p in r {
        if p.as_rule() == Rule::EOI {
            break;
        } else {
            debug_assert_eq!(p.as_rule(), Rule::function);
            parse_function(p, dic)?;
        }
    }

    Ok(())
}

fn to_binop(op: Pair<Rule>) -> Result<BinaryOperator> {
    Ok(match op.as_rule() {
        Rule::add => BinaryOperator::Add,
        Rule::sub => BinaryOperator::Sub,
        Rule::mul => BinaryOperator::Mul,
        Rule::div => BinaryOperator::Div,
        Rule::rem => BinaryOperator::Rem,
        Rule::eq => BinaryOperator::Equal,
        Rule::ne => BinaryOperator::NotEqual,
        Rule::gt => BinaryOperator::Greater,
        Rule::ge => BinaryOperator::GreaterOrEqual,
        Rule::lt => BinaryOperator::Less,
        Rule::le => BinaryOperator::LessOrEqual,
        Rule::bitor => BinaryOperator::BitOr,
        Rule::bitand => BinaryOperator::BitAnd,
        Rule::or => BinaryOperator::Or,
        Rule::and => BinaryOperator::And,
        Rule::xor => BinaryOperator::Xor,
        _ => bail!("Invalid op pair {:?}", op),
    })
}
