use self::parser::{Rule, PREC_CLIMBER};
use anyhow::Result;
use itertools::Itertools;
use pest::{iterators::Pair, Parser};

mod parser;

bitflags::bitflags! {
    pub struct PrintFlags: u32 {
        const NEWLINE = 0x1;
        const WAIT = 0x2;
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Power,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Num(i64),
    Str(String),
    VarExpr {
        name: String,
        is_chara: bool,
        args: Vec<Self>,
    },
    BinExpr(Box<Self>, BinOp, Box<Self>),
}

#[derive(Clone, Debug)]
pub struct PrintFormText {
    pub first: String,
    pub pairs: Vec<(Expr, String)>,
}

impl From<String> for PrintFormText {
    fn from(s: String) -> Self {
        Self {
            first: s,
            pairs: Vec::new(),
        }
    }
}

impl<'a> From<&'a str> for PrintFormText {
    fn from(s: &'a str) -> Self {
        Self {
            first: s.into(),
            pairs: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ProgramLine {
    PrintCom {
        flags: PrintFlags,
        text: PrintFormText,
    },
    Call {
        func: String,
        args: Vec<Expr>,
    },
}

fn parse_expr(p: Pair<Rule>) -> Result<Expr> {
    PREC_CLIMBER.climb(
        p.into_inner(),
        |p| match p.as_rule() {
            Rule::string_inner => Ok(Expr::Str(p.as_str().into())),
            Rule::num => Ok(Expr::Num(p.as_str().parse()?)),
            Rule::var_expr => {
                let mut pairs = p.into_inner();
                let var = pairs.next().unwrap();
                let is_chara =
                    matches!(var.as_rule(), Rule::cvar_0d | Rule::cvar_1d | Rule::cvar_2d);
                let mut args = pairs.map(parse_expr).collect::<Result<Vec<_>>>()?;

                let expected_len = match var.as_rule() {
                    Rule::gvar_0d => 0,
                    Rule::cvar_0d | Rule::gvar_1d => 1,
                    Rule::cvar_1d | Rule::gvar_2d => 2,
                    Rule::cvar_2d | Rule::gvar_3d => 3,
                    _ => unreachable!(),
                };

                args.resize(expected_len, Expr::Num(0));

                Ok(Expr::VarExpr {
                    name: var.as_str().into(),
                    is_chara,
                    args,
                })
            }
            _ => unreachable!(),
        },
        |lhs, op, rhs| match op.as_rule() {
            Rule::add => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Add, Box::new(rhs?))),
            Rule::subtract => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Sub, Box::new(rhs?))),
            Rule::divide => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Div, Box::new(rhs?))),
            Rule::multiply => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Mul, Box::new(rhs?))),
            Rule::power => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Power, Box::new(rhs?))),
            _ => todo!(),
        },
    )
}

fn parse_line(p: Pair<Rule>) -> Result<ProgramLine> {
    let rule = p.as_rule();
    let mut pairs = p.into_inner();

    let line = match rule {
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

            let text = if text.as_rule() == Rule::print_text {
                text.as_str().into()
            } else {
                let mut pairs = text.into_inner();
                let first = pairs.next().unwrap().as_str().into();

                let pairs = pairs
                    .tuples()
                    .map(|(expr, text)| {
                        let expr = parse_expr(expr)?;
                        let text = text.as_str().into();
                        Ok((expr, text))
                    })
                    .collect::<Result<_>>()?;

                PrintFormText { first, pairs }
            };

            ProgramLine::PrintCom { text, flags }
        }
        Rule::call_com => {
            let func = pairs.next().unwrap().as_str().into();
            let args = pairs.map(parse_expr).collect::<Result<_>>()?;

            ProgramLine::Call { func, args }
        }
        _ => unreachable!("{:?}", rule),
    };

    Ok(line)
}

pub fn compile(s: &str) -> Result<Vec<ProgramLine>> {
    let r = self::parser::ErbParser::parse(self::parser::Rule::program, s).unwrap();

    r.map_while(|p| {
        if p.as_rule() == Rule::EOI {
            None
        } else {
            Some(parse_line(p))
        }
    })
    .collect()
}
