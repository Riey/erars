use crate::vm::VariableInfo;

use self::parser::{ErbParser, Rule, PREC_CLIMBER};
use anyhow::{anyhow, Result};
use hashbrown::HashMap;
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
    Rem,
}

#[derive(Clone, Debug)]
pub struct VarExpr {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Num(i64),
    Str(String),
    FormText(PrintFormText),
    Method { name: String, args: Vec<Self> },
    VarExpr(VarExpr),
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
    Assign {
        var: VarExpr,
        expr: Expr,
    },
}

fn parse_var_expr(p: Pair<Rule>, infos: &HashMap<String, VariableInfo>) -> Result<VarExpr> {
    let mut pairs = p.into_inner();
    let var = pairs.next().unwrap();
    let info = infos
        .get(var.as_str())
        .ok_or_else(|| anyhow!("Unknown variable {}", var.as_str()))?;
    let mut args = pairs
        .map(|p| parse_expr(p, infos))
        .collect::<Result<Vec<_>>>()?;
    args.resize(info.arg_len(), Expr::Num(0));
    Ok(VarExpr {
        name: var.as_str().into(),
        args,
    })
}

fn parse_expr(p: Pair<Rule>, infos: &HashMap<String, VariableInfo>) -> Result<Expr> {
    match p.as_rule() {
        Rule::binop_expr => PREC_CLIMBER.climb(
            p.into_inner(),
            |p| parse_expr(p, infos),
            |lhs, op, rhs| match op.as_rule() {
                Rule::add => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Add, Box::new(rhs?))),
                Rule::sub => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Sub, Box::new(rhs?))),
                Rule::div => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Div, Box::new(rhs?))),
                Rule::mul => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Mul, Box::new(rhs?))),
                Rule::rem => Ok(Expr::BinExpr(Box::new(lhs?), BinOp::Rem, Box::new(rhs?))),
                _ => todo!(),
            },
        ),
        Rule::string => Ok(Expr::Str(p.into_inner().next().unwrap().as_str().into())),
        Rule::string_inner => Ok(Expr::Str(p.as_str().into())),
        Rule::num => Ok(Expr::Num(p.as_str().parse()?)),
        Rule::var_expr => parse_var_expr(p, infos).map(Expr::VarExpr),
        Rule::method_expr => {
            let mut pairs = p.into_inner();
            let name = pairs.next().unwrap().as_str().into();
            let args = pairs.map(|p| parse_expr(p, infos)).collect::<Result<_>>()?;
            Ok(Expr::Method { name, args })
        }
        Rule::formstring_expr => {
            let formtext = parse_formtext(p.into_inner().next().unwrap(), infos)?;
            Ok(Expr::FormText(formtext))
        }
        _ => unreachable!("{:?}", p),
    }
}

fn parse_formtext(p: Pair<Rule>, infos: &HashMap<String, VariableInfo>) -> Result<PrintFormText> {
    let mut pairs = p.into_inner();
    let first = pairs.next().unwrap().as_str().into();

    let pairs = pairs
        .tuples()
        .map(|(expr, text)| {
            let expr = parse_expr(expr, infos)?;
            let text = text.as_str().into();
            Ok((expr, text))
        })
        .collect::<Result<_>>()?;

    Ok(PrintFormText { first, pairs })
}

fn parse_line(p: Pair<Rule>, infos: &HashMap<String, VariableInfo>) -> Result<ProgramLine> {
    let rule = p.as_rule();
    let mut pairs = p.into_inner();

    let line = match dbg!(rule) {
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
                parse_formtext(text, infos)?
            };

            ProgramLine::PrintCom { text, flags }
        }
        Rule::assign_line => {
            let var = pairs.next().unwrap();
            let rhs = pairs.next().unwrap();

            ProgramLine::Assign {
                var: parse_var_expr(var, infos)?,
                expr: parse_expr(rhs, infos)?,
            }
        }
        _ => unreachable!("{:?}", rule),
    };

    Ok(line)
}

fn parse_function(
    p: Pair<Rule>,
    infos: &HashMap<String, VariableInfo>,
) -> Result<(String, Vec<ProgramLine>)> {
    let mut pairs = p.into_inner();

    let label = pairs.next().unwrap().as_str().to_string();

    Ok((
        label,
        pairs.map(|p| parse_line(p, infos)).collect::<Result<_>>()?,
    ))
}

pub fn compile(
    s: &str,
    infos: &HashMap<String, VariableInfo>,
) -> Result<Vec<(String, Vec<ProgramLine>)>> {
    let r = ErbParser::parse(Rule::program, s).unwrap();

    r.map_while(|p| {
        if p.as_rule() == Rule::EOI {
            None
        } else {
            debug_assert_eq!(p.as_rule(), Rule::function);
            Some(parse_function(p, infos))
        }
    })
    .collect()
}
