use std::fmt;

use serde::{Deserialize, Serialize};

use crate::{BinaryOperator, EventFlags, PrintFlags, UnaryOperator};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Variable {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Stmt {
    Print(PrintFlags, String),
    PrintForm(PrintFlags, FormText),
    Assign(Variable, Option<BinaryOperator>, Expr),
    Sif(Expr, Box<Stmt>),
    If(Vec<(Expr, Vec<Stmt>)>, Option<Vec<Stmt>>),
    Call(String, Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub header: FunctionHeader,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionHeader {
    pub name: String,
    pub infos: Vec<FunctionInfo>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionInfo {
    EventFlag(EventFlags),
}

#[derive(Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct FormText {
    pub first: String,
    pub other: Vec<(Expr, String)>,
}

impl FormText {
    pub fn new(first: String) -> Self {
        Self {
            first,
            other: Vec::new(),
        }
    }

    pub fn push(&mut self, expr: Expr, text: String) {
        self.other.push((expr, text));
    }
}

impl fmt::Debug for FormText {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.first)?;

        for (expr, text) in self.other.iter() {
            write!(f, "{{{:?}}}{}", expr, text)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Expr {
    StringLit(String),
    IntLit(i64),
    FormText(FormText),
    Var(Variable),
    Method(String, Vec<Self>),
    UnaryopExpr(Box<Self>, UnaryOperator),
    BinopExpr(Box<Self>, BinaryOperator, Box<Self>),
    CondExpr(Box<Self>, Box<Self>, Box<Self>),
}

impl Expr {
    pub fn int(i: impl Into<i64>) -> Self {
        Self::IntLit(i.into())
    }

    pub fn str(s: impl Into<String>) -> Self {
        Self::StringLit(s.into())
    }

    pub fn var(n: impl Into<String>, args: Vec<Self>) -> Self {
        Self::Var(Variable {
            name: n.into(),
            args,
        })
    }

    pub fn unary(op1: Self, op: UnaryOperator) -> Self {
        Self::UnaryopExpr(Box::new(op1), op)
    }

    pub fn binary(op1: Self, op: BinaryOperator, op2: Self) -> Self {
        Self::BinopExpr(Box::new(op1), op, Box::new(op2))
    }

    pub fn cond(op1: Self, op2: Self, op3: Self) -> Self {
        Self::CondExpr(Box::new(op1), Box::new(op2), Box::new(op3))
    }
}
