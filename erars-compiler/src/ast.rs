use crate::{BinaryOperator, EventFlags, EventType, PrintFlags, UnaryOperator};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Print(PrintFlags, String),
    PrintForm(PrintFlags, String, Vec<(Expr, String)>),
    Assign(Expr, Expr),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub header: FunctionHeader,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionHeader {
    pub name: String,
    pub infos: Vec<FunctionInfo>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionInfo {
    EventFlag(EventFlags),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    StringLit(String),
    IntLit(i64),
    Var(String, Vec<Self>),
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
        Self::Var(n.into(), args)
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
