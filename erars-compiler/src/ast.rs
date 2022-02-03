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
