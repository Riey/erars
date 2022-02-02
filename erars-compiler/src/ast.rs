use crate::{PrintFlags, BinaryOperator};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Print(PrintFlags, String),
    PrintForm(PrintFlags, String, Vec<(Expr, String)>),
}


#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    StringLit(String),
    IntLit(i64),
    BinopExpr(Box<Self>, BinaryOperator, Box<Self>),
}
