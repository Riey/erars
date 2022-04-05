use std::fmt;

use ordered_float::NotNan;
use serde::{Deserialize, Serialize};
use smartstring::{LazyCompact, SmartString};

use crate::{
    Alignment, BeginType, BinaryOperator, BuiltinCommand, EventFlags, PrintFlags, UnaryOperator,
    VariableIndex, VariableInfo,
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Stmt {
    Label(SmartString<LazyCompact>),
    SelectCase(
        Expr,
        Vec<(Vec<SelectCaseCond>, Vec<Stmt>)>,
        Option<Vec<Stmt>>,
    ),
    Print(PrintFlags, Expr),
    PrintList(PrintFlags, Vec<Expr>),
    PrintForm(PrintFlags, FormText),
    ReuseLastLine(String),
    Assign(Variable, Option<BinaryOperator>, Expr),
    Sif(Expr, Box<Stmt>),
    If(Vec<(Expr, Vec<Stmt>)>, Option<Vec<Stmt>>),
    Times(Variable, NotNan<f32>),
    Goto {
        label: Expr,
        catch: Option<Vec<Stmt>>,
    },
    Call {
        name: Expr,
        args: Vec<Expr>,
        jump: bool,
        catch: Option<Vec<Stmt>>,
    },
    Begin(BeginType),
    Varset(Variable, Vec<Expr>),
    Repeat(Expr, Vec<Stmt>),
    Do(Expr, Vec<Stmt>),
    For(Variable, Expr, Expr, Expr, Vec<Stmt>),
    Continue,
    Break,
    Command(BuiltinCommand, Vec<Expr>),
    Return(Vec<Expr>),
    ReturnF(Expr),
    Alignment(Alignment),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LocalVariable {
    pub idx: VariableIndex,
    pub init: Vec<Expr>,
    pub info: VariableInfo,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub header: FunctionHeader,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionHeader {
    pub name: String,
    pub args: Vec<(Variable, Option<Expr>)>,
    pub infos: Vec<FunctionInfo>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionInfo {
    EventFlag(EventFlags),
    LocalSize(usize),
    LocalSSize(usize),
    Dim(LocalVariable),
    Function,
    FunctionS,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Expr {
    StringLit(String),
    IntLit(i64),
    FormText(FormText),
    Var(Variable),
    Method(String, Vec<Self>),
    UnaryopExpr(Box<Self>, UnaryOperator),
    /// ++/-- var ++/--
    IncOpExpr {
        var: Variable,
        is_pre: bool,
        is_inc: bool,
    },
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

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Variable {
    pub var_idx: VariableIndex,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum SelectCaseCond {
    Single(Expr),
    To(Expr, Expr),
    Is(BinaryOperator, Expr),
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FormExpr {
    pub expr: Expr,
    pub padding: Option<Expr>,
    pub align: Option<Alignment>,
}

impl fmt::Debug for FormExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.expr)
    }
}

#[derive(Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct FormText {
    pub first: String,
    pub other: Vec<(FormExpr, String)>,
}

impl FormText {
    pub fn new(first: String) -> Self {
        Self {
            first,
            other: Vec::new(),
        }
    }

    pub fn push(
        &mut self,
        expr: Expr,
        padding: Option<Expr>,
        align: Option<Alignment>,
        text: String,
    ) {
        self.other.push((
            FormExpr {
                expr,
                padding,
                align,
            },
            text,
        ));
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
