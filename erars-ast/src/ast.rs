use std::fmt;

use bitflags::bitflags;
use ordered_float::NotNan;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use strum::{Display, EnumString};

use crate::{
    Alignment, BinaryOperator, BuiltinCommand, EventFlags, LocalVariable, UnaryOperator, Variable,
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Stmt {
    Label(SmolStr),
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
    If(Vec<(Expr, Vec<Stmt>)>, Vec<Stmt>),
    Times(Variable, NotNan<f32>),
    Goto {
        label: Expr,
        catch: Option<Vec<Stmt>>,
    },
    Call {
        name: Expr,
        args: Vec<Expr>,
        is_jump: bool,
        catch: Option<Vec<Stmt>>,
    },
    Begin(BeginType),
    Varset(Variable, Vec<Expr>),
    Repeat(Expr, Vec<Stmt>),
    Do(Expr, Vec<Stmt>),
    While(Expr, Vec<Stmt>),
    For(Variable, Expr, Expr, Expr, Vec<Stmt>),
    Continue,
    Break,
    Command(BuiltinCommand, Vec<Expr>),
    Return(Vec<Expr>),
    ReturnF(Expr),
    Alignment(Alignment),
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub header: FunctionHeader,
    pub body: Vec<Stmt>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
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
    String(String),
    Int(i64),
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
        Self::Int(i.into())
    }

    pub fn str(s: impl Into<String>) -> Self {
        Self::String(s.into())
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

option_set::option_set! {
    pub struct PrintFlags: UpperSnake + u32 {
        const NEWLINE = 0x1;
        const WAIT = 0x2;
        const LEFT_ALIGN = 0x4;
        const RIGHT_ALIGN = 0x8;
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize, EnumString, Display)]
pub enum BeginType {
    #[strum(to_string = "TITLE")]
    Title,
    #[strum(to_string = "FIRST")]
    First,
    #[strum(to_string = "SHOP")]
    Shop,
    #[strum(to_string = "TURNEND")]
    TurnEnd,
    #[strum(to_string = "AFTERTRAIN")]
    AfterTrain,
}