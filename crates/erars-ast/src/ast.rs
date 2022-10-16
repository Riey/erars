use std::fmt;

use bitflags::bitflags;
use ordered_float::NotNan;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumString};

use crate::{
    command::BuiltinMethod, Alignment, BinaryOperator, BuiltinCommand, BuiltinVariable, EventFlags,
    EventType, InlineValue, Interner, LocalVariable, StrKey, UnaryOperator, Variable,
};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Stmt {
    Label(StrKey),
    SelectCase(
        Expr,
        Vec<(Vec<SelectCaseCond>, Vec<StmtWithPos>)>,
        Option<Vec<StmtWithPos>>,
    ),
    Print(PrintFlags, Expr),
    PrintList(PrintFlags, Vec<Expr>),
    PrintFormS(PrintFlags, Expr),
    PrintData(PrintFlags, Option<Expr>, Vec<Vec<Expr>>),
    ReuseLastLine(StrKey),
    Assign(Variable, Option<BinaryOperator>, Expr),
    Sif(Expr, Box<StmtWithPos>),
    If(Vec<(Expr, Vec<StmtWithPos>)>, Vec<StmtWithPos>),
    Times(Variable, NotNan<f32>),
    Goto {
        label: Expr,
        catch_body: Option<Vec<StmtWithPos>>,
    },
    Call {
        name: Expr,
        args: Vec<Expr>,
        is_jump: bool,
        is_method: bool,

        try_body: Vec<StmtWithPos>,
        catch_body: Option<Vec<StmtWithPos>>,
    },
    CallEvent(EventType),
    Begin(BeginType),
    Repeat(Expr, Vec<StmtWithPos>),
    Do(Expr, Vec<StmtWithPos>),
    While(Expr, Vec<StmtWithPos>),
    For(Variable, Box<(Expr, Expr, Expr)>, Vec<StmtWithPos>),
    Continue,
    Break,
    Command(BuiltinCommand, Vec<Expr>),
    Method(BuiltinMethod, Vec<Expr>),
    Alignment(Alignment),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StmtWithPos(pub Stmt, pub ScriptPosition);

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ScriptPosition {
    /// 0 based line index
    pub line: u32,
}

impl fmt::Display for ScriptPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.line + 1)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Function {
    pub header: FunctionHeader,
    pub body: Vec<StmtWithPos>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct FunctionHeader {
    pub file_path: StrKey,
    pub name: StrKey,
    pub args: Vec<(Variable, Option<InlineValue>)>,
    pub infos: Vec<FunctionInfo>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum FunctionInfo {
    EventFlag(EventFlags),
    LocalSize(u32),
    LocalSSize(u32),
    Dim(LocalVariable),
    Function,
    FunctionS,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Expr {
    String(StrKey),
    Int(i64),
    FormText(FormText),
    Var(Variable),
    BuiltinVar(BuiltinVariable, Vec<Self>),
    Method(StrKey, Vec<Self>),
    BuiltinMethod(BuiltinMethod, Vec<Self>),
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

    pub fn str(interner: &Interner, s: impl AsRef<str>) -> Self {
        Self::String(interner.get_or_intern(s))
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

    pub fn into_var(self) -> Option<Variable> {
        match self {
            Self::Var(var) => Some(var),
            _ => None,
        }
    }

    pub fn into_const_int(self) -> Option<i64> {
        match self {
            Self::Int(i) => Some(i),
            _ => None,
        }
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
    pub first: StrKey,
    pub other: Vec<(FormExpr, StrKey)>,
}

impl FormText {
    pub fn new(first: StrKey) -> Self {
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
        text: StrKey,
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
        write!(f, "{:?}", self.first)?;

        for (expr, text) in self.other.iter() {
            write!(f, "{{{:?}}}{:?}", expr, text)?;
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
        const SINGLE = 0x10;
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, EnumString, Display, Serialize, Deserialize)]
pub enum BeginType {
    #[strum(to_string = "TITLE")]
    Title,
    #[strum(to_string = "TRAIN")]
    Train,
    #[strum(to_string = "FIRST")]
    First,
    #[strum(to_string = "SHOP")]
    Shop,
    #[strum(to_string = "TURNEND")]
    TurnEnd,
    #[strum(to_string = "AFTERTRAIN")]
    AfterTrain,
    #[strum(to_string = "ABLUP")]
    AblUp,
}
