use crate::{
    compiler::{Alignment, PrintFlags},
    operator::{BinaryOperator, TernaryOperator, UnaryOperator},
};
use serde::{Deserialize, Serialize};
use strum::EnumString;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize, EnumString)]
pub enum BeginType {
    #[strum(to_string = "TITLE")]
    Title,
    #[strum(to_string = "FIRST")]
    First,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Nop,
    Pop,
    Duplicate,
    ListBegin,
    ListEnd,
    LoadInt(i64),
    LoadStr(String),
    LoadVar,
    StoreVar,
    CallMethod,
    Call,
    Begin(BeginType),
    Print(PrintFlags),
    ReuseLastLine,
    ConcatString,
    Command,
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    TernaryOperator(TernaryOperator),
    SetAlignment(Alignment),
    Goto(u32),
    GotoIfNot(u32),
}
