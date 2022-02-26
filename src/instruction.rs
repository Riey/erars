use crate::{
    compiler::{Alignment, PrintFlags},
    operator::{BinaryOperator, UnaryOperator},
};
use serde::{Deserialize, Serialize};
use ordered_float::NotNan;
use strum::EnumString;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize, EnumString)]
pub enum BeginType {
    #[strum(to_string = "TITLE")]
    Title,
    #[strum(to_string = "FIRST")]
    First,
    #[strum(to_string = "SHOP")]
    Shop,
}

#[non_exhaustive]
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
    Times(NotNan<f32>),
    Return,
    ReturnF,
    Call,
    Begin(BeginType),
    Print(PrintFlags),
    ReuseLastLine,
    ConcatString,
    Command,
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    SetAlignment(Alignment),
    Goto(u32),
    GotoIfNot(u32),
}
