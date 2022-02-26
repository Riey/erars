use crate::{
    compiler::{Alignment, PrintFlags},
    operator::{BinaryOperator, UnaryOperator},
};
use ordered_float::NotNan;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumString};

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
    AlignString,
    ReuseLastLine,
    ConcatString,
    Command,
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    SetAlignment(Alignment),
    Goto(u32),
    GotoIfNot(u32),
}
