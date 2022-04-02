use crate::{Alignment, BinaryOperator, PrintFlags, UnaryOperator, VariableIndex};
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

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Nop,
    Pop,
    /// Duplicate first value in stack
    Duplicate,
    /// Duplicate second value in stack
    DuplicatePrev,
    LoadInt(i64),
    LoadStr(String),
    LoadVar(VariableIndex, u32),
    StoreVar(VariableIndex, u32),
    Varset {
        code: VariableIndex,
        args: u32,
        varset_args: u32,
    },
    CallMethod(u32),
    Call(u32),
    Begin(BeginType),
    Print(PrintFlags),
    ReuseLastLine,
    ConcatString(u32),
    Command(u32),
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    SetAlignment(Alignment),
    Times(NotNan<f32>),
    Return,
    ReturnF,
    Goto(u32),
    GotoIfNot(u32),
}
