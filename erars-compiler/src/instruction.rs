use crate::{Alignment, BinaryOperator, BuiltinCommand, PrintFlags, UnaryOperator, VariableIndex};
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
    PadStr(Alignment),
    LoadVar(VariableIndex, u32),
    StoreVar(VariableIndex, u32),
    Varset {
        code: VariableIndex,
        args: u32,
        varset_args: u32,
    },
    CallMethod(u32),
    Call(u32),
    /// if name is not exists, push 1
    TryCall(u32),
    Jump(u32),
    /// if name is not exists, push 1
    TryJump(u32),
    Begin(BeginType),
    Print(PrintFlags),
    ReuseLastLine,
    ConcatString(u32),
    Command(BuiltinCommand, u32),
    BinaryOperator(BinaryOperator),
    UnaryOperator(UnaryOperator),
    SetAlignment(Alignment),
    Times(NotNan<f32>),
    Return,
    ReturnF,
    GotoLabel,
    /// if name is not exists, push 1
    TryGotoLabel,
    Goto(u32),
    GotoIfNot(u32),
}
