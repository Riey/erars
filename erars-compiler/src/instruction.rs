use erars_ast::{
    Alignment, BeginType, BinaryOperator, BuiltinCommand, NotNan, PrintFlags, UnaryOperator,
};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

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
    LoadVar(SmolStr, u32),
    StoreVar(SmolStr, u32),
    Varset {
        code: SmolStr,
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
