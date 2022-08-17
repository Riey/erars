use erars_ast::{
    Alignment, BeginType, BinaryOperator, BuiltinCommand, NotNan, PrintFlags, ScriptPosition,
    UnaryOperator,
};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Nop,
    Pop,
    /// For debugging purpose
    ReportPosition(ScriptPosition),
    /// Duplicate first value in stack
    Duplicate,
    /// Duplicate second value in stack
    DuplicatePrev,
    LoadInt(i64),
    LoadStr(Box<str>),
    EvalFormString,
    PadStr(Alignment),
    LoadVarRef(u32),
    LoadExternVarRef(u32),
    /// Read VarRef into Value
    /// If value is not VarRef, This is noop
    ReadVar,
    StoreVar,
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
