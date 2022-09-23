use erars_ast::{
    Alignment, BeginType, BinaryOperator, BuiltinCommand, BuiltinMethod, BuiltinVariable,
    EventType, NotNan, PrintFlags, ScriptPosition, UnaryOperator,
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
    /// Padding first value
    ///
    /// Int value will converted into String
    PadStr(Alignment),
    LoadVarRef(u32),
    LoadExternVarRef(u32),
    /// Read VarRef into Value
    ///
    /// If value is not VarRef, This is noop
    ReadVar,
    StoreVar,
    /// Store top value to RESULT/RESULTS
    StoreResult,
    Call(u32),
    /// if name is not exists, push 1
    TryCall(u32),
    Jump(u32),
    /// if name is not exists, push 1
    TryJump(u32),
    Begin(BeginType),
    CallEvent(EventType),
    /// Print first value
    Print(PrintFlags),
    ReuseLastLine,
    ConcatString(u32),
    BuiltinVar(BuiltinVariable, u32),
    BuiltinCommand(BuiltinCommand, u32),
    BuiltinMethod(BuiltinMethod, u32),
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
    GotoIf(u32),
}
