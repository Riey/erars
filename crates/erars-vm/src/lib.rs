mod context;
mod function;
mod save_data;
mod system_func;
mod terminal_vm;
mod variable;

use erars_ast::BeginType;
use erars_ui::InputRequest;
use pad::PadStr;
use strum::Display;

pub use crate::{
    context::{Callstack, LocalValue, VmContext},
    function::{FunctionBody, FunctionDic},
    system_func::SystemState,
    terminal_vm::TerminalVm,
    variable::{UniformVariable, VariableStorage, VmVariable},
};

pub use erars_compiler::{EraConfig, HeaderInfo, Instruction, Language};

#[derive(Display, Debug, Clone, PartialEq, Eq)]
pub enum Workflow {
    Return,
    SwitchState(SystemState),
    GotoState(SystemState),
    Begin(BeginType),
    Input { req: InputRequest, set_result: bool },
    Redraw,
    Exit,
}

impl Default for Workflow {
    fn default() -> Self {
        Self::Return
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum VmResult {
    Exit,
    Redraw,
    NeedInput { req: InputRequest, set_result: bool },
}
