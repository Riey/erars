mod context;
mod function;
mod save;
mod terminal_vm;
mod variable;

use erars_ast::{BeginType, Value};
use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
use hashbrown::HashMap;
use itertools::Either;
use pad::PadStr;
use strum::Display;

pub type SaveList = HashMap<u32, Either<SerializableVariableStorage, RawSaveData>>;

pub type ArgVec = tinyvec::ArrayVec<[u32; 4]>;

pub use crate::{
    context::{Callstack, LocalValue, VmContext},
    function::{EventCollection, FunctionArgDef, FunctionBody, FunctionDic, FunctionGotoLabel},
    save::{RawSaveData, SerializableGlobalVariableStorage, SerializableVariableStorage},
    terminal_vm::TerminalVm,
    variable::{UniformVariable, VariableStorage, VmVariable},
};

pub use erars_compiler::{EraConfig, HeaderInfo, Instruction, Language};

#[derive(Display, Debug, Clone)]
pub enum Workflow {
    Return,
    Exit,
    Begin(BeginType),
}

impl Default for Workflow {
    fn default() -> Self {
        Self::Return
    }
}

pub trait SystemFunctions {
    fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>>;

    fn input_redraw(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
    ) -> anyhow::Result<Option<Value>> {
        self.redraw(vconsole)?;
        self.input(req)
    }

    fn input_int_redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<i64> {
        let req = InputRequest::normal(vconsole.input_gen(), InputRequestType::Int);

        self.input_redraw(vconsole, req)?
            .ok_or_else(|| anyhow::anyhow!("Value is empty"))
            .and_then(Value::try_into_int)
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()>;
}

#[derive(Clone, Copy)]
pub struct NullSystemFunctions;

#[allow(unused_variables)]
impl SystemFunctions for NullSystemFunctions {
    fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        Ok(None)
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        Ok(())
    }
}
