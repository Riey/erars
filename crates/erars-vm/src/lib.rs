mod context;
mod function;
mod terminal_vm;
mod proxy;
#[allow(unused)]
mod proxy_msg_generated;
mod variable;

use erars_ast::{BeginType, Value};
use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
use hashbrown::HashMap;
use pad::PadStr;
use strum::Display;

pub type SaveList = HashMap<u32, SerializableVariableStorage>;

pub type ArgVec = tinyvec::ArrayVec<[u32; 4]>;

pub use crate::{
    context::{Callstack, LocalValue, VmContext},
    function::{EventCollection, FunctionArgDef, FunctionBody, FunctionDic, FunctionGotoLabel},
    terminal_vm::TerminalVm,
    variable::{
        SerializableGlobalVariableStorage, SerializableVariableStorage, UniformVariable,
        VariableStorage, VmVariable,
    },
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

#[async_trait::async_trait(?Send)]
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

        self.input_redraw(vconsole, req)
            ?
            .ok_or_else(|| anyhow::anyhow!("Value is empty"))
            .and_then(Value::try_into_int)
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()>;

    fn load_local_list(&mut self) -> anyhow::Result<SaveList>;
    fn load_local(&mut self, idx: u32)
        -> anyhow::Result<Option<SerializableVariableStorage>>;
    fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>>;
    fn save_local(
        &mut self,
        idx: u32,
        sav: SerializableVariableStorage,
    ) -> anyhow::Result<()>;
    fn remove_local(&mut self, idx: u32) -> anyhow::Result<()>;
    fn save_global(&mut self, sav: SerializableGlobalVariableStorage) -> anyhow::Result<()>;
}

#[derive(Clone, Copy)]
pub struct NullSystemFunctions;

#[async_trait::async_trait(?Send)]
#[allow(unused_variables)]
impl SystemFunctions for NullSystemFunctions {
    fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        Ok(None)
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        Ok(())
    }

    fn load_local_list(&mut self) -> anyhow::Result<SaveList> {
        Ok(SaveList::new())
    }
    fn load_local(
        &mut self,
        idx: u32,
    ) -> anyhow::Result<Option<SerializableVariableStorage>> {
        Ok(None)
    }
    fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>> {
        Ok(None)
    }
    fn save_local(
        &mut self,
        idx: u32,
        sav: SerializableVariableStorage,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    fn remove_local(&mut self, idx: u32) -> anyhow::Result<()> {
        Ok(())
    }
    fn save_global(&mut self, sav: SerializableGlobalVariableStorage) -> anyhow::Result<()> {
        Ok(())
    }
}
