mod context;
mod function;
mod terminal_vm;
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
    async fn input(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
    ) -> anyhow::Result<Option<Value>>;

    async fn input_int(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<i64> {
        let req = InputRequest::normal(vconsole.input_gen(), InputRequestType::Int);
        self.input(vconsole, req)
            .await?
            .ok_or_else(|| anyhow::anyhow!("Value is empty"))
            .and_then(Value::try_into_int)
    }

    async fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()>;

    async fn load_local_list(&mut self) -> anyhow::Result<SaveList>;
    async fn load_local(&mut self, idx: u32)
        -> anyhow::Result<Option<SerializableVariableStorage>>;
    async fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>>;
    async fn save_local(
        &mut self,
        idx: u32,
        sav: &SerializableVariableStorage,
    ) -> anyhow::Result<()>;
    async fn remove_local(&mut self, idx: u32) -> anyhow::Result<()>;
    async fn save_global(&mut self, sav: &SerializableGlobalVariableStorage) -> anyhow::Result<()>;

    fn clone_functions(&self) -> Box<dyn SystemFunctions>;
}

impl Clone for Box<dyn SystemFunctions> {
    fn clone(&self) -> Self {
        self.clone_functions()
    }
}

#[derive(Debug, Clone)]
pub enum VmArg {
    Empty,
    Input(Value),
    GlobalSav(SerializableGlobalVariableStorage),
    LocalSav(SerializableVariableStorage),
}

#[derive(Display, Debug, Clone)]
pub enum VmResult {
    Exit,
    Redraw,
    RefreshGlobalSave,
    RefreshLocalSave,
    SaveLocal(u32, SerializableVariableStorage),
    SaveGlobal(SerializableGlobalVariableStorage),
    Input { req: InputRequest, set_result: bool },
}
