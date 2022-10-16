mod context;
mod function;
mod system_func;
mod terminal_vm;
mod variable;

use erars_ast::BeginType;
use erars_ui::InputRequest;
use hashbrown::HashMap;
use pad::PadStr;
use strum::Display;

pub type SaveList = HashMap<u32, SerializableVariableStorage>;

pub trait SaveLoadManager {
    fn load_local_list(&self) -> SaveList;
    fn load_local(&self, idx: u32) -> Option<SerializableVariableStorage>;
    fn load_global(&self) -> Option<SerializableGlobalVariableStorage>;
    fn save_local(&self, idx: u32, sav: &SerializableVariableStorage);
    fn remove_local(&self, idx: u32);
    fn save_global(&self, sav: &SerializableGlobalVariableStorage);
    fn clone_manager(&self) -> Box<dyn SaveLoadManager>;
}

impl Clone for Box<dyn SaveLoadManager> {
    fn clone(&self) -> Self {
        self.clone_manager()
    }
}

pub struct NullSaveLoadManager;

impl SaveLoadManager for NullSaveLoadManager {
    fn load_local_list(&self) -> SaveList {
        SaveList::new()
    }

    fn load_local(&self, _idx: u32) -> Option<SerializableVariableStorage> {
        None
    }

    fn load_global(&self) -> Option<SerializableGlobalVariableStorage> {
        None
    }

    fn save_local(&self, _idx: u32, _sav: &SerializableVariableStorage) {}

    fn remove_local(&self, _idx: u32) {}

    fn save_global(&self, _sav: &SerializableGlobalVariableStorage) {}

    fn clone_manager(&self) -> Box<dyn SaveLoadManager> {
        Box::new(NullSaveLoadManager)
    }
}

pub type ArgVec = tinyvec::ArrayVec<[u32; 4]>;

pub use crate::{
    context::{Callstack, LocalValue, VmContext},
    function::{
        EventCollection, FunctionArgDef, FunctionBody, FunctionBodyHeader, FunctionDic,
        FunctionGotoLabel,
    },
    system_func::SystemState,
    terminal_vm::TerminalVm,
    variable::{
        SerializableGlobalVariableStorage, SerializableVariableStorage, UniformVariable,
        VariableStorage, VmVariable,
    },
};

pub use erars_compiler::{EraConfig, HeaderInfo, Instruction, Language};

#[derive(Display, Debug, Clone, PartialEq, Eq)]
pub enum Workflow {
    Return,
    SwitchState(SystemState),
    GotoState(SystemState),
    Begin(BeginType),
    Upstream(VmResult),
}

impl Default for Workflow {
    fn default() -> Self {
        Self::Return
    }
}

#[derive(Display, Debug, Clone, PartialEq, Eq)]
pub enum VmResult {
    Exit,
    Redraw,
    Input { req: InputRequest, set_result: bool },
}
