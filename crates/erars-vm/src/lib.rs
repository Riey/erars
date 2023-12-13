mod context;
mod function;
mod save;
mod terminal_vm;
mod variable;

use std::future::Future;
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

pub trait SystemFunctions: Send {
    fn input(&mut self, req: InputRequest) -> impl Future<Output = anyhow::Result<Option<Value>>> + Send;

    fn input_redraw(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
    ) -> impl Future<Output = anyhow::Result<Option<Value>>> + Send {
        async move {
            self.redraw(vconsole).await?;
            self.input(req).await
        }
    }

    fn input_int_redraw(&mut self, vconsole: &mut VirtualConsole) -> impl Future<Output = anyhow::Result<i64>> + Send {
        async move {
            let req = InputRequest::normal(vconsole.input_gen(), InputRequestType::Int);

            self.input_redraw(vconsole, req).await?
                .ok_or_else(|| anyhow::anyhow!("Value is empty"))
                .and_then(Value::try_into_int)
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> impl Future<Output = anyhow::Result<()>> + Send;
}

#[derive(Clone, Copy)]
pub struct NullSystemFunctions;

#[allow(unused_variables)]
impl SystemFunctions for NullSystemFunctions {
    fn input(&mut self, req: InputRequest) -> impl Future<Output = anyhow::Result<Option<Value>>> + Send {
        async {
            Ok(None)
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> impl Future<Output = anyhow::Result<()>> + Send {
        async {
            Ok(())
        }
    }
}
