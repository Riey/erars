mod context;
mod function;
mod save;
mod terminal_vm;
mod variable;

use erars_ast::{BeginType, Value};
use erars_ui::{Color, ConsoleConfig, InputRequest, InputRequestType, VirtualConsole};
use hashbrown::HashMap;
use itertools::Either;
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

/// Console construction parameters derived from `emuera.config`
/// (spec Component 2): the PRINTC field width, the backlog size, the
/// game encoding that decides half/full cells, and the three colours.
/// Used by `erars-loader`, `tests/run_tests.rs` and the renderer tests.
pub fn console_config(cfg: &EraConfig) -> ConsoleConfig {
    ConsoleConfig {
        printc_width: cfg.printc_width,
        max_log: cfg.max_log,
        encoding: cfg.lang.encoding(),
        fore_color: Color(cfg.fore_color),
        bg_color: Color(cfg.bg_color),
        focus_color: Color(cfg.focus_color),
    }
}

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

#[cfg(test)]
mod console_config_tests {
    use super::*;

    #[test]
    fn console_config_uses_language_encoding_and_colours() {
        let cfg = EraConfig {
            lang: Language::Japanese,
            printc_width: 25,
            max_log: 7,
            fore_color: [1, 2, 3],
            bg_color: [4, 5, 6],
            focus_color: [7, 8, 9],
            ..Default::default()
        };
        let c = console_config(&cfg);
        assert_eq!(c.encoding, encoding_rs::SHIFT_JIS);
        assert_eq!(c.printc_width, 25);
        assert_eq!(c.max_log, 7);
        assert_eq!(c.fore_color, Color([1, 2, 3]));
        assert_eq!(c.bg_color, Color([4, 5, 6]));
        assert_eq!(c.focus_color, Color([7, 8, 9]));

        let kr = EraConfig {
            lang: Language::Korean,
            ..Default::default()
        };
        assert_eq!(console_config(&kr).encoding, encoding_rs::EUC_KR);
    }
}
