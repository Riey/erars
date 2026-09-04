mod context;
/// The `@`-prefixed debug console commands, interpreted in the engine so
/// every front end gets them. Public because `erars-stdio` forwards a raw
/// line to it from its own input loop.
pub mod debug_console;
mod dotnet_number;
mod function;
/// Graphics state, and the [`graphics::Painted`] token that orders publishing
/// before painting. Public because every `SystemFunctions` implementor lives
/// in another crate and has to name the token in its `redraw` signature.
pub mod graphics;
mod html;
/// The per-session input log: what the user was asked, what they answered,
/// and how long they took. Public because `erars-loader` opens it and the
/// `analyze_inputs` example reads it back.
pub mod input_logger;
mod save;
/// The `resources/` startup loader. Public because `erars-loader` drives it.
pub mod resources;
mod terminal_vm;
mod variable;

use erars_ast::{BeginType, Value};
use erars_ui::{
    Color, ConsoleConfig, InputRequest, InputRequestType, InputState, MouseKeyEvent,
    VirtualConsole,
};
use hashbrown::HashMap;
use itertools::Either;
use strum::Display;

pub type SaveList = HashMap<u32, Either<SerializableVariableStorage, RawSaveData>>;

pub type ArgVec = tinyvec::ArrayVec<[u32; 4]>;

pub use crate::{
    context::{Callstack, LocalValue, VmContext},
    debug_console::{DebugCommand, DebugConsoleQuit, DebugLine},
    function::{EventCollection, FunctionArgDef, FunctionBody, FunctionDic, FunctionGotoLabel},
    graphics::{Bitmap, ColorMatrix, Font, GraphicsStore, Pen, Rect, Sprite, MAX_IMAGE_SIZE},
    input_logger::{InputAnswer, InputEvent, InputLogEntry, InputLogger, MouseKeyEventLog},
    save::{RawSaveData, SerializableGlobalVariableStorage, SerializableVariableStorage},
    terminal_vm::TerminalVm,
    variable::{UniformVariable, VariableStorage, VmVariable},
};

pub use erars_compiler::{EraConfig, HeaderInfo, Instruction, Language};

/// Console construction parameters derived from `emuera.config`
/// (spec Component 2): the PRINTC field width, the backlog size, the
/// drawable width in cells, the game encoding that decides half/full cells,
/// and the three colours.
/// Used by `erars-loader`, `tests/run_tests.rs` and the renderer tests.
pub fn console_config(cfg: &EraConfig) -> ConsoleConfig {
    // Emuera: `DrawingParam_ShapePositionShift = max(2, FontSize / 6)` and
    // `DrawableWidth = WindowX - shift` (`Config/Config.cs:222-225`) for every
    // drawing mode but WINAPI. A half-width cell is `FontSize / 2` px in the
    // MS Gothic metrics Emuera's defaults are stated against, which is what
    // makes the default 760 px window an 84-character `DRAWLINE`.
    let shift = (cfg.font_size / 6).max(2);
    let half_w = (cfg.font_size / 2).max(1);
    ConsoleConfig {
        printc_width: cfg.printc_width,
        max_log: cfg.max_log,
        drawable_cells: (cfg.window_width.saturating_sub(shift) / half_w) as usize,
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

    /// The one place a wait meets the console. Emuera force-paints when it
    /// settles into a wait (`EmueraConsole.cs:1184`), so this repaint ignores
    /// `REDRAW 0`, and it runs Emuera's message-skip loop
    /// (`EmueraConsole.cs:1145-1160`): while the user is fast-forwarding,
    /// message waits answer themselves and never reach the front-end.
    fn input_redraw(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
        painted: graphics::Painted<'_>,
    ) -> anyhow::Result<Option<Value>> {
        if vconsole.mes_skip() {
            if matches!(
                req.ty,
                InputRequestType::AnyKey | InputRequestType::EnterKey
            ) {
                self.redraw(vconsole, painted)?;
                return Ok(None);
            }
            // Emuera breaks the skip loop on a wait that needs a value
            // (`NeedValue`) or refuses to be skipped (`StopMesskip`), and
            // clears the flag as the run ends.
            vconsole.set_mes_skip(false);
        }
        self.redraw(vconsole, painted)?;
        let ret = self.input(req)?;
        if self.take_mes_skip() {
            vconsole.set_mes_skip(true);
        }
        Ok(ret)
    }

    /// Did the input just delivered also ask to start message skip? Emuera's
    /// triggers are Escape, a right click, and `\e` inside an entered line
    /// (`EmueraConsole.cs:1078`, `:1131`). Reading clears the request.
    fn take_mes_skip(&mut self) -> bool {
        false
    }

    fn input_int_redraw(
        &mut self,
        vconsole: &mut VirtualConsole,
        painted: graphics::Painted<'_>,
    ) -> anyhow::Result<i64> {
        let req = InputRequest::normal(vconsole.input_gen(), InputRequestType::Int);

        self.input_redraw(vconsole, req, painted)?
            .ok_or_else(|| anyhow::anyhow!("Value is empty"))
            .and_then(Value::try_into_int)
    }

    /// INPUTMOUSEKEY: wait for one raw mouse or key event. Emuera's console
    /// force-paints before and after such a wait
    /// (`EmueraConsole.cs:1050-1072`), so this repaints like `input_redraw`
    /// and is never skipped by `MESSKIP`.
    ///
    /// A front-end with no raw event source can only ever produce the
    /// expiry event, which is exactly what Emuera delivers when the time
    /// limit runs out.
    fn input_mouse_key(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
        painted: graphics::Painted<'_>,
    ) -> anyhow::Result<MouseKeyEvent> {
        let _ = req;
        self.redraw(vconsole, painted)?;
        Ok(MouseKeyEvent::TIMEOUT)
    }

    /// CHKFONT: is a font family with this name available for SETFONT?
    /// Emuera asks `InstalledFontCollection`; a front-end that keeps no font
    /// database can only answer "not here", which is also the truthful answer
    /// for one that cannot render fonts at all.
    fn chk_font(&mut self, name: &str) -> anyhow::Result<bool> {
        let _ = name;
        Ok(false)
    }

    /// GETKEY / GETKEYTRIGGERED / MOUSEX / MOUSEY: the live keyboard and
    /// cursor. Emuera queries the OS per call (`Creator.Method.cs:6725`,
    /// `EmueraConsole.cs:1981-1990`); erars asks the front-end, because only
    /// it has a window.
    ///
    /// The default is the state Emuera itself reports before its window
    /// exists: `GetMousePosition` returns `new Point()` for a null window
    /// (`EmueraConsole.cs:1983-1984`), and a front-end with no keyboard has
    /// nothing down.
    fn input_state(&mut self) -> anyhow::Result<InputState> {
        Ok(InputState::default())
    }

    /// Paint the console. The [`graphics::Painted`] argument is proof that
    /// `GraphicsStore::publish` already ran for this frame, so an
    /// implementation may read `vconsole.images` and know the pixels match the
    /// text. It cannot be forged outside `graphics.rs`, which is what stops a
    /// new call site from repainting stale pixels.
    fn redraw(
        &mut self,
        vconsole: &mut VirtualConsole,
        painted: graphics::Painted<'_>,
    ) -> anyhow::Result<()>;
}

#[derive(Clone, Copy)]
pub struct NullSystemFunctions;

#[allow(unused_variables)]
impl SystemFunctions for NullSystemFunctions {
    fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        Ok(None)
    }

    fn redraw(
        &mut self,
        vconsole: &mut VirtualConsole,
        painted: graphics::Painted<'_>,
    ) -> anyhow::Result<()> {
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
