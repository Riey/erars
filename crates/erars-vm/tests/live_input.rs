//! `GETKEY` / `GETKEYTRIGGERED` / `MOUSEX` / `MOUSEY` against a scripted
//! front-end.
//!
//! Emuera serves the first two from one `GetKeyStateMethod`
//! (`GameData/Function/Creator.Method.cs:6710-6735`) over a process-wide
//! `static readonly short[] keytoggle`, so "triggered" is not a press edge the
//! window reported: it is "down, and the toggle bit differs from whatever the
//! last call — of *either* name — stored". Every expectation below is derived
//! from that source, not from erars' output.
//!
//! The state list is consumed one entry per query, which also pins the order
//! of effects: an out-of-range keycode returns 0 *before* the OS is asked
//! (`:6722-6723`), so it must not consume an entry, and the final call count
//! proves it.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use erars_ast::{StrKey, Value};
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::{InputRequest, InputState, VirtualConsole};
use erars_vm::{console_config, FunctionDic, SystemFunctions, TerminalVm, VmContext};

/// Hands out the next scripted [`InputState`] per query and counts them.
struct Keyboard {
    states: Vec<InputState>,
    queries: Arc<AtomicUsize>,
}

impl SystemFunctions for Keyboard {
    fn input(&mut self, _req: InputRequest) -> anyhow::Result<Option<Value>> {
        Ok(None)
    }

    fn input_state(&mut self) -> anyhow::Result<InputState> {
        let n = self.queries.fetch_add(1, Ordering::Relaxed);
        let state = *self
            .states
            .get(n)
            .unwrap_or_else(|| panic!("unscripted input_state query #{n}"));
        Ok(state)
    }

    fn redraw(
        &mut self,
        _vconsole: &mut VirtualConsole,
        _painted: erars_vm::graphics::Painted<'_>,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

const VK_SHIFT: u8 = 0x10;
const VK_CONTROL: u8 = 0x11;

/// `down` = `GetKeyState`'s high bit, `toggled` = its low bit.
fn key(vk: u8, down: bool, toggled: bool) -> InputState {
    let mut s = InputState::default();
    s.set_down(vk, down);
    if toggled {
        s.flip_toggled(vk);
    }
    s
}

const SCRIPT: &str = r#"@SYSTEM_TITLE
PRINTFORML {GETKEYTRIGGERED(0x10)}
PRINTFORML {GETKEYTRIGGERED(0x10)}
PRINTFORML {GETKEY(0x10)}
PRINTFORML {GETKEY(0x10)}
PRINTFORML {GETKEYTRIGGERED(0x10)}
PRINTFORML {GETKEYTRIGGERED(0x10)}
PRINTFORML {GETKEY(-1)}
PRINTFORML {GETKEY(256)}
PRINTFORML {GETKEYTRIGGERED(0x11)}
PRINTFORML {MOUSEX()},{MOUSEY()}
PRINTFORML {ISACTIVE()}
QUIT
"#;

fn run(states: Vec<InputState>) -> (Vec<String>, usize) {
    erars_ast::init_interner();
    let info = HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    };
    let header = Arc::new(info);
    let config = EraConfig::default();
    let queries = Arc::new(AtomicUsize::new(0));
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(Keyboard {
            states,
            queries: queries.clone(),
        }),
        "sav".into(),
        "resources".into(),
    );

    let parser = ParserContext::new(header.clone(), StrKey::new("LIVE_INPUT.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser.parse_program_str(SCRIPT).unwrap() {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    let vm = TerminalVm::new(dic, header);
    let ok = vm.start(&mut tx, &mut ctx);
    let lines: Vec<String> = tx.lines.iter().map(ToString::to_string).collect();
    assert!(ok, "VM error:\n{}", lines.join("\n"));
    (lines, queries.load(Ordering::Relaxed))
}

#[test]
fn the_toggle_latch_is_shared_by_both_names_and_survives_a_release() {
    let mut mouse = InputState::default();
    mouse.mouse_x = 37;
    mouse.mouse_y = -12;

    let (lines, queries) = run(vec![
        // 1. A held key, first observation: latch 0 -> 2, and it is down.
        key(VK_SHIFT, true, true),
        // 2. Nothing changed, so the same call now answers 0.
        key(VK_SHIFT, true, true),
        // 3. Released with the toggle bit still on: plain GETKEY, 0, and the
        //    latch it writes (2) is the one it already held.
        key(VK_SHIFT, false, true),
        // 4. Pressed again, so Windows flipped the bit off: GETKEY answers 1
        //    and stores 1 — consuming the edge.
        key(VK_SHIFT, true, false),
        // 5. Which is why GETKEYTRIGGERED now says 0 for a key that *was*
        //    freshly pressed.
        key(VK_SHIFT, true, false),
        // 6. A further press flips the bit again: triggered once more.
        key(VK_SHIFT, true, true),
        // (no entry for GETKEY(-1) or GETKEY(256): neither reaches the OS)
        // 7. A different key keeps its own latch, so its first observation
        //    triggers even though VK_SHIFT's latch is warm.
        key(VK_CONTROL, true, true),
        // 8. MOUSEX and MOUSEY are one query each.
        mouse,
        mouse,
    ]);

    assert_eq!(
        lines,
        [
            "1", "0", "0", "1", "0", "1", // GETKEY / GETKEYTRIGGERED
            "0", "0", // out of range
            "1", // VK_CONTROL
            "37,-12", "1",
        ]
    );
    assert_eq!(queries, 9, "an out-of-range keycode must not query the OS");
}
