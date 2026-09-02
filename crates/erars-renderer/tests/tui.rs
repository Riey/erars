//! Integration: compile and run the synthetic `tests/games/tui` game headlessly
//! with a scripted `SystemFunctions`, capture the console frame at the first
//! INPUT and snapshot its layout for the KOREAN and JAPANESE configs. With a
//! GPU adapter the KOREAN frame is also rendered to a PNG under `target/tmp`.
//!
//! Expected geometry (Emuera defaults 760 px, 18/19 px, PRINTC 25, bundled
//! Noto Sans Mono through `FontChain::from_files`, pinned CellMetrics):
//! - title row Center: 20 cells = 180 px → x0 = 380 − 90 = 290
//! - DRAWLINE: 84 `-` = 756 px; CUSTOMDRAWLINE ━: 42 × 18 = 756 px
//! - PRINTC row: 3 × 25 cells; `한` at 189, `a` at 423, `▒` at 639 (KR, 2 cells)
//!   or 657 (JP, 1 cell)
//! - PRINTLC row: 2 × 26 cells; `1` at 234
//! - Right row `▒░█═║`: KR 6 cells → x0 706, JP 5 cells → x0 715
//! - bold orange `굵은 주황` (c=FF8000 s=B) then `보통` at x 81
//! - PRINTPLAINFORM `ab\ncd`: row `ab` then a continuation row `cd`
//! - buttons: `[0] 시작` (w 72) + `[1] 이어하기` (w 108, `\n` stripped) on one
//!   row; the 94-cell button wraps into fragments of 756 px and 90 px

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use erars_ast::Value;
use erars_compiler::{EraConfig, Language};
use erars_proxy_system::ConsoleFrame;
use erars_renderer::font::{FontChain, FontConfig};
use erars_renderer::headless::{render_frame_on, write_png};
use erars_renderer::layout::{layout, layout_snapshot, Geometry};
use erars_renderer::test_support::{gpu_device, gpu_lock};
use erars_renderer::text::{CellMetrics, Shaper};
use erars_ui::width::WidthTable;
use erars_ui::{InputRequest, VirtualConsole};
use erars_vm::SystemFunctions;

const GAME: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/games/tui");
const BUNDLED: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/assets/NotoSansMono-Regular.ttf"
);
/// Pinned metrics (spec Testing §4): row geometry is font-independent.
const M: CellMetrics = CellMetrics {
    scale: 1.0,
    font_px: 18,
    half_w: 9,
    line_h: 19,
    baseline: 15,
    shift: 3,
};
/// `emuera.config` 文字色 default.
const DEFAULT_FG: [u8; 3] = [192, 192, 192];

/// Records the latest redraw; freezes it when the game first asks for input;
/// answers that INPUT with 0 and refuses any further one.
struct Scripted {
    latest: ConsoleFrame,
    at_input: Arc<Mutex<Option<ConsoleFrame>>>,
    answers: Vec<Value>,
}

impl SystemFunctions for Scripted {
    fn input(&mut self, _req: InputRequest) -> anyhow::Result<Option<Value>> {
        let mut slot = self.at_input.lock().unwrap();
        if slot.is_none() {
            *slot = Some(self.latest.clone());
        }
        match self.answers.pop() {
            Some(v) => Ok(Some(v)),
            None => anyhow::bail!("script exhausted"),
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        self.latest = ConsoleFrame::from_vconsole(vconsole);
        Ok(())
    }
}

fn read_config(name: &str) -> EraConfig {
    let text = std::fs::read_to_string(Path::new(GAME).join(name)).unwrap();
    EraConfig::from_text(text.trim_start_matches('\u{feff}')).unwrap()
}

fn console_text(tx: &VirtualConsole) -> String {
    let mut out: Vec<String> = tx.lines_from(0).iter().map(|l| l.to_string()).collect();
    out.push(tx.last_line.to_string());
    out.join("\n")
}

/// Compile + run the fixture with `config`, return the frame at the first
/// INPUT with everything the loader printed before `==TUI==` removed.
/// Runs are serialised: the VM shares one global string interner.
fn run_game(config_name: &str) -> ConsoleFrame {
    static RUN: Mutex<()> = Mutex::new(());
    let _guard = RUN.lock().unwrap_or_else(|e| e.into_inner());
    let at_input = Arc::new(Mutex::new(None));
    let system = Box::new(Scripted {
        latest: ConsoleFrame::default(),
        at_input: at_input.clone(),
        answers: vec![Value::Int(0)],
    });
    let (vm, mut ctx, mut tx) =
        erars_loader::run_script(GAME, system, read_config(config_name), true, false)
            .expect("compile tests/games/tui");
    let ok = vm.start(&mut tx, &mut ctx);
    assert!(ok, "VM error:\n{}", console_text(&tx));
    let frame = at_input
        .lock()
        .unwrap()
        .take()
        .expect("the game never asked for INPUT");
    let marker = frame
        .lines
        .iter()
        .position(|l| l.to_string() == "==TUI==")
        .expect("==TUI== marker line");
    ConsoleFrame {
        lines: frame.lines[marker + 1..].to_vec(),
        ..frame
    }
}

fn snapshot_of(frame: &ConsoleFrame, lang: Language) -> String {
    let mut shaper = Shaper::new(
        FontChain::from_files(&[PathBuf::from(BUNDLED)], lang),
        WidthTable::new(lang.encoding()),
        M,
    );
    let g = Geometry::new(760, M);
    assert_eq!(g.drawable_w, 757);
    let l = layout(&frame.lines, &g, &mut shaper);
    assert_eq!(l.rows.len(), 15, "rows");
    assert_eq!(l.buttons.len(), 4, "button fragments");
    layout_snapshot(&l, DEFAULT_FG)
}

#[test]
fn tui_layout_korean() {
    let frame = run_game("emuera.config");
    assert_eq!(frame.bg_color.0, [0, 0, 0]);
    assert_eq!(frame.hl_color.0, [255, 255, 0]);
    assert_eq!(frame.fore_color.0, DEFAULT_FG);
    k9::snapshot!(
        snapshot_of(&frame, Language::Korean),
        r#"
row 0 line 0 x0=290 w=180
  0:2 "★"
  18:1 " "
  27:2 "텍"
  45:2 "스"
  63:2 "트"
  81:1 " "
  90:1 "U"
  99:1 "I"
  108:1 " "
  117:2 "데"
  135:2 "모"
  153:1 " "
  162:2 "★"
row 1 line 1 x0=0 w=756
  0:1 "-"
  9:1 "-"
  18:1 "-"
  27:1 "-"
  36:1 "-"
  45:1 "-"
  54:1 "-"
  63:1 "-"
  72:1 "-"
  81:1 "-"
  90:1 "-"
  99:1 "-"
  108:1 "-"
  117:1 "-"
  126:1 "-"
  135:1 "-"
  144:1 "-"
  153:1 "-"
  162:1 "-"
  171:1 "-"
  180:1 "-"
  189:1 "-"
  198:1 "-"
  207:1 "-"
  216:1 "-"
  225:1 "-"
  234:1 "-"
  243:1 "-"
  252:1 "-"
  261:1 "-"
  270:1 "-"
  279:1 "-"
  288:1 "-"
  297:1 "-"
  306:1 "-"
  315:1 "-"
  324:1 "-"
  333:1 "-"
  342:1 "-"
  351:1 "-"
  360:1 "-"
  369:1 "-"
  378:1 "-"
  387:1 "-"
  396:1 "-"
  405:1 "-"
  414:1 "-"
  423:1 "-"
  432:1 "-"
  441:1 "-"
  450:1 "-"
  459:1 "-"
  468:1 "-"
  477:1 "-"
  486:1 "-"
  495:1 "-"
  504:1 "-"
  513:1 "-"
  522:1 "-"
  531:1 "-"
  540:1 "-"
  549:1 "-"
  558:1 "-"
  567:1 "-"
  576:1 "-"
  585:1 "-"
  594:1 "-"
  603:1 "-"
  612:1 "-"
  621:1 "-"
  630:1 "-"
  639:1 "-"
  648:1 "-"
  657:1 "-"
  666:1 "-"
  675:1 "-"
  684:1 "-"
  693:1 "-"
  702:1 "-"
  711:1 "-"
  720:1 "-"
  729:1 "-"
  738:1 "-"
  747:1 "-"
row 2 line 2 x0=0 w=108
  0:2 "┏"
  18:2 "━"
  36:2 "━"
  54:2 "━"
  72:2 "━"
  90:2 "┓"
row 3 line 3 x0=0 w=108
  0:2 "┃"
  18:2 "가"
  36:1 "A"
  45:1 " "
  54:1 " "
  63:1 " "
  72:1 " "
  81:1 " "
  90:2 "┃"
row 4 line 4 x0=0 w=108
  0:2 "┗"
  18:2 "━"
  36:2 "━"
  54:2 "━"
  72:2 "━"
  90:2 "┛"
row 5 line 5 x0=0 w=756
  0:2 "━"
  18:2 "━"
  36:2 "━"
  54:2 "━"
  72:2 "━"
  90:2 "━"
  108:2 "━"
  126:2 "━"
  144:2 "━"
  162:2 "━"
  180:2 "━"
  198:2 "━"
  216:2 "━"
  234:2 "━"
  252:2 "━"
  270:2 "━"
  288:2 "━"
  306:2 "━"
  324:2 "━"
  342:2 "━"
  360:2 "━"
  378:2 "━"
  396:2 "━"
  414:2 "━"
  432:2 "━"
  450:2 "━"
  468:2 "━"
  486:2 "━"
  504:2 "━"
  522:2 "━"
  540:2 "━"
  558:2 "━"
  576:2 "━"
  594:2 "━"
  612:2 "━"
  630:2 "━"
  648:2 "━"
  666:2 "━"
  684:2 "━"
  702:2 "━"
  720:2 "━"
  738:2 "━"
row 6 line 6 x0=0 w=675
  0:1 " "
  9:1 " "
  18:1 " "
  27:1 " "
  36:1 " "
  45:1 " "
  54:1 " "
  63:1 " "
  72:1 " "
  81:1 " "
  90:1 " "
  99:1 " "
  108:1 " "
  117:1 " "
  126:1 " "
  135:1 " "
  144:1 " "
  153:1 " "
  162:1 " "
  171:1 " "
  180:1 " "
  189:2 "한"
  207:2 "글"
  225:1 " "
  234:1 " "
  243:1 " "
  252:1 " "
  261:1 " "
  270:1 " "
  279:1 " "
  288:1 " "
  297:1 " "
  306:1 " "
  315:1 " "
  324:1 " "
  333:1 " "
  342:1 " "
  351:1 " "
  360:1 " "
  369:1 " "
  378:1 " "
  387:1 " "
  396:1 " "
  405:1 " "
  414:1 " "
  423:1 "a"
  432:1 "b"
  441:1 "c"
  450:1 " "
  459:1 " "
  468:1 " "
  477:1 " "
  486:1 " "
  495:1 " "
  504:1 " "
  513:1 " "
  522:1 " "
  531:1 " "
  540:1 " "
  549:1 " "
  558:1 " "
  567:1 " "
  576:1 " "
  585:1 " "
  594:1 " "
  603:1 " "
  612:1 " "
  621:1 " "
  630:1 " "
  639:2 "▒"
  657:2 "▒"
row 7 line 7 x0=0 w=468
  0:2 "항"
  18:2 "목"
  36:1 " "
  45:1 " "
  54:1 " "
  63:1 " "
  72:1 " "
  81:1 " "
  90:1 " "
  99:1 " "
  108:1 " "
  117:1 " "
  126:1 " "
  135:1 " "
  144:1 " "
  153:1 " "
  162:1 " "
  171:1 " "
  180:1 " "
  189:1 " "
  198:1 " "
  207:1 " "
  216:1 " "
  225:1 " "
  234:1 "1"
  243:1 "2"
  252:1 "3"
  261:1 "4"
  270:1 "5"
  279:1 " "
  288:1 " "
  297:1 " "
  306:1 " "
  315:1 " "
  324:1 " "
  333:1 " "
  342:1 " "
  351:1 " "
  360:1 " "
  369:1 " "
  378:1 " "
  387:1 " "
  396:1 " "
  405:1 " "
  414:1 " "
  423:1 " "
  432:1 " "
  441:1 " "
  450:1 " "
  459:1 " "
row 8 line 8 x0=706 w=54
  0:2 "▒"
  18:1 "░"
  27:1 "█"
  36:1 "═"
  45:1 "║"
row 9 line 9 x0=0 w=117
  0:2 "굵" c=FF8000 s=B
  18:2 "은" c=FF8000 s=B
  36:1 " " c=FF8000 s=B
  45:2 "주" c=FF8000 s=B
  63:2 "황" c=FF8000 s=B
  81:2 "보"
  99:2 "통"
row 10 line 10 x0=0 w=18
  0:1 "a"
  9:1 "b"
row 11 line 10+ x0=0 w=18
  0:1 "c"
  9:1 "d"
row 12 line 11 x0=0 w=180
  0:1 "[" btn=0
  9:1 "0" btn=0
  18:1 "]" btn=0
  27:1 " " btn=0
  36:2 "시" btn=0
  54:2 "작" btn=0
  72:1 "[" btn=1
  81:1 "1" btn=1
  90:1 "]" btn=1
  99:1 " " btn=1
  108:2 "이" btn=1
  126:2 "어" btn=1
  144:2 "하" btn=1
  162:2 "기" btn=1
row 13 line 12 x0=0 w=756
  0:1 "[" btn=2
  9:1 "2" btn=2
  18:1 "]" btn=2
  27:1 " " btn=2
  36:1 "x" btn=2
  45:1 "x" btn=2
  54:1 "x" btn=2
  63:1 "x" btn=2
  72:1 "x" btn=2
  81:1 "x" btn=2
  90:1 "x" btn=2
  99:1 "x" btn=2
  108:1 "x" btn=2
  117:1 "x" btn=2
  126:1 "x" btn=2
  135:1 "x" btn=2
  144:1 "x" btn=2
  153:1 "x" btn=2
  162:1 "x" btn=2
  171:1 "x" btn=2
  180:1 "x" btn=2
  189:1 "x" btn=2
  198:1 "x" btn=2
  207:1 "x" btn=2
  216:1 "x" btn=2
  225:1 "x" btn=2
  234:1 "x" btn=2
  243:1 "x" btn=2
  252:1 "x" btn=2
  261:1 "x" btn=2
  270:1 "x" btn=2
  279:1 "x" btn=2
  288:1 "x" btn=2
  297:1 "x" btn=2
  306:1 "x" btn=2
  315:1 "x" btn=2
  324:1 "x" btn=2
  333:1 "x" btn=2
  342:1 "x" btn=2
  351:1 "x" btn=2
  360:1 "x" btn=2
  369:1 "x" btn=2
  378:1 "x" btn=2
  387:1 "x" btn=2
  396:1 "x" btn=2
  405:1 "x" btn=2
  414:1 "x" btn=2
  423:1 "x" btn=2
  432:1 "x" btn=2
  441:1 "x" btn=2
  450:1 "x" btn=2
  459:1 "x" btn=2
  468:1 "x" btn=2
  477:1 "x" btn=2
  486:1 "x" btn=2
  495:1 "x" btn=2
  504:1 "x" btn=2
  513:1 "x" btn=2
  522:1 "x" btn=2
  531:1 "x" btn=2
  540:1 "x" btn=2
  549:1 "x" btn=2
  558:1 "x" btn=2
  567:1 "x" btn=2
  576:1 "x" btn=2
  585:1 "x" btn=2
  594:1 "x" btn=2
  603:1 "x" btn=2
  612:1 "x" btn=2
  621:1 "x" btn=2
  630:1 "x" btn=2
  639:1 "x" btn=2
  648:1 "x" btn=2
  657:1 "x" btn=2
  666:1 "x" btn=2
  675:1 "x" btn=2
  684:1 "x" btn=2
  693:1 "x" btn=2
  702:1 "x" btn=2
  711:1 "x" btn=2
  720:1 "x" btn=2
  729:1 "x" btn=2
  738:1 "x" btn=2
  747:1 "x" btn=2
row 14 line 12+ x0=0 w=90
  0:1 "x" btn=3
  9:1 "x" btn=3
  18:1 "x" btn=3
  27:1 "x" btn=3
  36:1 "x" btn=3
  45:1 "x" btn=3
  54:1 "x" btn=3
  63:1 "x" btn=3
  72:1 "x" btn=3
  81:1 "x" btn=3
btn 0 row=12 x=0 w=72 gen=0 value=Int(0)
btn 1 row=12 x=72 w=108 gen=0 value=Int(1)
btn 2 row=13 x=0 w=756 gen=0 value=Int(2)
btn 3 row=14 x=0 w=90 gen=0 value=Int(2)
"#
    );
}

#[test]
fn tui_layout_japanese() {
    let frame = run_game("emuera.jp.config");
    k9::snapshot!(
        snapshot_of(&frame, Language::Japanese),
        r#"
row 0 line 0 x0=290 w=180
  0:2 "★"
  18:1 " "
  27:2 "텍"
  45:2 "스"
  63:2 "트"
  81:1 " "
  90:1 "U"
  99:1 "I"
  108:1 " "
  117:2 "데"
  135:2 "모"
  153:1 " "
  162:2 "★"
row 1 line 1 x0=0 w=756
  0:1 "-"
  9:1 "-"
  18:1 "-"
  27:1 "-"
  36:1 "-"
  45:1 "-"
  54:1 "-"
  63:1 "-"
  72:1 "-"
  81:1 "-"
  90:1 "-"
  99:1 "-"
  108:1 "-"
  117:1 "-"
  126:1 "-"
  135:1 "-"
  144:1 "-"
  153:1 "-"
  162:1 "-"
  171:1 "-"
  180:1 "-"
  189:1 "-"
  198:1 "-"
  207:1 "-"
  216:1 "-"
  225:1 "-"
  234:1 "-"
  243:1 "-"
  252:1 "-"
  261:1 "-"
  270:1 "-"
  279:1 "-"
  288:1 "-"
  297:1 "-"
  306:1 "-"
  315:1 "-"
  324:1 "-"
  333:1 "-"
  342:1 "-"
  351:1 "-"
  360:1 "-"
  369:1 "-"
  378:1 "-"
  387:1 "-"
  396:1 "-"
  405:1 "-"
  414:1 "-"
  423:1 "-"
  432:1 "-"
  441:1 "-"
  450:1 "-"
  459:1 "-"
  468:1 "-"
  477:1 "-"
  486:1 "-"
  495:1 "-"
  504:1 "-"
  513:1 "-"
  522:1 "-"
  531:1 "-"
  540:1 "-"
  549:1 "-"
  558:1 "-"
  567:1 "-"
  576:1 "-"
  585:1 "-"
  594:1 "-"
  603:1 "-"
  612:1 "-"
  621:1 "-"
  630:1 "-"
  639:1 "-"
  648:1 "-"
  657:1 "-"
  666:1 "-"
  675:1 "-"
  684:1 "-"
  693:1 "-"
  702:1 "-"
  711:1 "-"
  720:1 "-"
  729:1 "-"
  738:1 "-"
  747:1 "-"
row 2 line 2 x0=0 w=108
  0:2 "┏"
  18:2 "━"
  36:2 "━"
  54:2 "━"
  72:2 "━"
  90:2 "┓"
row 3 line 3 x0=0 w=108
  0:2 "┃"
  18:2 "가"
  36:1 "A"
  45:1 " "
  54:1 " "
  63:1 " "
  72:1 " "
  81:1 " "
  90:2 "┃"
row 4 line 4 x0=0 w=108
  0:2 "┗"
  18:2 "━"
  36:2 "━"
  54:2 "━"
  72:2 "━"
  90:2 "┛"
row 5 line 5 x0=0 w=756
  0:2 "━"
  18:2 "━"
  36:2 "━"
  54:2 "━"
  72:2 "━"
  90:2 "━"
  108:2 "━"
  126:2 "━"
  144:2 "━"
  162:2 "━"
  180:2 "━"
  198:2 "━"
  216:2 "━"
  234:2 "━"
  252:2 "━"
  270:2 "━"
  288:2 "━"
  306:2 "━"
  324:2 "━"
  342:2 "━"
  360:2 "━"
  378:2 "━"
  396:2 "━"
  414:2 "━"
  432:2 "━"
  450:2 "━"
  468:2 "━"
  486:2 "━"
  504:2 "━"
  522:2 "━"
  540:2 "━"
  558:2 "━"
  576:2 "━"
  594:2 "━"
  612:2 "━"
  630:2 "━"
  648:2 "━"
  666:2 "━"
  684:2 "━"
  702:2 "━"
  720:2 "━"
  738:2 "━"
row 6 line 6 x0=0 w=675
  0:1 " "
  9:1 " "
  18:1 " "
  27:1 " "
  36:1 " "
  45:1 " "
  54:1 " "
  63:1 " "
  72:1 " "
  81:1 " "
  90:1 " "
  99:1 " "
  108:1 " "
  117:1 " "
  126:1 " "
  135:1 " "
  144:1 " "
  153:1 " "
  162:1 " "
  171:1 " "
  180:1 " "
  189:2 "한"
  207:2 "글"
  225:1 " "
  234:1 " "
  243:1 " "
  252:1 " "
  261:1 " "
  270:1 " "
  279:1 " "
  288:1 " "
  297:1 " "
  306:1 " "
  315:1 " "
  324:1 " "
  333:1 " "
  342:1 " "
  351:1 " "
  360:1 " "
  369:1 " "
  378:1 " "
  387:1 " "
  396:1 " "
  405:1 " "
  414:1 " "
  423:1 "a"
  432:1 "b"
  441:1 "c"
  450:1 " "
  459:1 " "
  468:1 " "
  477:1 " "
  486:1 " "
  495:1 " "
  504:1 " "
  513:1 " "
  522:1 " "
  531:1 " "
  540:1 " "
  549:1 " "
  558:1 " "
  567:1 " "
  576:1 " "
  585:1 " "
  594:1 " "
  603:1 " "
  612:1 " "
  621:1 " "
  630:1 " "
  639:1 " "
  648:1 " "
  657:1 "▒"
  666:1 "▒"
row 7 line 7 x0=0 w=468
  0:2 "항"
  18:2 "목"
  36:1 " "
  45:1 " "
  54:1 " "
  63:1 " "
  72:1 " "
  81:1 " "
  90:1 " "
  99:1 " "
  108:1 " "
  117:1 " "
  126:1 " "
  135:1 " "
  144:1 " "
  153:1 " "
  162:1 " "
  171:1 " "
  180:1 " "
  189:1 " "
  198:1 " "
  207:1 " "
  216:1 " "
  225:1 " "
  234:1 "1"
  243:1 "2"
  252:1 "3"
  261:1 "4"
  270:1 "5"
  279:1 " "
  288:1 " "
  297:1 " "
  306:1 " "
  315:1 " "
  324:1 " "
  333:1 " "
  342:1 " "
  351:1 " "
  360:1 " "
  369:1 " "
  378:1 " "
  387:1 " "
  396:1 " "
  405:1 " "
  414:1 " "
  423:1 " "
  432:1 " "
  441:1 " "
  450:1 " "
  459:1 " "
row 8 line 8 x0=715 w=45
  0:1 "▒"
  9:1 "░"
  18:1 "█"
  27:1 "═"
  36:1 "║"
row 9 line 9 x0=0 w=117
  0:2 "굵" c=FF8000 s=B
  18:2 "은" c=FF8000 s=B
  36:1 " " c=FF8000 s=B
  45:2 "주" c=FF8000 s=B
  63:2 "황" c=FF8000 s=B
  81:2 "보"
  99:2 "통"
row 10 line 10 x0=0 w=18
  0:1 "a"
  9:1 "b"
row 11 line 10+ x0=0 w=18
  0:1 "c"
  9:1 "d"
row 12 line 11 x0=0 w=180
  0:1 "[" btn=0
  9:1 "0" btn=0
  18:1 "]" btn=0
  27:1 " " btn=0
  36:2 "시" btn=0
  54:2 "작" btn=0
  72:1 "[" btn=1
  81:1 "1" btn=1
  90:1 "]" btn=1
  99:1 " " btn=1
  108:2 "이" btn=1
  126:2 "어" btn=1
  144:2 "하" btn=1
  162:2 "기" btn=1
row 13 line 12 x0=0 w=756
  0:1 "[" btn=2
  9:1 "2" btn=2
  18:1 "]" btn=2
  27:1 " " btn=2
  36:1 "x" btn=2
  45:1 "x" btn=2
  54:1 "x" btn=2
  63:1 "x" btn=2
  72:1 "x" btn=2
  81:1 "x" btn=2
  90:1 "x" btn=2
  99:1 "x" btn=2
  108:1 "x" btn=2
  117:1 "x" btn=2
  126:1 "x" btn=2
  135:1 "x" btn=2
  144:1 "x" btn=2
  153:1 "x" btn=2
  162:1 "x" btn=2
  171:1 "x" btn=2
  180:1 "x" btn=2
  189:1 "x" btn=2
  198:1 "x" btn=2
  207:1 "x" btn=2
  216:1 "x" btn=2
  225:1 "x" btn=2
  234:1 "x" btn=2
  243:1 "x" btn=2
  252:1 "x" btn=2
  261:1 "x" btn=2
  270:1 "x" btn=2
  279:1 "x" btn=2
  288:1 "x" btn=2
  297:1 "x" btn=2
  306:1 "x" btn=2
  315:1 "x" btn=2
  324:1 "x" btn=2
  333:1 "x" btn=2
  342:1 "x" btn=2
  351:1 "x" btn=2
  360:1 "x" btn=2
  369:1 "x" btn=2
  378:1 "x" btn=2
  387:1 "x" btn=2
  396:1 "x" btn=2
  405:1 "x" btn=2
  414:1 "x" btn=2
  423:1 "x" btn=2
  432:1 "x" btn=2
  441:1 "x" btn=2
  450:1 "x" btn=2
  459:1 "x" btn=2
  468:1 "x" btn=2
  477:1 "x" btn=2
  486:1 "x" btn=2
  495:1 "x" btn=2
  504:1 "x" btn=2
  513:1 "x" btn=2
  522:1 "x" btn=2
  531:1 "x" btn=2
  540:1 "x" btn=2
  549:1 "x" btn=2
  558:1 "x" btn=2
  567:1 "x" btn=2
  576:1 "x" btn=2
  585:1 "x" btn=2
  594:1 "x" btn=2
  603:1 "x" btn=2
  612:1 "x" btn=2
  621:1 "x" btn=2
  630:1 "x" btn=2
  639:1 "x" btn=2
  648:1 "x" btn=2
  657:1 "x" btn=2
  666:1 "x" btn=2
  675:1 "x" btn=2
  684:1 "x" btn=2
  693:1 "x" btn=2
  702:1 "x" btn=2
  711:1 "x" btn=2
  720:1 "x" btn=2
  729:1 "x" btn=2
  738:1 "x" btn=2
  747:1 "x" btn=2
row 14 line 12+ x0=0 w=90
  0:1 "x" btn=3
  9:1 "x" btn=3
  18:1 "x" btn=3
  27:1 "x" btn=3
  36:1 "x" btn=3
  45:1 "x" btn=3
  54:1 "x" btn=3
  63:1 "x" btn=3
  72:1 "x" btn=3
  81:1 "x" btn=3
btn 0 row=12 x=0 w=72 gen=0 value=Int(0)
btn 1 row=12 x=72 w=108 gen=0 value=Int(1)
btn 2 row=13 x=0 w=756 gen=0 value=Int(2)
btn 3 row=14 x=0 w=90 gen=0 value=Int(2)
"#
    );
}

/// GPU: render the KOREAN frame with the real font chain (system fonts, so
/// Hangul is legible where a CJK font is installed) to
/// `target/tmp/tui-korean.png` for eyeballing.
#[test]
fn tui_png_korean() {
    let _lock = gpu_lock();
    let Some((device, queue)) = gpu_device() else {
        return; // gpu_device printed SKIP (or panicked under ERARS_REQUIRE_GPU=1)
    };
    let frame = run_game("emuera.config");
    let cfg = FontConfig {
        family: "",
        game_dir: Path::new(GAME),
        extra_dir: std::env::var_os("ERARS_FONT_DIR").map(PathBuf::from),
        lang: Language::Korean,
    };
    let mut chain = FontChain::new(&cfg);
    let primary_id = chain.primary();
    let primary = chain.font(primary_id);
    let m = CellMetrics::from_primary(&primary, 18, 19, 1.0);
    let mut shaper = Shaper::new(chain, WidthTable::new(Language::Korean.encoding()), m);
    let img = render_frame_on(
        &device,
        &queue,
        &mut shaper,
        &frame,
        760,
        480,
        Some(""),
        None,
        true,
    )
    .expect("headless render");
    assert_eq!((img.width, img.height), (760, 480));
    assert!(
        img.rgba.chunks_exact(4).any(|p| p[0] > 0 || p[1] > 0 || p[2] > 0),
        "nothing was drawn"
    );
    let out = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("tui-korean.png");
    write_png(out.to_str().unwrap(), &img).unwrap();
    eprintln!("wrote {}", out.display());
}
