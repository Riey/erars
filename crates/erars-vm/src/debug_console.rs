//! The `@`-prefixed debug console commands.
//!
//! Emuera runs these in its front end only because `EmueraConsole` owns the
//! input loop: `PressEnterKey` diverts a line starting with `@` to
//! `doSystemCommand` *before* the pending `InputRequest` is consumed and
//! returns without answering it, so the very same prompt comes back
//! (`GameView/EmueraConsole.cs:1103-1110`).
//!
//! In erars the VM owns the input loop, so the equivalent interception lives
//! in [`VmContext::input_redraw`](crate::VmContext::input_redraw): the answer
//! is classified by [`classify`], run by [`run`], and the request is re-issued.
//! Every front end therefore gets the commands for free, and the semantics
//! stay in the engine rather than being reimplemented per surface.

use std::fmt::Write as _;

use anyhow::{ensure, Context as _, Result};
use erars_compiler::EraConfigKey;
use erars_ui::VirtualConsole;
use strum::IntoEnumIterator;

use crate::context::VmContext;
use crate::variable::StrKeyLike as _;

/// A command `doSystemCommand` recognises by name
/// (`GameView/EmueraConsole.cs:1343-1377`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugCommand {
    /// `@REBOOT` — `window.Reboot()` (`Forms/MainWindow.cs:807-812`).
    Reboot,
    /// `@OUTPUT` / `@OUTPUTLOG` — `OutputSystemLog(WorkingDir + "emuera.log")`
    /// (`GameView/EmueraConsole.cs:1349-1355`).
    OutputLog,
    /// `@EXIT` / `@QUIT` — `window.Close()`
    /// (`GameView/EmueraConsole.cs:1357-1361`).
    Exit,
    /// `@CONFIG` — `window.ShowConfigDialog()` (`Forms/MainWindow.cs:841-849`).
    Config,
    /// `@DEBUG` — `OpenDebugDialog()` (`GameView/EmueraConsole.cs:1814-1835`).
    Debug,
}

/// What one answered input line is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugLine<'s> {
    /// Not a console command: an ordinary answer to the pending request.
    Answer,
    /// `@` with nothing after it. Emuera returns without printing anything
    /// (`GameView/EmueraConsole.cs:1340-1341`), leaving the request pending.
    Empty,
    /// One of the named commands.
    Known(DebugCommand),
    /// The `else` branch: an arbitrary ERB fragment for `DebugCommand`
    /// (`GameView/EmueraConsole.cs:1377-1388`), gated on
    /// `デバッグコマンドを使用する`.
    Other(&'s str),
}

/// Split an answered input line the way `PressEnterKey` and the head of
/// `doSystemCommand` do.
///
/// `one_input` is Emuera's `inputReq.OneInput`: a ONEINPUT-family request
/// takes the keystroke literally and is never diverted
/// (`GameView/EmueraConsole.cs:1107`).
///
/// The comparisons are `Config.SCVariable`, which is `OrdinalIgnoreCase` when
/// `大文字小文字の違いを無視する` is on and `Ordinal` when it is off
/// (`Config/Config.cs:32-45`). Every command name is ASCII, so an ordinal
/// case-insensitive compare is exactly `eq_ignore_ascii_case`. The match is
/// whole-string, so `@REBOOT NOW` is *not* `REBOOT` — it falls through to
/// [`DebugLine::Other`].
pub fn classify(line: &str, one_input: bool, ignore_case: bool) -> DebugLine<'_> {
    if one_input || !line.starts_with('@') {
        return DebugLine::Answer;
    }

    // `command.Substring(1)` (`GameView/EmueraConsole.cs:1339`).
    let com = &line['@'.len_utf8()..];

    if com.is_empty() {
        return DebugLine::Empty;
    }

    let is = |name: &str| {
        if ignore_case {
            com.eq_ignore_ascii_case(name)
        } else {
            com == name
        }
    };

    // The C#'s order, which only matters for the `OUTPUT`/`OUTPUTLOG` and
    // `QUIT`/`EXIT` pairs sharing one branch each.
    if is("REBOOT") {
        DebugLine::Known(DebugCommand::Reboot)
    } else if is("OUTPUT") || is("OUTPUTLOG") {
        DebugLine::Known(DebugCommand::OutputLog)
    } else if is("QUIT") || is("EXIT") {
        DebugLine::Known(DebugCommand::Exit)
    } else if is("CONFIG") {
        DebugLine::Known(DebugCommand::Config)
    } else if is("DEBUG") {
        DebugLine::Known(DebugCommand::Debug)
    } else {
        DebugLine::Other(com)
    }
}

/// `@EXIT` and `@REBOOT` end the run, which no input path can express by
/// returning a value: the answer is consumed several frames below
/// [`crate::TerminalVm::start`]. Both are raised as this error instead, which
/// `start` recognises and turns into a clean [`crate::Workflow::Exit`] rather
/// than a reported VM error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DebugConsoleQuit {
    /// `@REBOOT` set `Program.Reboot` before closing the window
    /// (`Forms/MainWindow.cs:810-811`), asking the outer loop to build a fresh
    /// engine. Reported by [`VmContext::reboot_requested`].
    pub reboot: bool,
}

impl std::fmt::Display for DebugConsoleQuit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.reboot {
            f.write_str("@REBOOT")
        } else {
            f.write_str("@EXIT")
        }
    }
}

impl std::error::Error for DebugConsoleQuit {}

/// 「タイマー系命令の待ち時間中はコマンドを入力できません」
/// (`_Library/EvilMask/Lang.cs:984`).
const TIMER_WAIT: &str = "타이머 계열 명령의 대기 시간 중에는 커맨드를 입력할 수 없습니다";
/// 「デバッグウインドウは-Debug引数付きで起動したときのみ使えます」
/// (`_Library/EvilMask/Lang.cs:986`).
const NO_DEBUG_WINDOW: &str = "디버그 창은 --debug 인수를 붙여 실행했을 때에만 사용할 수 있습니다";
/// 「デバッグコマンドを使用できない設定になっています」
/// (`_Library/EvilMask/Lang.cs:987`).
const NO_DEBUG_COMMAND: &str = "디버그 커맨드를 사용할 수 없는 설정입니다";

/// Run one classified line, having already decided it is not an answer.
///
/// The order of effects is `doSystemCommand`'s
/// (`GameView/EmueraConsole.cs:1321-1390`):
///
/// 1. a timer is running → refuse, print nothing else;
/// 2. echo the command line to the console;
/// 3. dispatch, or refuse on the relevant gate.
///
/// Emuera's second guard, `if (IsInProcess)` (`:1330`), cannot arise here.
/// It is true while the script is running or re-entered
/// (`GameView/EmueraConsole.cs:301-312`), and erars' VM is single-threaded and
/// synchronous: reaching this function *means* the VM is parked in an input
/// request, so no script is scanning and there is no re-entrant debug command.
/// The same reasoning Emuera's own always-true `IsActive` gets.
pub fn run(line: DebugLine<'_>, tx: &mut VirtualConsole, ctx: &mut VmContext) -> Result<()> {
    let cmd = match line {
        DebugLine::Answer => return Ok(()),
        // `if (com.Length == 0) return;` — before the echo, so `@` alone
        // leaves no trace at all.
        DebugLine::Empty => return Ok(()),
        DebugLine::Known(cmd) => Some(cmd),
        DebugLine::Other(_) => None,
    };

    match cmd {
        Some(DebugCommand::Reboot) => Err(DebugConsoleQuit { reboot: true }.into()),
        Some(DebugCommand::Exit) => Err(DebugConsoleQuit { reboot: false }.into()),
        Some(DebugCommand::OutputLog) => {
            // `OutputSystemLog(Program.WorkingDir + "emuera.log")`: the
            // argument is fixed, so this is `OUTPUTLOG` with no argument.
            output_log(tx, ctx, "")
        }
        Some(DebugCommand::Config) => {
            show_config(tx, ctx);
            Ok(())
        }
        Some(DebugCommand::Debug) => {
            // `OpenDebugDialog` is gated twice on `Program.DebugMode`: once by
            // `doSystemCommand` with a message (`:1367-1373`) and once
            // silently at the top of the method itself (`:1816-1817`).
            if !ctx.debug_mode {
                tx.print_line(NO_DEBUG_WINDOW.into());
                return Ok(());
            }
            show_debug(tx, ctx);
            Ok(())
        }
        None => {
            // The generic fall-through. erars implements the gate and refuses
            // exactly as Emuera does when the gate is shut; it does not
            // implement `DebugCommand`'s in-memory ERB evaluator, so an
            // enabled gate reports that rather than pretending to run the
            // line. See §5.16 of
            // `docs/research/2026-09-03-emuera-command-gap.md`.
            if !ctx.config.use_debug_command {
                tx.print_line(NO_DEBUG_COMMAND.into());
            } else {
                tx.print_line(
                    "디버그 커맨드로 ERB 식을 실행하는 기능은 구현되어 있지 않습니다".into(),
                );
            }
            Ok(())
        }
    }
}

/// Emuera refuses a console command outright while a timer runs
/// (`GameView/EmueraConsole.cs:1323-1329`). `timer.Enabled` is set for the
/// duration of a `TINPUT`-family wait (`presetTimer`, `:575-580`), which is
/// exactly the requests carrying a [`erars_ui::Timeout`].
pub fn refuse_for_timer(tx: &mut VirtualConsole) {
    tx.print_line(TIMER_WAIT.into());
    // `PrintError("")` — a blank line so the timer's own repaint cannot eat
    // the message (`:1326`).
    tx.print_line(String::new());
}

/// `EmueraConsole.OutputLog` (`GameView/EmueraConsole.Print.cs:683-712`) and
/// `outputLog` (`:658-680`), shared by the `OUTPUTLOG` instruction and
/// `@OUTPUT`.
///
/// An empty name means `emuera.log`; the path is resolved under the game
/// directory and refused if it contains `../` or escapes that directory. The
/// file is rewritten from scratch with every completed console line, as
/// UTF-16LE with a BOM and CRLF ends (`StreamWriter(path, false,
/// Encoding.Unicode)`).
///
/// `@OUTPUT` reaches `OutputSystemLog` (`:714-736`) instead, which is the same
/// method minus the `../` test — unobservably so, since its argument is the
/// fixed `WorkingDir + "emuera.log"`.
pub fn output_log(tx: &mut VirtualConsole, ctx: &mut VmContext, name: &str) -> Result<()> {
    // Emuera's `WorkingDir` is the game directory (`Program.cs:57-63`), and
    // that is what `sav_path` starts from: `<game>/sav` when
    // `セーブデータをsavフォルダ内に作成する` is on, `<game>` itself when it is
    // off (`crates/erars-loader/src/lib.rs:101-108`, `Config/Config.cs:228-234`).
    // Climbing unconditionally would put the log *outside* the game with the
    // key at its Emuera default of `NO`.
    let work_dir = if ctx.config.use_save_folder {
        ctx.sav_dir.parent().unwrap_or(&ctx.sav_dir)
    } else {
        ctx.sav_dir.as_path()
    };
    let path = work_dir.join(if name.is_empty() { "emuera.log" } else { name });

    ensure!(
        !name.contains("../") && path.starts_with(work_dir),
        "OUTPUTLOG: 게임 디렉터리 밖으로는 출력할 수 없습니다: {name}"
    );

    let mut bytes = Vec::new();
    // UTF-16LE byte-order mark, as written by `Encoding.Unicode`.
    bytes.extend_from_slice(&[0xFF, 0xFE]);
    let mut buf = String::new();
    for line in tx.lines_from(0).iter() {
        buf.clear();
        let _ = write!(buf, "{line}\r\n");
        bytes.extend(buf.encode_utf16().flat_map(u16::to_le_bytes));
    }
    std::fs::write(&path, &bytes)
        .with_context(|| format!("OUTPUTLOG 파일을 쓰지 못했습니다: {}", path.display()))?;

    // `LogFileHasBeenCreated` (`_Library/EvilMask/Lang.cs:1284`) takes the
    // resolved path minus the working directory (`:704`), so the default
    // argument still reports `emuera.log`.
    let shown = path.strip_prefix(work_dir).unwrap_or(&path).display();
    tx.print_line(format!("※※※ログファイルを{shown}に出力しました※※※"));

    Ok(())
}

/// DELIBERATE: `@CONFIG` opens a Windows Forms dialog
/// (`Forms/MainWindow.cs:841-849`), whose content is one widget per
/// `ConfigCode` showing that item's name and current value
/// (`Forms/ConfigDialog.cs:315-345`). erars has no dialog, so the same content
/// is written to the console as `name:value` lines — the names being Emuera's
/// own, because `EraConfigKey`'s `Display` *is* the `emuera.config` key.
///
/// Two divergences follow and are recorded in §5.16 of
/// `docs/research/2026-09-03-emuera-command-gap.md`: the listing is
/// read-only, where the dialog can edit and save (and reboot on
/// `SaveReboot`, `Forms/MainWindow.cs:850-855`), and it covers every key
/// erars knows rather than only those with a widget — the same superset
/// `GETCONFIG` already answers for.
fn show_config(tx: &mut VirtualConsole, ctx: &VmContext) {
    for line in config_lines(&ctx.config, &ctx.header_info.replace) {
        tx.print_line(line);
    }
}

/// One `name:value` line per config key, in `EraConfigKey` order.
fn config_lines<'c>(
    config: &'c erars_compiler::EraConfig,
    replace: &'c erars_compiler::ReplaceInfo,
) -> impl Iterator<Item = String> + 'c {
    EraConfigKey::iter().map(move |key| match config.get_config(key, replace) {
        erars_ast::Value::Int(i) => format!("{key}:{i}"),
        erars_ast::Value::String(s) => format!("{key}:{s}"),
    })
}

/// DELIBERATE: `@DEBUG` opens the three-tab debug dialog
/// (`GameView/EmueraConsole.cs:1814-1835`, `Forms/DebugDialog.cs`). erars has
/// no dialog, so the two tabs whose content the engine owns are written to the
/// console instead:
///
/// * **Stack trace** — `GetDebugTraceLog` (`GameView/EmueraConsole.cs:1788-1812`),
///   reproduced label for label: the line being executed, then the call stack
///   innermost first.
/// * **Console** — `DebugConsoleLog` (`:1783`), the `DEBUGPRINT` buffer, which
///   erars keeps in [`VirtualConsole::debug_lines`].
///
/// The **variable watch** tab is not reproduced: it evaluates expressions the
/// user typed into the dialog's grid, which needs the interactive surface
/// erars does not have. Recorded in §5.16 of
/// `docs/research/2026-09-03-emuera-command-gap.md`.
fn show_debug(tx: &mut VirtualConsole, ctx: &VmContext) {
    // 「*実行中の行」 (`_Library/EvilMask/Lang.cs:1276`).
    tx.print_line("*実行中の行".into());

    match ctx.call_stack().last() {
        Some(frame) => {
            // 「ファイル名:{0}」 and 「行番号:{0} 関数名:{1}」 (`:1279-1280`).
            tx.print_line(format!(
                "ファイル名:{file}",
                file = frame.file_path.resolve_key(&ctx.var)
            ));
            tx.print_line(format!(
                "行番号:{line} 関数名:{func}",
                line = frame.script_position.line,
                func = frame.func_name.resolve_key(&ctx.var),
            ));
        }
        None => {
            // 「ファイル名:なし」/「行番号:なし 関数名:なし」 (`:1277-1278`).
            tx.print_line("ファイル名:なし".into());
            tx.print_line("行番号:なし 関数名:なし".into());
        }
    }
    tx.print_line(String::new());

    // 「*スタックトレース」 (`:1281`). `GetDebugTraceLog` walks
    // `dTraceLogList` backwards, so the innermost frame prints first.
    tx.print_line("*スタックトレース".into());
    // DELIBERATE: the *line number* is erars', not Emuera's. Emuera pushes a
    // trace entry when the call is made and fills it with the **callee's**
    // name, file and declaration position (`GameProc/Process.State.cs:437`,
    // `:459-461`), so an entry never moves again. erars has no separate trace
    // list: it reads the live call stack, whose every frame carries its own
    // current position because `update_position` writes only the innermost
    // one (`crates/erars-vm/src/context.rs:212-216`). An outer frame
    // therefore reports where it *is* — the call site — where Emuera reports
    // where the callee *began*. The frames and their order are identical;
    // only that number differs. §5.16 of
    // `docs/research/2026-09-03-emuera-command-gap.md`.
    for frame in ctx.call_stack().iter().rev() {
        // 「CALL :@{0}:{1}:{2}行目」 (`:1262`), the format
        // `DebugAddTraceLog` is fed at `GameProc/Process.State.cs:437`.
        tx.print_line(format!(
            "CALL :@{func}:{file}:{line}行目",
            func = frame.func_name.resolve_key(&ctx.var),
            file = frame.file_path.resolve_key(&ctx.var),
            line = frame.script_position.line,
        ));
    }

    for line in tx.debug_lines().to_vec() {
        tx.print_line(line);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classify_matches_whole_names_case_insensitively() {
        // `com.Equals("REBOOT", Config.SCVariable)` with `OrdinalIgnoreCase`.
        assert_eq!(
            classify("@REBOOT", false, true),
            DebugLine::Known(DebugCommand::Reboot)
        );
        assert_eq!(
            classify("@reboot", false, true),
            DebugLine::Known(DebugCommand::Reboot)
        );
        // With `大文字小文字の違いを無視する` off the compare is `Ordinal`.
        assert_eq!(classify("@reboot", false, false), DebugLine::Other("reboot"));

        // Both spellings of the two aliased branches.
        assert_eq!(
            classify("@OUTPUT", false, true),
            DebugLine::Known(DebugCommand::OutputLog)
        );
        assert_eq!(
            classify("@OUTPUTLOG", false, true),
            DebugLine::Known(DebugCommand::OutputLog)
        );
        assert_eq!(
            classify("@EXIT", false, true),
            DebugLine::Known(DebugCommand::Exit)
        );
        assert_eq!(
            classify("@QUIT", false, true),
            DebugLine::Known(DebugCommand::Exit)
        );
        assert_eq!(
            classify("@CONFIG", false, true),
            DebugLine::Known(DebugCommand::Config)
        );
        assert_eq!(
            classify("@DEBUG", false, true),
            DebugLine::Known(DebugCommand::Debug)
        );
    }

    #[test]
    fn classify_rejects_partial_matches_and_plain_answers() {
        // Whole-string compare: an argument makes it a generic debug command.
        assert_eq!(
            classify("@REBOOT NOW", false, true),
            DebugLine::Other("REBOOT NOW")
        );
        assert_eq!(classify("@REBOO", false, true), DebugLine::Other("REBOO"));
        // `@` alone: silent no-op, request still pending.
        assert_eq!(classify("@", false, true), DebugLine::Empty);
        // No `@`: an ordinary answer.
        assert_eq!(classify("REBOOT", false, true), DebugLine::Answer);
        assert_eq!(classify("", false, true), DebugLine::Answer);
        // A ONEINPUT-family request takes the keystroke literally.
        assert_eq!(classify("@REBOOT", true, true), DebugLine::Answer);
    }

    /// The config listing is the dialog's content, so every key erars knows
    /// has to appear under its `emuera.config` name with its current value —
    /// `EraConfigKey`'s `Display` *is* that name.
    #[test]
    fn config_listing_names_every_key_with_its_value() {
        let config = crate::EraConfig {
            printc_count: 5,
            ..Default::default()
        };
        let replace = erars_compiler::ReplaceInfo::default();
        let lines: Vec<String> = config_lines(&config, &replace).collect();

        assert_eq!(lines.len(), EraConfigKey::iter().count());
        // A key whose value the test set, an untouched boolean reported as
        // 1/0 the way `GETCONFIG` reports it, a string-valued key, and one of
        // the keys served out of `_Replace.csv` rather than the config.
        assert!(lines.contains(&"PRINTCを並べる数:5".to_owned()), "{lines:?}");
        assert!(
            lines.contains(&"デバッグコマンドを使用する:0".to_owned()),
            "{lines:?}"
        );
        assert!(lines.iter().any(|l| l.starts_with("フォント名:")), "{lines:?}");
        assert!(
            lines.iter().any(|l| l.starts_with("お金の単位:")),
            "{lines:?}"
        );
    }
}
