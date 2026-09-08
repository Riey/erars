//! `ロード時にレポートを表示する` (`emuera.config` `DisplayReport`, default
//! `NO`): OFF prints `_replace.csv`'s `起動時簡略表示` message
//! (`"Now Loading..."` by default); ON prints a load summary instead
//! (`crates/erars-loader/src/lib.rs`, near `run_script`'s end-of-load path).
//! `docs/research/emuera-wiki/replace.md:22-23` documents the two as
//! mutually exclusive.

use erars_compiler::EraConfig;
use erars_loader::run_script;
use erars_vm::NullSystemFunctions;

/// A one-function game in its own scratch directory, so `run_script` has a
/// real `ERB/` to glob. Not a `tempfile::TempDir` — this crate has no
/// `tempfile` dev-dependency, and one throwaway directory per test process
/// does not need one.
struct ScratchDir(std::path::PathBuf);

impl Drop for ScratchDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn fixture_dir(name: &str) -> ScratchDir {
    write_fixture(name, "@SYSTEM_TITLE\nPRINTL a\n")
}

fn write_fixture(name: &str, erb: &str) -> ScratchDir {
    let dir = std::env::temp_dir()
        .join(format!("erars-display-report-test-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("ERB")).unwrap();
    std::fs::write(dir.join("ERB/MAIN.ERB"), erb).unwrap();
    ScratchDir(dir)
}

fn console_text(tx: &erars_ui::VirtualConsole) -> String {
    let mut out: Vec<String> = tx.lines_from(0).iter().map(|l| l.to_string()).collect();
    out.push(tx.last_line.to_string());
    out.join("\n")
}

#[test]
fn display_report_off_shows_the_replace_csv_loading_message() {
    let dir = fixture_dir("off");
    let config = EraConfig::default();
    assert!(!config.display_report, "DisplayReport defaults to NO");
    let (_vm, _ctx, tx) =
        run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false)
            .expect("compile failed");
    let text = console_text(&tx);
    assert!(text.contains("Now Loading..."), "expected the default loading message, got: {text:?}");
}

#[test]
fn display_report_on_shows_a_load_summary_instead() {
    let dir = fixture_dir("on");
    let mut config = EraConfig::default();
    config.display_report = true;
    let (_vm, _ctx, tx) =
        run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false)
            .expect("compile failed");
    let text = console_text(&tx);
    assert!(!text.contains("Now Loading..."), "the loading message must not appear when the report does, got: {text:?}");
    assert!(text.contains("함수"), "expected a function-count report line, got: {text:?}");
}

/// The report's third figure is Emuera's `usedLabelCount`: how many functions
/// the load-time call graph reached (`非コメント行数:{0}, 全関数合計:{1},
/// 被呼出関数合計:{2}`, `GameProc/ErbLoader.cs:753`). Four functions are
/// defined; `SYSTEM_TITLE` calls `A`, `A` calls `B`, and `DEAD` is reached by
/// nothing, so three were called. This is the end-to-end proof that the
/// compiler records constant `CALL` targets
/// (`crates/erars-compiler/src/compiler.rs`, `CompiledFunction::calls`) and
/// that the loader walks them.
#[test]
fn display_report_counts_the_called_functions() {
    let dir = write_fixture(
        "called",
        "@SYSTEM_TITLE\nCALL A\n\n@A\nCALL B\n\n@B\nRETURN 0\n\n@DEAD\nRETURN 0\n",
    );
    let mut config = EraConfig::default();
    config.display_report = true;
    let (_vm, _ctx, tx) =
        run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false)
            .expect("compile failed");
    let text = console_text(&tx);
    assert!(text.contains("전체 함수 수:4"), "got: {text:?}");
    assert!(text.contains("피호출 함수 수:3"), "got: {text:?}");
}

/// A computed target (`CALLFORM`) sets Emuera's `useCallForm`, which abandons
/// the uncalled-function *check* (`GameProc/ErbLoader.cs:667-676`) but does
/// **not** raise `usedLabelCount`: that counter is only incremented inside the
/// depth walk (`:645`), and the `useCallForm` branch never touches it. So the
/// report still shows only the reachable count. Proves the compiler
/// distinguishes a literal target from a form one — with the form target
/// treated as a literal, `A0` would be reached and the count would be 2.
#[test]
fn display_report_counts_every_function_when_a_target_is_computed() {
    let dir = write_fixture(
        "callform",
        "@SYSTEM_TITLE\nCALLFORM A{0}\n\n@A0\nRETURN 0\n\n@DEAD\nRETURN 0\n",
    );
    let mut config = EraConfig::default();
    config.display_report = true;
    let (_vm, _ctx, tx) =
        run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false)
            .expect("compile failed");
    let text = console_text(&tx);
    assert!(text.contains("전체 함수 수:3"), "got: {text:?}");
    assert!(text.contains("피호출 함수 수:1"), "got: {text:?}");
}
