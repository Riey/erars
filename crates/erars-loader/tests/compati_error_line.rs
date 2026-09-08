//! `解釈不能な行があっても実行する` (`emuera.config` `CompatiErrorLine`, default
//! `NO` = `false`). Real Emuera's default refuses to leave the title screen
//! when any ERB line failed to parse
//! (`GameProc/Process.SystemProc.cs:152-160`); erars was previously
//! hardcoded to the *non-default* `YES` behaviour — it printed the parse
//! diagnostics and kept loading regardless
//! (`crates/erars-loader/src/lib.rs`, `run_script`'s end-of-load path).
//! `docs/research/2026-09-06-language-feature-work.md` §6.2 catalogued this
//! as a residual: `compati_error_line`.

use erars_compiler::EraConfig;
use erars_loader::run_script;
use erars_vm::NullSystemFunctions;

/// A one-function game in its own scratch directory, so `run_script` has a
/// real `ERB/` to glob. Not a `tempfile::TempDir` — this crate has no
/// `tempfile` dev-dependency, and one throwaway directory per test process
/// does not need one. Mirrors `tests/display_report.rs`'s fixture.
struct ScratchDir(std::path::PathBuf);

impl Drop for ScratchDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn fixture_dir_with_erb(name: &str, erb: &str) -> ScratchDir {
    let dir = std::env::temp_dir()
        .join(format!("erars-compati-error-line-test-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("ERB")).unwrap();
    std::fs::write(dir.join("ERB/MAIN.ERB"), erb).unwrap();
    ScratchDir(dir)
}

/// An unbalanced expression is not a recoverable statement — the parser
/// reports it as an "E2000" `Diagnostic::error()`
/// (`crates/erars-loader/src/lib.rs:711`) and moves on to the next line,
/// exactly the "解釈不能な行" (unparseable line) real Emuera's `ErbLoader`
/// sets `noError = false` for (`GameProc/ErbLoader.cs:428`).
const BAD_LINE_ERB: &str = "@SYSTEM_TITLE\nA = (1 + 2\nPRINTL ok\n";

#[test]
fn compati_error_line_off_aborts_the_load_on_an_unparseable_line() {
    let dir = fixture_dir_with_erb("off", BAD_LINE_ERB);
    let config = EraConfig::default();
    assert!(!config.compati_error_line, "CompatiErrorLine defaults to NO");
    let result = run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false);
    let err = match result {
        Ok(_) => panic!("an unparseable line must abort the load when CompatiErrorLine is off"),
        Err(err) => err,
    };
    let msg = err.to_string();
    assert!(
        msg.contains("解釈不可能な行"),
        "expected Emuera's own title-screen abort wording, got: {msg:?}"
    );
}

#[test]
fn compati_error_line_on_keeps_loading_past_an_unparseable_line() {
    let dir = fixture_dir_with_erb("on", BAD_LINE_ERB);
    let mut config = EraConfig::default();
    config.compati_error_line = true;
    let (_vm, _ctx, _tx) =
        run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false)
            .expect("CompatiErrorLine:YES must let the load through the bad line");
}

/// A game with no unparseable lines is unaffected either way — the abort
/// path must never fire on a clean load.
#[test]
fn compati_error_line_off_does_not_abort_a_clean_load() {
    let dir = fixture_dir_with_erb("clean", "@SYSTEM_TITLE\nPRINTL a\n");
    let config = EraConfig::default();
    assert!(!config.compati_error_line);
    let (_vm, _ctx, _tx) =
        run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false)
            .expect("a clean load must succeed regardless of CompatiErrorLine");
}

/// A refused function registration is the *other* real-Emuera trigger for
/// `noError = false` (`GameProc/ErbLoader.cs:366`, the label-rejection
/// branch), not just a parser-level "E2000" line. `@TOSTR` collides with an
/// in-expression function name, which `registration_diagnostics` refuses
/// outright when `AllowFunctionOverloading:NO`
/// (`crates/erars-loader/src/lib.rs`'s `registration_diagnostics`, the
/// `warn_overloading && is_system_method && !allow_function_overloading`
/// arm) — no "E2000" diagnostic is involved, so this exercises the
/// `had_rejected_registration` path specifically.
const REJECTED_REGISTRATION_ERB: &str = "@SYSTEM_TITLE\nPRINTL a\n\n@TOSTR\nPRINTL b\n";

#[test]
fn compati_error_line_off_aborts_on_a_rejected_function_registration() {
    let dir = fixture_dir_with_erb("reg-off", REJECTED_REGISTRATION_ERB);
    let mut config = EraConfig::default();
    config.allow_function_overloading = false;
    assert!(!config.compati_error_line, "CompatiErrorLine defaults to NO");
    let result = run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false);
    match result {
        Ok(_) => panic!("a refused function registration must abort the load when CompatiErrorLine is off"),
        Err(err) => assert!(
            err.to_string().contains("解釈不可能な行"),
            "expected Emuera's own title-screen abort wording, got: {err}"
        ),
    }
}

#[test]
fn compati_error_line_on_keeps_loading_past_a_rejected_function_registration() {
    let dir = fixture_dir_with_erb("reg-on", REJECTED_REGISTRATION_ERB);
    let mut config = EraConfig::default();
    config.allow_function_overloading = false;
    config.compati_error_line = true;
    let (_vm, _ctx, _tx) =
        run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false)
            .expect("CompatiErrorLine:YES must let the load through the refused @TOSTR definition");
}
