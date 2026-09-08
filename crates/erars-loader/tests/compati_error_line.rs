//! `解釈不能な行があっても実行する` (`emuera.config` `CompatiErrorLine`, default
//! `NO` = `false`). Real Emuera refuses to leave the title screen when any
//! ERB line *failed to parse at the shape level* and the flag is off
//! (`GameProc/Process.SystemProc.cs:152-160`); erars was previously hardcoded
//! to the *non-default* `YES` behaviour — it printed the parse diagnostics and
//! kept loading regardless (`crates/erars-loader/src/lib.rs`, `run_script`'s
//! end-of-load path). `docs/research/2026-09-06-language-feature-work.md` §6.2
//! catalogued this as a residual: `compati_error_line`.
//!
//! The abort must fire only for the *line-shape* class — the four
//! `ErbLoader.noError = false` sites (`#` at `:355`, `@` at `:368`, label/`$`
//! at `:407`, statement shape at `:428`). An *argument*-class failure (an
//! otherwise well-formed statement whose argument expression did not parse)
//! must never abort: Emuera defers an argument's reduction behind
//! `ロード時に引数を解析する` (`ErbLoader.cs:876`) and the argument builder's
//! failure path marks the line `IsError` — a throw-if-reached marker — without
//! touching `noError`, so the game boots either way. eramegaten ships six such
//! lines (its Korean-translation damage), which is why this classification is
//! a live requirement and not an abstraction.
//!
//! The classifying predicate in `crates/erars-loader/src/lib.rs` enumerates
//! the line-shape set (closed: exactly the four sites above) and treats
//! everything else as argument-class — not the reverse. A first version
//! enumerated the argument-class messages instead and aborted on anything
//! unlisted; since that set is open (it grows with every new argument
//! validator `erars-compiler` gains), every message this file didn't yet
//! know about became a silent new false abort. `ALIGNMENT`/`BEGIN`'s
//! `Invalid alignment`, `CALLEVENT`'s target check, `TRYGOTOLIST`'s argument
//! check, and `system_ignore_string_set`'s
//! `文字列代入は禁止されています` are exactly that: each aborted under the
//! first version despite being unambiguously argument-class, and each gets
//! its own regression test below.

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

/// A genuinely unrecognisable line — no statement, no `@`, no `#` — that the
/// lexer reports as `[lexer] Unknown line`. This is the *line-shape* class:
/// the same thing Emuera's `ParseLine` returns as an `InvalidLine` and clears
/// `noError` for (`GameProc/ErbLoader.cs:428`). It never looks like a
/// statement with a broken argument, so it must abort with the flag off.
const GARBAGE_LINE_ERB: &str = "@SYSTEM_TITLE\nQUACK QUACK QUACK\nPRINTL ok\n";

#[test]
fn compati_error_line_off_aborts_the_load_on_an_unrecognisable_line() {
    let dir = fixture_dir_with_erb("garbage-off", GARBAGE_LINE_ERB);
    let config = EraConfig::default();
    assert!(!config.compati_error_line, "CompatiErrorLine defaults to NO");
    let result =
        run_script(dir.0.to_str().unwrap(), Box::new(NullSystemFunctions), config, false, false, false);
    let err = match result {
        Ok(_) => panic!("an unrecognisable line must abort the load when CompatiErrorLine is off"),
        Err(err) => err,
    };
    let msg = err.to_string();
    assert!(
        msg.contains("解釈不可能な行"),
        "expected Emuera's own title-screen abort wording, got: {msg:?}"
    );
}

#[test]
fn compati_error_line_on_keeps_loading_past_an_unrecognisable_line() {
    let dir = fixture_dir_with_erb("garbage-on", GARBAGE_LINE_ERB);
    let mut config = EraConfig::default();
    config.compati_error_line = true;
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("CompatiErrorLine:YES must let the load through the unrecognisable line");
}

/// An *argument*-class failure: the statement shape (`PRINTFORMW`) is fine,
/// only its FORM expression is malformed (`hel%lo` — an unclosed `%`). This is
/// the eramegaten regression: the corpus's six Korean-translation FORM lines
/// fail exactly this way (`Expression parsing failed: expected '%' at end of
/// line`), and under its own shipped config (`ロード時に引数を解析する:NO`,
/// `解釈不可能な行があっても実行する:NO`) Emuera boots it because an argument
/// failure never touches `noError` (`ErbLoader.cs:876` gates argument
/// reduction; `ArgumentBuilder.assignwarn`/`warn` marks `IsError`, never
/// `noError`). erars recovers the line to a `THROW` stand-in and loads —
/// loading here is the acceptance criterion for the class split.
const MALFORMED_FORM_ERB: &str = "@SYSTEM_TITLE\nPRINTFORMW hel%lo\nPRINTL ok\n";

/// `ALIGNMENT`/`BEGIN` share `Invalid alignment` (`parser.rs:3059,3063`) when
/// their argument isn't a valid enum variant — an otherwise well-formed
/// statement whose one argument didn't parse, exactly like the FORM case
/// above. Argument-class, so it must not abort — the first version of the
/// predicate aborted on this because it enumerated argument-class messages
/// and this one wasn't on the list.
const INVALID_ALIGNMENT_ERB: &str = "@SYSTEM_TITLE\nALIGNMENT GARBAGE\nPRINTL ok\n";

#[test]
fn compati_error_line_off_does_not_abort_on_an_invalid_alignment_argument() {
    let dir = fixture_dir_with_erb("alignment-off", INVALID_ALIGNMENT_ERB);
    let config = EraConfig::default();
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("an invalid ALIGNMENT argument must not abort, even with CompatiErrorLine off");
}

const INVALID_BEGIN_ERB: &str = "@SYSTEM_TITLE\nBEGIN GARBAGE\nPRINTL ok\n";

#[test]
fn compati_error_line_off_does_not_abort_on_an_invalid_begin_argument() {
    let dir = fixture_dir_with_erb("begin-off", INVALID_BEGIN_ERB);
    let config = EraConfig::default();
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("an invalid BEGIN argument must not abort, even with CompatiErrorLine off");
}

/// `CALLEVENT`'s target must be one of the nine `EventType` names
/// (`parser.rs:3484-3492`); an unrecognised name is argument-class, not a
/// statement-shape failure.
const CALLEVENT_TARGET_ERROR_ERB: &str = "@SYSTEM_TITLE\nCALLEVENT NOTANEVENT\nPRINTL ok\n";

#[test]
fn compati_error_line_off_does_not_abort_on_a_callevent_target_error() {
    let dir = fixture_dir_with_erb("callevent-off", CALLEVENT_TARGET_ERROR_ERB);
    let config = EraConfig::default();
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("a CALLEVENT target error must not abort, even with CompatiErrorLine off");
}

/// A `TRYGOTOLIST` candidate may not carry arguments (`parser.rs:3442-3447`,
/// `ErbLoader.cs:1330-1385`); `FUNC LABEL1, 1` violates that, but the
/// `TRYGOTOLIST`/`FUNC`/`ENDFUNC` shape itself is fine — argument-class.
const TRYGOTOLIST_ARGUMENT_ERROR_ERB: &str =
    "@SYSTEM_TITLE\nTRYGOTOLIST\nFUNC LABEL1, 1\nENDFUNC\nPRINTL ok\n";

#[test]
fn compati_error_line_off_does_not_abort_on_a_trygotolist_argument_error() {
    let dir = fixture_dir_with_erb("trygotolist-off", TRYGOTOLIST_ARGUMENT_ERROR_ERB);
    let config = EraConfig::default();
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("a TRYGOTOLIST argument error must not abort, even with CompatiErrorLine off");
}

/// `文字列変数の代入に文字列式を強制する` (`system_ignore_string_set`) refuses a
/// plain `=` on a string variable and demands `'=` instead
/// (`ArgumentBuilder.cs:777-779`: `文字列代入は禁止されています`). The `LOCALS =`
/// statement shape is recognised fine; only this argument-level check fails —
/// argument-class, not line-shape.
const FORCED_STRING_ASSIGN_ERB: &str = "@SYSTEM_TITLE\nLOCALS = hello, world\nPRINTL ok\n";

#[test]
fn compati_error_line_off_does_not_abort_on_a_forced_string_assignment() {
    let dir = fixture_dir_with_erb("string-forced-off", FORCED_STRING_ASSIGN_ERB);
    let mut config = EraConfig::default();
    config.system_ignore_string_set = true;
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("a forced-string-assignment refusal must not abort, even with CompatiErrorLine off");
}

#[test]
fn compati_error_line_off_does_not_abort_on_an_argument_expression_failure() {
    let dir = fixture_dir_with_erb("arg-off", MALFORMED_FORM_ERB);
    let config = EraConfig::default();
    assert!(!config.compati_error_line, "CompatiErrorLine defaults to NO");
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("an argument-expression failure must NOT abort, even with CompatiErrorLine off");
}

/// `A = (1 + 2` is also argument-class: `A =` is a recognised assignment, and
/// only its RHS argument list failed to parse (`대입할 값이 없습니다`, from
/// `assign_stmt_from_list`). Same Emuera mechanism — not a shape failure, so
/// no abort.
const BROKEN_RHS_ERB: &str = "@SYSTEM_TITLE\nA = (1 + 2\nPRINTL ok\n";

#[test]
fn compati_error_line_off_does_not_abort_on_a_broken_assignment_rhs() {
    let dir = fixture_dir_with_erb("rhs-off", BROKEN_RHS_ERB);
    let config = EraConfig::default();
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("a broken assignment RHS must not abort, even with CompatiErrorLine off");
}

/// A game with no unparseable lines is unaffected either way — the abort
/// path must never fire on a clean load.
#[test]
fn compati_error_line_off_does_not_abort_a_clean_load() {
    let dir = fixture_dir_with_erb("clean", "@SYSTEM_TITLE\nPRINTL a\n");
    let config = EraConfig::default();
    assert!(!config.compati_error_line);
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
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
    let result = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    );
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
    let (_vm, _ctx, _tx) = run_script(
        dir.0.to_str().unwrap(),
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("CompatiErrorLine:YES must let the load through the refused @TOSTR definition");
}
