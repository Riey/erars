//! The four executor-side compatibility config keys, each exercised in both
//! positions so a flipped value fails the test.
//!
//! Every key defaults to `false` (`Config/ConfigData.cs:108-115`), and the
//! `false` half of each test below is what erars did before the keys were
//! wired, except where noted in the test's own comment.

mod test_util;

use std::sync::Arc;

use erars_compiler::{compile, EraConfig, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

/// One scratch directory per process *and* calling test thread, swept before
/// creation and removed on drop — the convention
/// `tests/wiki_coverage.rs`'s `Runner` and `tests/run_tests.rs`'s
/// `ScratchGuard` use. A fixed name under the temp dir would be shared by
/// every run of this suite that has ever happened on the machine, and `cargo
/// test` runs these tests on parallel threads, so a save slot or `*.dat` left
/// by an earlier run (or a sibling test) could decide a verdict from machine
/// history rather than from the code under test.
struct Scratch(std::path::PathBuf);

impl Scratch {
    fn new() -> Self {
        let root = std::env::temp_dir().join(format!(
            "erars-config-exec-keys-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(root.join("sav")).unwrap();
        std::fs::create_dir_all(root.join("resources")).unwrap();
        Self(root)
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        // Best-effort, exactly like `ScratchGuard`: a killed process skips
        // this, which is why `new` also sweeps before creating.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// Parses, compiles and runs one script under `config`, exactly the way
/// `tests/run_tests.rs` does, and returns the console text plus whether the
/// run finished without a VM error.
fn run(script: &str, config: EraConfig) -> (bool, String) {
    erars_ast::init_interner();

    let parser = ParserContext::new(
        test_util::get_ctx("CONFIG_EXEC_KEYS.ERB").header.try_as_arc().unwrap(),
        erars_ast::StrKey::new("CONFIG_EXEC_KEYS.ERB"),
    )
    .with_ignore_string_set(config.system_ignore_string_set)
    // Emuera `Config.ICFunction = IgnoreCase && !CompatiFunctionNoignoreCase`
    // (`Config/Config.cs:36`), negated.
    .with_case_sensitive_functions(
        !(config.ignore_case && !config.compati_function_no_ignore_case),
    );

    let program = match parser.parse_program_str(script) {
        Ok(program) => program,
        Err((err, _)) => return (false, format!("PARSE ERROR: {err}")),
    };

    let scratch = Scratch::new();
    let header = parser.header.try_as_arc().unwrap();
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(NullSystemFunctions),
        scratch.0.join("sav"),
        scratch.0.join("resources"),
    );

    let mut dic = FunctionDic::new();
    for func in program {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    let vm = TerminalVm::new(dic, header);
    let ok = vm.start(&mut tx, &mut ctx);
    let mut lines: Vec<String> = tx.lines_from(0).iter().map(ToString::to_string).collect();
    lines.push(tx.last_line.to_string());
    drop(scratch);

    (ok, lines.join("\n").trim_end().to_owned())
}

fn config_with(f: impl FnOnce(&mut EraConfig)) -> EraConfig {
    let mut config = EraConfig::default();
    f(&mut config);
    config
}

/// `ユーザー関数の全ての引数の省略を許可する` (`CompatiFuncArgOptional`).
///
/// A global parameter has no default value — Emuera's implicit `0`/`""` is
/// only given to `ARG`/`ARGS`/private variables
/// (`GameProc/ErbLoader.cs:580-590`) — so omitting it is refused
/// (`GameProc/Process.CalledFunction.cs:191-198`) unless this key is on, in
/// which case the callee's variable keeps whatever it held
/// (`UserDefinedFunctionArgument.SetTransporter`,
/// `GameProc/Process.CalledFunction.cs:36-37`).
const OPTIONAL_ARG: &str = "\
@SYSTEM_TITLE
FLAG:0 = 42
CALL SUB
PRINTFORML kept={FLAG:0}

@SUB, FLAG:0
";

#[test]
fn func_arg_optional_keeps_the_previous_value() {
    let (ok, out) = run(
        OPTIONAL_ARG,
        config_with(|c| c.compati_func_arg_optional = true),
    );
    assert!(ok, "VM error:\n{out}");
    assert_eq!(out, "kept=42");
}

#[test]
fn func_arg_optional_off_refuses_the_call() {
    let (ok, out) = run(
        OPTIONAL_ARG,
        config_with(|c| c.compati_func_arg_optional = false),
    );
    assert!(!ok, "the call should have been refused, got:\n{out}");
    assert!(
        out.contains("생략할 수 없습니다"),
        "unexpected failure:\n{out}"
    );
}

/// `ユーザー関数の引数に自動的にTOSTRを補完する` (`CompatiFuncArgAutoConvert`):
/// an int passed to a string parameter is an error unless the key wraps it in
/// `TOSTR` (`GameProc/Process.CalledFunction.cs:199-219`).
const AUTO_CONVERT: &str = "\
@SYSTEM_TITLE
CALL SUB, 12

@SUB, LOCALS
PRINTFORML got=[{LOCALS}]
";

#[test]
fn func_arg_auto_convert_applies_tostr() {
    let (ok, out) = run(
        AUTO_CONVERT,
        config_with(|c| c.compati_func_arg_auto_convert = true),
    );
    assert!(ok, "VM error:\n{out}");
    assert_eq!(out, "got=[12]");
}

#[test]
fn func_arg_auto_convert_off_refuses_the_call() {
    let (ok, out) = run(
        AUTO_CONVERT,
        config_with(|c| c.compati_func_arg_auto_convert = false),
    );
    assert!(!ok, "the call should have been refused, got:\n{out}");
    assert!(
        out.contains("문자열형으로 변환할 수 없습니다"),
        "unexpected failure:\n{out}"
    );
}

/// `キャラクタ変数の引数を補完しない` (`SystemNoTarget`): a character variable
/// written without its character index resolves through `TARGET` by default,
/// and is refused when the key is on
/// (`GameData/Variable/VariableParser.cs:108-137`).
const NO_TARGET: &str = "\
@SYSTEM_TITLE
ADDCHARA 3
TARGET = 0
CFLAG:3 = 7
PRINTFORML cflag={CFLAG:3}
";

#[test]
fn no_target_off_completes_target() {
    let (ok, out) = run(NO_TARGET, config_with(|c| c.system_no_target = false));
    assert!(ok, "VM error:\n{out}");
    assert_eq!(out, "cflag=7");
}

#[test]
fn no_target_on_refuses_the_bare_access() {
    let (ok, out) = run(NO_TARGET, config_with(|c| c.system_no_target = true));
    assert!(!ok, "the access should have been refused, got:\n{out}");
    assert!(
        out.contains("캐릭터 변수의 인수는 생략할 수 없습니다"),
        "unexpected failure:\n{out}"
    );
}

/// `文字列変数の代入に文字列式を強制する` (`SystemIgnoreStringSet`): plain `=`
/// on a string variable is rejected at *parse* time and the script must use
/// `'=` (`GameProc/Function/ArgumentBuilder.cs:777-779`).
const STRING_SET: &str = "\
@SYSTEM_TITLE
LOCALS = hello
PRINTFORML s={LOCALS}
";

#[test]
fn ignore_string_set_off_accepts_plain_assign() {
    let (ok, out) = run(
        STRING_SET,
        config_with(|c| c.system_ignore_string_set = false),
    );
    assert!(ok, "VM error:\n{out}");
    assert_eq!(out, "s=hello");
}

#[test]
fn ignore_string_set_on_rejects_plain_assign() {
    let (ok, out) = run(
        STRING_SET,
        config_with(|c| c.system_ignore_string_set = true),
    );
    assert!(!ok, "the assignment should have been rejected, got:\n{out}");
    assert!(
        out.contains("文字列代入は禁止されています"),
        "unexpected failure:\n{out}"
    );
}

/// `'=` is the escape hatch the refusal message names, so it must still work
/// with the key on.
#[test]
fn ignore_string_set_on_still_allows_str_assign() {
    let (ok, out) = run(
        "\
@SYSTEM_TITLE
LOCALS '= \"hello\"
PRINTFORML s={LOCALS}
",
        config_with(|c| c.system_ignore_string_set = true),
    );
    assert!(ok, "VM error:\n{out}");
    assert_eq!(out, "s=hello");
}

/// `擬似変数RANDの仕様をeramakerに合わせる` (`CompatiRAND`, default `false`,
/// `Config/ConfigData.cs:96`).
///
/// The key swaps `RandToken` for `CompatiRandToken`
/// (`GameData/Variable/VariableData.cs:293-297`). Both draw from the *same*
/// generator; only the reduction differs — `CompatiRandToken.GetIntValue`
/// returns `0` for `0`, negates a negative argument and yields
/// `GetNextRand(32768) % i` (`GameData/Variable/VariableToken.cs:1471-1479`),
/// which is why the result can never be `>= 32767` and is biased once the
/// range stops dividing 32768. The two parse-time refusals (omitted argument,
/// literal `0`) are gated on the same key
/// (`GameData/Variable/VariableParser.cs:167-179`).
const COMPATI_RAND: &str = "\
@SYSTEM_TITLE
PRINTFORML zero={RAND:0}
PRINTFORML neg={RAND:-4}
PRINTFORML bare={RAND}
";

#[test]
fn compati_rand_accepts_zero_negative_and_bare() {
    let (ok, out) = run(COMPATI_RAND, config_with(|c| c.compati_rand = true));
    assert!(ok, "VM error:\n{out}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "zero=0", "RAND:0 is 0, not an error");
    assert_eq!(lines[2], "bare=0", "a bare RAND is RAND:0");
    // `% 4` after negation, so the only thing that can vary is which of 0..3.
    let neg: i64 = lines[1].strip_prefix("neg=").unwrap().parse().unwrap();
    assert!((0..4).contains(&neg), "negative argument was negated: {neg}");
}

#[test]
fn compati_rand_off_still_refuses_zero() {
    let (ok, out) = run(COMPATI_RAND, config_with(|c| c.compati_rand = false));
    assert!(!ok, "RAND:0 should have been refused, got:\n{out}");
    assert!(
        out.contains("0 이하의 값"),
        "unexpected failure:\n{out}"
    );
}

/// The compat reduction is `GetNextRand(32768) % i`, so it can never reach
/// 32767 and — unlike the default mode, which reduces a 64-bit draw modulo the
/// requested bound — cannot produce the whole requested range at all once the
/// bound exceeds 32768. Drawing a wide range is therefore the sharpest
/// separator between the two modes that does not depend on the generator's
/// exact sequence.
#[test]
fn compati_rand_never_reaches_32767() {
    let script = "\
@SYSTEM_TITLE
LOCAL:1 = 0
REPEAT 200
\tLOCAL = RAND:1000000
\tIF LOCAL >= 32767
\t\tLOCAL:1 = 1
\tENDIF
REND
PRINTFORML big={LOCAL:1}
";
    let (ok, out) = run(script, config_with(|c| c.compati_rand = true));
    assert!(ok, "VM error:\n{out}");
    assert_eq!(out, "big=0", "compat RAND is bounded by 32768");

    let (ok, out) = run(script, config_with(|c| c.compati_rand = false));
    assert!(ok, "VM error:\n{out}");
    assert_eq!(
        out, "big=1",
        "the default (Emuera-exact) RAND spans the whole requested range"
    );
}

/// `関数・属性については大文字小文字を無視しない` (`CompatiFunctionNoignoreCase`,
/// default `false`, `Config/ConfigData.cs:98`), combined with
/// `大文字小文字の違いを無視する` (`IgnoreCase`, default `true`,
/// `Config/ConfigData.cs:40`).
///
/// Emuera derives one flag from both: `ICFunction = IgnoreCase &&
/// !CompatiFunctionNoignoreCase` (`Config/Config.cs:34-50`). So `IgnoreCase`
/// wins when it is off — function names are case-sensitive regardless of the
/// compat key — and the compat key only has anything to say while
/// `IgnoreCase` is on. Variables stay `ICVariable = IgnoreCase`
/// (`Config/Config.cs:37`, `:401-425`).
const CASE_CALL: &str = "\
@SYSTEM_TITLE
CALL sub
PRINTFORML after

@SUB
PRINTFORML in_sub
";

#[test]
fn function_names_fold_by_default() {
    let (ok, out) = run(CASE_CALL, EraConfig::default());
    assert!(ok, "VM error:\n{out}");
    assert_eq!(out, "in_sub\nafter");
}

#[test]
fn function_no_ignore_case_makes_the_call_miss() {
    let (ok, out) = run(
        CASE_CALL,
        config_with(|c| c.compati_function_no_ignore_case = true),
    );
    assert!(!ok, "`CALL sub` should not have found `@SUB`, got:\n{out}");
    assert!(out.contains("sub"), "unexpected failure:\n{out}");
}

/// With the compat key on, a call whose case matches the definition still
/// resolves — the names are compared, not folded away.
#[test]
fn function_no_ignore_case_keeps_exact_case_working() {
    let (ok, out) = run(
        "\
@SYSTEM_TITLE
CALL sub
PRINTFORML after

@sub
PRINTFORML in_sub
",
        config_with(|c| c.compati_function_no_ignore_case = true),
    );
    assert!(ok, "VM error:\n{out}");
    assert_eq!(out, "in_sub\nafter");
}

/// `IgnoreCase:NO` alone already makes function names case-sensitive
/// (`ICFunction = IgnoreCase && !CompatiFunctionNoignoreCase`), and setting
/// the compat key on top changes nothing — that is the combination worth
/// pinning, since the two keys disagree in exactly this cell.
#[test]
fn ignore_case_off_wins_over_the_compat_key() {
    for compat in [false, true] {
        let (ok, out) = run(
            CASE_CALL,
            config_with(|c| {
                c.ignore_case = false;
                c.compati_function_no_ignore_case = compat;
            }),
        );
        assert!(
            !ok,
            "IgnoreCase:NO must make `CALL sub` miss `@SUB` (compat={compat}), got:\n{out}"
        );
    }
}

/// `eramaker互換性に関する警告を表示する` (`WarnBackCompatibility`, default
/// **YES**, `Config/ConfigData.cs:86`).
///
/// It gates exactly the warnings Emuera raises with `isBackComp: true`
/// (`GameData/ParserMediator.cs:128`), and a search of the whole C# source
/// finds exactly one such call: the level-0 `SIF` warning at
/// `GameProc/Function/Instraction.Child.cs:1795`. None of the other compat
/// keys wired so far emit a warning at all — they raise hard errors — so this
/// one warning is the key's entire scope.
///
/// Level 0 is *below* the default `表示する最低警告レベル` of 1
/// (`Config/ConfigData.cs:72`), so the loader drops it unless the level is
/// lowered; this test reads the parser's own warning list, where the level is
/// still attached.
const SIF_BACK_COMPAT: &str = "\
@SYSTEM_TITLE
SIF 1

\tPRINTFORML body
";

fn sif_warnings(warn_back_compatibility: bool) -> Vec<(String, u8)> {
    erars_ast::init_interner();
    let ctx = test_util::get_ctx("SIF_BACK_COMPAT.ERB")
        .with_warn_back_compatibility(warn_back_compatibility);
    let erb = ctx
        .parse_and_compile(
            &mut ctx.preprocessor(SIF_BACK_COMPAT),
            &mut erars_compiler::Bump::new(),
        )
        .unwrap();
    erb.warnings.into_iter().map(|(msg, _, level)| (msg, level)).collect()
}

#[test]
fn back_compat_warning_is_on_by_default() {
    let warnings = sif_warnings(true);
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    assert_eq!(warnings[0].1, 0, "Emuera raises this at level 0");
    assert!(warnings[0].0.contains("SIF"), "{warnings:?}");
}

#[test]
fn back_compat_warning_off_suppresses_it() {
    let warnings = sif_warnings(false);
    assert!(warnings.is_empty(), "{warnings:?}");
}
