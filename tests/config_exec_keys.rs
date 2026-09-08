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

/// Parses, compiles and runs one script under `config`, exactly the way
/// `tests/run_tests.rs` does, and returns the console text plus whether the
/// run finished without a VM error.
fn run(script: &str, config: EraConfig) -> (bool, String) {
    erars_ast::init_interner();

    let parser = ParserContext::new(
        test_util::get_ctx("CONFIG_EXEC_KEYS.ERB").header.try_as_arc().unwrap(),
        erars_ast::StrKey::new("CONFIG_EXEC_KEYS.ERB"),
    )
    .with_ignore_string_set(config.system_ignore_string_set);

    let program = match parser.parse_program_str(script) {
        Ok(program) => program,
        Err((err, _)) => return (false, format!("PARSE ERROR: {err}")),
    };

    let header = parser.header.try_as_arc().unwrap();
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(NullSystemFunctions),
        std::env::temp_dir().join("erars-config-exec-keys/sav"),
        std::env::temp_dir().join("erars-config-exec-keys/resources"),
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
