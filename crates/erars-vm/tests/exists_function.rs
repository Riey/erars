//! `EXISTFUNCTION`'s reference spec (Emuera's own documented behaviour):
//! `EXISTFUNCTION(name)` returns 0 when `name` names no function or a system
//! builtin, 1 for a normal `@`-function, 2 for a `#FUNCTION`-declared numeric
//! expression function, and 3 for a `#FUNCTIONS`-declared string expression
//! function. Found missing while driving eramegaten past character creation
//! for the bytecode-opt arc's dynamic instruction histogram:
//! `ROLE_FUNCTION.ERB`'s `SIF !EXISTFUNCTION(@"ROLE_%ARGS%_%ARGS:1%") /
//! RETURNF 0` uses it to guard a dynamically-named `CALLFORMF`, and erars had
//! no `EXISTFUNCTION` at all, aborting the VM with "Function EXISTFUNCTION is
//! not exists".

use std::sync::Arc;

use erars_ast::StrKey;
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = concat!(
    "@SYSTEM_TITLE\n",
    "PRINTFORML {EXISTFUNCTION(\"NOT_A_FUNC_AT_ALL\")}\n",
    "PRINTFORML {EXISTFUNCTION(\"PRINTFORML\")}\n",
    "PRINTFORML {EXISTFUNCTION(\"NORMAL_FUNC\")}\n",
    "PRINTFORML {EXISTFUNCTION(\"NUM_FUNC\")}\n",
    "PRINTFORML {EXISTFUNCTION(\"STR_FUNC\")}\n",
    "\n",
    "@NORMAL_FUNC\n",
    "PRINTFORML normal\n",
    "\n",
    "@NUM_FUNC\n",
    "#FUNCTION\n",
    "RETURNF 1\n",
    "\n",
    "@STR_FUNC\n",
    "#FUNCTIONS\n",
    "RETURNF \"x\"\n",
);

#[test]
fn exists_function_distinguishes_function_kinds() {
    erars_ast::init_interner();

    let header = Arc::new(HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    });

    let config = EraConfig::default();
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(NullSystemFunctions),
        "sav".into(),
        "resources".into(),
    );
    let parser = ParserContext::new(header.clone(), StrKey::new("EXISTFUNCTION_TEST.ERB"));
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

    // A nonexistent name and a system builtin (PRINTFORML) both read 0; the
    // three user-declared kinds read 1 (normal), 2 (#FUNCTION), 3
    // (#FUNCTIONS) respectively.
    assert_eq!(lines[0].trim(), "0");
    assert_eq!(lines[1].trim(), "0");
    assert_eq!(lines[2].trim(), "1");
    assert_eq!(lines[3].trim(), "2");
    assert_eq!(lines[4].trim(), "3");
}
