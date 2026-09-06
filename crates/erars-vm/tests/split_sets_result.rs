//! `SPLIT`'s reference spec (Emuera's own documented behaviour, confirmed
//! against `evilmask.gitlab.io/emuera.em.doc/Reference/SPLIT.html`): the
//! number of elements written is assigned to the system variable `RESULT`,
//! e.g. `SPLIT "A,B,C", ",", STR` sets `RESULT` to `3`. Found missing while
//! driving eramegaten past its title screen for the bytecode-opt arc's
//! dynamic instruction histogram: `RAND_SPLIT.ERB`'s
//! `RETURNF LOCALS:(RAND:L_RESULT)` (`L_RESULT = RESULT` read immediately
//! after `SPLIT`) relies on this to pick a random split element, and with
//! `RESULT` left untouched by `SPLIT` it inherited a stale, often-zero
//! ambient `RESULT` value from earlier in the call chain instead, sending
//! `RAND` a non-positive argument and crashing the VM.

use std::sync::Arc;

use erars_ast::StrKey;
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = concat!(
    "@SYSTEM_TITLE\n",
    "#DIM RESULT_OUT\n",
    "SPLIT \"F1_F2_F3_X\", \"_\", LOCALS\n",
    "RESULT_OUT = RESULT\n",
    "PRINTFORML {RESULT_OUT}\n",
    "PRINTFORML %LOCALS:0%\n",
    "PRINTFORML %LOCALS:3%\n",
);

#[test]
fn split_sets_result_to_element_count() {
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
    let parser = ParserContext::new(header.clone(), StrKey::new("SPLIT_TEST.ERB"));
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

    // "F1_F2_F3_X" split on "_" is 4 elements: RESULT must be 4, and the
    // written slots must hold the actual split parts, not just a count that
    // happens to be right.
    assert_eq!(lines[0].trim(), "4");
    assert_eq!(lines[1].trim(), "F1");
    assert_eq!(lines[2].trim(), "X");
}
