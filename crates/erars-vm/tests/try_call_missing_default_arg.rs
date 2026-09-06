//! Emuera's `TRYCALLFORM`/`TRYCCALLFORM` name a function that may not exist
//! and are documented to skip silently when it doesn't
//! (`docs/research/emuera-wiki/excom.md`'s TRYC-CATCH-ENDCATCH section).
//! Found missing while driving eramegaten past character creation for the
//! bytecode-opt arc's dynamic instruction histogram: `SYSTEM_STATUS_CALC.ERB`
//! calls `TRYCCALLFORM SKILL_ADD_STATUS_%TEMP_SKILLS%, "HP補正", ,
//! L_CHARA, TEMP_SKILL, TRYSKILL_VAR` with an *omitted* positional argument
//! (the empty slot between `"HP補正"` and `L_CHARA`). An omitted argument
//! compiles to `LoadDefaultArgument`, which resolved the callee's parameter
//! defaults via a hard `FunctionDic::get_func` lookup — so a `TRYCCALLFORM`
//! naming a genuinely absent function (the whole point of "try") aborted the
//! VM with "Function ... is not exists" instead of reaching the `try_call`
//! that was supposed to tolerate exactly that.

use std::sync::Arc;

use erars_ast::StrKey;
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = concat!(
    "@SYSTEM_TITLE\n",
    "TRYCCALLFORM MISSING_FUNC, \"a\", , 1, 2\n",
    "\tPRINTFORML unreachable\n",
    "CATCH\n",
    "ENDCATCH\n",
    "PRINTFORML done\n",
);

#[test]
fn try_call_with_omitted_arg_skips_missing_function() {
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
    let parser = ParserContext::new(header.clone(), StrKey::new("TRYC_OMITTED_ARG_TEST.ERB"));
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

    // The missing function's body never runs (no "unreachable" line), and
    // the empty CATCH body means execution falls straight through to the
    // statement after ENDCATCH.
    assert_eq!(lines, vec!["done".to_string()]);
}
