//! `check_variable_exist_inner` (W1001, `crates/erars-lint/src/lib.rs`)
//! recognised a variable reference only via the classic
//! `LoadStr(name) -> LoadVarRef(count)` adjacency: `inst.is_load_var_ref()`
//! then reading `func.body()[i - 1].as_load_str()`. P2's fusion
//! (`crates/erars-compiler/src/compiler.rs`'s `push_var_ref`) rewrites that
//! pair into one `LoadVarRefNamed{0..3}` instruction for every non-extern
//! reference with `count <= 3` — the overwhelming majority in real corpora
//! (see the bytecode-dispatch-optimization doc's §14) — which silently
//! stopped this lint from ever firing again, since neither instruction
//! shape it recognised occurs any more for those references. This test
//! defends the fix: a W1001 warning must still fire for an undeclared
//! variable referenced through the fused form, at more than one `count`.
use std::sync::Arc;

use erars_compiler::{compile, EraConfig, HeaderInfo, InstructionType, ParserContext};
use erars_lint::{check_function, ErarsFiles};
use erars_vm::{FunctionDic, NullSystemFunctions, VmContext};

const SCRIPT: &str = r#"@LINT_TEST
#DIM A0
#DIM A1, 3
PRINTFORML {A0}
PRINTFORML {A1:1}
PRINTFORML {UNDECLARED0}
PRINTFORML {UNDECLARED1:1}
"#;

#[test]
fn w1001_fires_for_undeclared_variable_through_fused_load_var_ref() {
    erars_ast::init_interner();

    let header = Arc::new(HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    });
    let config = Arc::new(EraConfig::default());
    let mut ctx = VmContext::new(
        header.clone(),
        config,
        Box::new(NullSystemFunctions),
        "sav".into(),
        "resources".into(),
    );

    let parser =
        ParserContext::new(header.clone(), erars_ast::StrKey::new("tests/LINT_TEST.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser.parse_program_str(SCRIPT).unwrap() {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    // Sanity: both undeclared references actually compiled to the fused
    // form (count 0 and count 1), not the classic fallback — otherwise this
    // test would pass even with the old, broken lint.
    let fn_name = erars_ast::get_interner().get_or_intern("LINT_TEST");
    let func = dic.normal.get(&fn_name).unwrap();
    let mut named0 = 0u32;
    let mut named1 = 0u32;
    let mut classic = 0u32;
    for &inst in func.body() {
        match inst.ty() {
            InstructionType::LoadVarRefNamed0 => named0 += 1,
            InstructionType::LoadVarRefNamed1 => named1 += 1,
            InstructionType::LoadVarRef => classic += 1,
            _ => {}
        }
    }
    assert_eq!(named0, 2, "A0 and UNDECLARED0 are both count=0 fused refs");
    assert_eq!(named1, 2, "A1:1 and UNDECLARED1:1 are both count=1 fused refs");
    assert_eq!(classic, 0, "no reference here exceeds count=3");

    let mut files = ErarsFiles::new();
    let diagnostics = check_function(&dic, &ctx.var, &mut files);

    let w1001: Vec<&str> = diagnostics
        .iter()
        .filter(|d| d.code.as_deref() == Some("W1001"))
        .map(|d| d.message.as_str())
        .collect();

    assert_eq!(
        w1001.len(),
        2,
        "exactly UNDECLARED0 and UNDECLARED1 should warn, not A0/A1; got: {w1001:?}"
    );
    assert!(w1001.iter().any(|m| m.contains("UNDECLARED0")), "{w1001:?}");
    assert!(w1001.iter().any(|m| m.contains("UNDECLARED1")), "{w1001:?}");
    assert!(
        !w1001.iter().any(|m| m.contains("`A0`") || m.contains("`A1`")),
        "declared #DIM locals must never warn: {w1001:?}"
    );
}
