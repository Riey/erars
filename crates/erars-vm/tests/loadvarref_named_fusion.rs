//! `LoadStr(name)` immediately followed by `LoadVarRef(count)` is fused into
//! one `LoadVarRefNamed{count}` instruction in `push_var_ref`
//! (`erars-compiler/src/compiler.rs`) whenever `count <= 3` — the only range
//! observed across a corpus-wide census of eraTHYMKR and eramegaten (~1.4M
//! combined `LoadVarRef` occurrences, 100% immediately preceded by
//! `LoadStr`, max `count` 3). `count > 3` falls back to the classic
//! two-instruction form, unobserved in both corpora but not impossible.
//!
//! Two cases in one `#[test]`: `erars_ast::init_interner()` is
//! process-global (see `erars-ast/tests/restore_guard.rs`), and a second
//! `#[test]` in this file could race this one's setup.

use std::sync::Arc;

use erars_ast::{
    get_interner, Expr, Function, FunctionHeader, ScriptPosition, Stmt, StmtWithPos, StrKey,
    Variable,
};
use erars_compiler::{compile, EraConfig, HeaderInfo, InstructionType, ParserContext};
use erars_ui::{InputRequest, VirtualConsole};
use erars_vm::{console_config, FunctionDic, SystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = r#"@SYSTEM_TITLE
CALL FUSION_TEST
QUIT

@FUSION_TEST
#DIM A0
#DIM A1, 3
#DIM A2, 3, 3
#DIM A3, 3, 3, 3
A0 = 111
A1:1 = 222
A2:1:2 = 333
A3:1:2:0 = 444
PRINTFORML {A0}
PRINTFORML {A1:1}
PRINTFORML {A2:1:2}
PRINTFORML {A3:1:2:0}
"#;

/// No script in this test ever waits on input.
struct NoInput;

impl SystemFunctions for NoInput {
    fn input(&mut self, _req: InputRequest) -> anyhow::Result<Option<erars_ast::Value>> {
        unreachable!("this script never requests input")
    }

    fn redraw(
        &mut self,
        _vconsole: &mut VirtualConsole,
        _painted: erars_vm::graphics::Painted<'_>,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

#[test]
fn load_var_ref_named_fusion_reads_writes_and_falls_back_past_count_3() {
    erars_ast::init_interner();

    // --- Case 1: counts 0..=3 through the real parser + a real VM run ---
    // covers both the compiler's new fused emission and the executor's four
    // new dispatch arms, for both a write (`Stmt::Assign`) and a read
    // (`PRINTFORML {..}`'s embedded `Expr::Var`).
    let info = HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    };
    let header = Arc::new(info);
    let config = Arc::new(EraConfig::default());
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        config,
        Box::new(NoInput),
        "sav".into(),
        "resources".into(),
    );

    let parser = ParserContext::new(header.clone(), StrKey::new("FUSION.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser.parse_program_str(SCRIPT).unwrap() {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    // Inspect the compiled instruction stream for `FUSION_TEST` before
    // handing `dic` to the VM (which consumes it by move): every one of the
    // four non-extern variable references in that function (A0 write+read,
    // A1 write+read, A2 write+read, A3 write+read = 8 references, each
    // `count` in 0..=3) must have been fused, and the classic `LoadVarRef`
    // must not appear at all.
    let fusion_fn = dic
        .normal
        .get(&get_interner().get_or_intern("FUSION_TEST"))
        .expect("FUSION_TEST is a normal function");
    let insts = fusion_fn.body();
    let mut fused_counts = [0u32; 4];
    let mut classic_load_var_ref = 0u32;
    for &inst in insts.iter() {
        match inst.ty() {
            InstructionType::LoadVarRefNamed0 => fused_counts[0] += 1,
            InstructionType::LoadVarRefNamed1 => fused_counts[1] += 1,
            InstructionType::LoadVarRefNamed2 => fused_counts[2] += 1,
            InstructionType::LoadVarRefNamed3 => fused_counts[3] += 1,
            InstructionType::LoadVarRef => classic_load_var_ref += 1,
            _ => {}
        }
    }
    assert_eq!(classic_load_var_ref, 0, "count 0..=3 must never fall back to the classic form");
    assert_eq!(fused_counts, [2, 2, 2, 2], "each of A0..A3 is referenced twice (write + read)");

    let vm = TerminalVm::new(dic, header);
    let ok = vm.start(&mut tx, &mut ctx);
    let lines: Vec<String> = tx.lines.iter().map(ToString::to_string).collect();
    assert!(ok, "VM error:\n{}", lines.join("\n"));
    assert_eq!(lines, vec!["111", "222", "333", "444"]);

    // --- Case 2: count > 3 falls back to the classic two-instruction form ---
    // `compile()` takes no `HeaderInfo` and never validates a `Variable`'s
    // `args` length against any declared dimensionality (that happens only
    // in the parser, which this bypasses) — a hand-built AST with 4 index
    // expressions compiles exactly like `positions_line_encoding.rs`'s
    // hand-built minimal functions.
    let file_path = StrKey::new("FALLBACK.ERB");
    let fn_name = StrKey::new("FALLBACK_TEST");
    let var_name = StrKey::new("SOME_VAR");
    let header = FunctionHeader {
        file_path,
        name: fn_name,
        args: Vec::new(),
        infos: Vec::new(),
    };
    let assign = Stmt::Assign(
        Variable {
            var: var_name,
            func_extern: None,
            args: vec![Expr::Int(0), Expr::Int(0), Expr::Int(0), Expr::Int(0)],
        },
        None,
        Expr::Int(0),
    );
    let func = Function {
        header,
        body: vec![StmtWithPos(assign, ScriptPosition { line: 1 })],
    };
    let compiled = compile(func).expect("compile a 4-index variable reference");

    let mut fused_any = false;
    let mut saw_load_str_then_load_var_ref_4 = false;
    for (i, &inst) in compiled.body.iter().enumerate() {
        match inst.ty() {
            InstructionType::LoadVarRefNamed0
            | InstructionType::LoadVarRefNamed1
            | InstructionType::LoadVarRefNamed2
            | InstructionType::LoadVarRefNamed3 => fused_any = true,
            InstructionType::LoadVarRef if inst.as_load_var_ref() == Some(4) => {
                assert!(i > 0, "LoadVarRef must be preceded by something");
                assert_eq!(
                    compiled.body[i - 1].as_load_str(),
                    Some(var_name),
                    "the classic form's LoadStr must carry the variable's own name"
                );
                saw_load_str_then_load_var_ref_4 = true;
            }
            _ => {}
        }
    }
    assert!(!fused_any, "count 4 must not use any fused LoadVarRefNamed variant");
    assert!(
        saw_load_str_then_load_var_ref_4,
        "count 4 must fall back to LoadStr(name) + LoadVarRef(4)"
    );
}
