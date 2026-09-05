//! `write_positions`/`read_positions` (`erars-bytecode/src/lib.rs`) choose a
//! narrow (6-byte `(u32, u16)`) or wide (8-byte `(u32, u32)`) entry encoding
//! per function, based on whether every line number in that function fits a
//! `u16`. This is a supported overflow path, not a truncation or a panic: a
//! generated or concatenated ERB file could plausibly produce a function
//! whose line count exceeds 65,535, and that must still round-trip through
//! `game.era` correctly rather than corrupt or reject the file.
//!
//! One `#[test]` on purpose: `compile()`/`FunctionDic::new()` reach for the
//! process-global interner (see `erars-ast/tests/restore_guard.rs`), and a
//! second `#[test]` in this file could race this one's setup.

use erars_ast::{Function, FunctionHeader, ScriptPosition, Stmt, StmtWithPos, StrKey};
use erars_bytecode::{read_from, write_to};
use erars_compiler::compile;
use erars_vm::{FunctionBody, FunctionDic};

/// Compiles a minimal one-statement function whose sole position is at
/// `line`, via the real `compile()` entry point (not a hand-built
/// `CompiledFunction`), and wraps the result in a `FunctionBody` the same way
/// `VariableStorage::insert_compiled_func` does for its `body`/`positions`
/// fields.
fn compile_function_at_line(file_path: StrKey, fn_name: StrKey, label: StrKey, line: u32) -> FunctionBody {
    let header = FunctionHeader {
        file_path,
        name: fn_name,
        args: Vec::new(),
        infos: Vec::new(),
    };
    let func = Function {
        header,
        body: vec![StmtWithPos(Stmt::Label(label), ScriptPosition { line })],
    };

    let compiled = compile(func).expect("compile a minimal one-statement function");
    assert_eq!(
        compiled.positions.as_ref(),
        &[(0, line)],
        "the real compiler must still record exactly one (pc, line) entry for one statement"
    );

    FunctionBody {
        file_path,
        is_function: false,
        is_functions: false,
        goto_labels: Box::new([]),
        args: Box::new([]),
        body: compiled.body,
        positions: compiled.positions,
    }
}

#[test]
fn line_number_past_u16_max_round_trips_via_wide_fallback() {
    erars_ast::init_interner();

    let file_path = StrKey::from_u32(1);

    // A line comfortably inside u16's range (both real corpora measured for
    // this arc — eraTHYMKR max 35,398, eramegaten max 11,107 — land here):
    // must serialize via the narrow, 6-byte-per-entry path.
    let narrow_fn = StrKey::from_u32(2);
    let narrow_label = StrKey::from_u32(3);
    let narrow_line = 12_345u32;
    let narrow_body = compile_function_at_line(file_path, narrow_fn, narrow_label, narrow_line);

    // A line past u16::MAX (65,535) — the scenario a generated or
    // concatenated ERB file could plausibly hit — must serialize via the
    // wide, 8-byte-per-entry fallback instead of truncating or panicking.
    let wide_fn = StrKey::from_u32(4);
    let wide_label = StrKey::from_u32(5);
    let wide_line = 70_000u32;
    assert!(wide_line > u16::MAX as u32);
    let wide_body = compile_function_at_line(file_path, wide_fn, wide_label, wide_line);

    let mut dic = FunctionDic::new();
    dic.normal.insert(narrow_fn, narrow_body.clone());
    dic.normal.insert(wide_fn, wide_body.clone());

    let mut buf = Vec::new();
    write_to(&mut buf, &dic).expect("write_to");

    let dic2 = unsafe { read_from(&buf[..]).expect("read_from") };

    let narrow2 = dic2
        .normal
        .get(&narrow_fn)
        .expect("narrow function survives the round trip");
    assert_eq!(narrow2.positions(), &*narrow_body.positions);
    assert_eq!(narrow2.line_at(0), Some(narrow_line));

    let wide2 = dic2
        .normal
        .get(&wide_fn)
        .expect("wide function survives the round trip");
    assert_eq!(wide2.positions(), &*wide_body.positions);
    assert_eq!(
        wide2.line_at(0),
        Some(wide_line),
        "a line number past u16::MAX must round-trip exactly, not wrap or truncate into u16 range"
    );
}
