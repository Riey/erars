//! Both `FunctionBody::positions` round-tripping and `VERSION_MAGIC`
//! rejection live in one `#[test]` on purpose: `FunctionDic::new()` calls
//! `erars_ast::get_interner()`, which panics unless `init_interner` has run
//! first, and `init_interner`/`read_from`'s internal `update_interner` share
//! one process-global `Once` — a second `#[test]` in this file touching the
//! interner could race this one's setup (see
//! `erars-ast/tests/restore_guard.rs`).

use erars_ast::StrKey;
use erars_bytecode::{read_from, write_to};
use erars_vm::{FunctionBody, FunctionDic, Instruction};

#[test]
fn stale_magic_is_rejected_and_positions_round_trip() {
    // A buffer starting with the *old* (pre-`positions`) `VERSION_MAGIC`,
    // followed by bytes that would be read as this crate's current
    // `FunctionBody` layout if the magic check were skipped or loose. The
    // instruction stream is a raw memcpy of the in-memory layout, so a
    // reader that let a stale file through would decode this tail as
    // garbage `FunctionDic` content instead of refusing to load it — that
    // is exactly the failure mode the magic bump exists to turn into a
    // clean, up-front rejection.
    let mut stale = vec![2u8, 3, 2, 3, 0, 0, 0, 11];
    stale.extend_from_slice(&[0u8; 64]);

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| unsafe {
        read_from(&stale[..])
    }));
    assert!(
        result.is_err(),
        "a file written with the pre-`positions` VERSION_MAGIC must be rejected, not misread"
    );

    // `FunctionDic::new()` reaches for the process-global interner; nothing
    // above touched it (the magic check panics before `read_interner` runs).
    erars_ast::init_interner();

    let file_path = StrKey::from_u32(1);
    let fn_name = StrKey::from_u32(2);

    let body = FunctionBody {
        file_path,
        is_function: false,
        is_functions: true,
        goto_labels: Box::new([]),
        args: Box::new([]),
        body: Box::new([Instruction::nop(), Instruction::pop(), Instruction::nop()]),
        // Deliberately non-contiguous pcs and non-monotonic-looking lines,
        // to catch a byte-order or field-order mistake in the new
        // `write_arr!(positions, (u32, u32))` / `read_arr!((u32, u32))`
        // pair that a suspiciously tidy `[(0, 1), (1, 2)]` fixture would
        // hide.
        positions: Box::new([(0, 17), (1, 4_000_000_000), (2, 9)]),
    };

    let mut dic = FunctionDic::new();
    dic.normal.insert(fn_name, body.clone());

    let mut buf = Vec::new();
    write_to(&mut buf, &dic).expect("write_to");

    let dic2 = unsafe { read_from(&buf[..]).expect("read_from") };
    let body2 = dic2.normal.get(&fn_name).expect("function survives the round trip");

    assert_eq!(body2.file_path(), body.file_path);
    assert_eq!(body2.is_function(), body.is_function);
    assert_eq!(body2.is_functions(), body.is_functions);
    assert_eq!(body2.body(), &*body.body);
    assert_eq!(body2.positions(), &*body.positions);
}

