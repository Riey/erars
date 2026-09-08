//! `VERSION_MAGIC` is monotonic and never reused, even when a format
//! reverts back to what an earlier value once named (see
//! `docs/research/2026-09-05-bytecode-dispatch-optimization.md` §17). This
//! arc alone has burned four values before the current one: 11 (pre-arc
//! `35435b9`), 12 (`d0f1162`, P1v2's positions table), 13 (`75a7315`, the
//! u16 line encoding), and 14 (`454fb16`, P2's `LoadStr`/`LoadVarRef`
//! fusion). Reverting `d0f1162`/`75a7315` restored 12's on-disk layout in
//! spirit, but a `game.era` written under any of the four spent values must
//! still be rejected — a straight revert of the code that had bumped magic
//! 11→12 would silently roll the constant back to 12 too, and every real
//! `d0f1162..75a7315`-era `game.era` still on disk (caches, benchmark
//! artifacts, mid-arc worktrees) would then be accepted and misread as the
//! restored format instead of refused.
//!
//! All four rejection cases plus the current format's round trip live in
//! one `#[test]` on purpose: `FunctionDic::new()` reaches for
//! `erars_ast::get_interner()`, which panics unless `init_interner` has run
//! first, and `init_interner`/`read_from`'s internal `update_interner` share
//! one process-global `Once` — a second `#[test]` in this file touching the
//! interner could race this one's setup (see
//! `erars-ast/tests/restore_guard.rs`).

use erars_ast::{ScriptPosition, StrKey};
use erars_bytecode::{read_from, write_to};
use erars_vm::{FunctionBody, FunctionDic, Instruction};

#[test]
fn every_spent_magic_is_rejected_and_current_format_round_trips() {
    // Every magic value this arc has ever shipped under, except the current
    // one. A buffer starting with each, followed by bytes that would be
    // read as this crate's current `FunctionBody` layout if the magic check
    // were skipped or loose — the instruction stream is a raw memcpy of the
    // in-memory layout, so a reader that let a stale file through would
    // decode this tail as garbage `FunctionDic` content instead of refusing
    // to load it. That silent-misread is exactly the failure mode the
    // magic check exists to turn into a clean, up-front rejection.
    // 15 joined them when 16 added the cache-identity fingerprint word.
    //
    // The rejection is now a clean `InvalidData` error rather than a panic:
    // a stale cache is an ordinary thing to find on disk, and a panic under
    // `--quite` printed nothing at all.
    for spent_magic in [11u8, 12, 13, 14, 15] {
        let mut stale = vec![2u8, 3, 2, 3, 0, 0, 0, spent_magic];
        stale.extend_from_slice(&[0u8; 64]);

        let result = unsafe { read_from(&stale[..]) };
        let err = result
            .err()
            .unwrap_or_else(|| panic!("spent VERSION_MAGIC {spent_magic} must be rejected, not misread"));
        assert_eq!(err.kind(), std::io::ErrorKind::InvalidData);
    }

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
        body: Box::new([
            Instruction::nop(),
            Instruction::report_position(ScriptPosition { line: 4_000_000_000 }),
            Instruction::pop(),
            Instruction::report_position(ScriptPosition { line: 9 }),
        ]),
    };

    let mut dic = FunctionDic::new();
    dic.normal.insert(fn_name, body.clone());

    let mut buf = Vec::new();
    write_to(&mut buf, &dic, 0xfeed_beef_dead_1234).expect("write_to");

    // The current magic must be present and distinct from every spent one.
    assert_eq!(&buf[..7], &[2, 3, 2, 3, 0, 0, 0]);
    let current_magic = buf[7];
    assert!(
        ![11, 12, 13, 14, 15].contains(&current_magic),
        "current VERSION_MAGIC ({current_magic}) reuses a value this arc already spent"
    );

    let (dic2, fingerprint) = unsafe { read_from(&buf[..]).expect("read_from") };
    assert_eq!(
        fingerprint, 0xfeed_beef_dead_1234,
        "the cache-identity fingerprint must survive the round trip verbatim"
    );
    let body2 = dic2
        .normal
        .get(&fn_name)
        .expect("function survives the round trip");

    assert_eq!(body2.file_path(), body.file_path);
    assert_eq!(body2.is_function(), body.is_function);
    assert_eq!(body2.is_functions(), body.is_functions);
    assert_eq!(body2.body(), &*body.body);
}
