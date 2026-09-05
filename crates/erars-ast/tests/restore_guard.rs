//! `Interner::restore` (used to reload `game.era`) writes directly into the
//! live global slots and dedup map instead of installing a fresh store, so it
//! must refuse to run once the store already has real entries — otherwise a
//! `StrKey` some earlier caller already holds would silently start resolving
//! different content. A file under `tests/` builds as its own process, which
//! is what makes the "store starts out empty" half of this test meaningful:
//! in `src/interner.rs`'s own unit tests, several sibling tests intern things
//! into the same process-global store first, so a call to `restore` there is
//! never the first thing to touch it.
//!
//! Both halves live in one `#[test]` function on purpose: cargo may run the
//! functions in one test binary concurrently, and this crate's interner is a
//! single set of process-global statics with no per-test isolation, so a
//! second `#[test]` in this file touching the interner could race this one's
//! "still empty" assumption.

use erars_ast::{Interner, StrKey};

#[test]
fn restore_preserves_gaps_then_refuses_a_second_call() {
    let interner = Interner::new();

    // Nothing has touched the interner yet in this process, so this is
    // exactly what a fresh `read_from` does: restore keys with a gap between
    // them (a reservation block that a run only partly filled).
    let pairs = [(500_000u32, "test_restore_a"), (500_002u32, "test_restore_b")];
    interner.restore(&pairs);

    assert_eq!(interner.resolve(&StrKey::from_u32(500_000)), "test_restore_a");
    assert_eq!(interner.resolve(&StrKey::from_u32(500_001)), "");
    assert_eq!(interner.resolve(&StrKey::from_u32(500_002)), "test_restore_b");

    assert_eq!(interner.get_or_intern("test_restore_a"), StrKey::from_u32(500_000));
    assert_eq!(interner.get_or_intern("test_restore_b"), StrKey::from_u32(500_002));

    // A new identifier lands after the restored range, not in its gap.
    let fresh = interner.get_or_intern("test_restore_fresh_after_gap");
    assert!(fresh.to_u32() > 500_002);

    // A second `restore` — the scenario a `load_script` called twice in one
    // process, or a `run_script` followed by a `--load`, would produce — must
    // panic instead of silently overwriting slot 500_000's bytes out from
    // under the `StrKey`s this test is still holding.
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        Interner::new().restore(&[(500_000, "test_restore_replaced")]);
    }));
    assert!(result.is_err(), "restore must refuse to run on a non-empty store");

    // The refused call must not have touched anything.
    assert_eq!(interner.resolve(&StrKey::from_u32(500_000)), "test_restore_a");
}
