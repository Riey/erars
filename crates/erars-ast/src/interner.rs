//! Dense, deduplicating store for identifiers — the interner's other half.
//!
//! [`crate::literal_store`] exists because a literal is never looked up by
//! content again; an identifier is the opposite case. `LOCAL` has to come
//! back as the same [`StrKey`] every time it is interned, because that key is
//! what the variable dictionary, the function table and every `#DIM` are
//! keyed by. So this store still needs the thing a literal never does: a
//! concurrent index from string content to key.
//!
//! `lasso::ThreadedRodeo` answered that with two `DashMap`s (`&str -> key` and
//! `key -> &str`) sitting in front of a `LockfreeArena`. The second map is
//! pure overhead — every `StrKey::resolve()` (called, among other places, for
//! every parsed variable reference in `variable_arg`) paid a `DashMap` hash
//! and shard lock for an answer a plain array index already has once a key is
//! nothing but a slot number. And the arena had a real bug: `store_str`
//! doubles a bucket's capacity whenever a thread finds no room, uninterlocked,
//! so two threads racing the same full bucket each allocate their own
//! doubled replacement — the loser's allocation is never freed. Measured on
//! eraMegaten, that tail put a 22 MB interner at 64 MB on 2 of 14 runs.
//!
//! This store keeps exactly one concurrent map — content to key, the part
//! with no substitute — and answers everything else the way
//! [`crate::literal_store`] does: a dense `SLOTS` array indexed directly by
//! key, filled through thread-local 8 KiB bump chunks claimed in blocks of
//! [`BLOCK`] so that appending is an uncontended thread-local increment
//! fifteen times out of sixteen, exactly as measured safe for literals. No
//! allocation ever doubles, so there is no tail to have.

use std::{
    alloc::{alloc, handle_alloc_error, Layout},
    cell::Cell,
    ptr,
    sync::{
        atomic::{AtomicPtr, AtomicU32, AtomicUsize, Ordering},
        LazyLock,
    },
};

use dashmap::{mapref::entry::Entry, DashMap};

use crate::StrKey;

/// Bytes of the length that precedes an identifier's bytes in the arena.
const HEADER: usize = 4;

/// Bump chunk size. Identifiers are short and the overwhelming majority of
/// `get_or_intern` calls are repeats that never reach this store at all (the
/// per-thread [`crate::intern_cache`] catches those, and a hit on *this*
/// store's own dedup map catches an already-globally-known name on a thread
/// that has not personally interned it yet) — so a thread that does append
/// something new usually appends very few of them, and a smaller chunk than
/// the literal store's wastes less on threads that touch this arena lightly.
const CHUNK: usize = 8 * 1024;

/// Slots a thread claims per counter round trip; see
/// `crate::literal_store::BLOCK` for the measurement (plain
/// `fetch_add(1)` cost eraTHYMKR's parallel wall clock +27%) that fixed this
/// number for that store and applies unchanged here.
const BLOCK: u32 = 16;

/// Slots, and so the largest number of distinct identifiers a corpus may
/// hold. eraMegaten's corpus needs about 200_000; this is twenty times that,
/// and — like `crate::literal_store::LIT_CAP` — the cost of the headroom is a
/// page table entry, not memory: the array lives in `.bss` and an unfilled
/// slot is a zero page that is never faulted in.
const ID_CAP: usize = 4_000_000;

/// `SLOTS[i]` points at the length-prefixed bytes of identifier key `i`, or is
/// null when identifier `i` is the empty string.
static SLOTS: [AtomicPtr<u8>; ID_CAP] = [const { AtomicPtr::new(ptr::null_mut()) }; ID_CAP];

/// Next unreserved slot. Slot 0 is never handed out: [`StrKey`] is backed by
/// a `NonZeroU32`, so 0 is not a representable key.
static NEXT_SLOT: AtomicU32 = AtomicU32::new(1);

thread_local! {
    /// `(cursor, bytes left)` of the calling thread's current bump chunk.
    static BUMP: Cell<(*mut u8, usize)> = const { Cell::new((ptr::null_mut(), 0)) };
    /// `(next slot, slots left)` of the calling thread's reservation.
    static RESERVED: Cell<(u32, u32)> = const { Cell::new((0, 0)) };
}

/// Content-to-key index — the one map this store keeps, because identity is
/// the one question [`crate::literal_store`] never has to answer.
type Dedup = DashMap<&'static str, StrKey, ahash::RandomState>;

static DEDUP: LazyLock<Dedup> = LazyLock::new(|| DashMap::with_hasher(ahash::RandomState::default()));

/// Bytes handed out to arena chunks so far — the arena's reserved capacity,
/// not the bytes actually written into it, matching what
/// `lasso::ThreadedRodeo::current_memory_usage` reported for the store this
/// replaces. Every chunk this counts is a fixed [`CHUNK`] (or, rarely, one
/// exact-fit allocation for a string longer than that), so unlike the old
/// `LockfreeArena` a thread can never inflate it by racing another thread for
/// room in a bucket both are about to replace.
static ARENA_BYTES: AtomicUsize = AtomicUsize::new(0);

/// Claim one slot, in blocks, so that registering a new identifier is an
/// uncontended thread-local increment fifteen times out of sixteen.
fn reserve_slot() -> u32 {
    let (next, left) = RESERVED.get();

    if left > 0 {
        RESERVED.set((next + 1, left - 1));
        return next;
    }

    let base = NEXT_SLOT.fetch_add(BLOCK, Ordering::Relaxed);
    assert!(
        (base as usize) < ID_CAP,
        "identifier interner overflow: capacity {ID_CAP}"
    );
    RESERVED.set((base + 1, BLOCK - 1));

    base
}

fn alloc_bytes(size: usize) -> *mut u8 {
    // Aligned for the `u32` header; the arena is never freed, so the pointers
    // it hands out are `'static`.
    let layout = Layout::from_size_align(size, HEADER).unwrap();
    let ptr = unsafe { alloc(layout) };
    if ptr.is_null() {
        handle_alloc_error(layout);
    }
    ARENA_BYTES.fetch_add(size, Ordering::Relaxed);
    ptr
}

/// Copy `s` into the calling thread's arena, prefixed with its length.
fn arena_alloc(s: &str) -> *mut u8 {
    debug_assert!(!s.is_empty());

    let need = (HEADER + s.len() + HEADER - 1) & !(HEADER - 1);
    let (cur, left) = BUMP.get();

    let ptr = if left >= need {
        BUMP.set((unsafe { cur.add(need) }, left - need));
        cur
    } else if need > CHUNK {
        alloc_bytes(need)
    } else {
        let chunk = alloc_bytes(CHUNK);
        BUMP.set((unsafe { chunk.add(need) }, CHUNK - need));
        chunk
    };

    unsafe {
        ptr.cast::<u32>().write(s.len() as u32);
        ptr::copy_nonoverlapping(s.as_ptr(), ptr.add(HEADER), s.len());
    }

    ptr
}

/// The identifier at key `idx`. `idx` is a [`StrKey`] with the literal bit
/// clear — every caller through [`crate::StrKey::resolve`] already branched
/// on that.
#[inline]
pub(crate) fn resolve(idx: u32) -> &'static str {
    let ptr = SLOTS[idx as usize].load(Ordering::Acquire);

    if ptr.is_null() {
        return "";
    }

    unsafe {
        let len = ptr.cast::<u32>().read() as usize;
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr.add(HEADER), len))
    }
}

/// Register `s` as a brand new identifier and return its key, without asking
/// whether it is already stored — the caller has just asked [`DEDUP`] that.
fn store_new(s: &str) -> StrKey {
    let idx = reserve_slot();

    if !s.is_empty() {
        let ptr = arena_alloc(s);
        // Release pairs with the `Acquire` in `resolve`: a thread that gets
        // hold of this key sees the bytes it points at.
        SLOTS[idx as usize].store(ptr, Ordering::Release);
    }

    StrKey::from_u32(idx)
}

/// `get_or_intern`, retried once against a key another thread just won the
/// race to register — the arena slot `store_new` claimed for the loser is
/// wasted, exactly as a hole in a reservation block is: it costs the array
/// entry it was never going to get back and nothing else.
pub fn get_or_intern(s: &str) -> StrKey {
    if let Some(key) = DEDUP.get(s) {
        return *key;
    }

    let key = store_new(s);
    let stored: &'static str = resolve(key.to_u32());

    match DEDUP.entry(stored) {
        Entry::Occupied(e) => *e.get(),
        Entry::Vacant(e) => {
            e.insert(key);
            key
        }
    }
}

/// The key for `s`, without registering it if it is not already known.
pub fn get(s: &str) -> Option<StrKey> {
    DEDUP.get(s).map(|key| *key)
}

/// Identifiers registered so far — the count of real entries in [`DEDUP`],
/// which is not [`NEXT_SLOT`]: a reservation block a thread stopped short of
/// filling leaves slots that were never handed to any caller as a key, and so
/// were never inserted here either.
pub fn len() -> usize {
    DEDUP.len()
}

/// Bytes reserved by the arena so far. See [`ARENA_BYTES`].
pub fn current_memory_usage() -> usize {
    ARENA_BYTES.load(Ordering::Relaxed)
}

/// Every registered identifier with its key, in no particular order — the
/// order [`crate::literal_store`] cannot avoid caring about does not matter
/// here, because [`restore`] takes each key explicitly instead of replaying
/// insertions positionally.
pub fn iter() -> impl Iterator<Item = (StrKey, &'static str)> {
    DEDUP.iter().map(|entry| (*entry.value(), *entry.key()))
}

/// Refill the store from `(key, string)` pairs written by [`iter`], so that
/// `key` resolves to `string` again and future `get_or_intern` calls answer
/// with the same key a caller already has compiled into an instruction.
///
/// Unlike [`crate::literal_store::restore_literals`] this is not positional:
/// a reservation block can leave a gap between one run's assigned keys, and
/// replaying `iter`'s order into fresh sequential keys would shift every
/// identifier after the first gap onto the wrong number. Placing each string
/// at the exact key `iter` reported side-steps that; the only cost is a
/// second `u32` per identifier in the file.
pub fn restore(pairs: &[(u32, &str)]) {
    let mut next = NEXT_SLOT.load(Ordering::Relaxed).max(1);

    for &(key, s) in pairs {
        assert!(
            (key as usize) < ID_CAP,
            "identifier interner overflow: capacity {ID_CAP}"
        );

        if !s.is_empty() {
            SLOTS[key as usize].store(arena_alloc(s), Ordering::Release);
        }

        next = next.max(key + 1);
        DEDUP.insert(resolve(key), StrKey::from_u32(key));
    }

    NEXT_SLOT.store(next, Ordering::Release);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dedups_across_calls() {
        let a = get_or_intern("test_dedups_across_calls_first");
        let b = get_or_intern("test_dedups_across_calls_first");
        assert_eq!(a, b);

        let c = get_or_intern("test_dedups_across_calls_second");
        assert_ne!(a, c);

        assert_eq!(resolve(a.to_u32()), "test_dedups_across_calls_first");
        assert_eq!(resolve(c.to_u32()), "test_dedups_across_calls_second");
    }

    #[test]
    fn empty_string_round_trips() {
        let key = get_or_intern("test_empty_string_round_trips_sentinel_never_matches");
        assert_ne!(resolve(key.to_u32()), "");

        let empty = get_or_intern("");
        assert_eq!(resolve(empty.to_u32()), "");
        assert_eq!(get_or_intern(""), empty);
    }

    #[test]
    fn restore_preserves_exact_keys_across_a_gap() {
        let pairs = [(500_000u32, "test_restore_a"), (500_002u32, "test_restore_b")];
        restore(&pairs);

        assert_eq!(resolve(500_000), "test_restore_a");
        assert_eq!(resolve(500_001), "");
        assert_eq!(resolve(500_002), "test_restore_b");

        assert_eq!(get_or_intern("test_restore_a"), StrKey::from_u32(500_000));
        assert_eq!(get_or_intern("test_restore_b"), StrKey::from_u32(500_002));

        // A new identifier lands after the restored range, not in its gap.
        let fresh = get_or_intern("test_restore_fresh_after_gap");
        assert!(fresh.to_u32() > 500_002);
    }

    #[test]
    fn concurrent_first_touch_agrees_on_one_key() {
        use std::sync::Barrier;

        let barrier = std::sync::Arc::new(Barrier::new(8));
        let word = "test_concurrent_first_touch_agrees_on_one_key";

        let handles: Vec<_> = (0..8)
            .map(|_| {
                let barrier = barrier.clone();
                std::thread::spawn(move || {
                    barrier.wait();
                    get_or_intern(word)
                })
            })
            .collect();

        let keys: Vec<StrKey> = handles.into_iter().map(|h| h.join().unwrap()).collect();
        assert!(keys.iter().all(|k| *k == keys[0]));
    }
}
