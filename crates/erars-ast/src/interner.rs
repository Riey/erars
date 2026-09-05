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
//!
//! [`DEDUP`] itself was `DashMap` until it was measured against a synthetic
//! per-call `rdtsc` timer on eraTHYMKR and eraMegaten: of every call that
//! reaches this store, 56-79% are a *first-ever* insertion of a string no
//! thread has interned before (confirmed by matching the miss counter
//! exactly against `len()`'s growth across the measurement window), and that
//! write path is what widens 3.4-4.1x under 32-thread contention — not a
//! cache-shaped read problem any snapshot or bigger cache could recover.
//! `DashMap`'s per-shard `RwLock` pays for that widening in cache-line
//! bouncing on every insert; `papaya`'s lock-free table does not, at roughly
//! 2-3x less aggregate CPU time in this path on both corpora under the same
//! 32-thread load. The gain depends entirely on avoiding `papaya`'s
//! incremental-resize tax, which taxes *every* concurrent operation while a
//! resize is in flight, not just inserts — starting the table at its default
//! empty capacity regressed both hit and miss costs by an order of
//! magnitude versus `DashMap`, matching papaya's own documented weakness on
//! insert-heavy workloads. [`DEDUP`] is built with a capacity chosen well
//! above eraMegaten's ~200k identifiers up front instead.

use std::{
    alloc::{alloc, handle_alloc_error, Layout},
    cell::Cell,
    ptr,
    sync::{
        atomic::{AtomicPtr, AtomicU32, AtomicUsize, Ordering},
        LazyLock,
    },
};

use papaya::HashMap as PapayaMap;

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
type Dedup = PapayaMap<&'static str, StrKey, ahash::RandomState>;

/// [`DEDUP`]'s starting capacity, and the reason it is not left at `papaya`'s
/// default of zero.
///
/// `papaya`'s incremental resize means a table growing towards its capacity
/// makes every concurrent operation — reads included — search both the old
/// and new table while a resize is in flight; a table built at capacity zero
/// is *always* mid-resize under a whole corpus load, since it never stops
/// growing. Measured on both corpora: an unsized [`DEDUP`] cost 5-15x more
/// per call than the `DashMap` it replaced, on *every* operation, hits
/// included — not the improvement the module doc comment describes, its
/// mirror image. Presizing past the load fixes it outright.
///
/// Unlike [`SLOTS`], this table is a real allocation, not a `.bss` array of
/// zero pages that costs nothing unfilled, so the headroom this constant
/// buys is not free the way [`ID_CAP`]'s twenty-times margin is. A first cut
/// picked `1_000_000` on the reasoning that five times eraMegaten's real
/// ~198k identifiers was a conservative-but-affordable margin; measured
/// against a proper sweep, it was neither. Presizing to 262_144, 524_288 and
/// 1_048_576 and comparing peak RSS through the full `phases` pipeline and
/// per-call cost through the isolated interner path (both corpora, serial
/// and 32-thread) found:
///
/// - Peak RSS rises monotonically and by a real amount across that range —
///   +16 MB on eraTHYMKR, +38 MB on eraMegaten between the smallest and
///   largest candidate — because every one of these candidates is already
///   far past the load `papaya` would ever resize at, so the only thing a
///   bigger capacity buys past that point is more empty table.
/// - Per-call cost does not improve past the smallest candidate. If
///   anything it measured slightly *worse* at the largest: a bigger, sparser
///   table spreads live entries over more cache lines and TLB entries for
///   the same lookup, and there is no resize tax left to amortize away once
///   every candidate is already sized well clear of that threshold.
///
/// So the smallest candidate measured, not the largest, is the balance
/// struck: `262_144`, about 2.65x eraMegaten's real ~198k identifiers —
/// enough that nothing several times larger would cross into resize range,
/// at the cheapest point on both curves measured, with nothing traded away
/// for it.
///
/// `papaya` does not allocate this many slots: `HashMap::builder().capacity`
/// is a *load* target, converted to a table size internally as
/// `next_power_of_two(capacity * 8 / 6)` (`papaya::raw::probe::entries_for`)
/// so the table stays under its own resize threshold at the requested load.
/// `262_144` requested is a `524_288`-slot table; the `1_000_000` this
/// replaces was actually a `2_097_152`-slot table, more than double what its
/// own name suggested — a rounder decimal constant does not buy a rounder
/// allocation, `papaya` rounds it to the next power of two regardless, so a
/// requested capacity that is not already a power of two is simply
/// misleading about what it allocates. Picking `262_144` outright sidesteps
/// that: what is requested is what the load-factor math would have rounded
/// any nearby value to anyway.
///
/// If a corpus ever does exceed this, correctness is unaffected — `papaya`
/// resizes and every key is still found — but every concurrent operation
/// pays the incremental-resize tax above until the table catches up, i.e.
/// this whole store degrades toward the "unsized" case measured above for
/// as long as growth continues past this line. That is a performance cliff
/// to notice happening, not a bug to fix reactively: raise the constant
/// before eraMegaten's own identifier count gets within reach of it.
const DEDUP_CAPACITY: usize = 262_144;

static DEDUP: LazyLock<Dedup> = LazyLock::new(|| {
    PapayaMap::builder()
        .capacity(DEDUP_CAPACITY)
        .hasher(ahash::RandomState::default())
        .build()
});

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
///
/// A `papaya` operation needs a guard (`DEDUP.guard()`), and this used to be
/// a thread-local pinned once and reused for the thread's lifetime, on the
/// reasoning that pinning once is cheaper than pinning per call and that
/// [`DEDUP`] only ever grows, so nothing here could be blocked behind
/// reclaiming a guard held too long. Measured against a fresh `DEDUP.guard()`
/// per call — alternating which one ran first across repeated rounds after a
/// warm-up pass, since testing them back to back once made whichever ran
/// *second* look faster purely from the table already being warm in cache —
/// the fresh guard was consistently ~2x faster, not merely close, on both
/// corpora, serial and 32-thread alike. `papaya`'s guard is built to be
/// cheap enough to pin per operation; a `thread_local!` with a non-`const`
/// initializer pays a per-access state check (initialized? being
/// initialized? already torn down?) that a plain fresh pin skips, and that
/// check cost more than the pin it was supposed to save. Caching it was also
/// the wrong call for reasons beyond speed: a long-held guard blocks
/// reclaiming anything `papaya` retires while it is pinned, and a resize
/// retires the table it replaces — so a cached guard would have quietly
/// leaked the old table for the process's lifetime the first time [`DEDUP`]
/// ever grew past [`DEDUP_CAPACITY`]. Pinning fresh removes that coupling
/// entirely instead of documenting it as a hazard to watch for.
pub fn get_or_intern(s: &str) -> StrKey {
    let guard = DEDUP.guard();
    if let Some(key) = DEDUP.get(s, &guard) {
        return *key;
    }

    let key = store_new(s);
    let stored: &'static str = resolve(key.to_u32());

    *DEDUP.get_or_insert_with(stored, || key, &guard)
}

/// The key for `s`, without registering it if it is not already known. See
/// [`get_or_intern`] for why the guard is pinned fresh here rather than
/// cached.
pub fn get(s: &str) -> Option<StrKey> {
    DEDUP.get(s, &DEDUP.guard()).copied()
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
    let guard = DEDUP.guard();
    DEDUP
        .iter(&guard)
        .map(|(k, v)| (*v, *k))
        .collect::<Vec<_>>()
        .into_iter()
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
///
/// Also unlike `restore_literals`, this never resets first: it writes
/// straight into the live [`SLOTS`]/[`DEDUP`], so a key already in use before
/// this call would have its slot silently overwritten with whatever `pairs`
/// puts there instead, and any `StrKey` a caller already holds into that slot
/// would start resolving to different content with no error at all — a
/// literal key is thrown away every parse and nothing outlives that, but an
/// identifier key is a permanent identity kept for the life of the process
/// (a variable name, a function name, a `$LABEL`), so silently repointing one
/// is a correctness bug, not a wasted allocation. Every real caller loads
/// `game.era` as the very first thing that touches the interner in a fresh
/// process (`load_script`, `crates/erars-loader/src/lib.rs`), so the store is
/// always empty here; panic instead of merging if that is ever not true,
/// rather than resolving wrong silently. There is no legitimate "restore
/// again to replace what is already there" use, so unlike `literal_store`
/// this store carries no `GENERATION` counter to support one.
pub fn restore(pairs: &[(u32, &str)]) {
    assert!(
        NEXT_SLOT.load(Ordering::Relaxed) <= 1 && DEDUP.len() == 0,
        "Interner::restore called on a non-empty interner ({} identifiers \
         already registered): restore writes directly into the live global \
         slots and dedup map instead of installing a fresh store, so a \
         caller already holding a StrKey into it would silently start \
         resolving a different string. Load game.era as the first thing \
         that touches the interner in a fresh process.",
        DEDUP.len(),
    );

    let mut next = 1u32;
    let guard = DEDUP.guard();

    for &(key, s) in pairs {
        assert!(
            (key as usize) < ID_CAP,
            "identifier interner overflow: capacity {ID_CAP}"
        );

        if !s.is_empty() {
            SLOTS[key as usize].store(arena_alloc(s), Ordering::Release);
        }

        next = next.max(key + 1);
        DEDUP.insert(resolve(key), StrKey::from_u32(key), &guard);
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
