//! Dense, append-only store for the string literals of an ERB corpus.
//!
//! An interner answers "which key does this string have?", and for an
//! identifier that is the question: `LOCAL` appears in every one of a game's
//! thousands of files and has to come back as the same key each time, because
//! a key is what the variable dictionary is looked up by. A *literal* is asked
//! nothing of the sort. `PRINTFORML 彼女は少し困ったような顔をした。` names no
//! entity; the key it gets is only ever handed back to `resolve`, and the
//! sentence is, in a corpus of 61 MB, almost certainly unique.
//!
//! Answering it through `ThreadedRodeo::get_or_intern` anyway costs an ahash
//! of the whole sentence, a `DashMap` shard write lock contended by every
//! other parser thread, and a probe that chases a pointer into the interner's
//! arena to compare bytes it has never seen. Appending costs a `memcpy` into a
//! thread-local bump chunk and, fifteen times in sixteen, an increment of a
//! thread-local slot reservation.
//!
//! So literals are split by length. Anything up to [`WORD_LEN`] goes to
//! [`crate::intern_cached`] — the punctuation and particles between form-string
//! substitutions repeat relentlessly, and the thread-local memo in front of the
//! interner already answers a repeat without touching the map at all. Anything
//! longer is appended here, unexamined and undeduplicated.
//!
//! A key from this store is marked with [`LIT_BIT`], which `lasso` can never
//! set: it hands out keys from 1 upwards and a corpus would need two billion
//! distinct strings to reach it. [`crate::StrKey::resolve`] branches on that
//! bit; everything that uses a key as an *identity* — a function name, a
//! variable name, a `$LABEL` — calls [`crate::StrKey::to_global`] first, which
//! trades a literal key for the interned one.
//!
//! The store is indexed from 0 and dense, which is what lets `game.era` carry
//! it as a bare block of length-prefixed strings: writing slot `i` at position
//! `i` and restoring in the same order reproduces every key in every serialized
//! instruction byte-for-byte. A slot no thread got round to filling — the tail
//! of a reservation — reads as the empty string, so a hole costs four bytes in
//! the file and nothing else.

use std::{
    alloc::{alloc, handle_alloc_error, Layout},
    cell::Cell,
    ptr,
    sync::atomic::{AtomicPtr, AtomicU32, Ordering},
};

use crate::{get_interner, intern_cache::WORD_LEN, StrKey};

/// Set on every [`StrKey`] that indexes this store rather than the interner.
pub const LIT_BIT: u32 = 1 << 31;

/// Slots, and so the largest number of long literals a corpus may hold.
///
/// eraMegaten, the biggest corpus at hand, fills about a fifth of this. The
/// cost of the reservation is a page table, not memory: the slot array lives
/// in `.bss`, so an unused slot is a zero page that is never faulted in.
/// Overflowing it is not an error — [`append_intern`] falls back to the
/// interner, which is merely slower.
pub const LIT_CAP: usize = 1_000_000;

/// Bytes of the length that precedes a literal's bytes in the arena.
///
/// Storing the length with the string rather than beside the pointer keeps a
/// slot at eight bytes — one atomic, no torn read to reason about — and puts
/// the length on the same cache line as the bytes the caller is about to read
/// anyway.
const HEADER: usize = 4;

/// Bump chunk size. Large enough that a chunk holds hundreds of sentences,
/// small enough that a thread which interns nothing wastes nothing.
const CHUNK: usize = 64 * 1024;

/// `SLOTS[i]` points at the length-prefixed bytes of literal `i`, or is null
/// when literal `i` is the empty string — which is why slot 0, the key
/// `LIT_BIT` alone, needs no arena allocation at all.
static SLOTS: [AtomicPtr<u8>; LIT_CAP] = [const { AtomicPtr::new(ptr::null_mut()) }; LIT_CAP];

/// Next unreserved slot. Slot 0 is `""` and is never handed out by a reservation.
static NEXT_SLOT: AtomicU32 = AtomicU32::new(1);

/// Slots a thread claims per `NEXT_SLOT` round trip.
const BLOCK: u32 = 16;

/// Bumped by every reset, so a thread cannot spend a reservation it made
/// before the store was emptied under it.
static GENERATION: AtomicU32 = AtomicU32::new(0);

thread_local! {
    /// `(cursor, bytes left)` of the calling thread's current bump chunk.
    static BUMP: Cell<(*mut u8, usize)> = const { Cell::new((ptr::null_mut(), 0)) };
    /// `(generation, next slot, slots left)` of the calling thread's reservation.
    static RESERVED: Cell<(u32, u32, u32)> = const { Cell::new((0, 0, 0)) };
}

/// Claim one slot, in blocks, so that appending a literal is an uncontended
/// thread-local increment fifteen times out of sixteen.
///
/// A thread that stops appending mid-block leaves its tail unused, and those
/// slots resolve to `""` like any other null slot.
fn reserve_slot() -> u32 {
    let generation = GENERATION.load(Ordering::Relaxed);
    let (reserved_at, next, left) = RESERVED.get();

    if left > 0 && reserved_at == generation {
        RESERVED.set((generation, next + 1, left - 1));
        return next;
    }

    let base = NEXT_SLOT.fetch_add(BLOCK, Ordering::Relaxed);
    RESERVED.set((generation, base + 1, BLOCK - 1));

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
    ptr
}

/// Copy `s` into the calling thread's arena, prefixed with its length.
fn arena_alloc(s: &str) -> *mut u8 {
    debug_assert!(!s.is_empty());

    // Round up so the next allocation's header stays aligned.
    let need = (HEADER + s.len() + HEADER - 1) & !(HEADER - 1);
    let (cur, left) = BUMP.get();

    let ptr = if left >= need {
        BUMP.set((unsafe { cur.add(need) }, left - need));
        cur
    } else if need > CHUNK {
        // A literal too big for a chunk gets its own allocation, and the
        // current chunk keeps its remainder for the next ordinary one.
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

/// The literal at `idx`, where `idx` is a [`StrKey`] with [`LIT_BIT`] cleared.
#[inline]
pub(crate) fn resolve_literal(idx: u32) -> &'static str {
    let ptr = SLOTS[idx as usize].load(Ordering::Acquire);

    // Null is the empty string: slot 0 always, and any slot a `restore_literals`
    // filled from an empty entry.
    if ptr.is_null() {
        return "";
    }

    unsafe {
        let len = ptr.cast::<u32>().read() as usize;
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr.add(HEADER), len))
    }
}

/// Append `s` and return its key, without asking whether it is already stored.
pub fn append_intern(s: &str) -> StrKey {
    if s.is_empty() {
        return StrKey::from_u32(LIT_BIT);
    }

    let idx = reserve_slot();

    if idx as usize >= LIT_CAP {
        // Clamp, so a load long enough to reserve four billion literals cannot
        // wrap the counter back into the live range.
        NEXT_SLOT.store(LIT_CAP as u32, Ordering::Relaxed);
        RESERVED.set((GENERATION.load(Ordering::Relaxed), LIT_CAP as u32, 0));
        return get_interner().get_or_intern(s);
    }

    let ptr = arena_alloc(s);

    // Release pairs with the `Acquire` in `resolve_literal`: a thread that gets
    // hold of this key sees the bytes it points at.
    SLOTS[idx as usize].store(ptr, Ordering::Release);

    StrKey::from_u32(LIT_BIT | idx)
}

/// The key for an ERB string literal.
///
/// Short literals go through the interner's thread-local memo, which
/// deduplicates the fragments that repeat — a hit costs no atomic and no hash
/// of anything but 16 bytes. Long ones are appended unexamined, because a
/// sentence is unique and hashing it only to discover that is the cost this
/// store exists to avoid.
#[inline]
pub fn intern_literal(s: &str) -> StrKey {
    if s.len() <= WORD_LEN {
        crate::intern_cached(s)
    } else {
        append_intern(s)
    }
}

/// Slots claimed, counting slot 0 and the unfilled tail of every thread's last
/// reservation.
pub fn literal_store_len() -> usize {
    (NEXT_SLOT.load(Ordering::Acquire) as usize).min(LIT_CAP)
}

/// Every stored literal, in slot order — index `i` is the literal of key
/// `LIT_BIT | i`.
pub fn literal_store_strings() -> Vec<&'static str> {
    (0..literal_store_len() as u32).map(resolve_literal).collect()
}

/// Refill the store from `strings`, so that `LIT_BIT | i` resolves to
/// `strings[i]` again.
///
/// The inverse of [`literal_store_strings`], and the reason a `game.era` can
/// carry raw instruction bytes: the keys in them are slot indices, so restoring
/// the slots in order restores the keys.
pub fn restore_literals(strings: &[&str]) {
    assert!(
        strings.len() <= LIT_CAP,
        "literal store overflow: {} strings, capacity {LIT_CAP}",
        strings.len()
    );

    reset_literal_store();

    for (idx, s) in strings.iter().enumerate() {
        if !s.is_empty() {
            SLOTS[idx].store(arena_alloc(s), Ordering::Release);
        }
    }

    // Slot 0 is `""` whether or not the file said so.
    NEXT_SLOT.store(strings.len().max(1) as u32, Ordering::Release);
}

/// Drop every literal, leaving only slot 0.
///
/// The arena is not reclaimed — a literal key handed out before the reset would
/// otherwise dangle, and this runs when a game is reloaded or between tests,
/// not in a loop.
pub fn reset_literal_store() {
    for slot in SLOTS[..literal_store_len()].iter() {
        slot.store(ptr::null_mut(), Ordering::Relaxed);
    }

    // Retire every outstanding reservation before the counter moves back, or a
    // thread holding one would write past `NEXT_SLOT` and lose the literals a
    // later append stored there.
    GENERATION.fetch_add(1, Ordering::Relaxed);
    NEXT_SLOT.store(1, Ordering::Release);
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `NEXT_SLOT` is process-global, so the cases run in sequence under one
    /// `#[test]` rather than racing each other's reservations.
    #[test]
    fn literal_store() {
        crate::init_interner();
        appended_literals_resolve();
        length_decides_which_half_stores_a_literal();
        reset_retires_outstanding_reservations();
    }

    fn appended_literals_resolve() {
        reset_literal_store();

        let long = "彼女は少し困ったような顔をして、それから小さく笑った。";
        let sentences = [long, "a".repeat(CHUNK * 2).as_str(), "", "x"]
            .map(|s| (s.to_owned(), append_intern(s)));

        for (s, key) in &sentences {
            assert!(key.is_literal(), "{s:?}");
            assert_eq!(key.resolve(), s.as_str());
            assert_eq!(key.to_string(), *s);
        }

        // Empty appends share slot 0 and reserve nothing.
        assert_eq!(sentences[2].1.to_u32(), LIT_BIT);

        // Two appends of one string are two slots: the store does not ask.
        assert_ne!(append_intern(long), append_intern(long));

        let strings = literal_store_strings();
        assert_eq!(strings.len(), literal_store_len());
        assert_eq!(strings[0], "");
        for (s, key) in &sentences {
            assert_eq!(strings[(key.to_u32() & !LIT_BIT) as usize], s.as_str());
        }

        // A round trip through the serialized form reproduces every key.
        let owned: Vec<String> = strings.iter().map(|s| s.to_string()).collect();
        let borrowed: Vec<&str> = owned.iter().map(String::as_str).collect();
        restore_literals(&borrowed);
        assert_eq!(literal_store_len(), owned.len());
        for (s, key) in &sentences {
            assert_eq!(key.resolve(), s.as_str());
        }

        reset_literal_store();
        assert_eq!(literal_store_len(), 1);
    }

    /// The case the generation counter exists for: a thread holding a block
    /// reserved before a reset must not keep filling slots the reset put back
    /// beyond `NEXT_SLOT`, or the literals it stores there are invisible to
    /// `literal_store_strings` and never reach the file.
    fn reset_retires_outstanding_reservations() {
        let long = "a".repeat(WORD_LEN + 1);

        reset_literal_store();

        // Spend enough of a block that the next append would come out of it.
        for _ in 0..BLOCK / 2 {
            append_intern(&long);
        }

        reset_literal_store();

        let key = append_intern(&long);
        assert_eq!(key.to_u32(), LIT_BIT | 1);

        // The literal is where the serializer will look for it, and the tail of
        // the fresh block reads as the empty string rather than as the text the
        // reset dropped.
        let strings = literal_store_strings();
        assert_eq!(strings[1], long);
        assert!(strings[2..].iter().all(|s| s.is_empty()), "{strings:?}");
    }

    fn length_decides_which_half_stores_a_literal() {
        let short = "、そして";
        assert!(short.len() <= WORD_LEN);
        assert!(!intern_literal(short).is_literal());
        assert_eq!(intern_literal(short), intern_literal(short));
        assert_eq!(intern_literal(short), crate::intern_cached(short));

        let long = "a".repeat(WORD_LEN + 1);
        assert!(intern_literal(&long).is_literal());
        assert_eq!(intern_literal(&long).resolve(), long);

        // A global key keeps its own identity under `to_global`, and a literal
        // one is traded for the interned key of the same text.
        let global = crate::intern_cached(&long);
        assert!(!global.is_literal());
        assert_eq!(global.to_global(), global);
        assert_eq!(intern_literal(&long).to_global(), global);
    }
}
