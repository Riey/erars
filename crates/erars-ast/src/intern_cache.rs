//! Per-thread memo in front of the global interner.

use crate::{get_interner, StrKey};

/// Bytes of a word stored inline in a slot; longer strings bypass the cache.
///
/// It has to hold an identifier — the longest in the eraTHYMKR corpus that is
/// asked for more than once is 21 bytes — and the short literal fragments
/// between form-string substitutions, which are punctuation and particles:
/// UTF-8 Korean and Japanese run three bytes to the character, so 24 bytes is
/// eight of them. Anything longer is a sentence, and a sentence is almost
/// always unique — caching those is what makes a memo lose (see below).
const WORD_LEN: usize = 24;

/// Slots, a power of two. 1024 of them is 32 KiB, which stays in L2 next to
/// everything else the parser touches.
///
/// Size is the whole point. A `HashMap<&'static str, StrKey>` that simply
/// remembers everything reaches 335_058 entries on this corpus, and then every
/// lookup is three cache misses — the control bytes, the entry, and the string
/// the entry points at, which lives off in the interner's arena. Measured, it
/// cost *exactly as much as the DashMap it was shadowing*: `DashMap::_get`
/// fell from 11.4% to 1.9% of parse+compile self time while the memo itself
/// rose from 5.8% to 15.9%, for no net gain, and 32 threads each holding a
/// 10 MiB copy made the parallel load 19% slower.
///
/// A slot array small enough to stay resident, holding its words inline so a
/// compare never leaves it, wins the same lookups without either cost. The
/// distribution is steeply skewed — 27% of instruction lines begin with
/// `PRINTFORMW` alone — so a small cache keeps the hot end and lets the tail
/// go to the interner, which is where the tail belongs.
const SLOTS: usize = 1024;

/// `len` of a slot that has never been filled. Real lengths are `0..=WORD_LEN`
/// and the empty string is a legitimate key, so it needs a value of its own.
const EMPTY: u8 = u8::MAX;

#[derive(Clone, Copy)]
struct Slot {
    word: [u8; WORD_LEN],
    len: u8,
    key: Option<StrKey>,
}

impl Slot {
    const EMPTY: Self = Self {
        word: [0; WORD_LEN],
        len: EMPTY,
        key: None,
    };
}

/// Direct-mapped, thread-local memo for `Interner::get_or_intern`.
///
/// `ThreadedRodeo::get_or_intern` is a `DashMap` lookup: an ahash of the whole
/// string, a shard read lock — an atomic to take and an atomic to release,
/// contended by every other parser thread — and a probe that chases a pointer
/// into the arena to compare. Together with the miss path it was the largest
/// single cost of loading a game: `DashMap::_get` 6.2%, `DashMap::_insert`
/// 5.2% and `hash_one` 5.2% of total runtime.
///
/// The questions repeat relentlessly. `LOCAL`, `ARG`, `RESULT`, `TARGET`, the
/// CSV variable names and the punctuation between form-string substitutions
/// recur in every one of a game's thousands of ERBs, and a `ParserContext` —
/// and any cache it owns — is built fresh for each of those files, so the memo
/// has to outlive a file to answer any of it. Thread-local is the widest scope
/// that still costs nothing to consult: no lock, no atomic, no sharing.
///
/// A hit is a hash of at most 16 bytes, one load from a resident array and one
/// short compare against bytes held inline, so it never chases a pointer.
pub struct InternCache {
    slots: [Slot; SLOTS],
}

impl InternCache {
    const fn new() -> Self {
        Self {
            slots: [Slot::EMPTY; SLOTS],
        }
    }

    fn get_or_intern(&mut self, s: &str) -> StrKey {
        let bytes = s.as_bytes();
        if bytes.len() > WORD_LEN {
            return get_interner().get_or_intern(s);
        }

        let slot = &mut self.slots[slot_index(bytes)];
        if slot.len as usize == bytes.len() && slot.word[..bytes.len()] == *bytes {
            // Only an `EMPTY` slot holds `None`, and `EMPTY` is not a length.
            if let Some(key) = slot.key {
                return key;
            }
        }

        let key = get_interner().get_or_intern(s);
        slot.word[..bytes.len()].copy_from_slice(bytes);
        slot.len = bytes.len() as u8;
        slot.key = Some(key);
        key
    }
}

/// A multiply-xor mix of the word's first and last eight bytes with its
/// length.
///
/// Reading both ends unaligned and ignoring anything in between costs three
/// multiplies whatever the length is. Words of 17..=24 bytes have a middle
/// that no longer reaches the hash, which can only cost a collision — and a
/// collision costs a slot, never a wrong answer, because the compare is
/// against the stored bytes.
#[inline]
fn slot_index(bytes: &[u8]) -> usize {
    const K1: u64 = 0x9E37_79B9_7F4A_7C15;
    const K2: u64 = 0xC2B2_AE3D_27D4_EB4F;

    let len = bytes.len();
    let (a, b) = if len >= 8 {
        (read_u64(bytes), read_u64(&bytes[len - 8..]))
    } else if len >= 4 {
        (read_u32(bytes) as u64, read_u32(&bytes[len - 4..]) as u64)
    } else if len > 0 {
        // Three one-byte loads always in range: `bytes[len / 2]` is the middle
        // of a 1..=3 byte word.
        (bytes[0] as u64, ((bytes[len - 1] as u64) << 8) | bytes[len / 2] as u64)
    } else {
        (0, 0)
    };

    let mut h = (a ^ (len as u64).wrapping_mul(K1)).wrapping_mul(K2);
    h ^= b.wrapping_mul(K1);
    h ^= h >> 31;
    (h.wrapping_mul(K2) >> 32) as usize & (SLOTS - 1)
}

#[inline]
fn read_u64(bytes: &[u8]) -> u64 {
    u64::from_le_bytes(bytes[..8].try_into().unwrap())
}

#[inline]
fn read_u32(bytes: &[u8]) -> u32 {
    u32::from_le_bytes(bytes[..4].try_into().unwrap())
}

thread_local! {
    static CACHE: core::cell::RefCell<InternCache> =
        const { core::cell::RefCell::new(InternCache::new()) };
}

/// `get_interner().get_or_intern(s)`, answered from the calling thread's memo
/// when that thread has recently seen `s`.
pub fn intern_cached(s: &str) -> StrKey {
    CACHE.with(|cache| cache.borrow_mut().get_or_intern(s))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The memo must be indistinguishable from asking the interner every time,
    /// whatever mix of strings it is asked about and in whatever order — most
    /// of all when two strings land in the same slot.
    #[test]
    fn answers_exactly_like_the_interner() {
        crate::init_interner();
        let interner = get_interner();

        let mut words: Vec<String> = vec![
            String::new(),
            "A".into(),
            "AB".into(),
            "ARG".into(),
            "LOCAL".into(),
            "RESULT".into(),
            "CALLNAME".into(),
            "」".into(),
            "。".into(),
            "이름은".into(),
            "a".repeat(WORD_LEN),
            "b".repeat(WORD_LEN + 1),
            "c".repeat(WORD_LEN * 4),
        ];
        // Enough distinct words to overflow the table many times over, so
        // every answer below is given after eviction.
        words.extend((0..SLOTS * 4).map(|n| format!("W{n}")));

        let mut cache = InternCache::new();
        for _ in 0..3 {
            for w in &words {
                assert_eq!(cache.get_or_intern(w), interner.get_or_intern(w), "{w:?}");
            }
        }
    }

    /// Words differing only outside the hashed window must still get their own
    /// answers: the hash ignores the middle of a long word, the compare does
    /// not.
    #[test]
    fn words_sharing_a_hash_window_keep_their_own_answers() {
        crate::init_interner();
        let interner = get_interner();

        let a = "PREFIX__aaaaaa__SUFFIX__";
        let b = "PREFIX__bbbbbb__SUFFIX__";
        assert_eq!(a.len(), WORD_LEN);
        assert_eq!(
            slot_index(a.as_bytes()),
            slot_index(b.as_bytes()),
            "expected the hash window to hide the difference"
        );

        let mut cache = InternCache::new();
        for _ in 0..3 {
            assert_eq!(cache.get_or_intern(a), interner.get_or_intern(a));
            assert_eq!(cache.get_or_intern(b), interner.get_or_intern(b));
        }
    }

    /// The whole point of the fixed size: the memo must not grow with the
    /// number of distinct strings it is shown.
    #[test]
    fn size_is_fixed() {
        assert_eq!(
            core::mem::size_of::<InternCache>(),
            SLOTS * core::mem::size_of::<Slot>()
        );
        assert!(core::mem::size_of::<Slot>() <= 32);
    }
}
