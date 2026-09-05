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
///
/// eraMegaten alone has 1,453 recurring identifiers over 24 bytes (CJK
/// content inflates byte length past what looks like a short name), carrying
/// 27,787 calls that bypass this cache — a plausible case for raising this
/// constant. Measured instead of assumed: raising it to 28 or 32 admits
/// those identifiers, but admits far more of the one-off sentence content
/// alongside them, and the sentences win the trade. Hit rate fell on *both*
/// corpora at every step — THYMKR 97.19%/96.51%/95.67%, eraMegaten
/// 94.01%/93.20%/92.66% at 24/28/32 — while `size_of::<InternCache>()` grew
/// 32,832/36,928/41,024 bytes doing it. Confirms this doc's own theory rather
/// than overturning it: do not raise this without a fresh measurement across
/// both corpora, the same way this one was.
pub(crate) const WORD_LEN: usize = 24;

/// Sets, a power of two, each holding two ways. 512 sets × 2 ways is 1024
/// physical slots — the same slot count as the direct-mapped table this
/// replaced — packed into 32,832 bytes (32.06 KiB, including a packed
/// most-recently-used bit per set; see `InternCache::mru`), close enough to
/// the original 32 KiB to stay resident next to everything else the parser
/// touches.
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
///
/// Why two ways instead of one: on eraMegaten (197,862 distinct identifiers,
/// ~3.7x eraTHYMKR's 53,639, against a cache that does not grow), a
/// direct-mapped 1024-slot table missed far more than eraTHYMKR's — measured
/// hit rates were 96.89%/93.30% (THYMKR/Megaten, serial parse). Instrumenting
/// every eviction showed *all* 1024 slots taking collisions, none of them a
/// clean two-word ping-pong (average 44.7/130.6 distinct words per conflicted
/// slot, THYMKR/Megaten) — this is working-set pressure, not a few
/// pathological hash collisions. An idealized oracle that always kept each
/// slot's single hottest word resident would only have prevented 15.2-15.6%
/// of collision misses; its top two words, 23.2-23.9%. Raw capacity alone
/// also shows steeply diminishing returns: sweeping a 1-way table from 512 to
/// 16384 slots gave Megaten miss rates of 7.70/6.70/6.06/5.57/5.25/4.99% —
/// doubling from 1024 to 16384 slots (16x the memory, 512 KiB/thread, well
/// past the point of staying resident) only bought 1.7 points.
///
/// A real 2-way set-associative table, measured, beats spending the same
/// extra memory on more direct-mapped slots: 512 sets × 2 ways (today's
/// ~32 KiB) hit 94.01% on Megaten, versus 93.94% for a 1-way table twice its
/// size (2048 slots, 64 KiB) — better hit rate, half the footprint. A 4-way
/// variant was also tried, using the same one-bit-of-recency scheme
/// generalized to "evict the way that is not the single most-recently-used
/// one" — it *regressed* to 93.53% at the same 1024 physical slots, because
/// one recency bit cannot express real LRU order among four ways; a correct
/// 4-way LRU needs more per-set state and more per-lookup bookkeeping than
/// this design's entire premise (a compare cheap enough to beat a `DashMap`
/// lookup) can spend. Two ways with one bit is the point where associativity
/// pays for itself without giving that back.
///
/// With this table, corpus hit rates: THYMKR 96.89% → 97.19%, Megaten
/// 93.30% → 94.01% (both serial parse). The residual misses are the tail of
/// a working set that is genuinely larger than any table small enough to
/// stay L2-resident — not a fixable collision pattern — so do not chase this
/// further by trying yet more ways or yet more slots without first measuring,
/// the same way this docstring does, that the exchange rate is still worth
/// it.
const SETS: usize = 512;

/// Ways per set. The eviction rule (`InternCache::evict`) is a single
/// most-recently-used bit — see the `SETS` docstring for why that is only
/// correct, and only pays for itself, at exactly two ways.
const WAYS: usize = 2;

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

/// 2-way set-associative, thread-local memo for `Interner::get_or_intern`.
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
/// A hit is a hash of at most 16 bytes, one load from a resident array and
/// one short compare against bytes held inline per way, so it never chases a
/// pointer.
pub struct InternCache {
    sets: [[Slot; WAYS]; SETS],
    /// One bit per set: which way (0 or 1) was most recently used. A `u8`
    /// field on `Set` would cost 4 padded bytes per set — `Slot`'s 4-byte
    /// alignment rounds a 65-byte `Set` up to 68 — 2 KiB across all 512 sets
    /// for one bit of information each. Packed here instead, the same 512
    /// bits cost 64 bytes total.
    mru: [u64; SETS / 64],
}

impl InternCache {
    const fn new() -> Self {
        Self {
            sets: [[Slot::EMPTY; WAYS]; SETS],
            mru: [0; SETS / 64],
        }
    }

    fn get_or_intern(&mut self, s: &str) -> StrKey {
        let bytes = s.as_bytes();
        if bytes.len() > WORD_LEN {
            return get_interner().get_or_intern(s);
        }

        let idx = slot_index(bytes);
        let set = &self.sets[idx];
        for (way, slot) in set.iter().enumerate() {
            if slot.len as usize == bytes.len() && slot.word[..bytes.len()] == *bytes {
                // Only an `EMPTY` slot holds `None`, and `EMPTY` is not a length.
                if let Some(key) = slot.key {
                    self.touch(idx, way);
                    return key;
                }
            }
        }

        let victim = self.evict(idx);
        let key = get_interner().get_or_intern(s);
        let slot = &mut self.sets[idx][victim];
        slot.word[..bytes.len()].copy_from_slice(bytes);
        slot.len = bytes.len() as u8;
        slot.key = Some(key);
        self.touch(idx, victim);
        key
    }

    /// Which way to overwrite: an empty way always wins over evicting a real
    /// entry; otherwise the way that was not the most recently used one.
    /// Correct only for `WAYS == 2` — see the `SETS` docstring.
    fn evict(&self, idx: usize) -> usize {
        let set = &self.sets[idx];
        if set[0].len == EMPTY {
            0
        } else if set[1].len == EMPTY {
            1
        } else {
            1 - self.mru_way(idx)
        }
    }

    fn mru_way(&self, idx: usize) -> usize {
        ((self.mru[idx / 64] >> (idx % 64)) & 1) as usize
    }

    fn touch(&mut self, idx: usize, way: usize) {
        let bit = 1u64 << (idx % 64);
        let word = &mut self.mru[idx / 64];
        if way == 1 {
            *word |= bit;
        } else {
            *word &= !bit;
        }
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
    (h.wrapping_mul(K2) >> 32) as usize & (SETS - 1)
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
    /// of all when several strings land in the same set.
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
        words.extend((0..SETS * 8).map(|n| format!("W{n}")));

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
        assert!(core::mem::size_of::<Slot>() <= 32);
        assert_eq!(
            core::mem::size_of::<InternCache>(),
            SETS * WAYS * core::mem::size_of::<Slot>() + (SETS / 64) * 8
        );
    }
}
