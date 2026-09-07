//! Emuera's exact pseudo-random generator, reimplemented bit-for-bit so a
//! `RANDDATA` array carried over from a real Emuera save produces the
//! identical `RAND` sequence in erars.
//!
//! ## What Emuera actually uses
//!
//! Despite the class being named `MTRandom`, Emuera does **not** use the
//! textbook Matsumoto–Nishimura MT19937 twist recursion. It embeds Rei
//! Hobara's C# port of **SFMT** (SIMD-oriented Fast Mersenne Twister, Saito
//! & Matsumoto), built with the `MT19937` parameter set selected by
//! `#define MT19937` (`_Library/SFMT.cs:33`). That `#define` only chooses
//! *which period's constants* to use — `gen_rand_all_19937`
//! (`_Library/SFMT.cs:380-408`) is still the SFMT 128-bit block recursion,
//! parameterised with `POS1=122, SL1=18, SR1=11,
//! MSK1..4={0xdfffffef,0xddfecb7f,0xbffaffff,0xbffffff6}`
//! (`_Library/SFMT.cs:185-200`) — the real SFMT-19937 reference constants,
//! not the scalar MT19937 recursion `mt[i] = mt[i+397] ^ ...`.
//!
//! - **State**: 624 `u32` words (`N32 = MEXP/128*4 = 156*4`,
//!   `_Library/SFMT.cs:264-265`) plus one refill index, exposed to ERB
//!   whole as the 625-`Int64` `RANDDATA` array —
//!   `GameData/ConstantData.cs:152` fixes
//!   `VariableIntArrayLength[RANDDATA] = 625`, and `SetRand`/`GetRand`
//!   (`_Library/SFMT.cs:89-106`, driven by `INITRAND`/`DUMPRAND` via
//!   `VariableEvaluator.cs:44,49`) map `RANDDATA[0..624]` to the state
//!   words and `RANDDATA[624]` to the index, **verbatim**: `array[i] =
//!   sfmt[i]` widens `u32` to `Int64` (never sign-extends, since `u32` is
//!   always representable in `Int64`), `sfmt[i] = (u32)array[i]` truncates
//!   back to the low 32 bits. [`EmuRandom::get_state`]/[`set_state`] do the
//!   same, byte for byte.
//! - **Seeding** (`_Library/SFMT.cs:296-309`, `init_gen_rand`): a linear
//!   congruential fill, `sfmt[i] = 1812433253 * (sfmt[i-1] ^
//!   (sfmt[i-1]>>30)) + i` for `i` in `1..624` (all `u32`, wrapping —
//!   `sfmt[0] = seed`), followed unconditionally by `period_certification`
//!   (`_Library/SFMT.cs:314-337`), a parity fixup against the fixed
//!   `PARITY = [1, 0, 0, 0x13c9e684]` vector that guarantees the
//!   generator's period. `RANDOMIZE n` truncates the `Int64` argument `n`
//!   to its low 32 bits exactly like C#'s `unchecked (uint)` cast
//!   (`GameProc/Function/Instraction.Child.cs:1233-1240`,
//!   `MTRandom(Int64 seed)` at `_Library/SFMT.cs:50-56`) — [`from_seed`]
//!   takes a `u32` for the same reason; callers truncate with `as u32`.
//! - **Draw** (`_Library/SFMT.cs:60-77`): `RAND` never consumes a single
//!   raw `u32`. Every draw builds a `u64` from two consecutive `u32`s
//!   (`NextUInt64`: high word first, then low —
//!   `ret = NextUInt32(); ret = (ret << 32) + NextUInt32();`,
//!   `_Library/SFMT.cs:72-77`) and reduces it with **plain modulo**,
//!   `NextUInt64() % (u64)max` (`NextInt64(Int64 max)`,
//!   `_Library/SFMT.cs:60-65`) — *not* rejection sampling. This reduction
//!   is measurably biased for `max` values that don't evenly divide 2^64
//!   (negligible for realistic in-game bounds, but real, and matching it
//!   exactly — not silently "fixing" it into unbiased sampling — is the
//!   entire point of this module. **Do not rejection-sample this.**
//!
//! `RAND:max` (`GameData/Variable/VariableToken.cs:1451-1463`,
//! `RandToken`, the default — `CompatiRandToken` only exists behind the
//! `CompatiRAND` legacy config flag, off by default, and is out of scope
//! here) and the `RAND(min, max)` / `RAND(max)` function
//! (`GameData/Function/Creator.Method.cs:922-972`, `RandMethod`) both
//! bottom out in `VariableEvaluator.GetNextRand`
//! (`GameData/Variable/VariableEvaluator.cs:51-54`), which is exactly
//! [`EmuRandom::next_bounded`]. See
//! `crates/erars-vm/src/terminal_vm/executor.rs`'s `Rand`/
//! `BuiltinMethod::Rand` arms for the two call sites.
//!
//! ## A generator this weak, matched deliberately
//!
//! SFMT is a fine general-purpose PRNG, but Emuera's *reduction* — plain
//! `% max` on a 64-bit draw — is a well-known source of modulo bias, and
//! its *seeding* — a 32-bit `RANDOMIZE` argument through a linear
//! congruential expansion — has a laughably small effective seed space by
//! modern standards. None of that is a bug to fix here: this module exists
//! solely so a real Emuera save's `RANDDATA` continues the exact same
//! sequence in erars. If Emuera's own weaknesses ever get "improved" here,
//! every compatibility guarantee this module exists for breaks with it.

/// Number of `u32` state words — `N32 = MEXP/128 * 4` for `MEXP = 19937`
/// (`N = 19937/128+1 = 156`, `N32 = N*4 = 624`; `_Library/SFMT.cs:264-265`).
pub const STATE_WORDS: usize = 624;

/// `RANDDATA`'s declared length: the 624 state words plus the refill index
/// (`GameData/ConstantData.cs:152`).
pub const STATE_LEN: usize = STATE_WORDS + 1;

const POS1: usize = 122;
const SL1: u32 = 18;
const SR1: u32 = 11;
const MSK1: u32 = 0xdfffffef;
const MSK2: u32 = 0xddfecb7f;
const MSK3: u32 = 0xbffaffff;
const MSK4: u32 = 0xbffffff6;
/// `_Library/SFMT.cs:199`: the *effective* MT19937 parity constant. The
/// file also keeps a commented-out `0x20000000U` right below it
/// (`_Library/SFMT.cs:200`) from an earlier reference-parameter revision;
/// it is dead source and never compiled in, and is not used here either.
const PARITY: [u32; 4] = [0x0000_0001, 0x0000_0000, 0x0000_0000, 0x13c9_e684];

/// Emuera's `MinorShift._Library.MTRandom`, configured for the
/// `MEXP = 19937` SFMT parameter set. See the module doc comment for the
/// full derivation and citations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EmuRandom {
    state: [u32; STATE_WORDS],
    idx: usize,
}

impl EmuRandom {
    /// `new MTRandom(seed)` / `RANDOMIZE n` (`_Library/SFMT.cs:50-56,
    /// 296-309`). `init_gen_rand` followed by the unconditional
    /// `period_certification` fixup.
    pub fn from_seed(seed: u32) -> Self {
        let mut state = [0u32; STATE_WORDS];
        state[0] = seed;
        for i in 1..STATE_WORDS {
            let prev = state[i - 1];
            state[i] = 1_812_433_253u32
                .wrapping_mul(prev ^ (prev >> 30))
                .wrapping_add(i as u32);
        }
        period_certification(&mut state);
        // `init_gen_rand` finishes with `idx = N32`, forcing a refill on
        // the very first draw (`_Library/SFMT.cs:308`).
        Self { state, idx: STATE_WORDS }
    }

    /// `INITRAND` / `MTRandom.SetRand` (`_Library/SFMT.cs:89-97`):
    /// restores the generator from a `RANDDATA`-shaped array.
    ///
    /// Real Emuera indexes `sfmt[idx++]` with no bounds check, so a
    /// corrupt or foreign `RANDDATA[624]` outside `0..=624` would throw
    /// `IndexOutOfRangeException` there. erars is not willing to panic on
    /// untrusted save data, so an out-of-range index is clamped to
    /// `STATE_WORDS` (forcing an immediate, deterministic refill on the
    /// next draw) instead of reproducing that crash.
    pub fn set_state(&mut self, arr: &[i64; STATE_LEN]) {
        for i in 0..STATE_WORDS {
            self.state[i] = arr[i] as u32;
        }
        let idx = arr[STATE_WORDS];
        self.idx = if (0..=STATE_WORDS as i64).contains(&idx) {
            idx as usize
        } else {
            STATE_WORDS
        };
    }

    /// `DUMPRAND` / `MTRandom.GetRand` (`_Library/SFMT.cs:99-106`): dumps
    /// the generator into a `RANDDATA`-shaped array.
    pub fn get_state(&self) -> [i64; STATE_LEN] {
        let mut out = [0i64; STATE_LEN];
        for i in 0..STATE_WORDS {
            out[i] = self.state[i] as i64;
        }
        out[STATE_WORDS] = self.idx as i64;
        out
    }

    /// `MTRandom.NextUInt32` (private in Emuera; `_Library/SFMT.cs:283-290`).
    fn next_u32(&mut self) -> u32 {
        if self.idx >= STATE_WORDS {
            self.gen_rand_all();
            self.idx = 0;
        }
        let v = self.state[self.idx];
        self.idx += 1;
        v
    }

    /// `MTRandom.NextUInt64` (`_Library/SFMT.cs:72-77`): high word first,
    /// then low — *not* the reverse.
    fn next_u64(&mut self) -> u64 {
        let hi = self.next_u32() as u64;
        let lo = self.next_u32() as u64;
        (hi << 32) + lo
    }

    /// `MTRandom.NextInt64(Int64 max)` (`_Library/SFMT.cs:60-65`):
    /// `GetNextRand`'s bounded draw, `[0, max)` via plain modulo. `max`
    /// must be positive; every caller in erars already validates that
    /// before reaching here, matching Emuera's own `CodeEE` guards at the
    /// `RAND` call sites.
    pub fn next_bounded(&mut self, max: i64) -> i64 {
        debug_assert!(max > 0, "RAND max must be positive, checked by callers");
        (self.next_u64() % (max as u64)) as i64
    }

    /// `MTRandom.gen_rand_all_19937` (`_Library/SFMT.cs:380-408`): the
    /// SFMT-19937 128-bit block recursion. Purely XOR/shift — no
    /// multiplication or addition — so plain `u32` wrapping shifts are
    /// exactly Emuera's `unchecked` `uint` arithmetic, bit for bit.
    fn gen_rand_all(&mut self) {
        let p = &mut self.state;
        let mut a = 0usize;
        let mut b = POS1 * 4;
        let mut c = STATE_WORDS - 8;
        let mut d = STATE_WORDS - 4;
        loop {
            p[a + 3] = p[a + 3]
                ^ (p[a + 3] << 8)
                ^ (p[a + 2] >> 24)
                ^ (p[c + 3] >> 8)
                ^ ((p[b + 3] >> SR1) & MSK4)
                ^ (p[d + 3] << SL1);
            p[a + 2] = p[a + 2]
                ^ (p[a + 2] << 8)
                ^ (p[a + 1] >> 24)
                ^ (p[c + 3] << 24)
                ^ (p[c + 2] >> 8)
                ^ ((p[b + 2] >> SR1) & MSK3)
                ^ (p[d + 2] << SL1);
            p[a + 1] = p[a + 1]
                ^ (p[a + 1] << 8)
                ^ (p[a + 0] >> 24)
                ^ (p[c + 2] << 24)
                ^ (p[c + 1] >> 8)
                ^ ((p[b + 1] >> SR1) & MSK2)
                ^ (p[d + 1] << SL1);
            p[a + 0] = p[a + 0]
                ^ (p[a + 0] << 8)
                ^ (p[c + 1] << 24)
                ^ (p[c + 0] >> 8)
                ^ ((p[b + 0] >> SR1) & MSK1)
                ^ (p[d + 0] << SL1);

            c = d;
            d = a;
            a += 4;
            b += 4;
            if b >= STATE_WORDS {
                b = 0;
            }
            if a >= STATE_WORDS {
                break;
            }
        }
    }
}

/// `MTRandom.period_certification` (`_Library/SFMT.cs:314-337`).
fn period_certification(state: &mut [u32; STATE_WORDS]) {
    let mut inner = 0u32;
    for i in 0..4 {
        inner ^= state[i] & PARITY[i];
    }
    let mut shift = 16;
    while shift > 0 {
        inner ^= inner >> shift;
        shift >>= 1;
    }
    if inner & 1 == 1 {
        return;
    }
    for i in 0..4 {
        let mut work = 1u32;
        for _ in 0..32 {
            if work & PARITY[i] != 0 {
                state[i] ^= work;
                return;
            }
            work <<= 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// State round-trips through the exact `RANDDATA` shape without loss.
    #[test]
    fn state_round_trips() {
        let rng = EmuRandom::from_seed(12345);
        let dumped = rng.get_state();
        let mut restored = EmuRandom::from_seed(0);
        restored.set_state(&dumped);
        assert_eq!(rng, restored);
        assert_eq!(dumped, restored.get_state());
    }

    /// A corrupt/foreign refill index is clamped, not indexed out of
    /// bounds.
    #[test]
    fn set_state_clamps_out_of_range_index() {
        let mut rng = EmuRandom::from_seed(1);
        let mut arr = rng.get_state();
        arr[STATE_WORDS] = -1;
        rng.set_state(&arr);
        // Doesn't panic, and forces an immediate refill on next draw.
        let _ = rng.next_bounded(10);

        let mut arr2 = rng.get_state();
        arr2[STATE_WORDS] = 999_999;
        rng.set_state(&arr2);
        let _ = rng.next_bounded(10);
    }

    /// `RANDOMIZE` truncates an `Int64` seed to its low 32 bits, matching
    /// C#'s `unchecked (uint)` cast — including negative inputs.
    #[test]
    fn randomize_seed_truncates_like_csharp_unchecked_cast() {
        let a = EmuRandom::from_seed((-1i64) as u32);
        let b = EmuRandom::from_seed(0xFFFF_FFFFu32);
        assert_eq!(a, b);
    }
}
