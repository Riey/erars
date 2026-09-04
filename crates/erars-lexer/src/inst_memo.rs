use crate::InstructionCode;

/// Number of slots. 64 is enough for one ERB file's vocabulary: measured over
/// the eraTHYMKR corpus with a fresh memo per file, 64 slots answer 97.4% of
/// the questions and 256 slots only reach 97.8%, so the smaller table — which
/// stays comfortably inside L1 — wins.
const SLOTS: usize = 64;

/// Words longer than this bypass the memo. 99 of the corpus's 1_028_278 lines
/// start with a longer word, so the cutoff costs nothing.
const MAX_LEN: usize = 24;

/// `len` of a slot that has never been filled. Real lengths are `0..=MAX_LEN`,
/// and the empty word is a legitimate key — 31_345 corpus lines start with a
/// non-identifier character — so it needs a value of its own.
const EMPTY: u8 = u8::MAX;

#[derive(Clone, Copy)]
struct Slot {
    word: [u8; MAX_LEN],
    len: u8,
    code: Option<InstructionCode>,
}

/// Direct-mapped memo for `InstructionCode::from_str`.
///
/// `InstructionCode` is a `phf` map, so every question costs a SipHash-1-3
/// over the whole word plus a probe and a key compare (`phf_shared::get_index`
/// 3.2% + `phf::Map::get_entry` 1.9% of parse+compile self time). The lexer
/// asks once per line — 890_801 times per pass over the corpus — and the
/// questions are extremely repetitive: those lines begin with only 230
/// distinct words, 27% of them with `PRINTFORMW` alone, and 52% of the words
/// are not instructions at all.
///
/// A slot is chosen with three arithmetic operations on the word's first byte,
/// last byte and length, so a hit is one cache-line load and one short
/// compare. Two properties make it safe to keep across lines:
///
/// * A slot stores the word's bytes, not a pointer to them, so a bump
///   allocation that has since been reset can never be mistaken for a match.
/// * The memoized value includes "not an instruction", so failed lookups are
///   memoized too.
#[derive(Clone, Copy)]
pub struct InstMemo {
    slots: [Slot; SLOTS],
}

impl InstMemo {
    pub const fn new() -> Self {
        Self {
            slots: [Slot {
                word: [0; MAX_LEN],
                len: EMPTY,
                code: None,
            }; SLOTS],
        }
    }

    /// `word.parse::<InstructionCode>().ok()`, answered from the memo when the
    /// slot for `word` already holds it.
    pub fn get(&mut self, word: &str) -> Option<InstructionCode> {
        let bytes = word.as_bytes();
        if bytes.len() > MAX_LEN {
            return word.parse().ok();
        }

        let slot = &mut self.slots[slot_index(bytes)];
        if slot.len as usize == bytes.len() && slot.word[..bytes.len()] == *bytes {
            return slot.code;
        }

        let code = word.parse().ok();
        slot.word[..bytes.len()].copy_from_slice(bytes);
        slot.len = bytes.len() as u8;
        slot.code = code;
        code
    }
}

/// Mixes the two bytes that differ most between instruction words with the
/// length. Measured over the corpus's first-word stream this collides least of
/// the cheap candidates: 98.8% hit at 64 slots against 93.7% for
/// `len ^ first << 2`.
fn slot_index(bytes: &[u8]) -> usize {
    let first = bytes.first().copied().unwrap_or(0) as usize;
    let last = bytes.last().copied().unwrap_or(0) as usize;
    (first ^ last.wrapping_mul(31)).wrapping_add(bytes.len()) % SLOTS
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The memo must be indistinguishable from asking the phf map every time,
    /// whatever mix of words it is asked about and in whatever order.
    #[test]
    fn answers_exactly_like_from_str() {
        let mut memo = InstMemo::new();
        let words = [
            "",
            "PRINTFORMW",
            "PRINTFORML",
            "IF",
            "ENDIF",
            "CFLAG",
            "SIF",
            "만트라",
            "PRINT_SHOPITEM",
            "A_WORD_LONGER_THAN_THE_MEMO_LIMIT",
            "TALENT",
            "RETURN",
        ];

        for _ in 0..4 {
            for w in words {
                assert_eq!(memo.get(w), w.parse().ok(), "{w:?}");
            }
        }
    }

    /// Words that collide into the same slot must not shadow each other's
    /// answers.
    #[test]
    fn colliding_words_keep_their_own_answers() {
        let mut memo = InstMemo::new();
        let mut collisions = Vec::new();
        for code in <InstructionCode as strum::IntoEnumIterator>::iter() {
            let name: &'static str = code.into();
            if name.len() <= MAX_LEN && slot_index(name.as_bytes()) == slot_index(b"IF") {
                collisions.push(name);
            }
        }
        assert!(collisions.len() > 1, "expected a colliding pair to test");

        for _ in 0..3 {
            for name in &collisions {
                assert_eq!(memo.get(name), name.parse().ok(), "{name:?}");
            }
        }
    }
}
