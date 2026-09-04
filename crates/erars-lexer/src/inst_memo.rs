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

/// Direct-mapped, case-insensitive memo for `InstructionCode::from_str`.
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
///
/// Both the slot index and the compare fold ASCII case, so the caller hands
/// over the source word as written. The `phf` map's keys are all uppercase, so
/// only the miss path — 1.2% of the questions — pays for the conversion, and
/// it pays into a stack buffer instead of the `String` that
/// `cow_to_ascii_uppercase` allocated on every line.
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

    /// `word.to_ascii_uppercase().parse::<InstructionCode>().ok()`, answered
    /// from the memo when the slot for `word` already holds it.
    pub fn get(&mut self, word: &str) -> Option<InstructionCode> {
        let bytes = word.as_bytes();
        if bytes.len() > MAX_LEN {
            return parse_upper(bytes);
        }

        let slot = &mut self.slots[slot_index(bytes)];
        if slot.len as usize == bytes.len() && slot.word[..bytes.len()].eq_ignore_ascii_case(bytes)
        {
            return slot.code;
        }

        let code = parse_upper(bytes);
        slot.word[..bytes.len()].copy_from_slice(bytes);
        slot.len = bytes.len() as u8;
        slot.code = code;
        code
    }
}

/// The uppercase spelling of `bytes` looked up in the `phf` map.
///
/// Uppercasing only ASCII leaves every other byte alone, so a valid UTF-8
/// input stays valid UTF-8 and the conversion can be done in place in a stack
/// buffer. Words longer than the buffer are rare enough — 99 of the corpus's
/// 1_028_278 lines — to be worth an allocation.
fn parse_upper(bytes: &[u8]) -> Option<InstructionCode> {
    let mut buf = [0u8; MAX_LEN];
    let upper: &[u8] = match buf.get_mut(..bytes.len()) {
        Some(buf) => {
            buf.copy_from_slice(bytes);
            buf.make_ascii_uppercase();
            buf
        }
        None => return String::from_utf8(bytes.to_ascii_uppercase()).ok()?.parse().ok(),
    };

    // Safety-free revalidation of at most `MAX_LEN` bytes, on the cold path.
    core::str::from_utf8(upper).ok()?.parse().ok()
}

/// Mixes the two bytes that differ most between instruction words with the
/// length. Measured over the corpus's first-word stream this collides least of
/// the cheap candidates: 98.8% hit at 64 slots against 93.7% for
/// `len ^ first << 2`.
///
/// `| 0x20` folds the ASCII letters that pick the slot, so `Print` and `PRINT`
/// land in the same one; it also folds a handful of non-letters together,
/// which only ever costs a collision.
fn slot_index(bytes: &[u8]) -> usize {
    let first = (bytes.first().copied().unwrap_or(0) | 0x20) as usize;
    let last = (bytes.last().copied().unwrap_or(0) | 0x20) as usize;
    (first ^ last.wrapping_mul(31)).wrapping_add(bytes.len()) % SLOTS
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The memo must be indistinguishable from uppercasing the word and asking
    /// the phf map every time, whatever mix of words it is asked about and in
    /// whatever order.
    #[test]
    fn answers_exactly_like_from_str() {
        let mut memo = InstMemo::new();
        let words = [
            "",
            "PRINTFORMW",
            "printformw",
            "PrintFormW",
            "PRINTFORML",
            "IF",
            "if",
            "If",
            "ENDIF",
            "CFLAG",
            "SIF",
            "만트라",
            "PRINT_SHOPITEM",
            "print_shopitem",
            "A_WORD_LONGER_THAN_THE_MEMO_LIMIT",
            "a_word_longer_than_the_memo_limit",
            "TALENT",
            "RETURN",
            "리ETURN",
        ];

        for _ in 0..4 {
            for w in words {
                assert_eq!(memo.get(w), w.to_ascii_uppercase().parse().ok(), "{w:?}");
            }
        }
    }

    /// A word and its other-case spellings share one slot, so neither may be
    /// answered with the other's identity — they are the same instruction.
    #[test]
    fn case_variants_share_an_answer() {
        let mut memo = InstMemo::new();
        assert_eq!(memo.get("RETURN"), Some(InstructionCode::RETURN));
        assert_eq!(memo.get("return"), Some(InstructionCode::RETURN));
        assert_eq!(memo.get("Return"), Some(InstructionCode::RETURN));
        assert_eq!(
            slot_index(b"return"),
            slot_index(b"RETURN"),
            "case variants must pick the same slot"
        );
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
