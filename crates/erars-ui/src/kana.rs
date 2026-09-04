//! `FORCEKANA` — the kana normalisation Emuera applies to a string on its way
//! to the console. `ExpressionMediator.ForceKana(Int64)`
//! (`GameData/Expression/ExpressionMediator.cs:37-44`) latches the mode and
//! `ExpressionMediator.ConvertStringType` (`:68-82`) applies it by delegating
//! to the VB6-compatible `Strings.StrConv(str, conv, 0x0411)`:
//!
//! | `FORCEKANA` | `VbStrConv`                | effect                          |
//! |-------------|----------------------------|---------------------------------|
//! | 0           | (early-out, `:70-71`)      | identity                        |
//! | 1           | `Katakana`                 | hiragana -> katakana            |
//! | 2           | `Hiragana`                 | katakana -> hiragana            |
//! | 3           | `Hiragana` \| `Wide`       | half-width -> full-width, *then* katakana -> hiragana |
//!
//! Mode 3 composes in that order and the order is observable: half-width `ﾊﾟ`
//! (`U+FF8A U+FF9F`) widens to the one full-width `パ` and only then folds to
//! hiragana `ぱ`. Widening first is also what makes the sound-mark composition
//! possible at all — a half-width mark only pairs with a half-width base.
//! Neither mode 1 nor mode 2 carries `Wide`, so under those two a half-width
//! `ｶ` stays exactly `ｶ`.
//!
//! Where the data comes from:
//! * Hiragana `U+3041`..=`U+3096` and katakana `U+30A1`..=`U+30F6` are the same
//!   86 code points in the same order, so both directions are `±0x60`. Both
//!   ranges deliberately stop short of everything without a counterpart:
//!   `U+3097`/`U+3098` are unassigned, katakana `U+30F7`..=`U+30FA` (`ヷヸヹヺ`)
//!   have no hiragana form, and `U+3099`..=`U+309A` / `U+30FB`..=`U+30FC`
//!   (`゙゚` and `・ー`) are marks that belong to neither syllabary.
//! * The iteration marks `ゝゞ` (`U+309D`/`U+309E`) and `ヽヾ`
//!   (`U+30FD`/`U+30FE`) *are* folded, at the same `±0x60`. Wine's NLS
//!   conformance test pins `LCMAP_KATAKANA` mapping `U+309D` to `U+30FD`
//!   (`dlls/kernel32/tests/locale.c:2503-2517`, `japanese_text[1]` against
//!   `katakana_text[1]`); `ゞ`/`ヾ` are the voiced member of that same pair at
//!   the same offset. `ゟ` (`U+309F`, YORI) and `ヿ` (`U+30FF`, KOTO) are
//!   different digraphs, not a pair, so they stay put.
//! * [`WIDE_KANA`] is the set of Unicode `<narrow>` compatibility
//!   decompositions of `U+FF61`..=`U+FF9F`; [`voiced_wide_form`] and
//!   [`semi_voiced_wide_form`] are the NFC compositions of those forms with
//!   `U+3099`/`U+309A`, and [`narrow_form`] inverts all three.
//! * One deliberate departure from that data: an unpaired `U+FF9E`/`U+FF9F`
//!   widens to the *spacing* marks `゛`/`゜` (`U+309B`/`U+309C`), not to the
//!   combining `U+3099`/`U+309A` that its decomposition names. That is what
//!   `VbStrConv.Wide` produces, and it is the only sane choice here: a bare
//!   combining mark would attach itself to whatever the console printed before.
//!
//! [`to_full`] and [`to_half`] are the same machinery under `TOFULL`/`TOHALF`
//! (`Creator.Method.cs:4531-4534`), which call `StrConv` with plain
//! `VbStrConv.Wide`/`VbStrConv.Narrow` and so never touch the syllabary.

use serde::{Deserialize, Serialize};

/// Emuera `FORCEKANA` mode (`ExpressionMediator.cs:37-51`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum ForceKana {
    /// `FORCEKANA 0` — no conversion.
    #[default]
    Off,
    /// `FORCEKANA 1` — `VbStrConv.Katakana`.
    Katakana,
    /// `FORCEKANA 2` — `VbStrConv.Hiragana`.
    Hiragana,
    /// `FORCEKANA 3` — `VbStrConv.Hiragana | VbStrConv.Wide`.
    HiraganaWide,
}

impl ForceKana {
    /// `FORCEKANA <flag>`; Emuera rejects anything outside 0..=3 with
    /// `OoRForcekanaArg` (`ExpressionMediator.cs:39-40`).
    pub fn from_flag(flag: i64) -> Option<Self> {
        Some(match flag {
            0 => Self::Off,
            1 => Self::Katakana,
            2 => Self::Hiragana,
            3 => Self::HiraganaWide,
            _ => return None,
        })
    }

    /// `ExpressionMediator.ForceKana()` (`:46-49`), the early-out guard of
    /// `ConvertStringType`.
    pub fn is_off(self) -> bool {
        matches!(self, Self::Off)
    }

    /// Emuera `ConvertStringType` (`ExpressionMediator.cs:68-81`).
    /// Returns the input untouched (no allocation) when nothing changes.
    pub fn convert(self, s: String) -> String {
        match self {
            Self::Off => s,
            Self::Katakana => map_chars(s, to_katakana),
            Self::Hiragana => map_chars(s, to_hiragana),
            Self::HiraganaWide => widen(s, to_hiragana),
        }
    }
}

/// Rewrites `s` through `f`, moving the buffer through unchanged when `f` is
/// the identity on every character and copying only from the first character
/// it is not. Both kana directions map a 3-byte character to a 3-byte one, so
/// the reserved capacity is exact.
fn map_chars(s: String, f: fn(char) -> char) -> String {
    let Some(at) = s.char_indices().find(|&(_, c)| f(c) != c).map(|(i, _)| i) else {
        return s;
    };
    let mut out = String::with_capacity(s.len());
    out.push_str(&s[..at]);
    out.extend(s[at..].chars().map(f));
    out
}

/// `VbStrConv.Wide` followed by a per-character `fold`
/// (`ExpressionMediator.cs:77` passes `VbStrConv.Hiragana | VbStrConv.Wide`,
/// `Creator.Method.cs:4534` passes `Wide` alone). Widening runs first and is
/// observable: half-width `ﾊﾟ` composes into the one full-width `パ` before
/// `fold` sees it.
fn widen(s: String, fold: fn(char) -> char) -> String {
    // A half-width sound mark is only consumed together with a base that is
    // itself half-width, and such a base is rewritten by `wide_form` on its
    // own. So the first character this pass touches is never the second half
    // of a pair, and everything before it can be copied verbatim.
    let mut at = None;
    let mut extra = 0;
    for (i, c) in s.char_indices() {
        match wide_form(c) {
            Some(w) => {
                at.get_or_insert(i);
                // Widening never shrinks a character: `U+0020`..=`U+007E` grow
                // from 1 byte to 3, `U+FF61`..=`U+FF9F` stay at 3.
                extra += w.len_utf8() - c.len_utf8();
            }
            None => {
                if fold(c) != c {
                    at.get_or_insert(i);
                }
            }
        }
    }
    let Some(at) = at else { return s };

    // An upper bound: composing a sound mark into its base drops 3 bytes.
    let mut out = String::with_capacity(s.len() + extra);
    out.push_str(&s[..at]);
    let mut rest = s[at..].chars().peekable();
    while let Some(c) = rest.next() {
        let composed = match rest.peek().copied() {
            Some(VOICED_MARK) => voiced_wide_form(c),
            Some(SEMI_VOICED_MARK) => semi_voiced_wide_form(c),
            _ => None,
        };
        let full = match composed {
            Some(full) => {
                rest.next();
                full
            }
            None => wide_form(c).unwrap_or(c),
        };
        out.push(fold(full));
    }
    out
}

/// Hiragana `U+3041`..=`U+3096` (`ぁ`..`ゖ`) plus the iteration marks `ゝゞ`
/// (`U+309D`..=`U+309E`) -> katakana `U+30A1`..=`U+30F6` (`ァ`..`ヶ`) and
/// `ヽヾ` (`U+30FD`..=`U+30FE`). Everything else — `ゟ`, the sound marks,
/// half-width katakana, ASCII — is outside both ranges and left alone.
fn to_katakana(c: char) -> char {
    match c {
        '\u{3041}'..='\u{3096}' | '\u{309D}'..='\u{309E}' => shift(c, 0x60),
        _ => c,
    }
}

/// Katakana `U+30A1`..=`U+30F6` (`ァ`..`ヶ`) plus `ヽヾ` (`U+30FD`..=`U+30FE`)
/// -> hiragana `U+3041`..=`U+3096` (`ぁ`..`ゖ`) and `ゝゞ`
/// (`U+309D`..=`U+309E`). `ヷヸヹヺ` (`U+30F7`..=`U+30FA`) have no hiragana
/// form, `・ー` (`U+30FB`..=`U+30FC`) are not katakana letters, and `ヿ`
/// (`U+30FF`) is a different digraph from `ゟ`, so all of them stay put.
fn to_hiragana(c: char) -> char {
    match c {
        '\u{30A1}'..='\u{30F6}' | '\u{30FD}'..='\u{30FE}' => shift(c, -0x60),
        _ => c,
    }
}

/// `TOFULL` — `VbStrConv.Wide` (`Creator.Method.cs:4534`). Printable ASCII is
/// widened and half-width katakana is widened with any following sound mark
/// composed into its base (`ｳﾞ` -> `ヴ`), which is exactly what Wine's NLS test
/// pins for `LCMAP_FULLWIDTH` (`dlls/kernel32/tests/locale.c:2610-2613`,
/// `halfwidth_text` -> `japanese_text`). Hiragana is already full width and the
/// mapping never changes syllabary.
///
/// DELIBERATE DEVIATION: `LCMAP_FULLWIDTH` also folds the Mathematical
/// Alphanumeric Symbols and the CJK Compatibility Ideographs Supplement
/// (`locale.c:2618-2633`); those are astral blocks no ERB game emits, and erars
/// leaves them alone.
pub fn to_full(s: String) -> String {
    widen(s, |c| c)
}

/// `TOHALF` — `VbStrConv.Narrow` (`Creator.Method.cs:4532`). The inverse of
/// [`to_full`]: full-width ASCII narrows, and full-width katakana narrows with
/// its voiced/semi-voiced mark *decomposed* into a following `U+FF9E`/`U+FF9F`
/// (`ヴ` -> `ｳﾞ`). Hiragana has no half-width form and is left alone — Wine's
/// `LCMAP_HALFWIDTH` case carries `い`, `ゝ`, `や`, `の`, `は`, `だ` and `よ`
/// through untouched while narrowing every katakana beside them
/// (`locale.c:2503-2522`, `japanese_text` -> `halfwidth_text`).
pub fn to_half(s: String) -> String {
    let Some(at) = s
        .char_indices()
        .find(|&(_, c)| narrow_form(c).is_some())
        .map(|(i, _)| i)
    else {
        return s;
    };

    // Narrowing never grows the byte count: a 3-byte full-width character
    // becomes either 1 ASCII byte or a 3-byte half-width kana, and a decomposed
    // pair is 6 bytes against the 3 it replaces — so `s.len()` is not an upper
    // bound and `+ 3` per remaining character is. Cheaper: keep `s.len()` and
    // let the rare voiced run grow the buffer once.
    let mut out = String::with_capacity(s.len());
    out.push_str(&s[..at]);
    for c in s[at..].chars() {
        match narrow_form(c) {
            Some((base, mark)) => {
                out.push(base);
                out.extend(mark);
            }
            None => out.push(c),
        }
    }
    out
}

/// `VbStrConv.Narrow` for one character: the inverse of [`wide_form`], plus the
/// voiced/semi-voiced decomposition. The second element is the trailing sound
/// mark, if the character carried one. `None` means nothing `Narrow` touches.
fn narrow_form(c: char) -> Option<(char, Option<char>)> {
    match c {
        // The one full-width character whose narrow form is not at `-0xFEE0`.
        '\u{3000}' => return Some((' ', None)),
        // `U+FF01`..=`U+FF5E` -> `U+0021`..=`U+007E`.
        '！'..='～' => return char::from_u32(c as u32 - 0xFEE0).map(|c| (c, None)),
        _ => {}
    }

    // 63 entries scanned against one character. Deriving the reverse direction
    // from the forward tables is what keeps the two from drifting apart, and a
    // linear scan over a static array beats a second table to maintain.
    for (i, &wide) in WIDE_KANA.iter().enumerate() {
        let half = char::from_u32(0xFF61 + i as u32).expect("inside the half-width block");
        if wide == c {
            return Some((half, None));
        }
        if voiced_wide_form(half) == Some(c) {
            return Some((half, Some(VOICED_MARK)));
        }
        if semi_voiced_wide_form(half) == Some(c) {
            return Some((half, Some(SEMI_VOICED_MARK)));
        }
    }
    None
}

/// Both kana ranges sit in the BMP well clear of the surrogate block, so the
/// shift always lands on a valid scalar value. `unwrap_or` keeps the character
/// as it was rather than panicking should a range ever be widened.
fn shift(c: char, delta: i32) -> char {
    char::from_u32((c as i32 + delta) as u32).unwrap_or(c)
}

/// `U+FF9E`, the half-width voiced sound mark, as in `ｶﾞ`.
const VOICED_MARK: char = '\u{FF9E}';
/// `U+FF9F`, the half-width semi-voiced sound mark, as in `ﾊﾟ`.
const SEMI_VOICED_MARK: char = '\u{FF9F}';

/// `VbStrConv.Wide` for one character: printable ASCII and the half-width
/// katakana block. `None` means nothing `Wide` touches — a control character,
/// or something already full-width.
fn wide_form(c: char) -> Option<char> {
    match c {
        // `U+0020` is the one printable ASCII character whose full-width form
        // is not at `+0xFEE0`; it maps to the ideographic space.
        ' ' => Some('\u{3000}'),
        // `U+0021`..=`U+007E` -> `U+FF01`..=`U+FF5E`.
        '!'..='~' => char::from_u32(c as u32 + 0xFEE0),
        '\u{FF61}'..='\u{FF9F}' => Some(WIDE_KANA[c as usize - 0xFF61]),
        _ => None,
    }
}

/// `U+FF61`..=`U+FF9F` widened: the Unicode `<narrow>` compatibility
/// decompositions, except that the last two entries are the spacing marks
/// `U+309B`/`U+309C` rather than the combining `U+3099`/`U+309A` (see the
/// module header).
#[rustfmt::skip]
static WIDE_KANA: [char; 63] = [
    // U+FF61..U+FF65 punctuation
    '。', '「', '」', '、', '・',
    // U+FF66..U+FF6F ヲ, the small vowels, the small ya row and small tu
    'ヲ', 'ァ', 'ィ', 'ゥ', 'ェ', 'ォ', 'ャ', 'ュ', 'ョ', 'ッ',
    // U+FF70..U+FF75 prolonged sound mark and the vowels
    'ー', 'ア', 'イ', 'ウ', 'エ', 'オ',
    // U+FF76..U+FF7A ka row
    'カ', 'キ', 'ク', 'ケ', 'コ',
    // U+FF7B..U+FF7F sa row
    'サ', 'シ', 'ス', 'セ', 'ソ',
    // U+FF80..U+FF84 ta row
    'タ', 'チ', 'ツ', 'テ', 'ト',
    // U+FF85..U+FF89 na row
    'ナ', 'ニ', 'ヌ', 'ネ', 'ノ',
    // U+FF8A..U+FF8E ha row
    'ハ', 'ヒ', 'フ', 'ヘ', 'ホ',
    // U+FF8F..U+FF93 ma row
    'マ', 'ミ', 'ム', 'メ', 'モ',
    // U+FF94..U+FF96 ya row
    'ヤ', 'ユ', 'ヨ',
    // U+FF97..U+FF9B ra row
    'ラ', 'リ', 'ル', 'レ', 'ロ',
    // U+FF9C..U+FF9D ワ and ン
    'ワ', 'ン',
    // U+FF9E..U+FF9F sound marks
    '゛', '゜',
];

/// `ｶ` + `U+FF9E` -> `ガ`: the single full-width kana that a half-width base
/// composes into with the voiced sound mark. `None` for the bases that have no
/// voiced form, which then keep the mark as a separate character.
fn voiced_wide_form(c: char) -> Option<char> {
    Some(match c {
        // `ｳ`/`ﾜ`/`ｦ` voice to `U+30F4`/`U+30F7`/`U+30FA`, away from their rows.
        'ｳ' => 'ヴ',
        'ﾜ' => 'ヷ',
        'ｦ' => 'ヺ',
        // ka row
        'ｶ' => 'ガ',
        'ｷ' => 'ギ',
        'ｸ' => 'グ',
        'ｹ' => 'ゲ',
        'ｺ' => 'ゴ',
        // sa row
        'ｻ' => 'ザ',
        'ｼ' => 'ジ',
        'ｽ' => 'ズ',
        'ｾ' => 'ゼ',
        'ｿ' => 'ゾ',
        // ta row
        'ﾀ' => 'ダ',
        'ﾁ' => 'ヂ',
        'ﾂ' => 'ヅ',
        'ﾃ' => 'デ',
        'ﾄ' => 'ド',
        // ha row
        'ﾊ' => 'バ',
        'ﾋ' => 'ビ',
        'ﾌ' => 'ブ',
        'ﾍ' => 'ベ',
        'ﾎ' => 'ボ',
        _ => return None,
    })
}

/// `ﾊ` + `U+FF9F` -> `パ`. Only the ha row has a semi-voiced form.
fn semi_voiced_wide_form(c: char) -> Option<char> {
    Some(match c {
        'ﾊ' => 'パ',
        'ﾋ' => 'ピ',
        'ﾌ' => 'プ',
        'ﾍ' => 'ペ',
        'ﾎ' => 'ポ',
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::{to_full, to_half, ForceKana};

    fn conv(mode: ForceKana, s: &str) -> String {
        mode.convert(s.to_owned())
    }

    /// `ConvertStringType` hands `str` straight back when no flag is set
    /// (`ExpressionMediator.cs:70-71`); we must not even reallocate.
    #[test]
    fn off_is_identity() {
        let s = String::from("あアｶ abc 한글ﾊﾟ");
        let ptr = s.as_ptr();
        let out = ForceKana::Off.convert(s);
        assert_eq!(out, "あアｶ abc 한글ﾊﾟ");
        assert_eq!(out.as_ptr(), ptr, "Off must move the buffer through");
        assert!(ForceKana::Off.is_off());
        assert!(!ForceKana::Katakana.is_off());
        assert!(!ForceKana::Hiragana.is_off());
        assert!(!ForceKana::HiraganaWide.is_off());
    }

    /// Nothing to change in any mode -> the buffer is moved through, not copied.
    #[test]
    fn unchanged_input_is_not_reallocated() {
        for (mode, s) in [
            (ForceKana::Katakana, "アイウ、ｶ abc"),
            (ForceKana::Hiragana, "あいう、ｶ abc"),
            (ForceKana::HiraganaWide, "あいう。ゕ"),
        ] {
            let owned = String::from(s);
            let ptr = owned.as_ptr();
            let out = mode.convert(owned);
            assert_eq!(out, s);
            assert_eq!(out.as_ptr(), ptr, "{mode:?} reallocated {s:?}");
        }
    }

    /// `ForceKana(Int64)` throws on anything outside 0..=3
    /// (`ExpressionMediator.cs:39-40`).
    #[test]
    fn from_flag_accepts_only_0_to_3() {
        assert_eq!(ForceKana::from_flag(0), Some(ForceKana::Off));
        assert_eq!(ForceKana::from_flag(1), Some(ForceKana::Katakana));
        assert_eq!(ForceKana::from_flag(2), Some(ForceKana::Hiragana));
        assert_eq!(ForceKana::from_flag(3), Some(ForceKana::HiraganaWide));
        assert_eq!(ForceKana::from_flag(-1), None);
        assert_eq!(ForceKana::from_flag(4), None);
        assert_eq!(ForceKana::from_flag(i64::MIN), None);
        assert_eq!(ForceKana::from_flag(i64::MAX), None);
        assert_eq!(ForceKana::default(), ForceKana::Off);
    }

    #[test]
    fn katakana_mode() {
        assert_eq!(conv(ForceKana::Katakana, "あいう"), "アイウ");
        assert_eq!(conv(ForceKana::Katakana, "アイウ"), "アイウ");
        assert_eq!(conv(ForceKana::Katakana, "ぐ"), "グ");
        // The ends of the arithmetic range.
        assert_eq!(conv(ForceKana::Katakana, "ぁゖ"), "ァヶ");
        assert_eq!(conv(ForceKana::Katakana, "ゔ"), "ヴ");
        // The iteration marks are folded; the digraph `ゟ` is not.
        assert_eq!(conv(ForceKana::Katakana, "ゝゞゟ"), "ヽヾゟ");
        assert_eq!(conv(ForceKana::Katakana, "ヽヾヿ"), "ヽヾヿ");
        // Outside the range: `ヷ`, the sound marks, `ー`, `・`.
        assert_eq!(conv(ForceKana::Katakana, "ヷ"), "ヷ");
        assert_eq!(conv(ForceKana::Katakana, "゛゜ー・"), "゛゜ー・");
        assert_eq!(conv(ForceKana::Katakana, "abc XYZ 09"), "abc XYZ 09");
        assert_eq!(conv(ForceKana::Katakana, "한글"), "한글");
        // Mixed: the copy starts at the first changed character.
        assert_eq!(conv(ForceKana::Katakana, "a한あ한aア"), "a한ア한aア");
    }

    /// `VbStrConv.Katakana` does not imply `Wide`: half-width katakana are
    /// already katakana, so mode 1 leaves them half-width.
    #[test]
    fn katakana_mode_does_not_widen() {
        assert_eq!(conv(ForceKana::Katakana, "ｶ"), "ｶ");
        assert_eq!(conv(ForceKana::Katakana, "ﾊﾟ"), "ﾊﾟ");
        assert_eq!(conv(ForceKana::Katakana, " a!"), " a!");
    }

    #[test]
    fn hiragana_mode() {
        assert_eq!(conv(ForceKana::Hiragana, "アイウ"), "あいう");
        assert_eq!(conv(ForceKana::Hiragana, "あいう"), "あいう");
        assert_eq!(conv(ForceKana::Hiragana, "グ"), "ぐ");
        assert_eq!(conv(ForceKana::Hiragana, "ァヶ"), "ぁゖ");
        // Small `ヵ`/`ヶ` do have hiragana forms, `U+3095`/`U+3096`.
        assert_eq!(conv(ForceKana::Hiragana, "ヵヶ"), "ゕゖ");
        // `ヴ` -> `ゔ` (`U+30F4` -> `U+3094`).
        assert_eq!(conv(ForceKana::Hiragana, "ヴ"), "ゔ");
        // `ヷヸヹヺ` have no hiragana form; `U+3097`/`U+3098` are unassigned.
        assert_eq!(conv(ForceKana::Hiragana, "ヷヸヹヺ"), "ヷヸヹヺ");
        assert_eq!(conv(ForceKana::Hiragana, "ー"), "ー");
        assert_eq!(conv(ForceKana::Hiragana, "ヽヾヿ"), "ゝゞヿ");
        assert_eq!(conv(ForceKana::Hiragana, "・"), "・");
        assert_eq!(conv(ForceKana::Hiragana, "ゝゞ"), "ゝゞ");
        // Mode 2 is not `Wide` either.
        assert_eq!(conv(ForceKana::Hiragana, "ｶﾞ abc"), "ｶﾞ abc");
    }

    /// Mode 3 widens first and folds to hiragana second
    /// (`ExpressionMediator.cs:77`) — the only order under which half-width
    /// `ﾊﾟ` becomes a single `ぱ` rather than `パ` or `は゜`.
    #[test]
    fn hiragana_wide_composes_sound_marks() {
        assert_eq!(conv(ForceKana::HiraganaWide, "ﾊﾟ"), "ぱ");
        assert_eq!(conv(ForceKana::HiraganaWide, "ｶﾞ"), "が");
        assert_eq!(conv(ForceKana::HiraganaWide, "ｳﾞ"), "ゔ");
        assert_eq!(conv(ForceKana::HiraganaWide, "ﾊﾞ"), "ば");
        assert_eq!(conv(ForceKana::HiraganaWide, "ﾂﾞ"), "づ");
        // `ﾜﾞ` composes to `ヷ`, which has no hiragana form and so survives
        // the second half of the conversion unchanged. Likewise `ｦﾞ` -> `ヺ`.
        assert_eq!(conv(ForceKana::HiraganaWide, "ﾜﾞ"), "ヷ");
        assert_eq!(conv(ForceKana::HiraganaWide, "ｦﾞ"), "ヺ");
        // A base with no voiced form keeps the mark as its own character.
        assert_eq!(conv(ForceKana::HiraganaWide, "ｱﾞ"), "あ゛");
        assert_eq!(conv(ForceKana::HiraganaWide, "ｱﾟ"), "あ゜");
        assert_eq!(conv(ForceKana::HiraganaWide, "ｶﾟ"), "か゜");
        // Unpaired marks widen to the spacing forms `U+309B`/`U+309C`.
        assert_eq!(conv(ForceKana::HiraganaWide, "ﾞ"), "゛");
        assert_eq!(conv(ForceKana::HiraganaWide, "ﾟ"), "゜");
        assert_eq!(conv(ForceKana::HiraganaWide, "ｶﾞﾞ"), "が゛");
    }

    #[test]
    fn hiragana_wide_widens_ascii_and_punctuation() {
        assert_eq!(conv(ForceKana::HiraganaWide, "abc"), "ａｂｃ");
        assert_eq!(conv(ForceKana::HiraganaWide, " "), "　");
        assert_eq!(conv(ForceKana::HiraganaWide, "!~"), "！～");
        assert_eq!(conv(ForceKana::HiraganaWide, "A1@\\"), "Ａ１＠＼");
        // `U+FF61`..=`U+FF65` are punctuation, not kana.
        assert_eq!(conv(ForceKana::HiraganaWide, "｡｢｣､･"), "。「」、・");
        assert_eq!(conv(ForceKana::HiraganaWide, "ｰ"), "ー");
        // Controls are outside `U+0020`..=`U+007E`.
        assert_eq!(conv(ForceKana::HiraganaWide, "\n\t"), "\n\t");
        assert_eq!(conv(ForceKana::HiraganaWide, "한글"), "한글");
    }

    /// Full-width katakana already in the string still folds to hiragana, and
    /// the widening runs over the whole tail from the first changed character.
    #[test]
    fn hiragana_wide_mixed_string() {
        assert_eq!(
            conv(ForceKana::HiraganaWide, "ﾃｽﾄ:ﾊﾟｽ ｶﾞｯ 한 ア"),
            "てすと：ぱす　がっ　한　あ"
        );
        assert_eq!(conv(ForceKana::HiraganaWide, "한ｱ"), "한あ");
    }

    /// `dlls/kernel32/tests/locale.c:2610-2613` asserts
    /// `LCMAP_FULLWIDTH(halfwidth_text) == japanese_text`, and `:2652-2655`
    /// asserts `LCMAP_HALFWIDTH(japanese_text) == halfwidth_text`. Those two
    /// literals, transcribed from their code-point lists, are the reference
    /// pair for `TOFULL`/`TOHALF`.
    const JAPANESE_TEXT: &str = "いゝや、イーハトーヴォの野原は広いんだよ。";
    const HALFWIDTH_TEXT: &str = "いゝや､ｲｰﾊﾄｰｳﾞｫの野原は広いんだよ｡";

    #[test]
    fn to_full_matches_lcmap_fullwidth() {
        assert_eq!(to_full(HALFWIDTH_TEXT.to_owned()), JAPANESE_TEXT);
        // Sound marks compose into their base.
        assert_eq!(to_full("ｳﾞ".to_owned()), "ヴ");
        assert_eq!(to_full("ｶﾞｯｺｳ".to_owned()), "ガッコウ");
        assert_eq!(to_full("ﾊﾟ".to_owned()), "パ");
        // ASCII widens; the syllabary never changes.
        assert_eq!(to_full("abc A1@ ".to_owned()), "ａｂｃ　Ａ１＠　");
        assert_eq!(to_full("あいう".to_owned()), "あいう");
        assert_eq!(to_full("アイウ".to_owned()), "アイウ");
        // Nothing to widen -> the buffer moves through.
        let owned = String::from("あいうアイウ野原");
        let ptr = owned.as_ptr();
        let out = to_full(owned);
        assert_eq!(out.as_ptr(), ptr, "to_full reallocated an unchanged string");
    }

    #[test]
    fn to_half_matches_lcmap_halfwidth() {
        assert_eq!(to_half(JAPANESE_TEXT.to_owned()), HALFWIDTH_TEXT);
        // Voiced and semi-voiced katakana decompose into a trailing mark.
        assert_eq!(to_half("ヴ".to_owned()), "ｳﾞ");
        assert_eq!(to_half("ガッコウ".to_owned()), "ｶﾞｯｺｳ");
        assert_eq!(to_half("パ".to_owned()), "ﾊﾟ");
        assert_eq!(to_half("ヷヺ".to_owned()), "ﾜﾞｦﾞ");
        // Full-width ASCII narrows; the ideographic space becomes `U+0020`.
        assert_eq!(to_half("ａｂｃ　Ａ１＠".to_owned()), "abc A1@");
        // Hiragana has no half-width form: `LCMAP_HALFWIDTH` leaves `だ`
        // undecomposed where it decomposes `ヴ`.
        assert_eq!(to_half("だ".to_owned()), "だ");
        assert_eq!(to_half("あいうゝ".to_owned()), "あいうゝ");
        // The spacing sound marks narrow to the half-width ones.
        assert_eq!(to_half("゛゜".to_owned()), "ﾞﾟ");
        // Round trip through the pair that Wine pins.
        assert_eq!(to_full(to_half(JAPANESE_TEXT.to_owned())), JAPANESE_TEXT);
        // Nothing to narrow -> the buffer moves through.
        let owned = String::from("あいうｱｲｳ野原");
        let ptr = owned.as_ptr();
        let out = to_half(owned);
        assert_eq!(out.as_ptr(), ptr, "to_half reallocated an unchanged string");
    }
}
