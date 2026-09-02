//! Cell-width classifier shared by the VM (`STRLEN`, `SUBSTRING`, `PadStr`,
//! PRINTC padding) and the renderer (cluster boxes).
//!
//! Every code point occupies 0, 1 or 2 half-width cells, decided by the code
//! point and the game *encoding* alone — never by the font that draws it. See
//! `docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md`,
//! Component 1, for the rule and the accepted deviations from Emuera.
//!
//! Rule, in order:
//! 1. U+00AD and U+D7B0–U+D7FF → 0; `unicode_width::UnicodeWidthChar::width`
//!    (non-CJK table) `None` (controls) or `Some(0)` (combining marks, format
//!    characters, Hangul V/T jamo) → 0.
//! 2. Encodable in the game encoding (WHATWG encoder, `Unmappable` = not
//!    encodable) → the byte count, 1 or 2.
//! 2b. Shift_JIS only: Windows cp932 best-fit code points `¢ £ ¬ — ‖ 〜` and
//!    the user-defined area U+E000–U+E757 → 2.
//! 3. `width == Some(2)` or Regional_Indicator U+1F1E6–U+1F1FF → 2; else 1.

use encoding_rs::{Encoder, EncoderResult, Encoding, SHIFT_JIS};
use std::fmt;
use unicode_width::UnicodeWidthChar;

/// Bytes in the packed BMP table: 65 536 code points × 2 bits.
const BMP_TABLE_LEN: usize = 0x1_0000 / 4;

/// A `\t` advances to the next multiple of this many cells (uEmuera /
/// GRAPHICS-mode behaviour). The renderer expands tabs with the same stop.
pub const TAB_CELLS: usize = 8;

/// Per-encoding cell widths: 2 bits per BMP code point, built once.
#[derive(Clone)]
pub struct WidthTable {
    encoding: &'static Encoding,
    bmp: Box<[u8]>,
}

impl WidthTable {
    /// Build the table for one game encoding (`SHIFT_JIS`, `EUC_KR`, `GBK`,
    /// `BIG5`). About 10-50 ms; build once per console / shaper.
    pub fn new(encoding: &'static Encoding) -> Self {
        let mut bmp = vec![0u8; BMP_TABLE_LEN].into_boxed_slice();
        let mut encoder = encoding.new_encoder();
        for cp in 0u32..0x1_0000 {
            // Surrogates are not chars; their slots stay 0 and are never read.
            let Some(c) = char::from_u32(cp) else { continue };
            let cells = classify(c, encoding, &mut encoder);
            debug_assert!(cells <= 2, "U+{cp:04X} classified {cells}");
            bmp[(cp >> 2) as usize] |= cells << ((cp & 3) * 2);
        }
        Self { encoding, bmp }
    }

    /// The encoding this table was built for.
    pub fn encoding(&self) -> &'static Encoding {
        self.encoding
    }

    /// Cells occupied by `c`: 0, 1 or 2. Controls (including `\n` and `\t`)
    /// are 0 — the console splits `\n` and `str_cells` expands `\t` itself.
    #[inline]
    pub fn char_cells(&self, c: char) -> u8 {
        let cp = c as u32;
        if cp < 0x1_0000 {
            (self.bmp[(cp >> 2) as usize] >> ((cp & 3) * 2)) & 3
        } else {
            // Astral: no table; the four legacy encoders never map these, so
            // this is rule 1 / rule 3 only, but run the full rule for clarity.
            classify(c, self.encoding, &mut self.encoding.new_encoder())
        }
    }

    /// Sum of `char_cells` over `s`, with `\t` advancing to the next multiple
    /// of [`TAB_CELLS`].
    pub fn str_cells(&self, s: &str) -> usize {
        let mut cells = 0usize;
        for c in s.chars() {
            if c == '\t' {
                cells = (cells / TAB_CELLS + 1) * TAB_CELLS;
            } else {
                cells += usize::from(self.char_cells(c));
            }
        }
        cells
    }
}

impl fmt::Debug for WidthTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WidthTable")
            .field("encoding", &self.encoding.name())
            .finish_non_exhaustive()
    }
}

/// Windows cp932 best-fit code points that WHATWG Shift_JIS cannot encode but
/// Emuera's `STRLEN` counts as 2 bytes and MS Gothic draws full-width
/// (`¢ £ ¬ — ‖ 〜` and the user-defined area).
#[inline]
fn jp_best_fit(cp: u32) -> bool {
    matches!(cp, 0x00A2 | 0x00A3 | 0x00AC | 0x2014 | 0x2016 | 0x301C)
        || (0xE000..=0xE757).contains(&cp)
}

/// The rule of Component 1 for one code point. `encoder` must be an encoder
/// of `encoding`; it is reused across calls (`last = false`, stateless
/// legacy encoders), so a table build performs no allocation per char.
fn classify(c: char, encoding: &'static Encoding, encoder: &mut Encoder) -> u8 {
    let cp = c as u32;

    // Step 1: zero width. Explicit overrides first (unicode-width 0.1.11
    // returns Some(1) for U+00AD and the V/T jamo extensions).
    if cp == 0x00AD || (0xD7B0..=0xD7FF).contains(&cp) {
        return 0;
    }
    let width = UnicodeWidthChar::width(c);
    if matches!(width, None | Some(0)) {
        return 0;
    }

    // Step 2: encodable in the game encoding → byte count (1 or 2).
    let mut utf8 = [0u8; 4];
    let src: &str = c.encode_utf8(&mut utf8);
    let mut dst = [0u8; 4];
    let (result, _read, written) =
        encoder.encode_from_utf8_without_replacement(src, &mut dst, false);
    match result {
        EncoderResult::InputEmpty if (1..=2).contains(&written) => return written as u8,
        // Neither can happen for Shift_JIS / EUC-KR / GBK / Big5 (≤ 2 bytes per
        // code point, 4-byte buffer); treat like "not encodable" if it ever does.
        EncoderResult::InputEmpty | EncoderResult::OutputFull => {}
        EncoderResult::Unmappable(_) => {}
    }

    // Step 2b: Windows cp932 best-fit, Japanese only.
    if std::ptr::eq(encoding, SHIFT_JIS) && jp_best_fit(cp) {
        return 2;
    }

    // Step 3: East-Asian wide / fullwidth, emoji presentation, regional indicators.
    if width == Some(2) || (0x1F1E6..=0x1F1FF).contains(&cp) {
        return 2;
    }
    1
}

#[cfg(test)]
mod tests {
    use super::{classify, WidthTable, TAB_CELLS};
    use encoding_rs::{Encoding, BIG5, EUC_KR, GBK, SHIFT_JIS};
    use once_cell::sync::Lazy;

    static JP: Lazy<WidthTable> = Lazy::new(|| WidthTable::new(SHIFT_JIS));
    static KR: Lazy<WidthTable> = Lazy::new(|| WidthTable::new(EUC_KR));
    static HANS: Lazy<WidthTable> = Lazy::new(|| WidthTable::new(GBK));
    static HANT: Lazy<WidthTable> = Lazy::new(|| WidthTable::new(BIG5));

    fn check(table: &WidthTable, want: u8, chars: &[char]) {
        for &c in chars {
            assert_eq!(
                table.char_cells(c),
                want,
                "U+{:04X} {:?} in {}",
                c as u32,
                c,
                table.encoding().name()
            );
        }
    }

    // Spec Component 1, "Expected values (tested)", row JP.
    #[test]
    fn japanese_shift_jis() {
        check(&JP, 1, &['A', 'ｱ', '═', '║', '░', '█', '▶', 'é', '♥', '¥']);
        check(
            &JP,
            2,
            &[
                'あ', '─', '°', '※', '★', 'α', 'А', '①', '〜', '‖', '¢', '−', '\u{E000}', '한',
                '😀',
            ],
        );
        check(&JP, 0, &['\u{0301}', '\u{200D}', '\u{00AD}']);
    }

    // Row KR: KS X 1001 has the single/mixed-weight box glyphs and `▒`, but
    // not `═ ░ █`; `¢` U+00A2 is not in WHATWG EUC-KR (0xA1CB is U+FFE0).
    #[test]
    fn korean_euc_kr() {
        check(&KR, 1, &['A', 'ｱ', '═', '░', '█', '¢']);
        check(&KR, 2, &['한', 'あ', '─', '▒', '★', '①', '😀']);
        check(&KR, 0, &['\u{0301}', '\u{1160}']);
    }

    // Row ZH: GBK and Big5 encode the double-line box characters and `█`.
    #[test]
    fn chinese_gbk_and_big5() {
        for table in [&*HANS, &*HANT] {
            check(table, 1, &['A', 'ｱ', '░']);
            check(table, 2, &['═', '║', '█', '中', '한']);
            check(table, 0, &['\u{0301}']);
        }
        assert_eq!(HANS.str_cells("╔══╗"), 8);
        assert_eq!(HANT.str_cells("╔══╗"), 8);
        assert_eq!(JP.str_cells("╔══╗"), 4);
        assert_eq!(KR.str_cells("╔══╗"), 4);
    }

    // Rule 2b applies to Shift_JIS only; elsewhere the same code points follow
    // the plain rule (unmappable → EAW: `〜` is W, the rest are A/N → 1).
    #[test]
    fn jp_best_fit_overrides_and_eudc() {
        for c in ['\u{00A2}', '\u{00A3}', '\u{00AC}', '\u{2014}', '\u{2016}', '\u{301C}'] {
            assert_eq!(JP.char_cells(c), 2, "JP U+{:04X}", c as u32);
        }
        check(&KR, 1, &['\u{00A2}', '\u{00A3}', '\u{00AC}', '\u{2014}', '\u{2016}']);
        assert_eq!(KR.char_cells('\u{301C}'), 2);
        assert_eq!(HANT.char_cells('\u{2016}'), 1);
        // User-defined area U+E000–U+E757 (cp932 gaiji): 2 in Japanese only.
        check(&JP, 2, &['\u{E000}', '\u{E3FF}', '\u{E757}']);
        check(&JP, 1, &['\u{E758}', '\u{F8FF}']);
        check(&KR, 1, &['\u{E000}', '\u{E757}']);
        check(&HANT, 1, &['\u{E000}']);
        // Encodable code points keep their WHATWG byte count: `−` is 0x817C in
        // Shift_JIS (2), unmappable in EUC-KR (1); `¥` / `‾` / `ｱ` are 1 byte.
        assert_eq!(JP.char_cells('−'), 2);
        assert_eq!(KR.char_cells('−'), 1);
        check(&JP, 1, &['¥', '\u{203E}', 'ｱ', '\u{FF9E}']);
        check(&JP, 2, &['\u{FF0D}', '\u{2225}', '\u{FF5E}', '\u{3000}', '→']);
    }

    // Rule 1: overrides, controls, combining marks, format characters.
    #[test]
    fn zero_width_and_controls() {
        check(&JP, 0, &['\u{00AD}', '\u{D7B0}', '\u{D7FF}']);
        check(&JP, 0, &['\0', '\t', '\n', '\r', '\u{1B}', '\u{7F}', '\u{80}', '\u{9F}']);
        check(
            &JP,
            0,
            &[
                '\u{0301}', '\u{3099}', '\u{200C}', '\u{200D}', '\u{FE0E}', '\u{FE0F}', '\u{FEFF}',
                '\u{2060}', '\u{2064}', '\u{1160}', '\u{11FF}', '\u{E0001}', '\u{E0100}',
            ],
        );
        // Spacing characters that look like the above are not zero.
        check(&JP, 1, &['\u{00A0}', '\u{2028}']);
        assert_eq!(JP.char_cells('\u{309B}'), 2); // spacing voiced mark, in JIS
        for table in [&*KR, &*HANS, &*HANT] {
            check(table, 0, &['\u{00AD}', '\u{D7B0}', '\u{0301}', '\u{200D}', '\u{1160}', '\n']);
        }
    }

    // Astral code points bypass the table; regional indicators are forced to 2.
    #[test]
    fn astral_and_regional_indicators() {
        for table in [&*JP, &*KR, &*HANS, &*HANT] {
            check(
                table,
                2,
                &[
                    '😀',
                    '\u{1F1E6}',
                    '\u{1F1F0}',
                    '\u{1F1FF}',
                    '\u{1F468}',
                    '\u{1F3FD}',
                    '\u{20000}',
                ],
            );
            check(table, 1, &['\u{1D400}', '\u{10400}', '\u{1F170}', '\u{10FFFF}']);
            assert_eq!(table.str_cells("🇰🇷"), 4);
            assert_eq!(table.str_cells("👨\u{200D}👩\u{200D}👧"), 6);
        }
    }

    // `str_cells` of mixed strings and tab expansion to 8-cell stops.
    #[test]
    fn str_cells_mixed_and_tabs() {
        assert_eq!(TAB_CELLS, 8);
        assert_eq!(JP.str_cells(""), 0);
        assert_eq!(JP.str_cells("A한あ"), 5);
        assert_eq!(KR.str_cells("A한あ"), 5);
        assert_eq!(JP.str_cells("e\u{0301}"), 1);
        assert_eq!(JP.str_cells("❤\u{FE0F}"), 1);
        assert_eq!(JP.str_cells("┌──┐"), 8);
        assert_eq!(JP.str_cells("[ 0] 텍스트"), 11);
        assert_eq!(JP.str_cells("abc\ndef"), 6);
        assert_eq!(JP.str_cells("\u{00AD}"), 0);
        assert_eq!(JP.str_cells("\t"), 8);
        assert_eq!(JP.str_cells("a\tb"), 9);
        assert_eq!(JP.str_cells("1234567\tX"), 9);
        assert_eq!(JP.str_cells("12345678\t"), 16);
        assert_eq!(JP.str_cells("あ\tb"), 9);
        assert_eq!(JP.str_cells("\t\t"), 16);
    }

    // The packed table must reproduce the direct rule for every BMP code
    // point (checks the 2-bit packing and that no value is 3).
    #[test]
    fn bmp_table_matches_direct_rule() {
        for (table, encoding) in [(&*JP, SHIFT_JIS), (&*KR, EUC_KR), (&*HANS, GBK), (&*HANT, BIG5)]
        {
            let encoding: &'static Encoding = encoding;
            let mut encoder = encoding.new_encoder();
            for cp in 0u32..0x1_0000 {
                let Some(c) = char::from_u32(cp) else { continue };
                let direct = classify(c, encoding, &mut encoder);
                assert!(direct <= 2, "U+{cp:04X} classified {direct}");
                assert_eq!(table.char_cells(c), direct, "U+{cp:04X} in {}", encoding.name());
            }
        }
    }

    #[test]
    fn debug_and_clone() {
        assert_eq!(format!("{:?}", *JP), "WidthTable { encoding: \"Shift_JIS\", .. }");
        assert_eq!(format!("{:?}", *KR), "WidthTable { encoding: \"EUC-KR\", .. }");
        let copy: WidthTable = (*JP).clone();
        assert_eq!(copy.char_cells('あ'), 2);
        assert!(std::ptr::eq(copy.encoding(), SHIFT_JIS));
    }
}
