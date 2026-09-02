//! Cell-width string functions shared by `STRLEN`/`STRLENS`/`STRLENFORM`,
//! `SUBSTRING`, `STRFIND` and `{x, width}` / `%s, width%` padding.
//!
//! Ports Emuera's `LangManager` (`GetStrlenLang` / `GetUFTIndex` /
//! `GetSubStringLang`) and `StrForm.FormatPercent` with the console's
//! `char_cells` in place of `Encoding.GetByteCount`, so the VM measures with
//! the same cell function the renderer lays out with (spec Component 2).
//! The helpers take the cell function as a closure because `VirtualConsole`
//! exposes `char_cells(char)` / `cells(&str)` rather than its `WidthTable`.

use erars_ast::Alignment;

/// `LangManager.GetStrlenLang`: the cell count of `s`.
fn total_cells(s: &str, cells: &impl Fn(char) -> u8) -> i64 {
    s.chars().map(|c| i64::from(cells(c))).sum()
}

/// `LangManager.GetUFTIndex`: how many leading characters to skip so that the
/// skipped cells reach `lang_index` (whole characters only). `≤ 0` → 0;
/// at or beyond the string's total → the character count.
pub(crate) fn uft_index(s: &str, lang_index: i64, cells: impl Fn(char) -> u8) -> usize {
    if lang_index <= 0 {
        return 0;
    }
    if lang_index >= total_cells(s, &cells) {
        return s.chars().count();
    }
    let mut utf = 0;
    let mut jis = 0;
    for c in s.chars() {
        jis += i64::from(cells(c));
        utf += 1;
        if jis >= lang_index {
            break;
        }
    }
    utf
}

/// `LangManager.GetSubStringLang`: skip characters until the running cell
/// count reaches `start`, then append characters until the running count
/// reaches `length` (`None` or negative = to the end). Never splits a
/// character.
pub(crate) fn substring_cells(
    s: &str,
    start: i64,
    length: Option<i64>,
    cells: impl Fn(char) -> u8,
) -> String {
    let total = total_cells(s, &cells);
    if start >= total || length == Some(0) {
        return String::new();
    }
    let length = match length {
        Some(l) if l >= 0 && l <= total => l,
        _ => total,
    };

    let mut chars = s.chars().peekable();
    if start <= 0 {
        if length == total {
            return s.to_owned();
        }
    } else {
        let mut jis = 0;
        while let Some(c) = chars.next() {
            jis += i64::from(cells(c));
            if jis >= start {
                break;
            }
        }
        if chars.peek().is_none() {
            return String::new();
        }
    }

    let mut ret = String::new();
    let mut jis = 0;
    for c in chars {
        ret.push(c);
        jis += i64::from(cells(c));
        if jis >= length {
            break;
        }
    }
    ret
}

/// `STRFIND` (Emuera `StrFindMethod`, non-unicode branch): `start` is a cell
/// offset mapped through [`uft_index`]; the result is the cell count of the
/// text before the match (measured from the start of `target`), or `-1`.
pub(crate) fn strfind_cells(
    target: &str,
    word: &str,
    start: Option<i64>,
    cells: impl Fn(char) -> u8,
) -> i64 {
    let uft_start = start.map_or(0, |js| uft_index(target, js, &cells));
    if uft_start >= target.chars().count() {
        return -1;
    }
    let byte_start = target
        .char_indices()
        .nth(uft_start)
        .map_or(target.len(), |(b, _)| b);
    match target[byte_start..].find(word) {
        Some(rel) => total_cells(&target[..byte_start + rel], &cells),
        None => -1,
    }
}

/// `StrForm.FormatPercent`: pad `text` (occupying `text_cells` cells) with
/// spaces to `width` cells — Left → after, Right → before, Center (erars
/// extension) → `n/2` before and the rest after. Unchanged when it already
/// fills the field or `width` is smaller (including negative).
pub(crate) fn pad_str_cells(
    text: String,
    width: i64,
    align: Alignment,
    text_cells: usize,
) -> String {
    let n = width - text_cells as i64;
    if n <= 0 {
        return text;
    }
    let n = n as usize;
    let (before, after) = match align {
        Alignment::Left => (0, n),
        Alignment::Right => (n, 0),
        Alignment::Center => (n / 2, n - n / 2),
    };
    let mut ret = String::with_capacity(text.len() + n);
    ret.extend(std::iter::repeat(' ').take(before));
    ret.push_str(&text);
    ret.extend(std::iter::repeat(' ').take(after));
    ret
}

#[cfg(test)]
mod tests {
    use super::*;
    use erars_ui::width::WidthTable;

    fn jp() -> WidthTable {
        WidthTable::new(encoding_rs::SHIFT_JIS)
    }

    fn kr() -> WidthTable {
        WidthTable::new(encoding_rs::EUC_KR)
    }

    #[test]
    fn uft_index_maps_cells_to_chars() {
        let t = jp();
        let c = |ch| t.char_cells(ch);
        assert_eq!(uft_index("abc", 0, c), 0);
        assert_eq!(uft_index("abc", -5, c), 0);
        assert_eq!(uft_index("abc", 1, c), 1);
        assert_eq!(uft_index("abc", 3, c), 3);
        assert_eq!(uft_index("abc", 99, c), 3);
        assert_eq!(uft_index("", 1, c), 0);
        // an offset inside a 2-cell character skips that character whole
        assert_eq!(uft_index("한글a", 1, c), 1);
        assert_eq!(uft_index("한글a", 2, c), 1);
        assert_eq!(uft_index("한글a", 3, c), 2);
    }

    #[test]
    fn substring_walks_whole_characters() {
        let t = jp();
        let c = |ch| t.char_cells(ch);
        assert_eq!(substring_cells("한글abc", 2, Some(3), c), "글a");
        assert_eq!(substring_cells("한글abc", 1, Some(2), c), "글");
        assert_eq!(substring_cells("한글abc", 4, None, c), "abc");
        assert_eq!(substring_cells("─═║x", 0, Some(3), c), "─═");
        assert_eq!(substring_cells("😀xy", 2, Some(1), c), "x");
        assert_eq!(substring_cells("abc", -1, Some(2), c), "ab");
        assert_eq!(substring_cells("abc", 0, Some(-1), c), "abc");
        assert_eq!(substring_cells("abc", 0, Some(0), c), "");
        assert_eq!(substring_cells("abc", 3, Some(1), c), "");
        assert_eq!(substring_cells("abc", 0, Some(99), c), "abc");
        assert_eq!(substring_cells("", 0, None, c), "");
        // Hangul in a Japanese game: 2 cells each, no `&#NNNN;` inflation
        assert_eq!(substring_cells("정음x", 0, Some(4), c), "정음");
    }

    #[test]
    fn substring_depends_on_language() {
        let jp = jp();
        let kr = kr();
        // ‖ U+2016: 2 cells in JP (cp932 best-fit override), 1 cell in EUC-KR
        assert_eq!(substring_cells("‖ab", 2, Some(1), |ch| jp.char_cells(ch)), "a");
        assert_eq!(substring_cells("‖ab", 2, Some(1), |ch| kr.char_cells(ch)), "b");
        // tests/run_tests/sqn/substring.erb: "정음, " is 6 cells, SUBSTRING(s, 0, 4)
        assert_eq!(substring_cells("정음, ", 0, Some(4), |ch| kr.char_cells(ch)), "정음");
    }

    #[test]
    fn strfind_returns_cell_offsets() {
        let t = kr();
        let c = |ch| t.char_cells(ch);
        // tests/run_tests/basic/strfind.erb
        assert_eq!(strfind_cells("가나다", "다", None, c), 4);
        assert_eq!(strfind_cells("한글abc", "a", None, c), 4);
        assert_eq!(strfind_cells("─═║x", "x", None, c), 4);
        assert_eq!(strfind_cells("😀x", "x", None, c), 2);
        assert_eq!(strfind_cells("abcabc", "a", Some(1), c), 3);
        assert_eq!(strfind_cells("abc", "a", Some(1), c), -1);
        assert_eq!(strfind_cells("한글한", "한", Some(1), c), 4);
        assert_eq!(strfind_cells("abc", "z", None, c), -1);
        assert_eq!(strfind_cells("abc", "c", Some(5), c), -1);
        assert_eq!(strfind_cells("abc", "c", Some(-2), c), 2);
        assert_eq!(strfind_cells("abc", "", None, c), 0);
        // Emuera: `UFTstart >= target.Length` → -1, even for an empty needle
        assert_eq!(strfind_cells("", "", None, c), -1);
        assert_eq!(strfind_cells("정음x", "x", None, c), 4);
        assert_eq!(strfind_cells("‖ab", "b", None, c), 2);
        let j = jp();
        assert_eq!(strfind_cells("‖ab", "b", None, |ch| j.char_cells(ch)), 3);
    }

    #[test]
    fn pad_str_pads_by_cells() {
        let t = jp();
        let n = |s: &str| t.str_cells(s);
        assert_eq!(pad_str_cells("★●①".into(), 8, Alignment::Left, n("★●①")), "★●①  ");
        assert_eq!(pad_str_cells("★●①".into(), 6, Alignment::Left, n("★●①")), "★●①");
        assert_eq!(pad_str_cells("12".into(), 5, Alignment::Right, n("12")), "   12");
        assert_eq!(pad_str_cells("あ".into(), 1, Alignment::Right, n("あ")), "あ");
        assert_eq!(pad_str_cells("1".into(), -3, Alignment::Right, n("1")), "1");
        assert_eq!(pad_str_cells("7".into(), 4, Alignment::Center, n("7")), " 7  ");
        assert_eq!(pad_str_cells("한a".into(), 6, Alignment::Left, n("한a")), "한a   ");
        assert_eq!(pad_str_cells("‖".into(), 3, Alignment::Left, n("‖")), "‖ ");
    }
}
