use erars_ast::PrintFlags;

use crate::{Bump, PrintType};

pub fn cut_ident(line: &str) -> (&str, &str) {
    // Scanning bytes rather than decoding a `char` per byte is the win here:
    // the profile charged 2.5% of parse+compile self time to `is_ident_body`
    // and this closure, and the lexer calls it once for every one of the
    // corpus's 890_801 lines. Every ASCII byte that ends an identifier is a
    // whole character, so `pos` never lands inside a multi-byte one.
    //
    // U+3000 IDEOGRAPHIC SPACE ends one too — Emuera counts it as whitespace
    // by default (`Sub/LexicalAnalyzer.cs:749-752`,
    // `Config/ConfigData.cs:112`) and the corpus writes `SETCOLOR　0x000000`
    // — and it is the one terminator that is not ASCII. Its lead byte `0xE3`
    // is shared with all of hiragana and katakana, so the two continuation
    // bytes decide; both differ for every other character in the range.
    let bytes = line.as_bytes();
    let mut pos = 0;
    while let Some(&b) = bytes.get(pos) {
        if !is_ident_body_byte(b) || (b == 0xE3 && bytes[pos + 1..].starts_with(b"\x80\x80")) {
            return (&line[..pos], &line[pos..]);
        }
        pos += 1;
    }

    (line, "")
}

/// Drops the one separator character that follows an instruction name.
///
/// Emuera checks that the character after the name is `;`, a space, a tab, or
/// — while `全角スペースをホワイトスペースに含める` is on — U+3000, then
/// consumes exactly that one character and hands the remainder to the
/// instruction verbatim (`GameProc/LogicalLineParser.cs:428-436`). So `PRINT
/// \u{3000}a` prints `a` while `PRINT  a` keeps the second space.
pub fn strip_inst_separator(args: &str) -> &str {
    match args.as_bytes().first() {
        Some(b' ' | b'\t') => &args[1..],
        // U+3000 is `E3 80 80`; no other character starts with those bytes.
        Some(0xE3) if args.as_bytes()[1..].starts_with(b"\x80\x80") => &args[3..],
        _ => args,
    }
}

/// Length of the inline marker at the head of `bytes`, if one is live there.
///
/// `;!;` is always a marker and `;#;` is one only while `-debug` is on
/// (`Sub/LexicalAnalyzer.cs:753-765`, and the twin that handles a marker met
/// mid-expression at `:959-970`). A marker is *whitespace*: the three
/// characters vanish and the rest of the line is code. Any other `;` starts a
/// comment.
pub fn marker_len(bytes: &[u8], debug_mode: bool) -> Option<usize> {
    match bytes.first() {
        Some(b';') if marker_tail(&bytes[1..], debug_mode) => Some(3),
        _ => None,
    }
}

/// [`marker_len`] for a caller that has already stepped over the `;`.
pub fn marker_tail(after_semi: &[u8], debug_mode: bool) -> bool {
    after_semi.starts_with(b"!;") || (debug_mode && after_semi.starts_with(b"#;"))
}

/// Concatenates `parts` into one `&str` allocated in `b`.
fn concat_in<'a>(parts: &[&str], b: &'a Bump) -> &'a str {
    let buf = b.alloc_slice_fill_copy(parts.iter().map(|part| part.len()).sum(), 0u8);
    let mut at = 0;

    for part in parts {
        buf[at..at + part.len()].copy_from_slice(part.as_bytes());
        at += part.len();
    }

    // Every part is a whole `str`, so the concatenation is valid UTF-8.
    unsafe { std::str::from_utf8_unchecked(buf) }
}

/// Cuts the line at the first `;` that actually starts a comment, splicing out
/// any inline marker met on the way.
///
/// Emuera's `;` is token-positional: `SkipWhiteSpace` seeks to end-of-line
/// when it meets one (`Sub/LexicalAnalyzer.cs:753-765`), but a `;` inside a
/// `"…"` literal is consumed by `ReadString` and never reaches it — so
/// `HTML_PRINT "&lt;"` has to survive.
///
/// A marker ([`marker_len`]) is not a comment at all, and the line continues
/// past it, so it has to be removed from the middle of the text. That is the
/// only path here that allocates, and no line in either game triggers it.
pub fn cut_comment<'a>(line: &'a str, debug_mode: bool, b: &'a Bump) -> &'a str {
    let bytes = line.as_bytes();
    let Some(semi) = memchr::memchr(b';', bytes) else {
        return line;
    };
    // Overwhelmingly the common case: with no quote ahead of it no literal
    // can be open, so the first `;` is the comment. One extra `memchr` over a
    // short prefix is much cheaper than walking every one of the corpus's
    // 890_801 lines.
    if memchr::memchr(b'"', &bytes[..semi]).is_none()
        && marker_len(&bytes[semi..], debug_mode).is_none()
    {
        return &line[..semi];
    }

    // The text kept on either side of each spliced marker. Stays empty — and
    // so never allocates — unless a marker is actually met.
    let mut kept: Vec<&str> = Vec::new();
    let mut seg = 0;
    let mut pos = 0;
    let cut = 'scan: loop {
        // Outside a literal: whichever of the two comes first decides.
        let Some(at) = memchr::memchr2(b';', b'"', &bytes[pos..]).map(|off| pos + off) else {
            break line.len();
        };
        if bytes[at] == b';' {
            match marker_len(&bytes[at..], debug_mode) {
                Some(len) => {
                    kept.push(&line[seg..at]);
                    pos = at + len;
                    seg = pos;
                    continue;
                }
                None => break at,
            }
        }

        // Inside a literal, up to its closing quote. `\` escapes anything,
        // `"` included (`erars-compiler/src/parser/expr.rs:242-254`).
        pos = at + 1;
        loop {
            match memchr::memchr2(b'\\', b'"', &bytes[pos..]) {
                Some(off) if bytes[pos + off] == b'"' => {
                    pos += off + 1;
                    break;
                }
                // `\` plus the first byte of what it escapes. Any remaining
                // continuation bytes are >= 0x80 and so never match a
                // delimiter; a trailing `\` is clamped to the end and leaves
                // the literal unterminated, which the parser rejects.
                Some(off) => pos = (pos + off + 2).min(bytes.len()),
                // Unterminated literal: nothing past it can be a comment.
                None => break 'scan line.len(),
            }
        }
    };

    if kept.is_empty() {
        return &line[..cut];
    }

    kept.push(&line[seg..cut]);
    concat_in(&kept, b)
}

/// One bit per ASCII code point, set when the byte may appear in an
/// identifier. `extra` names code points that are excluded on top of the
/// punctuation, control and whitespace ranges.
const fn ident_mask(extra: &[std::ops::RangeInclusive<u8>]) -> u128 {
    let mut mask = 0u128;
    let mut b = 0u8;
    while b < 128 {
        let mut ok = !matches!(b, b'!'..=b'/' | b':'..=b'@' | b'['..=b'^' | b'{'..=b'~')
            && !b.is_ascii_control()
            && !b.is_ascii_whitespace();

        let mut k = 0;
        while k < extra.len() {
            if *extra[k].start() <= b && b <= *extra[k].end() {
                ok = false;
            }
            k += 1;
        }

        if ok {
            mask |= 1 << b;
        }
        b += 1;
    }
    mask
}

const IDENT_HEAD: u128 = ident_mask(&[b'0'..=b'9']);
const IDENT_BODY: u128 = ident_mask(&[]);

/// Non-ASCII bytes are always identifier material: every code point the
/// original `matches!` / `is_ascii_control` / `is_ascii_whitespace` predicate
/// rejected is ASCII, so a byte >= 0x80 — a UTF-8 lead or continuation byte —
/// always qualifies.
fn in_ident_mask(mask: u128, b: u8) -> bool {
    b >= 0x80 || mask >> b & 1 != 0
}

fn is_ident_body_byte(b: u8) -> bool {
    in_ident_mask(IDENT_BODY, b)
}

/// U+3000 IDEOGRAPHIC SPACE is the one non-ASCII character an identifier may
/// not contain. `ReadSingleIdentifier` ends the identifier on it when
/// `SystemAllowFullSpace` is on and throws `UnexpectedFullWidthSpace` when it
/// is off (`Sub/LexicalAnalyzer.cs:429-432`), so no build of Emuera ever
/// reads one into a name. The corpus relies on it: `FORMATION.ERB:419` is
/// `#DIM DYNAMIC　L_ACTOR` and `EVENT_P.ERB:135` is `IF ASSI　!= L_MOTHER`.
pub fn is_ident_head(c: char) -> bool {
    match c as u32 {
        0x3000 => false,
        cp if cp >= 0x80 => true,
        cp => IDENT_HEAD >> cp & 1 != 0,
    }
}

pub fn is_ident_body(c: char) -> bool {
    match c as u32 {
        0x3000 => false,
        cp if cp >= 0x80 => true,
        cp => IDENT_BODY >> cp & 1 != 0,
    }
}

pub fn is_ident(i: &str) -> bool {
    let mut chars = i.chars();
    if let Some(c) = chars.next() {
        is_ident_head(c) && chars.all(is_ident_body)
    } else {
        false
    }
}

pub fn parse_print_flags(mut s: &str) -> (&str, PrintFlags) {
    let mut flags = PrintFlags::empty();

    if let Some(ss) = strip_prefix_ignore_case(s, "SINGLE") {
        s = ss;
        flags |= PrintFlags::SINGLE;
    }

    if let Some(ss) = strip_prefix_ignore_case_char(s, 'C') {
        flags |= PrintFlags::RIGHT_ALIGN;
        s = ss;
    } else if let Some(ss) = strip_prefix_ignore_case(s, "LC") {
        flags |= PrintFlags::LEFT_ALIGN;
        s = ss;
    }

    if let Some(ss) = strip_prefix_ignore_case_char(s, 'D') {
        flags |= PrintFlags::DEFAULT_COLOR;
        s = ss;
    } else if let Some(ss) = strip_prefix_ignore_case_char(s, 'K') {
        flags |= PrintFlags::FORCE_KANA;
        s = ss;
    }

    if let Some(ss) = strip_prefix_ignore_case_char(s, 'L') {
        flags |= PrintFlags::NEWLINE;
        s = ss;
    } else if let Some(ss) = strip_prefix_ignore_case_char(s, 'W') {
        flags |= PrintFlags::WAIT | PrintFlags::NEWLINE;
        s = ss;
    }

    (s, flags)
}
pub fn strip_prefix_ignore_case_char(s: &str, pat: char) -> Option<&str> {
    let mut chars = s.chars();
    let next = chars.next()?;

    if next.eq_ignore_ascii_case(&pat) {
        Some(chars.as_str())
    } else {
        None
    }
}

pub fn strip_prefix_ignore_case<'s>(s: &'s str, pat: &str) -> Option<&'s str> {
    if !s.is_char_boundary(pat.len()) {
        None
    } else {
        let (l, r) = unsafe { (s.get_unchecked(..pat.len()), s.get_unchecked(pat.len()..)) };

        if l.eq_ignore_ascii_case(pat) {
            Some(r)
        } else {
            None
        }
    }
}

pub fn parse_print_left(mut s: &str) -> (PrintFlags, PrintType) {
    let mut flags = PrintFlags::empty();

    if let Some(ss) = strip_prefix_ignore_case(s, "SINGLE") {
        flags |= PrintFlags::SINGLE;
        s = ss;
    }

    let ty = if let Some(ss) = strip_prefix_ignore_case(s, "FORMS") {
        s = ss;
        PrintType::FormS
    } else if let Some(ss) = strip_prefix_ignore_case(s, "FORM") {
        s = ss;
        PrintType::Form
    } else if let Some(ss) = strip_prefix_ignore_case(s, "DATA") {
        s = ss;
        PrintType::Data
    } else if let Some(ss) = strip_prefix_ignore_case_char(s, 'V') {
        s = ss;
        PrintType::V
    } else if let Some(ss) = strip_prefix_ignore_case_char(s, 'S') {
        s = ss;
        PrintType::S
    } else {
        PrintType::Plain
    };

    let (_, f) = parse_print_flags(s);
    flags |= f;

    (flags, ty)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The pre-mask predicates, plus the U+3000 rule `is_ident_head`
    /// documents: `ReadSingleIdentifier` never reads an ideographic space into
    /// a name (`Sub/LexicalAnalyzer.cs:429-432`).
    fn is_ident_head_ref(c: char) -> bool {
        !matches!(c, '!'..='/' | ':'..='@' | '['..='^' | '{'..='~' | '0'..='9' | '\u{3000}')
            && !c.is_ascii_control()
            && !c.is_ascii_whitespace()
    }

    #[test]
    fn cut_comment_skips_string_literals() {
        let b = Bump::new();
        let cut = |line| cut_comment(line, false, &b);

        assert_eq!(cut("PRINTV A"), "PRINTV A");
        assert_eq!(cut("PRINTV A ; note"), "PRINTV A ");
        assert_eq!(cut("; whole line"), "");

        // A `;` inside a literal is text, one outside it still cuts.
        assert_eq!(cut(r#"X = "a;b""#), r#"X = "a;b""#);
        assert_eq!(cut(r#"X = "a;b" ; note"#), r#"X = "a;b" "#);
        assert_eq!(cut(r#"X = "a" + "b;c""#), r#"X = "a" + "b;c""#);
        assert_eq!(cut(r#"X = "a";Y = "b""#), r#"X = "a""#);

        // `\"` does not close the literal, `\\` does not escape the quote.
        assert_eq!(cut(r#"X = "a\";b""#), r#"X = "a\";b""#);
        assert_eq!(cut(r#"X = "a\\";b"#), r#"X = "a\\""#);

        // An unterminated literal swallows the rest of the line, exactly as
        // the expression parser will when it fails on it.
        assert_eq!(cut(r#"X = "a;b"#), r#"X = "a;b"#);
        // A trailing backslash must not run off the end.
        assert_eq!(cut(r#"X = "a\"#), r#"X = "a\"#);
        // Multi-byte payloads are stepped over safely.
        assert_eq!(cut(r#"X = "あ\あ;い" ; c"#), r#"X = "あ\あ;い" "#);
    }

    /// `;!;` is whitespace and `;#;` is whitespace only under `-debug`
    /// (`Sub/LexicalAnalyzer.cs:753-765`), so the code on both sides of one
    /// survives — mid-line included.
    #[test]
    fn cut_comment_splices_inline_markers() {
        let b = Bump::new();

        assert_eq!(cut_comment("A = 1 ;!; B = 2", false, &b), "A = 1  B = 2");
        assert_eq!(cut_comment("A = 1 ;!; B = 2 ; note", false, &b), "A = 1  B = 2 ");
        // Two of them, and the comment after still cuts.
        assert_eq!(cut_comment("A;!;B;!;C;D", false, &b), "ABC");

        // `;#;` is a comment without the flag and whitespace with it.
        assert_eq!(cut_comment("A = 1 ;#; B = 2", false, &b), "A = 1 ");
        assert_eq!(cut_comment("A = 1 ;#; B = 2", true, &b), "A = 1  B = 2");

        // Inside a literal a marker is text, exactly like a `;`.
        assert_eq!(cut_comment(r#"X = "a;!;b""#, false, &b), r#"X = "a;!;b""#);

        // A marker is not a comment, so an unterminated literal after one is
        // still the end of the line.
        assert_eq!(cut_comment(r#";!;X = "a;b"#, false, &b), r#"X = "a;b"#);
    }

    fn is_ident_body_ref(c: char) -> bool {
        !matches!(c, '!'..='/' | ':'..='@' | '['..='^' | '{'..='~' | '\u{3000}')
            && !c.is_ascii_control()
            && !c.is_ascii_whitespace()
    }

    /// The masks replaced these predicates for speed; they must classify every
    /// code point in the language — including the corpus's Korean and Japanese
    /// identifiers — exactly as before.
    #[test]
    fn masks_match_the_original_predicates() {
        for c in (0..=0x10FFFFu32).filter_map(char::from_u32) {
            assert_eq!(is_ident_head(c), is_ident_head_ref(c), "head {c:?}");
            assert_eq!(is_ident_body(c), is_ident_body_ref(c), "body {c:?}");
        }
    }

    /// `cut_ident` scans bytes, so it must still split on the first
    /// non-identifier *character* and never inside a multi-byte one.
    #[test]
    fn cut_ident_splits_on_characters() {
        for (line, want) in [
            ("PRINTFORMW abc", ("PRINTFORMW", " abc")),
            ("A:1 = 2", ("A", ":1 = 2")),
            ("만트라 = 1", ("만트라", " = 1")),
            ("가나다", ("가나다", "")),
            ("＄full_width", ("＄full_width", "")),
            ("", ("", "")),
            (";comment", ("", ";comment")),
        ] {
            assert_eq!(cut_ident(line), want, "{line:?}");
        }
    }
}
