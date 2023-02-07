use erars_ast::PrintFlags;

use crate::PrintType;

pub fn cut_ident(line: &str) -> (&str, &str) {
    line.find(|c| !is_ident_body(c)).map_or((line, ""), |pos| unsafe {
        (line.get_unchecked(..pos), line.get_unchecked(pos..))
    })
}

pub fn cut_comment(line: &str) -> &str {
    if let Some(pos) = memchr::memchr(b';', line.as_bytes()) {
        unsafe { line.get_unchecked(..pos) }
    } else {
        line
    }
}

pub fn is_ident_head(c: char) -> bool {
    !matches!(c, '!'..='/' | ':'..='@' | '['..='^' | '{'..='~' | '0'..='9')
        && !c.is_ascii_control()
        && !c.is_ascii_whitespace()
}

pub fn is_ident_body(c: char) -> bool {
    !matches!(c, '!'..='/' | ':'..='@' | '['..='^' | '{'..='~')
        && !c.is_ascii_control()
        && !c.is_ascii_whitespace()
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
