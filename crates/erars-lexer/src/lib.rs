mod inst;
mod sharp;

use std::ops::Range;

use logos::Logos;

use erars_ast::*;
use regex_automata::dfa::dense;
use regex_automata::dfa::Automaton;

pub use bumpalo::Bump;
pub use inst::InstructionCode;
use regex_automata::dfa::regex;
pub use sharp::SharpCode;
pub use strum::IntoEnumIterator;

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

fn strip_prefix_ignore_case_char(s: &str, pat: char) -> Option<&str> {
    let mut chars = s.chars();
    let next = chars.next()?;

    if next.eq_ignore_ascii_case(&pat) {
        Some(chars.as_str())
    } else {
        None
    }
}

fn strip_prefix_ignore_case<'s>(s: &'s str, pat: &str) -> Option<&'s str> {
    if !s.is_char_boundary(pat.len()) {
        None
    } else {
        let (l, r) = s.split_at(pat.len());

        if l.eq_ignore_ascii_case(pat) {
            Some(r)
        } else {
            None
        }
    }
}

fn trim_text(s: &str) -> &str {
    let s = s.strip_prefix(' ').unwrap_or(s);
    let s = s.strip_suffix('\r').unwrap_or(s);
    s
}

unsafe fn parse_print(mut s: &str) -> (PrintFlags, PrintType, &str) {
    let mut flags = PrintFlags::empty();

    // if let Some(ss) = s.strip_prefix("DEBUG") {
    //     flags |= PrintFlags::DEBUG;
    //     s = ss;
    // }

    // skip PRINT
    s = s.get_unchecked("PRINT".len()..);

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

    let (s, f) = parse_print_flags(s);
    flags |= f;

    (flags, ty, trim_text(s))
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrintType {
    Plain,
    Form,
    FormS,
    S,
    V,
    Data,
}

pub struct PreprocessorRegex {
    inst_dfa: dense::DFA<&'static [u32]>,
    sharp_dfa: dense::DFA<&'static [u32]>,
    skip_end: regex::Regex,
    skip_start: regex::Regex,
}

impl PreprocessorRegex {
    pub fn from_bytes(inst_bytes: &'static [u8], sharp_bytes: &'static [u8]) -> Self {
        macro_rules! build_re {
            ($pat:literal) => {
                regex::Builder::new()
                    .syntax(regex_automata::SyntaxConfig::new().case_insensitive(true).utf8(false))
                    .build($pat)
                    .unwrap()
            };
        }
        Self {
            inst_dfa: dense::DFA::from_bytes(inst_bytes).unwrap().0,
            sharp_dfa: dense::DFA::from_bytes(sharp_bytes).unwrap().0,
            skip_end: build_re!("\\[ *SKIPEND *\\]"),
            skip_start: build_re!("^\\[ *SKIPSTART *\\]"),
        }
    }
}

pub struct Preprocessor<'s> {
    re: &'s PreprocessorRegex,
    s: &'s str,

    line_pos: usize,
    start_len: usize,
    span_begin: usize,
    span_end: usize,
}

impl<'s> Preprocessor<'s> {
    pub fn new(re: &'s PreprocessorRegex, s: &'s str) -> Self {
        let no_bom = s.trim_start_matches('\u{feff}');
        let span_begin = no_bom.as_ptr() as usize - s.as_ptr() as usize;
        let s = no_bom;

        Self {
            re,
            s,

            line_pos: 0,
            start_len: s.len() + span_begin,
            span_begin,
            span_end: span_begin,
        }
    }

    fn current_pos(&self) -> usize {
        self.start_len - self.s.len()
    }

    fn skip_ws(&mut self) {
        let mut chars = self.s.chars();

        loop {
            match chars.next() {
                Some(' ' | '\t' | '\r') => {}
                Some('\n') => {
                    self.line_pos += 1;
                }
                Some(';' | '；') => {
                    if let Some(s) = chars.as_str().split_once('\n') {
                        self.line_pos += 1;
                        chars = s.1.chars();
                    } else {
                        self.s = "";
                        break;
                    }
                }
                Some(ch) => {
                    let ch_len = ch.len_utf8();
                    let s = chars.as_str();

                    self.s = unsafe {
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            s.as_ptr().sub(ch_len),
                            s.len() + ch_len,
                        ))
                    };

                    break;
                }
                None => {
                    self.s = "";
                    break;
                }
            }
        }
    }

    fn next_raw_line<'a>(&mut self, b: &'a Bump) -> Result<&'a str, (String, Range<usize>)>
    where
        's: 'a,
    {
        loop {
            self.skip_ws();

            self.span_begin = self.current_pos();

            let line = if let Some(open_brace) = self.s.strip_prefix('{') {
                let (raw, left) = open_brace.split_once('}').unwrap_or((open_brace, ""));
                self.s = left;
                unsafe {
                    let buf =
                        b.alloc_layout(std::alloc::Layout::from_size_align_unchecked(raw.len(), 1));

                    let mut start = 0;

                    for line in raw.split_terminator('\n') {
                        self.line_pos += 1;
                        start += line.len();
                        std::ptr::copy_nonoverlapping(
                            line.as_ptr(),
                            buf.as_ptr().add(start),
                            line.len(),
                        );
                    }

                    std::str::from_utf8_unchecked(std::slice::from_raw_parts(buf.as_ptr(), start))
                }
            } else {
                self.line_pos += 1;
                let (line, left) = self.s.split_once('\n').unwrap_or((self.s, ""));
                self.s = left;
                line
            };
            self.span_end = if !self.s.is_empty() {
                // skip newline
                self.current_pos() - 1
            } else {
                self.current_pos()
            };

            if line.starts_with('[') {
                if let Some(_) = self.re.skip_start.find_earliest(line.as_bytes()) {
                    let Some(end) = self.re.skip_end.find_earliest(self.s.as_bytes()) else {
                        return Err(("No SKIPEND".to_string(), self.span()));
                    };
                    self.line_pos +=
                        memchr::memchr_iter(b'\n', self.s[..end.start()].as_bytes()).count();
                    self.s = &self.s[end.end()..];
                } else {
                    log::warn!("TODO: {line}");
                    // TODO
                }
                continue;
            }

            break Ok(line);
        }
    }

    pub fn next_line<'a>(
        &mut self,
        b: &'a Bump,
    ) -> Result<Option<EraLine<'a>>, (String, Range<usize>)>
    where
        's: 'a,
    {
        use num_traits::FromPrimitive;

        let line = self.next_raw_line(b)?;

        debug_assert!(!line.ends_with('\n'));

        if line.is_empty() {
            Ok(None)
        } else if let Some(line) = line.strip_prefix('#') {
            if let Some(m) = self.re.sharp_dfa.find_leftmost_fwd(line.as_bytes()).unwrap() {
                if let Some(sharp) = SharpCode::from_u32(m.pattern().as_u32()) {
                    let line =
                        unsafe { line.get_unchecked(<&str>::from(sharp).len()..) }.trim_start();
                    Ok(Some(EraLine::SharpLine { sharp, args: line }))
                } else {
                    unreachable!()
                }
            } else {
                Err((format!("Unknown sharp line: {line}"), self.span()))
            }
        } else if let Some(line) = line.strip_prefix('@') {
            Ok(Some(EraLine::FunctionLine(line)))
        } else if let Some(line) = line.strip_prefix('$') {
            Ok(Some(EraLine::GotoLine(line)))
        } else if let Some(m) = self.re.inst_dfa.find_leftmost_fwd(line.as_bytes()).unwrap() {
            if let Some(inst) = InstructionCode::from_u32(m.pattern().as_u32()) {
                if inst == InstructionCode::PRINT {
                    let (flags, ty, line) = unsafe { parse_print(line) };
                    Ok(Some(EraLine::PrintLine {
                        flags,
                        ty,
                        args: line,
                    }))
                } else {
                    let line = unsafe { line.get_unchecked(<&str>::from(inst).len()..) };
                    let line = match inst {
                        InstructionCode::REUSELASTLINE | InstructionCode::THROW => {
                            line.strip_prefix(' ').unwrap_or(line)
                        }
                        _ => line.trim_start_matches(' '),
                    };
                    Ok(Some(EraLine::InstLine { inst, args: line }))
                }
            } else {
                unreachable!()
            }
        } else if let Some((mut left, right)) = line.split_once('=') {
            let complex_op = if let Some(l) = left.strip_suffix('+') {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::Add))
            } else if let Some(l) = left.strip_suffix('-') {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::Sub))
            } else if let Some(l) = left.strip_suffix('*') {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::Mul))
            } else if let Some(l) = left.strip_suffix('/') {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::Div))
            } else if let Some(l) = left.strip_suffix('%') {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::Rem))
            } else if let Some(l) = left.strip_suffix("<<") {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::Lhs))
            } else if let Some(l) = left.strip_suffix(">>") {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::Rhs))
            } else if let Some(l) = left.strip_suffix('^') {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::BitXor))
            } else if let Some(l) = left.strip_suffix('|') {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::BitOr))
            } else if let Some(l) = left.strip_suffix('&') {
                left = l;
                Some(ComplexAssign::Bin(BinaryOperator::BitAnd))
            } else if let Some(l) = left.strip_suffix('\'') {
                left = l;
                Some(ComplexAssign::Str)
            } else {
                None
            };

            Ok(Some(EraLine::VarAssign {
                lhs: left,
                complex_op,
                rhs: right,
            }))
        } else if let Some(left) = line.trim_end().strip_suffix("++") {
            Ok(Some(EraLine::VarInc {
                lhs: left,
                is_pre: false,
                is_inc: true,
            }))
        } else if let Some(left) = line.trim_end().strip_suffix("--") {
            Ok(Some(EraLine::VarInc {
                lhs: left,
                is_pre: false,
                is_inc: false,
            }))
        } else if let Some(left) = line.strip_prefix("++") {
            Ok(Some(EraLine::VarInc {
                lhs: left,
                is_pre: true,
                is_inc: true,
            }))
        } else if let Some(left) = line.strip_prefix("--") {
            Ok(Some(EraLine::VarInc {
                lhs: left,
                is_pre: true,
                is_inc: false,
            }))
        } else {
            Err((format!("Unknown line: {line}"), self.span()))
        }
    }

    pub fn span(&self) -> std::ops::Range<usize> {
        self.span_begin..self.span_end
    }

    pub fn script_pos(&self) -> ScriptPosition {
        ScriptPosition {
            line: self.line_pos as _,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum EraLine<'s> {
    FunctionLine(&'s str),
    SharpLine {
        sharp: SharpCode,
        args: &'s str,
    },
    GotoLine(&'s str),

    PrintLine {
        flags: PrintFlags,
        ty: PrintType,
        args: &'s str,
    },

    InstLine {
        inst: InstructionCode,
        args: &'s str,
    },

    VarInc {
        lhs: &'s str,
        is_pre: bool,
        is_inc: bool,
    },

    VarAssign {
        lhs: &'s str,
        complex_op: Option<ComplexAssign>,
        rhs: &'s str,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ComplexAssign {
    Bin(BinaryOperator),
    Str,
}

#[derive(Clone, Copy, Debug, Logos)]
pub enum ConfigToken<'s> {
    #[regex(r"[^:\r\n\u{FEFF}][^:\r\n]*:[^\r\n]*", |lex| lex.slice().split_once(':').unwrap())]
    Line((&'s str, &'s str)),

    #[error]
    // BOM
    #[token("\u{FEFF}", logos::skip)]
    #[regex(r"[ \t\r\n　]+", logos::skip)]
    #[regex(r"[；;][^\n]*", logos::skip)]
    Error,
}
