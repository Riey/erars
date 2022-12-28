mod inst;
mod sharp;

use logos::Logos;

use erars_ast::*;
use regex_automata::dfa::dense;
use regex_automata::dfa::Automaton;

pub use inst::InstructionCode;
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

unsafe fn parse_print_button(s: &str) -> (PrintFlags, &str) {
    // skip PRINTPLAIN
    let s = s.get_unchecked("PRINTBUTTON".len()..);

    let (flags, s) = if let Some(s) = strip_prefix_ignore_case(s, "LC") {
        (PrintFlags::LEFT_ALIGN, s)
    } else if let Some(s) = strip_prefix_ignore_case_char(s, 'C') {
        (PrintFlags::RIGHT_ALIGN, s)
    } else {
        (PrintFlags::empty(), s)
    };

    (flags, trim_text(s))
}

unsafe fn parse_print_plain(s: &str) -> (PrintType, &str) {
    // skip PRINTPLAIN
    let s = s.get_unchecked("PRINTPLAIN".len()..);

    let (ty, s) = if let Some(s) = strip_prefix_ignore_case(s, "FORM") {
        (PrintType::Form, s)
    } else {
        (PrintType::Plain, s)
    };

    (ty, trim_text(s))
}

unsafe fn parse_print(mut s: &str) -> (PrintFlags, PrintType, &str) {
    let mut flags = PrintFlags::empty();

    if let Some(ss) = s.strip_prefix("DEBUG") {
        flags |= PrintFlags::DEBUG;
        s = ss;
    }

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
}

impl PreprocessorRegex {
    pub fn from_bytes(inst_bytes: &'static [u8], sharp_bytes: &'static [u8]) -> Self {
        Self {
            inst_dfa: dense::DFA::from_bytes(inst_bytes).unwrap().0,
            sharp_dfa: dense::DFA::from_bytes(sharp_bytes).unwrap().0,
        }
    }
}

pub struct Preprocessor<'s> {
    re: &'s PreprocessorRegex,
    s: &'s str,

    line_pos: usize,
    span_begin: usize,
    span_end: usize,
}

impl<'s> Preprocessor<'s> {
    pub fn new(re: &'s PreprocessorRegex, s: &'s str) -> Self {
        let no_bom = s.trim_start_matches('\u{feff}');
        let span_begin = no_bom.as_ptr() as usize - s.as_ptr() as usize;

        Self {
            re,
            s: no_bom,

            line_pos: 1,
            span_begin,
            span_end: span_begin,
        }
    }

    fn current_pos(&self) -> usize {
        self.s.as_ptr() as usize
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

    fn next_raw_line<'a>(&mut self, line_buf: &'a mut String) -> &'a str
    where
        's: 'a,
    {
        self.skip_ws();

        self.span_begin = self.current_pos();

        let line = if let Some(open_brace) = self.s.strip_prefix('{') {
            let (raw, left) = open_brace.split_once('}').unwrap_or((open_brace, ""));
            self.s = left;
            line_buf.clear();
            for line in raw.split_terminator('\n') {
                line_buf.push_str(line);
            }
            line_buf
        } else {
            let (line, left) = self.s.split_once('\n').unwrap_or((self.s, ""));
            self.s = left;
            line
        };

        self.span_end = self.current_pos();

        line
    }

    pub fn next_line<'a>(&mut self, line_buf: &'a mut String) -> Option<EraLine<'a>>
    where
        's: 'a,
    {
        use num_traits::FromPrimitive;

        let line = self.next_raw_line(line_buf);

        if line.is_empty() {
            None
        } else if let Some(line) = line.strip_prefix('#') {
            if let Some(m) = self.re.sharp_dfa.find_leftmost_fwd(line.as_bytes()).unwrap() {
                if let Some(sharp) = SharpCode::from_u32(m.pattern().as_u32()) {
                    let line = unsafe { line.get_unchecked(<&str>::from(sharp).len()..) };
                    Some(EraLine::SharpLine { sharp, args: line })
                } else {
                    unreachable!()
                }
            } else {
                panic!("Unknown sharp line")
            }
        } else if let Some(line) = line.strip_prefix('@') {
            Some(EraLine::FunctionLine(line))
        } else if let Some(line) = line.strip_prefix('$') {
            Some(EraLine::GotoLine(line))
        } else if let Some(m) = self.re.inst_dfa.find_leftmost_fwd(line.as_bytes()).unwrap() {
            if let Some(inst) = InstructionCode::from_u32(m.pattern().as_u32()) {
                let line = unsafe { line.get_unchecked(<&str>::from(inst).len()..) };
                Some(EraLine::InstLine { inst, args: line })
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

            Some(EraLine::VarAssign {
                lhs: left,
                complex_op,
                rhs: right,
            })
        } else if let Some(left) = line.strip_prefix("++") {
            Some(EraLine::VarInc {
                lhs: left,
                is_pre: true,
                is_inc: true,
            })
        } else if let Some(left) = line.strip_prefix("--") {
            Some(EraLine::VarInc {
                lhs: left,
                is_pre: true,
                is_inc: false,
            })
        } else {
            panic!("Unknown line: {line}")
        }
    }

    pub fn span(&self) -> std::ops::Range<usize> {
        self.span_begin..self.span_end
    }

    pub fn line_pos(&self) -> usize {
        self.line_pos
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum EraLine<'s> {
    FunctionLine(&'s str),
    SharpLine {
        sharp: SharpCode,
        args: &'s str,
    },
    GotoLine(&'s str),

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

#[derive(Debug, Eq, PartialEq)]
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
