use std::ops::Range;

use erars_ast::{BuiltinCommand, BuiltinMethod, PrintFlags};
use once_cell::sync::Lazy;
use regex::Regex;

macro_rules! static_regex {
    ($expr:literal) => {{
        static REGEX: Lazy<Regex> = Lazy::new(|| Regex::new($expr).unwrap());
        &*REGEX
    }};
}

#[derive(thiserror::Error, Debug)]
pub enum LexerError {
    #[error("]]로 닫히지 않았습니다.")]
    UnclosedPreprocess,
    #[error("잘못된 출력 플래그 입니다.")]
    InvalidPrintFlags,
    #[error("알수없는 코드입니다.")]
    UnknownCode,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum JumpType {
    Call,
    Jump,
    Goto,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CallJumpInfo {
    pub ty: JumpType,
    pub is_try: bool,
    pub is_form: bool,
    pub is_catch: bool,
    pub is_method: bool,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'s> {
    // @function
    FunctionLine(&'s str),

    // $label
    LabelLine(&'s str),

    // #header
    Dim(&'s str),
    DimS(&'s str),
    LocalSize(&'s str),
    LocalsSize(&'s str),
    Function,
    FunctionS,
    Single,
    Pri,
    Later,
    PreprocessLine(&'s str),

    PrintLine(PrintFlags, PrintType, &'s str),
    Data(&'s str),
    DataForm(&'s str),
    DataList,
    EndList,
    EndData,

    ReuseLastLine(&'s str),

    CallLine(CallJumpInfo, &'s str),
    Catch,
    EndCatch,

    CommandLine(BuiltinCommand, &'s str),
    MethodLine(BuiltinMethod, &'s str),
}

pub type Spanned<'s> = (Token<'s>, Range<usize>);

pub struct ErbLexer<'s> {
    lines: regex::CaptureMatches<'static, 's>,
    line_count: u32,
    prev_span: Range<usize>,
}

impl<'s> ErbLexer<'s> {
    pub fn new(s: &'s str) -> Self {
        Self {
            lines: static_regex!(r"^\s*([^\n;]*?)\s*(?:;[^\n]*)?\r?$").captures_iter(s),
            line_count: 0,
            prev_span: 0..0,
        }
    }

    fn next_line(&mut self) -> Option<(&'s str, Range<usize>)> {
        while let Some(line) = self.lines.next() {
            self.line_count += 1;
            let line = line.get(1).unwrap();
            let line_str = line.as_str();
            if line_str.is_empty() {
                continue;
            }

            return Some((line_str, line.range()));
        }

        None
    }

    pub fn current_line(&self) -> u32 {
        self.line_count
    }

    pub fn span(&self) -> Range<usize> {
        self.prev_span.clone()
    }

    pub fn next_token(&mut self) -> Result<Option<Token<'s>>, (LexerError, Range<usize>)> {
        let (line_str, span) = match self.next_line() {
            Some(v) => v,
            None => return Ok(None),
        };

        let tok = lex(line_str).map_err(|err| (err, span.clone()))?;

        match tok {
            Some(tok) => {
                self.prev_span = span;
                Ok(Some(tok))
            }
            None => Ok(None),
        }
    }
}

fn lex(s: &str) -> Result<Option<Token<'_>>, LexerError> {
    let tok = if let Some(func) = s.strip_prefix('@') {
        Token::FunctionLine(func)
    } else if let Some(label) = s.strip_prefix('$') {
        Token::LabelLine(label)
    } else if let Some(header) = s.strip_prefix('#') {
        todo!()
    } else if let Some(pp) = s.strip_prefix("[[") {
        if let Some(pp) = pp.strip_suffix("]]") {
            Token::PreprocessLine(pp.trim())
        } else {
            return Err(LexerError::UnclosedPreprocess);
        }
    } else if let Some(reuse) = static_regex!(r"^(?i:REUSELASTLINE)(?: (.*))?$").captures(s) {
        Token::ReuseLastLine(reuse.get(1).map_or("", |m| m.as_str()))
    } else if let Some(print) =
        static_regex!(r"^(?i:PRINT((?:SINGLE)?(?:DATA|V|S|FORMS?)?[LW]?(?:L?C)?))(?: (.*))?$")
            .captures(s)
    {
        let (flags, ty) = parse_print(&print[1])?;

        Token::PrintLine(flags, ty, print.get(2).map_or("", |m| m.as_str()))
    } else if let Some(call) =
        static_regex!(r"^(?i:(TRY)?(C)?(CALL|JUMP|GOTO)(FORM)?(F)?)(?: (.*))?$").captures(s)
    {
        Token::CallLine(
            CallJumpInfo {
                is_try: call.get(1).is_some(),
                is_catch: call.get(2).is_some(),
                ty: match call.get(3).unwrap().as_str() {
                    s if s.eq_ignore_ascii_case("CALL") => JumpType::Call,
                    s if s.eq_ignore_ascii_case("JUMP") => JumpType::Jump,
                    _ => JumpType::Goto,
                },
                is_form: call.get(4).is_some(),
                is_method: call.get(5).is_some(),
            },
            call.get(6).map_or("", |m| m.as_str()),
        )
    } else {
        return Err(LexerError::UnknownCode);
    };

    Ok(Some(tok))
}

fn parse_print(mut s: &str) -> Result<(PrintFlags, PrintType), LexerError> {
    let mut flags = PrintFlags::empty();

    if let Some(ss) = s.strip_prefix("SINGLE") {
        flags |= PrintFlags::SINGLE;
        s = ss;
    }

    let ty = if let Some(ss) = s.strip_prefix("FORMS") {
        s = ss;
        PrintType::FormS
    } else if let Some(ss) = s.strip_prefix("FORM") {
        s = ss;
        PrintType::Form
    } else if let Some(ss) = s.strip_prefix("DATA") {
        s = ss;
        PrintType::Data
    } else if let Some(ss) = s.strip_prefix('V') {
        s = ss;
        PrintType::V
    } else if let Some(ss) = s.strip_prefix('S') {
        s = ss;
        PrintType::S
    } else {
        PrintType::Plain
    };

    let f = parse_print_flags(s)?;
    flags |= f;

    Ok((flags, ty))
}

fn parse_print_flags(mut s: &str) -> Result<PrintFlags, LexerError> {
    let mut flags = PrintFlags::empty();

    if let Some(ss) = s.strip_prefix("SINGLE") {
        s = ss;
        flags |= PrintFlags::SINGLE;
    }

    if let Some(ss) = s.strip_prefix('L') {
        if let Some(ss) = ss.strip_prefix('C') {
            flags |= PrintFlags::LEFT_ALIGN;
            s = ss;
        } else {
            flags |= PrintFlags::NEWLINE;
            s = ss;
        }
    } else if let Some(ss) = s.strip_prefix('W') {
        flags |= PrintFlags::WAIT | PrintFlags::NEWLINE;
        s = ss;
    }

    if let Some(ss) = s.strip_prefix('C') {
        flags |= PrintFlags::RIGHT_ALIGN;
        s = ss;
    } else if let Some(ss) = s.strip_prefix("LC") {
        flags |= PrintFlags::LEFT_ALIGN;
        s = ss;
    }

    if s.is_empty() {
        Ok(flags)
    } else {
        Err(LexerError::InvalidPrintFlags)
    }
}