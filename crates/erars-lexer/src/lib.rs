mod erb;

use logos::{internal::LexerInternal, Lexer, Logos};

pub use erb::{CallJumpInfo, ErbLexer, JumpType, LexerError, PrintType, Spanned, Token};

fn lex_line_left_erh<'s>(lex: &mut Lexer<'s, ErhToken<'s>>) -> &'s str {
    let args = lex.remainder();
    let s = match args.split_once('\n') {
        Some((args, _)) => {
            lex.bump_unchecked(args.len());
            args
        }
        None => {
            lex.bump_unchecked(args.len());
            args
        }
    };

    let s = s.strip_prefix(' ').unwrap_or(s);

    s.strip_suffix('\r').unwrap_or(s)
}

#[derive(Logos, Debug, Eq, PartialEq)]
pub enum ErhToken<'s> {
    #[token("#DEFINE", lex_line_left_erh)]
    Define(&'s str),

    #[token("#DIM", lex_line_left_erh)]
    Dim(&'s str),

    #[token("#DIMS", lex_line_left_erh)]
    DimS(&'s str),

    #[error]
    // BOM
    #[token("\u{FEFF}", logos::skip)]
    #[regex(r"[ \t\r\n]+", logos::skip)]
    #[regex(r";[^\n]*", logos::skip)]
    Error,
}

fn csv2<'s>(lex: &mut Lexer<'s, CsvToken<'s>>) -> (&'s str, &'s str) {
    let mut p = lex.slice().split(',');

    (p.next().unwrap().trim(), p.next().unwrap().trim())
}

fn csv3<'s>(lex: &mut Lexer<'s, CsvToken<'s>>) -> (&'s str, &'s str, &'s str) {
    let mut p = lex.slice().split(',');

    (
        p.next().unwrap().trim(),
        p.next().unwrap().trim(),
        p.next().unwrap().trim(),
    )
}

fn csv4<'s>(lex: &mut Lexer<'s, CsvToken<'s>>) -> (&'s str, &'s str, &'s str, &'s str) {
    let mut p = lex.slice().split(',');

    (
        p.next().unwrap().trim(),
        p.next().unwrap().trim(),
        p.next().unwrap().trim(),
        p.next().unwrap().trim(),
    )
}

#[derive(Logos, Debug)]
pub enum CsvToken<'s> {
    #[regex(r"[^,;\u{FEFF}\r\n\t　]+,[^,;\r\n\t　]*,?", csv2)]
    Csv2((&'s str, &'s str)),
    #[regex(r"[^,;\u{FEFF}\r\n\t　]+,[^,;\r\n\t　]*,[^,;\r\n\t　]+,?", csv3)]
    Csv3((&'s str, &'s str, &'s str)),
    #[regex(
        r"[^,;\u{FEFF}\r\n\t　]+,[^,;\r\n\t　]+,[^,;\r\n\t　]+,[^,;\r\n\t　]+,?",
        csv4
    )]
    Csv4((&'s str, &'s str, &'s str, &'s str)),

    #[error]
    // BOM
    #[token("\u{FEFF}", logos::skip)]
    #[regex(r"[ \t\r\n　]+", logos::skip)]
    #[regex(r";[^\n]*", logos::skip)]
    Error,
}

#[derive(Clone, Copy, Debug, Logos)]
pub enum ConfigToken<'s> {
    #[regex(r"[^:\r\n\u{FEFF}][^:\r\n]*:[^\r\n]*", |lex| lex.slice().split_once(':').unwrap())]
    Line((&'s str, &'s str)),

    #[error]
    // BOM
    #[token("\u{FEFF}", logos::skip)]
    #[regex(r"[ \t\r\n　]+", logos::skip)]
    #[regex(r";[^\n]*", logos::skip)]
    Error,
}
