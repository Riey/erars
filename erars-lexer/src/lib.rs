use logos::{internal::LexerInternal, Lexer, Logos};

use erars_ast::*;

pub fn parse_print_flags(mut s: &str) -> (&str, PrintFlags) {
    let mut flags = PrintFlags::empty();

    if let Some(ss) = s.strip_prefix('L') {
        if let Some(ss) = s.strip_prefix('C') {
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

    (s, flags)
}

unsafe fn parse_reuse(s: &str) -> Stmt {
    let s = s.get_unchecked("REUSELASTLINE".len()..);

    Stmt::ReuseLastLine(s.strip_prefix(' ').unwrap_or(s).into())
}

unsafe fn parse_print(s: &str) -> Stmt {
    // skip PRINT
    let s = s.get_unchecked("PRINT".len()..);

    let (s, flags) = parse_print_flags(s);

    match s.strip_prefix(' ') {
        Some(s) => Stmt::Print(flags, Expr::String(s.into())),
        None => Stmt::Print(flags, Expr::String(String::new())),
    }
}

unsafe fn parse_print_form(s: &str) -> (PrintFlags, &str) {
    // skip PRINTFORM
    let s = s.get_unchecked("PRINTFORM".len()..);

    let (s, flags) = parse_print_flags(s);

    (flags, s.strip_prefix(' ').unwrap_or_default())
}

#[inline]
fn single_command(com: BuiltinCommand) -> Stmt {
    Stmt::Command(com, Vec::new())
}

#[inline]
fn normal_expr_command<'s>(
    lex: &mut Lexer<'s, Token<'s>>,
    com: BuiltinCommand,
) -> (BuiltinCommand, &'s str) {
    (com, lex_line_left(lex))
}

fn lex_line_left<'s>(lex: &mut Lexer<'s, Token<'s>>) -> &'s str {
    let args = lex.remainder();
    match args.split_once('\n') {
        Some((args, _)) => {
            lex.bump_unchecked(args.len() + 1);
            args
        }
        None => {
            lex.bump_unchecked(args.len());
            args
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignOp(pub Option<BinaryOperator>);

#[derive(Logos, Debug, Eq, PartialEq)]
pub enum Token<'s> {
    #[token("@")]
    At,
    #[token("$", lex_line_left)]
    LabelLine(&'s str),

    #[token(",")]
    Comma,
    #[token("(")]
    OpenParan,
    #[token(")")]
    CloseParan,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token(":")]
    Colon,
    #[token("?")]
    Question,
    #[token("#FUNCTION")]
    Function,
    #[token("#FUNCTIONS")]
    FunctionS,
    #[token("#DIM")]
    Dim,
    #[token("#DIMS")]
    DimS,
    #[token("#PRI")]
    Pri,
    #[token("#LATER")]
    Later,
    #[token("#SINGLE")]
    Single,

    #[regex(r"\p{XID_Start}\p{XID_Continue}*")]
    Ident(&'s str),
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse())]
    Number(i64),

    #[regex(r"PRINTFORM[LW]?(L?C)?[^\n]*", |lex| unsafe { parse_print_form(lex.slice()) })]
    PrintForm((PrintFlags, &'s str)),

    #[token("SIF", lex_line_left)]
    Sif(&'s str),

    #[token("IF", lex_line_left)]
    If(&'s str),
    #[token("ELSEIF", lex_line_left)]
    ElseIf(&'s str),
    #[token("ELSE")]
    Else,
    #[token("ENDIF")]
    EndIf,

    #[token("FOR", lex_line_left)]
    For(&'s str),
    #[token("NEXT")]
    Next,

    #[token("REPEAT", lex_line_left)]
    Repeat(&'s str),
    #[token("REND")]
    Rend,

    #[token("SELECTCASE", lex_line_left)]
    SelectCase(&'s str),
    #[token("CASE", lex_line_left)]
    Case(&'s str),
    #[token("CASEELSE")]
    CaseElse,

    #[token("CALL")]
    #[token("CALLF")]
    Call,
    #[token("CALLFORM")]
    CallForm,
    #[token("GOTO")]
    Goto,
    #[token("ALIGNMENT")]
    Alignment,
    #[token("BEGIN")]
    Begin,
    #[token("VARSET", lex_line_left)]
    Varset(&'s str),
    #[token("TIMES", lex_line_left)]
    Times(&'s str),

    #[token("LIMIT", |lex| normal_expr_command(lex, BuiltinCommand::Limit))]
    #[token("INPUT", |lex| normal_expr_command(lex, BuiltinCommand::Input))]
    #[token("INPUTS", |lex| normal_expr_command(lex, BuiltinCommand::InputS))]
    #[token("TINPUT", |lex| normal_expr_command(lex, BuiltinCommand::TInput))]
    #[token("TINPUTS", |lex| normal_expr_command(lex, BuiltinCommand::TInputS))]
    #[token("RETURN", |lex| normal_expr_command(lex, BuiltinCommand::Return))]
    #[token("RETURNF", |lex| normal_expr_command(lex, BuiltinCommand::Return))]
    #[token("STRLENS", |lex| normal_expr_command(lex, BuiltinCommand::StrLenS))]
    #[token("STRLENSU", |lex| normal_expr_command(lex, BuiltinCommand::StrLenSU))]
    #[token("CUSTOMDRAWLINE", |lex| normal_expr_command(lex, BuiltinCommand::CustomDrawLine))]
    #[token("CLEARLINE", |lex| normal_expr_command(lex, BuiltinCommand::ClearLine))]
    #[token("ADDCHARA", |lex| normal_expr_command(lex, BuiltinCommand::AddChara))]
    #[token("DELCHARA", |lex| normal_expr_command(lex, BuiltinCommand::DelChara))]
    #[token("SWAPCHARA", |lex| normal_expr_command(lex, BuiltinCommand::SwapChara))]
    #[token("FINDCHARA", |lex| normal_expr_command(lex, BuiltinCommand::FindChara))]
    NormalExprCommand((BuiltinCommand, &'s str)),

    #[regex(r"PRINT[LW]?(L?C)? [^\r\n]*", |lex| unsafe { parse_print(lex.slice()) })]
    #[regex(r"REUSELASTLINE [^\r\n]*", |lex| unsafe { parse_reuse(lex.slice()) })]
    #[token("QUIT", |_| single_command(BuiltinCommand::Quit))]
    #[token("WAIT", |_| single_command(BuiltinCommand::Wait))]
    #[token("RESTART", |_| single_command(BuiltinCommand::Restart))]
    #[token("SAVEGLOBAL", |_| single_command(BuiltinCommand::SaveGlobal))]
    #[token("LOADGLOBAL", |_| single_command(BuiltinCommand::LoadGlobal))]
    #[token("DRAWLINE", |_| single_command(BuiltinCommand::DrawLine))]
    #[token("RESETDATA", |_| single_command(BuiltinCommand::ResetData))]
    #[token("ADDDEFCHARA", |_| single_command(BuiltinCommand::AddDefChara))]
    #[token("FONTBOLD", |_| single_command(BuiltinCommand::FontBold))]
    #[token("FONTITALIC", |_| single_command(BuiltinCommand::FontItalic))]
    #[token("FONTREGULAR", |_| single_command(BuiltinCommand::FontRegular))]
    #[token("CONTINUE", |_| Stmt::Continue)]
    #[token("BREAK", |_| Stmt::Break)]
    DirectStmt(Stmt),

    #[error]
    // BOM
    #[token("\u{FEFF}", logos::skip)]
    #[regex(r"[ \t\r\n]+", logos::skip)]
    #[regex(r";[^\n]*", logos::skip)]
    Error,
}
