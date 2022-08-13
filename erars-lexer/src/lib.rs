use logos::{internal::LexerInternal, Lexer, Logos};

use erars_ast::*;

pub fn parse_print_flags(mut s: &str) -> (&str, PrintFlags) {
    let mut flags = PrintFlags::empty();

    if let Some(ss) = s.strip_prefix("SINGLE") {
        s = ss;
        flags |= PrintFlags::SINGLE;
    }

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

unsafe fn parse_print(s: &str) -> (PrintFlags, PrintType, &str) {
    // skip PRINT
    let mut s = s.get_unchecked("PRINT".len()..);
    let mut flags = PrintFlags::empty();

    if let Some(ss) = s.strip_prefix("SINGLE") {
        flags |= PrintFlags::SINGLE;
        s = ss;
    }

    let ty = if let Some(ss) = s.strip_prefix("FORMS") {
        s = ss;
        todo!("PRINTFORMS")
    } else if let Some(ss) = s.strip_prefix("FORM") {
        s = ss;
        PrintType::Form
    } else if let Some(ss) = s.strip_prefix('V') {
        s = ss;
        PrintType::V
    } else if let Some(ss) = s.strip_prefix('S') {
        s = ss;
        PrintType::S
    } else {
        PrintType::Plain
    };

    let (s, f) = parse_print_flags(s);
    flags |= f;

    (flags, ty, s.strip_prefix(' ').unwrap_or_default())
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

fn lex_line_left_erh<'s>(lex: &mut Lexer<'s, ErhToken<'s>>) -> &'s str {
    let args = lex.remainder();
    let s = match args.split_once('\n') {
        Some((args, _)) => {
            lex.bump_unchecked(args.len() + 1);
            args
        }
        None => {
            lex.bump_unchecked(args.len());
            args
        }
    };

    s.strip_prefix(' ').unwrap_or(s)
}

fn lex_line_left<'s>(lex: &mut Lexer<'s, Token<'s>>) -> &'s str {
    let args = lex.remainder();
    let s = match args.split_once('\n') {
        Some((args, _)) => {
            lex.bump_unchecked(args.len() + 1);
            args
        }
        None => {
            lex.bump_unchecked(args.len());
            args
        }
    };

    s.strip_prefix(' ').unwrap_or(s)
}

fn call_jump_line<'s>(lex: &mut Lexer<'s, Token<'s>>) -> (CallJumpInfo, &'s str) {
    let mut com = lex.slice();
    let mut info = CallJumpInfo {
        ty: JumpType::Call,
        is_catch: false,
        is_form: false,
        is_try: false,
    };

    if let Some(c) = com.strip_prefix("TRY") {
        info.is_try = true;
        com = c;
    }

    if let Some(c) = com.strip_prefix("C") {
        if !c.starts_with("A") {
            info.is_catch = true;
            com = c;
        }
    }

    if let Some(c) = com.strip_prefix("CALL") {
        com = c;
    } else if let Some(c) = com.strip_prefix("JUMP") {
        info.ty = JumpType::Jump;
        com = c;
    } else if let Some(c) = com.strip_prefix("GOTO") {
        info.ty = JumpType::Goto;
        com = c;
    } else {
        unreachable!()
    }

    if com == "FORM" {
        info.is_form = true;
        com = "";
    }

    if !com.is_empty() {
        unreachable!()
    }

    let args = lex_line_left(lex);

    (info, args)
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
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrintType {
    Plain,
    Form,
    S,
    V,
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

#[derive(Logos, Debug, Eq, PartialEq)]
pub enum Token<'s> {
    #[token("@", lex_line_left)]
    At(&'s str),
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
    #[token("++")]
    Inc,
    #[token("--")]
    Dec,
    #[token("#FUNCTION")]
    Function,
    #[token("#FUNCTIONS")]
    FunctionS,
    #[token("#LOCALSIZE", lex_line_left)]
    LocalSize(&'s str),
    #[token("#LOCALSSIZE", lex_line_left)]
    LocalSSize(&'s str),
    #[token("#DIM", lex_line_left)]
    Dim(&'s str),
    #[token("#DIMS", lex_line_left)]
    DimS(&'s str),
    #[token("#PRI")]
    Pri,
    #[token("#LATER")]
    Later,
    #[token("#SINGLE")]
    Single,

    #[regex(r"\p{XID_Start}\p{XID_Continue}*")]
    Ident(&'s str),

    #[regex(r"PRINT(SINGLE)?(V|S|FORMS?)?[LW]?(L?C)?[^\n]*", |lex| unsafe { parse_print(lex.slice()) })]
    Print((PrintFlags, PrintType, &'s str)),

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

    #[token("DO")]
    Do,
    #[token("LOOP", lex_line_left)]
    Loop(&'s str),

    #[token("REPEAT", lex_line_left)]
    Repeat(&'s str),
    #[token("REND")]
    Rend,

    #[token("WHILE", lex_line_left)]
    While(&'s str),
    #[token("WEND")]
    Wend,

    #[token("SELECTCASE", lex_line_left)]
    SelectCase(&'s str),
    #[token("CASE", lex_line_left)]
    Case(&'s str),
    #[token("CASEELSE")]
    CaseElse,
    #[token("ENDSELECT")]
    EndSelect,

    #[regex("(TRY)?C?(CALL|JUMP|GOTO)(FORM)?", call_jump_line)]
    CallJump((CallJumpInfo, &'s str)),
    #[token("CALLF", lex_line_left)]
    CallF(&'s str),
    #[token("CATCH")]
    Catch,
    #[token("ENDCATCH")]
    EndCatch,
    #[token("ALIGNMENT")]
    Alignment,
    #[token("BEGIN")]
    Begin,
    #[token("TIMES", lex_line_left)]
    Times(&'s str),

    #[token("VARSET", |lex| normal_expr_command(lex, BuiltinCommand::Varset))]
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
    #[token("GETCHARA", |lex| normal_expr_command(lex, BuiltinCommand::GetChara))]
    #[token("DELCHARA", |lex| normal_expr_command(lex, BuiltinCommand::DelChara))]
    #[token("SWAPCHARA", |lex| normal_expr_command(lex, BuiltinCommand::SwapChara))]
    #[token("FINDCHARA", |lex| normal_expr_command(lex, BuiltinCommand::FindChara))]
    #[token("SETCOLOR", |lex| normal_expr_command(lex, BuiltinCommand::SetColor))]
    #[token("SETBIT", |lex| normal_expr_command(lex, BuiltinCommand::SetBit))]
    #[token("GETBIT", |lex| normal_expr_command(lex, BuiltinCommand::GetBit))]
    #[token("CLEARBIT", |lex| normal_expr_command(lex, BuiltinCommand::ClearBit))]
    #[token("POWER", |lex| normal_expr_command(lex, BuiltinCommand::Power))]
    #[token("GETEXPLV", |lex| normal_expr_command(lex, BuiltinCommand::GetExpLv))]
    #[token("GETPALAMLV", |lex| normal_expr_command(lex, BuiltinCommand::GetPalamLv))]
    #[token("CHKDATA", |lex| normal_expr_command(lex, BuiltinCommand::ChkData))]
    #[token("UNICODE", |lex| normal_expr_command(lex, BuiltinCommand::Unicode))]
    #[token("BAR", |lex| normal_expr_command(lex, BuiltinCommand::Bar))]
    NormalExprCommand((BuiltinCommand, &'s str)),

    #[regex(r"REUSELASTLINE [^\r\n]*", |lex| unsafe { parse_reuse(lex.slice()) })]
    #[token("QUIT", |_| single_command(BuiltinCommand::Quit))]
    #[token("WAIT", |_| single_command(BuiltinCommand::Wait))]
    #[token("RESTART", |_| single_command(BuiltinCommand::Restart))]
    #[token("SAVEGLOBAL", |_| single_command(BuiltinCommand::SaveGlobal))]
    #[token("LOADGLOBAL", |_| single_command(BuiltinCommand::LoadGlobal))]
    #[token("DRAWLINE", |_| single_command(BuiltinCommand::DrawLine))]
    #[token("RESETDATA", |_| single_command(BuiltinCommand::ResetData))]
    #[token("RESETCOLOR", |_| single_command(BuiltinCommand::ResetColor))]
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
