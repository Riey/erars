use logos::{internal::LexerInternal, Lexer, Logos};

use erars_ast::*;

pub fn parse_print_flags(mut s: &str) -> (&str, PrintFlags) {
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

    let (s, f) = parse_print_flags(s);
    flags |= f;

    let s = s.strip_prefix(' ').unwrap_or(s);
    let s = s.strip_suffix('\r').unwrap_or(s);

    (flags, ty, s)
}

#[inline]
fn single_command(com: BuiltinCommand) -> Stmt {
    Stmt::Command(com, Vec::new())
}

#[inline]
fn single_method(meth: BuiltinMethod) -> Stmt {
    Stmt::Method(meth, Vec::new())
}

#[inline]
fn normal_expr_command<'s>(
    lex: &mut Lexer<'s, Token<'s>>,
    com: BuiltinCommand,
) -> (BuiltinCommand, &'s str) {
    (com, lex_line_left(lex))
}

#[inline]
fn normal_expr_method<'s>(
    lex: &mut Lexer<'s, Token<'s>>,
    meth: BuiltinMethod,
) -> (BuiltinMethod, &'s str) {
    (meth, lex_line_left(lex))
}

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

fn lex_line_left<'s>(lex: &mut Lexer<'s, Token<'s>>) -> &'s str {
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

fn call_jump_line<'s>(lex: &mut Lexer<'s, Token<'s>>) -> (CallJumpInfo, &'s str) {
    let mut com = lex.slice();
    let mut info = CallJumpInfo {
        ty: JumpType::Call,
        is_catch: false,
        is_form: false,
        is_try: false,
        is_method: false,
    };

    if let Some(c) = com.strip_prefix("TRY") {
        info.is_try = true;
        com = c;
    }

    if let Some(c) = com.strip_prefix('C') {
        if !c.starts_with('A') {
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

    if let Some(c) = com.strip_prefix("FORM") {
        info.is_form = true;
        com = c;
    }

    if com == "F" {
        info.is_method = true;
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

    #[token("CALLEVENT")]
    CallEvent,

    #[regex(r"PRINT(SINGLE)?(DATA|V|S|FORMS?)?[LW]?(L?C)?[^\n]*", |lex| unsafe { parse_print(lex.slice()) })]
    Print((PrintFlags, PrintType, &'s str)),
    #[token("DATA", lex_line_left)]
    Data(&'s str),
    #[token("DATAFORM", lex_line_left)]
    DataForm(&'s str),
    #[token("DATALIST")]
    DataList,
    #[token("ENDLIST")]
    EndList,
    #[token("ENDDATA")]
    EndData,

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

    #[regex("(TRY)?C?(CALL|JUMP|GOTO)(FORM)?F?", call_jump_line)]
    CallJump((CallJumpInfo, &'s str)),
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
    #[token("THROW", lex_line_left)]
    Throw(&'s str),
    #[token("CUSTOMDRAWLINE", lex_line_left)]
    CustomDrawLine(&'s str),

    #[token("RETURNFORM", |lex| normal_expr_command(lex, BuiltinCommand::Return))]
    #[token("PUTFORM", |lex| normal_expr_command(lex, BuiltinCommand::PutForm))]
    #[token("SETCOLORBYNAME", |lex| normal_expr_command(lex, BuiltinCommand::SetColorByName))]
    #[token("SETBGCOLORBYNAME", |lex| normal_expr_command(lex, BuiltinCommand::SetBgColorByName))]
    StrFormCommand((BuiltinCommand, &'s str)),

    #[token("STRLENFORM", |lex| normal_expr_method(lex, BuiltinMethod::StrLenS))]
    #[token("STRLENFORMU", |lex| normal_expr_method(lex, BuiltinMethod::StrLenSU))]
    StrFormMethod((BuiltinMethod, &'s str)),

    #[token("VARSET", |lex| normal_expr_command(lex, BuiltinCommand::Varset))]
    #[token("CVARSET", |lex| normal_expr_command(lex, BuiltinCommand::CVarset))]
    #[token("RESET_STAIN", |lex| normal_expr_command(lex, BuiltinCommand::ResetStain))]
    #[token("SKIPDISP", |lex| normal_expr_command(lex, BuiltinCommand::SkipDisp))]
    #[token("NOSKIP", |lex| normal_expr_command(lex, BuiltinCommand::NoSkip))]
    #[token("ENDNOSKIP", |lex| normal_expr_command(lex, BuiltinCommand::EndNoSkip))]
    #[token("INPUT", |lex| normal_expr_command(lex, BuiltinCommand::Input))]
    #[token("INPUTS", |lex| normal_expr_command(lex, BuiltinCommand::InputS))]
    #[token("TINPUT", |lex| normal_expr_command(lex, BuiltinCommand::TInput))]
    #[token("TINPUTS", |lex| normal_expr_command(lex, BuiltinCommand::TInputS))]
    #[token("ONEINPUT", |lex| normal_expr_command(lex, BuiltinCommand::OneInput))]
    #[token("ONEINPUTS", |lex| normal_expr_command(lex, BuiltinCommand::OneInputS))]
    #[token("TONEINPUT", |lex| normal_expr_command(lex, BuiltinCommand::TOneInput))]
    #[token("TONEINPUTS", |lex| normal_expr_command(lex, BuiltinCommand::TOneInputS))]
    #[token("RETURN", |lex| normal_expr_command(lex, BuiltinCommand::Return))]
    #[token("RETURNF", |lex| normal_expr_command(lex, BuiltinCommand::ReturnF))]
    #[token("CALLTRAIN", |lex| normal_expr_command(lex, BuiltinCommand::CallTrain))]
    #[token("DOTRAIN", |lex| normal_expr_command(lex, BuiltinCommand::DoTrain))]
    #[token("CLEARLINE", |lex| normal_expr_command(lex, BuiltinCommand::ClearLine))]
    #[token("SETCOLOR", |lex| normal_expr_command(lex, BuiltinCommand::SetColor))]
    #[token("SETBGCOLOR", |lex| normal_expr_command(lex, BuiltinCommand::SetBgColor))]
    #[token("SETBIT", |lex| normal_expr_command(lex, BuiltinCommand::SetBit))]
    #[token("CLEARBIT", |lex| normal_expr_command(lex, BuiltinCommand::ClearBit))]
    #[token("INVERTBIT", |lex| normal_expr_command(lex, BuiltinCommand::InvertBit))]
    #[token("BAR", |lex| normal_expr_command(lex, BuiltinCommand::Bar))]
    #[token("ARRAYSHIFT", |lex| normal_expr_command(lex, BuiltinCommand::ArrayShift))]
    #[token("SWAP", |lex| normal_expr_command(lex, BuiltinCommand::Swap))]
    #[token("REDRAW", |lex| normal_expr_command(lex, BuiltinCommand::Redraw))]
    #[token("CHKFONT", |lex| normal_expr_command(lex, BuiltinCommand::ChkFont))]
    #[token("SETFONT", |lex| normal_expr_command(lex, BuiltinCommand::SetFont))]
    #[token("FONTSTYLE", |lex| normal_expr_command(lex, BuiltinCommand::FontStyle))]
    #[token("SAVEDATA", |lex| normal_expr_command(lex, BuiltinCommand::SaveData))]
    #[token("LOADDATA", |lex| normal_expr_command(lex, BuiltinCommand::LoadData))]
    #[token("DELDATA", |lex| normal_expr_command(lex, BuiltinCommand::DelData))]
    #[token("SAVENOS", |lex| normal_expr_command(lex, BuiltinCommand::SaveNos))]
    #[token("SAVECHARA", |lex| normal_expr_command(lex, BuiltinCommand::SaveChara))]
    #[token("LOADCHARA", |lex| normal_expr_command(lex, BuiltinCommand::LoadChara))]
    #[token("ADDCHARA", |lex| normal_expr_command(lex, BuiltinCommand::AddChara))]
    #[token("DELCHARA", |lex| normal_expr_command(lex, BuiltinCommand::DelChara))]
    #[token("SWAPCHARA", |lex| normal_expr_command(lex, BuiltinCommand::SwapChara))]
    #[token("SORTCHARA", |lex| normal_expr_command(lex, BuiltinCommand::SortChara))]
    #[token("PICKUPCHARA", |lex| normal_expr_command(lex, BuiltinCommand::PickupChara))]
    #[token("SPLIT", |lex| normal_expr_command(lex, BuiltinCommand::Split))]
    #[token("CUPCHECK", |lex| normal_expr_command(lex, BuiltinCommand::CUpCheck))]
    NormalExprCommand((BuiltinCommand, &'s str)),

    #[token("ISSKIP", |lex| normal_expr_method(lex, BuiltinMethod::IsSkip))]
    #[token("LIMIT", |lex| normal_expr_method(lex, BuiltinMethod::Limit))]
    #[token("ESCAPE", |lex| normal_expr_method(lex, BuiltinMethod::Escape))]
    #[token("REPLACE", |lex| normal_expr_method(lex, BuiltinMethod::Replace))]
    #[token("STRLENS", |lex| normal_expr_method(lex, BuiltinMethod::StrLenS))]
    #[token("STRLENSU", |lex| normal_expr_method(lex, BuiltinMethod::StrLenSU))]
    #[token("STRFIND", |lex| normal_expr_method(lex, BuiltinMethod::StrFind))]
    #[token("STRFINDU", |lex| normal_expr_method(lex, BuiltinMethod::StrFindU))]
    #[token("BARSTR", |lex| normal_expr_method(lex, BuiltinMethod::BarStr))]
    #[token("GETBIT", |lex| normal_expr_method(lex, BuiltinMethod::GetBit))]
    #[token("ABS", |lex| normal_expr_method(lex, BuiltinMethod::Abs))]
    #[token("SIGN", |lex| normal_expr_method(lex, BuiltinMethod::Sign))]
    #[token("INRANGE", |lex| normal_expr_method(lex, BuiltinMethod::InRange))]
    #[token("POWER", |lex| normal_expr_method(lex, BuiltinMethod::Power))]
    #[token("GETEXPLV", |lex| normal_expr_method(lex, BuiltinMethod::GetExpLv))]
    #[token("GETPALAMLV", |lex| normal_expr_method(lex, BuiltinMethod::GetPalamLv))]
    #[token("UNICODE", |lex| normal_expr_method(lex, BuiltinMethod::Unicode))]
    #[token("STRCOUNT", |lex| normal_expr_method(lex, BuiltinMethod::StrCount))]
    #[token("SUBSTRING", |lex| normal_expr_method(lex, BuiltinMethod::SubString))]
    #[token("SUBSTRINGU", |lex| normal_expr_method(lex, BuiltinMethod::SubStringU))]
    #[token("GETCHARA", |lex| normal_expr_method(lex, BuiltinMethod::GetChara))]
    #[token("CHKDATA", |lex| normal_expr_method(lex, BuiltinMethod::ChkData))]
    #[token("CHKCHARADATA", |lex| normal_expr_method(lex, BuiltinMethod::ChkCharaData))]
    #[token("FINDCHARA", |lex| normal_expr_method(lex, BuiltinMethod::FindChara))]
    #[token("FIND_CHARADATA", |lex| normal_expr_method(lex, BuiltinMethod::FindCharaData))]
    #[token("CSVNAME", |lex| normal_expr_method(lex, BuiltinMethod::CsvName))]
    #[token("CSVCALLNAME", |lex| normal_expr_method(lex, BuiltinMethod::CsvCallName))]
    #[token("CSVNICKNAME", |lex| normal_expr_method(lex, BuiltinMethod::CsvNickName))]
    #[token("CSVMASTERNAME", |lex| normal_expr_method(lex, BuiltinMethod::CsvMasterName))]
    #[token("CSVBASE", |lex| normal_expr_method(lex, BuiltinMethod::CsvBase))]
    #[token("CSVCSTR", |lex| normal_expr_method(lex, BuiltinMethod::CsvCstr))]
    #[token("CSVABL", |lex| normal_expr_method(lex, BuiltinMethod::CsvAbl))]
    #[token("CSVTALENT", |lex| normal_expr_method(lex, BuiltinMethod::CsvTalent))]
    #[token("CSVMARK", |lex| normal_expr_method(lex, BuiltinMethod::CsvMark))]
    #[token("CSVEX", |lex| normal_expr_method(lex, BuiltinMethod::CsvEx))]
    #[token("CSVEXP", |lex| normal_expr_method(lex, BuiltinMethod::CsvExp))]
    #[token("CSVRELATION", |lex| normal_expr_method(lex, BuiltinMethod::CsvRelation))]
    #[token("CSVJUEL", |lex| normal_expr_method(lex, BuiltinMethod::CsvJuel))]
    #[token("CSVEQUIP", |lex| normal_expr_method(lex, BuiltinMethod::CsvEquip))]
    #[token("CSVCFLAG", |lex| normal_expr_method(lex, BuiltinMethod::CsvCflag))]
    NormalExprMethod((BuiltinMethod, &'s str)),

    #[regex(r"REUSELASTLINE( [^\r\n]*)?", |lex| unsafe { parse_reuse(lex.slice()) })]
    #[token("GETTIME", |_| single_method(BuiltinMethod::GetTime))]
    #[token("GETFONT", |_| single_method(BuiltinMethod::GetFont))]
    #[token("GETCOLOR", |_| single_method(BuiltinMethod::GetColor))]
    #[token("GETDEFCOLOR", |_| single_method(BuiltinMethod::GetDefColor))]
    #[token("GETBGCOLOR", |_| single_method(BuiltinMethod::GetBgColor))]
    #[token("GETDEFBGCOLOR", |_| single_method(BuiltinMethod::GetDefBgColor))]
    #[token("GETFOCUSCOLOR", |_| single_method(BuiltinMethod::GetFocusColor))]
    #[token("QUIT", |_| single_command(BuiltinCommand::Quit))]
    #[token("WAIT", |_| single_command(BuiltinCommand::Wait))]
    #[token("RESTART", |_| single_command(BuiltinCommand::Restart))]
    #[token("SAVEGAME", |_| single_command(BuiltinCommand::SaveGame))]
    #[token("LOADGAME", |_| single_command(BuiltinCommand::LoadGame))]
    #[token("SAVEGLOBAL", |_| single_command(BuiltinCommand::SaveGlobal))]
    #[token("LOADGLOBAL", |_| single_command(BuiltinCommand::LoadGlobal))]
    #[token("DRAWLINE", |_| single_command(BuiltinCommand::DrawLine))]
    #[token("RESETDATA", |_| single_command(BuiltinCommand::ResetData))]
    #[token("RESETCOLOR", |_| single_command(BuiltinCommand::ResetColor))]
    #[token("RESETBGCOLOR", |_| single_command(BuiltinCommand::ResetBgColor))]
    #[token("ADDDEFCHARA", |_| single_command(BuiltinCommand::AddDefChara))]
    #[token("FONTBOLD", |_| single_command(BuiltinCommand::FontBold))]
    #[token("FONTITALIC", |_| single_command(BuiltinCommand::FontItalic))]
    #[token("FONTREGULAR", |_| single_command(BuiltinCommand::FontRegular))]
    #[token("UPCHECK", |_| single_command(BuiltinCommand::UpCheck))]
    #[token("CONTINUE", |_| Stmt::Continue)]
    #[token("BREAK", |_| Stmt::Break)]
    DirectStmt(Stmt),

    #[regex(r"\[[^\]]+\]")]
    Preprocess(&'s str),

    #[token("\n")]
    Newline,

    #[error]
    // BOM
    #[token("\u{FEFF}", logos::skip)]
    #[regex(r"[ \t\r　]+", logos::skip)]
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
