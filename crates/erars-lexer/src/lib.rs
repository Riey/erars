use logos::{internal::LexerInternal, Lexer, Logos};

use erars_ast::*;

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

unsafe fn parse_print(s: &str) -> (PrintFlags, PrintType, &str) {
    // skip PRINT
    let mut s = s.get_unchecked("PRINT".len()..);
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

    if let Some(c) = strip_prefix_ignore_case(com, "TRY") {
        info.is_try = true;
        com = c;
    }

    if let Some(c) = strip_prefix_ignore_case_char(com, 'C') {
        if !c.starts_with('A') {
            info.is_catch = true;
            com = c;
        }
    }

    if let Some(c) = strip_prefix_ignore_case(com, "CALL") {
        com = c;
    } else if let Some(c) = strip_prefix_ignore_case(com, "JUMP") {
        info.ty = JumpType::Jump;
        com = c;
    } else if let Some(c) = strip_prefix_ignore_case(com, "GOTO") {
        info.ty = JumpType::Goto;
        com = c;
    } else {
        unreachable!()
    }

    if let Some(c) = strip_prefix_ignore_case(com, "FORM") {
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
    #[token("#DEFINE", lex_line_left_erh, ignore(ascii_case))]
    Define(&'s str),

    #[token("#DIM", lex_line_left_erh, ignore(ascii_case))]
    Dim(&'s str),

    #[token("#DIMS", lex_line_left_erh, ignore(ascii_case))]
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
    #[token("#FUNCTION", ignore(ascii_case))]
    Function,
    #[token("#FUNCTIONS", ignore(ascii_case))]
    FunctionS,
    #[token("#LOCALSIZE", lex_line_left, ignore(ascii_case))]
    LocalSize(&'s str),
    #[token("#LOCALSSIZE", lex_line_left, ignore(ascii_case))]
    LocalSSize(&'s str),
    #[token("#DIM", lex_line_left, ignore(ascii_case))]
    Dim(&'s str),
    #[token("#DIMS", lex_line_left, ignore(ascii_case))]
    DimS(&'s str),
    #[token("#PRI", ignore(ascii_case))]
    Pri,
    #[token("#LATER", ignore(ascii_case))]
    Later,
    #[token("#SINGLE", ignore(ascii_case))]
    Single,

    #[regex(r"\p{XID_Start}\p{XID_Continue}*")]
    Ident(&'s str),

    #[token("CALLEVENT", ignore(ascii_case))]
    CallEvent,

    #[regex(r"PRINT(SINGLE)?(DATA|V|S|FORMS?)?[LW]?(L?C)?[^\n]*", |lex| unsafe { parse_print(lex.slice()) }, ignore(ascii_case))]
    // #[regex(r"(?i)[pP][rR][iI][nN][tT]([sS][iI][nN][gG][lL][eE])?([dD][aA][tT][aA]|[vV]|[sS]|[fF][oO][rR][mM][sS]?)?[lLwW]?([lL]?[cC])?[^\n]*", |lex| unsafe { parse_print(lex.slice()) })]
    Print((PrintFlags, PrintType, &'s str)),
    #[token("DATA", lex_line_left, ignore(ascii_case))]
    Data(&'s str),
    #[token("DATAFORM", lex_line_left, ignore(ascii_case))]
    DataForm(&'s str),
    #[token("DATALIST", ignore(ascii_case))]
    DataList,
    #[token("ENDLIST", ignore(ascii_case))]
    EndList,
    #[token("ENDDATA", ignore(ascii_case))]
    EndData,

    #[token("SIF", lex_line_left, ignore(ascii_case))]
    Sif(&'s str),

    #[token("IF", lex_line_left, ignore(ascii_case))]
    If(&'s str),
    #[token("ELSEIF", lex_line_left, ignore(ascii_case))]
    ElseIf(&'s str),
    #[token("ELSE", ignore(ascii_case))]
    Else,
    #[token("ENDIF", ignore(ascii_case))]
    EndIf,

    #[token("FOR", lex_line_left, ignore(ascii_case))]
    For(&'s str),
    #[token("NEXT", ignore(ascii_case))]
    Next,

    #[token("DO", ignore(ascii_case))]
    Do,
    #[token("LOOP", lex_line_left, ignore(ascii_case))]
    Loop(&'s str),

    #[token("REPEAT", lex_line_left, ignore(ascii_case))]
    Repeat(&'s str),
    #[token("REND", ignore(ascii_case))]
    Rend,

    #[token("WHILE", lex_line_left, ignore(ascii_case))]
    While(&'s str),
    #[token("WEND", ignore(ascii_case))]
    Wend,

    #[token("SELECTCASE", lex_line_left, ignore(ascii_case))]
    SelectCase(&'s str),
    #[token("CASE", lex_line_left, ignore(ascii_case))]
    Case(&'s str),
    #[token("CASEELSE", ignore(ascii_case))]
    CaseElse,
    #[token("ENDSELECT", ignore(ascii_case))]
    EndSelect,

    #[regex(
        "(TRY)?C?(CALL|JUMP|GOTO)(FORM)?F?",
        call_jump_line,
        ignore(ascii_case)
    )]
    CallJump((CallJumpInfo, &'s str)),
    #[token("CATCH", ignore(ascii_case))]
    Catch,
    #[token("ENDCATCH", ignore(ascii_case))]
    EndCatch,
    #[token("ALIGNMENT", ignore(ascii_case))]
    Alignment,
    #[token("BEGIN", ignore(ascii_case))]
    Begin,
    #[token("TIMES", lex_line_left, ignore(ascii_case))]
    Times(&'s str),
    #[token("THROW", lex_line_left, ignore(ascii_case))]
    Throw(&'s str),
    #[token("CUSTOMDRAWLINE", lex_line_left, ignore(ascii_case))]
    CustomDrawLine(&'s str),
    #[regex("REUSELASTLINE", lex_line_left, ignore(ascii_case))]
    ReuseLastLine(&'s str),

    #[token("RETURNFORM", |lex| normal_expr_command(lex, BuiltinCommand::Return), ignore(ascii_case))]
    #[token("PUTFORM", |lex| normal_expr_command(lex, BuiltinCommand::PutForm), ignore(ascii_case))]
    #[token("SETCOLORBYNAME", |lex| normal_expr_command(lex, BuiltinCommand::SetColorByName), ignore(ascii_case))]
    #[token("SETBGCOLORBYNAME", |lex| normal_expr_command(lex, BuiltinCommand::SetBgColorByName), ignore(ascii_case))]
    StrFormCommand((BuiltinCommand, &'s str)),

    #[token("STRLENFORM", |lex| normal_expr_method(lex, BuiltinMethod::StrLenS), ignore(ascii_case))]
    #[token("STRLENFORMU", |lex| normal_expr_method(lex, BuiltinMethod::StrLenSU), ignore(ascii_case))]
    StrFormMethod((BuiltinMethod, &'s str)),

    #[token("VARSET", |lex| normal_expr_command(lex, BuiltinCommand::Varset), ignore(ascii_case))]
    #[token("CVARSET", |lex| normal_expr_command(lex, BuiltinCommand::CVarset), ignore(ascii_case))]
    #[token("RESET_STAIN", |lex| normal_expr_command(lex, BuiltinCommand::ResetStain), ignore(ascii_case))]
    #[token("SKIPDISP", |lex| normal_expr_command(lex, BuiltinCommand::SkipDisp), ignore(ascii_case))]
    #[token("NOSKIP", |lex| normal_expr_command(lex, BuiltinCommand::NoSkip), ignore(ascii_case))]
    #[token("ENDNOSKIP", |lex| normal_expr_command(lex, BuiltinCommand::EndNoSkip), ignore(ascii_case))]
    #[token("INPUT", |lex| normal_expr_command(lex, BuiltinCommand::Input), ignore(ascii_case))]
    #[token("INPUTS", |lex| normal_expr_command(lex, BuiltinCommand::InputS), ignore(ascii_case))]
    #[token("TINPUT", |lex| normal_expr_command(lex, BuiltinCommand::TInput), ignore(ascii_case))]
    #[token("TINPUTS", |lex| normal_expr_command(lex, BuiltinCommand::TInputS), ignore(ascii_case))]
    #[token("ONEINPUT", |lex| normal_expr_command(lex, BuiltinCommand::OneInput), ignore(ascii_case))]
    #[token("ONEINPUTS", |lex| normal_expr_command(lex, BuiltinCommand::OneInputS), ignore(ascii_case))]
    #[token("TONEINPUT", |lex| normal_expr_command(lex, BuiltinCommand::TOneInput), ignore(ascii_case))]
    #[token("TONEINPUTS", |lex| normal_expr_command(lex, BuiltinCommand::TOneInputS), ignore(ascii_case))]
    #[token("TWAIT", |lex| normal_expr_command(lex, BuiltinCommand::Twait), ignore(ascii_case))]
    #[token("RETURN", |lex| normal_expr_command(lex, BuiltinCommand::Return), ignore(ascii_case))]
    #[token("RETURNF", |lex| normal_expr_command(lex, BuiltinCommand::ReturnF), ignore(ascii_case))]
    #[token("CALLTRAIN", |lex| normal_expr_command(lex, BuiltinCommand::CallTrain), ignore(ascii_case))]
    #[token("DOTRAIN", |lex| normal_expr_command(lex, BuiltinCommand::DoTrain), ignore(ascii_case))]
    #[token("CLEARLINE", |lex| normal_expr_command(lex, BuiltinCommand::ClearLine), ignore(ascii_case))]
    #[token("SETCOLOR", |lex| normal_expr_command(lex, BuiltinCommand::SetColor), ignore(ascii_case))]
    #[token("SETBGCOLOR", |lex| normal_expr_command(lex, BuiltinCommand::SetBgColor), ignore(ascii_case))]
    #[token("SETBIT", |lex| normal_expr_command(lex, BuiltinCommand::SetBit), ignore(ascii_case))]
    #[token("CLEARBIT", |lex| normal_expr_command(lex, BuiltinCommand::ClearBit), ignore(ascii_case))]
    #[token("INVERTBIT", |lex| normal_expr_command(lex, BuiltinCommand::InvertBit), ignore(ascii_case))]
    #[token("BAR", |lex| normal_expr_command(lex, BuiltinCommand::Bar), ignore(ascii_case))]
    #[token("ARRAYSHIFT", |lex| normal_expr_command(lex, BuiltinCommand::ArrayShift), ignore(ascii_case))]
    #[token("SWAP", |lex| normal_expr_command(lex, BuiltinCommand::Swap), ignore(ascii_case))]
    #[token("REDRAW", |lex| normal_expr_command(lex, BuiltinCommand::Redraw), ignore(ascii_case))]
    #[token("CHKFONT", |lex| normal_expr_command(lex, BuiltinCommand::ChkFont), ignore(ascii_case))]
    #[token("SETFONT", |lex| normal_expr_command(lex, BuiltinCommand::SetFont), ignore(ascii_case))]
    #[token("FONTSTYLE", |lex| normal_expr_command(lex, BuiltinCommand::FontStyle), ignore(ascii_case))]
    #[token("SAVEDATA", |lex| normal_expr_command(lex, BuiltinCommand::SaveData), ignore(ascii_case))]
    #[token("LOADDATA", |lex| normal_expr_command(lex, BuiltinCommand::LoadData), ignore(ascii_case))]
    #[token("DELDATA", |lex| normal_expr_command(lex, BuiltinCommand::DelData), ignore(ascii_case))]
    #[token("SAVENOS", |lex| normal_expr_command(lex, BuiltinCommand::SaveNos), ignore(ascii_case))]
    #[token("SAVECHARA", |lex| normal_expr_command(lex, BuiltinCommand::SaveChara), ignore(ascii_case))]
    #[token("LOADCHARA", |lex| normal_expr_command(lex, BuiltinCommand::LoadChara), ignore(ascii_case))]
    #[token("ADDCHARA", |lex| normal_expr_command(lex, BuiltinCommand::AddChara), ignore(ascii_case))]
    #[token("DELCHARA", |lex| normal_expr_command(lex, BuiltinCommand::DelChara), ignore(ascii_case))]
    #[token("SWAPCHARA", |lex| normal_expr_command(lex, BuiltinCommand::SwapChara), ignore(ascii_case))]
    #[token("SORTCHARA", |lex| normal_expr_command(lex, BuiltinCommand::SortChara), ignore(ascii_case))]
    #[token("PICKUPCHARA", |lex| normal_expr_command(lex, BuiltinCommand::PickupChara), ignore(ascii_case))]
    #[token("SPLIT", |lex| normal_expr_command(lex, BuiltinCommand::Split), ignore(ascii_case))]
    #[token("CUPCHECK", |lex| normal_expr_command(lex, BuiltinCommand::CUpCheck), ignore(ascii_case))]
    NormalExprCommand((BuiltinCommand, &'s str)),

    #[token("ISSKIP", |lex| normal_expr_method(lex, BuiltinMethod::IsSkip), ignore(ascii_case))]
    #[token("LIMIT", |lex| normal_expr_method(lex, BuiltinMethod::Limit), ignore(ascii_case))]
    #[token("ESCAPE", |lex| normal_expr_method(lex, BuiltinMethod::Escape), ignore(ascii_case))]
    #[token("REPLACE", |lex| normal_expr_method(lex, BuiltinMethod::Replace), ignore(ascii_case))]
    #[token("STRLENS", |lex| normal_expr_method(lex, BuiltinMethod::StrLenS), ignore(ascii_case))]
    #[token("STRLENSU", |lex| normal_expr_method(lex, BuiltinMethod::StrLenSU), ignore(ascii_case))]
    #[token("STRFIND", |lex| normal_expr_method(lex, BuiltinMethod::StrFind), ignore(ascii_case))]
    #[token("STRFINDU", |lex| normal_expr_method(lex, BuiltinMethod::StrFindU), ignore(ascii_case))]
    #[token("STRJOIN", |lex| normal_expr_method(lex, BuiltinMethod::StrJoin), ignore(ascii_case))]
    #[token("BARSTR", |lex| normal_expr_method(lex, BuiltinMethod::BarStr), ignore(ascii_case))]
    #[token("GETBIT", |lex| normal_expr_method(lex, BuiltinMethod::GetBit), ignore(ascii_case))]
    #[token("ABS", |lex| normal_expr_method(lex, BuiltinMethod::Abs), ignore(ascii_case))]
    #[token("SIGN", |lex| normal_expr_method(lex, BuiltinMethod::Sign), ignore(ascii_case))]
    #[token("INRANGE", |lex| normal_expr_method(lex, BuiltinMethod::InRange), ignore(ascii_case))]
    #[token("POWER", |lex| normal_expr_method(lex, BuiltinMethod::Power), ignore(ascii_case))]
    #[token("GETEXPLV", |lex| normal_expr_method(lex, BuiltinMethod::GetExpLv), ignore(ascii_case))]
    #[token("GETPALAMLV", |lex| normal_expr_method(lex, BuiltinMethod::GetPalamLv), ignore(ascii_case))]
    #[token("UNICODE", |lex| normal_expr_method(lex, BuiltinMethod::Unicode), ignore(ascii_case))]
    #[token("STRCOUNT", |lex| normal_expr_method(lex, BuiltinMethod::StrCount), ignore(ascii_case))]
    #[token("SUBSTRING", |lex| normal_expr_method(lex, BuiltinMethod::SubString), ignore(ascii_case))]
    #[token("SUBSTRINGU", |lex| normal_expr_method(lex, BuiltinMethod::SubStringU), ignore(ascii_case))]
    #[token("GETCHARA", |lex| normal_expr_method(lex, BuiltinMethod::GetChara), ignore(ascii_case))]
    #[token("CHKDATA", |lex| normal_expr_method(lex, BuiltinMethod::ChkData), ignore(ascii_case))]
    #[token("CHKCHARADATA", |lex| normal_expr_method(lex, BuiltinMethod::ChkCharaData), ignore(ascii_case))]
    #[token("FINDCHARA", |lex| normal_expr_method(lex, BuiltinMethod::FindChara), ignore(ascii_case))]
    #[token("FIND_CHARADATA", |lex| normal_expr_method(lex, BuiltinMethod::FindCharaData), ignore(ascii_case))]
    #[token("CSVNAME", |lex| normal_expr_method(lex, BuiltinMethod::CsvName), ignore(ascii_case))]
    #[token("CSVCALLNAME", |lex| normal_expr_method(lex, BuiltinMethod::CsvCallName), ignore(ascii_case))]
    #[token("CSVNICKNAME", |lex| normal_expr_method(lex, BuiltinMethod::CsvNickName), ignore(ascii_case))]
    #[token("CSVMASTERNAME", |lex| normal_expr_method(lex, BuiltinMethod::CsvMasterName), ignore(ascii_case))]
    #[token("CSVBASE", |lex| normal_expr_method(lex, BuiltinMethod::CsvBase), ignore(ascii_case))]
    #[token("CSVCSTR", |lex| normal_expr_method(lex, BuiltinMethod::CsvCstr), ignore(ascii_case))]
    #[token("CSVABL", |lex| normal_expr_method(lex, BuiltinMethod::CsvAbl), ignore(ascii_case))]
    #[token("CSVTALENT", |lex| normal_expr_method(lex, BuiltinMethod::CsvTalent), ignore(ascii_case))]
    #[token("CSVMARK", |lex| normal_expr_method(lex, BuiltinMethod::CsvMark), ignore(ascii_case))]
    #[token("CSVEX", |lex| normal_expr_method(lex, BuiltinMethod::CsvEx), ignore(ascii_case))]
    #[token("CSVEXP", |lex| normal_expr_method(lex, BuiltinMethod::CsvExp), ignore(ascii_case))]
    #[token("CSVRELATION", |lex| normal_expr_method(lex, BuiltinMethod::CsvRelation), ignore(ascii_case))]
    #[token("CSVJUEL", |lex| normal_expr_method(lex, BuiltinMethod::CsvJuel), ignore(ascii_case))]
    #[token("CSVEQUIP", |lex| normal_expr_method(lex, BuiltinMethod::CsvEquip), ignore(ascii_case))]
    #[token("CSVCFLAG", |lex| normal_expr_method(lex, BuiltinMethod::CsvCflag), ignore(ascii_case))]
    NormalExprMethod((BuiltinMethod, &'s str)),

    #[token("GETTIME", |_| single_method(BuiltinMethod::GetTime), ignore(ascii_case))]
    #[token("GETFONT", |_| single_method(BuiltinMethod::GetFont), ignore(ascii_case))]
    #[token("GETCOLOR", |_| single_method(BuiltinMethod::GetColor), ignore(ascii_case))]
    #[token("GETDEFCOLOR", |_| single_method(BuiltinMethod::GetDefColor), ignore(ascii_case))]
    #[token("GETBGCOLOR", |_| single_method(BuiltinMethod::GetBgColor), ignore(ascii_case))]
    #[token("GETDEFBGCOLOR", |_| single_method(BuiltinMethod::GetDefBgColor), ignore(ascii_case))]
    #[token("GETFOCUSCOLOR", |_| single_method(BuiltinMethod::GetFocusColor), ignore(ascii_case))]
    #[token("QUIT", |_| single_command(BuiltinCommand::Quit), ignore(ascii_case))]
    #[token("FORCEWAIT", |_| single_command(BuiltinCommand::ForceWait), ignore(ascii_case))]
    #[token("WAIT", |_| single_command(BuiltinCommand::Wait), ignore(ascii_case))]
    #[token("WAITANYKEY", |_| single_command(BuiltinCommand::WaitAnykey), ignore(ascii_case))]
    #[token("RESTART", |_| single_command(BuiltinCommand::Restart), ignore(ascii_case))]
    #[token("SAVEGAME", |_| single_command(BuiltinCommand::SaveGame), ignore(ascii_case))]
    #[token("LOADGAME", |_| single_command(BuiltinCommand::LoadGame), ignore(ascii_case))]
    #[token("SAVEGLOBAL", |_| single_command(BuiltinCommand::SaveGlobal), ignore(ascii_case))]
    #[token("LOADGLOBAL", |_| single_command(BuiltinCommand::LoadGlobal), ignore(ascii_case))]
    #[token("DRAWLINE", |_| single_command(BuiltinCommand::DrawLine), ignore(ascii_case))]
    #[token("RESETDATA", |_| single_command(BuiltinCommand::ResetData), ignore(ascii_case))]
    #[token("RESETCOLOR", |_| single_command(BuiltinCommand::ResetColor), ignore(ascii_case))]
    #[token("RESETBGCOLOR", |_| single_command(BuiltinCommand::ResetBgColor), ignore(ascii_case))]
    #[token("ADDDEFCHARA", |_| single_command(BuiltinCommand::AddDefChara), ignore(ascii_case))]
    #[token("FONTBOLD", |_| single_command(BuiltinCommand::FontBold), ignore(ascii_case))]
    #[token("FONTITALIC", |_| single_command(BuiltinCommand::FontItalic), ignore(ascii_case))]
    #[token("FONTREGULAR", |_| single_command(BuiltinCommand::FontRegular), ignore(ascii_case))]
    #[token("UPCHECK", |_| single_command(BuiltinCommand::UpCheck), ignore(ascii_case))]
    #[token("CONTINUE", |_| Stmt::Continue, ignore(ascii_case))]
    #[token("BREAK", |_| Stmt::Break, ignore(ascii_case))]
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
