mod csv;
mod expr;

use erars_ast::{
    Alignment, BeginType, BinaryOperator, BuiltinCommand, EventFlags, EventType, Expr, Function,
    FunctionHeader, FunctionInfo, ScriptPosition, Stmt, StmtWithPos, Variable, VariableInfo,
};
use erars_lexer::{ConfigToken, ErhToken, JumpType, PrintType, Token};
use hashbrown::{HashMap, HashSet};
use logos::{internal::LexerInternal, Lexer};
use smol_str::SmolStr;
use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    collections::BTreeMap,
    mem,
    sync::Arc,
};
use strum::{Display, EnumString};

pub use crate::error::{ParserError, ParserResult};
use crate::CompiledFunction;
pub use expr::normal_form_str;

macro_rules! error {
    ($lex:expr, $msg:expr) => {{
        return Err((String::from($msg), $lex.span()));
    }};
}

macro_rules! take_ident {
    ($ty:ty, $lex:expr) => {
        match $lex.next() {
            Some(Token::Ident(ident)) => match ident.parse::<$ty>() {
                Ok(ret) => ret,
                Err(_) => error!($lex, format!("Unknown ident: {}", ident)),
            },
            other => error!($lex, format!("Expect ident, found: {:?}", other)),
        }
    };
    ($lex:expr) => {
        match $lex.next() {
            Some(Token::Ident(ident)) => ident,
            other => error!($lex, format!("Expect ident, found: {:?}", other)),
        }
    };
}

macro_rules! try_nom {
    ($lex:expr, $ret:expr) => {
        match $ret {
            Ok(ret) => ret,
            Err(err) => match err {
                nom::Err::Error(err) | nom::Err::Failure(err) => {
                    error!($lex, format!("Expression parsing failed: {}", err))
                }
                _ => unreachable!(),
            },
        }
    };
}

// macro_rules! erb_assert_eq {
//     ($lex:expr, $lhs:expr, $rhs:expr, $msg:expr) => {
//         if $lhs != $rhs {
//             error!($lex, $msg);
//         }
//     };
// }

#[derive(Debug, Default)]
pub struct CharacterTemplate {
    pub no: u32,
    pub is_assi: bool,
    pub name: String,
    pub call_name: String,
    pub nick_name: String,
    pub master_name: String,
    pub base: HashMap<u32, u32>,
    pub abl: HashMap<u32, u32>,
    pub cflag: HashMap<u32, u32>,
    pub equip: HashMap<u32, u32>,
    pub juel: HashMap<u32, u32>,
    pub cstr: HashMap<u32, String>,
    pub talent: HashMap<u32, u32>,
    pub exp: HashMap<u32, u32>,
    pub ex: HashMap<u32, u32>,
    pub mark: HashMap<u32, u32>,
    pub relation: HashMap<u32, u32>,
}

#[derive(Debug)]
pub struct ReplaceInfo {
    pub money_unit: String,
    pub unit_forward: bool,
    pub start_message: String,
    pub sell_item_count: i64,
    pub drawline_str: String,
    pub bar_str1: String,
    pub bar_str2: String,
    pub system_menu0: String,
    pub system_menu1: String,
    pub comable_init: i64,
    pub stain_init: Vec<i64>,
    pub timeout_message: String,
    pub explv_init: Vec<i64>,
    pub palamlv_init: Vec<i64>,
    pub pband_init: i64,
    pub relation_init: i64,
}

impl Default for ReplaceInfo {
    fn default() -> Self {
        Self {
            money_unit: "$".into(),
            unit_forward: false,
            start_message: "Now Loading...".into(),
            sell_item_count: 100,
            drawline_str: "-".into(),
            bar_str1: "*".into(),
            bar_str2: ".".into(),
            system_menu0: "[0] 最初からはじめる".into(),
            system_menu1: "[1] ロードしてはじめる".into(),
            comable_init: 0,
            stain_init: vec![0, 0, 2, 1, 8],
            timeout_message: "時間切れ".into(),
            explv_init: vec![0, 1, 4, 20, 50, 200],
            palamlv_init: vec![
                0, 100, 500, 3000, 10000, 30000, 60000, 100000, 150000, 250000,
            ],
            pband_init: 4,
            relation_init: 0,
        }
    }
}

#[derive(Clone, Debug, derivative::Derivative)]
#[derivative(Default)]
pub struct EraConfig {
    pub lang: Language,
    pub save_nos: usize,
    #[derivative(Default(value = "4"))]
    pub printc_count: usize,
    #[derivative(Default(value = "30"))]
    pub printc_width: usize,
}

impl EraConfig {
    pub fn from_text(s: &str) -> ParserResult<Self> {
        let mut ret = Self::default();

        let mut lex = Lexer::new(s);

        while let Some(line) = lex.next() {
            match line {
                ConfigToken::Line((key, value)) => match key {
                    "PRINTCを並べる数" => {
                        ret.printc_count = match value.parse() {
                            Ok(l) => l,
                            Err(_) => error!(lex, format!("Invalid integer {value}")),
                        };
                    }
                    "PRINTCの文字数" => {
                        ret.printc_width = match value.parse() {
                            Ok(l) => l,
                            Err(_) => error!(lex, format!("Invalid integer {value}")),
                        };
                    }
                    "内部で使用する東アジア言語" => {
                        ret.lang = match value.parse() {
                            Ok(l) => l,
                            Err(_) => error!(lex, format!("Invalid language {value}")),
                        };
                    }
                    "表示するセーブデータ数" => {
                        ret.save_nos = match value.parse() {
                            Ok(l) => l,
                            Err(_) => error!(lex, format!("Invalid save_nos {value}")),
                        };
                    }
                    _ => {}
                },
                ConfigToken::Error => error!(lex, format!("Invalid token: {}", lex.slice())),
            }
        }

        Ok(ret)
    }
}

#[derive(Clone, Copy, Debug, EnumString, Display)]
pub enum Language {
    #[strum(to_string = "JAPANESE")]
    Japanese,
    #[strum(to_string = "KOREAN")]
    Korean,
    #[strum(to_string = "CHINESE_HANS")]
    ChineseHant,
    #[strum(to_string = "CHINESE_HANT")]
    ChineseHans,
}

impl Default for Language {
    fn default() -> Self {
        Self::Japanese
    }
}

#[derive(Debug)]
pub struct DefaultLocalVarSize {
    pub default_local_size: Option<usize>,
    pub default_locals_size: Option<usize>,
    pub default_arg_size: Option<usize>,
    pub default_args_size: Option<usize>,
}

impl Default for DefaultLocalVarSize {
    fn default() -> Self {
        Self {
            default_arg_size: Some(1000),
            default_local_size: Some(1000),
            default_args_size: Some(100),
            default_locals_size: Some(100),
        }
    }
}

#[derive(Debug, Default)]
pub struct HeaderInfo {
    pub macros: HashMap<String, String>,
    pub replace: ReplaceInfo,
    pub character_templates: HashMap<u32, CharacterTemplate>,
    pub item_price: HashMap<u32, u32>,
    pub var_names: HashMap<(SmolStr, SmolStr), u32>,
    pub var_name_var: HashMap<SmolStr, BTreeMap<u32, SmolStr>>,
    pub global_variables: HashMap<SmolStr, VariableInfo>,
    pub default_local_size: DefaultLocalVarSize,
}

impl HeaderInfo {
    pub fn merge_chara_csv(&mut self, s: &str) -> ParserResult<()> {
        let mut lex = Lexer::new(s);
        let mut template = CharacterTemplate::default();

        macro_rules! insert_template {
            ($name:expr, $var:ident, $val1:expr, $val2:expr) => {{
                let idx = match $val1.parse::<u32>() {
                    Ok(idx) => idx,
                    Err(_) => match self
                        .var_names
                        .get(&(SmolStr::new_inline($name), SmolStr::new($val1)))
                        .copied()
                    {
                        Some(idx) => idx,
                        None => error!(lex, "잘못된 숫자입니다."),
                    },
                };

                let value = match $val2.parse::<u32>() {
                    Ok(idx) => idx,
                    Err(_) => error!(lex, "잘못된 숫자입니다."),
                };

                template.$var.insert(idx, value);
            }};
            (@bool $name:expr, $var:ident, $val1:expr, $val2:expr) => {{
                let idx = match $val1.parse::<u32>() {
                    Ok(idx) => idx,
                    Err(_) => match self
                        .var_names
                        .get(&(SmolStr::new_inline($name), SmolStr::new($val1)))
                        .copied()
                    {
                        Some(idx) => idx,
                        None => error!(lex, "잘못된 숫자입니다."),
                    },
                };
                template.$var.insert(idx, 1);
            }};
            (@str $name:expr, $var:ident, $val1:expr, $val2:expr) => {{
                let idx = match $val1.parse::<u32>() {
                    Ok(idx) => idx,
                    Err(_) => match self
                        .var_names
                        .get(&(SmolStr::new_inline($name), SmolStr::new($val1)))
                        .copied()
                    {
                        Some(idx) => idx,
                        None => error!(lex, "잘못된 숫자입니다."),
                    },
                };
                template.$var.insert(idx, $val2.into());
            }};
        }

        while let Some((name, val1, val2)) = self::csv::chara_line(&mut lex)? {
            match name {
                "番号" => template.no = val1.parse().unwrap(),
                "名前" => template.name = val1.into(),
                "主人の呼び方" => template.master_name = val1.into(),
                "呼び名" => template.call_name = val1.into(),
                "あだ名" => template.nick_name = val1.into(),
                "助手" => template.is_assi = val1.trim() == "1",

                "CSTR" => insert_template!(@str "CSTR", cstr, val1, val2),

                "素質" => insert_template!(@bool "TALENT", talent, val1, val2),

                "基礎" => insert_template!("BASE", base, val1, val2),
                "刻印" => insert_template!("MARK", mark, val1, val2),
                "能力" => insert_template!("ABL", abl, val1, val2),
                "経験" => insert_template!("EXP", exp, val1, val2),
                "相性" => insert_template!("RELATION", relation, val1, val2),
                "装着物" => insert_template!("EQUIP", equip, val1, val2),
                "フラグ" => insert_template!("CFLAG", cflag, val1, val2),
                other => log::warn!("Unknown character template name: {other}"),
            }
        }

        self.character_templates.insert(template.no, template);

        Ok(())
    }

    pub fn merge_name_csv(&mut self, var: &str, s: &str) -> ParserResult<()> {
        let mut lex = Lexer::new(s);
        let var = SmolStr::from(var);
        let mut name_var = BTreeMap::new();

        while let Some((n, s)) = self::csv::name_csv_line(&mut lex)? {
            self.var_names.insert((var.clone(), s.clone()), n);
            name_var.insert(n, s);
        }

        self.var_name_var.insert(var, name_var);

        Ok(())
    }

    pub fn merge_item_csv(&mut self, s: &str) -> ParserResult<()> {
        let mut lex = Lexer::new(s);
        let var = SmolStr::new_inline("ITEM");

        while let Some((n, s, price)) = self::csv::name_item_line(&mut lex)? {
            self.item_price.insert(n, price);
            self.var_names.insert((var.clone(), s.clone()), n);
            self.var_name_var.entry(var.clone()).or_default().insert(n, s);
        }

        Ok(())
    }

    pub fn merge_variable_size_csv(&mut self, s: &str) -> ParserResult<()> {
        let mut lex = Lexer::new(s);

        while let Some((name, sizes)) = self::csv::variable_size_line(&mut lex)? {
            match name.as_str() {
                "ARG" => {
                    self.default_local_size.default_arg_size =
                        sizes.and_then(|v| v.first().copied())
                }
                "ARGS" => {
                    self.default_local_size.default_args_size =
                        sizes.and_then(|v| v.first().copied())
                }
                "LOCAL" => {
                    self.default_local_size.default_local_size =
                        sizes.and_then(|v| v.first().copied())
                }
                "LOCALS" => {
                    self.default_local_size.default_locals_size =
                        sizes.and_then(|v| v.first().copied())
                }
                name => {
                    match sizes {
                        Some(sizes) => match self.global_variables.get_mut(name) {
                            Some(info) => {
                                let info_len = info.size.len();
                                if info.size.len() != sizes.len() {
                                    log::error!("Variable size for {name} is not matched! Expected: {info_len} Actual: {size_len}", size_len = sizes.len());
                                } else {
                                    info.size.copy_from_slice(&sizes[..info_len]);
                                }
                            }
                            None => {
                                log::error!(
                                    "Variable {name} is not exists but defined in variablesize.csv"
                                );
                            }
                        },
                        None => {
                            // FORBIDDEN
                            log::info!("Don't use {name}");
                            self.global_variables.remove(name);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub fn merge_replace_csv(&mut self, s: &str) -> ParserResult<()> {
        let mut lex = Lexer::new(s);

        while let Some((k, v)) = self::csv::csv2_line(&mut lex)? {
            macro_rules! define_replace_parser {
                (
                    @direct [$(($dr_key:literal, $dr_field:ident),)*]
                    @parse [$(($pa_key:literal, $pa_field:ident),)*]
                    @match_ [$(($ma_key:literal, $ma_field:ident, [$($ma_subkey:literal => $ma_value:expr,)+]),)*]
                    @arr [$(($ar_key:literal, $ar_field:ident),)*]
                ) => {
                    match k {
                        $(
                            $dr_key => self.replace.$dr_field = v.into(),
                        )*
                        $(
                            $pa_key => match v.parse() {
                                Ok(v) => self.replace.$pa_field = v,
                                Err(_) => {
                                    log::error!("Invalid value for `{k}`: {v}");
                                    continue;
                                },
                            },
                        )*
                        $(
                            $ma_key => self.replace.$ma_field = match v {
                                $(
                                    $ma_subkey => $ma_value,
                                )*
                                _ => {
                                    log::error!("Invalid value for `{k}`: {v}");
                                    continue;
                                },
                            },
                        )*
                        $(
                            $ar_key => match v.split('/').map(|s| s.parse()).collect::<Result<Vec<_>, _>>() {
                                Ok(v) => self.replace.$ar_field = v,
                                Err(_) => {
                                    log::error!("Invalid value for `{k}`: {v}");
                                    continue;
                                },
                            },
                        )*
                        _ => {
                            log::error!("Unknown replace key: {k}");
                        }
                    }
                };
            }

            define_replace_parser! {
                @direct [
                    ("お金の単位", money_unit),
                    ("起動時簡略表示", start_message),
                    ("DRAWLINE文字", drawline_str),
                    ("BAR文字1", bar_str1),
                    ("BAR文字2", bar_str2),
                    ("システムメニュー0", system_menu0),
                    ("システムメニュー1", system_menu1),
                    ("時間切れ表示", timeout_message),
                ]
                @parse [
                    ("販売アイテム数", sell_item_count),
                    ("COM_ABLE初期値", comable_init),
                    ("PBANDの初期値", pband_init),
                    ("RELATIONの初期値", relation_init),
                ]
                @match_ [
                    ("単位の位置", unit_forward, [
                        "前" => true,
                        "後" => false,
                    ]),
                ]
                @arr [
                    ("汚れの初期値", stain_init),
                    ("EXPLVの初期値", explv_init),
                    ("PALAMLVの初期値", palamlv_init),
                ]
            }
        }

        Ok(())
    }

    pub fn merge_header(&mut self, s: &str) -> ParserResult<()> {
        let ctx = ParserContext::default();
        let mut lex = Lexer::new(s);

        loop {
            match lex.next() {
                Some(ErhToken::Define(def)) => {
                    let (def, ident) = try_nom!(lex, self::expr::ident(def));
                    self.macros.insert(ident.to_string(), def.trim().to_string());
                }
                Some(ErhToken::Dim(dim)) => {
                    let var = try_nom!(lex, self::expr::dim_line(&ctx, false)(dim)).1;
                    self.global_variables.insert(var.var, var.info);
                }
                Some(ErhToken::DimS(dims)) => {
                    let var = try_nom!(lex, self::expr::dim_line(&ctx, true)(dims)).1;
                    self.global_variables.insert(var.var, var.info);
                }
                Some(ErhToken::Error) => error!(lex, "Invalid token"),
                None => break,
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct ParserContext {
    pub header: Arc<HeaderInfo>,
    pub local_strs: RefCell<HashSet<SmolStr>>,
    pub is_arg: Cell<bool>,
    pub ban_percent: Cell<bool>,
    pub file_path: SmolStr,
    pub line: Cell<u32>,
}

impl Default for ParserContext {
    fn default() -> Self {
        Self::new(Arc::default(), "".into())
    }
}

impl ParserContext {
    pub fn new(header: Arc<HeaderInfo>, file_path: SmolStr) -> Self {
        Self {
            header,
            file_path,
            local_strs: RefCell::default(),
            is_arg: Cell::new(false),
            ban_percent: Cell::new(false),
            line: Cell::new(0),
        }
    }

    fn current_pos(&self) -> ScriptPosition {
        ScriptPosition {
            line: self.line.get(),
        }
    }

    fn next_token<'s>(&self, lex: &mut Lexer<'s, Token<'s>>) -> ParserResult<Option<Token<'s>>> {
        loop {
            match lex.next() {
                Some(Token::Preprocess("[SKIPSTART]")) => loop {
                    match lex.next() {
                        Some(Token::Preprocess("[SKIPEND]")) => break,
                        Some(Token::Newline) => {
                            self.line.set(self.line.get() + 1);
                        }
                        None => error!(lex, "[SKIPSTART]가 [SKIPEND]없이 끝났습니다."),
                        _ => {}
                    }
                },
                Some(Token::Preprocess(_)) => {}
                Some(Token::Newline) => {
                    self.line.set(self.line.get() + 1);
                }
                Some(tok) => break Ok(Some(tok)),
                None => break Ok(None),
            }
        }
    }

    pub fn is_str_var(&self, ident: &str) -> bool {
        if matches!(ident, "LOCALS" | "ARGS") || self.local_strs.borrow().contains(ident) {
            true
        } else if let Some(v) = self.header.global_variables.get(ident) {
            v.is_str
        } else {
            false
        }
    }

    pub fn replace<'s>(&self, s: &'s str) -> Cow<'s, str> {
        let mut ret = Cow::Borrowed(s);

        while let Some(new) = self.header.macros.get(ret.as_ref()) {
            ret = Cow::Owned(new.clone());
        }

        ret
    }

    pub fn parse_stmt<'s>(
        &self,
        first: Token<'s>,
        lex: &mut Lexer<'s, Token<'s>>,
    ) -> ParserResult<StmtWithPos> {
        let first_pos = self.current_pos();

        let stmt = match first {
            Token::DirectStmt(stmt) => stmt,
            Token::Alignment => Stmt::Alignment(take_ident!(Alignment, lex)),
            Token::Begin => Stmt::Begin(take_ident!(BeginType, lex)),
            Token::CallEvent => Stmt::CallEvent(take_ident!(EventType, lex)),
            Token::LabelLine(label) => Stmt::Label(label.into()),
            Token::StrFormMethod((method, left)) => {
                let (_, form) = try_nom!(lex, self::expr::normal_form_str(self)(left));
                Stmt::Method(method, vec![form])
            }
            Token::StrFormCommand((com, left)) => {
                let (_, form) = try_nom!(lex, self::expr::normal_form_str(self)(left));
                Stmt::Command(com, vec![form])
            }
            Token::CustomDrawLine(custom) => {
                Stmt::Command(BuiltinCommand::CustomDrawLine, vec![Expr::str(custom)])
            }
            Token::Times(left) => try_nom!(lex, self::expr::times_line(self)(left)).1,
            Token::Throw(left) => Stmt::Command(
                BuiltinCommand::Throw,
                vec![try_nom!(lex, self::expr::normal_form_str(self)(left)).1],
            ),
            Token::Print((flags, PrintType::Plain, form)) => Stmt::Print(flags, Expr::str(form)),
            Token::Print((flags, PrintType::Data, form)) => {
                let form = form.trim();
                let cond = if form.is_empty() {
                    None
                } else {
                    Some(try_nom!(lex, self::expr::expr(self)(form)).1)
                };
                let mut list = Vec::new();

                loop {
                    match self.next_token(lex)? {
                        Some(Token::Data(data)) => list.push(vec![Expr::str(data)]),
                        Some(Token::DataForm(data)) => list.push(vec![
                            try_nom!(lex, self::expr::normal_form_str(self)(data)).1,
                        ]),
                        Some(Token::DataList) => {
                            let mut cur_list = Vec::new();
                            loop {
                                match self.next_token(lex)? {
                                    Some(Token::Data(data)) => cur_list.push(Expr::str(data)),
                                    Some(Token::DataForm(data)) => cur_list.push(
                                        try_nom!(lex, self::expr::normal_form_str(self)(data)).1,
                                    ),
                                    Some(Token::EndList) => break,
                                    Some(_) => {
                                        error!(lex, "DATALIST에 잘못된 토큰이 들어왔습니다")
                                    }
                                    None => error!(lex, "ENDLIST없이 DATALIST가 끝났습니다."),
                                }
                            }
                            list.push(cur_list);
                        }
                        Some(Token::EndData) => break,
                        Some(_) => error!(lex, "PRINTDATA에 잘못된 토큰이 들어왔습니다"),
                        None => error!(lex, "ENDDATA없이 PRINTDATA가 끝났습니다."),
                    }
                }

                Stmt::PrintData(flags, cond, list)
            }
            Token::Print((flags, PrintType::Form, form)) => {
                let (_, form) = try_nom!(lex, self::expr::normal_form_str(self)(form));
                Stmt::Print(flags, form)
            }
            Token::Print((flags, PrintType::S, form)) => {
                let (_, s) = try_nom!(lex, self::expr::expr(self)(form));
                Stmt::Print(flags, s)
            }
            Token::Print((flags, PrintType::FormS, form)) => {
                let (_, s) = try_nom!(lex, self::expr::expr(self)(form));
                Stmt::PrintFormS(flags, s)
            }
            Token::Print((flags, PrintType::V, form)) => {
                let (_, s) = try_nom!(lex, self::expr::expr_list(self)(form));
                Stmt::PrintList(flags, s)
            }
            Token::CallJump((info, args)) => {
                let (name, args) =
                    try_nom!(lex, self::expr::call_jump_line(self, info.is_form)(args)).1;
                let mut try_body = Vec::new();

                let catch_body = if info.is_catch {
                    let mut catch_body = Vec::new();

                    loop {
                        match self.next_token(lex)? {
                            Some(Token::Catch) => {
                                break;
                            }
                            Some(tok) => try_body.push(self.parse_stmt(tok, lex)?),
                            None => error!(lex, "CATCH없이 끝났습니다."),
                        }
                    }

                    loop {
                        match self.next_token(lex)? {
                            Some(Token::EndCatch) => {
                                break;
                            }
                            Some(tok) => catch_body.push(self.parse_stmt(tok, lex)?),
                            None => error!(lex, "ENDCATCH없이 끝났습니다."),
                        }
                    }

                    Some(catch_body)
                } else if info.is_try {
                    Some(Vec::new())
                } else {
                    None
                };

                match info.ty {
                    JumpType::Goto => Stmt::Goto {
                        label: name,
                        catch_body,
                    },
                    JumpType::Call | JumpType::Jump => Stmt::Call {
                        name,
                        args,
                        try_body,
                        catch_body,
                        is_jump: info.ty == JumpType::Jump,
                        is_method: info.is_method,
                    },
                }
            }
            Token::Sif(cond) => {
                let cond = try_nom!(lex, self::expr::expr(self)(cond)).1;
                let first = match self.next_token(lex)? {
                    Some(first) => first,
                    None => error!(lex, "Unexpected EOF after SIF"),
                };
                let body = self.parse_stmt(first, lex)?;

                Stmt::Sif(cond, Box::new(body))
            }
            Token::SelectCase(cond) => {
                let cond = try_nom!(lex, self::expr::expr(self)(cond)).1;
                let mut has_else = false;
                let mut body = Vec::new();
                let mut cases: Vec<(_, Vec<StmtWithPos>)> = Vec::new();

                loop {
                    match self.next_token(lex)? {
                        Some(Token::Case(case)) => {
                            if !cases.is_empty() {
                                cases.last_mut().unwrap().1 = mem::take(&mut body);
                            }
                            let case = try_nom!(lex, self::expr::case_line(self)(case)).1;
                            cases.push((case, Vec::new()));
                        }
                        Some(Token::CaseElse) => {
                            cases.last_mut().unwrap().1 = mem::take(&mut body);
                            has_else = true;
                        }
                        Some(Token::EndSelect) => break,
                        Some(tok) => {
                            body.push(self.parse_stmt(tok, lex)?);
                        }
                        None => error!(lex, "Unexpected EOF after SELECTCASE"),
                    }
                }

                if has_else {
                    Stmt::SelectCase(cond, cases, Some(body))
                } else {
                    if let Some(last) = cases.last_mut() {
                        last.1 = body;
                    }
                    Stmt::SelectCase(cond, cases, None)
                }
            }
            Token::For(left) => {
                let (var, init, end, step) = try_nom!(lex, self::expr::for_line(self)(left)).1;
                let mut body = Vec::new();

                loop {
                    match self.next_token(lex)? {
                        Some(Token::Next) => {
                            break Stmt::For(var, Box::new((init, end, step)), body)
                        }
                        Some(other) => {
                            body.push(self.parse_stmt(other, lex)?);
                        }
                        None => error!(lex, "Unexpected EOF after FOR"),
                    }
                }
            }
            Token::While(left) => {
                let cond = try_nom!(lex, self::expr::expr(self)(left)).1;
                let mut body = Vec::new();
                loop {
                    match self.next_token(lex)? {
                        Some(Token::Wend) => {
                            break Stmt::While(cond, body);
                        }
                        Some(tok) => {
                            body.push(self.parse_stmt(tok, lex)?);
                        }
                        None => error!(lex, "Unexpected EOF after DO"),
                    }
                }
            }
            Token::Do => {
                let mut body = Vec::new();
                loop {
                    match self.next_token(lex)? {
                        Some(Token::Loop(left)) => {
                            break Stmt::Do(try_nom!(lex, self::expr::expr(self)(left)).1, body);
                        }
                        Some(tok) => {
                            body.push(self.parse_stmt(tok, lex)?);
                        }
                        None => error!(lex, "Unexpected EOF after DO"),
                    }
                }
            }
            Token::Repeat(left) => {
                let arg = try_nom!(lex, self::expr::expr(self)(left)).1;
                let mut body = Vec::new();

                loop {
                    match self.next_token(lex)? {
                        Some(Token::Rend) => break Stmt::Repeat(arg, body),
                        Some(other) => {
                            body.push(self.parse_stmt(other, lex)?);
                        }
                        None => error!(lex, "Unexpected EOF after FOR"),
                    }
                }
            }
            Token::If(left) => {
                let mut is_else = false;
                let mut cond = try_nom!(lex, self::expr::expr(self)(left)).1;
                let mut block = Vec::new();
                let mut if_elses = Vec::new();

                loop {
                    match self.next_token(lex)? {
                        Some(Token::ElseIf(left)) => {
                            let left = left.trim_start_matches(' ');

                            if_elses.push((cond, block));
                            block = Vec::new();
                            cond = if left.is_empty() {
                                Expr::Int(1)
                            } else {
                                try_nom!(lex, self::expr::expr(self)(left)).1
                            };
                        }
                        Some(Token::Else) => {
                            is_else = true;
                            if_elses.push((cond, block));
                            cond = Expr::Int(1);
                            block = Vec::new();
                        }
                        Some(Token::EndIf) => {
                            if !is_else {
                                if_elses.push((cond, block));
                                block = Vec::new();
                            }
                            break;
                        }
                        Some(token) => {
                            block.push(self.parse_stmt(token, lex)?);
                        }
                        None => break,
                    }
                }

                Stmt::If(if_elses, block)
            }
            Token::Inc => {
                let ident = self.replace(take_ident!(lex));
                let left = cut_line(lex);
                let (left, func_extern) = try_nom!(lex, self::expr::var_func_extern(left));
                let args = try_nom!(lex, self::expr::variable_arg(self, &ident)(left)).1;
                let var = Variable {
                    var: ident.into(),
                    func_extern,
                    args,
                };
                Stmt::Assign(var, Some(BinaryOperator::Add), Expr::Int(1))
            }
            Token::Dec => {
                let ident = self.replace(take_ident!(lex));
                let left = cut_line(lex);
                let (left, func_extern) = try_nom!(lex, self::expr::var_func_extern(left));
                let args = try_nom!(lex, self::expr::variable_arg(self, &ident)(left)).1;
                let var = Variable {
                    var: ident.into(),
                    func_extern,
                    args,
                };
                Stmt::Assign(var, Some(BinaryOperator::Sub), Expr::Int(1))
            }
            Token::Ident(var) => {
                let i = cut_line(lex);
                try_nom!(lex, self::expr::assign_line(self, var)(i)).1
            }
            Token::NormalExprCommand((com, args)) => {
                let args = try_nom!(lex, self::expr::expr_list(self)(args)).1;
                Stmt::Command(com, args)
            }
            Token::NormalExprMethod((meth, args)) => {
                let args = try_nom!(lex, self::expr::expr_list(self)(args)).1;
                Stmt::Method(meth, args)
            }
            other => error!(lex, format!("[Stmt] Invalid token: {:?}", other)),
        };

        Ok(StmtWithPos(stmt, first_pos))
    }

    pub fn parse_and_compile<'s>(
        &self,
        lex: &mut Lexer<'s, Token<'s>>,
    ) -> ParserResult<Vec<CompiledFunction>> {
        let mut out = Vec::with_capacity(1024);
        let mut compiler = crate::compiler::Compiler::new();

        let mut current_func_header = FunctionHeader::default();

        loop {
            match self.next_token(lex)? {
                Some(Token::At(left)) => {
                    let (label, args) = try_nom!(lex, self::expr::function_line(self)(left)).1;
                    if !current_func_header.name.is_empty() {
                        out.push(CompiledFunction {
                            header: mem::take(&mut current_func_header),
                            body: compiler.out.into_boxed_slice(),
                            goto_labels: compiler.goto_labels,
                        });
                        compiler = crate::compiler::Compiler::new();
                    }
                    self.local_strs.borrow_mut().clear();
                    current_func_header.file_path = self.file_path.clone();
                    current_func_header.name = label.into();
                    current_func_header.args = args;
                }
                Some(Token::Function) => {
                    current_func_header.infos.push(FunctionInfo::Function);
                }
                Some(Token::FunctionS) => {
                    current_func_header.infos.push(FunctionInfo::FunctionS);
                }
                Some(Token::Pri) => {
                    current_func_header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Pre));
                }
                Some(Token::Later) => {
                    current_func_header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Later));
                }
                Some(Token::Single) => {
                    current_func_header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Single));
                }
                Some(Token::Dim(left)) => {
                    let var = try_nom!(lex, self::expr::dim_line(self, false)(left)).1;
                    current_func_header.infos.push(FunctionInfo::Dim(var));
                }
                Some(Token::DimS(left)) => {
                    let var = try_nom!(lex, self::expr::dim_line(self, true)(left)).1;
                    self.local_strs.borrow_mut().insert(var.var.clone());
                    current_func_header.infos.push(FunctionInfo::Dim(var));
                }
                Some(Token::LocalSize(size)) => {
                    let var = try_nom!(lex, self::expr::expr(self)(size))
                        .1
                        .into_const_int()
                        .unwrap();
                    current_func_header.infos.push(FunctionInfo::LocalSize(var as usize));
                }
                Some(Token::LocalSSize(size)) => {
                    let var = try_nom!(lex, self::expr::expr(self)(size))
                        .1
                        .into_const_int()
                        .unwrap();
                    current_func_header.infos.push(FunctionInfo::LocalSSize(var as usize));
                }
                Some(other) => match self.parse_stmt(other, lex) {
                    Ok(stmt) => match compiler.push_stmt_with_pos(stmt) {
                        Ok(_) => {}
                        Err(err) => error!(lex, err.to_string()),
                    },
                    Err(err) => {
                        return Err(err);
                    }
                },
                None => break,
            }
        }

        if !current_func_header.name.is_empty() {
            out.push(CompiledFunction {
                header: mem::take(&mut current_func_header),
                body: compiler.out.into_boxed_slice(),
                goto_labels: compiler.goto_labels,
            });
        }

        Ok(out)
    }

    pub fn parse<'s>(&self, lex: &mut Lexer<'s, Token<'s>>) -> ParserResult<Vec<Function>> {
        let mut out = Vec::new();
        let mut current_func = Function::default();

        loop {
            match self.next_token(lex)? {
                Some(Token::At(left)) => {
                    let (label, args) = try_nom!(lex, self::expr::function_line(self)(left)).1;
                    if !current_func.header.name.is_empty() {
                        out.push(mem::take(&mut current_func));
                    }
                    self.local_strs.borrow_mut().clear();
                    current_func.header.file_path = self.file_path.clone();
                    current_func.header.name = label.into();
                    current_func.header.args = args;
                }
                Some(Token::Function) => {
                    current_func.header.infos.push(FunctionInfo::Function);
                }
                Some(Token::FunctionS) => {
                    current_func.header.infos.push(FunctionInfo::FunctionS);
                }
                Some(Token::Pri) => {
                    current_func
                        .header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Pre));
                }
                Some(Token::Later) => {
                    current_func
                        .header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Later));
                }
                Some(Token::Single) => {
                    current_func
                        .header
                        .infos
                        .push(FunctionInfo::EventFlag(EventFlags::Single));
                }
                Some(Token::Dim(left)) => {
                    let var = try_nom!(lex, self::expr::dim_line(self, false)(left)).1;
                    current_func.header.infos.push(FunctionInfo::Dim(var));
                }
                Some(Token::DimS(left)) => {
                    let var = try_nom!(lex, self::expr::dim_line(self, true)(left)).1;
                    self.local_strs.borrow_mut().insert(var.var.clone());
                    current_func.header.infos.push(FunctionInfo::Dim(var));
                }
                Some(Token::LocalSize(size)) => {
                    let var = try_nom!(lex, self::expr::expr(self)(size))
                        .1
                        .into_const_int()
                        .unwrap();
                    current_func.header.infos.push(FunctionInfo::LocalSize(var as usize));
                }
                Some(Token::LocalSSize(size)) => {
                    let var = try_nom!(lex, self::expr::expr(self)(size))
                        .1
                        .into_const_int()
                        .unwrap();
                    current_func.header.infos.push(FunctionInfo::LocalSSize(var as usize));
                }
                Some(other) => match self.parse_stmt(other, lex) {
                    Ok(stmt) => current_func.body.push(stmt),
                    Err(err) => {
                        return Err(err);
                    }
                },
                None => break,
            }
        }

        if !current_func.header.name.is_empty() {
            out.push(current_func);
        }

        Ok(out)
    }
}

impl ParserContext {
    pub fn parse_program_str<'s>(&self, s: &'s str) -> ParserResult<Vec<Function>> {
        self.parse(&mut Lexer::new(s))
    }

    pub fn parse_function_str<'s>(&self, s: &'s str) -> ParserResult<Function> {
        self.parse_program_str(s).map(|f| f.into_iter().next().unwrap())
    }

    pub fn parse_expr_str<'s>(&self, s: &'s str) -> ParserResult<Expr> {
        let lex = Lexer::<Token<'s>>::new(s);
        Ok(try_nom!(lex, self::expr::expr(self)(s)).1)
    }

    pub fn parse_body_str<'s>(&self, s: &'s str) -> ParserResult<Vec<StmtWithPos>> {
        let mut lex = Lexer::<Token<'s>>::new(s);
        let mut body = Vec::new();

        loop {
            match self.next_token(&mut lex)? {
                Some(tok) => body.push(self.parse_stmt(tok, &mut lex)?),
                None => break,
            }
        }

        Ok(body)
    }

    pub fn parse_stmt_str<'s>(&self, s: &'s str) -> ParserResult<StmtWithPos> {
        let mut lex = Lexer::new(s);
        let first = self.next_token(&mut lex)?.unwrap();
        self.parse_stmt(first, &mut lex)
    }
}

fn cut_line<'s>(lex: &mut Lexer<'s, Token<'s>>) -> &'s str {
    let i = lex.remainder();
    let l = i.split_once('\n').map(|(l, _)| l).unwrap_or(i);
    lex.bump_unchecked(l.len());
    l
}