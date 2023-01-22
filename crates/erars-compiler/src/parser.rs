mod csv;
mod expr;

use anyhow::{bail, Context};
use erars_ast::{
    get_interner, BinaryOperator, BuiltinCommand, BuiltinMethod, EventFlags, Expr, ExprWithPos,
    Function, FunctionHeader, FunctionInfo, Interner, PrintFlags, Stmt, StmtWithPos, StrKey,
    UnaryOperator, Value, VariableInfo,
};
use erars_lexer::{
    Bump, ComplexAssign, ConfigToken, EraLine, InstructionCode, Preprocessor, PrintType, SharpCode,
};
use hashbrown::{HashMap, HashSet};
use itertools::Itertools;
use logos::Lexer;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    collections::BTreeMap,
    mem,
    sync::Arc,
};
use strum::{Display, EnumString};

pub use crate::error::{ParserError, ParserResult};
use crate::{compiler::Compiler, CompiledFunction, PP_REGEX};
pub use expr::normal_form_str;

macro_rules! error_csv {
    ($msg:expr, $span:expr) => {{
        return Err((String::from($msg), $span));
    }};
}

macro_rules! csv_parse_int {
    ($s:expr, $span:expr) => {
        match $s.parse() {
            Ok(n) => n,
            Err(_) => error_csv!("Invalid number", $span.clone()),
        }
    };
}

macro_rules! error {
    ($span:expr, $msg:expr) => {{
        return Err((String::from($msg), $span));
    }};
}

macro_rules! try_nom {
    (@str $s:expr, $ret:expr) => {
        match $ret {
            Ok(ret) => ret,
            Err(err) => match err {
                nom::Err::Error(err) | nom::Err::Failure(err) => {
                    return Err((format!("Expression parsing failed: {}", err), 0..$s.len()));
                }
                _ => unreachable!(),
            },
        }
    };
    (@span $span:expr, $ret:expr) => {
        match $ret {
            Ok(ret) => ret,
            Err(err) => match err {
                nom::Err::Error(err) | nom::Err::Failure(err) => {
                    error!($span, format!("Expression parsing failed: {}", err))
                }
                _ => unreachable!(),
            },
        }
    };
    ($pp:expr, $ret:expr) => {
        try_nom!(@span $pp.span(), $ret)
    };
}

// macro_rules! erb_assert_eq {
//     ($lex:expr, $lhs:expr, $rhs:expr, $msg:expr) => {
//         if $lhs != $rhs {
//             error!($lex, $msg);
//         }
//     };
// }

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct CharacterTemplate {
    pub no: i64,
    pub is_assi: bool,
    pub name: String,
    pub call_name: String,
    pub nick_name: String,
    pub master_name: String,
    pub base: HashMap<u32, i64>,
    pub abl: HashMap<u32, i64>,
    pub cflag: HashMap<u32, i64>,
    pub equip: HashMap<u32, i64>,
    pub juel: HashMap<u32, i64>,
    pub cstr: HashMap<u32, String>,
    pub talent: HashMap<u32, i64>,
    pub exp: HashMap<u32, i64>,
    pub ex: HashMap<u32, i64>,
    pub mark: HashMap<u32, i64>,
    pub relation: HashMap<u32, i64>,
}

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Gamebase {
    pub code: u32,
    pub version: u32,
    pub allow_version: u32,
    pub default_chara: u32,
    pub no_item: u32,
    pub author: String,
    pub info: String,
    pub year: String,
    pub title: String,
}

#[derive(Clone, Copy, Debug, Display, EnumString)]
pub enum EraConfigKey {
    #[strum(to_string = "内部で使用する東アジア言語")]
    Lang,

    #[strum(to_string = "表示するセーブデータ数")]
    SaveNos,

    #[strum(to_string = "フォント名")]
    FontFamily,

    #[strum(to_string = "フォントサイズ")]
    FontSize,

    #[strum(to_string = "一行の高さ")]
    LineHeight,

    #[strum(to_string = "PRINTCを並べる数")]
    PrintcCount,

    #[strum(to_string = "PRINTCの文字数")]
    PrintcWidth,

    #[strum(to_string = "履歴ログの行数")]
    MaxLog,
}

#[derive(Clone, Debug, derivative::Derivative, Serialize, Deserialize)]
#[derivative(Default)]
pub struct EraConfig {
    pub lang: Language,
    pub save_nos: usize,
    #[derivative(Default(value = "500"))]
    pub max_log: usize,
    #[derivative(Default(value = "4"))]
    pub printc_count: usize,
    #[derivative(Default(value = "30"))]
    pub printc_width: usize,

    #[derivative(Default(value = "String::from(\"D2Coding\")"))]
    pub font_family: String,
    #[derivative(Default(value = "18"))]
    pub font_size: u32,
    #[derivative(Default(value = "19"))]
    pub line_height: u32,
}

impl EraConfig {
    pub fn get_config(&self, key: EraConfigKey) -> erars_ast::Value {
        match key {
            EraConfigKey::PrintcCount => self.printc_count.into(),
            EraConfigKey::MaxLog => self.max_log.into(),
            EraConfigKey::PrintcWidth => self.printc_width.into(),
            EraConfigKey::Lang => self.lang.to_string().into(),
            EraConfigKey::SaveNos => self.save_nos.into(),
            EraConfigKey::FontFamily => self.font_family.clone().into(),
            EraConfigKey::FontSize => self.font_size.into(),
            EraConfigKey::LineHeight => self.line_height.into(),
        }
    }

    pub fn from_text(s: &str) -> ParserResult<Self> {
        let mut ret = Self::default();

        let mut lex = Lexer::new(s);

        while let Some(line) = lex.next() {
            match line {
                ConfigToken::Line((key, value)) => {
                    if let Ok(key) = key.parse() {
                        match key {
                            EraConfigKey::PrintcCount => {
                                ret.printc_count = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid integer {value}"))
                                    }
                                };
                            }
                            EraConfigKey::MaxLog => {
                                ret.max_log = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid integer {value}"))
                                    }
                                };
                            }
                            EraConfigKey::PrintcWidth => {
                                ret.printc_width = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid integer {value}"))
                                    }
                                };
                            }
                            EraConfigKey::Lang => {
                                ret.lang = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid language {value}"))
                                    }
                                };
                            }
                            EraConfigKey::SaveNos => {
                                ret.save_nos = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid save_nos {value}"))
                                    }
                                };
                            }
                            EraConfigKey::FontFamily => {
                                ret.font_family = value.into();
                            }
                            EraConfigKey::FontSize => {
                                ret.font_size = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid font_size {value}"))
                                    }
                                };
                            }
                            EraConfigKey::LineHeight => {
                                ret.line_height = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid line_height {value}"))
                                    }
                                };
                            }
                        }
                    }
                }
                ConfigToken::Error => error!(lex.span(), format!("Invalid token: {}", lex.slice())),
            }
        }

        Ok(ret)
    }
}

#[derive(Clone, Copy, Debug, EnumString, Display, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
pub struct DefaultLocalVarSize {
    pub default_local_size: Option<u32>,
    pub default_locals_size: Option<u32>,
    pub default_arg_size: Option<u32>,
    pub default_args_size: Option<u32>,
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

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct HeaderInfo {
    pub macros: HashMap<String, String>,
    pub gamebase: Gamebase,
    pub rename: HashMap<StrKey, String>,
    pub replace: ReplaceInfo,
    pub character_templates: HashMap<i64, CharacterTemplate>,
    pub item_price: HashMap<u32, u32>,
    pub var_names: HashMap<StrKey, HashMap<StrKey, u32>>,
    pub var_name_var: HashMap<StrKey, BTreeMap<u32, StrKey>>,
    pub global_variables: HashMap<StrKey, VariableInfo>,
    pub default_local_size: DefaultLocalVarSize,
}

impl HeaderInfo {
    pub fn const_eval_log_error(&self, expr: &Expr) -> Value {
        match self.const_eval(expr) {
            Ok(v) => v,
            Err(err) => {
                log::error!("Const evaluation failed for expr {expr:?}: {err}");
                Value::Int(0)
            }
        }
    }

    pub fn const_eval<'e>(&self, expr: &'e Expr) -> anyhow::Result<Value> {
        match expr {
            Expr::Int(i) => Ok(Value::Int(*i)),
            Expr::String(s) => Ok(Value::String(s.to_string())),
            Expr::FormText(form) => {
                use std::fmt::Write;

                let mut s = String::new();

                write!(s, "{}", form.first).unwrap();

                for (arg, text) in form.other.iter() {
                    // TODO: padding
                    match self.const_eval(&arg.expr)? {
                        Value::Int(expr) => write!(s, "{expr}{text}").unwrap(),
                        Value::String(expr) => write!(s, "{expr}{text}").unwrap(),
                    }
                }

                Ok(s.into())
            }
            Expr::UnaryopExpr(expr, op) => match op {
                UnaryOperator::Minus => match self.const_eval(expr)? {
                    Value::Int(i) => Ok(Value::Int(-i)),
                    _ => bail!("Minus operator can only used for Int value"),
                },
                UnaryOperator::Not => match self.const_eval(expr)? {
                    Value::Int(i) => Ok(Value::Int(!i)),
                    _ => bail!("Not operator can only used for Int value"),
                },
            },
            Expr::BinopExpr(lhs, op, rhs) => {
                let lhs = self.const_eval(lhs)?;
                let rhs = self.const_eval(rhs)?;
                let ret = match op {
                    BinaryOperator::Add => match lhs {
                        Value::Int(i) => Value::Int(i + rhs.try_into_int()?),
                        Value::String(s) => Value::String(s + rhs.into_str().as_str()),
                    },
                    BinaryOperator::Mul => match lhs {
                        Value::Int(i) => Value::Int(i * rhs.try_into_int()?),
                        Value::String(s) => {
                            Value::String(s.repeat(usize::try_from(rhs.try_into_int()?)?))
                        }
                    },
                    BinaryOperator::Sub => Value::Int(lhs.try_into_int()? - rhs.try_into_int()?),
                    BinaryOperator::Div => Value::Int(lhs.try_into_int()? / rhs.try_into_int()?),
                    BinaryOperator::Rem => Value::Int(lhs.try_into_int()? % rhs.try_into_int()?),
                    BinaryOperator::Less => {
                        Value::Int((lhs.try_into_int()? < rhs.try_into_int()?).into())
                    }
                    BinaryOperator::LessOrEqual => {
                        Value::Int((lhs.try_into_int()? <= rhs.try_into_int()?).into())
                    }
                    BinaryOperator::Greater => {
                        Value::Int((lhs.try_into_int()? > rhs.try_into_int()?).into())
                    }
                    BinaryOperator::GreaterOrEqual => {
                        Value::Int((lhs.try_into_int()? >= rhs.try_into_int()?).into())
                    }
                    BinaryOperator::Equal => Value::Int(i64::from(lhs == rhs)),
                    BinaryOperator::NotEqual => Value::Int(i64::from(lhs != rhs)),
                    BinaryOperator::And => Value::Int(i64::from(lhs.as_bool() && rhs.as_bool())),
                    BinaryOperator::Or => Value::Int(i64::from(lhs.as_bool() || rhs.as_bool())),
                    BinaryOperator::Xor => Value::Int(i64::from(lhs.as_bool() ^ rhs.as_bool())),
                    BinaryOperator::BitAnd => Value::Int(lhs.try_into_int()? & rhs.try_into_int()?),
                    BinaryOperator::BitOr => Value::Int(lhs.try_into_int()? | rhs.try_into_int()?),
                    BinaryOperator::BitXor => Value::Int(lhs.try_into_int()? ^ rhs.try_into_int()?),
                    BinaryOperator::Lhs => Value::Int(lhs.try_into_int()? << rhs.try_into_int()?),
                    BinaryOperator::Rhs => Value::Int(lhs.try_into_int()? >> rhs.try_into_int()?),
                };

                Ok(ret)
            }
            Expr::Var(var) => {
                if let Some(var_info) = match var.func_extern {
                    Some(_func) => {
                        log::warn!("TODO: local const");
                        bail!("TODO local const");
                    }
                    None => self.global_variables.get(&var.var),
                } {
                    if var_info.is_const {
                        let Some(init) = var_info.init.get(0) else { bail!("No value"); };
                        let init = self.const_eval(init)?;
                        Ok(init)
                    } else {
                        bail!("Variable {} is not const", var.var);
                    }
                } else {
                    bail!("Variable {} is not exists", var.var);
                }
            }
            Expr::BuiltinMethod(meth, args) => match meth {
                BuiltinMethod::Unicode => {
                    let arg = args
                        .first()
                        .and_then(|v| v.as_ref())
                        .context("No argument for UNICODE method")?;
                    let arg = self
                        .const_eval(arg)?
                        .try_into_int()
                        .context("Invalid argument for UNICODE method")?;
                    Ok(Value::String(
                        char::from_u32(arg as u32).context("Invalid unicode")?.to_string(),
                    ))
                }
                _ => bail!("Method {meth} can't be used in const context"),
            },
            _ => bail!("Can't be used in const context"),
        }
    }

    pub fn merge_rename_csv(&mut self, s: &str) -> ParserResult<()> {
        let interner = get_interner();

        for (mut line, _) in csv::lines(s) {
            if let Some((value, key)) = line.next_tuple() {
                self.rename.insert(interner.get_or_intern(key), value.into());
            }
        }

        Ok(())
    }

    pub fn merge_chara_csv(&mut self, s: &str) -> ParserResult<()> {
        let interner = get_interner();
        let mut template = CharacterTemplate::default();

        macro_rules! define_keys {
            ($(
                $name:ident = $str:expr;
            )+) => {
                $(
                    let $name = interner.get_or_intern_static($str);
                )+
            };
        }

        define_keys! {
            cstr = "CSTR";
            talent = "TALENT";
            base = "BASE";
            mark = "MARK";
            abl = "ABL";
            exp = "EXP";
            relation = "RELATION";
            equip = "EQUIP";
            cflag = "CFLAG";
        }

        macro_rules! insert_template {
            ($var:ident, $val1:expr, $val2:expr, $span:expr) => {{
                let idx = match $val1.parse::<u32>() {
                    Ok(idx) => idx,
                    Err(_) => {
                        match self
                            .var_names
                            .get(&$var)
                            .and_then(|names| names.get(&interner.get_or_intern($val1)))
                            .copied()
                        {
                            Some(idx) => idx,
                            _ => error_csv!("알수없는 플래그 인덱스입니다.", $span),
                        }
                    }
                };

                let value = match $val2.parse::<i64>() {
                    Ok(idx) => idx,
                    _ if $val2.is_empty() => 0,
                    _ => error_csv!("잘못된 숫자입니다.", $span),
                };

                template.$var.insert(idx, value);
            }};
            (@bool $var:ident, $val1:expr, $val2:expr, $span:expr) => {{
                let idx = match $val1.parse::<u32>() {
                    Ok(idx) => idx,
                    Err(_) => {
                        match self
                            .var_names
                            .get(&$var)
                            .and_then(|names| names.get(&interner.get_or_intern($val1)))
                            .copied()
                        {
                            Some(idx) => idx,
                            _ => error_csv!("잘못된 숫자입니다.", $span),
                        }
                    }
                };
                template.$var.insert(idx, 1);
            }};
            (@str $var:ident, $val1:expr, $val2:expr, $span:expr) => {{
                let idx = match $val1.parse::<u32>() {
                    Ok(idx) => idx,
                    Err(_) => {
                        match self
                            .var_names
                            .get(&$var)
                            .and_then(|names| names.get(&interner.get_or_intern($val1)))
                            .copied()
                        {
                            Some(idx) => idx,
                            _ => error_csv!("잘못된 숫자입니다.", $span),
                        }
                    }
                };
                template.$var.insert(idx, $val2.into());
            }};
        }

        for (mut line, span) in csv::lines(s) {
            if let Some((name, val1)) = line.next_tuple() {
                let val2 = line.next().unwrap_or("");

                match name {
                    "番号" => template.no = val1.parse().unwrap(),
                    "名前" => template.name = val1.into(),
                    "主人の呼び方" => template.master_name = val1.into(),
                    "呼び名" => template.call_name = val1.into(),
                    "あだ名" => template.nick_name = val1.into(),
                    "助手" => template.is_assi = val1.trim() == "1",

                    "CSTR" => insert_template!(@str cstr, val1, val2, span),

                    "素質" => insert_template!(@bool talent, val1, val2, span),

                    "基礎" => insert_template!(base, val1, val2, span),
                    "刻印" => insert_template!(mark, val1, val2, span),
                    "能力" => insert_template!(abl, val1, val2, span),
                    "経験" => insert_template!(exp, val1, val2, span),
                    "相性" => insert_template!(relation, val1, val2, span),
                    "装着物" => insert_template!(equip, val1, val2, span),
                    "フラグ" => insert_template!(cflag, val1, val2, span),
                    other => log::warn!("Unknown character template name: {other}"),
                }
            }
        }

        self.character_templates.insert(template.no, template);

        Ok(())
    }

    pub fn merge_gamebase_csv(&mut self, s: &str) -> ParserResult<()> {
        for (mut line, span) in csv::lines(s) {
            if let Some((name, val)) = line.next_tuple() {
                match name {
                    "コード" => {
                        self.gamebase.code = csv_parse_int!(val, span);
                    }
                    "バージョン" => {
                        self.gamebase.version = csv_parse_int!(val, span);
                    }
                    "バージョン違い認める" => {
                        self.gamebase.allow_version = csv_parse_int!(val, span);
                    }
                    "最初からいるキャラ" => {
                        self.gamebase.default_chara = csv_parse_int!(val, span);
                    }
                    "アイテムなし" => {
                        self.gamebase.no_item = csv_parse_int!(val, span);
                    }
                    "作者" => self.gamebase.author = val.into(),
                    "追加情報" => self.gamebase.info = val.into(),
                    "製作年" => self.gamebase.year = val.into(),
                    "タイトル" => self.gamebase.title = val.into(),
                    other => log::error!("Unknown gamebase key: {other}"),
                }
            }
        }

        Ok(())
    }

    pub fn merge_name_csv(&mut self, var: &str, s: &str) -> ParserResult<()> {
        let interner = get_interner();
        let var = interner.get_or_intern(var);
        let mut name_var = BTreeMap::new();

        for (mut line, span) in csv::lines(s) {
            if let Some((n, s)) = line.next_tuple() {
                let n = csv_parse_int!(n, span);
                let s = interner.get_or_intern(s);
                self.var_names.entry(var).or_default().insert(s, n);
                name_var.insert(n, s);
            }
        }

        self.var_name_var.insert(var, name_var);

        Ok(())
    }

    pub fn merge_item_csv(&mut self, s: &str) -> ParserResult<()> {
        let interner = get_interner();
        let var = interner.get_or_intern_static("ITEM");

        for (mut line, span) in csv::lines(s) {
            if let Some((idx, name)) = line.next_tuple() {
                let idx = match idx.parse() {
                    Ok(idx) => idx,
                    _ => error_csv!("Invalid digit", span),
                };
                let name = interner.get_or_intern(name);
                let price = line.next().and_then(|s| s.parse().ok()).unwrap_or(0);

                self.item_price.insert(idx, price);
                self.var_names.entry(var).or_default().insert(name, idx);
                self.var_name_var.entry(var).or_default().insert(idx, name);
            }
        }

        Ok(())
    }

    pub fn merge_variable_size_csv(&mut self, s: &str) -> ParserResult<()> {
        let interner = get_interner();

        for (mut line, _) in csv::lines(s) {
            if let Some(name) = line.next() {
                macro_rules! next {
                    () => {
                        line.next().and_then(|s| s.parse().ok())
                    };
                }

                match name {
                    "ARG" => {
                        self.default_local_size.default_arg_size = next!();
                    }
                    "ARGS" => {
                        self.default_local_size.default_args_size = next!();
                    }
                    "LOCAL" => {
                        self.default_local_size.default_local_size = next!();
                    }
                    "LOCALS" => {
                        self.default_local_size.default_locals_size = next!();
                    }
                    // just ignore
                    "ITEMNAME" | "ABLNAME" | "TALENTNAME" | "EXPNAME" | "MARKNAME"
                    | "PALAMNAME" | "TRAINNAME" | "BASENAME" | "SOURCENAME" | "EXNAME"
                    | "EQUIPNAME" | "TEQUIPNAME" | "FLAGNAME" | "TFLAGNAME" | "CFLAGNAME" => {}
                    name => {
                        let name_key = interner.get_or_intern(name);
                        let mut sizes: Vec<u32> = Vec::with_capacity(4);

                        let mut forbidden = false;

                        for part in line {
                            match part.parse::<u32>() {
                                Ok(n) => sizes.push(n),
                                _ => {
                                    forbidden = true;
                                    log::info!("Don't use {name}");
                                    self.global_variables.remove(&name_key);
                                    break;
                                }
                            }
                        }

                        if !forbidden {
                            match self.global_variables.get_mut(&name_key) {
                                Some(info) => {
                                    let info_len = info.size.len();
                                    if info.size.len() != sizes.len() {
                                        log::error!("Variable size for {name} is not matched! Expected: {info_len} Actual: {size_len}", size_len = sizes.len());
                                    } else {
                                        info.size.copy_from_slice(&sizes[..info_len]);
                                    }
                                }
                                None => {
                                    log::warn!(
                                        "Variable {name} is not exists but defined in variablesize.csv"
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub fn merge_replace_csv(&mut self, s: &str) -> ParserResult<()> {
        for (mut line, _span) in csv::lines(s) {
            if let Some((k, v)) = line.next_tuple() {
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
        }

        Ok(())
    }

    pub fn merge_header(&mut self, s: &str) -> ParserResult<()> {
        let ctx = ParserContext::default();
        let mut pp = Preprocessor::new(&PP_REGEX, s);
        let mut b = Bump::new();

        loop {
            match pp.next_line(&b)? {
                Some(EraLine::SharpLine {
                    sharp: SharpCode::DEFINE,
                    args,
                }) => {
                    let (args, ident) = try_nom!(pp, self::expr::ident(args));
                    self.macros.insert(ident.to_string(), args.trim().to_string());
                }
                Some(EraLine::SharpLine {
                    sharp: SharpCode::DIM,
                    args: dim,
                }) => {
                    let var = try_nom!(pp, self::expr::dim_line(&ctx, false)(dim)).1;
                    self.global_variables.insert(var.var, var.info);
                }
                Some(EraLine::SharpLine {
                    sharp: SharpCode::DIMS,
                    args: dims,
                }) => {
                    let var = try_nom!(pp, self::expr::dim_line(&ctx, true)(dims)).1;
                    self.global_variables.insert(var.var, var.info);
                }
                Some(_) => error!(pp.span(), "Invalid line"),
                None => break,
            }

            b.reset();
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct ParserContext {
    pub interner: &'static Interner,
    pub locals_key: StrKey,
    pub args_key: StrKey,
    pub header: Arc<HeaderInfo>,
    pub local_strs: RefCell<HashSet<StrKey>>,
    pub is_arg: Cell<bool>,
    pub ban_percent: Cell<bool>,
    pub file_path: StrKey,
}

impl Default for ParserContext {
    fn default() -> Self {
        Self::new(Arc::default(), StrKey::new("DEFAULT.ERB"))
    }
}

impl ParserContext {
    pub fn new(header: Arc<HeaderInfo>, file_path: StrKey) -> Self {
        let interner = get_interner();
        Self {
            interner,
            locals_key: interner.get_or_intern_static("LOCALS"),
            args_key: interner.get_or_intern_static("ARGS"),
            header,
            file_path,
            local_strs: RefCell::default(),
            is_arg: Cell::new(false),
            ban_percent: Cell::new(false),
        }
    }

    pub fn is_str_var(&self, key: StrKey) -> bool {
        if key == self.locals_key || key == self.args_key || self.local_strs.borrow().contains(&key)
        {
            true
        } else if let Some(v) = self.header.global_variables.get(&key) {
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

    fn read_body_until(
        &self,
        end: InstructionCode,
        pp: &mut Preprocessor,
        b: &Bump,
    ) -> ParserResult<Vec<StmtWithPos>> {
        let mut out = Vec::new();

        loop {
            match pp.next_line(b)? {
                Some(EraLine::InstLine { inst, args: _ }) if inst == end => {
                    break Ok(out);
                }
                Some(line) => {
                    out.push(self.parse_stmt(line, pp, b)?);
                }
                None => {
                    error!(pp.span(), format!("Block doesn't end with {end}"));
                }
            }
        }
    }

    fn read_body_until_and_expr(
        &self,
        end: InstructionCode,
        pp: &mut Preprocessor,
        b: &Bump,
    ) -> ParserResult<(Expr, Vec<StmtWithPos>)> {
        let mut out = Vec::new();

        loop {
            match pp.next_line(b)? {
                Some(EraLine::InstLine { inst, args }) if inst == end => {
                    break Ok((try_nom!(pp, self::expr::expr(self)(args)).1, out));
                }
                Some(line) => {
                    out.push(self.parse_stmt(line, pp, b)?);
                }
                None => {
                    error!(pp.span(), format!("Block doesn't end with {end}"));
                }
            }
        }
    }

    pub fn parse_stmt(
        &self,
        line: EraLine,
        pp: &mut Preprocessor,
        b: &Bump,
    ) -> ParserResult<StmtWithPos> {
        let pos = pp.script_pos();
        let stmt = match line {
            EraLine::GotoLine(line) => Stmt::Label(self.interner.get_or_intern(line.trim())),
            EraLine::PrintLine {
                flags,
                ty,
                args: form,
            } => match ty {
                PrintType::Plain => Stmt::Print(flags, Expr::str(&self.interner, form)),
                PrintType::Data => {
                    let form = form.trim();
                    let cond = if form.is_empty() {
                        None
                    } else {
                        Some(try_nom!(pp, self::expr::expr(self)(form)).1)
                    };
                    let mut list = Vec::new();

                    loop {
                        match pp.next_line(b)? {
                            Some(EraLine::InstLine {
                                inst: InstructionCode::DATA,
                                args,
                            }) => list.push(vec![Expr::str(&self.interner, args)]),
                            Some(EraLine::InstLine {
                                inst: InstructionCode::DATAFORM,
                                args,
                            }) => list.push(vec![
                                try_nom!(pp, self::expr::normal_form_str(self)(args)).1,
                            ]),
                            Some(EraLine::InstLine {
                                inst: InstructionCode::DATALIST,
                                args: _,
                            }) => {
                                let mut cur_list = Vec::new();
                                loop {
                                    match pp.next_line(b)? {
                                        Some(EraLine::InstLine {
                                            inst: InstructionCode::DATA,
                                            args,
                                        }) => cur_list.push(Expr::str(&self.interner, args)),
                                        Some(EraLine::InstLine {
                                            inst: InstructionCode::DATAFORM,
                                            args,
                                        }) => cur_list.push(
                                            try_nom!(pp, self::expr::normal_form_str(self)(args)).1,
                                        ),
                                        Some(EraLine::InstLine {
                                            inst: InstructionCode::ENDLIST,
                                            args: _,
                                        }) => break,
                                        Some(_) => {
                                            error!(
                                                pp.span(),
                                                "DATALIST에 잘못된 토큰이 들어왔습니다"
                                            )
                                        }
                                        None => {
                                            error!(pp.span(), "ENDLIST없이 DATALIST가 끝났습니다.")
                                        }
                                    }
                                }
                                list.push(cur_list);
                            }
                            Some(EraLine::InstLine {
                                inst: InstructionCode::ENDDATA,
                                args: _,
                            }) => break,
                            Some(_) => error!(pp.span(), "PRINTDATA에 잘못된 토큰이 들어왔습니다"),
                            None => error!(pp.span(), "ENDDATA없이 PRINTDATA가 끝났습니다."),
                        }
                    }

                    Stmt::PrintData(flags, cond, list)
                }
                PrintType::Form => {
                    let (_, form) = try_nom!(pp, self::expr::normal_form_str(self)(form));
                    Stmt::Print(flags, form)
                }
                PrintType::FormS => {
                    let (_, s) = try_nom!(pp, self::expr::expr(self)(form));
                    Stmt::PrintFormS(flags, s)
                }
                PrintType::S => {
                    let (_, s) = try_nom!(pp, self::expr::expr(self)(form));
                    Stmt::Print(flags, s)
                }
                PrintType::V => {
                    let (_, s) = try_nom!(pp, self::expr::expr_or_blank_list(self)(form));
                    Stmt::PrintList(flags, s)
                }
            },
            EraLine::InstLine { inst, args } => {
                use erars_lexer::InstructionCode::*;

                macro_rules! normal_command {
                    ($com:expr) => {{
                        let args = try_nom!(pp, self::expr::expr_list(self)(args)).1;
                        Stmt::Command($com, args)
                    }};
                }
                macro_rules! normal_method {
                    ($meth:expr) => {{
                        let args = try_nom!(pp, self::expr::expr_list(self)(args)).1;
                        Stmt::Method($meth, args)
                    }};
                }
                macro_rules! strform_command {
                    ($com:expr) => {{
                        let args = try_nom!(pp, self::expr::normal_form_str(self)(args)).1;
                        Stmt::Command($com, vec![Some(args)])
                    }};
                }
                macro_rules! strform_method {
                    ($meth:expr) => {{
                        let args = try_nom!(pp, self::expr::normal_form_str(self)(args)).1;
                        Stmt::Method($meth, vec![Some(args)])
                    }};
                }
                match inst {
                    PRINT => unreachable!(),
                    PRINTBUTTON | PRINTBUTTONC | PRINTBUTTONLC => {
                        let flags = if inst == PRINTBUTTON {
                            PrintFlags::empty()
                        } else if inst == PRINTBUTTONC {
                            PrintFlags::RIGHT_ALIGN
                        } else {
                            PrintFlags::LEFT_ALIGN
                        };
                        let (text, value) = try_nom!(pp, self::expr::expr_pair(self)(args)).1;
                        Stmt::PrintButton { flags, text, value }
                    }
                    PRINTPLAINFORM => Stmt::Print(
                        PrintFlags::PLAIN,
                        try_nom!(pp, self::expr::normal_form_str(self)(args)).1,
                    ),
                    PRINTPLAIN => Stmt::Print(PrintFlags::PLAIN, Expr::str(self.interner, args)),
                    DEBUGPRINT | DEBUGPRINTL | DEBUGPRINTFORM | DEBUGPRINTFORML | DEBUGCLEAR => {
                        // TODO: debug
                        Stmt::Print(PrintFlags::empty(), Expr::str(self.interner, ""))
                    }
                    DRAWLINE => Stmt::Command(BuiltinCommand::DrawLine, vec![]),
                    DRAWLINEFORM => strform_command!(BuiltinCommand::CustomDrawLine),
                    CUSTOMDRAWLINE => Stmt::Command(
                        BuiltinCommand::CustomDrawLine,
                        vec![Some(Expr::str(self.interner, args.trim_start()))],
                    ),
                    ALIGNMENT => match args.trim().parse() {
                        Ok(align) => Stmt::Alignment(align),
                        Err(_) => error!(pp.span(), "Invalid alignment"),
                    },
                    BEGIN => match args.trim().parse() {
                        Ok(ty) => Stmt::Begin(ty),
                        Err(_) => error!(pp.span(), "Invalid alignment"),
                    },
                    TIMES => try_nom!(pp, self::expr::times_line(self)(args)).1,

                    RETURN => normal_command!(BuiltinCommand::Return),
                    RETURNF => normal_command!(BuiltinCommand::ReturnF),
                    RETURNFORM => try_nom!(pp, self::expr::returnform_line(self)(args)).1,
                    RESTART => normal_command!(BuiltinCommand::Restart),
                    CONTINUE => Stmt::Continue,
                    BREAK => Stmt::Break,
                    QUIT => Stmt::Command(BuiltinCommand::Quit, Vec::new()),

                    CSVNAME => normal_method!(BuiltinMethod::CsvName),
                    CSVCALLNAME => normal_method!(BuiltinMethod::CsvCallName),
                    CSVMASTERNAME => normal_method!(BuiltinMethod::CsvMasterName),
                    CSVNICKNAME => normal_method!(BuiltinMethod::CsvNickName),
                    CSVBASE => normal_method!(BuiltinMethod::CsvBase),
                    CSVCSTR => normal_method!(BuiltinMethod::CsvCstr),
                    CSVABL => normal_method!(BuiltinMethod::CsvAbl),
                    CSVTALENT => normal_method!(BuiltinMethod::CsvTalent),
                    CSVMARK => normal_method!(BuiltinMethod::CsvMark),
                    CSVEXP => normal_method!(BuiltinMethod::CsvExp),
                    CSVEX => normal_method!(BuiltinMethod::CsvEx),
                    CSVRELATION => normal_method!(BuiltinMethod::CsvRelation),
                    CSVJUEL => normal_method!(BuiltinMethod::CsvJuel),
                    CSVEQUIP => normal_method!(BuiltinMethod::CsvEquip),
                    CSVCFLAG => normal_method!(BuiltinMethod::CsvCflag),

                    GETBIT => normal_method!(BuiltinMethod::GetBit),
                    SETBIT => normal_command!(BuiltinCommand::SetBit),
                    INVERTBIT => normal_command!(BuiltinCommand::InvertBit),
                    CLEARBIT => normal_command!(BuiltinCommand::ClearBit),

                    CLEARLINE => normal_command!(BuiltinCommand::ClearLine),
                    INPUT => normal_command!(BuiltinCommand::Input),
                    INPUTS => normal_command!(BuiltinCommand::InputS),
                    TINPUT => normal_command!(BuiltinCommand::TInput),
                    TINPUTS => normal_command!(BuiltinCommand::TInputS),
                    WAIT => normal_command!(BuiltinCommand::Wait),
                    WAITANYKEY => normal_command!(BuiltinCommand::WaitAnykey),
                    TWAIT => normal_command!(BuiltinCommand::Twait),
                    ONEINPUT => normal_command!(BuiltinCommand::OneInput),
                    ONEINPUTS => normal_command!(BuiltinCommand::OneInputS),
                    TONEINPUT => normal_command!(BuiltinCommand::TOneInput),
                    TONEINPUTS => normal_command!(BuiltinCommand::TOneInputS),
                    FORCEWAIT => normal_command!(BuiltinCommand::ForceWait),
                    RESETDATA => normal_command!(BuiltinCommand::ResetData),
                    RESET_STAIN => normal_command!(BuiltinCommand::ResetStain),
                    ADDCHARA => normal_command!(BuiltinCommand::AddChara),
                    ADDDEFCHARA => normal_command!(BuiltinCommand::AddDefChara),
                    ADDCOPYCHARA => normal_command!(BuiltinCommand::AddCopyChara),
                    GETCHARA => normal_method!(BuiltinMethod::GetChara),
                    FINDCHARA => normal_method!(BuiltinMethod::FindChara),
                    // FINDLASTCHARA => normal_method!(BuiltinMethod::FindLastChara),
                    FINDCHARADATA => normal_method!(BuiltinMethod::FindCharaData),
                    DELCHARA => normal_command!(BuiltinCommand::DelChara),
                    SORTCHARA => try_nom!(pp, self::expr::sortchara_line(self)(args)).1,
                    COPYCHARA => normal_command!(BuiltinCommand::CopyChara),
                    SWAPCHARA => normal_command!(BuiltinCommand::SwapChara),
                    PICKUPCHARA => normal_command!(BuiltinCommand::PickupChara),
                    FONTBOLD => normal_command!(BuiltinCommand::FontBold),
                    FONTITALIC => normal_command!(BuiltinCommand::FontItalic),
                    FONTREGULAR => normal_command!(BuiltinCommand::FontRegular),
                    FONTSTYLE => normal_command!(BuiltinCommand::FontStyle),
                    SETFONT => normal_command!(BuiltinCommand::SetFont),
                    CHKFONT => normal_method!(BuiltinMethod::ChkFont),
                    GETFONT => normal_method!(BuiltinMethod::GetFont),

                    REDRAW => normal_method!(BuiltinMethod::CurrentRedraw),
                    THROW => strform_command!(BuiltinCommand::Throw),

                    SETCOLOR => normal_command!(BuiltinCommand::SetColor),
                    SETCOLORBYNAME => strform_command!(BuiltinCommand::SetColorByName),
                    SETBGCOLOR => normal_command!(BuiltinCommand::SetBgColor),
                    SETBGCOLORBYNAME => strform_command!(BuiltinCommand::SetBgColorByName),
                    GETCOLOR => normal_method!(BuiltinMethod::GetColor),
                    GETDEFCOLOR => normal_method!(BuiltinMethod::GetDefColor),
                    GETBGCOLOR => normal_method!(BuiltinMethod::GetBgColor),
                    GETDEFBGCOLOR => normal_method!(BuiltinMethod::GetDefBgColor),
                    GETFOCUSCOLOR => normal_method!(BuiltinMethod::GetFocusColor),
                    RESETCOLOR => normal_command!(BuiltinCommand::ResetColor),
                    RESETBGCOLOR => normal_command!(BuiltinCommand::ResetBgColor),

                    STRLEN => normal_method!(BuiltinMethod::StrLenS),
                    STRLENS => normal_method!(BuiltinMethod::StrLenS),
                    STRLENSU => normal_method!(BuiltinMethod::StrLenSU),
                    STRLENFORM => strform_method!(BuiltinMethod::StrLenS),
                    STRLENFORMU => strform_method!(BuiltinMethod::StrLenSU),
                    UNICODE => normal_method!(BuiltinMethod::Unicode),
                    ENCODETOUNI => strform_command!(BuiltinCommand::EncodeToUni),
                    REPLACE => normal_method!(BuiltinMethod::Replace),
                    ESCAPE => normal_method!(BuiltinMethod::Escape),

                    SPLIT => normal_command!(BuiltinCommand::Split),
                    SWAP => normal_command!(BuiltinCommand::Swap),
                    SAVEGLOBAL => normal_command!(BuiltinCommand::SaveGlobal),
                    LOADGLOBAL => normal_command!(BuiltinCommand::LoadGlobal),
                    POWER => normal_command!(BuiltinCommand::Power),
                    MIN => normal_method!(BuiltinMethod::Min),
                    MAX => normal_method!(BuiltinMethod::Max),
                    LIMIT => normal_method!(BuiltinMethod::Limit),
                    ABS => normal_method!(BuiltinMethod::Abs),
                    LOG => normal_method!(BuiltinMethod::Log),
                    LOG10 => normal_method!(BuiltinMethod::Log10),
                    SQRT => normal_method!(BuiltinMethod::Sqrt),
                    SIGN => normal_method!(BuiltinMethod::Sign),

                    SAVEGAME => normal_command!(BuiltinCommand::SaveGame),
                    SAVEDATA => normal_command!(BuiltinCommand::SaveData),
                    LOADDATA => normal_command!(BuiltinCommand::LoadData),
                    CHKDATA => normal_method!(BuiltinMethod::ChkData),
                    DELDATA => normal_command!(BuiltinCommand::DelData),
                    GETTIME => normal_command!(BuiltinCommand::GetTime),
                    GETSECOND => normal_method!(BuiltinMethod::GetSecond),
                    GETMILLISECOND => normal_method!(BuiltinMethod::GetMillisecond),
                    PUTFORM => strform_command!(BuiltinCommand::PutForm),

                    VARSET => normal_command!(BuiltinCommand::Varset),
                    CVARSET => normal_command!(BuiltinCommand::CVarset),
                    VARSIZE => normal_method!(BuiltinMethod::VarSize),
                    SUBSTRING => normal_method!(BuiltinMethod::SubString),
                    SUBSTRINGU => normal_method!(BuiltinMethod::SubStringU),
                    ARRAYCOPY => normal_command!(BuiltinCommand::ArrayCopy),
                    ARRAYREMOVE => normal_command!(BuiltinCommand::ArrayRemove),
                    ARRAYMOVE => normal_command!(BuiltinCommand::ArrayMove),
                    ARRAYSHIFT => normal_command!(BuiltinCommand::ArrayShift),
                    ARRAYSORT => try_nom!(pp, self::expr::arraysort_line(self)(args)).1,
                    GETNUM => normal_method!(BuiltinMethod::GetNum),
                    GETEXPLV => normal_method!(BuiltinMethod::GetExpLv),
                    GETPALAMLV => normal_method!(BuiltinMethod::GetPalamLv),
                    GETCONFIG => normal_method!(BuiltinMethod::GetConfig),

                    BAR => normal_command!(BuiltinCommand::Bar),
                    SKIPDISP => normal_command!(BuiltinCommand::SkipDisp),

                    HTML_PRINT => normal_command!(BuiltinCommand::HtmlPrint),

                    REUSELASTLINE => Stmt::ReuseLastLine(self.interner.get_or_intern(args)),

                    CALL | JUMP | CALLFORM | JUMPFORM | CALLF | CALLFORMF | TRYCALL
                    | TRYCALLFORM | TRYJUMP | TRYJUMPFORM | TRYCCALL | TRYCCALLFORM | TRYCJUMP
                    | TRYCJUMPFORM | GOTO | GOTOFORM | TRYGOTO | TRYGOTOFORM | TRYCGOTO
                    | TRYCGOTOFORM => {
                        let (name, args) = try_nom!(
                            pp,
                            self::expr::call_jump_line(
                                self,
                                matches!(
                                    inst,
                                    CALLFORM
                                        | CALLFORMF
                                        | JUMPFORM
                                        | TRYCALLFORM
                                        | TRYCCALLFORM
                                        | TRYJUMPFORM
                                        | TRYCJUMPFORM
                                        | GOTOFORM
                                        | TRYGOTOFORM
                                        | TRYCGOTOFORM
                                )
                            )(args)
                        )
                        .1;

                        let is_try = matches!(
                            inst,
                            TRYCALL
                                | TRYCALLFORM
                                | TRYCCALLFORM
                                | TRYJUMP
                                | TRYJUMPFORM
                                | TRYCJUMPFORM
                                | TRYGOTO
                                | TRYCGOTO
                                | TRYGOTOFORM
                                | TRYCGOTOFORM
                        );

                        let is_catch = matches!(
                            inst,
                            TRYCCALL
                                | TRYCCALLFORM
                                | TRYCJUMP
                                | TRYCJUMPFORM
                                | TRYCGOTO
                                | TRYCGOTOFORM
                        );

                        let (try_body, catch_body) = if is_catch {
                            let try_body = self.read_body_until(CATCH, pp, b)?;
                            let catch_body = self.read_body_until(ENDCATCH, pp, b)?;

                            (try_body, Some(catch_body))
                        } else if is_try {
                            (Vec::new(), Some(Vec::new()))
                        } else {
                            (Vec::new(), None)
                        };

                        if matches!(
                            inst,
                            GOTO | GOTOFORM | TRYGOTO | TRYGOTOFORM | TRYCGOTO | TRYCGOTOFORM
                        ) {
                            Stmt::Goto {
                                label: name,
                                catch_body,
                            }
                        } else {
                            Stmt::Call {
                                name,
                                args,
                                is_jump: matches!(
                                    inst,
                                    JUMP | JUMPFORM
                                        | TRYJUMP
                                        | TRYJUMPFORM
                                        | TRYCJUMP
                                        | TRYCJUMPFORM
                                ),
                                is_method: matches!(inst, CALLF | CALLFORMF),
                                try_body,
                                catch_body,
                            }
                        }
                    }

                    SIF => {
                        let cond = try_nom!(pp, self::expr::expr(self)(args)).1;
                        let Some(body) = pp.next_line(b)? else { error!(pp.span(), "No body statement in SIF"); };
                        Stmt::Sif(cond, Box::new(self.parse_stmt(body, pp, b)?))
                    }

                    IF => {
                        let mut is_else = false;
                        let mut cond_pos = pp.script_pos();
                        let mut cond = try_nom!(pp, self::expr::expr_or_one(self)(args)).1;
                        let mut block = Vec::new();
                        let mut if_elses = Vec::new();

                        loop {
                            match pp.next_line(b)? {
                                Some(EraLine::InstLine { inst: ELSEIF, args }) => {
                                    if_elses.push((ExprWithPos(cond, cond_pos), block));
                                    block = Vec::new();
                                    cond_pos = pp.script_pos();
                                    cond = try_nom!(pp, self::expr::expr_or_one(self)(args)).1;
                                }
                                Some(EraLine::InstLine {
                                    inst: ELSE,
                                    args: _,
                                }) => {
                                    is_else = true;
                                    if_elses.push((ExprWithPos(cond, cond_pos), block));
                                    cond_pos = pp.script_pos();
                                    cond = Expr::Int(1);
                                    block = Vec::new();
                                }
                                Some(EraLine::InstLine {
                                    inst: ENDIF,
                                    args: _,
                                }) => {
                                    if !is_else {
                                        if_elses.push((ExprWithPos(cond, cond_pos), block));
                                        block = Vec::new();
                                    }
                                    break;
                                }
                                Some(line) => {
                                    block.push(self.parse_stmt(line, pp, b)?);
                                }
                                None => break,
                            }
                        }

                        Stmt::If(if_elses, block)
                    }

                    SELECTCASE => {
                        let cond = try_nom!(pp, self::expr::expr(self)(args)).1;
                        let mut has_else = false;
                        let mut body = Vec::new();
                        let mut cases: Vec<(_, Vec<StmtWithPos>)> = Vec::new();

                        loop {
                            match pp.next_line(b)? {
                                Some(EraLine::InstLine { inst: CASE, args }) => {
                                    if let Some((_, case)) = cases.last_mut() {
                                        *case = mem::take(&mut body);
                                    }
                                    let case = try_nom!(pp, self::expr::case_line(self)(args)).1;
                                    cases.push((case, Vec::new()));
                                }
                                Some(EraLine::InstLine {
                                    inst: CASEELSE,
                                    args: _,
                                }) => {
                                    if let Some((_, case)) = cases.last_mut() {
                                        *case = mem::take(&mut body);
                                    }
                                    has_else = true;
                                }
                                Some(EraLine::InstLine {
                                    inst: ENDSELECT,
                                    args: _,
                                }) => break,
                                Some(line) => {
                                    body.push(self.parse_stmt(line, pp, b)?);
                                }
                                None => error!(pp.span(), "Unexpected EOF after SELECTCASE"),
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

                    FOR => {
                        let (var, arg1, arg2, arg3) =
                            try_nom!(pp, self::expr::for_line(self)(args)).1;

                        let body = self.read_body_until(NEXT, pp, b)?;

                        Stmt::For(var, Box::new((arg1, arg2, arg3)), body)
                    }
                    REPEAT => {
                        let expr = try_nom!(pp, self::expr::expr(self)(args)).1;
                        let body = self.read_body_until(REND, pp, b)?;

                        Stmt::Repeat(expr, body)
                    }
                    WHILE => {
                        let cond = try_nom!(pp, self::expr::expr(self)(args)).1;

                        let body = self.read_body_until(WEND, pp, b)?;

                        Stmt::While(cond, body)
                    }
                    DO => {
                        let (cond, body) = self.read_body_until_and_expr(LOOP, pp, b)?;

                        Stmt::Do(cond, body)
                    }

                    // TODO
                    inst => {
                        log::warn!("{inst} is not yet implemented this line will occur error when executed.");
                        Stmt::Command(
                            BuiltinCommand::Throw,
                            vec![Some(Expr::str(
                                self.interner,
                                format!("[compiler] TODO: {inst}"),
                            ))],
                        )
                    }
                }
            }
            EraLine::VarAssign {
                lhs,
                complex_op,
                rhs,
            } => {
                let var = try_nom!(pp, self::expr::variable(self)(lhs)).1;

                match complex_op {
                    Some(ComplexAssign::Bin(bin_op)) => {
                        let rhs = try_nom!(pp, self::expr::expr(self)(rhs)).1;
                        Stmt::Assign(var, Some(bin_op), rhs)
                    }
                    Some(ComplexAssign::Str) => {
                        let rhs = try_nom!(pp, self::expr::expr(self)(rhs)).1;
                        Stmt::Assign(var, None, rhs)
                    }
                    None => {
                        let rhs = if self.is_str_var(var.var) {
                            try_nom!(pp, self::expr::form_arg_expr(self)(rhs)).1
                        } else {
                            try_nom!(pp, self::expr::expr(self)(rhs)).1
                        };

                        Stmt::Assign(var, None, rhs)
                    }
                }
            }
            EraLine::VarInc {
                lhs,
                is_pre: _,
                is_inc,
            } => {
                let lhs = try_nom!(pp, self::expr::variable(self)(lhs)).1;

                Stmt::Assign(
                    lhs,
                    Some(if is_inc {
                        BinaryOperator::Add
                    } else {
                        BinaryOperator::Sub
                    }),
                    Expr::Int(1),
                )
            }
            EraLine::SharpLine { .. } | EraLine::FunctionLine(_) => {
                error!(
                    pp.span(),
                    format!("Invalid line `{line:?}` for parsing as statement")
                )
            }
        };

        Ok(StmtWithPos(stmt, pos))
    }

    fn push_info(
        &self,
        sharp: SharpCode,
        args: &str,
        pp: &mut Preprocessor,
        infos: &mut Vec<FunctionInfo>,
    ) -> ParserResult<()> {
        match sharp {
            SharpCode::DEFINE => {
                error!(pp.span(), "#DEFINE only avaliable in ERH")
            }
            SharpCode::DIM => {
                let var = try_nom!(pp, self::expr::dim_line(self, false)(args)).1;
                infos.push(FunctionInfo::Dim(var));
            }
            SharpCode::DIMS => {
                let var = try_nom!(pp, self::expr::dim_line(self, true)(args)).1;
                self.local_strs.borrow_mut().insert(var.var);
                infos.push(FunctionInfo::Dim(var));
            }
            SharpCode::FUNCTION => {
                infos.push(FunctionInfo::Function);
            }
            SharpCode::FUNCTIONS => {
                infos.push(FunctionInfo::FunctionS);
            }
            SharpCode::LOCALSIZE => {
                let size = try_nom!(pp, self::expr::expr(self)(args)).1;
                infos.push(FunctionInfo::LocalSize(size));
            }
            SharpCode::LOCALSSIZE => {
                let size = try_nom!(pp, self::expr::expr(self)(args)).1;
                infos.push(FunctionInfo::LocalSSize(size));
            }
            SharpCode::PRI => infos.push(FunctionInfo::EventFlag(EventFlags::Pre)),
            SharpCode::LATER => infos.push(FunctionInfo::EventFlag(EventFlags::Later)),
            SharpCode::SINGLE => infos.push(FunctionInfo::EventFlag(EventFlags::Single)),
        }
        Ok(())
    }

    pub fn parse_and_compile<'s>(
        &self,
        pp: &mut Preprocessor<'s>,
        b: &mut Bump,
    ) -> ParserResult<Vec<CompiledFunction>> {
        let s = pp.left_text();
        let mut out = Vec::with_capacity(1024);

        match pp.next_line(b)? {
            Some(EraLine::FunctionLine(mut func_line)) => 'outer: loop {
                self.local_strs.borrow_mut().clear();
                let mut compiler = Compiler::new();
                let (label, args) = try_nom!(pp, self::expr::function_line(self)(func_line)).1;
                let label = self.interner.get_or_intern(label);

                let mut infos = Vec::new();

                'inner: loop {
                    b.reset();
                    match pp.next_line(b)? {
                        Some(EraLine::FunctionLine(f)) => {
                            func_line = f;

                            out.push(CompiledFunction {
                                header: FunctionHeader {
                                    file_path: self.file_path,
                                    name: label,
                                    args,
                                    infos,
                                },
                                goto_labels: compiler.goto_labels,
                                body: compiler.out.into_boxed_slice(),
                            });

                            break 'inner;
                        }
                        None => {
                            out.push(CompiledFunction {
                                header: FunctionHeader {
                                    file_path: self.file_path,
                                    name: label,
                                    args,
                                    infos,
                                },
                                goto_labels: compiler.goto_labels,
                                body: compiler.out.into_boxed_slice(),
                            });
                            break 'outer;
                        }
                        Some(EraLine::SharpLine { sharp, args }) => {
                            self.push_info(sharp, args, pp, &mut infos)?;
                        }
                        Some(line) => {
                            let stmt = self.parse_stmt(line, pp, b)?;
                            if let Err(err) = compiler.push_stmt_with_pos(stmt) {
                                let line =
                                    s.lines().nth(compiler.current_pos().line as usize).unwrap();
                                let diff = line.as_ptr() as usize - s.as_ptr() as usize;
                                error!(diff..diff + line.len(), err.to_string());
                            }
                        }
                    }
                }
            },
            Some(_) => {
                error!(pp.span(), "First line should be function line");
            }
            None => {
                return Ok(Vec::new());
            }
        };

        Ok(out)
    }

    pub fn parse(&self, pp: &mut Preprocessor, b: &mut Bump) -> ParserResult<Vec<Function>> {
        let mut out = Vec::new();

        match pp.next_line(b)? {
            Some(EraLine::FunctionLine(mut func_line)) => 'outer: loop {
                self.local_strs.borrow_mut().clear();
                let mut body = Vec::new();
                let (label, args) = try_nom!(pp, self::expr::function_line(self)(func_line)).1;
                let label = self.interner.get_or_intern(label);

                let mut infos = Vec::new();

                'inner: loop {
                    b.reset();
                    match pp.next_line(b)? {
                        Some(EraLine::FunctionLine(f)) => {
                            func_line = f;

                            out.push(Function {
                                header: FunctionHeader {
                                    file_path: self.file_path,
                                    name: label,
                                    args,
                                    infos,
                                },
                                body,
                            });

                            break 'inner;
                        }
                        None => {
                            out.push(Function {
                                header: FunctionHeader {
                                    file_path: self.file_path,
                                    name: label,
                                    args,
                                    infos,
                                },
                                body,
                            });
                            break 'outer;
                        }
                        Some(EraLine::SharpLine { sharp, args }) => {
                            self.push_info(sharp, args, pp, &mut infos)?;
                        }
                        Some(line) => {
                            let stmt = self.parse_stmt(line, pp, b)?;
                            body.push(stmt);
                        }
                    }
                }
            },
            Some(_) => {
                error!(pp.span(), "First line should be function line");
            }
            None => {
                return Ok(Vec::new());
            }
        };

        Ok(out)
    }
}

impl ParserContext {
    pub fn parse_program_str(&self, s: &str) -> ParserResult<Vec<Function>> {
        let mut pp = Preprocessor::new(&crate::PP_REGEX, s);
        let mut b = Bump::new();
        self.parse(&mut pp, &mut b)
    }

    pub fn parse_function_str(&self, s: &str) -> ParserResult<Function> {
        self.parse_program_str(s).map(|f| f.into_iter().next().unwrap())
    }

    pub fn parse_expr_str(&self, s: &str) -> ParserResult<Expr> {
        Ok(try_nom!(@str s, self::expr::expr(self)(s)).1)
    }

    pub fn parse_body_str(&self, s: &str) -> ParserResult<Vec<StmtWithPos>> {
        let mut pp = Preprocessor::new(&crate::PP_REGEX, s);
        let mut b = Bump::new();
        let mut body = Vec::new();

        while let Some(line) = pp.next_line(&b)? {
            body.push(self.parse_stmt(line, &mut pp, &b)?);
            b.reset();
        }

        Ok(body)
    }

    pub fn parse_stmt_str(&self, s: &str) -> ParserResult<StmtWithPos> {
        let mut pp = Preprocessor::new(&crate::PP_REGEX, s);
        let b = Bump::new();

        if let Some(line) = pp.next_line(&b)? {
            self.parse_stmt(line, &mut pp, &b)
        } else {
            error!(pp.span(), "No stmt")
        }
    }
}
