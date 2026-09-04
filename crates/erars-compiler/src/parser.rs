mod csv;
mod expr;

use anyhow::{bail, Context};
use erars_ast::{
    get_interner, BinaryOperator, BuiltinCommand, BuiltinMethod, EventFlags, EventType, Expr,
    ExprWithPos, Function, FunctionHeader, FunctionInfo, Interner, LocalVariable, PrintFlags,
    ScriptPosition, Stmt, StmtWithPos, StrKey, UnaryOperator, Value, Variable, VariableInfo,
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
    collections::{BTreeMap, VecDeque},
    mem,
    sync::Arc,
};
use strum::{Display, EnumString};

pub use crate::error::{ParserError, ParserResult, ParserWarning};
use crate::{compiler::Compiler, CompiledErb, CompiledFunction};
pub use expr::{normal_form_str, DimDecl};

macro_rules! error_csv {
    ($msg:expr, $span:expr) => {{
        return Err((String::from($msg), $span));
    }};
}

/// Emuera parses every CSV number with `Int64`/`Int32.TryParse`, which fails
/// unless the *whole* token is a number (`GameData/ConstantData.cs:1416`,
/// `:1726`, `GameData/GameBase.cs:67`). `csv::lines` has already trimmed the
/// field, so `all_consuming` is exactly that predicate — without it a name
/// like `3人目` or a price like `124:comment` would silently read as `3`/`124`
/// instead of being rejected.
fn try_parse_csv_int64(s: &str) -> Option<i64> {
    nom::combinator::all_consuming(nom::character::complete::i64::<_, nom::error::Error<_>>)(s)
        .ok()
        .map(|(_, n)| n)
}

fn try_parse_csv_int(s: &str) -> Option<u32> {
    nom::combinator::all_consuming(nom::character::complete::u32::<_, nom::error::Error<_>>)(s)
        .ok()
        .map(|(_, n)| n)
}

macro_rules! csv_parse_int {
    ($s:expr, $span:expr) => {
        match try_parse_csv_int($s) {
            Some(n) => n,
            None => error_csv!("Invalid number", $span.clone()),
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

/// `#`-directive diagnostics, one function per `_Library/EvilMask/Lang.cs`
/// entry. Korean like the rest of erars' messages, with the Japanese original
/// quoted so the C# line stays greppable (`crates/erars-compiler/src/error.rs:12-24`).
///
/// Where Emuera has three or four identical sentences differing only in the
/// directive name, this has one function taking the flag: the `Lang.cs` lines
/// of every one of them are cited together.
mod sharp_msg {
    use erars_ast::EventFlags;
    use erars_lexer::SharpCode;

    /// The directive that sets `flag`. `EventFlags::None` is never a directive
    /// — it is the absence of one — so it cannot reach any message here.
    fn flag(flag: EventFlags) -> &'static str {
        match flag {
            EventFlags::Pre => "PRI",
            EventFlags::Later => "LATER",
            EventFlags::Single => "SINGLE",
            EventFlags::Only => "ONLY",
            EventFlags::None => "",
        }
    }

    /// `LOCALSIZE` sizes `LOCAL`, `LOCALSSIZE` sizes `LOCALS`: Emuera cuts the
    /// last four characters off the directive (`LogicalLineParser.cs:210`).
    fn sized_var(sharp: SharpCode) -> &'static str {
        match sharp {
            SharpCode::LOCALSSIZE => "LOCALS",
            _ => "LOCAL",
        }
    }

    /// 「式中関数では#～～は機能しません」 (`Lang.cs:840`, `:844`, `:849`, `:853`).
    pub fn use_user_func(f: EventFlags) -> String {
        format!("식중 함수에서 #{} 지정은 동작하지 않습니다", flag(f))
    }

    /// 「イベント関数以外では#～～は機能しません」 (`Lang.cs:841`, `:845`, `:850`, `:854`).
    pub fn usable_event_func(f: EventFlags) -> String {
        format!("이벤트 함수 이외에서 #{} 지정은 동작하지 않습니다", flag(f))
    }

    /// 「#～～が重複して使われています」 (`Lang.cs:842`, `:846`, `:851`, `:855`).
    pub fn duplicate_flag(f: EventFlags) -> String {
        format!("#{} 지정이 중복됐습니다", flag(f))
    }

    /// 「#ONLYが指定されたイベント関数では#～～は機能しません」
    /// (`Lang.cs:843`, `:847`, `:852`).
    pub fn only_with(f: EventFlags) -> String {
        format!(
            "#ONLY가 지정된 이벤트 함수에서 #{} 지정은 동작하지 않습니다",
            flag(f)
        )
    }

    /// 「このイベント関数には#～～が宣言されていますが無視されます」
    /// (`Lang.cs:857-859`).
    pub fn be_ignore(f: EventFlags) -> String {
        format!("이 이벤트 함수의 #{} 선언은 무시됩니다", flag(f))
    }

    /// 「#PRIと#LATERが重複して使われています(この関数は2度呼ばれます)」
    /// (`Lang.cs:848`).
    pub fn pri_with_later() -> String {
        "#PRI와 #LATER가 중복 지정됐습니다(Emuera에서는 이 함수가 두 번 호출됩니다)".into()
    }

    /// 「関数"{0}"にはすでに#{1}が宣言されています(この行は無視されます)」
    /// (`Lang.cs:862`).
    pub fn already_sharp_declared(label: &str, sharp: SharpCode) -> String {
        format!("함수 \"{label}\"에는 이미 #{sharp} 선언이 있습니다(이 행은 무시됩니다)")
    }

    /// 「関数"{0}"にはすでに#FUNCTION(S)が宣言されています」 (`Lang.cs:863`, `:864`).
    pub fn already_declared_sharp_function(label: &str, is_str: bool) -> String {
        let declared = if is_str { "FUNCTIONS" } else { "FUNCTION" };
        format!("함수 \"{label}\"에는 이미 #{declared} 선언이 있습니다")
    }

    /// 「システム関数に#{0}が指定されています」 (`Lang.cs:865`).
    pub fn use_sharp_in_system_func(sharp: SharpCode) -> String {
        format!("시스템 함수에 #{sharp} 지정이 있습니다")
    }

    /// 「"#{0}"属性は関数名が数字で始まる関数には指定できません」 (`Lang.cs:860`).
    pub fn can_not_declared_begin_number_function(sharp: SharpCode) -> String {
        format!("#{sharp} 속성은 함수명이 숫자로 시작하는 함수에는 지정할 수 없습니다")
    }

    /// 「関数名が数字で始まっています」 (`Lang.cs:861`) — the function's own
    /// error message, not a warning.
    pub fn func_name_begin_number() -> String {
        "함수명이 숫자로 시작합니다".into()
    }

    /// 「#{0}の後に有効な数値が指定されていません」 (`Lang.cs:866`).
    pub fn sharp_has_not_valid_value(sharp: SharpCode) -> String {
        format!("#{sharp} 뒤에 유효한 수치가 지정되지 않았습니다")
    }

    /// 「イベント関数では#{0}による{1}のサイズ指定は無視されます」 (`Lang.cs:867`).
    pub fn event_func_ignore_specified(sharp: SharpCode) -> String {
        format!(
            "이벤트 함수에서는 #{sharp}에 의한 {} 크기 지정이 무시됩니다",
            sized_var(sharp)
        )
    }

    /// 「#{0}に0以下の値({1})が与えられました。設定は無視されます」 (`Lang.cs:868`).
    pub fn localsize_less_than_1(sharp: SharpCode, size: i64) -> String {
        format!("#{sharp}에 0 이하의 값({size})이 지정됐습니다. 설정은 무시됩니다")
    }

    /// 「#{0}に大きすぎる値({1})が与えられました。設定は無視されます」 (`Lang.cs:869`).
    pub fn too_many_localsize(sharp: SharpCode, size: i64) -> String {
        format!("#{sharp}에 너무 큰 값({size})이 지정됐습니다. 설정은 무시됩니다")
    }

    /// 「この関数にはすでに#LOCALSIZE(SSIZE)が定義されています。（以前の定義は無視されます）」
    /// (`Lang.cs:871`, `:872`).
    pub fn duplicate_localsize(sharp: SharpCode) -> String {
        format!("이 함수에는 이미 #{sharp} 정의가 있습니다(이전 정의는 무시됩니다)")
    }
}

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
    /// `システムメニュー0` — Emuera `TitleMenuString0` (`Config/ConfigData.cs:193`).
    /// Stored bare: the default title screen prepends `[0] ` at print time
    /// (`GameProc/Process.SystemProc.cs:208`).
    pub system_menu0: String,
    /// `システムメニュー1` — Emuera `TitleMenuString1` (`Config/ConfigData.cs:194`).
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
            // Emuera `単位の位置` defaults to 前 (`ConfigData.cs:187`).
            unit_forward: true,
            start_message: "Now Loading...".into(),
            sell_item_count: 100,
            drawline_str: "-".into(),
            bar_str1: "*".into(),
            bar_str2: ".".into(),
            system_menu0: "最初からはじめる".into(),
            system_menu1: "ロードしてはじめる".into(),
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
    /// `ウィンドウタイトル` — Emuera `ScriptWindowTitle`
    /// (`GameData/GameBase.cs:30`, set at `:147-149`), shown as the console's
    /// window title (`GameProc/Process.cs:144` `console.SetWindowTitle`).
    ///
    /// Already resolved: Emuera applies its absent-key fallback at the end of
    /// the same load (`GameData/GameBase.cs:184-190`), so this is never the
    /// bare CSV value when the key is missing.
    pub window_title: String,
}

impl Gamebase {
    /// Emuera `ScriptVersionText` (`GameData/GameBase.cs:31-44`): the version
    /// integer split at the thousands digit, with the remainder printed to
    /// three places when it ends in a non-zero digit and to two otherwise, so
    /// `1234` reads `1.234` and `1230` reads `1.23`.
    pub fn version_text(&self) -> String {
        let (major, rem) = (self.version / 1000, self.version % 1000);
        if self.version % 10 != 0 {
            format!("{major}.{rem:03}")
        } else {
            format!("{major}.{:02}", rem / 10)
        }
    }
}

#[derive(Clone, Copy, Debug, Display, EnumString, strum::EnumIter)]
#[strum(use_phf)]
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

    #[strum(to_string = "ウィンドウ幅")]
    WindowWidth,

    #[strum(to_string = "ウィンドウ高さ")]
    WindowHeight,

    /// Emuera `ForeColor` — default text colour, `r,g,b`.
    #[strum(to_string = "文字色")]
    ForeColor,

    /// Emuera `BackColor` — console background, `r,g,b`.
    #[strum(to_string = "背景色")]
    BgColor,

    /// Emuera `FocusColor` — hovered-button text colour, `r,g,b`.
    #[strum(to_string = "選択中文字色")]
    FocusColor,

    /// Emuera `CompatiSPChara` — when off, SP characters do not exist and
    /// `ADDSPCHARA` is an error. Off by default, as in Emuera.
    #[strum(to_string = "SPキャラを使用する")]
    UseSpChara,

    /// Emuera `TextDrawingMode` (`Config/ConfigData.cs:62`) — which Win32 text
    /// backend the console draws with. erars has its own renderer and no use
    /// for the value, but `GETCONFIGS` must still report it: eraMegaten asks
    /// for it to decide whether image compositing is available at all
    /// (`ERB/関数/組み込み関数/画像関連関数/01_画像取り込み.ERB:35`).
    #[strum(to_string = "描画インターフェース")]
    TextDrawingMode,

    // ─── Behavioural keys (`Config/ConfigData.cs:52-127`) ───
    //
    // Emuera reads every one of these from `emuera.config`; §5 of
    // `docs/research/2026-09-03-emuera-command-gap.md` records, key by key,
    // which ones erars acts on and what it does instead where it cannot.

    /// Emuera `IgnoreCase` (`ConfigData.cs:52`).
    #[strum(to_string = "大文字小文字の違いを無視する")]
    IgnoreCase,
    /// Emuera `UseRenameFile` (`ConfigData.cs:53`).
    #[strum(to_string = "_Rename.csvを利用する")]
    UseRenameFile,
    /// Emuera `UseReplaceFile` (`ConfigData.cs:54`).
    #[strum(to_string = "_Replace.csvを利用する")]
    UseReplaceFile,
    /// Emuera `UseDebugCommand` (`ConfigData.cs:57`).
    #[strum(to_string = "デバッグコマンドを使用する")]
    UseDebugCommand,
    /// Emuera `AutoSave` (`ConfigData.cs:59`).
    #[strum(to_string = "オートセーブを行なう")]
    AutoSave,
    /// Emuera `UseKeyMacro` (`ConfigData.cs:60`).
    #[strum(to_string = "キーボードマクロを使用する")]
    UseKeyMacro,
    /// Emuera `InfiniteLoopAlertTime` (`ConfigData.cs:83`).
    #[strum(to_string = "無限ループ警告までのミリ秒数")]
    InfiniteLoopAlertTime,
    /// Emuera `DisplayWarningLevel` (`ConfigData.cs:84`): a warning whose
    /// level is below this is never shown (`GameData/ParserMediator.cs:26`).
    #[strum(to_string = "表示する最低警告レベル")]
    DisplayWarningLevel,
    /// Emuera `DisplayReport` (`ConfigData.cs:85`).
    #[strum(to_string = "ロード時にレポートを表示する")]
    DisplayReport,
    /// Emuera `ReduceArgumentOnLoad` (`ConfigData.cs:86`).
    #[strum(to_string = "ロード時に引数を解析する")]
    ReduceArgumentOnLoad,
    /// Emuera `IgnoreUncalledFunction` (`ConfigData.cs:88`).
    #[strum(to_string = "呼び出されなかった関数を無視する")]
    IgnoreUncalledFunction,
    /// Emuera `FunctionNotFoundWarning` (`ConfigData.cs:89`).
    #[strum(to_string = "関数が見つからない警告の扱い")]
    FunctionNotFoundWarning,
    /// Emuera `FunctionNotCalledWarning` (`ConfigData.cs:90`).
    #[strum(to_string = "関数が呼び出されなかった警告の扱い")]
    FunctionNotCalledWarning,
    /// Emuera `ButtonWrap` (`ConfigData.cs:94`).
    #[strum(to_string = "ボタンの途中で行を折りかえさない")]
    ButtonWrap,
    /// Emuera `SearchSubdirectory` (`ConfigData.cs:95`).
    #[strum(to_string = "サブディレクトリを検索する")]
    SearchSubdirectory,
    /// Emuera `SortWithFilename` (`ConfigData.cs:96`).
    #[strum(to_string = "読み込み順をファイル名順にソートする")]
    SortWithFilename,
    /// Emuera `WarnBackCompatibility` (`ConfigData.cs:99`).
    #[strum(to_string = "eramaker互換性に関する警告を表示する")]
    WarnBackCompatibility,
    /// Emuera `AllowFunctionOverloading` (`ConfigData.cs:100`).
    #[strum(to_string = "システム関数の上書きを許可する")]
    AllowFunctionOverloading,
    /// Emuera `WarnFunctionOverloading` (`ConfigData.cs:101`).
    #[strum(to_string = "システム関数が上書きされたとき警告を表示する")]
    WarnFunctionOverloading,
    /// Emuera `WarnNormalFunctionOverloading` (`ConfigData.cs:105`).
    #[strum(to_string = "同名の非イベント関数が複数定義されたとき警告する")]
    WarnNormalFunctionOverloading,
    /// Emuera `CompatiErrorLine` (`ConfigData.cs:106`).
    #[strum(to_string = "解釈不可能な行があっても実行する")]
    CompatiErrorLine,
    /// Emuera `CompatiCALLNAME` (`ConfigData.cs:107`).
    #[strum(to_string = "CALLNAMEが空文字列の時にNAMEを代入する")]
    CompatiCallName,
    /// Emuera `UseSaveFolder` (`ConfigData.cs:108`).
    #[strum(to_string = "セーブデータをsavフォルダ内に作成する")]
    UseSaveFolder,
    /// Emuera `CompatiRAND` (`ConfigData.cs:109`).
    #[strum(to_string = "擬似変数RANDの仕様をeramakerに合わせる")]
    CompatiRand,
    /// Emuera `CompatiFunctionNoignoreCase` (`ConfigData.cs:111`).
    #[strum(to_string = "関数・属性については大文字小文字を無視しない")]
    CompatiFunctionNoIgnoreCase,
    /// Emuera `SystemAllowFullSpace` (`ConfigData.cs:112`).
    #[strum(to_string = "全角スペースをホワイトスペースに含める")]
    SystemAllowFullSpace,
    /// Emuera `SystemSaveInUTF8` (`ConfigData.cs:113`).
    #[strum(to_string = "セーブデータをUTF-8で保存する")]
    SystemSaveInUtf8,
    /// Emuera `CompatiLinefeedAs1739` (`ConfigData.cs:114`).
    #[strum(to_string = "ver1739以前の非ボタン折り返しを再現する")]
    CompatiLinefeedAs1739,
    /// Emuera `AllowLongInputByMouse` (`ConfigData.cs:116`).
    #[strum(to_string = "ONEINPUT系命令でマウスによる2文字以上の入力を許可する")]
    AllowLongInputByMouse,
    /// Emuera `CompatiCallEvent` (`ConfigData.cs:117`): with it on, an event
    /// function is also reachable by `CALL` (`GameProc/LabelDictionary.cs:84`).
    #[strum(to_string = "イベント関数のCALLを許可する")]
    CompatiCallEvent,
    /// Emuera `SystemSaveInBinary` (`ConfigData.cs:120`).
    #[strum(to_string = "セーブデータをバイナリ形式で保存する")]
    SystemSaveInBinary,
    /// Emuera `CompatiFuncArgOptional` (`ConfigData.cs:121`).
    #[strum(to_string = "ユーザー関数の全ての引数の省略を許可する")]
    CompatiFuncArgOptional,
    /// Emuera `CompatiFuncArgAutoConvert` (`ConfigData.cs:122`).
    #[strum(to_string = "ユーザー関数の引数に自動的にTOSTRを補完する")]
    CompatiFuncArgAutoConvert,
    /// Emuera `SystemIgnoreTripleSymbol` (`ConfigData.cs:123`).
    #[strum(to_string = "FORM中の三連記号を展開しない")]
    SystemIgnoreTripleSymbol,
    /// Emuera `TimesNotRigorousCalculation` (`ConfigData.cs:124`).
    #[strum(to_string = "TIMESの計算をeramakerにあわせる")]
    TimesNotRigorousCalculation,
    /// Emuera `SystemNoTarget` (`ConfigData.cs:126`).
    #[strum(to_string = "キャラクタ変数の引数を補完しない")]
    SystemNoTarget,
    /// Emuera `SystemIgnoreStringSet` (`ConfigData.cs:127`).
    #[strum(to_string = "文字列変数の代入に文字列式を強制する")]
    SystemIgnoreStringSet,
    /// Emuera `LogColor` (`ConfigData.cs:79`) — the history-pane text colour.
    /// Kept because `GETCONFIG` reports it (`ConfigData.cs:519`).
    #[strum(to_string = "履歴文字色")]
    LogColor,

    // ─── `_replace.csv` items `GETCONFIG` can read ───
    //
    // Emuera's `GetItem` searches the replace array too, so these are
    // readable through `GETCONFIG`/`GETCONFIGS` even though they are set in
    // `_replace.csv` and never in `emuera.config`
    // (`Config/ConfigData.cs:186-201`, `:385-397`, `:497-547`).
    /// Emuera `MoneyFirst` (`ConfigData.cs:187`).
    #[strum(to_string = "単位の位置")]
    MoneyFirst,
    /// Emuera `MaxShopItem` (`ConfigData.cs:189`).
    #[strum(to_string = "販売アイテム数")]
    MaxShopItem,
    /// Emuera `ComAbleDefault` (`ConfigData.cs:195`).
    #[strum(to_string = "COM_ABLE初期値")]
    ComAbleDefault,
    /// Emuera `pbandDef` (`ConfigData.cs:200`).
    #[strum(to_string = "PBANDの初期値")]
    PBandDefault,
    /// Emuera `RelationDef` (`ConfigData.cs:201`).
    #[strum(to_string = "RELATIONの初期値")]
    RelationDefault,
    /// Emuera `MoneyLabel` (`ConfigData.cs:186`).
    #[strum(to_string = "お金の単位")]
    MoneyLabel,
    /// Emuera `LoadLabel` (`ConfigData.cs:188`).
    #[strum(to_string = "起動時簡略表示")]
    LoadLabel,
    /// Emuera `DrawLineString` (`ConfigData.cs:190`).
    #[strum(to_string = "DRAWLINE文字")]
    DrawLineString,
    /// Emuera `BarChar1` (`ConfigData.cs:191`).
    #[strum(to_string = "BAR文字1")]
    BarChar1,
    /// Emuera `BarChar2` (`ConfigData.cs:192`).
    #[strum(to_string = "BAR文字2")]
    BarChar2,
    /// Emuera `TitleMenuString0` (`ConfigData.cs:193`).
    #[strum(to_string = "システムメニュー0")]
    TitleMenuString0,
    /// Emuera `TitleMenuString1` (`ConfigData.cs:194`).
    #[strum(to_string = "システムメニュー1")]
    TitleMenuString1,
    /// Emuera `TimeupLabel` (`ConfigData.cs:197`).
    #[strum(to_string = "時間切れ表示")]
    TimeupLabel,
}

#[derive(Clone, Debug, derivative::Derivative, Serialize, Deserialize)]
#[derivative(Default)]
pub struct EraConfig {
    pub lang: Language,
    /// `表示するセーブデータ数` — Emuera SaveDataNos, whose default is 20.
    #[derivative(Default(value = "20"))]
    pub save_nos: usize,
    /// `SPキャラを使用する` — Emuera CompatiSPChara. When set, a CHARA CSV
    /// entry with a non-zero `フラグ,0` defines an SP character reachable only
    /// through `ADDSPCHARA`, and `ADDCHARA` no longer sees it.
    pub use_sp_chara: bool,
    #[derivative(Default(value = "500"))]
    pub max_log: usize,
    /// `PRINTCを並べる数` — Emuera PrintCPerLine.
    #[derivative(Default(value = "3"))]
    pub printc_count: usize,
    /// `PRINTCの文字数` — Emuera PrintCLength (PRINTLC pads to this + 1).
    #[derivative(Default(value = "25"))]
    pub printc_width: usize,

    /// `フォント名`. Empty means "no configured family": the renderer's
    /// per-language font chain applies, and `SETFONT` without an argument
    /// resets to it.
    pub font_family: String,
    #[derivative(Default(value = "18"))]
    pub font_size: u32,
    #[derivative(Default(value = "19"))]
    pub line_height: u32,

    /// `ウィンドウ幅` — Emuera WindowX.
    #[derivative(Default(value = "760"))]
    pub window_width: u32,
    /// `ウィンドウ高さ` — Emuera WindowY (includes the input strip).
    #[derivative(Default(value = "480"))]
    pub window_height: u32,

    /// `文字色` — Emuera ForeColor.
    #[derivative(Default(value = "[192, 192, 192]"))]
    pub fore_color: [u8; 3],
    /// `背景色` — Emuera BackColor.
    #[derivative(Default(value = "[0, 0, 0]"))]
    pub bg_color: [u8; 3],
    /// `選択中文字色` — Emuera FocusColor.
    #[derivative(Default(value = "[255, 255, 0]"))]
    pub focus_color: [u8; 3],

    /// `描画インターフェース` — Emuera TextDrawingMode.
    pub text_drawing_mode: TextDrawingMode,

    /// `履歴文字色` — Emuera LogColor.
    #[derivative(Default(value = "[192, 192, 192]"))]
    pub log_color: [u8; 3],

    /// `大文字小文字の違いを無視する` — Emuera IgnoreCase.
    #[derivative(Default(value = "true"))]
    pub ignore_case: bool,
    /// `_Rename.csvを利用する` — Emuera UseRenameFile. When off, `_rename.csv`
    /// is not applied even if the file is there.
    pub use_rename_file: bool,
    /// `_Replace.csvを利用する` — Emuera UseReplaceFile.
    #[derivative(Default(value = "true"))]
    pub use_replace_file: bool,
    /// `デバッグコマンドを使用する` — Emuera UseDebugCommand. erars decides
    /// debug mode from `--debug` instead; see §5.12 of the gap document.
    pub use_debug_command: bool,
    /// `オートセーブを行なう` — Emuera AutoSave.
    #[derivative(Default(value = "true"))]
    pub auto_save: bool,
    /// `キーボードマクロを使用する` — Emuera UseKeyMacro.
    #[derivative(Default(value = "true"))]
    pub use_key_macro: bool,
    /// `無限ループ警告までのミリ秒数` — Emuera InfiniteLoopAlertTime.
    #[derivative(Default(value = "5000"))]
    pub infinite_loop_alert_time: u32,
    /// `表示する最低警告レベル` — Emuera DisplayWarningLevel: a load-time
    /// warning below this level is dropped, never printed
    /// (`GameData/ParserMediator.cs:26`).
    #[derivative(Default(value = "1"))]
    pub display_warning_level: u8,
    /// `ロード時にレポートを表示する` — Emuera DisplayReport.
    pub display_report: bool,
    /// `ロード時に引数を解析する` — Emuera ReduceArgumentOnLoad.
    pub reduce_argument_on_load: ReduceArgumentOnLoadFlag,
    /// `呼び出されなかった関数を無視する` — Emuera IgnoreUncalledFunction.
    #[derivative(Default(value = "true"))]
    pub ignore_uncalled_function: bool,
    /// `関数が見つからない警告の扱い` — Emuera FunctionNotFoundWarning.
    pub function_not_found_warning: DisplayWarningFlag,
    /// `関数が呼び出されなかった警告の扱い` — Emuera FunctionNotCalledWarning.
    pub function_not_called_warning: DisplayWarningFlag,
    /// `ボタンの途中で行を折りかえさない` — Emuera ButtonWrap.
    pub button_wrap: bool,
    /// `サブディレクトリを検索する` — Emuera SearchSubdirectory: when off,
    /// only `CSV/` and `ERB/` themselves are read, never their subfolders.
    pub search_subdirectory: bool,
    /// `読み込み順をファイル名順にソートする` — Emuera SortWithFilename.
    pub sort_with_filename: bool,
    /// `eramaker互換性に関する警告を表示する` — Emuera WarnBackCompatibility.
    #[derivative(Default(value = "true"))]
    pub warn_back_compatibility: bool,
    /// `システム関数の上書きを許可する` — Emuera AllowFunctionOverloading.
    #[derivative(Default(value = "true"))]
    pub allow_function_overloading: bool,
    /// `システム関数が上書きされたとき警告を表示する` — Emuera
    /// WarnFunctionOverloading.
    #[derivative(Default(value = "true"))]
    pub warn_function_overloading: bool,
    /// `同名の非イベント関数が複数定義されたとき警告する` — Emuera
    /// WarnNormalFunctionOverloading.
    pub warn_normal_function_overloading: bool,
    /// `解釈不可能な行があっても実行する` — Emuera CompatiErrorLine.
    pub compati_error_line: bool,
    /// `CALLNAMEが空文字列の時にNAMEを代入する` — Emuera CompatiCALLNAME,
    /// applied to the character templates once the CSVs are read
    /// (`GameData/ConstantData.cs:1239-1244`).
    pub compati_callname: bool,
    /// `セーブデータをsavフォルダ内に作成する` — Emuera UseSaveFolder.
    pub use_save_folder: bool,
    /// `擬似変数RANDの仕様をeramakerに合わせる` — Emuera CompatiRAND.
    pub compati_rand: bool,
    /// `関数・属性については大文字小文字を無視しない` — Emuera
    /// CompatiFunctionNoignoreCase.
    pub compati_function_no_ignore_case: bool,
    /// `全角スペースをホワイトスペースに含める` — Emuera SystemAllowFullSpace.
    #[derivative(Default(value = "true"))]
    pub system_allow_full_space: bool,
    /// `セーブデータをUTF-8で保存する` — Emuera SystemSaveInUTF8.
    pub system_save_in_utf8: bool,
    /// `ver1739以前の非ボタン折り返しを再現する` — Emuera
    /// CompatiLinefeedAs1739.
    pub compati_linefeed_as_1739: bool,
    /// `ONEINPUT系命令でマウスによる2文字以上の入力を許可する` — Emuera
    /// AllowLongInputByMouse.
    pub allow_long_input_by_mouse: bool,
    /// `イベント関数のCALLを許可する` — Emuera CompatiCallEvent: an event
    /// function also gets a normal-function entry, so `CALL` finds it
    /// (`GameProc/LabelDictionary.cs:83-84`).
    pub compati_call_event: bool,
    /// `セーブデータをバイナリ形式で保存する` — Emuera SystemSaveInBinary.
    pub system_save_in_binary: bool,
    /// `ユーザー関数の全ての引数の省略を許可する` — Emuera
    /// CompatiFuncArgOptional.
    pub compati_func_arg_optional: bool,
    /// `ユーザー関数の引数に自動的にTOSTRを補完する` — Emuera
    /// CompatiFuncArgAutoConvert.
    pub compati_func_arg_auto_convert: bool,
    /// `FORM中の三連記号を展開しない` — Emuera SystemIgnoreTripleSymbol.
    pub system_ignore_triple_symbol: bool,
    /// `TIMESの計算をeramakerにあわせる` — Emuera
    /// TimesNotRigorousCalculation.
    pub times_not_rigorous_calculation: bool,
    /// `キャラクタ変数の引数を補完しない` — Emuera SystemNoTarget.
    pub system_no_target: bool,
    /// `文字列変数の代入に文字列式を強制する` — Emuera SystemIgnoreStringSet.
    pub system_ignore_string_set: bool,
}

/// Parse an Emuera colour value `r,g,b`: split at `,`, at least three tokens,
/// each trimmed and in 0..=255; extra tokens are ignored
/// (Emuera `ConfigItem.tryStringsToColor`).
fn parse_color(s: &str) -> Option<[u8; 3]> {
    let mut tokens = s.split(',');
    let mut out = [0u8; 3];
    for slot in out.iter_mut() {
        *slot = tokens.next()?.trim().parse::<u8>().ok()?;
    }
    Some(out)
}

/// Does `s` hold `VARS`, in any case, anywhere?
///
/// [`ParserContext::hoist_var_decls`] re-lexes a whole function to find the
/// `VARS` declarations that have to be visible before their own line, so
/// `parse_and_compile` asks this first and skips the second pass for the files
/// — the large majority — that declare no dynamic string local.
///
/// The naive `s.as_bytes().windows(4).any(…)` this replaces compared four
/// bytes at every one of the corpus's 61_843_825 offsets, once per file, and
/// was most of the 6.4% of parse+compile self time charged to
/// `parse_and_compile` itself. `V` is rare in text that is overwhelmingly
/// Korean and Japanese, so seeking to the next one vectorises the scan and
/// leaves only a handful of three-byte compares.
fn contains_vars(s: &str) -> bool {
    let bytes = s.as_bytes();
    let mut at = 0;

    while let Some(off) = memchr::memchr2(b'V', b'v', &bytes[at..]) {
        let pos = at + off;
        match bytes.get(pos + 1..pos + 4) {
            Some(tail) if tail.eq_ignore_ascii_case(b"ARS") => return true,
            // Fewer than three bytes left: no window can match.
            None => return false,
            Some(_) => at = pos + 1,
        }
    }

    false
}

/// `0xRRGGBB`, the form Emuera's `GETCONFIG` returns for colour items
/// (`ConfigData.GetConfigValueInERB`: `((R * 256) + G) * 256 + B`).
fn color_to_int(c: [u8; 3]) -> i64 {
    ((c[0] as i64) << 16) | ((c[1] as i64) << 8) | (c[2] as i64)
}

/// Emuera `ConfigItem.tryStringToBool`: an integer means `!= 0`, otherwise
/// `NO`/`FALSE`/`後` and `YES`/`TRUE`/`前` (case-insensitive).
fn parse_bool(value: &str) -> Option<bool> {
    let value = value.trim();
    if let Ok(i) = value.parse::<i64>() {
        return Some(i != 0);
    }
    if value.eq_ignore_ascii_case("NO") || value.eq_ignore_ascii_case("FALSE") || value == "後" {
        Some(false)
    } else if value.eq_ignore_ascii_case("YES")
        || value.eq_ignore_ascii_case("TRUE")
        || value == "前"
    {
        Some(true)
    } else {
        None
    }
}

/// The parsed colour, or — on an invalid value — a warning and `default`
/// (Emuera aborts loading here; we keep the game runnable).
fn parse_color_or_default(value: &str, key: EraConfigKey, default: [u8; 3]) -> [u8; 3] {
    match parse_color(value) {
        Some(c) => c,
        None => {
            log::warn!("Invalid colour {value:?} for {key} (expected r,g,b); using {default:?}");
            default
        }
    }
}

impl EraConfig {
    /// The value `GETCONFIG`/`GETCONFIGS` reports for `key`.
    ///
    /// `replace` is needed because Emuera's `GetItem` searches the replace
    /// array as well as the config array, so a handful of `_replace.csv`
    /// items are readable from a script (`Config/ConfigData.cs:385-397`,
    /// `:497-547`).
    ///
    /// DELIBERATE: Emuera answers only for a whitelist and errors with
    /// 「{0}は取得できない設定項目です」 for every other item
    /// (`ConfigData.cs:552-556`). erars answers for every key it knows —
    /// a superset, so no Emuera script can tell the difference, and erars
    /// already relied on it for `描画インターフェース`. See §5 of
    /// `docs/research/2026-09-03-emuera-command-gap.md`.
    pub fn get_config(&self, key: EraConfigKey, replace: &ReplaceInfo) -> erars_ast::Value {
        match key {
            EraConfigKey::PrintcCount => self.printc_count.into(),
            EraConfigKey::MaxLog => self.max_log.into(),
            EraConfigKey::PrintcWidth => self.printc_width.into(),
            EraConfigKey::Lang => self.lang.to_string().into(),
            EraConfigKey::SaveNos => self.save_nos.into(),
            EraConfigKey::FontFamily => self.font_family.clone().into(),
            EraConfigKey::FontSize => self.font_size.into(),
            EraConfigKey::LineHeight => self.line_height.into(),
            EraConfigKey::WindowWidth => self.window_width.into(),
            EraConfigKey::WindowHeight => self.window_height.into(),
            EraConfigKey::ForeColor => color_to_int(self.fore_color).into(),
            EraConfigKey::BgColor => color_to_int(self.bg_color).into(),
            EraConfigKey::FocusColor => color_to_int(self.focus_color).into(),
            EraConfigKey::LogColor => color_to_int(self.log_color).into(),
            EraConfigKey::UseSpChara => self.use_sp_chara.into(),
            EraConfigKey::TextDrawingMode => self.text_drawing_mode.to_string().into(),

            EraConfigKey::IgnoreCase => self.ignore_case.into(),
            EraConfigKey::UseRenameFile => self.use_rename_file.into(),
            EraConfigKey::UseReplaceFile => self.use_replace_file.into(),
            EraConfigKey::UseDebugCommand => self.use_debug_command.into(),
            EraConfigKey::AutoSave => self.auto_save.into(),
            EraConfigKey::UseKeyMacro => self.use_key_macro.into(),
            EraConfigKey::InfiniteLoopAlertTime => self.infinite_loop_alert_time.into(),
            EraConfigKey::DisplayWarningLevel => (self.display_warning_level as u32).into(),
            EraConfigKey::DisplayReport => self.display_report.into(),
            EraConfigKey::ReduceArgumentOnLoad => {
                self.reduce_argument_on_load.to_string().into()
            }
            EraConfigKey::IgnoreUncalledFunction => self.ignore_uncalled_function.into(),
            EraConfigKey::FunctionNotFoundWarning => {
                self.function_not_found_warning.to_string().into()
            }
            EraConfigKey::FunctionNotCalledWarning => {
                self.function_not_called_warning.to_string().into()
            }
            EraConfigKey::ButtonWrap => self.button_wrap.into(),
            EraConfigKey::SearchSubdirectory => self.search_subdirectory.into(),
            EraConfigKey::SortWithFilename => self.sort_with_filename.into(),
            EraConfigKey::WarnBackCompatibility => self.warn_back_compatibility.into(),
            EraConfigKey::AllowFunctionOverloading => self.allow_function_overloading.into(),
            EraConfigKey::WarnFunctionOverloading => self.warn_function_overloading.into(),
            EraConfigKey::WarnNormalFunctionOverloading => {
                self.warn_normal_function_overloading.into()
            }
            EraConfigKey::CompatiErrorLine => self.compati_error_line.into(),
            EraConfigKey::CompatiCallName => self.compati_callname.into(),
            EraConfigKey::UseSaveFolder => self.use_save_folder.into(),
            EraConfigKey::CompatiRand => self.compati_rand.into(),
            EraConfigKey::CompatiFunctionNoIgnoreCase => {
                self.compati_function_no_ignore_case.into()
            }
            EraConfigKey::SystemAllowFullSpace => self.system_allow_full_space.into(),
            EraConfigKey::SystemSaveInUtf8 => self.system_save_in_utf8.into(),
            EraConfigKey::CompatiLinefeedAs1739 => self.compati_linefeed_as_1739.into(),
            EraConfigKey::AllowLongInputByMouse => self.allow_long_input_by_mouse.into(),
            EraConfigKey::CompatiCallEvent => self.compati_call_event.into(),
            EraConfigKey::SystemSaveInBinary => self.system_save_in_binary.into(),
            EraConfigKey::CompatiFuncArgOptional => self.compati_func_arg_optional.into(),
            EraConfigKey::CompatiFuncArgAutoConvert => self.compati_func_arg_auto_convert.into(),
            EraConfigKey::SystemIgnoreTripleSymbol => self.system_ignore_triple_symbol.into(),
            EraConfigKey::TimesNotRigorousCalculation => {
                self.times_not_rigorous_calculation.into()
            }
            EraConfigKey::SystemNoTarget => self.system_no_target.into(),
            EraConfigKey::SystemIgnoreStringSet => self.system_ignore_string_set.into(),

            EraConfigKey::MoneyFirst => replace.unit_forward.into(),
            EraConfigKey::MaxShopItem => replace.sell_item_count.into(),
            EraConfigKey::ComAbleDefault => replace.comable_init.into(),
            EraConfigKey::PBandDefault => replace.pband_init.into(),
            EraConfigKey::RelationDefault => replace.relation_init.into(),
            EraConfigKey::MoneyLabel => replace.money_unit.clone().into(),
            EraConfigKey::LoadLabel => replace.start_message.clone().into(),
            EraConfigKey::DrawLineString => replace.drawline_str.clone().into(),
            EraConfigKey::BarChar1 => replace.bar_str1.clone().into(),
            EraConfigKey::BarChar2 => replace.bar_str2.clone().into(),
            EraConfigKey::TitleMenuString0 => replace.system_menu0.clone().into(),
            EraConfigKey::TitleMenuString1 => replace.system_menu1.clone().into(),
            EraConfigKey::TimeupLabel => replace.timeout_message.clone().into(),
        }
    }

    pub fn from_text(s: &str) -> ParserResult<Self> {
        let mut ret = Self::default();
        ret.merge_text(s)?;
        Ok(ret)
    }

    /// Apply one config file onto this config, leaving every key the file does
    /// not mention alone.
    ///
    /// Emuera loads three files onto a single `ConfigData` instance in a fixed
    /// order — `csv/_default.config`, then the user's `emuera.config`, then
    /// `csv/_fixed.config` — so a game can ship both overridable defaults and
    /// settings the user cannot override (`Config/ConfigData.cs:642-664`). The
    /// `fix` flag of the third load only greys the widget out in the config
    /// dialogs (`Forms/ConfigDialog.cs:287-311`,
    /// `Forms/DebugConfigDialog.cs:74-98`) and has no other runtime effect, so
    /// load order alone reproduces it.
    pub fn merge_text(&mut self, s: &str) -> ParserResult<()> {
        let ret = self;

        let mut lex = Lexer::new(s);

        while let Some(line) = lex.next() {
            match line {
                Ok(ConfigToken::Line((key, value))) => {
                    if let Ok(key) = key.parse() {
                        macro_rules! set_bool {
                            ($field:ident) => {{
                                ret.$field = match parse_bool(value) {
                                    Some(b) => b,
                                    None => {
                                        error!(lex.span(), format!("Invalid boolean {value}"))
                                    }
                                };
                            }};
                        }
                        macro_rules! set_int {
                            ($field:ident) => {{
                                ret.$field = match value.parse() {
                                    Ok(v) => v,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid integer {value}"))
                                    }
                                };
                            }};
                        }
                        macro_rules! set_flag {
                            ($field:ident) => {{
                                ret.$field = match value.to_ascii_uppercase().parse() {
                                    Ok(v) => v,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid flag {value}"))
                                    }
                                };
                            }};
                        }
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
                            EraConfigKey::WindowWidth => {
                                ret.window_width = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid window_width {value}"))
                                    }
                                };
                            }
                            EraConfigKey::WindowHeight => {
                                ret.window_height = match value.parse() {
                                    Ok(l) => l,
                                    Err(_) => {
                                        error!(lex.span(), format!("Invalid window_height {value}"))
                                    }
                                };
                            }
                            EraConfigKey::ForeColor => {
                                ret.fore_color = parse_color_or_default(value, key, ret.fore_color);
                            }
                            EraConfigKey::BgColor => {
                                ret.bg_color = parse_color_or_default(value, key, ret.bg_color);
                            }
                            EraConfigKey::FocusColor => {
                                ret.focus_color =
                                    parse_color_or_default(value, key, ret.focus_color);
                            }
                            EraConfigKey::UseSpChara => {
                                ret.use_sp_chara = match parse_bool(value) {
                                    Some(b) => b,
                                    None => {
                                        error!(lex.span(), format!("Invalid boolean {value}"))
                                    }
                                };
                            }
                            EraConfigKey::TextDrawingMode => {
                                ret.text_drawing_mode = match value.parse() {
                                    Ok(m) => m,
                                    Err(_) => error!(
                                        lex.span(),
                                        format!("Invalid drawing interface {value}")
                                    ),
                                };
                            }

                            EraConfigKey::LogColor => {
                                ret.log_color = parse_color_or_default(value, key, ret.log_color);
                            }
                            EraConfigKey::IgnoreCase => set_bool!(ignore_case),
                            EraConfigKey::UseRenameFile => set_bool!(use_rename_file),
                            EraConfigKey::UseReplaceFile => set_bool!(use_replace_file),
                            EraConfigKey::UseDebugCommand => set_bool!(use_debug_command),
                            EraConfigKey::AutoSave => set_bool!(auto_save),
                            EraConfigKey::UseKeyMacro => set_bool!(use_key_macro),
                            EraConfigKey::InfiniteLoopAlertTime => {
                                set_int!(infinite_loop_alert_time)
                            }
                            EraConfigKey::DisplayWarningLevel => set_int!(display_warning_level),
                            EraConfigKey::DisplayReport => set_bool!(display_report),
                            EraConfigKey::ReduceArgumentOnLoad => {
                                set_flag!(reduce_argument_on_load)
                            }
                            EraConfigKey::IgnoreUncalledFunction => {
                                set_bool!(ignore_uncalled_function)
                            }
                            EraConfigKey::FunctionNotFoundWarning => {
                                set_flag!(function_not_found_warning)
                            }
                            EraConfigKey::FunctionNotCalledWarning => {
                                set_flag!(function_not_called_warning)
                            }
                            EraConfigKey::ButtonWrap => set_bool!(button_wrap),
                            EraConfigKey::SearchSubdirectory => set_bool!(search_subdirectory),
                            EraConfigKey::SortWithFilename => set_bool!(sort_with_filename),
                            EraConfigKey::WarnBackCompatibility => {
                                set_bool!(warn_back_compatibility)
                            }
                            EraConfigKey::AllowFunctionOverloading => {
                                set_bool!(allow_function_overloading)
                            }
                            EraConfigKey::WarnFunctionOverloading => {
                                set_bool!(warn_function_overloading)
                            }
                            EraConfigKey::WarnNormalFunctionOverloading => {
                                set_bool!(warn_normal_function_overloading)
                            }
                            EraConfigKey::CompatiErrorLine => set_bool!(compati_error_line),
                            EraConfigKey::CompatiCallName => set_bool!(compati_callname),
                            EraConfigKey::UseSaveFolder => set_bool!(use_save_folder),
                            EraConfigKey::CompatiRand => set_bool!(compati_rand),
                            EraConfigKey::CompatiFunctionNoIgnoreCase => {
                                set_bool!(compati_function_no_ignore_case)
                            }
                            EraConfigKey::SystemAllowFullSpace => {
                                set_bool!(system_allow_full_space)
                            }
                            EraConfigKey::SystemSaveInUtf8 => set_bool!(system_save_in_utf8),
                            EraConfigKey::CompatiLinefeedAs1739 => {
                                set_bool!(compati_linefeed_as_1739)
                            }
                            EraConfigKey::AllowLongInputByMouse => {
                                set_bool!(allow_long_input_by_mouse)
                            }
                            EraConfigKey::CompatiCallEvent => set_bool!(compati_call_event),
                            EraConfigKey::SystemSaveInBinary => set_bool!(system_save_in_binary),
                            EraConfigKey::CompatiFuncArgOptional => {
                                set_bool!(compati_func_arg_optional)
                            }
                            EraConfigKey::CompatiFuncArgAutoConvert => {
                                set_bool!(compati_func_arg_auto_convert)
                            }
                            EraConfigKey::SystemIgnoreTripleSymbol => {
                                set_bool!(system_ignore_triple_symbol)
                            }
                            EraConfigKey::TimesNotRigorousCalculation => {
                                set_bool!(times_not_rigorous_calculation)
                            }
                            EraConfigKey::SystemNoTarget => set_bool!(system_no_target),
                            EraConfigKey::SystemIgnoreStringSet => {
                                set_bool!(system_ignore_string_set)
                            }

                            // `_replace.csv` items. `GETCONFIG` reads them
                            // (`get_config` above), but `emuera.config` is not
                            // where they live, so a line naming one here is
                            // accepted and dropped exactly as Emuera drops it.
                            EraConfigKey::MoneyFirst
                            | EraConfigKey::MaxShopItem
                            | EraConfigKey::ComAbleDefault
                            | EraConfigKey::PBandDefault
                            | EraConfigKey::RelationDefault
                            | EraConfigKey::MoneyLabel
                            | EraConfigKey::LoadLabel
                            | EraConfigKey::DrawLineString
                            | EraConfigKey::BarChar1
                            | EraConfigKey::BarChar2
                            | EraConfigKey::TitleMenuString0
                            | EraConfigKey::TitleMenuString1
                            | EraConfigKey::TimeupLabel => {}
                        }
                    }
                }
                Err(_) => error!(lex.span(), format!("Invalid token: {}", lex.slice())),
            }
        }

        Ok(())
    }
}

/// The game's East-Asian language (`内部で使用する東アジア言語`). It decides the
/// legacy code page Emuera uses for its byte-counting string functions and,
/// through it, the console's cell-width table.
#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumString, Display, Serialize, Deserialize)]
pub enum Language {
    #[strum(to_string = "JAPANESE")]
    Japanese,
    #[strum(to_string = "KOREAN")]
    Korean,
    #[strum(to_string = "CHINESE_HANS")]
    ChineseHans,
    #[strum(to_string = "CHINESE_HANT")]
    ChineseHant,
}

impl Default for Language {
    fn default() -> Self {
        Self::Japanese
    }
}

impl Language {
    /// Emuera's code page for this language — cp932 / cp949 / cp936 / cp950,
    /// i.e. WHATWG Shift_JIS / EUC-KR / GBK / Big5 in encoding_rs.
    pub fn encoding(&self) -> &'static encoding_rs::Encoding {
        match self {
            Language::Japanese => encoding_rs::SHIFT_JIS,
            Language::Korean => encoding_rs::EUC_KR,
            Language::ChineseHans => encoding_rs::GBK,
            Language::ChineseHant => encoding_rs::BIG5,
        }
    }
}

/// Emuera's text-drawing backend (`Config/ConfigCode.cs:23-28`), the value of
/// `描画インターフェース`. `GETCONFIGS` yields the name, because Emuera formats the
/// item with `TextDrawingMode.ToString()` (`Config/ConfigData.cs:549-551`);
/// `GETCONFIG` on it is a type error there and here.
#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumString, Display, Serialize, Deserialize)]
pub enum TextDrawingMode {
    #[strum(to_string = "GRAPHICS")]
    Graphics,
    /// Emuera's default (`Config/ConfigData.cs:62`, `:219`).
    #[strum(to_string = "TEXTRENDERER")]
    TextRenderer,
    #[strum(to_string = "WINAPI")]
    WinApi,
}

impl Default for TextDrawingMode {
    fn default() -> Self {
        Self::TextRenderer
    }
}

/// `ロード時に引数を解析する` — Emuera `ReduceArgumentOnLoadFlag`
/// (`Config/ConfigCode.cs`, item at `Config/ConfigData.cs:86`): whether the
/// loader resolves call arguments as it reads, never (`NO`), only for
/// functions it sees called once (`ONCE`), or always (`YES`).
#[derive(
    Clone, Copy, Debug, Default, PartialEq, Eq, EnumString, Display, Serialize, Deserialize,
)]
pub enum ReduceArgumentOnLoadFlag {
    #[default]
    #[strum(to_string = "NO")]
    No,
    #[strum(to_string = "ONCE")]
    Once,
    #[strum(to_string = "YES")]
    Yes,
}

/// `関数が見つからない警告の扱い` / `関数が呼び出されなかった警告の扱い` — Emuera
/// `DisplayWarningFlag` (items at `Config/ConfigData.cs:89-90`): drop the
/// warning, hold it until the load ends, report the first one only, or report
/// every one.
#[derive(
    Clone, Copy, Debug, Default, PartialEq, Eq, EnumString, Display, Serialize, Deserialize,
)]
pub enum DisplayWarningFlag {
    #[default]
    #[strum(to_string = "IGNORE")]
    Ignore,
    #[strum(to_string = "LATER")]
    Later,
    #[strum(to_string = "ONCE")]
    Once,
    #[strum(to_string = "DISPLAY")]
    Display,
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
    pub rename: HashMap<String, String>,
    pub replace: ReplaceInfo,
    pub str_templates: HashMap<u32, String>,
    /// Templates with a zero (or absent) `フラグ,0`.
    pub character_templates: HashMap<i64, CharacterTemplate>,
    /// Templates with a non-zero `フラグ,0` — Emuera `CharacterTemplate.SetSpFlag`.
    /// Only reachable separately when `EraConfig::use_sp_chara` is set; otherwise
    /// `ADDCHARA` falls back to this map too, matching Emuera's single list.
    #[serde(default)]
    pub character_sp_templates: HashMap<i64, CharacterTemplate>,
    pub item_price: HashMap<u32, u32>,
    pub var_names: HashMap<StrKey, HashMap<StrKey, u32>>,
    pub var_name_var: HashMap<StrKey, BTreeMap<u32, StrKey>>,
    pub global_variables: HashMap<StrKey, VariableInfo>,
    pub default_local_size: DefaultLocalVarSize,
}

/// `const_eval` found no declaration for a name.
///
/// Emuera raises `IdentifierNotFoundCodeEE` for exactly this case
/// (`GameData/IdentifierDictionary.cs:635`, `:663`), and its header loader
/// catches *only* that type in order to re-queue a `#DIM` line
/// (`GameProc/HeaderFileLoader.cs:341-348`): the missing name may still be
/// declared by a `#DIM` line that has not run yet. Every other failure is a
/// plain `CodeEE` and is reported on the spot
/// (`GameProc/HeaderFileLoader.cs:355-359`), so the distinction has to be a
/// type rather than a message, or a genuinely broken declaration would be
/// reported as a dependency failure.
#[derive(Debug, Clone, Copy, thiserror::Error)]
#[error("Variable {0} is not exists")]
pub struct IdentifierNotFound(pub StrKey);

/// A `#DIM`/`#DIMS` line read by [`HeaderInfo::merge_header_defines`] and held
/// back for [`HeaderInfo::resolve_pending_dims`].
#[derive(Debug)]
pub struct PendingDim {
    /// Caller-supplied file id, handed back with any diagnostic so the loader
    /// can name the file the line came from.
    pub file: usize,
    /// `#DIMS` rather than `#DIM`.
    pub is_str: bool,
    /// Byte range of the whole `#DIM` line inside that file.
    pub span: std::ops::Range<usize>,
    /// Everything after `#DIM`/`#DIMS`, verbatim.
    pub args: String,
}

impl HeaderInfo {
    /// The template `ADDCHARA`/`ADDSPCHARA` should use, following Emuera
    /// `ConstantData.GetCharacterTemplate_UseSp`: the SP/normal split only
    /// exists while `CompatiSPChara` is on, so with it off both maps are
    /// searched and `sp` is irrelevant.
    pub fn chara_template(
        &self,
        no: i64,
        sp: bool,
        use_sp_chara: bool,
    ) -> Option<&CharacterTemplate> {
        if use_sp_chara {
            if sp {
                self.character_sp_templates.get(&no)
            } else {
                self.character_templates.get(&no)
            }
        } else {
            self.character_templates
                .get(&no)
                .or_else(|| self.character_sp_templates.get(&no))
        }
    }

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
                    BinaryOperator::Nand => {
                        Value::Int(i64::from(!(lhs.as_bool() && rhs.as_bool())))
                    }
                    BinaryOperator::Nor => Value::Int(i64::from(!(lhs.as_bool() || rhs.as_bool()))),
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
                        let Some(init) = var_info.init.get(0) else {
                            bail!("No value");
                        };
                        let init = self.const_eval(init)?;
                        Ok(init)
                    } else {
                        bail!("Variable {} is not const", var.var);
                    }
                } else {
                    Err(IdentifierNotFound(var.var).into())
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
                // Emuera folds `VARSIZE` whenever both arguments are already
                // constant and the named variable is not a `REF`
                // (`GameData/Function/Creator.Method.cs:2346-2359`), which is
                // what lets a `#DIM`'s own size term use it — that term is
                // `Restructure`d and then required to be a `SingleTerm`
                // (`GameProc/UserDefinedVariable.cs:230-236`).
                BuiltinMethod::VarSize => {
                    let name = args
                        .first()
                        .and_then(|v| v.as_ref())
                        .context("No argument for VARSIZE method")?;
                    let name = self.const_eval(name)?.into_str().to_ascii_uppercase();
                    let dim = match args.get(1).and_then(|v| v.as_ref()) {
                        Some(dim) => usize::try_from(self.const_eval(dim)?.try_into_int()?)?,
                        None => 0,
                    };

                    // `VarsizeDimConfig` ("VARSIZEの次元指定をERD機能に合わせる",
                    // `Config/ConfigData.cs:136`) would subtract one from a
                    // positive `dim`, but it defaults off and the game leaves
                    // it alone.
                    let key = get_interner().get_or_intern(&name);
                    let Some(info) = self.global_variables.get(&key) else {
                        // Retryable: the variable may be declared by a `#DIM`
                        // this pass has not reached yet.
                        return Err(IdentifierNotFound(key).into());
                    };
                    if info.is_ref {
                        bail!("VARSIZE of REF variable {name} is not constant");
                    }

                    match info.size.get(dim) {
                        Some(size) => Ok(Value::Int(i64::from(*size))),
                        // A variable declared without a size holds one value.
                        None if info.size.is_empty() && dim == 0 => Ok(Value::Int(1)),
                        None => bail!(
                            "VARSIZE dimension {dim} of {name} is out of range, it has {} dimension(s)",
                            info.size.len()
                        ),
                    }
                }
                _ => bail!("Method {meth} can't be used in const context"),
            },
            _ => bail!("Can't be used in const context"),
        }
    }

    pub fn merge_rename_csv(&mut self, s: &str) -> ParserResult<()> {
        for (mut line, _) in csv::lines(s) {
            if let Some((value, key)) = line.next_tuple() {
                self.rename.insert(key.into(), value.into());
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
            juel = "JUEL";
            cflag = "CFLAG";
            name_var = "NAME";
        }

        macro_rules! insert_template {
            ($var:ident, $val1:expr, $val2:expr, $span:expr) => {{
                let idx = match try_parse_csv_int($val1) {
                    Some(idx) => idx,
                    None => {
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

                let value = match try_parse_csv_int64($val2) {
                    Some(idx) => idx,
                    _ => 1,
                    // _ => error_csv!("잘못된 숫자입니다.", $span),
                };

                template.$var.insert(idx, value);
            }};
            (@bool $var:ident, $val1:expr, $val2:expr, $span:expr) => {{
                let idx = match try_parse_csv_int($val1) {
                    Some(idx) => idx,
                    None => {
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
                let idx = match try_parse_csv_int($val1) {
                    Some(idx) => idx,
                    None => {
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

        // Emuera uppercases the key before dispatching on it
        // (`GameData/ConstantData.cs:1518` `tokens[0].ToUpper()`) and gives
        // every fixed name an English alias beside the Japanese one (switch
        // `:1519-1607`); `番号`/`NO` is compared separately and case-
        // insensitively at `:1408-1409` (`Config.SCVariable` is
        // `OrdinalIgnoreCase` whenever `IgnoreCase` is on, `Config/Config.cs:39`,
        // which is its default). This corpus writes `NICKNAME`, `EQUIP`,
        // `CFLAG` and `EXP` in 3_438 lines across 2_323 chara files.
        //
        // The keys are short and ASCII-only, and `ToUpper` leaves the Japanese
        // ones alone, so uppercasing into a stack buffer keeps the whole pass
        // allocation-free. A longer key cannot be a fixed name and falls
        // through to the same warning Emuera gives it.
        let mut upper = [0u8; 16];

        for (mut line, span) in csv::lines(s) {
            if let Some((name, val1)) = line.next_tuple() {
                let val2 = line.next().unwrap_or("");

                let key = if name.is_ascii() && name.len() <= upper.len() {
                    let buf = &mut upper[..name.len()];
                    buf.copy_from_slice(name.as_bytes());
                    buf.make_ascii_uppercase();
                    // SAFETY: the ASCII-uppercase of ASCII is still ASCII.
                    unsafe { std::str::from_utf8_unchecked(buf) }
                } else {
                    name
                };

                match key {
                    // Emuera warns `CanNotConvertToInt` at level 1 and skips
                    // the line rather than aborting the file
                    // (`ConstantData.cs:1417-1420`).
                    "NO" | "番号" => match val1.parse() {
                        Ok(no) => template.no = no,
                        Err(_) => {
                            log::warn!("Character number {val1} is not an integer");
                        }
                    },
                    "NAME" | "名前" => template.name = val1.into(),
                    "MASTERNAME" | "主人の呼び方" => template.master_name = val1.into(),
                    "CALLNAME" | "呼び名" => template.call_name = val1.into(),
                    "NICKNAME" | "あだ名" => template.nick_name = val1.into(),
                    "ISASSI" | "助手" => template.is_assi = val1.trim() == "1",

                    "CSTR" => insert_template!(@str cstr, val1, val2, span),

                    "TALENT" | "素質" => insert_template!(@bool talent, val1, val2, span),

                    "BASE" | "基礎" => insert_template!(base, val1, val2, span),
                    "MARK" | "刻印" => insert_template!(mark, val1, val2, span),
                    "ABL" | "能力" => insert_template!(abl, val1, val2, span),
                    "EXP" | "経験" => insert_template!(exp, val1, val2, span),
                    "RELATION" | "相性" => insert_template!(relation, val1, val2, span),
                    "EQUIP" | "装着物" => insert_template!(equip, val1, val2, span),
                    "JUEL" | "珠" => insert_template!(juel, val1, val2, span),
                    "CFLAG" | "フラグ" => insert_template!(cflag, val1, val2, span),
                    // Emuera's `default:` is `Warn(CanNotInterpreted, 1)`
                    // (`ConstantData.cs:1607-1609`) — a level-1 warning that
                    // leaves the rest of the file to load.
                    other => log::warn!("Unknown character template name: {other}"),
                }
            }
        }

        // Emuera builds a reverse dictionary from every template's Name,
        // Callname and Nickname to that template's `No`
        // (`GameData/ConstantData.cs:690-700`), first definition winning, and
        // hands it to *both* `RELATION` (`allowIndex 1`) and `NAME`
        // (`allowIndex -1`) as their index-name table, with `chara*.csv` as the
        // error position (`:1061-1070`). Without it `RELATION:0:キャラ名` cannot
        // resolve at all.
        //
        // Emuera runs this pass after every chara file has loaded, walking
        // `CharacterTmplList` in load order; merging per file in the same order
        // gives the same winner. A number outside `u32` is skipped rather than
        // wrapped: `var_names` indexes are `u32`, and such a character is not
        // addressable by index either.
        if let Ok(no) = u32::try_from(template.no) {
            for name in [&template.name, &template.call_name, &template.nick_name] {
                if name.is_empty() {
                    continue;
                }
                let name = interner.get_or_intern(name.as_str());
                for var in [relation, name_var] {
                    self.var_names.entry(var).or_default().entry(name).or_insert(no);
                }
            }
        }

        // Emuera `CharacterTemplate.SetSpFlag`: a non-zero `フラグ,0` marks the
        // template as an SP character.
        if template.cflag.get(&0).is_some_and(|f| *f != 0) {
            self.character_sp_templates.insert(template.no, template);
        } else {
            self.character_templates.insert(template.no, template);
        }

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
                    "ウィンドウタイトル" => self.gamebase.window_title = val.into(),
                    // Emuera's GAMEBASE switch (`GameData/GameBase.cs:114-173`)
                    // has no `default:` arm, so a key it does not know is
                    // silently ignored and never diagnosed. This game's
                    // `Data/CSV/GameBase.csv:3` is `타이틀,ShinEraTenseiP` —
                    // Korean, not the katakana `タイトル` the switch compares —
                    // so real Emuera drops it and leaves `ScriptTitle` empty.
                    // Erroring on it would refuse a game its target engine
                    // loads, so this arm has to stay quiet.
                    _ => (),
                }
            }
        }

        // Emuera resolves the fallback at the end of the same load
        // (`GameData/GameBase.cs:184-190`): a titleless game gets the literal
        // `"Emuera"`, otherwise the title followed by `ScriptVersionText`.
        if self.gamebase.window_title.is_empty() {
            self.gamebase.window_title = if self.gamebase.title.is_empty() {
                "Emuera".into()
            } else {
                format!("{} {}", self.gamebase.title, self.gamebase.version_text())
            };
        }

        Ok(())
    }

    pub fn merge_str_csv(&mut self, s: &str) -> ParserResult<()> {
        for (mut line, span) in csv::lines(s) {
            if let Some((n, val)) = line.next_tuple() {
                let n = csv_parse_int!(n, span);
                self.str_templates.insert(n, val.into());
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
                let idx = csv_parse_int!(idx, span);
                let name = interner.get_or_intern(name);
                // A malformed price leaves Emuera's `targetI[index]` unwritten
                // (0) after a `CanNotReadAmountOfMoney` warning
                // (`ConstantData.cs:1726-1731`). The value matches; erars has
                // no CSV warning channel, so the diagnostic is dropped.
                let price = line.next().and_then(try_parse_csv_int).unwrap_or(0);

                self.item_price.insert(idx, price);
                self.var_names.entry(var).or_default().insert(name, idx);
                self.var_name_var.entry(var).or_default().insert(idx, name);
            }
        }

        Ok(())
    }

    fn change_var_size(&mut self, name: &str, sizes: Vec<u32>) -> ParserResult<()> {
        let name_key = get_interner().get_or_intern(name);

        match self.global_variables.get_mut(&name_key) {
            Some(info) => {
                let info_len = info.size.len();
                if info.size.len() != sizes.len() {
                    log::error!("Variable size for {name} is not matched! Expected: {info_len} Actual: {size_len}", size_len = sizes.len());
                    return Ok(());
                }

                info.size.copy_from_slice(&sizes[..info_len]);
            }
            None => {
                log::warn!("Variable {name} is not exists but defined in variablesize.csv");
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
                    name => {
                        let name_key = interner.get_or_intern(name);
                        let mut sizes: Vec<u32> = Vec::with_capacity(4);

                        let mut forbidden = false;

                        for part in line {
                            match try_parse_csv_int(part) {
                                Some(n) => sizes.push(n),
                                _ => {
                                    forbidden = true;
                                    log::info!("Don't use {name}");
                                    self.global_variables.remove(&name_key);
                                    break;
                                }
                            }
                        }

                        if !forbidden {
                            const PAIRS: &[(&str, &str)] = &[
                                ("ABLNAME", "ABL"),
                                ("BASENAME", "BASE"),
                                ("TALENTNAME", "TALENT"),
                                ("FLAGNAME", "FLAG"),
                                ("EXNAME", "EX"),
                                ("EXPNAME", "EXP"),
                                ("CFLAGNAME", "CFLAG"),
                                ("CSTRNAME", "CSTR"),
                                ("STRNAME", "STR"),
                                ("TSTRNAME", "TSTR"),
                                ("EQUIPNAME", "EQUIP"),
                                ("TEQUIPNAME", "TEQUIP"),
                                ("TRAINNAME", "TRAIN"),
                                ("PALAMNAME", "PALAM"),
                                ("SOURCENAME", "SOURCE"),
                                ("STAINNAME", "STAIN"),
                                ("TCVARNAME", "TCVAR"),
                                ("GLOBALNAME", "GLOBAL"),
                                ("GLOBALSNAME", "GLOBALS"),
                                ("MARKNAME", "MARK"),
                                ("SAVESTRNAME", "SAVESTR"),
                                ("ITEMNAME", "ITEMPRICE"),
                                ("ITEMPRICE", "ITEM"),
                                ("ITEM", "ITEMNAME"),
                            ];

                            for pair in PAIRS {
                                if name == pair.0 {
                                    self.change_var_size(pair.1, sizes.clone())?;
                                } else if name == pair.1 {
                                    self.change_var_size(pair.0, sizes.clone())?;
                                }
                            }

                            self.change_var_size(name, sizes)?;
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

    /// Fold the size expressions of a parsed `#DIM` line into a variable.
    ///
    /// Emuera reduces the size terms inside `UserDefinedVariableData.Create`
    /// (`GameProc/UserDefinedVariable.cs:219-233`): each one has to collapse to
    /// a single `Int64`, and for a non-`REF` declaration it has to land in
    /// `1..=1000000`.
    pub fn finish_dim(&self, decl: self::expr::DimDecl) -> anyhow::Result<LocalVariable> {
        let self::expr::DimDecl {
            var,
            mut info,
            sizes,
        } = decl;

        if info.is_ref {
            // A REF variable's own storage is a single int holding the key pair
            // of whatever it points at, so it can be neither a string nor a
            // character array nor part of a save file, and the declared size
            // describes the *target*. The size list is dropped rather than
            // folded, which is also why Emuera runs no range check on this
            // path (`GameProc/UserDefinedVariable.cs:222-227`).
            info.is_dynamic = true;
            info.is_savedata = false;
            info.size = Vec::new();
            info.is_str = false;
            info.is_chara = false;
            return Ok(LocalVariable { var, info });
        }

        if let Some(sizes) = sizes {
            info.size = Vec::with_capacity(sizes.len());
            for size in sizes.iter() {
                let size = self.const_eval(size)?.into_int_err().map_err(|s| {
                    anyhow::anyhow!("Array size of {var} is a string ({s:?}), not an integer")
                })?;

                if !(1..=1_000_000).contains(&size) {
                    bail!("Array size {size} of {var} is out of range, must be 1..=1000000");
                }

                info.size.push(size as u32);
            }
        }

        Ok(LocalVariable { var, info })
    }

    /// Pass 1 of header loading: `#DEFINE` takes effect immediately, every
    /// `#DIM`/`#DIMS` line is queued onto `pending` for pass 2.
    ///
    /// Emuera splits the two the same way — `GameProc/HeaderFileLoader.cs:116`
    /// handles `#DEFINE` inline while `:123-131` only lexes a `#DIM` line and
    /// enqueues it, under the comment
    /// `1822 #DIMは保留しておいて後でまとめてやる` ("defer #DIM and do them all
    /// together later") — so a declaration may size itself with a constant
    /// declared further down the same file, or in any other header file, in
    /// either direction.
    pub fn merge_header_defines(
        &mut self,
        file: usize,
        s: &str,
        pending: &mut Vec<PendingDim>,
    ) -> ParserResult<()> {
        // ERH goes through `_Rename.csv` exactly like ERB does — Emuera reads
        // both through `EramakerFile`, whose `ReadLine` applies the rename
        // dictionary before anything else sees the text
        // (`GameProc/HeaderFileLoader.cs:86`). This corpus depends on it:
        // `RPG/依頼/REQUEST_95_近畿霊務局/REQUEST_95_近畿霊務局.ERH:1` is
        // `#DEFINE FLAG_REQ95_進行度 依頼フラグ:[[依頼:킨키영무국의뢰]]:0`,
        // whose macro body only becomes a readable variable reference once the
        // `[[…]]` splice has happened.
        let mut pp = Preprocessor::new_erh(&self.rename, s);
        let mut b = Bump::new();

        loop {
            match pp.next_line(&b)? {
                Some(EraLine::SharpLine {
                    sharp: SharpCode::DEFINE,
                    args,
                }) => {
                    let (args, ident) = try_nom!(pp, self::expr::ident_no_case(args));
                    self.macros.insert(ident.to_string(), args.trim().to_string());
                }
                Some(EraLine::SharpLine {
                    sharp: SharpCode::DIM,
                    args,
                }) => pending.push(PendingDim {
                    file,
                    is_str: false,
                    span: pp.span(),
                    args: args.to_string(),
                }),
                Some(EraLine::SharpLine {
                    sharp: SharpCode::DIMS,
                    args,
                }) => pending.push(PendingDim {
                    file,
                    is_str: true,
                    span: pp.span(),
                    args: args.to_string(),
                }),
                Some(_) => error!(pp.span(), "Invalid line"),
                None => break,
            }

            b.reset();
        }

        Ok(())
    }

    /// Pass 2 of header loading: declare every queued `#DIM`/`#DIMS` line,
    /// retrying the ones that are only waiting on a constant another queued
    /// line still has to declare. Returns one diagnostic per line that never
    /// resolved, tagged with the `PendingDim::file` it came from.
    ///
    /// This is Emuera's `analyzeSharpDimLines`
    /// (`GameProc/HeaderFileLoader.cs:276-364`): each pass dequeues what is
    /// left, re-enqueues only the lines that raised
    /// `IdentifierNotFoundCodeEE` (`:341-348`), and stops retrying as soon as
    /// a whole pass settles nothing (`:361-362`), after which the remainder is
    /// reported. Work per pass is therefore proportional to what is still
    /// unresolved, and a line that resolves is parsed exactly once.
    pub fn resolve_pending_dims(&mut self, pending: Vec<PendingDim>) -> Vec<(usize, ParserError)> {
        let mut ctx = ParserContext::new(self, StrKey::new("DEFAULT.ERH"));
        let mut queue = VecDeque::from(pending);
        let mut errors = Vec::new();
        let mut retry = true;

        while !queue.is_empty() {
            let count = queue.len();
            let mut settled = 0usize;

            for _ in 0..count {
                // `count` was taken from `queue.len()` and nothing else pops.
                let dim = queue.pop_front().unwrap();

                // Bound to a `let` so the parser closure and the `&HeaderInfo`
                // it reads through are both dropped before the insert below
                // needs `&mut`.
                let declared = self::expr::dim_line(&ctx, dim.is_str)(&dim.args)
                    .map_err(|err| match err {
                        nom::Err::Error(err) | nom::Err::Failure(err) => {
                            anyhow::anyhow!("Expression parsing failed: {err}")
                        }
                        nom::Err::Incomplete(_) => unreachable!(),
                    })
                    .and_then(|(_, decl)| ctx.header.as_ref().finish_dim(decl));

                let err = match declared {
                    Ok(var) => {
                        // `ctx` was built from `&mut *self`, so this is the
                        // caller's own `HeaderInfo`.
                        if let Some(old) = ctx
                            .header
                            .try_mut()
                            .unwrap()
                            .global_variables
                            .insert(var.var, var.info)
                        {
                            log::error!("Duplicate var name {} {old:?}", var.var);
                        }
                        settled += 1;
                        continue;
                    }
                    Err(err) => err,
                };

                if retry && err.downcast_ref::<IdentifierNotFound>().is_some() {
                    queue.push_back(dim);
                } else {
                    errors.push((dim.file, (err.to_string(), dim.span)));
                    settled += 1;
                }
            }

            if settled == 0 {
                retry = false;
            }
        }

        errors
    }

    /// Load one header file on its own: pass 1 then pass 2, the shape
    /// `LoadHeaderFiles` degenerates to for a single file
    /// (`GameProc/HeaderFileLoader.cs:37-77`).
    pub fn merge_header(&mut self, s: &str) -> ParserResult<()> {
        let mut pending = Vec::new();
        self.merge_header_defines(0, s, &mut pending)?;

        match self.resolve_pending_dims(pending).into_iter().next() {
            Some((_, err)) => Err(err),
            None => Ok(()),
        }
    }
}

#[derive(Debug)]
pub enum HeaderInfoRef<'p> {
    Ref(&'p HeaderInfo),
    Mut(&'p mut HeaderInfo),
    Arc(Arc<HeaderInfo>),
}

impl<'p> HeaderInfoRef<'p> {
    pub fn try_mut(&mut self) -> Option<&mut HeaderInfo> {
        match self {
            HeaderInfoRef::Ref(_) => None,
            HeaderInfoRef::Mut(r) => Some(r),
            HeaderInfoRef::Arc(_) => None,
        }
    }

    pub fn try_as_arc(&self) -> Option<Arc<HeaderInfo>> {
        match self {
            HeaderInfoRef::Ref(_) => None,
            HeaderInfoRef::Mut(_) => None,
            HeaderInfoRef::Arc(a) => Some(a.clone()),
        }
    }
}

impl<'p> From<&'p HeaderInfo> for HeaderInfoRef<'p> {
    fn from(r: &'p HeaderInfo) -> Self {
        Self::Ref(r)
    }
}

impl<'p> From<&'p mut HeaderInfo> for HeaderInfoRef<'p> {
    fn from(r: &'p mut HeaderInfo) -> Self {
        Self::Mut(r)
    }
}

impl From<Arc<HeaderInfo>> for HeaderInfoRef<'static> {
    fn from(a: Arc<HeaderInfo>) -> Self {
        Self::Arc(a)
    }
}

impl<'p> AsRef<HeaderInfo> for HeaderInfoRef<'p> {
    fn as_ref(&self) -> &HeaderInfo {
        match self {
            HeaderInfoRef::Ref(r) => r,
            HeaderInfoRef::Mut(r) => r,
            HeaderInfoRef::Arc(a) => a.as_ref(),
        }
    }
}

/// Drains the `[…]` preprocessor's warnings, tagged with Emuera's level 1:
/// `DuplicateSkipstart` and `UnexpectedSkipend` are both raised at that level
/// (`GameProc/ErbLoader.cs:154-171`, `:239-252`), so a game asking for
/// `表示する最低警告レベル:2` never sees them.
fn pp_warnings(pp: &mut Preprocessor) -> Vec<ParserWarning> {
    pp.take_warnings()
        .into_iter()
        .map(|(msg, span)| (msg, span, 1))
        .collect()
}

#[derive(Debug)]
pub struct ParserContext<'p> {
    pub interner: &'static Interner,
    pub locals_key: StrKey,
    pub args_key: StrKey,
    pub header: HeaderInfoRef<'p>,
    pub local_strs: RefCell<HashSet<StrKey>>,
    /// `VARI`/`VARS` declarations met while parsing the current function.
    ///
    /// They can sit anywhere in the body — inside an `IF`, a `WHILE`, a
    /// `SELECTCASE` arm — but the variable they declare belongs to the whole
    /// function, exactly like a `#DIM`. `parse_stmt` therefore drops them here
    /// and the function loop drains them into `FunctionHeader::infos`.
    pub local_dims: RefCell<Vec<LocalVariable>>,
    pub is_arg: Cell<bool>,
    pub ban_percent: Cell<bool>,
    pub file_path: StrKey,
    /// Emuera's `-DEBUG` (`Program.cs:219-220`). It decides the whole debug
    /// family at load time — `[IF_DEBUG]`/`[IF_NDEBUG]`, the `;#;` marker,
    /// and whether `DEBUGPRINT`/`ASSERT` lines are compiled at all — because
    /// Emuera fixes it from the command line before any script is read.
    debug_mode: bool,
}

impl<'p> ParserContext<'p> {
    pub fn new(header: impl Into<HeaderInfoRef<'p>>, file_path: StrKey) -> Self {
        let interner = get_interner();
        Self {
            interner,
            locals_key: interner.get_or_intern_static("LOCALS"),
            args_key: interner.get_or_intern_static("ARGS"),
            header: header.into(),
            file_path,
            local_strs: RefCell::default(),
            local_dims: RefCell::default(),
            is_arg: Cell::new(false),
            ban_percent: Cell::new(false),
            debug_mode: false,
        }
    }

    /// Turns on what Emuera's `-DEBUG` turns on: `[IF_DEBUG]`, the `;#;`
    /// marker and the `DEBUGPRINT` family (`Program.cs:82-88`,
    /// `Program.cs:219-220`).
    pub fn with_debug(mut self, debug_mode: bool) -> Self {
        self.debug_mode = debug_mode;
        self
    }

    /// The preprocessor for one ERB of this game.
    ///
    /// Everything it needs — the rename table, the `#DEFINE` names `[IF]`
    /// asks about, the debug flag — comes from the header, so no caller has
    /// to assemble it.
    pub fn preprocessor<'s>(&'s self, s: &'s str) -> Preprocessor<'s> {
        let header = self.header.as_ref();
        Preprocessor::new_erb(&header.rename, &header.macros, self.debug_mode, s)
    }

    /// Intern an identifier through the calling thread's memo.
    ///
    /// The memo is thread-local rather than a field here because a
    /// `ParserContext` is built fresh for every ERB, so a per-file map spent
    /// each file re-learning the identifiers — `LOCAL`, `ARG`, `RESULT`, the
    /// CSV variable names — that every other file also uses.
    fn intern_ident(&self, s: &str) -> StrKey {
        erars_ast::intern_cached(s)
    }

    pub fn is_str_var(&self, key: StrKey) -> bool {
        if key == self.locals_key || key == self.args_key || self.local_strs.borrow().contains(&key)
        {
            true
        } else if let Some(v) = self.header.as_ref().global_variables.get(&key) {
            v.is_str
        } else {
            false
        }
    }

    pub fn replace<'s>(&self, s: &'s str) -> Cow<'s, str> {
        let mut ret = Cow::Borrowed(s);

        while let Some(new) = self.header.as_ref().macros.get(ret.as_ref()) {
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
                    out.extend(self.parse_stmt(line, pp, b)?);
                }
                None => {
                    error!(pp.span(), format!("Block doesn't end with {end}"));
                }
            }
        }
    }

    /// The shared body of a `PRINTDATA`/`STRDATA` block: `DATA`, `DATAFORM`
    /// and `DATALIST`…`ENDLIST` entries up to `ENDDATA`
    /// (`GameProc/ErbLoader.cs:1268-1330`). One `Vec<Expr>` per entry; entries
    /// with more than one part are concatenated by the compiler.
    fn read_data_block(
        &self,
        pp: &mut Preprocessor,
        b: &Bump,
        in_strdata: bool,
    ) -> ParserResult<Vec<Vec<Expr>>> {
        let mut list = Vec::new();

        loop {
            match pp.next_line(b)? {
                Some(EraLine::InstLine {
                    inst: InstructionCode::DATA,
                    args,
                }) => list.push(vec![Expr::str(args)]),
                Some(EraLine::InstLine {
                    inst: InstructionCode::DATAFORM,
                    args,
                }) => {
                    list.push(vec![
                        try_nom!(pp, self::expr::normal_form_str(self)(args)).1,
                    ])
                }
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
                            }) => cur_list.push(Expr::str(args)),
                            Some(EraLine::InstLine {
                                inst: InstructionCode::DATAFORM,
                                args,
                            }) => cur_list
                                .push(try_nom!(pp, self::expr::normal_form_str(self)(args)).1),
                            Some(EraLine::InstLine {
                                inst: InstructionCode::ENDLIST,
                                args: _,
                            }) => break,
                            Some(_) => {
                                error!(pp.span(), "DATALIST에 잘못된 토큰이 들어왔습니다")
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
                // Neither block may contain the other and `STRDATA` may not
                // nest (`ErbLoader.cs:1248-1266`, messages at
                // `_Library/EvilMask/Lang.cs:822-824`).
                Some(EraLine::InstLine {
                    inst: InstructionCode::STRDATA,
                    args: _,
                }) => {
                    if in_strdata {
                        error!(pp.span(), "STRDATA命令が入れ子にされています")
                    } else {
                        error!(pp.span(), "PRINTDATA系命令の中にSTRDATA系命令が含まれています")
                    }
                }
                Some(EraLine::PrintLine {
                    ty: PrintType::Data,
                    ..
                }) if in_strdata => {
                    error!(pp.span(), "STRDATA系命令の中にPRINTDATA系命令が含まれています")
                }
                Some(_) if in_strdata => {
                    error!(pp.span(), "STRDATA에 잘못된 토큰이 들어왔습니다")
                }
                Some(_) => error!(pp.span(), "PRINTDATA에 잘못된 토큰이 들어왔습니다"),
                None if in_strdata => error!(pp.span(), "ENDDATA없이 STRDATA가 끝났습니다."),
                None => error!(pp.span(), "ENDDATA없이 PRINTDATA가 끝났습니다."),
            }
        }

        Ok(list)
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
                    out.extend(self.parse_stmt(line, pp, b)?);
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
    ) -> ParserResult<Option<StmtWithPos>> {
        let pos = pp.script_pos();
        let stmt = match line {
            EraLine::GotoLine(line) => Stmt::Label(self.intern_ident(line.trim())),
            EraLine::PrintLine {
                flags,
                ty,
                args: form,
            } => match ty {
                PrintType::Plain => Stmt::Print(flags, Expr::str(form)),
                PrintType::Data => {
                    let form = form.trim();
                    // The argument is the variable that *receives* the chosen
                    // index, not a selector (`ArgumentBuilder.cs:1619-1630`).
                    let var = if form.is_empty() {
                        None
                    } else {
                        Some(try_nom!(pp, self::expr::variable(self)(form)).1)
                    };
                    let list = self.read_data_block(pp, b, false)?;

                    Stmt::PrintData(flags, var, list)
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
            // Emuera's method-as-instruction line
            // (`FunctionIdentifier.cs:428-436` +
            // `Instraction.Child.cs:487-498`): evaluate the method with the
            // bare `METHOD` argument list and store its value in
            // `RESULT`/`RESULTS`, which is exactly `Stmt::Method`.
            EraLine::MethodLine { meth, args } => {
                let args = try_nom!(pp, self::expr::expr_list(self)(args)).1;
                Stmt::Method(meth, args)
            }
            EraLine::InstLine { inst, args } => {
                use erars_lexer::InstructionCode::*;

                // Emuera's `DEBUG_FUNC` lines. Without `-DEBUG` the line is
                // dropped *before* its arguments are parsed
                // (`GameProc/Process.ScriptProc.cs:33-40`,
                // `GameProc/Function/ArgumentParser.cs:22-27`), so a
                // `DEBUGPRINTFORM` holding an expression that would not even
                // compile is legal in a release run. It cannot become a
                // comment either — 「SIF文のためにコメント行扱いにはできない」
                // (`Process.ScriptProc.cs:35`) — so it still occupies one
                // statement slot, which is what `SIF` binds to.
                if !self.debug_mode
                    && matches!(
                        inst,
                        DEBUGPRINT
                            | DEBUGPRINTL
                            | DEBUGPRINTFORM
                            | DEBUGPRINTFORML
                            | DEBUGCLEAR
                            | ASSERT
                    )
                {
                    return Ok(Some(StmtWithPos(Stmt::Nop, pos)));
                }


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
                    PRINTPLAIN => Stmt::Print(PrintFlags::PLAIN, Expr::str(args)),
                    DEBUGPRINT => Stmt::Print(PrintFlags::DEBUG, Expr::str(args)),
                    DEBUGPRINTL => Stmt::Print(
                        PrintFlags::DEBUG | PrintFlags::NEWLINE,
                        Expr::str(args),
                    ),
                    DEBUGPRINTFORM => Stmt::Print(
                        PrintFlags::DEBUG,
                        try_nom!(pp, self::expr::normal_form_str(self)(args)).1,
                    ),
                    DEBUGPRINTFORML => Stmt::Print(
                        PrintFlags::DEBUG | PrintFlags::NEWLINE,
                        try_nom!(pp, self::expr::normal_form_str(self)(args)).1,
                    ),
                    DEBUGCLEAR => normal_command!(BuiltinCommand::DebugClear),
                    PRINT_SPACE => normal_command!(BuiltinCommand::PrintSpace),
                    PRINT_RECT => normal_command!(BuiltinCommand::PrintRect),
                    PRINT_IMG => normal_command!(BuiltinCommand::PrintImg),
                    OUTPUTLOG => normal_command!(BuiltinCommand::OutputLog),
                    // Line-head `PRINTCPERLINE` is `SP_GETINT`
                    // (`FunctionIdentifier.cs:397`): it stores the config value
                    // into a variable. The expression form
                    // `PRINTCPERLINE()` stays `BuiltinMethod::PrintCPerLine`
                    // (`Creator.cs:67`).
                    PRINTCPERLINE => normal_command!(BuiltinCommand::PrintCPerLine),
                    TOOLTIP_SETCOLOR => normal_command!(BuiltinCommand::TooltipSetColor),
                    TOOLTIP_SETDELAY => normal_command!(BuiltinCommand::TooltipSetDelay),
                    TOOLTIP_SETDURATION => normal_command!(BuiltinCommand::TooltipSetDuration),
                    DRAWLINE => Stmt::Command(BuiltinCommand::DrawLine, vec![]),
                    DRAWLINEFORM => strform_command!(BuiltinCommand::CustomDrawLine),
                    CUSTOMDRAWLINE => Stmt::Command(
                        BuiltinCommand::CustomDrawLine,
                        vec![Some(Expr::str(args.trim_start()))],
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
                    CLEARTEXTBOX => normal_command!(BuiltinCommand::ClearTextBox),
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
                    AWAIT => normal_command!(BuiltinCommand::Await),
                    RESETDATA => normal_command!(BuiltinCommand::ResetData),
                    RESET_STAIN => normal_command!(BuiltinCommand::ResetStain),
                    ADDCHARA => normal_command!(BuiltinCommand::AddChara),
                    ADDDEFCHARA => normal_command!(BuiltinCommand::AddDefChara),
                    ADDCOPYCHARA => normal_command!(BuiltinCommand::AddCopyChara),
                    ADDSPCHARA => normal_command!(BuiltinCommand::AddSpChara),
                    ADDVOIDCHARA => normal_command!(BuiltinCommand::AddVoidChara),
                    DELALLCHARA => normal_command!(BuiltinCommand::DelAllChara),
                    GETCHARA => normal_method!(BuiltinMethod::GetChara),
                    FINDCHARA => normal_method!(BuiltinMethod::FindChara),
                    FINDLASTCHARA => normal_method!(BuiltinMethod::FindLastChara),
                    FIND_CHARADATA => normal_method!(BuiltinMethod::FindCharaData),
                    EXISTCSV => normal_method!(BuiltinMethod::ExistCsv),
                    DELCHARA => normal_command!(BuiltinCommand::DelChara),
                    SAVECHARA => normal_command!(BuiltinCommand::SaveChara),
                    LOADCHARA => normal_command!(BuiltinCommand::LoadChara),
                    CHKCHARADATA => normal_method!(BuiltinMethod::ChkCharaData),
                    SORTCHARA => try_nom!(pp, self::expr::sortchara_line(self)(args)).1,
                    COPYCHARA => normal_command!(BuiltinCommand::CopyChara),
                    SWAPCHARA => normal_command!(BuiltinCommand::SwapChara),
                    PICKUPCHARA => normal_command!(BuiltinCommand::PickupChara),
                    FONTBOLD => normal_command!(BuiltinCommand::FontBold),
                    FONTITALIC => normal_command!(BuiltinCommand::FontItalic),
                    FONTREGULAR => normal_command!(BuiltinCommand::FontRegular),
                    FONTSTYLE => normal_command!(BuiltinCommand::FontStyle),
                    GETSTYLE => normal_method!(BuiltinMethod::GetStyle),
                    SETFONT => normal_command!(BuiltinCommand::SetFont),
                    CHKFONT => normal_method!(BuiltinMethod::ChkFont),
                    GETFONT => normal_method!(BuiltinMethod::GetFont),

                    REDRAW => normal_command!(BuiltinCommand::Redraw),
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
                    STRLENU => normal_method!(BuiltinMethod::StrLenSU),
                    STRLENFORM => strform_method!(BuiltinMethod::StrLenS),
                    STRLENFORMU => strform_method!(BuiltinMethod::StrLenSU),
                    STRFIND => normal_method!(BuiltinMethod::StrFind),
                    STRFINDU => normal_method!(BuiltinMethod::StrFindU),
                    UNICODE => normal_method!(BuiltinMethod::Unicode),
                    ENCODETOUNI => strform_command!(BuiltinCommand::EncodeToUni),
                    REPLACE => normal_method!(BuiltinMethod::Replace),
                    ESCAPE => normal_method!(BuiltinMethod::Escape),
                    STRCOUNT => normal_method!(BuiltinMethod::StrCount),
                    STRJOIN => normal_method!(BuiltinMethod::StrJoin),
                    BARSTR => normal_method!(BuiltinMethod::BarStr),
                    MONEYSTR => normal_method!(BuiltinMethod::MoneyStr),
                    TOUPPER => normal_method!(BuiltinMethod::ToUpper),
                    TOLOWER => normal_method!(BuiltinMethod::ToLower),
                    TOHALF => normal_method!(BuiltinMethod::ToHalf),
                    TOFULL => normal_method!(BuiltinMethod::ToFull),
                    ISNUMERIC => normal_method!(BuiltinMethod::IsNumeric),
                    CONVERT => normal_method!(BuiltinMethod::Convert),

                    SPLIT => normal_command!(BuiltinCommand::Split),
                    SWAP => normal_command!(BuiltinCommand::Swap),
                    SAVEGLOBAL => normal_command!(BuiltinCommand::SaveGlobal),
                    LOADGLOBAL => normal_command!(BuiltinCommand::LoadGlobal),
                    POWER => normal_command!(BuiltinCommand::Power),
                    FORCEKANA => normal_command!(BuiltinCommand::ForceKana),
                    MIN => normal_method!(BuiltinMethod::Min),
                    MAX => normal_method!(BuiltinMethod::Max),
                    LIMIT => normal_method!(BuiltinMethod::Limit),
                    ABS => normal_method!(BuiltinMethod::Abs),
                    LOG => normal_method!(BuiltinMethod::Log),
                    LOG10 => normal_method!(BuiltinMethod::Log10),
                    SQRT => normal_method!(BuiltinMethod::Sqrt),
                    SIGN => normal_method!(BuiltinMethod::Sign),
                    TOINT => normal_method!(BuiltinMethod::ToInt),
                    TOSTR => normal_method!(BuiltinMethod::ToStr),
                    INRANGE => normal_method!(BuiltinMethod::InRange),
                    LINEISEMPTY => normal_method!(BuiltinMethod::LineIsEmpty),
                    GROUPMATCH => normal_method!(BuiltinMethod::GroupMatch),
                    NOSAMES => normal_method!(BuiltinMethod::NoSames),
                    ALLSAMES => normal_method!(BuiltinMethod::AllSames),
                    FINDELEMENT => normal_method!(BuiltinMethod::FindElement),
                    FINDLASTELEMENT => normal_method!(BuiltinMethod::FindLastElement),
                    CURRENTALIGN => normal_method!(BuiltinMethod::CurrentAlign),
                    CURRENTREDRAW => normal_method!(BuiltinMethod::CurrentRedraw),

                    SAVEGAME => normal_command!(BuiltinCommand::SaveGame),
                    LOADGAME => normal_command!(BuiltinCommand::LoadGame),
                    SAVENOS => normal_command!(BuiltinCommand::SaveNos),
                    SAVEDATA => normal_command!(BuiltinCommand::SaveData),
                    LOADDATA => normal_command!(BuiltinCommand::LoadData),
                    CHKDATA => normal_method!(BuiltinMethod::ChkData),
                    DELDATA => normal_command!(BuiltinCommand::DelData),
                    GETTIME => normal_command!(BuiltinCommand::GetTime),
                    SAVETEXT => normal_method!(BuiltinMethod::SaveText),
                    LOADTEXT => normal_method!(BuiltinMethod::LoadText),
                    SAVEVAR => normal_command!(BuiltinCommand::SaveVar),
                    LOADVAR => normal_command!(BuiltinCommand::LoadVar),
                    CHKVARDATA => normal_method!(BuiltinMethod::ChkVarData),
                    RESETGLOBAL => normal_command!(BuiltinCommand::ResetGlobal),
                    GETTIMES => normal_method!(BuiltinMethod::GetTimeS),
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
                    ARRAYMSORT => normal_command!(BuiltinCommand::ArrayMSort),
                    GETNUM => normal_method!(BuiltinMethod::GetNum),
                    GETEXPLV => normal_method!(BuiltinMethod::GetExpLv),
                    GETPALAMLV => normal_method!(BuiltinMethod::GetPalamLv),
                    GETCONFIG => normal_method!(BuiltinMethod::GetConfig),
                    GETCONFIGS => normal_method!(BuiltinMethod::GetConfigS),

                    DOTRAIN => normal_command!(BuiltinCommand::DoTrain),
                    CALLTRAIN => normal_command!(BuiltinCommand::CallTrain),
                    UPCHECK => normal_command!(BuiltinCommand::UpCheck),
                    CUPCHECK => normal_command!(BuiltinCommand::CUpCheck),

                    SKIPDISP => normal_command!(BuiltinCommand::SkipDisp),
                    NOSKIP => normal_command!(BuiltinCommand::NoSkip),
                    ENDNOSKIP => normal_command!(BuiltinCommand::EndNoSkip),
                    ISSKIP => normal_method!(BuiltinMethod::IsSkip),
                    MESSKIP => normal_method!(BuiltinMethod::MesSkip),
                    MOUSESKIP => {
                        // Emuera's only `FuncDeprecated` warning
                        // (`Creator.Method.cs:2516-2518`); it is a parse-time
                        // diagnostic, not a runtime one.
                        log::warn!(
                            "MOUSESKIP()は推奨されません。代わりにMESSKIP()を使用してください"
                        );
                        normal_method!(BuiltinMethod::MouseSkip)
                    }

                    BAR => normal_command!(BuiltinCommand::Bar),
                    BARL => normal_command!(BuiltinCommand::BarL),

                    HTML_PRINT => normal_command!(BuiltinCommand::HtmlPrint),
                    HTML_PRINT_ISLAND => normal_command!(BuiltinCommand::HtmlPrintIsland),
                    HTML_PRINT_ISLAND_CLEAR => {
                        normal_command!(BuiltinCommand::HtmlPrintIslandClear)
                    }
                    MATCHALL => normal_command!(BuiltinCommand::MatchAll),
                    PRINT_ABL => normal_command!(BuiltinCommand::PrintAbl),
                    PRINT_TALENT => normal_command!(BuiltinCommand::PrintTalent),
                    PRINT_MARK => normal_command!(BuiltinCommand::PrintMark),
                    PRINT_EXP => normal_command!(BuiltinCommand::PrintExp),
                    PRINT_PALAM => normal_command!(BuiltinCommand::PrintPalam),
                    PRINT_ITEM => normal_command!(BuiltinCommand::PrintItem),
                    PRINT_SHOPITEM => normal_command!(BuiltinCommand::PrintShopItem),
                    HTML_TAGSPLIT => normal_command!(BuiltinCommand::HtmlTagSplit),
                    // In-expression string functions
                    // (`GameData/Function/Creator.cs:145-148`). An expression
                    // resolves them through `BuiltinMethod::from_str`
                    // (`parser/expr.rs:538`); these arms are for the statement
                    // position erars allows every method in.
                    HTML_ESCAPE => normal_method!(BuiltinMethod::HtmlEscape),
                    HTML_TOPLAINTEXT => normal_method!(BuiltinMethod::HtmlToPlainText),
                    HTML_GETPRINTEDSTR => normal_method!(BuiltinMethod::HtmlGetPrintedStr),
                    HTML_POPPRINTINGSTR => normal_method!(BuiltinMethod::HtmlPopPrintingStr),
                    // Wiki in-expression functions; same statement-position
                    // courtesy as the `HTML_*` arms above.
                    GETLINESTR => normal_method!(BuiltinMethod::GetLineStr),
                    STRFORM => normal_method!(BuiltinMethod::StrForm),
                    COLOR_FROMRGB => normal_method!(BuiltinMethod::ColorFromRgb),
                    COLOR_FROMNAME => normal_method!(BuiltinMethod::ColorFromName),
                    PRINTCLENGTH => normal_method!(BuiltinMethod::PrintCLength),
                    CBRT => normal_method!(BuiltinMethod::Cbrt),
                    EXPONENT => normal_method!(BuiltinMethod::Exponent),
                    GETSPCHARA => normal_method!(BuiltinMethod::GetSpChara),
                    INPUTMOUSEKEY => normal_command!(BuiltinCommand::InputMouseKey),

                    GCREATE => normal_method!(BuiltinMethod::GCreate),
                    GCREATED => normal_method!(BuiltinMethod::GCreated),
                    GDISPOSE => normal_method!(BuiltinMethod::GDispose),
                    GCLEAR => normal_method!(BuiltinMethod::GClear),
                    GWIDTH => normal_method!(BuiltinMethod::GWidth),
                    GHEIGHT => normal_method!(BuiltinMethod::GHeight),
                    GGETCOLOR => normal_method!(BuiltinMethod::GGetColor),
                    GSETCOLOR => normal_method!(BuiltinMethod::GSetColor),
                    GSETBRUSH => normal_method!(BuiltinMethod::GSetBrush),
                    GSETPEN => normal_method!(BuiltinMethod::GSetPen),
                    GSETFONT => normal_method!(BuiltinMethod::GSetFont),
                    GDRAWG => normal_method!(BuiltinMethod::GDrawG),
                    GDRAWGWITHMASK => normal_method!(BuiltinMethod::GDrawGWithMask),
                    GDRAWSPRITE => normal_method!(BuiltinMethod::GDrawSprite),
                    GSAVE => normal_method!(BuiltinMethod::GSave),
                    GLOAD => normal_method!(BuiltinMethod::GLoad),

                    SPRITECREATE => normal_method!(BuiltinMethod::SpriteCreate),
                    SPRITECREATED => normal_method!(BuiltinMethod::SpriteCreated),
                    SPRITEWIDTH => normal_method!(BuiltinMethod::SpriteWidth),
                    SPRITEHEIGHT => normal_method!(BuiltinMethod::SpriteHeight),
                    SPRITEPOSX => normal_method!(BuiltinMethod::SpritePosX),
                    SPRITEPOSY => normal_method!(BuiltinMethod::SpritePosY),
                    SPRITESETPOS => normal_method!(BuiltinMethod::SpriteSetPos),
                    SPRITEMOVE => normal_method!(BuiltinMethod::SpriteMove),
                    SPRITEDISPOSE => normal_method!(BuiltinMethod::SpriteDispose),

                    RANDOMIZE => normal_command!(BuiltinCommand::Randomize),
                    INITRAND => normal_command!(BuiltinCommand::InitRand),
                    DUMPRAND => normal_command!(BuiltinCommand::DumpRand),
                    RAND => normal_method!(BuiltinMethod::Rand),

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

                    // Emuera `ErbLoader.cs:1330-1385`: a TRY*LIST opens a
                    // block of `FUNC` candidate lines closed by `ENDFUNC`.
                    // Nesting is rejected, and under TRYGOTOLIST a candidate
                    // may not carry arguments.
                    TRYCALLLIST | TRYJUMPLIST | TRYGOTOLIST => {
                        let is_goto = inst == TRYGOTOLIST;
                        let mut candidates = Vec::new();

                        loop {
                            match pp.next_line(b)? {
                                Some(EraLine::InstLine { inst: FUNC, args }) => {
                                    // Emuera gives FUNC the `SP_CALLFORM`
                                    // builder, so the name is a form string in
                                    // every list type.
                                    let (name, func_args) = try_nom!(
                                        pp,
                                        self::expr::call_jump_line(self, true)(args)
                                    )
                                    .1;

                                    if is_goto && !func_args.is_empty() {
                                        error!(
                                            pp.span(),
                                            "TRYGOTOLIST의 호출 대상에 인수를 지정할 수 없습니다"
                                        );
                                    }

                                    candidates.push((name, func_args));
                                }
                                Some(EraLine::InstLine { inst: ENDFUNC, args: _ }) => break,
                                Some(_) => error!(
                                    pp.span(),
                                    format!("{inst}에 잘못된 토큰이 들어왔습니다")
                                ),
                                None => error!(
                                    pp.span(),
                                    format!("ENDFUNC없이 {inst}가 끝났습니다.")
                                ),
                            }
                        }

                        if is_goto {
                            Stmt::GotoList(candidates.into_iter().map(|(name, _)| name).collect())
                        } else {
                            Stmt::CallList {
                                candidates,
                                is_jump: inst == TRYJUMPLIST,
                            }
                        }
                    }

                    // Emuera `trerror.MissingTrycalllist`: both are only valid
                    // inside the block consumed by the arm above.
                    FUNC | ENDFUNC => {
                        error!(pp.span(), format!("대응하는 TRYCALLLIST 계열 명령이 없는 {inst}입니다"))
                    }

                    // Emuera `CALLEVENT_Instruction` takes a *constant* string
                    // (`func.Argument.ConstStr`) and uppercases it when
                    // `Config.ICFunction`, so the target is fixed at parse
                    // time. Its nine legal names are exactly `EventType`
                    // (`IdentifierDictionary.IsEventLabelName`).
                    CALLEVENT => {
                        let name = args.trim();
                        match name.to_ascii_uppercase().parse() {
                            Ok(ty) => Stmt::CallEvent(ty),
                            Err(_) => error!(
                                pp.span(),
                                format!("CALLEVENT의 대상 {name}은 이벤트 함수가 아닙니다")
                            ),
                        }
                    }

                    ASSERT => normal_command!(BuiltinCommand::Assert),
                    STOPCALLTRAIN => normal_command!(BuiltinCommand::StopCallTrain),

                    REF | REFBYNAME => {
                        let (target, src) = try_nom!(
                            pp,
                            self::expr::ref_line(self, inst == REFBYNAME)(args)
                        )
                        .1;
                        Stmt::Command(
                            if inst == REFBYNAME {
                                BuiltinCommand::RefByName
                            } else {
                                BuiltinCommand::Ref
                            },
                            vec![Some(target), Some(src)],
                        )
                    }

                    SIF => {
                        let cond = try_nom!(pp, self::expr::expr(self)(args)).1;
                        let Some(body) = pp.next_line(b)? else {
                            error!(pp.span(), "No body statement in SIF");
                        };
                        let Some(body) = self.parse_stmt(body, pp, b)? else {
                            error!(pp.span(), "No body statement in SIF");
                        };
                        Stmt::Sif(cond, Box::new(body))
                    }

                    IF => {
                        let mut is_else = false;
                        let mut cond_pos = pp.script_pos();
                        let mut cond = try_nom!(pp, self::expr::expr_or_one(self)(args)).1;
                        // `StmtWithPos` is 128 bytes, so the old
                        // `with_capacity(128)` reserved 16 KiB for every one
                        // of the corpus's 109_177 IF constructs. Measured over
                        // the corpus an IF/ELSEIF/ELSE block holds 2.16
                        // statements on average (median 2, p99 12) and only 13
                        // of 274_801 blocks exceed 128 — and the three other
                        // blocks of the same construct below already start
                        // empty.
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
                                    block.extend(self.parse_stmt(line, pp, b)?);
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
                                    body.extend(self.parse_stmt(line, pp, b)?);
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
                    STRDATA => {
                        let args = args.trim();
                        let var = if args.is_empty() {
                            // `VAR_STR_ArgumentBuilder` has `minArg = 0` and
                            // defaults to `RESULTS:0`
                            // (`ArgumentBuilder.cs:1640-1648`).
                            try_nom!(pp, self::expr::variable(self)("RESULTS")).1
                        } else {
                            try_nom!(pp, self::expr::variable(self)(args)).1
                        };
                        let mut list = self.read_data_block(pp, b, true)?;

                        // A chosen entry's lines are joined with `\n`
                        // (`Process.ScriptProc.cs:762-771`), unlike
                        // `PRINTDATA`, which gives each its own console line.
                        let newline = Expr::str("\n");
                        for part in list.iter_mut() {
                            if part.len() < 2 {
                                continue;
                            }
                            let mut joined = Vec::with_capacity(part.len() * 2 - 1);
                            for (i, line) in part.drain(..).enumerate() {
                                if i > 0 {
                                    joined.push(newline.clone());
                                }
                                joined.push(line);
                            }
                            *part = joined;
                        }

                        Stmt::StrData(var, list)
                    }

                    // Everything left is a block delimiter, and every block
                    // parser above consumes its own. Reaching here means the
                    // opener is missing, which Emuera reports at load time
                    // through `ParserMediator.Warn(…, 2, true, false)` — a
                    // level-2 warning, i.e. a fatal parse error
                    // (`GameProc/ErbLoader.cs:1063-1420`).
                    ELSE | ELSEIF => error!(
                        pp.span(),
                        // `InvalidElse` (`_Library/EvilMask/Lang.cs:811`).
                        format!("IF～ENDIFの外で\"{inst}\"文が使われました")
                    ),
                    // `UnexpectedEndif` (`:813`).
                    ENDIF => error!(pp.span(), "対応するIFの無いENDIF文です"),
                    CASE | CASEELSE => error!(
                        pp.span(),
                        // `OutsideSelectcase` (`:807`).
                        format!("SELECTCASE構文の分岐の外に命令\"{inst}\"が含まれています")
                    ),
                    // `UnexpectedEndselect` (`:816`).
                    ENDSELECT => error!(pp.span(), "対応するSELECTCASEの無いENDSELECT文です"),
                    // `MissingCorresponding` (`:818`), with `getParentFunc`'s
                    // pairing (`FunctionIdentifier.cs:465-468`).
                    REND | NEXT | WEND | LOOP => {
                        let parent = match inst {
                            REND => "REPEAT",
                            NEXT => "FOR",
                            WEND => "WHILE",
                            _ => "DO",
                        };
                        error!(
                            pp.span(),
                            format!("対応する\"{parent}\"の無い\"{inst}\"文です")
                        )
                    }
                    // `MissingTryc` (`:819`) and `UnexpectedEndcatch` (`:820`).
                    CATCH => error!(pp.span(), "対応するTRYC系命令がありません"),
                    ENDCATCH => error!(pp.span(), "対応するCATCHのないENDCATCHです"),
                    // `UnexpectedDatalist` (`:825`) and `UnexpectedEndlist`
                    // (`:826`).
                    DATALIST => error!(pp.span(), "対応するPRINTDATA系命令のないDATALISTです"),
                    ENDLIST => error!(pp.span(), "対応するDATALISTのないENDLISTです"),
                    // `MissingPrintdata` (`:828`) and
                    // `MissingPrintdataStrdata` (`:829`).
                    DATA | DATAFORM => error!(
                        pp.span(),
                        format!("対応するPRINTDATA系命令のない\"{inst}\"です")
                    ),
                    ENDDATA => error!(
                        pp.span(),
                        format!("対応するPRINTDATA系命令もしくはSTRDATAのない\"{inst}\"です")
                    ),
                    // The lexer never leaves these as an `InstLine`: it splits
                    // the initialiser off into `EraLine::VarDecl`, which the
                    // function loop handles beside `#DIM`.
                    VARI | VARS => error!(
                        pp.span(),
                        format!("\"{inst}\" reached statement parsing as a plain instruction")
                    ),
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
            EraLine::VarDecl {
                is_str,
                decl,
                init,
            } => match self.parse_var_decl(is_str, decl, init, pp)? {
                Some(stmt) => stmt,
                // A declaration with no initialiser has no runtime effect at
                // all; it only registered the variable with the function.
                None => return Ok(None),
            },
            EraLine::SharpLine { .. } | EraLine::FunctionLine(_) => {
                error!(
                    pp.span(),
                    format!("Invalid line `{line:?}` for parsing as statement")
                )
            }
        };

        Ok(Some(StmtWithPos(stmt, pos)))
    }

    /// Emuera's `#`-directive handling, `LogicalLineParser.cs:36-266`.
    ///
    /// `Err` is the one shape Emuera has no equivalent of: a directive erars
    /// cannot make sense of at all, which poisons its function. Everything
    /// Emuera reports as a level-1 warning goes to `pp.warn` and everything it
    /// reports as level 2 goes to `errors`, and in both cases the directive is
    /// ignored and the function survives — which is what Emuera does with the
    /// line (`break` inside the switch).
    fn push_info(
        &self,
        sharp: SharpCode,
        args: &str,
        label: StrKey,
        pp: &mut Preprocessor,
        infos: &mut Vec<FunctionInfo>,
        errors: &mut Vec<ParserError>,
    ) -> ParserResult<()> {
        // `label.IsEvent` (`GameProc/LogicalLine.cs:196-230`): the name is one
        // of the nine event functions.
        let is_event = || label.resolve().parse::<EventType>().is_ok();
        // `label.IsMethod`: `#FUNCTION`/`#FUNCTIONS` was read for this
        // function already.
        let is_method = || {
            infos
                .iter()
                .any(|i| matches!(i, FunctionInfo::Function | FunctionInfo::FunctionS))
        };
        let event_flag = || {
            infos.iter().rev().find_map(|i| match i {
                FunctionInfo::EventFlag(f) => Some(*f),
                _ => None,
            })
        };

        // `#PRI`/`#LATER`/`#SINGLE` share one shape: four reasons to ignore the
        // directive, each its own level-1 warning (`:36-108`).
        macro_rules! event_flag {
            ($flag:expr) => {{
                let flag = $flag;
                if is_method() {
                    pp.warn(sharp_msg::use_user_func(flag));
                } else if !is_event() {
                    pp.warn(sharp_msg::usable_event_func(flag));
                } else if event_flag() == Some(flag) {
                    pp.warn(sharp_msg::duplicate_flag(flag));
                } else if event_flag() == Some(EventFlags::Only) {
                    // `#ONLY` wins: it was declared first and every other flag
                    // of this function is dead (`:52-56`, `:75-79`, `:100-104`).
                    pp.warn(sharp_msg::only_with(flag));
                } else {
                    // DELIBERATE: Emuera's four flags are independent booleans,
                    // so `#PRI` + `#LATER` lands the body in *both* the pri and
                    // the later group and it runs twice (`LabelDictionary.cs:101-104`,
                    // 「eramakerの仕様」). `EventFlags` is one enum, so the later
                    // directive wins and the body runs once. The warning below
                    // is Emuera's; the divergence is §5 of
                    // docs/research/2026-09-03-emuera-command-gap.md.
                    if flag == EventFlags::Later && event_flag() == Some(EventFlags::Pre) {
                        pp.warn(sharp_msg::pri_with_later());
                    }
                    infos.push(FunctionInfo::EventFlag(flag));
                }
            }};
        }

        match sharp {
            SharpCode::DEFINE => {
                error!(pp.span(), "#DEFINE only avaliable in ERH")
            }
            // A function-local `#DIM` is *not* deferred: Emuera tried and gave
            // up on it (`GameProc/UserDefinedVariable.cs:33`, "1822
            // Privateの方もDIMだけ遅延させようとしたけどちょっと課題がおおいので
            // やめとく"), so a local declaration still has to be sizeable the
            // moment it is read. Failing to fold the size is a diagnostic here,
            // never a panic.
            SharpCode::DIM => {
                let decl = try_nom!(pp, self::expr::dim_line(self, false)(args)).1;
                let var = match self.header.as_ref().finish_dim(decl) {
                    Ok(var) => var,
                    Err(err) => error!(pp.span(), err.to_string()),
                };
                infos.push(FunctionInfo::Dim(var));
            }
            SharpCode::DIMS => {
                let decl = try_nom!(pp, self::expr::dim_line(self, true)(args)).1;
                let var = match self.header.as_ref().finish_dim(decl) {
                    Ok(var) => var,
                    Err(err) => error!(pp.span(), err.to_string()),
                };
                self.local_strs.borrow_mut().insert(var.var);
                infos.push(FunctionInfo::Dim(var));
            }
            // `#FUNCTION`/`#FUNCTIONS` (`:145-198`). Declaring the same one
            // twice is a level-1 warning and declaring the other one a level-2
            // error; either way the line is dropped, so both can never hold at
            // once — which is what the two `assert!`s in
            // `erars-vm/src/function.rs` used to enforce by panicking on game
            // text.
            SharpCode::FUNCTION | SharpCode::FUNCTIONS => {
                let is_str = sharp == SharpCode::FUNCTIONS;
                let already_int = infos.contains(&FunctionInfo::Function);
                let already_str = infos.contains(&FunctionInfo::FunctionS);

                // 「関数名が数字で始まっています」: the directive is dropped and
                // the function itself is poisoned (`label.IsError = true`).
                if label.resolve().starts_with(|c: char| c.is_ascii_digit()) {
                    pp.warn(sharp_msg::can_not_declared_begin_number_function(sharp));
                    error!(pp.span(), sharp_msg::func_name_begin_number())
                }

                match (already_int, already_str) {
                    (true, _) if !is_str => {
                        pp.warn(sharp_msg::already_sharp_declared(label.resolve(), sharp))
                    }
                    (_, true) if is_str => {
                        pp.warn(sharp_msg::already_sharp_declared(label.resolve(), sharp))
                    }
                    (true, _) => errors.push((
                        sharp_msg::already_declared_sharp_function(label.resolve(), false),
                        pp.span(),
                    )),
                    (_, true) => errors.push((
                        sharp_msg::already_declared_sharp_function(label.resolve(), true),
                        pp.span(),
                    )),
                    (false, false) => {
                        // `label.Depth == 0` (`:167-171`, level 2): a system
                        // function is never an expression function. DELIBERATE:
                        // Emuera's `Depth` covers its whole system-label set
                        // (`GameData/IdentifierDictionary.cs:74-116`:
                        // `SHOW_STATUS`, `USERSHOP`, `COM<n>`, `ABLUP<n>`, …),
                        // which erars does not enumerate at parse time, so only
                        // the nine event names are caught here. §5 of
                        // docs/research/2026-09-03-emuera-command-gap.md.
                        if is_event() {
                            errors.push((sharp_msg::use_sharp_in_system_func(sharp), pp.span()));
                            return Ok(());
                        }

                        // An expression function has no event flags: Emuera
                        // clears all four, one level-1 warning each (`:178-197`).
                        if let Some(flag) = event_flag() {
                            pp.warn(sharp_msg::use_user_func(flag));
                            infos.retain(|i| !matches!(i, FunctionInfo::EventFlag(_)));
                        }

                        infos.push(match is_str {
                            true => FunctionInfo::FunctionS,
                            false => FunctionInfo::Function,
                        });
                    }
                }
            }
            // `#LOCALSIZE`/`#LOCALSSIZE` (`:199-253`). Every rejection leaves
            // the size at the `!VariableSize.csv` default instead of dropping
            // `LOCAL` altogether, which is what folding this at load time in
            // `insert_compiled_func` used to do.
            SharpCode::LOCALSIZE | SharpCode::LOCALSSIZE => {
                let is_str = sharp == SharpCode::LOCALSSIZE;

                // `wc.EOL`, level 2.
                if args.trim().is_empty() {
                    errors.push((sharp_msg::sharp_has_not_valid_value(sharp), pp.span()));
                    return Ok(());
                }

                // 「イベント関数では指定しても無視される」 (`:207-212`).
                if is_event() {
                    pp.warn(sharp_msg::event_func_ignore_specified(sharp));
                    return Ok(());
                }

                // `ReduceIntegerTerm` + `Restructure` must yield a constant
                // integer (`:213-218`); anything else is level 2.
                let size = match self::expr::expr(self)(args) {
                    Ok((_, size)) => self.header.as_ref().const_eval(&size).ok(),
                    Err(_) => None,
                };
                let Some(Value::Int(size)) = size else {
                    errors.push((sharp_msg::sharp_has_not_valid_value(sharp), pp.span()));
                    return Ok(());
                };

                if size <= 0 {
                    pp.warn(sharp_msg::localsize_less_than_1(sharp, size));
                    return Ok(());
                }
                if size >= i32::MAX as i64 {
                    pp.warn(sharp_msg::too_many_localsize(sharp, size));
                    return Ok(());
                }

                let duplicate = infos.iter().any(|i| match i {
                    FunctionInfo::LocalSize(_) => !is_str,
                    FunctionInfo::LocalSSize(_) => is_str,
                    _ => false,
                });
                if duplicate {
                    // 「以前の定義は無視されます」: the last one wins.
                    pp.warn(sharp_msg::duplicate_localsize(sharp));
                }

                let size = size as u32;
                infos.push(match is_str {
                    true => FunctionInfo::LocalSSize(size),
                    false => FunctionInfo::LocalSize(size),
                });
            }
            SharpCode::PRI => event_flag!(EventFlags::Pre),
            SharpCode::LATER => event_flag!(EventFlags::Later),
            SharpCode::SINGLE => event_flag!(EventFlags::Single),
            // `#ONLY` (`:109-144`). It replaces whatever the other three set,
            // with one warning each, and is the reason they refuse to run
            // after it.
            SharpCode::ONLY => {
                if is_method() {
                    pp.warn(sharp_msg::use_user_func(EventFlags::Only));
                } else if !is_event() {
                    pp.warn(sharp_msg::usable_event_func(EventFlags::Only));
                } else if event_flag() == Some(EventFlags::Only) {
                    pp.warn(sharp_msg::duplicate_flag(EventFlags::Only));
                } else {
                    if let Some(flag) = event_flag() {
                        pp.warn(sharp_msg::be_ignore(flag));
                        infos.retain(|i| !matches!(i, FunctionInfo::EventFlag(_)));
                    }
                    infos.push(FunctionInfo::EventFlag(EventFlags::Only));
                }
            }
        }
        Ok(())
    }

    /// Lowers a `VARI`/`VARS` line: the declaration is queued onto
    /// `local_dims` for the enclosing function and an initialiser becomes an
    /// ordinary assignment statement where the line sits.
    ///
    /// The `.NET版` fork documents these as "`#DIM` と違い、任意の位置で宣言する
    /// ことが可能" and "関数を抜けるたびに破棄されるため、`#DIM DYNAMIC` とほぼ
    /// 同じ挙動をする", so the declaration is always dynamic, and it is a
    /// declaration rather than a statement: the corpus declares inside one
    /// branch of an `IF` and reads the variable after the `ENDIF`
    /// (`RPG/イベント/EVENT_201_白き鋼鉄のX2/EVENT_201_91_BATTLE_6_楽土.ERB:188`),
    /// which only works if the name is function-scoped.
    ///
    /// The initialiser cannot ride along in `VariableInfo::init` the way
    /// `#DIM`'s does: the corpus initialises from runtime state
    /// (`VARI L_GUEST = RESULT:1` right after a `CALL`,
    /// `VARI L_LINE = LINECOUNT`), which only holds the right value at this
    /// point in the body, not at function entry where a dynamic variable is
    /// created.
    fn parse_var_decl(
        &self,
        is_str: bool,
        decl: &str,
        init: Option<&str>,
        pp: &mut Preprocessor,
    ) -> ParserResult<Option<Stmt>> {
        let decl = try_nom!(pp, self::expr::dim_line(self, is_str)(decl)).1;
        let mut var = match self.header.as_ref().finish_dim(decl) {
            Ok(var) => var,
            Err(err) => error!(pp.span(), err.to_string()),
        };
        var.info.is_dynamic = true;

        let name = var.var;
        if is_str {
            // Has to happen before the initialiser is parsed: a string
            // assignment's right hand side is a form string, and that is
            // decided by looking the name up here.
            self.local_strs.borrow_mut().insert(name);
        }
        self.local_dims.borrow_mut().push(var);

        match init {
            Some(init) => {
                let rhs = if is_str {
                    try_nom!(pp, self::expr::form_arg_expr(self)(init)).1
                } else {
                    try_nom!(pp, self::expr::expr(self)(init)).1
                };

                Ok(Some(Stmt::Assign(
                    Variable {
                        var: name,
                        func_extern: None,
                        args: Vec::new(),
                    },
                    None,
                    rhs,
                )))
            }
            None => Ok(None),
        }
    }

    /// Pre-scans the function body for `VARS` declarations to hoist their
    /// names into `local_strs`.
    ///
    /// In the `.NET版` fork (`LogicalLineParser.cs:423-529`, `ErbLoader.cs:463`),
    /// `VARS` and `VARI` declare dynamic local variables at any location in a
    /// function body ("任意の位置で宣言することが可能"), and are registered
    /// via `parentLine.AddPrivateVariable` during line parsing while SET
    /// arguments are parsed afterwards by `setLabelsArg()` (`ErbLoader.cs:105,170`)
    /// or at runtime. Every `VARS` name in a function is therefore visible to
    /// every line regardless of position. Forward references to a `VARS`
    /// variable (such as `VAR = %FORM%` before `VARS VAR`) correctly parse the
    /// right-hand side as a form string rather than an integer expression.
    fn hoist_var_decls(&self, pp: &Preprocessor) {
        let mut scan_pp = pp.clone();
        let mut b = Bump::new();
        let mut prev_len = scan_pp.left_text().len();
        loop {
            b.reset();
            match scan_pp.next_line(&b) {
                Ok(Some(EraLine::FunctionLine(_))) | Ok(None) => break,
                Ok(Some(EraLine::VarDecl { is_str: true, decl, .. })) => {
                    if let Ok((_, decl)) = self::expr::dim_line(self, true)(decl) {
                        self.local_strs.borrow_mut().insert(decl.var);
                    }
                }
                Ok(Some(_)) => {}
                Err(_) => {
                    let cur_len = scan_pp.left_text().len();
                    if cur_len >= prev_len {
                        break;
                    }
                    prev_len = cur_len;
                }
            }
        }
    }

    /// Compiles every function in one ERB, keeping the ones that compile.
    ///
    /// Recovery follows Emuera line by line wherever it can. A line that fails
    /// on its own is kept as a throwing `InvalidLine`
    /// (`GameProc/ErbLoader.cs:403-407`, `:423-427`,
    /// `GameProc/LogicalLine.cs:74-85`) so the rest of its function stays
    /// callable, and a nest-check failure such as `CONTINUE` outside a loop
    /// (`:1041-1058`) is only a warning, exactly as in Emuera: it never sets
    /// the `noError` flag (`:353`, `:366`, `:405`, `:426`) that the game-start
    /// refusal in `GameProc/Process.SystemProc.cs:173-186` is keyed to.
    ///
    /// Only two failures are coarser. A line whose parse already swallowed the
    /// lines after it — a block opener such as `IF` or `FOR`, which erars
    /// parses recursively where Emuera pairs it up in a later pass — leaves no
    /// safe place to resume, so the rest of that function is skipped. And a
    /// label erars cannot read poisons the file, which is the one case Emuera
    /// also treats that way: `InvalidLabelLine` sets `noError`
    /// (`GameProc/ErbLoader.cs:366`).
    ///
    /// DELIBERATE: erars reports an unreadable line and still starts the game;
    /// Emuera refuses to start unless `解釈不可能な行があっても実行する` is on.
    /// See `docs/research/2026-09-03-emuera-command-gap.md` §5.
    pub fn parse_and_compile<'s>(
        &self,
        pp: &mut Preprocessor<'s>,
        b: &mut Bump,
    ) -> ParserResult<CompiledErb> {
        let s = pp.left_text();
        let has_vars = contains_vars(s);
        // `CompiledFunction` is 112 bytes, so the old `with_capacity(1024)`
        // reserved 112 KiB per file. Measured over the corpus a file defines
        // 20.5 functions on average (median 5, p90 64, p99 221) and exactly
        // one of 873 files exceeds 1024, so growth from empty — as `parse`
        // below already does — costs a handful of reallocs and keeps the
        // buffer proportional to the file.
        let mut out = Vec::new();
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        // `ScriptPosition::line` is 1-based — the lexer increments `line_pos`
        // before it hands the line over (`erars-lexer/src/lib.rs:295`) and the
        // call stack prints it as `@N` (`erars-ast/src/ast.rs:97-100`) — so
        // indexing `lines()` with it names the line after the offending one.
        let line_span = |pos: ScriptPosition| {
            s.lines().nth(pos.line.saturating_sub(1) as usize).map(|line| {
                let diff = line.as_ptr() as usize - s.as_ptr() as usize;
                diff..diff + line.len()
            })
        };

        match pp.next_line(b)? {
            Some(EraLine::FunctionLine(mut func_line)) => 'outer: loop {
                self.local_strs.borrow_mut().clear();
                self.local_dims.borrow_mut().clear();
                if has_vars {
                    self.hoist_var_decls(pp);
                }
                let mut compiler = Compiler::new();
                // A label erars cannot read is the one case Emuera also treats
                // as poisoning the load: `InvalidLabelLine` sets `noError`
                // (`GameProc/ErbLoader.cs:366`), which is what refuses to start
                // the game. Nothing after it can be attributed to a function.
                let (label, args) = try_nom!(pp, self::expr::function_line(self)(func_line)).1;
                let label = self.intern_ident(&label);

                let mut infos = Vec::new();
                // Set by the first failure in this function; the rest of its
                // lines are then only scanned for the next `@label`.
                let mut failed = false;

                macro_rules! finish {
                    () => {
                        if !failed {
                            infos.extend(
                                self.local_dims.borrow_mut().drain(..).map(FunctionInfo::Dim),
                            );
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
                        }
                    };
                }

                'inner: loop {
                    b.reset();

                    let left = pp.left_text().len();
                    let line = match pp.next_line(b) {
                        Ok(line) => line,
                        Err(err) => {
                            // `next_raw_line` has already consumed the physical
                            // line by the time any of these errors is raised, so
                            // scanning continues from the next one. Bailing out
                            // when it did not advance keeps a future lexer path
                            // that fails without consuming from spinning here.
                            if pp.left_text().len() == left {
                                return Err(err);
                            }
                            errors.push(err);
                            failed = true;
                            continue 'inner;
                        }
                    };

                    match line {
                        Some(EraLine::FunctionLine(f)) => {
                            func_line = f;
                            finish!();
                            break 'inner;
                        }
                        None => {
                            finish!();
                            break 'outer;
                        }
                        _ if failed => continue 'inner,
                        Some(EraLine::SharpLine { sharp, args }) => {
                            if let Err(err) =
                                self.push_info(sharp, args, label, pp, &mut infos, &mut errors)
                            {
                                errors.push(err);
                                failed = true;
                            }
                        }
                        Some(line) => {
                            // A block opener parses its own body here, where
                            // Emuera pairs it up in a later pass, so a failure
                            // that already consumed the lines after this one
                            // leaves nowhere safe to resume: only then is the
                            // rest of the function skipped.
                            let before = pp.left_text().len();
                            let stmt = match self.parse_stmt(line, pp, b) {
                                Ok(Some(stmt)) => stmt,
                                Ok(None) => continue 'inner,
                                Err(err) => {
                                    if pp.left_text().len() == before {
                                        compiler.push_invalid_line(&err.0);
                                    } else {
                                        failed = true;
                                    }
                                    errors.push(err);
                                    continue 'inner;
                                }
                            };
                            if let Err(err) = compiler.push_stmt_with_pos(stmt) {
                                let span = line_span(compiler.current_pos())
                                    .unwrap_or_else(|| pp.span());
                                errors.push((err.to_string(), span));
                                failed = true;
                            }
                            // Emuera reports a line-compiler warning at level
                            // 2 (`GameProc/ErbLoader.cs:1041-1058`).
                            warnings.extend(compiler.warnings.drain(..).map(|(msg, pos)| {
                                let span = line_span(pos).unwrap_or_else(|| 0..0);
                                (msg, span, 2)
                            }));
                        }
                    }
                }
            },
            Some(_) => {
                error!(pp.span(), "First line should be function line");
            }
            // A file with no function at all still had its `[…]` directives
            // read, so its warnings have to come out here too.
            None => {
                return Ok(CompiledErb {
                    warnings: pp_warnings(pp),
                    ..CompiledErb::default()
                })
            }
        };

        // The `[…]` preprocessor's own level-1 warnings, collected while the
        // lines above were read.
        warnings.extend(pp_warnings(pp));

        Ok(CompiledErb {
            functions: out,
            errors,
            warnings,
        })
    }

    pub fn parse(&self, pp: &mut Preprocessor, b: &mut Bump) -> ParserResult<Vec<Function>> {
        let mut out = Vec::new();
        let has_vars = pp.left_text().as_bytes().windows(4).any(|w| w.eq_ignore_ascii_case(b"vars"));
        match pp.next_line(b)? {
            Some(EraLine::FunctionLine(mut func_line)) => 'outer: loop {
                self.local_strs.borrow_mut().clear();
                self.local_dims.borrow_mut().clear();
                if has_vars {
                    self.hoist_var_decls(pp);
                }
                let mut body = Vec::new();
                let (label, args) = try_nom!(pp, self::expr::function_line(self)(func_line)).1;
                let label = self.intern_ident(&label);

                let mut infos = Vec::new();

                'inner: loop {
                    b.reset();
                    match pp.next_line(b)? {
                        Some(EraLine::FunctionLine(f)) => {
                            func_line = f;

                            infos.extend(
                                self.local_dims.borrow_mut().drain(..).map(FunctionInfo::Dim),
                            );
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
                            infos.extend(
                                self.local_dims.borrow_mut().drain(..).map(FunctionInfo::Dim),
                            );
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
                            // `parse` is the all-or-nothing path (tests, single
                            // statements): a level-2 directive diagnostic is an
                            // error here rather than something a caller could
                            // miss. Level-1 warnings ride `pp` as everywhere.
                            let mut errors = Vec::new();
                            self.push_info(sharp, args, label, pp, &mut infos, &mut errors)?;
                            if let Some(err) = errors.pop() {
                                return Err(err);
                            }
                        }
                        Some(line) => {
                            body.extend(self.parse_stmt(line, pp, b)?);
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

impl<'p> ParserContext<'p> {
    pub fn parse_program_str(&self, s: &str) -> ParserResult<Vec<Function>> {
        let mut pp = self.preprocessor(s);
        let mut b = Bump::new();
        self.parse(&mut pp, &mut b)
    }

    pub fn parse_function_str(&self, s: &str) -> ParserResult<Function> {
        self.parse_program_str(s).map(|f| f.into_iter().next().unwrap())
    }

    pub fn parse_expr_str(&self, s: &str) -> ParserResult<Expr> {
        Ok(try_nom!(@str s, self::expr::expr(self)(s.trim_start_matches('\u{feff}'))).1)
    }

    pub fn parse_body_str(&self, s: &str) -> ParserResult<Vec<StmtWithPos>> {
        let mut pp = self.preprocessor(s);
        let mut b = Bump::new();
        let mut body = Vec::new();
        self.local_strs.borrow_mut().clear();
        self.local_dims.borrow_mut().clear();
        if s.as_bytes().windows(4).any(|w| w.eq_ignore_ascii_case(b"vars")) {
            self.hoist_var_decls(&pp);
        }
        while let Some(line) = pp.next_line(&b)? {
            body.extend(self.parse_stmt(line, &mut pp, &b)?);
            b.reset();
        }

        Ok(body)
    }

    pub fn parse_stmt_str(&self, s: &str) -> ParserResult<StmtWithPos> {
        let mut pp = self.preprocessor(s);
        let b = Bump::new();
        self.local_strs.borrow_mut().clear();
        self.local_dims.borrow_mut().clear();
        if s.as_bytes().windows(4).any(|w| w.eq_ignore_ascii_case(b"vars")) {
            self.hoist_var_decls(&pp);
        }
        match pp.next_line(&b)? {
            Some(line) => match self.parse_stmt(line, &mut pp, &b)? {
                Some(stmt) => Ok(stmt),
                None => error!(pp.span(), "No stmt"),
            },
            None => error!(pp.span(), "No stmt"),
        }
    }
}

#[cfg(test)]
mod language_tests {
    use super::{EraConfig, Language};

    #[test]
    fn labels_round_trip() {
        for (label, lang) in [
            ("JAPANESE", Language::Japanese),
            ("KOREAN", Language::Korean),
            ("CHINESE_HANS", Language::ChineseHans),
            ("CHINESE_HANT", Language::ChineseHant),
        ] {
            assert_eq!(label.parse::<Language>().unwrap(), lang, "{label}");
            assert_eq!(lang.to_string(), label);
        }
        assert!("ENGLISH".parse::<Language>().is_err());
    }

    #[test]
    fn encoding_per_language() {
        assert_eq!(Language::Japanese.encoding(), encoding_rs::SHIFT_JIS);
        assert_eq!(Language::Korean.encoding(), encoding_rs::EUC_KR);
        assert_eq!(Language::ChineseHans.encoding(), encoding_rs::GBK);
        assert_eq!(Language::ChineseHant.encoding(), encoding_rs::BIG5);
    }

    #[test]
    fn chinese_config_labels_select_the_right_code_page() {
        // Regression: the CHINESE_HANS / CHINESE_HANT strum labels were swapped,
        // so a simplified-Chinese game got Big5.
        let hans = EraConfig::from_text("内部で使用する東アジア言語:CHINESE_HANS\n").unwrap();
        assert_eq!(hans.lang, Language::ChineseHans);
        assert_eq!(hans.lang.encoding(), encoding_rs::GBK);

        let hant = EraConfig::from_text("内部で使用する東アジア言語:CHINESE_HANT\n").unwrap();
        assert_eq!(hant.lang, Language::ChineseHant);
        assert_eq!(hant.lang.encoding(), encoding_rs::BIG5);
    }
}

#[cfg(test)]
mod config_tests {
    use super::{
        color_to_int, parse_color, DisplayWarningFlag, EraConfig, EraConfigKey, Language,
        ReduceArgumentOnLoadFlag, ReplaceInfo,
    };
    use erars_ast::Value;

    #[test]
    fn defaults_match_emuera() {
        // Emuera ConfigData.cs:47-64
        let c = EraConfig::default();
        assert_eq!(c.lang, Language::Japanese);
        assert_eq!(c.max_log, 500);
        assert_eq!(c.printc_count, 3);
        assert_eq!(c.printc_width, 25);
        assert_eq!(c.font_family, "");
        assert_eq!(c.font_size, 18);
        assert_eq!(c.line_height, 19);
        assert_eq!(c.window_width, 760);
        assert_eq!(c.window_height, 480);
        assert_eq!(c.fore_color, [192, 192, 192]);
        assert_eq!(c.bg_color, [0, 0, 0]);
        assert_eq!(c.focus_color, [255, 255, 0]);
    }

    #[test]
    fn from_text_parses_colour_keys() {
        let text = "\u{feff}内部で使用する東アジア言語:KOREAN\r\n\
                    文字色:255, 200,100\r\n\
                    背景色:16,16,16\r\n\
                    選択中文字色:0,255,255\r\n\
                    PRINTCの文字数:30\r\n";
        let c = EraConfig::from_text(text).unwrap();
        assert_eq!(c.lang, Language::Korean);
        assert_eq!(c.fore_color, [255, 200, 100]);
        assert_eq!(c.bg_color, [16, 16, 16]);
        assert_eq!(c.focus_color, [0, 255, 255]);
        assert_eq!(c.printc_width, 30);
    }

    #[test]
    fn invalid_colour_keeps_default() {
        let c = EraConfig::from_text("文字色:300,0,0\n背景色:1,2\n選択中文字色:red\n").unwrap();
        assert_eq!(c.fore_color, [192, 192, 192]);
        assert_eq!(c.bg_color, [0, 0, 0]);
        assert_eq!(c.focus_color, [255, 255, 0]);
    }

    #[test]
    fn parse_color_follows_emuera_try_strings_to_color() {
        assert_eq!(parse_color("192,192,192"), Some([192, 192, 192]));
        assert_eq!(parse_color(" 1 , 2 , 3 "), Some([1, 2, 3]));
        // Emuera ignores tokens after the third.
        assert_eq!(parse_color("1,2,3,4"), Some([1, 2, 3]));
        assert_eq!(parse_color("1,2"), None);
        assert_eq!(parse_color("256,0,0"), None);
        assert_eq!(parse_color("-1,0,0"), None);
        assert_eq!(parse_color("red"), None);
        assert_eq!(parse_color(""), None);
    }

    #[test]
    fn get_config_packs_colours_as_rrggbb() {
        // Emuera ConfigData.GetConfigValueInERB: ((R * 256) + G) * 256 + B
        assert_eq!(color_to_int([192, 192, 192]), 0xC0C0C0);
        let c = EraConfig::default();
        let r = ReplaceInfo::default();
        assert_eq!(c.get_config(EraConfigKey::ForeColor, &r), Value::Int(0xC0C0C0));
        assert_eq!(c.get_config(EraConfigKey::BgColor, &r), Value::Int(0));
        assert_eq!(c.get_config(EraConfigKey::FocusColor, &r), Value::Int(0xFFFF00));
        assert_eq!(c.get_config(EraConfigKey::LogColor, &r), Value::Int(0xC0C0C0));
        assert_eq!(c.get_config(EraConfigKey::PrintcWidth, &r), Value::Int(25));
        assert_eq!(c.get_config(EraConfigKey::PrintcCount, &r), Value::Int(3));
        assert_eq!(
            c.get_config(EraConfigKey::FontFamily, &r),
            Value::String(String::new())
        );
    }

    #[test]
    fn get_config_reports_bools_as_ints_and_reads_replace_items() {
        // Emuera reports a boolean item as 1/0 (`ConfigData.cs:505-511`) and
        // finds `_replace.csv` items through the same lookup (`:385-397`).
        let c = EraConfig::default();
        let r = ReplaceInfo::default();
        assert_eq!(c.get_config(EraConfigKey::AutoSave, &r), Value::Int(1));
        assert_eq!(c.get_config(EraConfigKey::UseSaveFolder, &r), Value::Int(0));
        assert_eq!(c.get_config(EraConfigKey::MoneyFirst, &r), Value::Int(1));
        assert_eq!(c.get_config(EraConfigKey::MaxShopItem, &r), Value::Int(100));
        assert_eq!(
            c.get_config(EraConfigKey::MoneyLabel, &r),
            Value::String("$".into())
        );
        assert_eq!(
            c.get_config(EraConfigKey::LoadLabel, &r),
            Value::String("Now Loading...".into())
        );
    }

    #[test]
    fn behaviour_keys_parse_from_their_japanese_labels() {
        let c = EraConfig::from_text(
            "表示する最低警告レベル:2\nイベント関数のCALLを許可する:NO\n_Rename.csvを利用する:YES\nサブディレクトリを検索する:YES\n関数が見つからない警告の扱い:ONCE\nロード時に引数を解析する:YES\n履歴文字色:10,20,30\n",
        )
        .unwrap();
        assert_eq!(c.display_warning_level, 2);
        assert!(!c.compati_call_event);
        assert!(c.use_rename_file);
        assert!(c.search_subdirectory);
        assert_eq!(c.function_not_found_warning, DisplayWarningFlag::Once);
        assert_eq!(c.reduce_argument_on_load, ReduceArgumentOnLoadFlag::Yes);
        assert_eq!(c.log_color, [10, 20, 30]);
    }

    #[test]
    fn colour_keys_parse_from_their_japanese_labels() {
        assert!(matches!(
            "文字色".parse::<EraConfigKey>(),
            Ok(EraConfigKey::ForeColor)
        ));
        assert!(matches!(
            "背景色".parse::<EraConfigKey>(),
            Ok(EraConfigKey::BgColor)
        ));
        assert!(matches!(
            "選択中文字色".parse::<EraConfigKey>(),
            Ok(EraConfigKey::FocusColor)
        ));
        assert_eq!(EraConfigKey::ForeColor.to_string(), "文字色");
    }
}

#[cfg(test)]
mod scan_tests {
    use super::contains_vars;

    /// The naive `windows(4).any(…)` this replaced, kept as the oracle.
    fn naive(s: &str) -> bool {
        s.as_bytes().windows(4).any(|w| w.eq_ignore_ascii_case(b"vars"))
    }

    #[test]
    fn answers_exactly_like_a_window_scan() {
        for s in [
            "",
            "V",
            "VAR",
            "VARS",
            "vars",
            "VaRs",
            "\tVARS NAME",
            "@FUNC\nVARI N\nVARS S\n",
            "@FUNC\nVARI N\n",
            // `V` right at the end, with fewer than three bytes after it.
            "PRINTL AV",
            "PRINTL AVA",
            "PRINTL AVAR",
            // A false start has to keep scanning.
            "VAVARS",
            "vvvvvars",
            // Non-ASCII around the match, and a `V` inside a UTF-8 sequence.
            "PRINTL 안녕 VARS 하세요",
            "PRINTL 안녕하세요",
        ] {
            assert_eq!(contains_vars(s), naive(s), "{s:?}");
        }
    }
}
