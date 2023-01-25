use serde::{Deserialize, Serialize};
use strum::{Display, EnumString, IntoStaticStr};

#[derive(
    Display, Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, IntoStaticStr, EnumString,
)]
#[strum(serialize_all = "UPPERCASE")]
#[serde(rename_all = "UPPERCASE")]
#[allow(non_camel_case_types)]
#[repr(u32)]
pub enum BuiltinMethod {
    ToStr = 0,
    ToInt,
    Limit,
    Min,
    Max,
    Power,
    Sqrt,
    Abs,
    Sign,
    Log,
    Log10,
    InRange,
    LineIsEmpty,
    GroupMatch,
    NoSames,
    AllSames,
    // No free form
    SumArray,
    // No free form
    SumCArray,
    IsSkip,
    MouseSkip,
    MesSkip,
    Convert,
    FindElement,
    FindLastElement,
    // No free form
    Match,
    // No free form
    CMatch,
    // No free form
    MaxArray,
    // No free form
    MinArray,
    // No free form
    MaxCArray,
    // No free form
    MinCArray,
    VarSize,

    Escape = 40,
    Replace,
    StrLenS,
    StrLenSU,
    StrCount,
    SubString,
    SubStringU,
    StrFind,
    StrFindU,
    StrJoin,
    BarStr,
    MoneyStr,
    Unicode,
    // different from command
    EncodeToUni,
    ToUpper,
    ToLower,
    ToHalf,
    ToFull,
    IsNumeric,

    GetExpLv = 60,
    GetPalamLv,
    GetColor,
    GetDefColor,
    GetBgColor,
    GetDefBgColor,
    GetFocusColor,
    GetFont,
    GetChara,
    GetBit,
    GetTime,
    GetTimeS,
    GetMillisecond,
    GetSecond,
    GetNum,
    CurrentAlign,
    CurrentRedraw,
    ChkFont,

    Rand = 80,
    ChkData,
    ChkCharaData,
    FindChara,
    #[strum(serialize = "FIND_CHARADATA")]
    FindCharaData,
    ExistCsv,
    SaveNos,
    GetConfig,
    GetConfigS,

    CsvName = 100,
    CsvCallName,
    CsvMasterName,
    CsvNickName,
    CsvBase,
    CsvCstr,
    CsvAbl,
    CsvTalent,
    CsvMark,
    CsvExp,
    CsvEx,
    CsvRelation,
    CsvJuel,
    CsvEquip,
    CsvCflag,

    SpriteCreated = 200,
    GCreated = 250,
}

#[derive(Display, Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, IntoStaticStr)]
#[strum(serialize_all = "UPPERCASE")]
#[serde(rename_all = "UPPERCASE")]
#[repr(u32)]
#[allow(non_camel_case_types)]
pub enum BuiltinCommand {
    Return = 0,
    ReturnF,
    Restart,
    Quit,
    Throw,
    DoTrain,
    CallTrain,

    Input = 10,
    InputS,
    TInput,
    TInputS,
    OneInput,
    OneInputS,
    TOneInput,
    TOneInputS,
    ForceWait,
    Wait,
    WaitAnykey,
    Twait,

    ResetStain = 25,
    ResetData,
    SaveData,
    LoadData,
    DelData,
    SaveGame,
    LoadGame,
    SaveGlobal,
    LoadGlobal,
    PutForm,

    UpCheck = 40,
    CUpCheck,
    SkipDisp,
    NoSkip,
    EndNoSkip,
    Swap,
    SetBit,
    ClearBit,
    InvertBit,
    Varset,
    CVarset,
    Split,
    ForceKana,
    // different from method
    EncodeToUni,
    // different from method
    GetTime,
    // different from method
    Power,

    Redraw = 60,
    DrawLine,
    CustomDrawLine,
    ClearLine,
    SetColor,
    SetBgColor,
    ResetColor,
    ResetBgColor,
    SetColorByName,
    SetBgColorByName,

    FontBold,
    FontItalic,
    FontRegular,
    FontStyle,
    GetStyle,
    SetFont,

    Bar = 89,

    SaveChara = 90,
    LoadChara,
    AddChara,
    AddDefChara,
    AddCopyChara,
    CopyChara,
    DelChara,
    SwapChara,
    SortChara,
    PickupChara,

    Randomize = 110,
    DumpRand,
    InitRand,

    ArrayShift = 200,
    ArrayRemove,
    ArraySort,
    ArrayCopy,
    ArrayMove,

    #[strum(serialize = "HTML_PRINT")]
    HtmlPrint = 300,
    SpriteCreate,
}
