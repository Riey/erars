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
    SumArray,
    IsSkip,

    Escape = 20,
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
    Unicode,

    GetExpLv = 40,
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
    CurrentAlign,
    CurrentRedraw,

    Rand = 60,
    ChkData,
    ChkCharaData,
    FindChara,
    #[strum(serialize = "FIND_CHARADATA")]
    FindCharaData,

    CsvName = 70,
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
    SaveNos,
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
    ArrayShift,
    Varset,
    CVarset,
    Split,
    ForceKana,

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
    ChkFont,

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
}
