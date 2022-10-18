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
    ToStr,
    ToInt,

    IsSkip,

    Limit,
    Min,
    Max,
    Power,
    Sqrt,
    Abs,
    Sign,
    InRange,
    LineIsEmpty,
    GroupMatch,
    SumArray,

    Escape,
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
    GetExpLv,
    GetPalamLv,
    GetColor,
    GetDefColor,
    GetBgColor,
    GetDefBgColor,
    GetFocusColor,
    GetFont,

    GetChara,
    GetBit,
    Log,
    Log10,

    GetTime,
    Rand,

    ChkData,
    ChkCharaData,
    FindChara,
    #[strum(serialize = "FIND_CHARADATA")]
    FindCharaData,

    CsvName,
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
    UpCheck,
    CUpCheck,

    Input,
    InputS,
    TInput,
    TInputS,
    OneInput,
    OneInputS,
    TOneInput,
    TOneInputS,
    Wait,
    WaitAnykey,

    SkipDisp,
    NoSkip,
    EndNoSkip,

    Split,

    Return,
    ReturnF,
    Restart,
    Quit,
    Throw,
    DoTrain,
    CallTrain,

    Redraw,
    DrawLine,
    CustomDrawLine,
    ClearLine,

    Swap,
    PutForm,

    ResetStain,

    ResetData,
    SaveData,
    LoadData,
    DelData,
    SaveNos,

    SaveGame,
    LoadGame,

    SaveGlobal,
    LoadGlobal,

    SaveChara,
    LoadChara,
    AddDefChara,
    AddChara,
    DelChara,
    SwapChara,
    SortChara,
    PickupChara,

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

    Bar,

    SetBit,
    ClearBit,
    InvertBit,

    ArrayShift,

    Varset,
    CVarset,
}
