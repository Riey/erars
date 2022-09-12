use serde::{Deserialize, Serialize};
use strum::{Display, EnumString, IntoStaticStr};

#[derive(
    Display, Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, IntoStaticStr, EnumString,
)]
#[strum(serialize_all = "UPPERCASE")]
#[serde(rename_all = "UPPERCASE")]
#[allow(non_camel_case_types)]
pub enum BuiltinMethod {
    ToStr,
    ToInt,

    Limit,
    Min,
    Max,
    Power,
    Sqrt,
    Abs,
    LineIsEmpty,
    GroupMatch,

    StrLenS,
    StrLenSU,
    StrLenForm,
    StrLenFormU,
    SubString,
    SubStringU,

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
#[allow(non_camel_case_types)]
pub enum BuiltinCommand {
    UpCheck,

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
    Split,

    Return,
    ReturnF,
    Restart,
    Quit,
    Throw,
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
    ChkData,
    SaveNos,

    SaveGlobal,
    LoadGlobal,

    SaveChara,
    LoadChara,
    ChkCharaData,
    FindCharaData,
    AddDefChara,
    AddChara,
    DelChara,
    SwapChara,
    FindChara,
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
    GetFont,
    ChkFont,

    Bar,

    SetBit,
    ClearBit,
    InvertBit,

    ArrayShift,

    Varset,
    CVarset,
}
