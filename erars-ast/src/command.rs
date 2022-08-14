use serde::{Deserialize, Serialize};
use strum::{Display, IntoStaticStr};

#[derive(Display, Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, IntoStaticStr)]
#[strum(serialize_all = "UPPERCASE")]
#[serde(rename_all = "UPPERCASE")]
#[allow(non_camel_case_types)]
pub enum BuiltinCommand {
    Limit,
    Min,
    Max,
    UpCheck,

    StrLenS,
    StrLenSU,
    StrLenForm,
    StrLenFormU,
    SubStringU,

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

    Return,
    Restart,
    Quit,
    Throw,
    CallTrain,

    Redraw,
    DrawLine,
    CustomDrawLine,
    ClearLine,

    Swap,
    Split,
    Unicode,
    PutForm,

    ResetStain,
    GetExpLv,
    GetPalamLv,

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
    GetChara,
    SwapChara,
    FindChara,
    PickupChara,

    SetColor,
    SetBgColor,
    ResetColor,
    ResetBgColor,
    SetColorByName,
    SetBgColorByName,
    GetColor,
    GetDefColor,
    GetBgColor,
    GetDefBgColor,
    GetFocusColor,

    FontBold,
    FontItalic,
    FontRegular,
    FontStyle,
    GetStyle,
    SetFont,

    Bar,

    SetBit,
    GetBit,
    ClearBit,
    InvertBit,
    Power,

    ArrayShift,

    Varset,
    CVarset,

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
    CsvRelation,
    CsvJuel,
    CsvEquip,
    CsvCflag,
}
