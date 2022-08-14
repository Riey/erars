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

    SaveGlobal,
    LoadGlobal,

    Redraw,
    DrawLine,
    CustomDrawLine,
    ClearLine,

    Swap,
    Split,
    Unicode,

    Reset_Stain,
    GetExpLv,
    GetPalamLv,

    ResetData,
    ChkData,

    AddDefChara,
    AddChara,
    DelChara,
    GetChara,
    SwapChara,
    FindChara,

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
    Power,

    ArrayShift,

    Varset,
}
