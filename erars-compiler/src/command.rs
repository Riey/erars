use serde::{Deserialize, Serialize};
use strum::{Display, EnumString};

#[derive(EnumString, Display, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[strum(serialize_all = "UPPERCASE")]
#[serde(rename_all = "UPPERCASE")]
#[allow(non_camel_case_types)]
pub enum BuiltinCommand {
    Limit,
    Min,
    Max,

    StrLenS,
    StrLenSU,
    SubStringU,

    Input,
    InputS,
    TInput,
    TInputS,
    Wait,
    WaitAnykey,

    Restart,
    Quit,

    LoadGlobal,

    DrawLine,
    CustomDrawLine,
    ClearLine,

    Split,
    Unicode,

    Reset_Stain,
    GetExpLv,

    ResetData,
    ChkData,

    AddDefChara,
    AddChara,
    DelChara,
    SwapChara,
    FindChara,

    SetColor,
    ResetColor,

    FontBold,
    FontItalic,
    FontRegular,

    Bar,
}
