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

    StrLenS,
    StrLenSU,
    SubStringU,

    Input,
    InputS,
    TInput,
    TInputS,
    Wait,
    WaitAnykey,

    Return,
    Restart,
    Quit,
    Throw,

    SaveGlobal,
    LoadGlobal,

    DrawLine,
    CustomDrawLine,
    ClearLine,

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
    ResetColor,

    FontBold,
    FontItalic,
    FontRegular,

    Bar,

    SetBit,
    GetBit,
    ClearBit,
    Power,

    Varset,
}

impl BuiltinCommand {
    pub fn set_var_arg_count(self) -> usize {
        use BuiltinCommand::*;
        match self {
            SetBit | ClearBit | Varset => 1,
            _ => 0,
        }
    }
}
