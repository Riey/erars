use serde::{Deserialize, Serialize};
use strum::EnumString;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, EnumString)]
#[repr(u32)]
pub enum Alignment {
    #[strum(to_string = "LEFT")]
    Left,
    #[strum(to_string = "CENTER")]
    Center,
    #[strum(to_string = "RIGHT")]
    Right,
}

impl Default for Alignment {
    fn default() -> Self {
        Alignment::Left
    }
}
