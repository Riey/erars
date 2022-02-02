use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum Token {
    Print(PrintFlags),
    PrintForm(PrintFlags),
    Ident(String),

    NewLine,
    OpenBrace,
    CloseBrace,
    OpenParan,
    CloseParan,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    NotEqual,
    Equal,
    Exclamation,
    Assign,
    Question,
    Sharp,
    At,

    StringLit(String),
    IntLit(i64),
}

bitflags::bitflags! {
    #[derive(Serialize, Deserialize)]
    pub struct PrintFlags: u32 {
        const NEWLINE = 0x1;
        const WAIT = 0x2;
        const LEFT_ALIGN = 0x4;
        const RIGHT_ALIGN = 0x8;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Alignment {
    Left,
    Center,
    Right,
}
