use serde::{Deserialize, Serialize};
use strum::{Display, EnumString, IntoStaticStr};

#[derive(
    Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, IntoStaticStr, Display, EnumString,
)]
pub enum UnaryOperator {
    /// !
    #[strum(to_string = "!")]
    Not,
}

#[derive(
    Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize, IntoStaticStr, Display, EnumString,
)]
pub enum BinaryOperator {
    /// +
    #[strum(to_string = "+")]
    Add,
    /// -
    #[strum(to_string = "-")]
    Sub,
    /// /
    #[strum(to_string = "/")]
    Div,
    /// *
    #[strum(to_string = "*")]
    Mul,
    /// %
    #[strum(to_string = "%")]
    Rem,
    /// &
    #[strum(to_string = "&")]
    BitAnd,
    /// &&
    #[strum(to_string = "&&")]
    And,
    /// |
    #[strum(to_string = "|")]
    BitOr,
    /// ||
    #[strum(to_string = "||")]
    Or,
    /// ^
    #[strum(to_string = "^")]
    Xor,
    /// ^^
    #[strum(to_string = "^^")]
    BitXor,

    /// ==
    #[strum(to_string = "==")]
    Equal,
    /// !=
    #[strum(to_string = "!=")]
    NotEqual,
    /// <
    #[strum(to_string = "<")]
    Less,
    /// <=
    #[strum(to_string = "<=")]
    LessOrEqual,
    /// >
    #[strum(to_string = ">")]
    Greater,
    /// >=
    #[strum(to_string = ">=")]
    GreaterOrEqual,
}
