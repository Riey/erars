use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum UnaryOperator {
    /// !
    Not,
}

impl UnaryOperator {
    pub fn name(self) -> &'static str {
        match self {
            UnaryOperator::Not => "!",
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum BinaryOperator {
    /// +
    Add,
    /// -
    Sub,
    /// /
    Div,
    /// *
    Mul,
    /// %
    Rem,
    /// &
    And,
    /// |
    Or,
    /// ^
    Xor,

    /// ==
    Equal,
    /// !=
    NotEqual,
    /// <
    Less,
    /// <=
    LessOrEqual,
    /// >
    Greater,
    /// >=
    GreaterOrEqual,
}

impl BinaryOperator {
    pub fn name(self) -> &'static str {
        match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Rem => "%",
            BinaryOperator::And => "&",
            BinaryOperator::Or => "|",
            BinaryOperator::Xor => "^",

            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::Greater => ">",
            BinaryOperator::GreaterOrEqual => ">=",
            BinaryOperator::Less => "<",
            BinaryOperator::LessOrEqual => "<=",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum TernaryOperator {
    /// ? :
    Conditional,
}

impl TernaryOperator {
    pub fn first_name(self) -> &'static str {
        match self {
            TernaryOperator::Conditional => "?",
        }
    }

    pub fn second_name(self) -> &'static str {
        match self {
            TernaryOperator::Conditional => ":",
        }
    }
}
