use serde::{Deserialize, Serialize};

#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    thiserror::Error,
)]
pub enum ValueTypeError {
    #[error("Value is not String")]
    NotString,
    #[error("Value is not Int")]
    NotInt,
    #[error("Value is exceeding i64 range")]
    NumberExceed,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash)]
pub enum Value {
    Int(i64),
    String(String),
}

impl Value {
    pub const ZERO: Value = Value::Int(0);
    pub const ONE: Value = Value::Int(1);

    pub fn try_into_int(self) -> Result<i64, ValueTypeError> {
        self.try_into()
    }

    pub fn try_into_str(self) -> Result<String, ValueTypeError> {
        self.try_into()
    }

    pub fn into_str(self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::String(s) => s,
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Int(0) => false,
            Value::String(s) if s.is_empty() => false,
            _ => true,
        }
    }

    pub fn expect_int(&self) -> i64 {
        match self {
            Value::Int(i) => *i,
            _ => panic!(),
        }
    }

    pub fn expect_str(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            _ => panic!(),
        }
    }
}

impl From<Value> for bool {
    fn from(v: Value) -> bool {
        v.as_bool()
    }
}

impl TryFrom<Value> for String {
    type Error = ValueTypeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Ok(s),
            _ => Err(ValueTypeError::NotString),
        }
    }
}

impl TryFrom<Value> for usize {
    type Error = ValueTypeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => i.try_into().map_err(|_| ValueTypeError::NumberExceed),
            _ => Err(ValueTypeError::NotInt),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = ValueTypeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => Ok(i),
            _ => Err(ValueTypeError::NotInt),
        }
    }
}

impl<'a> From<&'a str> for Value {
    fn from(v: &'a str) -> Self {
        Self::String(v.to_string())
    }
}

impl<'a> From<&'a String> for Value {
    fn from(v: &'a String) -> Self {
        Self::String(v.clone())
    }
}

impl<'a> From<&'a mut String> for Value {
    fn from(v: &'a mut String) -> Self {
        Self::String(v.clone())
    }
}

impl From<String> for Value {
    fn from(v: String) -> Self {
        Self::String(v)
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Self::Int(v)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Int(b as _)
    }
}

impl<'a> From<&'a i64> for Value {
    fn from(v: &'a i64) -> Self {
        Self::Int(*v)
    }
}

impl<'a> From<&'a mut i64> for Value {
    fn from(v: &'a mut i64) -> Self {
        Self::Int(*v)
    }
}
