use anyhow::{bail, Error, Result};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Int(i64),
    String(String),
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Int(0) => false,
            Value::String(s) if s.is_empty() => false,
            _ => true,
        }
    }
}

impl TryFrom<Value> for String {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(s) => Ok(s),
            _ => bail!("Value is not string"),
        }
    }
}

impl TryFrom<Value> for usize {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => Ok(i.try_into()?),
            _ => bail!("Value is not int"),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(i) => Ok(i),
            _ => bail!("Value is not int"),
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
