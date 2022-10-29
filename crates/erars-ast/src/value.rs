use anyhow::{bail, Error, Result};
use serde::{Deserialize, Serialize};

use crate::StrKey;

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum InlineValue {
    Int(i64),
    String(StrKey),
}

impl InlineValue {
    pub fn to_int(self) -> Option<i64> {
        match self {
            Self::Int(i) => Some(i),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Value {
    Int(i64),
    String(String),
}

impl Default for Value {
    fn default() -> Self {
        Value::Int(0)
    }
}

impl Value {
    pub const ZERO: Value = Value::Int(0);
    pub const ONE: Value = Value::Int(1);

    pub fn into_int_err(self) -> Result<i64, String> {
        match self {
            Value::Int(i) => Ok(i),
            Value::String(other) => Err(other),
        }
    }

    pub fn into_str_err(self) -> Result<String, i64> {
        match self {
            Value::String(s) => Ok(s),
            Value::Int(other) => Err(other),
        }
    }

    pub fn try_into_int(self) -> Result<i64> {
        self.try_into()
    }

    pub fn try_into_str(self) -> Result<String> {
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
}

impl From<Value> for bool {
    fn from(v: Value) -> bool {
        v.as_bool()
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

impl TryFrom<Value> for u32 {
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

impl From<u32> for Value {
    fn from(v: u32) -> Self {
        Self::Int(v as i64)
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        Self::Int(v as i64)
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
