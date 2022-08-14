use crate::{value::Value, Expr};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Variable {
    pub var: SmolStr,
    pub func_extern: Option<SmolStr>,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct LocalVariable {
    pub var: SmolStr,
    pub info: VariableInfo,
}

#[derive(Clone, Default, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(default)]
pub struct VariableInfo {
    pub is_chara: bool,
    pub is_str: bool,
    pub default_int: i64,
    pub size: Vec<usize>,
    pub init: Vec<Value>,
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }

    pub fn full_size(&self) -> usize {
        let mut ret = 1;

        for s in self.size.iter() {
            ret *= *s;
        }

        ret
    }
}
