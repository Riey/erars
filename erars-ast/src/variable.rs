use crate::{value::Value, Expr};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Variable {
    pub var: Box<str>,
    pub func_extern: Option<Box<str>>,
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
    pub is_global: bool,
    pub default_int: i64,
    pub size: Vec<usize>,
    pub init: Vec<Value>,
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }

    pub fn full_size(&self) -> usize {
        self.size.iter().copied().product()
    }

    pub fn calculate_single_idx(&self, idxs: &[usize]) -> (Option<usize>, usize) {
        match (self.is_chara, self.size.as_slice(), idxs) {
            (true, [..], []) => (None, 0),
            (false, [..], []) => (None, 0),

            (true, [], [chara, ..]) => (Some(*chara), 0),

            (true, [_], [idx]) => (None, *idx),
            (true, [_], [chara, idx, ..]) => (Some(*chara), *idx),

            (true, [_, _], [idx]) => (None, *idx),
            (true, [_, w], [y, idx]) => (None, *w * *y + *idx),
            (true, [_, w], [chara, y, idx, ..]) => (Some(*chara), *w * *y + *idx),

            (false, [..], [idx]) => (None, *idx),

            (false, [_, w], [y, idx, ..]) => (None, *w * *y + *idx),

            (false, [_, _, w], [y, idx]) => (None, *w * *y + *idx),
            (false, [_, h, w], [z, y, idx]) => (None, *z * *w * *h + *w * *y + *idx),
            _ => panic!("Invalid index"),
        }
    }
}

#[test]
fn index_test() {
    let info = VariableInfo {
        size: vec![1000, 1000, 1000],
        ..Default::default()
    };

    k9::assert_equal!(info.calculate_single_idx(&[]), (None, 0));
    k9::assert_equal!(info.calculate_single_idx(&[1]), (None, 1));
    k9::assert_equal!(info.calculate_single_idx(&[1, 1]), (None, 1001));
    k9::assert_equal!(info.calculate_single_idx(&[2, 1, 1]), (None, 2001001));
}
