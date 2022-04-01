use arrayvec::ArrayVec;
use enum_map::{Enum, EnumMap};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use smartstring::{LazyCompact, SmartString};
use strum::IntoStaticStr;
use strum::{EnumIter, IntoEnumIterator};

#[derive(
    Clone,
    Copy,
    Debug,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Enum,
    EnumIter,
    IntoStaticStr,
)]
#[strum(serialize_all = "UPPERCASE")]
#[allow(non_camel_case_types)]

/// only contains variable required from either VM or compiler
pub enum KnownVariables {
    Count,
    Target,
    Master,
    Result,
    ResultS,
    Local,
    LocalS,
    Arg,
    ArgS,
    Gamebase_Version,
    Gamebase_Author,
    Gamebase_Info,
    Gamebase_Title,
    Gamebase_Year,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct VariableInterner {
    name_idxs: HashMap<SmartString<LazyCompact>, VariableIndex>,
    names: Vec<SmartString<LazyCompact>>,
    known_idxs: EnumMap<KnownVariables, VariableIndex>,
    next: u32,
}

impl VariableInterner {
    pub fn new() -> Self {
        let mut ret = Self {
            name_idxs: HashMap::new(),
            names: vec![],
            known_idxs: EnumMap::default(),
            next: 0,
        };

        for known in KnownVariables::iter() {
            let idx = ret.intern(<&str>::from(known));
            ret.known_idxs[known] = idx;
        }

        ret
    }

    pub fn with_default_variables() -> Self {
        let mut ret = Self::new();

        macro_rules! interns {
            ($($name:literal)+) => {
                $(ret.intern($name);)+
            };
        }

        interns!(
            "A" "B" "C" "D" "E" "F"
            "MASTER" "TARGET" "ASSI"
            "FLAG" "STR"
            "CFLAG" "TALENT" "NAME" "NICKNAME" "MASTERNAME"
        );

        ret
    }

    pub fn intern(&mut self, name: impl Into<SmartString<LazyCompact>>) -> VariableIndex {
        *self.name_idxs.entry(name.into()).or_insert_with_key(|k| {
            let ret = self.next;
            self.names.push(k.clone());
            self.next += 1;
            VariableIndex(ret)
        })
    }

    pub fn get(&self, name: &str) -> Option<VariableIndex> {
        self.name_idxs.get(name).copied()
    }

    pub fn get_known(&self, known: KnownVariables) -> VariableIndex {
        self.known_idxs[known]
    }

    pub fn resolve(&self, idx: VariableIndex) -> Option<&SmartString<LazyCompact>> {
        self.names.get(idx.0 as usize)
    }

    pub fn resolve_known(&self, known: KnownVariables) -> &SmartString<LazyCompact> {
        unsafe { self.names.get_unchecked(self.get_known(known).0 as usize) }
    }
}

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct VariableInfo {
    is_chara: bool,
    is_str: bool,
    default_int: i64,
    size: ArrayVec<u32, 4>,
}

#[derive(
    Clone, Copy, Default, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
pub struct VariableIndex(u32);
