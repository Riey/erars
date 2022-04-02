use arrayvec::ArrayVec;
use enum_map::{Enum, EnumMap};
use hashbrown::HashMap;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use serde::{Deserialize, Serialize};
use smartstring::{LazyCompact, SmartString};
use strum::{EnumCount, IntoStaticStr};
use strum::{EnumIter, IntoEnumIterator};

#[derive(
    Copy,
    Clone,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumIter,
    Serialize,
    Deserialize,
    IntoStaticStr,
    EnumCount,
    FromPrimitive,
)]
pub enum BulitinVariable {
    #[strum(to_string = "GAMEBASE_AUTHOR")]
    GamebaseAuthor,
    #[strum(to_string = "GAMEBASE_TITLE")]
    GamebaseTitle,
    #[strum(to_string = "GAMEBASE_YEAR")]
    GamebaseYear,
    #[strum(to_string = "GAMEBASE_INFO")]
    GamebaseInfo,
    #[strum(to_string = "GAMEBASE_VERSION")]
    GamebaseVersion,
}

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
/// only contains variable required from either VM or compiler
pub enum KnownVariables {
    Count,
    Target,
    Master,
    Money,
    Item,
    NoItem,
    ItemSales,
    ItemPrice,
    No,
    CharaNum,
    Result,
    ResultS,
    Local,
    LocalS,
    Arg,
    ArgS,
    Exp,
    Juel,
    Param,
    Base,
    MaxBase,

    Rand,

    FlagName,
    TalentName,
    ItemName,
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
            name_idxs: HashMap::with_capacity(128),
            names: Vec::with_capacity(128),
            known_idxs: EnumMap::default(),
            next: 0,
        };

        for builtin in BulitinVariable::iter() {
            ret.intern(<&str>::from(builtin));
        }

        for known in KnownVariables::iter() {
            let idx = ret.intern(<&str>::from(known));
            ret.known_idxs[known] = idx;
        }

        ret
    }

    pub fn len(&self) -> u32 {
        self.next
    }

    pub fn idxs_without_builtin(&self) -> impl Iterator<Item = VariableIndex> {
        (BulitinVariable::COUNT as u32..self.next).map(VariableIndex)
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
            "FLAG" "DAY" "TIME"
            "STR"
            "ABL" "CFLAG" "TALENT" "NAME" "CALLNAME" "NICKNAME" "MASTERNAME"
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

impl VariableIndex {
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }

    #[inline]
    pub fn is_builtin(self) -> bool {
        self.as_usize() < BulitinVariable::COUNT
    }

    pub fn to_builtin(self) -> Option<BulitinVariable> {
        FromPrimitive::from_u32(self.0)
    }
}

impl std::fmt::Display for VariableIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Var[{}]", self.0)
    }
}
