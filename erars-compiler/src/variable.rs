use enum_map::{Enum, EnumMap};
use lasso::{Key, Resolver, Spur, ThreadedRodeo};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumCount, IntoStaticStr};
use strum::{EnumIter, IntoEnumIterator};

type Interner = ThreadedRodeo<Spur>;

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
    Display,
)]
#[strum(serialize_all = "UPPERCASE")]
#[allow(non_camel_case_types)]
pub enum BulitinVariable {
    Gamebase_Author,
    Gamebase_Title,
    Gamebase_Year,
    Gamebase_Info,
    Gamebase_Version,

    LastLoad_Version,

    Rand,
    IsAssi,

    AblName,
    ExpName,
    MarkName,
    PalamName,
    FlagName,
    TalentName,
    TrainName,
    ExName,
    ItemName,
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
    Local,
    LocalS,
    Arg,
    ArgS,
    Count,
    Target,
    Master,
    Assi,
    AssiPlay,
    Player,
    Money,

    SelectCom,
    PrevCom,
    NextCom,

    Item,
    NoItem,
    ItemSales,
    ItemPrice,

    No,
    CharaNum,
    Result,
    ResultS,
    Exp,
    ExpLv,
    Juel,
    Palam,
    PalamLv,
    Base,
    DownBase,
    MaxBase,

    Source,
    Up,
    Down,
    Tflag,
    Tequip,
    Tstr,
    Tcvar,
}

#[derive(Serialize, Deserialize)]
pub struct VariableInterner {
    interner: Interner,
    known_idxs: EnumMap<KnownVariables, VariableIndex>,
}

impl VariableInterner {
    pub fn new() -> Self {
        let mut ret = Self {
            interner: Interner::new(),
            known_idxs: EnumMap::default(),
        };

        for builtin in BulitinVariable::iter() {
            ret.interner.get_or_intern_static(<&str>::from(builtin));
        }

        for known in KnownVariables::iter() {
            let idx = ret.interner.get_or_intern_static(<&str>::from(known));
            ret.known_idxs[known] = VariableIndex(idx);
        }

        ret
    }

    pub fn len(&self) -> usize {
        self.interner.len()
    }

    pub fn idxs(&self) -> impl Iterator<Item = VariableIndex> + ExactSizeIterator {
        (0..self.len()).map(|n| VariableIndex(Spur::try_from_usize(n).unwrap()))
    }

    pub fn with_default_variables() -> Self {
        let mut ret = Self::new();

        macro_rules! interns {
            ($($name:literal)+) => {
                $(ret.interner.get_or_intern_static($name);)+
            };
        }

        interns!(
            "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q"
            "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
            "DA" "DB" "DC" "DD" "DE" "DF"
            "GLOBAL" "GLOBALS"
            "FLAG" "DAY" "TIME" "DITEMTYPE"
            "STR"
            "ABL" "CFLAG" "TALENT" "MARK" "EQUIP" "STAIN" "EJAC"
            "CSTR"
            "EX" "NOWEX"
            "NAME" "CALLNAME" "NICKNAME" "MASTERNAME"
            "RELATION"
        );

        ret
    }

    #[inline]
    pub fn get_or_intern(&self, name: impl AsRef<str>) -> VariableIndex {
        VariableIndex(self.interner.get_or_intern(name))
    }

    #[inline]
    pub fn get(&self, name: &str) -> Option<VariableIndex> {
        self.interner.get(name).map(VariableIndex)
    }

    pub fn get_known(&self, known: KnownVariables) -> VariableIndex {
        self.known_idxs[known]
    }

    pub fn resolve(&self, idx: VariableIndex) -> Option<&str> {
        self.interner.try_resolve(&idx.0)
    }

    pub fn resolve_known(&self, known: KnownVariables) -> &str {
        unsafe { self.interner.resolve_unchecked(&self.get_known(known).0) }
    }
}

#[derive(Clone, Default, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(default)]
pub struct VariableInfo {
    pub is_chara: bool,
    pub is_str: bool,
    pub default_int: i64,
    pub size: Vec<usize>,
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }
}

#[derive(
    Clone, Copy, Default, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash,
)]
#[repr(transparent)]
pub struct VariableIndex(Spur);

impl VariableIndex {
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0.into_inner().get() as usize
    }

    #[inline]
    pub fn is_builtin(self) -> bool {
        self.as_usize() <= BulitinVariable::COUNT
    }

    pub fn to_builtin(self) -> Option<BulitinVariable> {
        FromPrimitive::from_u32(self.0.into_inner().get() - 1)
    }
}

impl std::fmt::Display for VariableIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Var[{}]", self.0.into_inner())
    }
}
