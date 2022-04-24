use std::collections::BTreeMap;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering::Relaxed};

use arrayvec::ArrayVec;
use dashmap::DashMap;
use enum_map::{Enum, EnumMap};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use parking_lot::{Mutex, RwLock};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use strum::{Display, EnumCount, EnumString, IntoStaticStr};
use strum::{EnumIter, IntoEnumIterator};

use crate::Value;

#[derive(Serialize, Deserialize)]
pub struct VariableDic {
    global_datas: RwLock<Vec<(VariableInfo, SmolStr)>>,
    local_datas: Mutex<Vec<Vec<(VariableInfo, SmolStr)>>>,
    func_datas: Mutex<Vec<SmolStr>>,
    next_var: AtomicU32,
    next_local: AtomicU32,
    next_func: AtomicU32,

    var_names: DashMap<SmolStr, GlobalIndex, ahash::RandomState>,
    local_names: DashMap<(FunctionIndex, SmolStr), LocalIndex, ahash::RandomState>,
    func_names: DashMap<SmolStr, FunctionIndex, ahash::RandomState>,
    known_idxs: EnumMap<KnownVariables, GlobalIndex>,
}

pub fn default_infos() -> BTreeMap<String, VariableInfo> {
    let mut ret = BTreeMap::new();

    macro_rules! insert {
        (
            $(
                (
                    $name:expr
                    $(
                        , $field:ident: $val:expr
                    )*
                )$(,)?
            )+
        ) => {
            $(
                ret.insert($name.into(), VariableInfo { $($field: $val,)* ..Default::default() });
            )+
        };
        (
            @CHARA
            $(
                (
                    $name:expr
                    $(
                        , $field:ident: $val:expr
                    )*
                )$(,)?
            )+
        ) => {
            $(
                ret.insert($name.into(), VariableInfo { is_chara: true, $($field: $val,)* ..Default::default() });
            )+
        };
    }

    insert! {
        ("A") ("B") ("C") ("D") ("E") ("F") ("G") ("H") ("I") ("J") ("K") ("L") ("M") ("N") ("O") ("P") ("Q")
        ("DA", size: vec!(1000usize; 2)), ("DB", size: vec!(1000usize; 2)) ("DC", size: vec!(1000usize; 2)) ("DD", size: vec!(1000usize; 2)) ("DE", size: vec!(1000usize; 2)) ("DF", size: vec!(1000usize; 2))
        ("GLOBAL"), ("GLOBALS", is_str: true)
        ("RESULT"), ("RESULTS", is_str: true)
        ("STR", is_str: true),
        ("FLAG", size: vec!(10000usize; 1)) ("DAY") ("TIME") ("DITEMTYPE", size: vec!(1000usize; 2))
        ("CHARANUM") ("COUNT") ("TARGET") ("ASSI") ("MASTER") ("PLAYER") ("NO") ("ASSIPLAY")
        ("MONEY")
        ("SELECTCOM")("PREVCOM")("NEXTCOM")
        ("ITEM")("ITEMPRICE")("ITEMSALES")("NOITEM")
        ("TFLAG") ("TEQUIP") ("TSTR", is_str: true)
    }

    insert! {
        @CHARA
        ("ABL") ("CFLAG") ("TALENT") ("MARK") ("EQUIP") ("STAIN") ("EJAC")
        ("EX") ("NOWEX") ("EXP") ("EXPLV") ("JUEL") ("PALAM") ("PALAMLV")
        ("BASE") ("MAXBASE") ("DOWNBASE") ("UPBASE") ("SOURCE") ("UP") ("DOWN")
        ("RELATION")
        ("TCVAR")
        ("CSTR", is_str: true)
        ("NAME", is_str: true, size: vec![]) ("CALLNAME", is_str: true, size: vec![]) ("NICKNAME", is_str: true, size: vec![]) ("MASTERNAME", is_str: true, size: vec![])
    }

    ret
}

impl Default for VariableDic {
    fn default() -> Self {
        Self::new(&default_infos())
    }
}

impl VariableDic {
    pub fn new(var_infos: &BTreeMap<String, VariableInfo>) -> Self {
        let mut ret = Self {
            global_datas: Default::default(),
            local_datas: Default::default(),
            func_datas: Default::default(),
            next_var: AtomicU32::new(0),
            next_func: AtomicU32::new(0),
            next_local: AtomicU32::new(0),

            var_names: Default::default(),
            func_names: Default::default(),
            local_names: Default::default(),
            known_idxs: enum_map::enum_map! { _ => GlobalIndex(NonZeroU32::new(1).unwrap()) },
        };

        for builtin in BulitinVariable::iter() {
            ret.insert_global_var(<&str>::from(builtin), VariableInfo::default());
        }

        let mut known_visit = EnumMap::default();

        for (name, info) in var_infos.iter() {
            let name = SmolStr::new_inline(name);
            let idx = ret.insert_global_var(name.clone(), info.clone());

            if let Ok(known) = name.parse() {
                known_visit[known] = true;
                ret.known_idxs[known] = idx;
            }
        }

        for (known, visited) in known_visit {
            if !visited {
                panic!("KnownVariable {} should have variable info", known);
            }
        }

        ret
    }

    pub fn var_len(&self) -> usize {
        self.next_var.load(Relaxed) as usize
    }

    fn next_var_idx(&self) -> GlobalIndex {
        let ret = self.next_var.fetch_add(1, Relaxed);
        GlobalIndex::from_usize(ret as usize)
    }

    fn next_local_idx(&self) -> LocalIndex {
        let ret = self.next_local.fetch_add(1, Relaxed);
        LocalIndex::from_usize(ret as usize)
    }

    fn next_func_idx(&self) -> FunctionIndex {
        let ret = self.next_func.fetch_add(1, Relaxed);
        FunctionIndex::from_usize(ret as usize)
    }

    pub fn insert_global_var(&self, name: impl Into<SmolStr>, info: VariableInfo) -> GlobalIndex {
        let name = name.into();
        // must be locked before push data
        let mut data = self.global_datas.write();
        let next = self.next_var_idx();
        debug_assert_eq!(next.as_usize(), data.len());
        data.push((info, name.clone()));
        self.var_names.insert(name, next);
        next
    }

    pub fn insert_local_var(
        &self,
        func: FunctionIndex,
        name: impl Into<SmolStr>,
        info: VariableInfo,
    ) -> LocalIndex {
        let name = name.into();

        // must be locked before push data
        let mut local_data = self.local_datas.lock();
        let var_idx = self.next_local_idx();

        self.local_names.insert((func, name.clone()), var_idx);
        let locals = local_data.get_mut(func.as_usize()).unwrap();
        debug_assert_eq!(var_idx.as_usize(), locals.len());
        locals.push((info, name));

        var_idx
    }

    pub fn insert_func(&self, name: impl Into<SmolStr>) -> FunctionIndex {
        let name = name.into();

        *self.func_names.entry(name.clone()).or_insert_with(|| {
            // must be locked before push data
            let mut local = self.local_datas.lock();
            let mut func = self.func_datas.lock();
            let idx = self.next_func_idx();
            self.func_names.insert(name.clone(), idx);
            debug_assert_eq!(idx.as_usize(), local.len());
            local.push(Vec::with_capacity(4));
            debug_assert_eq!(idx.as_usize(), func.len());
            func.push(name);
            idx
        })
    }

    pub fn get_global(&self, name: impl AsRef<str>) -> Option<GlobalIndex> {
        self.var_names.get(name.as_ref()).map(|i| *i)
    }

    pub fn get_var(&self, name: impl Into<SmolStr>, func: FunctionIndex) -> Option<VariableIndex> {
        let name = name.into();
        self.get_local(name.clone(), func)
            .map(|i| VariableIndex::Local(func, i))
            .or_else(|| self.get_global(name.as_str()).map(VariableIndex::Global))
    }

    pub fn get_local(&self, name: impl Into<SmolStr>, func: FunctionIndex) -> Option<LocalIndex> {
        self.local_names.get(&(func, name.into())).map(|i| *i)
    }

    pub fn get_func(&self, name: impl AsRef<str>) -> Option<FunctionIndex> {
        self.func_names.get(name.as_ref()).map(|i| *i)
    }

    pub fn get_known(&self, known: KnownVariables) -> GlobalIndex {
        self.known_idxs[known]
    }

    pub fn resolve_var(&self, var: VariableIndex) -> Option<(SmolStr, VariableInfo)> {
        match var {
            VariableIndex::Global(global) => self.resolve_global_var(global),
            VariableIndex::Local(func, local) => self.resolve_local_var(func, local),
        }
    }

    pub fn resolve_global_var(&self, idx: GlobalIndex) -> Option<(SmolStr, VariableInfo)> {
        let data = self.global_datas.read();
        let (info, name) = data.get(idx.as_usize())?;

        Some((name.clone(), info.clone()))
    }

    pub fn resolve_local_var(
        &self,
        func: FunctionIndex,
        idx: LocalIndex,
    ) -> Option<(SmolStr, VariableInfo)> {
        let data = self.local_datas.lock();
        let (info, name) = data.get(func.as_usize())?.get(idx.as_usize())?;

        Some((name.clone(), info.clone()))
    }

    pub fn resolve_known(&self, known: KnownVariables) -> (SmolStr, VariableInfo) {
        let data = self.global_datas.read();
        let (info, name) = unsafe { data.get_unchecked(self.get_known(known).as_usize()) };

        (name.clone(), info.clone())
    }

    pub fn resolve_func(&self, idx: FunctionIndex) -> Option<SmolStr> {
        self.func_datas.lock().get(idx.as_usize()).cloned()
    }

    pub fn iter_global(&self, mut f: impl FnMut(GlobalIndex, &VariableInfo)) {
        let datas = self.global_datas.read();

        datas
            .iter()
            .enumerate()
            .for_each(|(c, (i, _))| f(GlobalIndex::from_usize(c), i));
    }

    pub fn iter_local(&self, func: FunctionIndex, mut f: impl FnMut(LocalIndex, &VariableInfo)) {
        let datas = self.local_datas.lock();

        datas[func.as_usize()]
            .iter()
            .enumerate()
            .for_each(|(c, (i, _))| f(LocalIndex::from_usize(c), i));
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(default)]
pub struct VariableInfo {
    pub is_chara: bool,
    pub is_str: bool,
    pub init: Vec<Value>,
    pub size: Vec<usize>,
}

impl Default for VariableInfo {
    fn default() -> Self {
        Self {
            is_chara: false,
            is_str: false,
            init: Vec::new(),
            size: vec![1000usize; 1],
        }
    }
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }
}

macro_rules! impl_index {
    ($ty:ident) => {
        #[derive(
            Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash,
        )]
        #[repr(transparent)]
        pub struct $ty(NonZeroU32);

        impl Default for $ty {
            fn default() -> Self {
                Self(NonZeroU32::new(1).unwrap())
            }
        }

        impl $ty {
            #[inline]
            pub fn as_usize(self) -> usize {
                self.0.get() as usize - 1
            }

            #[inline]
            fn from_usize(n: usize) -> Self {
                Self(unsafe { NonZeroU32::new_unchecked(n.saturating_add(1) as u32) })
            }
        }

        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}[{}]", stringify!($ty), self.0)
            }
        }
    };
}

impl_index!(FunctionIndex);
impl_index!(LocalIndex);
impl_index!(GlobalIndex);

impl GlobalIndex {
    #[inline]
    pub fn is_builtin(self) -> bool {
        self.as_usize() < BulitinVariable::COUNT
    }

    pub fn to_builtin(self) -> Option<BulitinVariable> {
        FromPrimitive::from_usize(self.as_usize())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum VariableIndex {
    Global(GlobalIndex),
    Local(FunctionIndex, LocalIndex),
}

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
    EnumString,
    Display,
)]
#[strum(serialize_all = "UPPERCASE")]
/// only contains variable required from either VM or compiler
pub enum KnownVariables {
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
