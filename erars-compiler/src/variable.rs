use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering::Relaxed};

use arrayvec::ArrayVec;
use dashmap::DashMap;
use enum_map::{Enum, EnumMap};
use hashbrown::HashMap;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use parking_lot::{Mutex, RwLock};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use strum::{Display, EnumCount, EnumString, IntoStaticStr};
use strum::{EnumIter, IntoEnumIterator};

#[derive(Serialize, Deserialize)]
pub struct VariableDic {
    global_datas: RwLock<Vec<(VariableInfo, SmolStr)>>,
    local_datas: Mutex<Vec<Vec<(VariableInfo, SmolStr)>>>,
    func_datas: Mutex<Vec<SmolStr>>,
    next_var: AtomicU32,
    next_local: AtomicU32,
    next_func: AtomicU32,

    var_names: DashMap<SmolStr, VariableIndex, ahash::RandomState>,
    local_names: DashMap<(FunctionIndex, SmolStr), LocalIndex, ahash::RandomState>,
    func_names: DashMap<SmolStr, FunctionIndex, ahash::RandomState>,
    known_idxs: EnumMap<KnownVariables, VariableIndex>,
}

pub fn default_infos() -> HashMap<String, VariableInfo> {
    let mut ret = HashMap::new();

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
        ("DA", size: arrvec!(1000usize; 2)), ("DB", size: arrvec!(1000usize; 2)) ("DC", size: arrvec!(1000usize; 2)) ("DD", size: arrvec!(1000usize; 2)) ("DE", size: arrvec!(1000usize; 2)) ("DF", size: arrvec!(1000usize; 2))
        ("GLOBAL"), ("GLOBALS", is_str: true)
        ("RESULT"), ("RESULTS", is_str: true)
        ("STR", is_str: true),
        ("FLAG", size: arrvec!(10000usize; 1)) ("DAY") ("TIME") ("DITEMTYPE", size: arrvec!(1000usize; 2))
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
        ("NAME", is_str: true, size: arrvec![]) ("CALLNAME", is_str: true, size: arrvec![]) ("NICKNAME", is_str: true, size: arrvec![]) ("MASTERNAME", is_str: true, size: arrvec![])
    }

    ret
}

impl Default for VariableDic {
    fn default() -> Self {
        Self::new(&default_infos())
    }
}

impl VariableDic {
    pub fn new(var_infos: &HashMap<String, VariableInfo>) -> Self {
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
            known_idxs: enum_map::enum_map! { _ => VariableIndex(NonZeroU32::new(1).unwrap()) },
        };

        for builtin in BulitinVariable::iter() {
            // builtin var doesn't have info
            ret.insert_var_name(<&str>::from(builtin));
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

    fn next_var_idx(&self) -> VariableIndex {
        let ret = self.next_var.fetch_add(1, Relaxed);
        unsafe { VariableIndex(NonZeroU32::new_unchecked(ret)) }
    }

    fn next_local_idx(&self) -> LocalIndex {
        let ret = self.next_local.fetch_add(1, Relaxed);
        unsafe { LocalIndex(NonZeroU32::new_unchecked(ret)) }
    }

    fn next_func_idx(&self) -> FunctionIndex {
        let ret = self.next_func.fetch_add(1, Relaxed);
        unsafe { FunctionIndex(NonZeroU32::new_unchecked(ret)) }
    }

    pub fn insert_var_name(&self, name: impl Into<SmolStr>) -> VariableIndex {
        let next = self.next_var_idx();
        let name = name.into();
        self.var_names.insert(name, next);
        next
    }

    pub fn insert_global_var(&self, name: impl Into<SmolStr>, info: VariableInfo) -> VariableIndex {
        let name = name.into();
        // must be locked before push data
        let mut data = self.global_datas.write();
        let next = self.next_var_idx();
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
        locals.push((info, name));

        var_idx
    }

    pub fn insert_func(&self, name: impl Into<SmolStr>) -> FunctionIndex {
        let name = name.into();
        // must be locked before push data
        let mut local = self.local_datas.lock();
        let mut func = self.func_datas.lock();
        let idx = self.next_func_idx();
        self.func_names.insert(name.clone(), idx);
        local.push(Vec::with_capacity(4));
        func.push(name);
        idx
    }

    pub fn get_var(&self, name: impl AsRef<str>) -> Option<VariableIndex> {
        self.var_names.get(name.as_ref()).map(|i| *i)
    }

    pub fn get_local(&self, name: impl Into<SmolStr>, func: FunctionIndex) -> Option<LocalIndex> {
        self.local_names.get(&(func, name.into())).map(|i| *i)
    }

    pub fn get_func(&self, name: impl AsRef<str>) -> Option<FunctionIndex> {
        self.func_names.get(name.as_ref()).map(|i| *i)
    }

    pub fn get_known(&self, known: KnownVariables) -> VariableIndex {
        self.known_idxs[known]
    }

    pub fn resolve_global_var(&self, idx: VariableIndex) -> Option<(SmolStr, VariableInfo)> {
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
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(default)]
pub struct VariableInfo {
    pub is_chara: bool,
    pub is_str: bool,
    pub default_int: i64,
    pub size: ArrayVec<usize, 3>,
}

impl Default for VariableInfo {
    fn default() -> Self {
        Self {
            is_chara: false,
            is_str: false,
            default_int: 0,
            size: arrvec![1000usize; 1],
        }
    }
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct FunctionIndex(NonZeroU32);

impl FunctionIndex {
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0.get() as usize - 1
    }
}

impl std::fmt::Display for FunctionIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Func[{}]", self.0)
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LocalIndex(NonZeroU32);

impl LocalIndex {
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0.get() as usize - 1
    }
}

impl std::fmt::Display for LocalIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Local[{}]", self.0)
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct VariableIndex(NonZeroU32);

impl VariableIndex {
    #[inline]
    pub fn as_usize(self) -> usize {
        self.0.get() as usize - 1
    }

    #[inline]
    pub fn is_builtin(self) -> bool {
        self.as_usize() < BulitinVariable::COUNT
    }

    pub fn to_builtin(self) -> Option<BulitinVariable> {
        FromPrimitive::from_usize(self.as_usize())
    }
}

impl std::fmt::Display for VariableIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Var[{}]", self.0)
    }
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
