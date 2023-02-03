use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Debug,
    sync::Arc,
};

use anyhow::{anyhow, bail, ensure, Result};
use enum_map::{Enum, EnumMap};
use erars_ast::{get_interner, EventType, Interner, StrKey, Value, VariableInfo};
use erars_compiler::{CharacterTemplate, HeaderInfo};
use hashbrown::HashMap;
use rand::SeedableRng;
use rand_chacha::ChaCha20Rng;
use serde::{Deserialize, Serialize};

use erars_ui::VirtualConsole;
use strum::{Display, IntoStaticStr};

use crate::{
    context::FunctionIdentifier, SerializableGlobalVariableStorage, SerializableVariableStorage,
};

macro_rules! set_var {
    ($self:expr, $name:ident, $value:expr) => {
        *$self.ref_int(KnownVariableNames::$name, &[])? = $value;
    };
    (@all $self:expr, $name:ident, $value:expr) => {
        match $self.get_var(KnownVariableNames::$name)?.1 {
            UniformVariable::Character(ref mut cvar) => {
                for var in cvar {
                    var.as_int()?.fill($value);
                }
            }
            UniformVariable::Normal(ref mut var) => {
                var.as_int()?.fill($value);
            }
        }
    };
}

#[derive(Clone)]
pub struct VariableStorage {
    interner: &'static Interner,
    header: Arc<HeaderInfo>,
    character_len: u32,
    rng: ChaCha20Rng,
    variables: HashMap<StrKey, (VariableInfo, UniformVariable)>,
    local_variables: HashMap<StrKey, HashMap<StrKey, (VariableInfo, Option<UniformVariable>)>>,
    known_variables: EnumMap<KnownVariableNames, StrKey>,
    event_keys: EnumMap<EventType, StrKey>,
}

impl VariableStorage {
    pub fn new(header: Arc<HeaderInfo>, infos: &HashMap<StrKey, VariableInfo>) -> Self {
        let mut variables = HashMap::new();

        for (k, v) in infos {
            variables.insert(*k, (v.clone(), UniformVariable::new(&header, v)));
        }

        let interner = get_interner();

        Self {
            character_len: 0,
            header,
            rng: ChaCha20Rng::from_entropy(),
            variables,
            local_variables: HashMap::new(),
            known_variables: enum_map::enum_map! {
                v => interner.get_or_intern_static(<&str>::from(v)),
            },
            event_keys: enum_map::enum_map! {
                v => interner.get_or_intern_static(<&str>::from(v)),
            },
            interner,
        }
    }

    #[inline]
    pub fn header(&self) -> &HeaderInfo {
        &self.header
    }

    #[inline]
    pub fn interner(&self) -> &'static Interner {
        &self.interner
    }

    #[inline]
    pub fn event_key(&self, ty: EventType) -> StrKey {
        self.event_keys[ty]
    }

    #[inline]
    pub fn known_key(&self, var: KnownVariableNames) -> StrKey {
        self.known_variables[var]
    }

    #[inline]
    pub fn resolve_key(&self, key: StrKey) -> &str {
        self.interner.resolve(&key)
    }

    pub fn check_var_exists(&self, fn_name: StrKey, name: StrKey) -> bool {
        self.variables.contains_key(&name)
            || self
                .local_variables
                .get(&fn_name)
                .map_or(false, |v| v.contains_key(&name))
    }

    pub fn clear_dynamic_vars(&mut self, name: StrKey) {
        if let Some(local_dic) = self.local_variables.get_mut(&name) {
            for (_, (info, var)) in local_dic.iter_mut() {
                if info.is_dynamic {
                    // remove dynamic variable
                    *var = None;
                }
            }
        }
    }

    pub fn local_infos(
        &self,
    ) -> impl Iterator<Item = (StrKey, Vec<(StrKey, &'_ VariableInfo)>)> + '_ {
        self.local_variables.iter().map(|(func_name, vars)| {
            (
                *func_name,
                vars.iter().map(|(key, (info, _))| (*key, info)).collect(),
            )
        })
    }

    fn load_variables(
        &mut self,
        mut variables: HashMap<StrKey, (VariableInfo, UniformVariable)>,
        mut local_variables: HashMap<StrKey, HashMap<StrKey, (VariableInfo, UniformVariable)>>,
        is_global: bool,
    ) {
        self.variables.iter_mut().for_each(|(name, (info, var))| {
            if info.is_global != is_global {
                return;
            }
            if let Some((sav_info, sav_var)) = variables.remove(name) {
                if *info == sav_info {
                    *var = sav_var;
                    return;
                }
            }
            *var = UniformVariable::with_character_len(&self.header, info, self.character_len);
        });

        self.local_variables.iter_mut().for_each(|(fn_name, vars)| {
            let Some(mut sav_vars) = local_variables.remove(fn_name) else {
                vars.values_mut().for_each(|v| if v.0.is_global == is_global { v.1 = None; });
                return;
            };

            vars.iter_mut().for_each(|(name, (info, var))| {
                if info.is_global != is_global {
                    return;
                }
                if let Some((sav_info, sav_var)) = sav_vars.remove(name) {
                    if *info == sav_info {
                        *var = Some(sav_var);
                        return;
                    }
                }
                *var = None;
            });
        });
    }

    pub fn load_global_serializable(
        &mut self,
        sav: SerializableGlobalVariableStorage,
        header: &HeaderInfo,
    ) -> Result<()> {
        self.load_variables(sav.variables, sav.local_variables, true);
        self.init(header)?;

        Ok(())
    }

    pub fn load_serializable(
        &mut self,
        sav: SerializableVariableStorage,
        header: &HeaderInfo,
    ) -> Result<()> {
        self.character_len = sav.character_len;
        self.rng = SeedableRng::from_seed(sav.rand_seed);

        self.load_variables(sav.variables, sav.local_variables, false);
        self.init(header)?;

        Ok(())
    }

    fn extract_var(
        &self,
        is_global: bool,
    ) -> (
        HashMap<StrKey, (VariableInfo, UniformVariable)>,
        HashMap<StrKey, HashMap<StrKey, (VariableInfo, UniformVariable)>>,
    ) {
        let this_vars = self.variables.iter();
        let this_local_vars = self.local_variables.iter();

        let variables = this_vars
            .filter_map(|(name, (info, var))| {
                if info.is_global == is_global {
                    if info.is_savedata {
                        Some((*name, (info.clone(), var.clone())))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        let local_variables = this_local_vars
            .filter_map(|(fn_name, vars)| {
                let vars: HashMap<_, _> = vars
                    .iter()
                    .filter_map(|(name, (info, var))| {
                        if info.is_global == is_global {
                            let var = if info.is_savedata {
                                var.clone()?
                            } else {
                                return None;
                            };
                            Some((*name, (info.clone(), var)))
                        } else {
                            None
                        }
                    })
                    .collect();

                if vars.is_empty() {
                    None
                } else {
                    Some((*fn_name, vars))
                }
            })
            .collect();

        (variables, local_variables)
    }

    pub fn get_serializable(
        &self,
        header: &HeaderInfo,
        description: String,
    ) -> SerializableVariableStorage {
        let (variables, local_variables) = self.extract_var(false);

        SerializableVariableStorage {
            code: header.gamebase.code,
            version: header.gamebase.version,
            variables,
            description,
            local_variables,
            character_len: self.character_len,
            rand_seed: self.rng.get_seed(),
        }
    }

    pub fn get_global_serializable(
        &self,
        header: &HeaderInfo,
    ) -> SerializableGlobalVariableStorage {
        let (variables, local_variables) = self.extract_var(true);

        SerializableGlobalVariableStorage {
            code: header.gamebase.code,
            version: header.gamebase.version,
            variables,
            local_variables,
        }
    }

    pub fn rng(&mut self) -> &mut impl rand::Rng {
        &mut self.rng
    }

    fn upcheck_internal(
        tx: &mut VirtualConsole,
        palam_name: &BTreeMap<u32, StrKey>,
        palam: &mut [i64],
        up: &mut [i64],
        down: &mut [i64],
        interner: &Interner,
    ) -> Result<()> {
        itertools::multizip((palam.iter_mut(), up.iter_mut(), down.iter_mut()))
            .enumerate()
            .for_each(|(no, (p, u, d))| {
                if *u == 0 && *d == 0 {
                    return;
                }

                let name = palam_name
                    .get(&(no as u32))
                    .map(|s| interner.resolve(s))
                    .unwrap_or("");

                tx.print(format!("{name} {p}"));

                if *u != 0 {
                    tx.print(format!("+{u}"));
                }

                if *d != 0 {
                    tx.print(format!("-{d}"));
                }

                *p += *u;
                *p -= *d;

                tx.print_line(format!("={p}"));
            });

        up.fill(0);
        down.fill(0);

        Ok(())
    }

    pub fn upcheck(
        &mut self,
        tx: &mut VirtualConsole,
        idx: u32,
        palam_name: &BTreeMap<u32, StrKey>,
    ) -> Result<()> {
        let interner = self.interner();
        let (palam, up, down) = self.get_var3(
            KnownVariableNames::Palam,
            KnownVariableNames::Up,
            KnownVariableNames::Down,
        )?;

        let palam = palam.1.assume_chara(idx).as_int()?;
        let up = up.1.assume_normal().as_int()?;
        let down = down.1.assume_normal().as_int()?;

        Self::upcheck_internal(tx, palam_name, palam, up, down, &interner)
    }

    pub fn cupcheck(
        &mut self,
        tx: &mut VirtualConsole,
        idx: u32,
        palam_name: &BTreeMap<u32, StrKey>,
    ) -> Result<()> {
        let interner = self.interner();
        let (palam, up, down) = self.get_var3(
            KnownVariableNames::Palam,
            KnownVariableNames::Cup,
            KnownVariableNames::Cdown,
        )?;

        let palam = palam.1.assume_chara(idx).as_int()?;
        let up = up.1.assume_chara(idx).as_int()?;
        let down = down.1.assume_chara(idx).as_int()?;

        Self::upcheck_internal(tx, palam_name, palam, up, down, &interner)
    }

    pub fn prepare_train_data(&mut self) -> Result<()> {
        self.reset_var(KnownVariableNames::Up)?;
        self.reset_var(KnownVariableNames::Down)?;
        self.reset_var(KnownVariableNames::LoseBase)?;
        self.reset_var(KnownVariableNames::Cup)?;
        self.reset_var(KnownVariableNames::Cdown)?;
        self.reset_var(KnownVariableNames::DownBase)?;

        Ok(())
    }

    pub fn reset_train_data(&mut self) -> Result<()> {
        set_var!(self, AssiPlay, 0);
        set_var!(self, PrevCom, -1);
        set_var!(self, NextCom, -1);

        set_var!(@all self, Tflag, 0);
        set_var!(@all self, Tequip, 0);
        set_var!(@all self, Palam, 0);
        set_var!(@all self, Stain, 0);
        set_var!(@all self, Source, 0);
        set_var!(@all self, GotJuel, 0);

        Ok(())
    }

    pub fn get_result(&mut self) -> i64 {
        self.read_int(KnownVariableNames::Result, &[]).unwrap()
    }

    pub fn get_results(&mut self) -> String {
        self.read_str(KnownVariableNames::ResultS, &[]).unwrap()
    }

    pub fn set_result(&mut self, i: i64) {
        log::debug!("set result {i}");
        *self.ref_int(KnownVariableNames::Result, &[]).unwrap() = i;
    }

    pub fn set_results(&mut self, s: String) {
        log::debug!("set results {s}");
        *self.ref_str(KnownVariableNames::ResultS, &[]).unwrap() = s;
    }

    pub fn character_len(&self) -> u32 {
        self.character_len
    }

    pub fn add_local_info(&mut self, func: StrKey, var_name: StrKey, info: VariableInfo) {
        self.local_variables
            .entry(func)
            .or_default()
            .insert(var_name, (info, None));
    }

    pub fn ref_int(&mut self, name: impl StrKeyLike, args: &[u32]) -> Result<&mut i64> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(&mut var.as_int()?[idx as usize])
    }

    pub fn ref_local_int(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<&mut i64> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(&mut var.as_int()?[idx as usize])
    }

    pub fn ref_maybe_local_int(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<&mut i64> {
        if self.is_local_var(func_name, name) {
            self.ref_local_int(func_name, name, args)
        } else {
            self.ref_int(name, args)
        }
    }

    pub fn ref_str(&mut self, name: impl StrKeyLike, args: &[u32]) -> Result<&mut String> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(&mut var.as_str()?[idx as usize])
    }

    pub fn ref_local_str(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<&mut String> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(&mut var.as_str()?[idx as usize])
    }

    pub fn ref_maybe_local_str(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<&mut String> {
        if self.is_local_var(func_name, name) {
            self.ref_local_str(func_name, name, args)
        } else {
            self.ref_str(name, args)
        }
    }

    pub fn read_int(&mut self, name: impl StrKeyLike, args: &[u32]) -> Result<i64> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(var.as_int()?[idx as usize])
    }

    pub fn read_local_int(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<i64> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(var.as_int()?[idx as usize])
    }

    pub fn read_maybe_local_int(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<i64> {
        if self.is_local_var(func_name, name) {
            self.read_local_int(func_name, name, args)
        } else {
            self.read_int(name, args)
        }
    }

    pub fn read_str(&mut self, name: impl StrKeyLike, args: &[u32]) -> Result<String> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(var.as_str()?[idx as usize].clone())
    }

    pub fn index_var(
        &mut self,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, u32)> {
        let name = name.get_key(self);
        let target_key = self.known_key(KnownVariableNames::Target);

        let target = if name != target_key {
            self.read_int(target_key, &[])?
        } else {
            // NEED for break recursion
            -1
        };

        let (info, var) = self.get_var(name)?;

        let (c_idx, idx) = info.calculate_single_idx(args);

        let vm_var = match var {
            UniformVariable::Character(cvar) => {
                let c_idx = c_idx.unwrap_or_else(|| target as u32);
                cvar.get_mut(c_idx as usize).ok_or_else(|| {
                    anyhow!("Variable {name:?} Character index {c_idx} not exists")
                })?
            }
            UniformVariable::Normal(var) => var,
        };

        Ok((info, vm_var, idx))
    }

    pub fn index_local_var(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, u32)> {
        let func_name = func_name.get_key(self);
        let name = name.get_key(self);

        let target = self.read_int("TARGET", &[])?;

        let (info, var) = self.get_local_var(func_name, name)?;

        let (c_idx, idx) = info.calculate_single_idx(args);

        let vm_var = match var {
            UniformVariable::Character(cvar) => {
                let c_idx = c_idx.unwrap_or(target as u32);
                cvar.get_mut(c_idx as usize).ok_or_else(|| {
                    anyhow!("Variable {name:?}@{func_name:?} Character index {c_idx} not exists",)
                })?
            }
            UniformVariable::Normal(var) => var,
        };

        Ok((info, vm_var, idx))
    }

    pub fn index_maybe_local_var(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, u32)> {
        if self.is_local_var(func_name, name) {
            self.index_local_var(func_name, name, args)
        } else {
            self.index_var(name, args)
        }
    }

    pub fn get_local_var(
        &mut self,
        func_name: impl StrKeyLike,
        var: impl StrKeyLike,
    ) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        let func_name = func_name.get_key(self);
        let var = var.get_key(self);
        let (info, var) = self
            .local_variables
            .get_mut(&func_name)
            .unwrap()
            .get_mut(&var)
            .ok_or_else(|| anyhow!("Variable {:?} is not exists", var))?;

        let var = var.get_or_insert_with(|| {
            UniformVariable::with_character_len(&self.header, info, self.character_len)
        });
        Ok((info, var))
    }

    pub fn is_local_var(&self, func: impl StrKeyLike, var: impl StrKeyLike) -> bool {
        match self.local_variables.get(&func.get_key(self)) {
            Some(v) => v.contains_key(&var.get_key(self)),
            None => false,
        }
    }

    pub fn get_maybe_local_var2(
        &mut self,
        func1: impl StrKeyLike,
        var1: impl StrKeyLike,
        func2: impl StrKeyLike,
        var2: impl StrKeyLike,
    ) -> Result<[(&mut VariableInfo, &mut UniformVariable); 2]> {
        let func1_name = func1.get_key(self);
        let func2_name = func2.get_key(self);
        let var1 = var1.get_key(self);
        let var2 = var2.get_key(self);

        match (
            self.is_local_var(func1_name, var1),
            self.is_local_var(func2_name, var2),
        ) {
            (true, true) if func1_name == func2_name => {
                let [(info1, var1), (info2, var2)] = self
                    .local_variables
                    .get_mut(&func1_name)
                    .unwrap()
                    .get_many_mut([&var1, &var2])
                    .ok_or_else(|| anyhow!("Variable {var1:?} and {var2:?} are not exist"))?;

                let var1 = var1.get_or_insert_with(|| {
                    UniformVariable::with_character_len(&self.header, info1, self.character_len)
                });
                let var2 = var2.get_or_insert_with(|| {
                    UniformVariable::with_character_len(&self.header, info2, self.character_len)
                });

                Ok([(info1, var1), (info2, var2)])
            }
            (true, true) => {
                let [dic1, dic2] =
                    self.local_variables.get_many_mut([&func1_name, &func2_name]).unwrap();

                let (info1, var1) = dic1
                    .get_mut(&var1)
                    .ok_or_else(|| anyhow!("Variable {var1:?} is not exist"))?;

                let (info2, var2) = dic2
                    .get_mut(&var2)
                    .ok_or_else(|| anyhow!("Variable {var2:?} is not exist"))?;

                let var1 = var1.get_or_insert_with(|| {
                    UniformVariable::with_character_len(&self.header, info1, self.character_len)
                });
                let var2 = var2.get_or_insert_with(|| {
                    UniformVariable::with_character_len(&self.header, info2, self.character_len)
                });

                Ok([(info1, var1), (info2, var2)])
            }
            (false, false) => {
                let [(info1, var1), (info2, var2)] = self
                    .variables
                    .get_many_mut([&var1, &var2])
                    .ok_or_else(|| anyhow!("Variable {var1:?} and {var2:?} are not exist"))?;

                Ok([(info1, var1), (info2, var2)])
            }
            (var1_is_local, _) => {
                let (local_var, global_var, func_name) = if var1_is_local {
                    (var1, var2, func1_name)
                } else {
                    (var2, var1, func2_name)
                };

                let (local_info, local_var) = self
                    .local_variables
                    .get_mut(&func_name)
                    .unwrap()
                    .get_mut(&local_var)
                    .ok_or_else(|| anyhow!("Variable {local_var:?} is not exist"))?;

                let local_var = local_var.get_or_insert_with(|| {
                    UniformVariable::with_character_len(
                        &self.header,
                        local_info,
                        self.character_len,
                    )
                });

                let (info, var) = self
                    .variables
                    .get_mut(&global_var)
                    .ok_or_else(|| anyhow!("Variable {global_var:?} is not exist"))?;

                if var1_is_local {
                    Ok([(local_info, local_var), (info, var)])
                } else {
                    Ok([(info, var), (local_info, local_var)])
                }
            }
        }
    }

    pub fn get_maybe_local_var(
        &mut self,
        func: impl StrKeyLike,
        var: impl StrKeyLike,
    ) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        if self.is_local_var(func, var) {
            self.get_local_var(func, var)
        } else {
            self.get_var(var)
        }
    }

    pub fn reset_var(&mut self, var: impl StrKeyLike) -> Result<()> {
        let (info, var) = self.get_var(var)?;

        ensure!(!info.is_const, "Cannot reset const variable");

        if info.is_str {
            match var {
                UniformVariable::Character(c) => {
                    c.iter_mut().for_each(|v| v.as_str().unwrap().fill(String::new()))
                }
                UniformVariable::Normal(v) => v.as_str().unwrap().fill(String::new()),
            }
        } else {
            match var {
                UniformVariable::Character(c) => {
                    c.iter_mut().for_each(|v| v.as_int().unwrap().fill(info.default_int))
                }
                UniformVariable::Normal(v) => v.as_int().unwrap().fill(info.default_int),
            }
        }

        Ok(())
    }

    pub fn get_var(
        &mut self,
        var: impl StrKeyLike,
    ) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        let var = var.get_key(self);
        let (l, r) = self
            .variables
            .get_mut(&var)
            .ok_or_else(|| anyhow!("Variable {var:?} is not exists"))?;

        Ok((l, r))
    }

    pub fn get_var2(
        &mut self,
        v1: impl StrKeyLike,
        v2: impl StrKeyLike,
    ) -> Result<(
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
    )> {
        match self.variables.get_many_mut([&v1.get_key(self), &v2.get_key(self)]) {
            Some([(ll, lr), (rl, rr)]) => Ok(((ll, lr), (rl, rr))),
            None => {
                bail!("Variable {v1:?} or {v2:?} is not exists");
            }
        }
    }

    pub fn get_var3(
        &mut self,
        v1: impl StrKeyLike,
        v2: impl StrKeyLike,
        v3: impl StrKeyLike,
    ) -> Result<(
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
    )> {
        match self
            .variables
            .get_many_mut([&v1.get_key(self), &v2.get_key(self), &v3.get_key(self)])
        {
            Some([(l1, r1), (l2, r2), (l3, r3)]) => Ok(((l1, r1), (l2, r2), (l3, r3))),
            None => {
                bail!("Variable {v1:?} or {v2:?} or {v3:?} is not exists");
            }
        }
    }

    pub fn reset_data(&mut self, header: &HeaderInfo) -> Result<()> {
        self.character_len = 0;
        for var in self.variables.values_mut() {
            var.1 = UniformVariable::new(header, &var.0);
        }
        self.init(header)?;

        Ok(())
    }

    pub fn swap_chara(&mut self, a: u32, b: u32) {
        self.variables.values_mut().for_each(|(_, var)| {
            var.swap_chara(a, b);
        });
    }

    pub fn add_chara(&mut self) {
        self.character_len += 1;
        self.variables.values_mut().for_each(|(info, var)| {
            var.add_chara(&self.header, info);
        });
    }

    pub fn add_copy_chara(&mut self, idx: u32) {
        self.character_len += 1;
        self.variables.values_mut().for_each(|(_, var)| {
            var.add_copy_chara(idx);
        });
    }

    pub fn copy_chara(&mut self, from: u32, to: u32) {
        self.variables.values_mut().for_each(|(_, var)| {
            var.copy_chara(from, to);
        });
    }

    pub fn del_chara(&mut self, idx: u32) {
        self.character_len -= 1;
        self.variables.values_mut().for_each(|(_, var)| {
            var.del_chara(idx);
        });
    }

    pub fn del_chara_list(&mut self, list: &BTreeSet<u32>) {
        self.character_len -= list.len() as u32;
        self.variables.values_mut().for_each(|(_, var)| {
            var.del_chara_list(list);
        });
    }

    pub fn get_chara(&mut self, target: i64) -> Result<Option<usize>> {
        let (_, no_var) = self.get_var(KnownVariableNames::No)?;
        match no_var {
            UniformVariable::Character(c) => {
                for (idx, var) in c.iter_mut().enumerate() {
                    if var.as_int()?[0] == target {
                        return Ok(Some(idx));
                    }
                }

                Ok(None)
            }
            UniformVariable::Normal(_) => bail!("NO can't be normal variable"),
        }
    }

    pub fn init(&mut self, header: &HeaderInfo) -> Result<()> {
        macro_rules! set {
            ($name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_normal().as_int()?;
                let arr = &header.replace.$field;

                var[..arr.len()].copy_from_slice(arr);
            };
        }

        set!(KnownVariableNames::PalamLv, palamlv_init);
        set!(KnownVariableNames::ExpLv, explv_init);

        self.get_var("RELATION")?.0.default_int = header.replace.relation_init;
        *self.ref_int("PBAND", &[])? = header.replace.pband_init;

        const NAMES: &[(&str, &str)] = &[
            ("ABLNAME", "ABL"),
            ("BASENAME", "BASE"),
            ("TALENTNAME", "TALENT"),
            ("ITEMNAME", "ITEM"),
            ("FLAGNAME", "FLAG"),
            ("EXNAME", "EX"),
            ("EXPNAME", "EXP"),
            ("CFLAGNAME", "CFLAG"),
            ("CSTRNAME", "CSTR"),
            ("STRNAME", "STR"),
            ("TSTRNAME", "TSTR"),
            ("EQUIPNAME", "EQUIP"),
            ("TEQUIPNAME", "TEQUIP"),
            ("TRAINNAME", "TRAIN"),
            ("PALAMNAME", "PALAM"),
            ("SOURCENAME", "SOURCE"),
            ("STAINNAME", "STAIN"),
            ("TCVARNAME", "TCVAR"),
            ("GLOBALNAME", "GLOBAL"),
            ("GLOBALSNAME", "GLOBALS"),
            ("MARKNAME", "MARK"),
            ("SAVESTRNAME", "SAVESTR"),
        ];

        for (var_name, var) in NAMES {
            let var = self.interner().get_or_intern_static(var);
            let arr = self.get_var(*var_name)?.1.assume_normal().as_str()?;
            if let Some(var_name_var) = header.var_name_var.get(&var) {
                for (idx, name) in var_name_var.iter() {
                    arr[*idx as usize] = name.to_string();
                }
            }
        }

        let price = self.get_var("ITEMPRICE")?.1.assume_normal().as_int()?;
        for (idx, value) in header.item_price.iter() {
            price[*idx as usize] = *value as i64;
        }

        Ok(())
    }

    pub fn set_character_template(&mut self, idx: u32, template: &CharacterTemplate) -> Result<()> {
        macro_rules! set {
            (@int $name:expr, $field:ident) => {
                self.get_var($name)?.1.assume_chara(idx).as_int()?[0] = template.$field as i64;
            };
            (@str $name:expr, $field:ident) => {
                self.get_var($name)?.1.assume_chara(idx).as_str()?[0] = template.$field.clone();
            };
            (@intarr $name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_chara(idx).as_int()?;

                for (k, v) in template.$field.iter() {
                    var[*k as usize] = *v as i64;
                }
            };
            (@strarr $name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_chara(idx).as_str()?;

                for (k, v) in template.$field.iter() {
                    var[*k as usize] = v.clone();
                }
            };
        }

        set!(@int "NO", no);
        set!(@int "ISASSI", is_assi);

        set!(@str "NAME", name);
        set!(@str "CALLNAME", call_name);
        set!(@str "NICKNAME", nick_name);

        set!(@intarr "ABL", abl);
        set!(@intarr "MAXBASE", base);
        set!(@intarr "BASE", base);
        set!(@intarr "EXP", exp);
        set!(@intarr "EX", ex);
        set!(@intarr "MARK", mark);
        set!(@intarr "TALENT", talent);
        set!(@intarr "CFLAG", cflag);
        set!(@intarr "RELATION", relation);

        set!(@strarr "CSTR", cstr);

        Ok(())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum VmVariable {
    Int(Vec<i64>),
    Str(Vec<String>),
}

impl VmVariable {
    pub fn new(header: &HeaderInfo, info: &VariableInfo) -> Self {
        let size = info.full_size();

        let mut ret = match info.is_str {
            false => Self::Int(vec![info.default_int; size]),
            true => Self::Str(vec![String::new(); size]),
        };

        for (idx, init_var) in info.init.iter().enumerate() {
            let _ = ret.set(idx as u32, header.const_eval_log_error(init_var));
        }

        ret
    }

    pub fn get(&self, idx: u32) -> Result<Value> {
        match self {
            Self::Int(i) => i
                .get(idx as usize)
                .ok_or_else(|| anyhow!("Variable out of range {} over {}", idx, i.len()))
                .copied()
                .map(Value::Int),
            Self::Str(i) => i
                .get(idx as usize)
                .ok_or_else(|| anyhow!("Variable out of range {} over {}", idx, i.len()))
                .cloned()
                .map(Value::String),
        }
    }

    pub fn set_or_default(&mut self, idx: u32, value: Option<Value>) -> Result<()> {
        match value {
            Some(v) => self.set(idx, v)?,
            None => match self {
                Self::Int(i) => {
                    *i.get_mut(idx as usize)
                        .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = 0;
                }
                Self::Str(i) => {
                    *i.get_mut(idx as usize)
                        .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = String::new();
                }
            },
        }

        Ok(())
    }

    pub fn set(&mut self, idx: u32, value: impl Into<Value>) -> Result<()> {
        match (self, value.into()) {
            (Self::Int(i), Value::Int(n)) => {
                *i.get_mut(idx as usize)
                    .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = n;
            }
            (Self::Str(i), Value::String(s)) => {
                *i.get_mut(idx as usize)
                    .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = s;
            }
            _ => bail!("Variable type mismatched"),
        }

        Ok(())
    }

    pub fn as_int(&mut self) -> Result<&mut Vec<i64>> {
        match self {
            Self::Int(i) => Ok(i),
            _ => bail!("Variable type is not Int"),
        }
    }

    pub fn as_str(&mut self) -> Result<&mut Vec<String>> {
        match self {
            Self::Str(i) => Ok(i),
            _ => bail!("Variable type is not Str"),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum UniformVariable {
    Normal(VmVariable),
    Character(Vec<VmVariable>),
}

impl UniformVariable {
    pub fn new(header: &HeaderInfo, info: &VariableInfo) -> Self {
        match info.is_chara {
            false => UniformVariable::Normal(VmVariable::new(header, info)),
            true => UniformVariable::Character(Vec::new()),
        }
    }

    pub fn with_character_len(
        header: &HeaderInfo,
        info: &VariableInfo,
        character_len: u32,
    ) -> Self {
        match info.is_chara {
            false => UniformVariable::Normal(VmVariable::new(header, info)),
            true => UniformVariable::Character(vec![
                VmVariable::new(header, info);
                character_len as usize
            ]),
        }
    }

    pub fn as_vm_var(&mut self, chara_no: u32) -> &mut VmVariable {
        match self {
            UniformVariable::Character(c) => &mut c[chara_no as usize],
            UniformVariable::Normal(v) => v,
        }
    }

    pub fn reset(&mut self, header: &HeaderInfo, info: &VariableInfo) {
        {
            match self {
                UniformVariable::Normal(var) => *var = VmVariable::new(header, info),
                UniformVariable::Character(cvar) => {
                    cvar.iter_mut().for_each(|var| {
                        *var = VmVariable::new(header, info);
                    });
                }
            }
        }
    }

    pub fn assume_normal(&mut self) -> &mut VmVariable {
        if let Self::Normal(v) = self {
            v
        } else {
            panic!("Variable is not normal variable")
        }
    }

    pub fn assume_chara_vec(&mut self) -> &mut Vec<VmVariable> {
        if let Self::Character(c) = self {
            c
        } else {
            panic!("Variable is not character variable")
        }
    }

    pub fn assume_chara(&mut self, idx: u32) -> &mut VmVariable {
        if let Self::Character(c) = self {
            &mut c[idx as usize]
        } else {
            panic!("Variable is not character variable")
        }
    }

    pub fn swap_chara(&mut self, a: u32, b: u32) {
        if let Self::Character(c) = self {
            c.swap(a as usize, b as usize);
        }
    }

    pub fn copy_chara(&mut self, a: u32, b: u32) {
        if let Self::Character(c) = self {
            let tmp = c[a as usize].clone();
            c[b as usize] = tmp;
        }
    }

    pub fn add_chara(&mut self, header: &HeaderInfo, info: &VariableInfo) {
        if let Self::Character(c) = self {
            c.push(VmVariable::new(header, info));
        }
    }

    pub fn add_copy_chara(&mut self, idx: u32) {
        if let Self::Character(c) = self {
            let prev_c = c[idx as usize].clone();
            c.push(prev_c);
        }
    }

    pub fn del_chara(&mut self, idx: u32) {
        if let Self::Character(c) = self {
            c.remove(idx as usize);
        }
    }

    pub fn del_chara_list(&mut self, list: &BTreeSet<u32>) {
        if let Self::Character(c) = self {
            for i in (0..c.len()).rev() {
                if !list.contains(&(i as u32)) {
                    c.remove(i);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum, IntoStaticStr, Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum KnownVariableNames {
    No,
    Count,
    NextCom,
    PrevCom,
    SelectCom,
    Master,
    Target,
    Assi,
    AssiPlay,
    Local,
    LocalS,
    Arg,
    ArgS,
    Result,
    ResultS,
    Palam,
    PalamLv,
    Exp,
    ExpLv,
    Up,
    Down,
    Cup,
    Cdown,
    Stain,
    #[allow(non_camel_case_types)]
    SaveData_Text,

    Base,
    DownBase,
    LoseBase,

    Train,
    Tflag,
    Tcvar,
    Tequip,
    Source,
    Juel,
    GotJuel,
}

pub trait StrKeyLike: Debug + Copy {
    fn get_key(self, var: &VariableStorage) -> StrKey;

    fn resolve_key(self, var: &VariableStorage) -> &str {
        var.resolve_key(self.get_key(var))
    }
}

impl StrKeyLike for StrKey {
    #[inline(always)]
    fn get_key(self, _: &VariableStorage) -> StrKey {
        self
    }
}

impl<'a> StrKeyLike for &'a String {
    fn get_key(self, var: &VariableStorage) -> StrKey {
        var.interner().get_or_intern(self)
    }
}

impl<'a> StrKeyLike for &'a str {
    fn get_key(self, var: &VariableStorage) -> StrKey {
        var.interner().get_or_intern(self)
    }
}

impl StrKeyLike for KnownVariableNames {
    #[inline(always)]
    fn get_key(self, var: &VariableStorage) -> StrKey {
        var.known_key(self)
    }
}

impl StrKeyLike for EventType {
    #[inline(always)]
    fn get_key(self, var: &VariableStorage) -> StrKey {
        var.event_key(self)
    }
}

impl StrKeyLike for FunctionIdentifier {
    fn get_key(self, var: &VariableStorage) -> StrKey {
        match self {
            FunctionIdentifier::Normal(key) => key,
            FunctionIdentifier::Event(ty) => ty.get_key(var),
        }
    }
}
