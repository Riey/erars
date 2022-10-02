use std::collections::{BTreeMap, BTreeSet};

use anyhow::{anyhow, bail, Result};
use erars_ast::{Value, VariableInfo};
use erars_compiler::{CharacterTemplate, ReplaceInfo};
use hashbrown::HashMap;
use rand::SeedableRng;
use rand_chacha::ChaCha20Rng;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::ui::ConsoleSender;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SerializableVariableStorage {
    pub description: String,
    character_len: usize,
    rand_seed: [u8; 32],
    variables: HashMap<SmolStr, (VariableInfo, UniformVariable)>,
    local_variables: HashMap<SmolStr, HashMap<SmolStr, (VariableInfo, Option<UniformVariable>)>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct SerializableGlobalVariableStorage {
    variables: HashMap<SmolStr, (VariableInfo, UniformVariable)>,
    local_variables: HashMap<SmolStr, HashMap<SmolStr, (VariableInfo, Option<UniformVariable>)>>,
}

#[derive(Clone)]
pub struct VariableStorage {
    character_len: usize,
    rng: ChaCha20Rng,
    variables: HashMap<SmolStr, (VariableInfo, UniformVariable)>,
    local_variables: HashMap<SmolStr, HashMap<SmolStr, (VariableInfo, Option<UniformVariable>)>>,
}

impl VariableStorage {
    pub fn new(infos: &HashMap<SmolStr, VariableInfo>) -> Self {
        let mut variables = HashMap::new();

        for (k, v) in infos {
            variables.insert(k.clone(), (v.clone(), UniformVariable::new(v)));
        }

        Self {
            character_len: 0,
            rng: ChaCha20Rng::from_entropy(),
            variables,
            local_variables: HashMap::new(),
        }
    }

    fn load_variables(
        &mut self,
        variables: HashMap<SmolStr, (VariableInfo, UniformVariable)>,
        local_variables: HashMap<
            SmolStr,
            HashMap<SmolStr, (VariableInfo, Option<UniformVariable>)>,
        >,
    ) {
        for (k, (info, var)) in variables {
            let (now_info, now_var) = match self.variables.get_mut(&k) {
                Some(v) => v,
                _ => {
                    log::warn!("Ignore non existing variable {k}");
                    continue;
                }
            };

            if info != *now_info {
                log::warn!("Info mismatched! Ignore load variable {k}");
                continue;
            }

            *now_var = var;
        }

        for (func_name, vars) in local_variables {
            let now_local_var = match self.local_variables.get_mut(&func_name) {
                Some(v) => v,
                None => {
                    log::warn!("Ignore non existing function {func_name}");
                    continue;
                }
            };

            for (k, (info, var)) in vars {
                let (now_info, now_var) = match now_local_var.get_mut(&k) {
                    Some(v) => v,
                    _ => {
                        log::warn!("Ignore non existing variable {func_name}@{k}");
                        continue;
                    }
                };

                if info != *now_info {
                    log::warn!("Info mismatched! Ignore load variable {k}");
                    continue;
                }

                *now_var = var;
            }
        }
    }

    pub fn load_global_serializable(
        &mut self,
        sav: SerializableGlobalVariableStorage,
        replace: &ReplaceInfo,
    ) -> Result<()> {
        self.load_variables(sav.variables, sav.local_variables);
        self.init_replace(replace)?;

        Ok(())
    }

    pub fn load_serializable(
        &mut self,
        sav: SerializableVariableStorage,
        replace: &ReplaceInfo,
    ) -> Result<()> {
        self.character_len = sav.character_len;
        self.rng = SeedableRng::from_seed(sav.rand_seed);

        self.load_variables(sav.variables, sav.local_variables);
        self.init_replace(replace)?;

        Ok(())
    }

    pub fn get_serializable(&self, description: String) -> SerializableVariableStorage {
        let variables = self
            .variables
            .iter()
            .filter_map(|(name, (info, var))| {
                if info.is_savedata && !info.is_global {
                    Some((name.clone(), (info.clone(), var.clone())))
                } else {
                    None
                }
            })
            .collect();

        let mut local_variables = HashMap::<_, HashMap<_, _>>::new();

        for (fn_name, vars) in self.local_variables.iter() {
            for (name, (info, var)) in vars.iter() {
                if info.is_savedata && !info.is_global {
                    local_variables
                        .entry(fn_name.clone())
                        .or_default()
                        .insert(name.clone(), (info.clone(), var.clone()));
                }
            }
        }

        SerializableVariableStorage {
            variables,
            description,
            local_variables,
            character_len: self.character_len,
            rand_seed: self.rng.get_seed(),
        }
    }

    pub fn get_global_serializable(&self) -> SerializableGlobalVariableStorage {
        let variables = self
            .variables
            .iter()
            .filter_map(|(name, (info, var))| {
                if info.is_savedata && info.is_global {
                    Some((name.clone(), (info.clone(), var.clone())))
                } else {
                    None
                }
            })
            .collect();

        let mut local_variables = HashMap::<_, HashMap<_, _>>::new();

        for (fn_name, vars) in self.local_variables.iter() {
            for (name, (info, var)) in vars.iter() {
                if var.is_some() && info.is_savedata && info.is_global {
                    local_variables
                        .entry(fn_name.clone())
                        .or_default()
                        .insert(name.clone(), (info.clone(), var.clone()));
                }
            }
        }

        SerializableGlobalVariableStorage {
            variables,
            local_variables,
        }
    }

    pub fn rng(&mut self) -> &mut impl rand::Rng {
        &mut self.rng
    }

    fn upcheck_internal(
        tx: &mut ConsoleSender,
        palam_name: &BTreeMap<u32, SmolStr>,
        palam: &mut Vec<i64>,
        up: &mut Vec<i64>,
        down: &mut Vec<i64>,
    ) -> Result<()> {
        itertools::multizip((palam.iter_mut(), up.iter_mut(), down.iter_mut()))
            .enumerate()
            .for_each(|(no, (p, u, d))| {
                if *u == 0 && *d == 0 {
                    return;
                }

                let name = palam_name.get(&(no as u32)).map(|s| s.as_str()).unwrap_or("");

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
        tx: &mut ConsoleSender,
        idx: usize,
        palam_name: &BTreeMap<u32, SmolStr>,
    ) -> Result<()> {
        let (palam, up, down) = self.get_var3("PALAM", "UP", "DOWN")?;

        let palam = palam.1.assume_chara(idx).as_int()?;
        let up = up.1.assume_normal().as_int()?;
        let down = down.1.assume_normal().as_int()?;

        Self::upcheck_internal(tx, palam_name, palam, up, down)
    }

    pub fn cupcheck(
        &mut self,
        tx: &mut ConsoleSender,
        idx: usize,
        palam_name: &BTreeMap<u32, SmolStr>,
    ) -> Result<()> {
        let (palam, up, down) = self.get_var3("PALAM", "CUP", "CDOWN")?;

        let palam = palam.1.assume_chara(idx).as_int()?;
        let up = up.1.assume_chara(idx).as_int()?;
        let down = down.1.assume_chara(idx).as_int()?;

        Self::upcheck_internal(tx, palam_name, palam, up, down)
    }

    pub fn reset_train_data(&mut self) -> Result<()> {
        macro_rules! set_var {
            ($name:expr, $value:expr) => {
                *self.ref_int($name, &[])? = $value;
            };
            (@all $name:expr, $value:expr) => {
                match self.get_var($name)?.1 {
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

        set_var!("ASSIPLAY", 0);
        set_var!("PREVCOM", -1);
        set_var!("NEXTCOM", -1);

        set_var!(@all "TFLAG", 0);
        set_var!(@all "TEQUIP", 0);
        set_var!(@all "PALAM", 0);
        set_var!(@all "STAIN", 0);
        set_var!(@all "SOURCE", 0);
        set_var!(@all "GOTJUEL", 0);

        Ok(())
    }

    pub fn get_result(&mut self) -> i64 {
        self.read_int("RESULT", &[]).unwrap()
    }

    pub fn get_results(&mut self) -> String {
        self.read_str("RESULTS", &[]).unwrap()
    }

    pub fn set_result(&mut self, i: i64) {
        *self.ref_int("RESULT", &[]).unwrap() = i;
    }

    pub fn set_results(&mut self, s: String) {
        *self.ref_str("RESULTS", &[]).unwrap() = s;
    }

    pub fn character_len(&self) -> usize {
        self.character_len
    }

    pub fn add_local_info(&mut self, func: SmolStr, var_name: SmolStr, info: VariableInfo) {
        self.local_variables
            .entry(func)
            .or_default()
            .insert(var_name, (info, None));
    }

    pub fn ref_int(&mut self, name: &str, args: &[usize]) -> Result<&mut i64> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(&mut var.as_int()?[idx])
    }

    pub fn ref_local_int(
        &mut self,
        func_name: &str,
        name: &str,
        args: &[usize],
    ) -> Result<&mut i64> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(&mut var.as_int()?[idx])
    }

    pub fn ref_maybe_local_int(
        &mut self,
        func_name: &str,
        name: &str,
        args: &[usize],
    ) -> Result<&mut i64> {
        if self.is_local_var(func_name, name) {
            self.ref_local_int(func_name, name, args)
        } else {
            self.ref_int(name, args)
        }
    }

    pub fn ref_str(&mut self, name: &str, args: &[usize]) -> Result<&mut String> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(&mut var.as_str()?[idx])
    }

    pub fn ref_local_str(
        &mut self,
        func_name: &str,
        name: &str,
        args: &[usize],
    ) -> Result<&mut String> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(&mut var.as_str()?[idx])
    }

    pub fn ref_maybe_local_str(
        &mut self,
        func_name: &str,
        name: &str,
        args: &[usize],
    ) -> Result<&mut String> {
        if self.is_local_var(func_name, name) {
            self.ref_local_str(func_name, name, args)
        } else {
            self.ref_str(name, args)
        }
    }

    pub fn read_int(&mut self, name: &str, args: &[usize]) -> Result<i64> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(var.as_int()?[idx])
    }

    pub fn read_local_int(&mut self, func_name: &str, name: &str, args: &[usize]) -> Result<i64> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(var.as_int()?[idx])
    }

    pub fn read_maybe_local_int(
        &mut self,
        func_name: &str,
        name: &str,
        args: &[usize],
    ) -> Result<i64> {
        if self.is_local_var(func_name, name) {
            self.read_local_int(func_name, name, args)
        } else {
            self.read_int(name, args)
        }
    }

    pub fn read_str(&mut self, name: &str, args: &[usize]) -> Result<String> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(var.as_str()?[idx].clone())
    }

    pub fn index_var(
        &mut self,
        name: &str,
        args: &[usize],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, usize)> {
        let target = if name != "TARGET" {
            self.read_int("TARGET", &[])?
        } else {
            // NEED for break recursion
            -1
        };

        let (info, var) = self.get_var(name)?;

        let (c_idx, idx) = info.calculate_single_idx(args);

        let vm_var = match var {
            UniformVariable::Character(cvar) => {
                let c_idx = c_idx.unwrap_or_else(|| target as usize);
                cvar.get_mut(c_idx)
                    .ok_or_else(|| anyhow!("Variable {name} Character index {c_idx} not exists"))?
            }
            UniformVariable::Normal(var) => var,
        };

        Ok((info, vm_var, idx))
    }

    pub fn index_local_var(
        &mut self,
        func_name: &str,
        name: &str,
        args: &[usize],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, usize)> {
        let target = self.read_int("TARGET", &[])?;

        let (info, var) = self.get_local_var(func_name, name)?;

        let (c_idx, idx) = info.calculate_single_idx(args);

        let vm_var = match var {
            UniformVariable::Character(cvar) => {
                let c_idx = c_idx.unwrap_or_else(|| target as usize);
                cvar.get_mut(c_idx).ok_or_else(|| {
                    anyhow!("Variable {name}@{func_name} Character index {c_idx} not exists")
                })?
            }
            UniformVariable::Normal(var) => var,
        };

        Ok((info, vm_var, idx))
    }

    pub fn index_maybe_local_var(
        &mut self,
        func_name: &str,
        name: &str,
        args: &[usize],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, usize)> {
        if self.is_local_var(func_name, name) {
            self.index_local_var(func_name, name, args)
        } else {
            self.index_var(name, args)
        }
    }

    pub fn get_local_var(
        &mut self,
        func_name: &str,
        var: &str,
    ) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        let (info, var) = self
            .local_variables
            .get_mut(func_name)
            .unwrap()
            .get_mut(var)
            .ok_or_else(|| anyhow!("Variable {} is not exists", var))?;

        if var.is_none() {
            let mut var_ = UniformVariable::new(info);
            if !info.init.is_empty() {
                let var_ = var_.assume_normal();
                for (idx, init_var) in info.init.iter().enumerate() {
                    var_.set(idx, init_var.clone()).unwrap();
                }
            }
            *var = Some(var_);
        }

        Ok((info, var.as_mut().unwrap()))
    }

    pub fn is_local_var(&self, func: &str, var: &str) -> bool {
        match self.local_variables.get(func) {
            Some(v) => v.contains_key(var),
            None => false,
        }
    }

    pub fn get_maybe_local_var(
        &mut self,
        func: &str,
        var: &str,
    ) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        if self.is_local_var(func, var) {
            self.get_local_var(func, var)
        } else {
            self.get_var(var)
        }
    }

    pub fn reset_var(&mut self, var: &str) -> Result<()> {
        let (info, var) = self.get_var(var)?;

        if info.is_str {
            match var {
                UniformVariable::Character(c) => {
                    c.par_iter_mut().for_each(|v| v.as_str().unwrap().fill(String::new()))
                }
                UniformVariable::Normal(v) => v.as_str().unwrap().fill(String::new()),
            }
        } else {
            match var {
                UniformVariable::Character(c) => c
                    .par_iter_mut()
                    .for_each(|v| v.as_int().unwrap().fill(info.default_int)),
                UniformVariable::Normal(v) => v.as_int().unwrap().fill(info.default_int),
            }
        }

        Ok(())
    }

    pub fn get_var(&mut self, var: &str) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        let (l, r) = self
            .variables
            .get_mut(var)
            .ok_or_else(|| anyhow!("Variable {} is not exists", var))?;

        Ok((l, r))
    }

    pub fn get_var2(
        &mut self,
        l: &str,
        r: &str,
    ) -> Result<(
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
    )> {
        match self.variables.get_many_mut([l, r]) {
            Some([(ll, lr), (rl, rr)]) => Ok(((ll, lr), (rl, rr))),
            None => {
                bail!("Variable {l} or {r} is not exists");
            }
        }
    }

    pub fn get_var3(
        &mut self,
        v1: &str,
        v2: &str,
        v3: &str,
    ) -> Result<(
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
    )> {
        match self.variables.get_many_mut([v1, v2, v3]) {
            Some([(l1, r1), (l2, r2), (l3, r3)]) => Ok(((l1, r1), (l2, r2), (l3, r3))),
            None => {
                bail!("Variable {v1} or {v2} or {v3} is not exists");
            }
        }
    }

    pub fn reset_data(&mut self, replace: &ReplaceInfo) -> Result<()> {
        self.character_len = 0;
        for var in self.variables.values_mut() {
            var.1 = UniformVariable::new(&var.0);
        }
        self.init_replace(replace)?;

        Ok(())
    }

    pub fn swap_chara(&mut self, a: usize, b: usize) {
        self.variables.values_mut().for_each(|(_, var)| {
            var.swap_chara(a, b);
        });
    }

    pub fn add_chara(&mut self) {
        self.character_len += 1;
        self.variables.values_mut().for_each(|(info, var)| {
            var.add_chara(info);
        });
    }

    pub fn del_chara(&mut self, idx: usize) {
        self.character_len -= 1;
        self.variables.values_mut().for_each(|(_, var)| {
            var.del_chara(idx);
        });
    }

    pub fn del_chara_list(&mut self, list: &BTreeSet<usize>) {
        self.character_len -= list.len();
        self.variables.values_mut().for_each(|(_, var)| {
            var.del_chara_list(list);
        });
    }

    pub fn get_chara(&mut self, target: i64) -> Result<Option<usize>> {
        let (_, no_var) = self.variables.get_mut("NO").unwrap();
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

    pub fn init_replace(&mut self, replace: &ReplaceInfo) -> Result<()> {
        macro_rules! set {
            ($name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_normal().as_int()?;
                let arr = &replace.$field;

                var[..arr.len()].copy_from_slice(arr);
            };
        }

        set!("PALAMLV", palamlv_init);
        set!("EXPLV", explv_init);

        self.get_var("RELATION")?.0.default_int = replace.relation_init;
        *self.ref_int("PBAND", &[])? = replace.pband_init;

        Ok(())
    }

    pub fn set_character_template(
        &mut self,
        idx: usize,
        template: &CharacterTemplate,
    ) -> Result<()> {
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
    fn new(info: &VariableInfo) -> Self {
        let size = info.full_size();

        match info.is_str {
            false => Self::Int(vec![info.default_int; size]),
            true => Self::Str(vec![String::new(); size]),
        }
    }

    pub fn get(&self, idx: usize) -> Result<Value> {
        match self {
            Self::Int(i) => i
                .get(idx)
                .ok_or_else(|| anyhow!("Variable out of range {} over {}", idx, i.len()))
                .copied()
                .map(Value::Int),
            Self::Str(i) => i
                .get(idx)
                .ok_or_else(|| anyhow!("Variable out of range {} over {}", idx, i.len()))
                .cloned()
                .map(Value::String),
        }
    }

    pub fn set(&mut self, idx: usize, value: impl Into<Value>) -> Result<()> {
        match (self, value.into()) {
            (Self::Int(i), Value::Int(n)) => {
                *i.get_mut(idx)
                    .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = n;
            }
            (Self::Str(i), Value::String(s)) => {
                *i.get_mut(idx)
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
    pub fn new(info: &VariableInfo) -> Self {
        match info.is_chara {
            false => UniformVariable::Normal(VmVariable::new(info)),
            true => UniformVariable::Character(Vec::new()),
        }
    }

    pub fn as_vm_var(&mut self, chara_no: usize) -> &mut VmVariable {
        match self {
            UniformVariable::Character(c) => &mut c[chara_no],
            UniformVariable::Normal(v) => v,
        }
    }

    pub fn assume_normal(&mut self) -> &mut VmVariable {
        match self {
            Self::Normal(v) => v,
            _ => panic!("Variable is not normal variable"),
        }
    }

    pub fn assume_chara_vec(&mut self) -> &mut Vec<VmVariable> {
        match self {
            Self::Character(c) => c,
            _ => panic!("Variable is not character variable"),
        }
    }

    pub fn assume_chara(&mut self, idx: usize) -> &mut VmVariable {
        match self {
            Self::Character(c) => &mut c[idx],
            _ => panic!("Variable is not character variable"),
        }
    }

    pub fn swap_chara(&mut self, a: usize, b: usize) {
        match self {
            UniformVariable::Character(c) => c.swap(a, b),
            _ => {}
        }
    }

    pub fn add_chara(&mut self, info: &VariableInfo) {
        match self {
            UniformVariable::Character(c) => c.push(VmVariable::new(info)),
            _ => {}
        }
    }

    pub fn del_chara(&mut self, idx: usize) {
        match self {
            Self::Character(c) => {
                c.remove(idx);
            }
            _ => {}
        }
    }

    pub fn del_chara_list(&mut self, list: &BTreeSet<usize>) {
        match self {
            Self::Character(c) => {
                for i in (0..c.len()).rev() {
                    if !list.contains(&i) {
                        c.remove(i);
                    }
                }
            }
            _ => {}
        }
    }
}
