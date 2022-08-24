use anyhow::{anyhow, bail, Result};
use erars_ast::{Value, VariableInfo};
use erars_compiler::CharacterTemplate;
use hashbrown::HashMap;
use smol_str::SmolStr;
use std::iter;

#[derive(Clone)]
pub struct VariableStorage {
    character_len: usize,
    variables: HashMap<SmolStr, (VariableInfo, UniformVariable)>,
    local_variables: HashMap<SmolStr, HashMap<SmolStr, (VariableInfo, Option<UniformVariable>)>>,
    #[allow(unused)]
    global_variables: HashMap<SmolStr, VmVariable>,
}

impl VariableStorage {
    pub fn new(infos: &HashMap<SmolStr, VariableInfo>) -> Self {
        let mut variables = HashMap::new();
        let mut global_variables = HashMap::new();

        for (k, v) in infos {
            if v.is_global {
                assert!(!v.is_chara, "전역변수는 캐릭터변수일수 없습니다.");
                global_variables.insert(k.clone(), VmVariable::new(v));
            } else {
                variables.insert(k.clone(), (v.clone(), UniformVariable::new(v)));
            }
        }

        Self {
            character_len: 0,
            variables,
            local_variables: HashMap::new(),
            global_variables,
        }
    }

    pub fn reset_train_data(&mut self) -> Result<()> {
        macro_rules! set_var {
            ($name:expr, $value:expr) => {
                self.get_var($name)?
                    .1
                    .assume_normal()
                    .set(iter::empty(), Value::Int($value))?;
            };
            (@all $name:expr, $value:expr) => {
                match self.get_var($name)?.1 {
                    UniformVariable::Character(ref mut cvar) => {
                        for var in cvar {
                            match var {
                                VmVariable::Int1D(ref mut arr) => {
                                    arr.fill($value);
                                }
                                _ => bail!("Variable {} is not Int1D", $name),
                            }
                        }
                    }
                    UniformVariable::Normal(ref mut var) => match var {
                        VmVariable::Int1D(ref mut arr) => {
                            arr.fill($value);
                        }
                        _ => bail!("Variable {} is not Int1D", $name),
                    },
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

    pub fn set_result(&mut self, i: i64) {
        self.get_var("RESULT")
            .unwrap()
            .1
            .assume_normal()
            .set(iter::empty(), Value::Int(i))
            .unwrap();
    }

    pub fn set_results(&mut self, s: String) {
        self.get_var("RESULTS")
            .unwrap()
            .1
            .assume_normal()
            .set(iter::empty(), Value::String(s))
            .unwrap();
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
                    var_.set(iter::once(idx), init_var.clone()).unwrap();
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
        assert_ne!(l, r);

        match self.variables.get_many_mut([l, r]) {
            Some([(ll, lr), (rl, rr)]) => Ok(((ll, lr), (rl, rr))),
            None => {
                bail!("Variable {l} or {r} is not exists");
            }
        }
    }

    pub fn reset_data(&mut self) {
        self.character_len = 0;
        for var in self.variables.values_mut() {
            var.1 = UniformVariable::new(&var.0);
        }
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

    pub fn get_chara(&mut self, target: i64) -> Result<Option<usize>> {
        let (_, no_var) = self.variables.get_mut("NO").unwrap();
        match no_var {
            UniformVariable::Character(c) => {
                for (idx, var) in c.iter_mut().enumerate() {
                    if *var.get_int(iter::empty())? == target {
                        return Ok(Some(idx));
                    }
                }

                Ok(None)
            }
            UniformVariable::Normal(_) => bail!("NO can't be normal variable"),
        }
    }

    pub fn set_character_template(
        &mut self,
        idx: usize,
        template: &CharacterTemplate,
    ) -> Result<()> {
        macro_rules! set {
            (@int $name:expr, $field:ident) => {
                self.get_var($name)?
                    .1
                    .assume_chara(idx)
                    .set(iter::empty(), Value::Int(template.$field as i64))?;
            };
            (@str $name:expr, $field:ident) => {
                self.get_var($name)?
                    .1
                    .assume_chara(idx)
                    .set(iter::empty(), Value::String(template.$field.clone()))?;
            };
            (@intarr $name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_chara(idx);

                match var {
                    VmVariable::Int1D(arr) => {
                        for (k, v) in template.$field.iter() {
                            arr[*k as usize] = *v as i64;
                        }
                    }
                    _ => unreachable!(),
                }
            };
            (@strarr $name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_chara(idx);

                match var {
                    VmVariable::Str1D(arr) => {
                        for (k, v) in template.$field.iter() {
                            arr[*k as usize] = v.clone();
                        }
                    }
                    _ => unreachable!(),
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

#[derive(Clone, Debug)]
pub enum VmVariable {
    Int0D(i64),
    Int1D(Vec<i64>),
    Int2D(Vec<Vec<i64>>),
    Int3D(Vec<Vec<Vec<i64>>>),
    Str0D(String),
    Str1D(Vec<String>),
    Str2D(Vec<Vec<String>>),
    Str3D(Vec<Vec<Vec<String>>>),
}

impl VmVariable {
    pub fn new(info: &VariableInfo) -> Self {
        match (info.is_str, info.size.as_slice()) {
            (false, []) => Self::Int0D(info.default_int),
            (false, [a]) => Self::Int1D(vec![info.default_int; *a]),
            (false, [a, b]) => Self::Int2D(vec![vec![info.default_int; *a]; *b]),
            (false, [a, b, c]) => Self::Int3D(vec![vec![vec![info.default_int; *a]; *b]; *c]),
            (true, []) => Self::Str0D(String::new()),
            (true, [a]) => Self::Str1D(vec![String::new(); *a]),
            (true, [a, b]) => Self::Str2D(vec![vec![String::new(); *a]; *b]),
            (true, [a, b, c]) => Self::Str3D(vec![vec![vec![String::new(); *a]; *b]; *c]),
            _ => panic!("size length can't be greater than 3"),
        }
    }

    pub fn set(&mut self, args: impl Iterator<Item = usize>, value: Value) -> Result<()> {
        match self {
            VmVariable::Int0D(..)
            | VmVariable::Int1D(..)
            | VmVariable::Int2D(..)
            | VmVariable::Int3D(..) => *self.get_int(args)? = value.try_into()?,
            VmVariable::Str0D(..)
            | VmVariable::Str1D(..)
            | VmVariable::Str2D(..)
            | VmVariable::Str3D(..) => *self.get_str(args)? = value.try_into()?,
        }

        Ok(())
    }

    pub fn get(&mut self, args: impl Iterator<Item = usize>) -> Result<Value> {
        match self {
            VmVariable::Int0D(..)
            | VmVariable::Int1D(..)
            | VmVariable::Int2D(..)
            | VmVariable::Int3D(..) => self.get_int(args).map(Value::from),
            VmVariable::Str0D(..)
            | VmVariable::Str1D(..)
            | VmVariable::Str2D(..)
            | VmVariable::Str3D(..) => self.get_str(args).map(Value::from),
        }
    }

    pub fn get_int(&mut self, mut args: impl Iterator<Item = usize>) -> Result<&mut i64> {
        let mut last_arg;

        macro_rules! a {
            () => {{
                last_arg = args.next().unwrap_or(0);
                last_arg
            }};
        }
        match self {
            VmVariable::Int0D(s) => Ok(s),
            VmVariable::Int1D(s) => s
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index {last_arg} out of range")),
            VmVariable::Int2D(s) => s
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index {last_arg} out of range"))?
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index {last_arg} out of range")),
            VmVariable::Int3D(s) => s
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index {last_arg} out of range"))?
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index {last_arg} out of range"))?
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index {last_arg} out of range")),
            _ => bail!("Variable is Str type"),
        }
    }

    pub fn get_str(&mut self, mut args: impl Iterator<Item = usize>) -> Result<&mut String> {
        macro_rules! a {
            () => {
                args.next().unwrap_or(0)
            };
        }
        match self {
            VmVariable::Str0D(s) => Ok(s),
            VmVariable::Str1D(s) => s.get_mut(a!()).ok_or_else(|| anyhow!("Index out of range")),
            VmVariable::Str2D(s) => s
                .get_mut(a!())
                .unwrap()
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index out of range")),
            VmVariable::Str3D(s) => s
                .get_mut(a!())
                .unwrap()
                .get_mut(a!())
                .unwrap()
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index out of range")),
            _ => bail!("Variable is Int type"),
        }
    }
}

#[derive(Clone)]
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

    pub fn assume_normal(&mut self) -> &mut VmVariable {
        match self {
            Self::Normal(v) => v,
            _ => panic!("Variable is not normal variable"),
        }
    }

    pub fn assume_chara(&mut self, idx: usize) -> &mut VmVariable {
        match self {
            Self::Character(c) => &mut c[idx],
            _ => panic!("Variable is not character variable"),
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
}
