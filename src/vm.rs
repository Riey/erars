use std::sync::Arc;
use std::{fmt, io, iter};

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use itertools::Itertools;
use smol_str::SmolStr;
use strum::Display;

use hashbrown::HashMap;

use erars_ast::{
    BeginType, BinaryOperator, BuiltinCommand, EventType, PrintFlags, ScriptPosition,
    UnaryOperator, Value, VariableInfo,
};
use erars_compiler::{CharacterTemplate, HeaderInfo, Instruction, ParserContext};

use crate::function::{FunctionBody, FunctionDic};
use crate::ui::{ConsoleChannel, ConsoleMessage, ConsoleResult, InputRequest};

macro_rules! report_error {
    ($chan:expr, $($t:tt)+) => {
        log::error!($($t)+);
        $chan.send_msg(ConsoleMessage::Print(format!($($t)+)));
        $chan.send_msg(ConsoleMessage::NewLine);
    };
}

#[derive(Clone, Debug)]
enum VmVariable {
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
enum UniformVariable {
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
}

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

    pub fn add_local_info(&mut self, func: SmolStr, var_name: SmolStr, info: VariableInfo) {
        self.local_variables
            .entry(func)
            .or_default()
            .insert(var_name, (info, None));
    }

    fn get_local_var(
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

    fn is_local_var(&self, func: &str, var: &str) -> bool {
        match self.local_variables.get(func) {
            Some(v) => v.contains_key(var),
            None => false,
        }
    }

    fn get_maybe_local_var(
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

    fn get_var(&mut self, var: &str) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        let (l, r) = self
            .variables
            .get_mut(var)
            .ok_or_else(|| anyhow!("Variable {} is not exists", var))?;

        Ok((l, r))
    }

    fn get_var2(
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

#[derive(Display, Debug, Clone, Copy)]
enum Workflow {
    Return,
    Exit,
}

#[derive(Debug, Clone)]
struct Callstack {
    func_name: Result<SmolStr, EventType>,
    file_path: SmolStr,
    script_position: ScriptPosition,
    stack_base: usize,
}

#[derive(Clone)]
struct VariableRef {
    name: String,
    func_name: String,
    idxs: ArrayVec<usize, 4>,
}

impl fmt::Debug for VariableRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.name, self.func_name)?;

        for idx in self.idxs.iter() {
            write!(f, ":{}", idx)?;
        }

        Ok(())
    }
}

impl fmt::Display for VariableRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        for idx in self.idxs.iter() {
            write!(f, ":{}", idx)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
enum LocalValue {
    Value(Value),
    VarRef(VariableRef),
}

impl<T> From<T> for LocalValue
where
    Value: From<T>,
{
    fn from(v: T) -> Self {
        Self::Value(Value::from(v))
    }
}

impl From<VariableRef> for LocalValue {
    fn from(r: VariableRef) -> Self {
        Self::VarRef(r)
    }
}

impl TryFrom<LocalValue> for VariableRef {
    type Error = anyhow::Error;

    fn try_from(value: LocalValue) -> Result<VariableRef, Self::Error> {
        match value {
            LocalValue::VarRef(v) => Ok(v),
            _ => bail!("LocalValue type is not VariableRef"),
        }
    }
}

#[derive(Clone)]
pub struct VmContext {
    var: VariableStorage,
    header_info: Arc<HeaderInfo>,
    begin: Option<BeginType>,
    stack: Vec<LocalValue>,
    call_stack: Vec<Callstack>,
    current_pos: ScriptPosition,
    color: u32,
    hl_color: u32,
    bg_color: u32,
}

impl VmContext {
    pub fn new(header_info: Arc<HeaderInfo>) -> Self {
        Self {
            var: VariableStorage::new(&header_info.global_variables),
            header_info,
            begin: Some(BeginType::Title),
            stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(512),
            color: u32::from_le_bytes([0xFF, 0xFF, 0xFF, 0x00]),
            hl_color: u32::from_le_bytes([0xFF, 0xFF, 0x00, 0x00]),
            bg_color: u32::from_le_bytes([0x00, 0x00, 0x00, 0x00]),
            current_pos: ScriptPosition::default(),
        }
    }

    pub fn var_mut(&mut self) -> &mut VariableStorage {
        &mut self.var
    }

    fn read_var_ref(&mut self, var_ref: &VariableRef) -> Result<Value> {
        let value = match var_ref.name.as_str() {
            "GAMEBASE_VERSION" => Value::Int(0),
            "GAMEBASE_AUTHOR" => "Riey".into(),
            "GAMEBASE_YEAR" => Value::Int(2022),
            "GAMEBASE_TITLE" => "eraTHYMKR".into(),
            "GAMEBASE_INFO" => "".into(),
            "CHARANUM" => (self.var.character_len as i64).into(),
            "ITEMPRICE" => {
                let arg = var_ref.idxs[0] as u32;
                self.header_info
                    .item_price
                    .get(&arg)
                    .copied()
                    .unwrap_or(0)
                    .into()
            }
            "ITEMNAME" | "FLAGNAME" | "ABLNAME" => {
                let name = var_ref.name.as_str().strip_suffix("NAME").unwrap();
                let arg = var_ref.idxs[0] as u32;
                self.header_info
                    .var_name_var
                    .get(name)
                    .and_then(|d| Some(d.get(&arg)?.as_str()))
                    .unwrap_or("")
                    .into()
            }
            _ => {
                let (var, args) = self.resolve_var_ref(&var_ref)?;

                var.get(args)?
            }
        };
        Ok(value)
    }

    fn set_var_ref(&mut self, var_ref: &VariableRef, value: Value) -> Result<()> {
        let (var, args) = self.resolve_var_ref(var_ref)?;
        var.set(args, value)?;
        Ok(())
    }

    fn resolve_var_ref<'c>(
        &'c mut self,
        r: &VariableRef,
    ) -> Result<(&'c mut VmVariable, arrayvec::IntoIter<usize, 4>)> {
        let target = *self
            .var
            .get_var("TARGET".into())
            .unwrap()
            .1
            .assume_normal()
            .get_int(iter::empty())?;

        let (info, var, mut args) = self.resolve_var_ref_raw(r)?;

        match var {
            UniformVariable::Character(c) => {
                let no = if args.len() < info.arg_len() {
                    target.try_into()?
                } else {
                    args.next().unwrap()
                };
                let c_len = c.len();
                Ok((
                    c.get_mut(no).ok_or_else(|| {
                        anyhow!("캐릭터 번호가 너무 큽니다. {no}, 최대 {}", c_len)
                    })?,
                    args,
                ))
            }
            UniformVariable::Normal(v) => Ok((v, args)),
        }
    }

    fn resolve_var_ref_raw<'c>(
        &'c mut self,
        r: &VariableRef,
    ) -> Result<(
        &'c mut VariableInfo,
        &'c mut UniformVariable,
        arrayvec::IntoIter<usize, 4>,
    )> {
        let (info, var) = self.var.get_maybe_local_var(&r.func_name, &r.name)?;

        Ok((info, var, r.idxs.clone().into_iter()))
    }

    pub fn new_func(&mut self, func_name: Result<SmolStr, EventType>, file_path: SmolStr) {
        if let Some(last) = self.call_stack.last_mut() {
            last.script_position = std::mem::take(&mut self.current_pos);
        }

        self.call_stack.push(Callstack {
            func_name,
            file_path,
            script_position: ScriptPosition::default(),
            stack_base: self.stack.len(),
        });
    }

    pub fn end_func(&mut self) {
        self.call_stack.pop();
    }

    pub fn return_func(&mut self) -> Result<impl Iterator<Item = Value>> {
        let call_stack = self.call_stack.last().unwrap();
        let count = self.stack.len() - call_stack.stack_base;
        Ok(self.take_value_list(count as u32)?.into_iter())
    }

    fn take_arg_list(&mut self, count: u32) -> Result<ArrayVec<usize, 4>> {
        self.take_value_list(count)?
            .into_iter()
            .map(usize::try_from)
            .collect()
    }

    fn take_value_list(&mut self, count: u32) -> Result<ArrayVec<Value, 16>> {
        let mut ret = ArrayVec::new();
        let list = self.take_list(count).collect::<ArrayVec<LocalValue, 16>>();

        for arg in list {
            match arg {
                LocalValue::Value(v) => ret.push(v),
                LocalValue::VarRef(var) => {
                    ret.push(self.read_var_ref(&var)?);
                }
            }
        }

        Ok(ret)
    }

    fn take_list(&mut self, count: u32) -> impl Iterator<Item = LocalValue> + '_ {
        self.stack.drain(self.stack.len() - count as usize..)
    }

    fn dup(&mut self) {
        let last = self.stack.last().unwrap().clone();
        self.stack.push(last);
    }

    fn dup_prev(&mut self) {
        let prev = self.stack[self.stack.len() - 2].clone();
        self.stack.push(prev);
    }

    fn push_var_ref(&mut self, name: String, func_name: String, idxs: ArrayVec<usize, 4>) {
        let var_ref = VariableRef {
            func_name,
            name,
            idxs,
        };
        self.stack.push(LocalValue::VarRef(var_ref));
    }

    fn push(&mut self, value: impl Into<Value>) {
        self.stack.push(LocalValue::Value(value.into()));
    }

    fn pop(&mut self) -> LocalValue {
        // if this failed, it must be compiler error
        self.stack
            .pop()
            .unwrap_or_else(|| unreachable!("Unknown compiler error"))
    }

    fn pop_value(&mut self) -> Result<Value> {
        match self.stack.pop() {
            Some(LocalValue::Value(v)) => Ok(v),
            Some(LocalValue::VarRef(var_ref)) => self.read_var_ref(&var_ref),
            None => bail!("Stack is empty"),
        }
    }

    fn pop_var_ref(&mut self) -> Result<VariableRef> {
        self.pop().try_into()
    }

    fn pop_str(&mut self) -> Result<String> {
        self.pop_value().and_then(|v| v.try_into_str())
    }

    #[allow(unused)]
    fn pop_int(&mut self) -> Result<i64> {
        self.pop_value().and_then(|v| v.try_into_int())
    }
}

pub struct TerminalVm {
    dic: FunctionDic,
}

impl TerminalVm {
    pub fn new(function_dic: FunctionDic) -> Self {
        Self { dic: function_dic }
    }

    fn run_instruction(
        &self,
        func_name: &str,
        goto_labels: &HashMap<SmolStr, u32>,
        inst: &Instruction,
        cursor: &mut usize,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        log::trace!(
            "[{func_name}] `{inst:?}`, stack: {stack:?}",
            stack = ctx.stack
        );

        match inst {
            Instruction::ReportPosition(pos) => ctx.current_pos = pos.clone(),
            Instruction::LoadInt(n) => ctx.push(*n),
            Instruction::LoadStr(s) => ctx.push(s.to_string()),
            Instruction::Nop => {}
            Instruction::Pop => drop(ctx.pop()),
            Instruction::Duplicate => ctx.dup(),
            Instruction::DuplicatePrev => ctx.dup_prev(),
            Instruction::ReadVar => {
                let value = ctx.pop_value()?;
                ctx.push(value);
            }
            Instruction::EvalFormString => {
                let form = ctx.pop_str()?;
                let parser_ctx = ParserContext::new(ctx.header_info.clone(), "FORMS.ERB".into());
                let expr = erars_compiler::normal_form_str(&parser_ctx)(&form)
                    .unwrap()
                    .1;
                let insts = erars_compiler::compile_expr(expr).unwrap();

                for inst in insts.iter() {
                    self.run_instruction(func_name, goto_labels, inst, &mut 0, chan, ctx)?;
                }
            }
            Instruction::GotoLabel => {
                *cursor = goto_labels[ctx.pop_str()?.as_str()] as usize;
            }
            Instruction::LoadExternVarRef(c) => {
                let func_extern = ctx.pop_str()?;
                let name = ctx.pop_str()?;
                let args = ctx.take_arg_list(*c)?;
                ctx.push_var_ref(name, func_extern, args);
            }
            Instruction::LoadVarRef(c) => {
                let name = ctx.pop_str()?;
                let args = ctx.take_arg_list(*c)?;
                ctx.push_var_ref(name, func_name.to_string(), args);
            }
            Instruction::StoreVar => {
                let var_ref = ctx.pop_var_ref()?;
                let value = ctx.pop_value()?;

                ctx.set_var_ref(&var_ref, value)?;
            }
            Instruction::ReuseLastLine => {
                chan.send_msg(ConsoleMessage::ReuseLastLine(ctx.pop_str()?));
                chan.request_redraw();
            }
            Instruction::Print(flags) => {
                chan.send_msg(ConsoleMessage::Print(ctx.pop_str()?));
                if flags.contains(PrintFlags::NEWLINE) {
                    chan.send_msg(ConsoleMessage::NewLine);
                }
                chan.request_redraw();

                // TODO: PRINTW
            }
            Instruction::CallMethod(c) => {
                let func = ctx.pop_str()?;
                match func.as_str() {
                    "STRLENS" => {
                        let s = ctx.pop_str()?;
                        ctx.push(encoding_rs::SHIFT_JIS.encode(&s).0.as_ref().len() as i64);
                    }
                    "STRLENSU" => {
                        let s = ctx.pop_str()?;
                        ctx.push(s.len() as i64);
                    }
                    "TOSTR" => {
                        let ret;

                        match *c {
                            1 => {
                                let value = ctx.pop_int()?;
                                ret = value.to_string();
                            }
                            2 => {
                                let _format = ctx.pop_str()?;
                                let value = ctx.pop_int()?;
                                ret = format!("{00}", value);
                            }
                            _ => bail!("TOSTR의 매개변수는 1개 또는 2개여야 합니다."),
                        }

                        ctx.push(ret);
                    }
                    "MAX" => {
                        let args = ctx.take_value_list(*c)?.into_iter();

                        ctx.push(args.max().unwrap_or(Value::Int(0)));
                    }
                    "MIN" => {
                        let args = ctx.take_value_list(*c)?.into_iter();

                        ctx.push(args.min().unwrap_or(Value::Int(0)));
                    }
                    "LIMIT" => {
                        if *c != 3 {
                            bail!("LIMIT의 매개변수는 3개여야합니다");
                        } else {
                            let high = ctx.pop_int()?;
                            let low = ctx.pop_int()?;
                            let v = ctx.pop_int()?;

                            ctx.push(v.clamp(low, high));
                        }
                    }
                    label => {
                        let args = ctx.take_value_list(*c)?;

                        match self.call(label, args.as_slice(), chan, ctx)? {
                            Workflow::Exit => return Ok(Some(Workflow::Exit)),
                            Workflow::Return => {}
                        }
                    }
                }
            }
            Instruction::Call(c) => {
                let func = ctx.pop_str()?;
                let args = ctx.take_value_list(*c)?;

                match func.as_str() {
                    _ => {
                        self.call(&func, &args, chan, ctx)?;
                    }
                }
            }
            Instruction::Begin(b) => {
                ctx.begin = Some(*b);
                return Ok(Some(Workflow::Exit));
            }
            Instruction::ConcatString(c) => {
                let args = ctx.take_value_list(*c)?;
                let ret = args
                    .into_iter()
                    .fold(String::new(), |s, l| s + l.into_str().as_str());
                ctx.push(ret);
            }
            Instruction::Times(t) => {
                let arg = ctx.pop_int()?;
                let ret = (arg as f32 * t.into_inner()) as i64;
                ctx.push(ret);
            }
            Instruction::UnaryOperator(op) => match op {
                UnaryOperator::Not => {
                    let operand = ctx.pop_value()?.as_bool();
                    ctx.push(!operand);
                }
                UnaryOperator::Minus => {
                    let operand = ctx.pop_int()?;
                    ctx.push(-operand);
                }
            },
            Instruction::BinaryOperator(op) => {
                let rhs = ctx.pop_value()?;
                let lhs = ctx.pop_value()?;

                let ret = match op {
                    BinaryOperator::Add => match lhs {
                        Value::Int(i) => Value::Int(i + rhs.try_into_int()?),
                        Value::String(s) => Value::String(s + rhs.into_str().as_str()),
                    },
                    BinaryOperator::Sub => Value::Int(lhs.try_into_int()? - rhs.try_into_int()?),
                    BinaryOperator::Div => Value::Int(lhs.try_into_int()? / rhs.try_into_int()?),
                    BinaryOperator::Mul => Value::Int(lhs.try_into_int()? * rhs.try_into_int()?),
                    BinaryOperator::Rem => Value::Int(lhs.try_into_int()? % rhs.try_into_int()?),
                    BinaryOperator::Less => {
                        Value::Int((lhs.try_into_int()? < rhs.try_into_int()?).into())
                    }
                    BinaryOperator::LessOrEqual => {
                        Value::Int((lhs.try_into_int()? <= rhs.try_into_int()?).into())
                    }
                    BinaryOperator::Greater => {
                        Value::Int((lhs.try_into_int()? > rhs.try_into_int()?).into())
                    }
                    BinaryOperator::GreaterOrEqual => {
                        Value::Int((lhs.try_into_int()? >= rhs.try_into_int()?).into())
                    }
                    BinaryOperator::Equal => Value::Int(i64::from(lhs == rhs)),
                    BinaryOperator::NotEqual => Value::Int(i64::from(lhs != rhs)),
                    BinaryOperator::And => Value::Int(i64::from(lhs.as_bool() && rhs.as_bool())),
                    BinaryOperator::Or => Value::Int(i64::from(lhs.as_bool() || rhs.as_bool())),
                    BinaryOperator::Xor => Value::Int(i64::from(lhs.as_bool() ^ rhs.as_bool())),
                    BinaryOperator::BitAnd => {
                        Value::Int(i64::from(lhs.try_into_int()? & rhs.try_into_int()?))
                    }
                    BinaryOperator::BitOr => {
                        Value::Int(i64::from(lhs.try_into_int()? | rhs.try_into_int()?))
                    }
                    BinaryOperator::BitXor => {
                        Value::Int(i64::from(lhs.try_into_int()? ^ rhs.try_into_int()?))
                    }
                    BinaryOperator::Lhs => {
                        Value::Int(i64::from(lhs.try_into_int()? << rhs.try_into_int()?))
                    }
                    BinaryOperator::Rhs => {
                        Value::Int(i64::from(lhs.try_into_int()? >> rhs.try_into_int()?))
                    }
                };

                ctx.push(ret);
            }
            Instruction::Goto(no) => {
                *cursor = *no as usize;
            }
            Instruction::GotoIfNot(no) => {
                let cond = ctx.pop_value()?.as_bool();
                if !cond {
                    *cursor = *no as usize;
                }
            }
            Instruction::GotoIf(no) => {
                let cond = ctx.pop_value()?.as_bool();
                if cond {
                    *cursor = *no as usize;
                }
            }
            Instruction::SetAlignment(align) => {
                chan.send_msg(ConsoleMessage::Alignment(*align));
            }
            Instruction::PadStr(align) => {
                use erars_ast::Alignment;
                use pad::{Alignment as PadAlign, PadStr};

                let size = ctx.pop_int()?;
                let text = match ctx.pop_value()? {
                    Value::String(s) => s,
                    Value::Int(i) => i.to_string(),
                };

                let align = match align {
                    Alignment::Left => PadAlign::Left,
                    Alignment::Center => PadAlign::Middle,
                    Alignment::Right => PadAlign::Right,
                };

                ctx.push(text.pad_to_width_with_alignment(size as usize, align));
            }
            Instruction::Command(com, c) => {
                let mut args = ctx.take_list(*c).collect::<ArrayVec<_, 8>>().into_iter();

                macro_rules! pop {
                    () => {
                        pop!(@opt).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
                    };
                    (@opt) => {
                        args.next()
                    };
                    (@var) => {
                        match args.next() {
                            Some(LocalValue::VarRef(r)) => r,
                            Some(LocalValue::Value(_)) => bail!("매개변수가 VarRef가 아닙니다"),
                            None => bail!("매개변수가 부족합니다"),
                        }
                    };
                    (@value) => {
                        pop!(@opt @value).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
                    };
                    (@opt @value) => {
                        match args.next() {
                            Some(LocalValue::VarRef(r)) => Some(ctx.read_var_ref(&r)?),
                            Some(LocalValue::Value(v)) => Some(v),
                            None => None,
                        }
                    };
                    (@$t:ty) => {
                        pop!(@opt @$t).ok_or_else(|| anyhow!("매개변수가 부족합니다"))?
                    };
                    (@opt @$t:ty) => {
                        pop!(@opt @value).and_then(|v| <$t>::try_from(v).ok())
                    };
                }

                match com {
                    BuiltinCommand::Unicode => {
                        let code = pop!(@i64).try_into()?;

                        ctx.push(
                            char::from_u32(code)
                                .ok_or_else(|| {
                                    anyhow!("u32 {code} is not valid unicode codepoint")
                                })?
                                .to_string(),
                        );
                    }
                    BuiltinCommand::Throw => {
                        let msg = pop!(@opt @String);

                        match msg {
                            Some(msg) => bail!("스크립트에서 예외발생: {msg}"),
                            None => bail!("스크립트에서 예외발생"),
                        }
                    }
                    BuiltinCommand::Varset => {
                        let var = pop!(@var);
                        let value = pop!(@opt @value);
                        let start = pop!(@opt @usize);
                        let end = pop!(@opt @usize);

                        let (info, var, args) = ctx.resolve_var_ref_raw(&var)?;

                        match (value, start, end) {
                            (None, None, None) => {
                                *var = UniformVariable::new(info);
                            }
                            (Some(value), start, end) => {
                                let var = var.assume_normal();
                                let start = start.unwrap_or(0);
                                let end = end.unwrap_or(info.size.last().copied().unwrap_or(1));

                                for i in start..end {
                                    let args = args.clone().chain(Some(i));
                                    var.set(args, value.clone())?;
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    BuiltinCommand::Split => {
                        let s = pop!(@String);
                        let delimiter = pop!(@String);
                        let mut var = pop!(@var);

                        for (idx, part) in s.split(delimiter.as_str()).enumerate() {
                            var.idxs.push(idx);

                            ctx.set_var_ref(&var, part.into())?;

                            var.idxs.pop();
                        }
                    }
                    BuiltinCommand::Bar => {
                        let length = pop!(@i64);
                        let max = pop!(@i64);
                        let var = pop!(@i64);

                        let mut ret = String::with_capacity(length as usize);

                        ret.push('[');

                        let i = var * length;
                        let fill = (i.checked_div(max).unwrap_or(length)).clamp(0, length);

                        let blank = length - fill;

                        const FILL_CHAR: char = '*';
                        const BLANK_CHAR: char = '.';

                        for _ in 0..fill {
                            ret.push(FILL_CHAR);
                        }

                        for _ in 0..blank {
                            ret.push(BLANK_CHAR);
                        }

                        ret.push(']');

                        chan.send_msg(ConsoleMessage::Print(ret));
                        chan.send_msg(ConsoleMessage::NewLine);
                    }
                    BuiltinCommand::ReturnF => {
                        let ret = pop!(@value);

                        if args.next().is_some() {
                            log::warn!("RETURNF는 한개의 값만 반환할 수 있습니다.");
                        }

                        let left_stack = ctx.return_func()?.collect::<ArrayVec<_, 8>>();

                        if !left_stack.is_empty() {
                            log::warn!("반환되는 함수에 값이 남아있습니다. 프로그램이 잘못되었습니다: {left_stack:?}");
                        }

                        ctx.push(ret);
                    }
                    BuiltinCommand::Return => {
                        let left_stack = ctx.return_func()?.collect::<ArrayVec<_, 8>>();

                        if !left_stack.is_empty() {
                            log::warn!("반환되는 함수에 값이 남아있습니다. 프로그램이 잘못되었습니다: {left_stack:?}");
                        }

                        let mut result_idx = 0usize;
                        let mut results_idx = 0usize;

                        let args: ArrayVec<_, 8> = args
                            .map(|v| match v {
                                LocalValue::Value(v) => Ok(v),
                                LocalValue::VarRef(v) => ctx.read_var_ref(&v),
                            })
                            .try_collect()?;

                        let ((_, result), (_, results)) =
                            ctx.var.get_var2("RESULT".into(), "RESULTS".into()).unwrap();
                        let result = result.assume_normal();
                        let results = results.assume_normal();

                        for arg in args {
                            match arg {
                                value @ Value::Int(_) => {
                                    result.set(iter::once(result_idx), value)?;
                                    result_idx += 1;
                                }
                                value @ Value::String(_) => {
                                    results.set(iter::once(results_idx), value)?;
                                    results_idx += 1;
                                }
                            }
                        }

                        return Ok(Some(Workflow::Return));
                    }
                    BuiltinCommand::StrLenS => {
                        let s = pop!(@String);

                        ctx.var
                            .get_var("RESULT".into())
                            .unwrap()
                            .1
                            .assume_normal()
                            .set(
                                iter::empty(),
                                Value::Int(
                                    encoding_rs::SHIFT_JIS.encode(&s).0.as_ref().len() as i64
                                ),
                            )?;
                    }
                    BuiltinCommand::StrLenSU => {
                        let s = pop!(@String);

                        ctx.var
                            .get_var("RESULT".into())
                            .unwrap()
                            .1
                            .assume_normal()
                            .set(iter::empty(), Value::Int(s.len() as i64))?;
                    }
                    BuiltinCommand::DrawLine => {
                        chan.send_msg(ConsoleMessage::DrawLine);
                        chan.request_redraw();
                    }
                    BuiltinCommand::FontStyle => {
                        log::warn!("TODO: fontstyle({})", pop!(@i64));
                    }
                    BuiltinCommand::SetColor => {
                        let c = pop!(@i64);

                        let (r, g, b) = match pop!(@opt @i64) {
                            Some(g) => {
                                let b = pop!(@i64);
                                (c as u8, g as u8, b as u8)
                            }
                            None => {
                                let [r, g, b, _] = (c as u32).to_le_bytes();
                                (r, g, b)
                            }
                        };

                        ctx.color = u32::from_le_bytes([r, g, b, 0x00]);

                        chan.send_msg(ConsoleMessage::SetColor(r, g, b));
                    }
                    BuiltinCommand::ResetColor => {
                        ctx.color = u32::from_le_bytes([0xFF, 0xFF, 0xFF, 0x00]);
                        chan.send_msg(ConsoleMessage::SetColor(0xFF, 0xFF, 0xFF));
                    }
                    BuiltinCommand::ResetBgColor => {
                        ctx.color = u32::from_le_bytes([0x00, 0x00, 0x00, 0x00]);
                        log::warn!("TODO: RESETBGCOLOR");
                        // chan.send_msg(ConsoleMessage::SetColor(0xFF, 0xFF, 0xFF));
                    }
                    BuiltinCommand::GetColor => {
                        ctx.push(ctx.color as i64);
                    }
                    BuiltinCommand::GetBgColor => {
                        ctx.push(ctx.bg_color as i64);
                    }
                    BuiltinCommand::GetFocusColor => {
                        ctx.push(ctx.hl_color as i64);
                    }
                    BuiltinCommand::Input | BuiltinCommand::InputS => {
                        let req = match com {
                            BuiltinCommand::InputS => InputRequest::Str,
                            BuiltinCommand::Input => InputRequest::Int,
                            _ => unreachable!(),
                        };
                        match input(chan, req) {
                            ConsoleResult::Quit => {
                                log::info!("User Quit");
                                return Ok(Some(Workflow::Exit));
                            }
                            ConsoleResult::Value(ret) => {
                                ctx.var
                                    .get_var(if req == InputRequest::Int {
                                        "RESULT"
                                    } else {
                                        "RESULTS"
                                    })
                                    .unwrap()
                                    .1
                                    .assume_normal()
                                    .set(iter::empty(), ret)?;
                            }
                        }
                    }
                    BuiltinCommand::Quit => {
                        log::info!("Run QUIT");
                        chan.exit();
                        return Ok(Some(Workflow::Exit));
                    }
                    BuiltinCommand::AddChara => {
                        let no = pop!(@i64).try_into()?;
                        let template = ctx
                            .header_info
                            .character_templates
                            .get(&no)
                            .ok_or_else(|| anyhow!("존재하지 않는 캐릭터 번호입니다({no})"))?;

                        let idx = ctx.var.character_len;

                        ctx.var.add_chara();
                        ctx.var.set_character_template(idx, template)?;
                    }
                    BuiltinCommand::AddDefChara => {
                        let idx = ctx.var.character_len;

                        ctx.var.add_chara();

                        match ctx.header_info.character_templates.get(&0) {
                            Some(template) => {
                                ctx.var.set_character_template(idx, template)?;
                            }
                            None => {}
                        }
                    }
                    BuiltinCommand::ResetData => {
                        ctx.var.reset_data();
                    }
                    BuiltinCommand::LoadGlobal
                    | BuiltinCommand::SaveGlobal
                    | BuiltinCommand::SaveData
                    | BuiltinCommand::LoadData => {
                        log::warn!("TODO: Save/Load");
                    }
                    _ => {
                        bail!("TODO: {}({:?})", com, args.collect_vec());
                    }
                }
            }
            _ => bail!("TODO: {:?}", inst),
        }

        Ok(None)
    }

    fn run_body(
        &self,
        func_name: &str,
        goto_labels: &HashMap<SmolStr, u32>,
        body: &FunctionBody,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let mut cursor = 0;

        let insts = body.body();

        while let Some(inst) = insts.get(cursor) {
            cursor += 1;
            match self.run_instruction(func_name, goto_labels, inst, &mut cursor, chan, ctx) {
                Ok(None) => {}
                Ok(Some(Workflow::Return)) => break,
                Ok(Some(flow)) => return Ok(flow),
                Err(err) => {
                    return Err(err);
                }
            }
        }

        Ok(Workflow::Return)
    }

    fn call(
        &self,
        label: &str,
        args: &[Value],
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let body = self.dic.get_func(label)?;

        ctx.new_func(Ok(label.into()), body.file_path().clone());

        let mut args = args.iter().cloned();

        for (var_idx, default_value, arg_indices) in body.args().iter() {
            let var = ctx.var.get_maybe_local_var(label, var_idx)?;

            var.1.assume_normal().set(
                arg_indices.iter().copied(),
                args.next()
                    .or_else(|| default_value.clone())
                    .unwrap_or_else(|| {
                        if var.0.is_str {
                            Value::String("".into())
                        } else {
                            Value::Int(0)
                        }
                    }),
            )?;
        }

        let ret = self.run_body(label, body.goto_labels(), body, chan, ctx)?;

        ctx.end_func();

        Ok(ret)
    }

    fn call_event(&self, ty: EventType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        self.dic.get_event(ty).run(|body| {
            let label: &str = ty.into();
            ctx.new_func(Err(ty), body.file_path().clone());
            self.run_body(label, body.goto_labels(), body, chan, ctx)?;
            ctx.end_func();

            Ok(())
        })
    }

    fn begin(&self, ty: BeginType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        log::trace!("Begin {ty}");

        match ty {
            BeginType::Title => {
                self.call("SYSTEM_TITLE".into(), &[], chan, ctx)?;
                Ok(())
            }
            BeginType::First => {
                self.call_event(EventType::First, chan, ctx)?;
                Ok(())
            }
            BeginType::Shop => {
                self.call_event(EventType::Shop, chan, ctx)?;

                while ctx.begin.is_none() {
                    self.call("SHOW_SHOP", &[], chan, ctx)?;

                    match input(chan, InputRequest::Int) {
                        ConsoleResult::Quit => break,
                        ConsoleResult::Value(Value::Int(i)) => {
                            ctx.var
                                .get_var("RESULT")?
                                .1
                                .assume_normal()
                                .set(iter::empty(), Value::Int(i))?;

                            if i >= 0 && i < 100 {
                                let sales = *ctx
                                    .var
                                    .get_var("ITEMSALES")?
                                    .1
                                    .assume_normal()
                                    .get_int(iter::once(i as usize))?;

                                #[allow(unreachable_code)]
                                #[allow(unused_variables)]
                                if sales != 0 {
                                    let price = todo!("ITEMPRICE");
                                    let money = ctx
                                        .var
                                        .get_var("MONEY")?
                                        .1
                                        .assume_normal()
                                        .get_int(iter::empty())?;

                                    if *money >= price {
                                        *money -= price;
                                        drop(money);
                                        *ctx.var
                                            .get_var("ITEM")?
                                            .1
                                            .assume_normal()
                                            .get_int(iter::once(i as usize))? += 1;
                                    }
                                }
                            } else {
                                self.call("USERSHOP", &[], chan, ctx)?;
                            }
                        }
                        ConsoleResult::Value(Value::String(_)) => {
                            log::error!("콘솔에서 잘못된 타입의 응답을 보냈습니다.");
                            break;
                        }
                    }
                }

                Ok(())
            }
            _ => bail!("TODO: {}", ty),
        }
    }

    fn report_stack(stack: &Callstack, chan: &ConsoleChannel, position: Option<ScriptPosition>) {
        fn read_source(path: &str, pos: &ScriptPosition) -> io::Result<Option<String>> {
            use io::{BufRead, BufReader};
            BufReader::new(std::fs::File::open(path)?)
                .lines()
                .nth(pos.line as usize)
                .transpose()
        }

        let position = position.unwrap_or(stack.script_position.clone());

        match &stack.func_name {
            Ok(name) => match read_source(stack.file_path.as_str(), &position) {
                Ok(Some(s)) => {
                    let source = s.replace("\n", "\\n");
                    report_error!(
                        chan,
                        "    at function {name}{position} `{}` [{source}]",
                        stack.file_path,
                    );
                }
                _ => {
                    report_error!(
                        chan,
                        "    at function {name}@{position} `{}`",
                        stack.file_path,
                    );
                }
            },
            Err(ty) => match read_source(stack.file_path.as_str(), &position) {
                Ok(Some(s)) => {
                    let source = s.replace("\n", "\\n");
                    report_error!(
                        chan,
                        "    at {ty}{position} `{}` [{source}]",
                        stack.file_path,
                    );
                }
                _ => {
                    report_error!(chan, "    at {ty}@{position} `{}`", stack.file_path,);
                }
            },
        }
    }

    pub fn start(&self, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        while let Some(begin) = ctx.begin.take() {
            match self.begin(begin, chan, ctx) {
                Ok(()) => {}
                Err(err) => {
                    report_error!(chan, "VM failed with: {err}");

                    ctx.call_stack.last_mut().unwrap().script_position = ctx.current_pos.clone();

                    for stack in ctx.call_stack.iter().rev() {
                        Self::report_stack(stack, chan, None);
                    }

                    return Err(err);
                }
            }
        }

        Ok(())
    }
}

fn input(chan: &ConsoleChannel, req: InputRequest) -> ConsoleResult {
    chan.send_msg(ConsoleMessage::Input(req));
    let ret = chan.recv_ret();
    log::trace!("Console Recv {ret:?}");
    ret
}
