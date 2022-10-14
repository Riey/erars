use anyhow::{bail, Result};
use std::fmt;
use std::sync::Arc;

use erars_ast::{BeginType, EventType, ScriptPosition, StrKey, Value, VariableInfo};
use erars_compiler::{EraConfig, HeaderInfo};

use crate::variable::StrKeyLike;
use crate::{ArgVec, SystemState, VariableStorage, VmVariable};

use super::UniformVariable;

#[derive(Clone, Debug)]
struct StateCallStack {
    state: SystemState,
    phase: usize,
    call_stack_base: usize,
}

#[derive(Clone)]
pub struct VmContext {
    pub var: VariableStorage,
    pub header_info: Arc<HeaderInfo>,
    pub config: Arc<EraConfig>,

    state: Vec<StateCallStack>,
    /// For NOSKIP/ENDNOSKIP
    pub(crate) prev_skipdisp: Option<bool>,
    /// Set `true` during SAVEGAME
    pub(crate) put_form_enabled: bool,

    pub(crate) lastload_version: u32,
    pub(crate) lastload_no: u32,
    pub(crate) lastload_text: String,

    stack: Vec<LocalValue>,
    call_stack: Vec<Callstack>,
    current_pos: ScriptPosition,
}

impl VmContext {
    pub fn new(header_info: Arc<HeaderInfo>, config: Arc<EraConfig>) -> Self {
        let mut ret = Self {
            var: VariableStorage::new(&header_info.global_variables),
            header_info,
            state: vec![StateCallStack {
                state: (BeginType::Title.into()),
                phase: 0,
                call_stack_base: 0,
            }],
            stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(512),
            prev_skipdisp: None,
            put_form_enabled: false,
            lastload_no: 0,
            lastload_text: "".into(),
            lastload_version: 0,
            config,
            current_pos: ScriptPosition::default(),
        };

        ret.init_variable().unwrap();

        ret
    }

    pub fn encoding(&self) -> &'static encoding_rs::Encoding {
        use erars_compiler::Language;

        match self.config.lang {
            // 949
            Language::Korean => encoding_rs::EUC_KR,
            // 932
            Language::Japanese => encoding_rs::SHIFT_JIS,
            // 936
            Language::ChineseHans => encoding_rs::GBK,
            // 950
            Language::ChineseHant => encoding_rs::BIG5,
        }
    }

    fn init_variable(&mut self) -> Result<()> {
        let info = self.header_info.clone();
        let replace = &info.replace;

        self.var.init_replace(replace)?;

        Ok(())
    }

    pub fn var_mut(&mut self) -> &mut VariableStorage {
        &mut self.var
    }

    pub fn call_stack(&self) -> &[Callstack] {
        &self.call_stack
    }

    pub fn current_pos(&self) -> &ScriptPosition {
        &self.current_pos
    }

    pub fn stack(&self) -> &[LocalValue] {
        &self.stack
    }

    pub fn update_position(&mut self, pos: ScriptPosition) {
        self.current_pos = pos;
    }

    pub fn reduce_local_value(&mut self, value: LocalValue) -> Result<Value> {
        match value {
            LocalValue::Value(v) => Ok(v),
            LocalValue::InternedStr(s) => Ok(self.var.resolve_key(s).into()),
            LocalValue::VarRef(r) => self.read_var_ref(&r),
        }
    }

    pub fn read_var_ref(&mut self, var_ref: &VariableRef) -> Result<Value> {
        let (_, var, idx) = self.resolve_var_ref(var_ref)?;
        // log::info!("Read {} -> {:?}", var_ref.name, var.get(idx)?);

        var.get(idx)
    }

    pub fn ref_int_var_ref(&mut self, var_ref: &VariableRef) -> Result<&mut i64> {
        let (_, var, idx) = self.resolve_var_ref(var_ref)?;

        var.as_int()?
            .get_mut(idx)
            .ok_or_else(|| anyhow::anyhow!("Variable {:?} out of index", var_ref.name))
    }

    pub fn set_var_ref(&mut self, var_ref: &VariableRef, value: Value) -> Result<()> {
        let (_, var, idx) = self.resolve_var_ref(var_ref)?;
        var.set(idx, value)?;
        Ok(())
    }

    pub fn resolve_var_ref<'c>(
        &'c mut self,
        r: &VariableRef,
    ) -> Result<(&'c mut VariableInfo, &'c mut VmVariable, usize)> {
        self.var.index_maybe_local_var(r.func_name, r.name, &r.idxs)
    }

    pub fn resolve_var_ref_raw<'c>(
        &'c mut self,
        r: &VariableRef,
    ) -> Result<(&'c mut VariableInfo, &'c mut UniformVariable, ArgVec)> {
        let (info, var) = self.var.get_maybe_local_var(r.func_name, r.name)?;

        Ok((info, var, r.idxs.clone()))
    }

    pub fn last_call_stack_base(&self) -> usize {
        self.state.last().map_or(0, |s| s.call_stack_base)
    }

    pub fn push_state(&mut self, state: SystemState, call_stack_base: usize) {
        self.state.push(StateCallStack {
            state,
            phase: 0,
            call_stack_base,
        });
    }

    pub fn push_state_with_phase(
        &mut self,
        state: SystemState,
        phase: usize,
        call_stack_base: usize,
    ) {
        self.state.push(StateCallStack {
            state,
            phase,
            call_stack_base,
        });
    }

    pub fn pop_state(&mut self) -> Option<(SystemState, usize, usize)> {
        self.state.pop().map(|s| (s.state, s.phase, s.call_stack_base))
    }

    pub fn clear_state(&mut self) {
        self.state.clear();
    }

    pub fn push_current_call_stack(
        &mut self,
        func_name: FunctionIdentifier,
        file_path: StrKey,
        instruction_pos: usize,
    ) {
        self.call_stack.push(Callstack {
            func_name,
            file_path,
            instruction_pos,
            script_position: std::mem::take(&mut self.current_pos),
            stack_base: self.stack.len(),
        });
    }

    pub fn pop_call_stack(&mut self) -> Option<Callstack> {
        self.call_stack.pop()
    }

    pub fn pop_call_stack_check(&mut self, call_stack_base: usize) -> Option<Callstack> {
        if self.call_stack.len() <= call_stack_base {
            return None;
        }

        self.call_stack.pop()
    }

    pub fn clear_call_stack(&mut self) {
        self.call_stack.clear();
    }

    pub fn return_func(&mut self) -> Result<impl Iterator<Item = Value>> {
        let count = self.current_stack_count();
        Ok(self.take_value_list(count as u32)?.into_iter())
    }

    pub fn current_stack_count(&self) -> usize {
        self.stack.len() - self.call_stack.last().map_or(0, |s| s.stack_base)
    }

    pub fn take_arg_list(&mut self, var_name: Option<StrKey>, count: u32) -> Result<ArgVec> {
        self.take_value_list(count)?
            .into_iter()
            .map(|value| match value {
                Value::Int(i) => usize::try_from(i).map_err(anyhow::Error::from),
                Value::String(str) => match var_name
                    .map(|s| self.var.resolve_key(s))
                    .map(erars_ast::var_name_alias)
                    .map(|s| self.var.interner().get_or_intern(s))
                    .and_then(|var_name| {
                        self.header_info.var_names.get(&var_name.get_key(&self.var))
                    })
                    .and_then(|names| names.get(&str.get_key(&self.var)))
                {
                    Some(value) => Ok(*value as usize),
                    None => anyhow::bail!("Can't index variable with String"),
                },
            })
            .collect()
    }

    pub fn take_value_list(&mut self, count: u32) -> Result<Vec<Value>> {
        let mut ret = Vec::new();

        for arg in self.stack.drain(self.stack.len() - count as usize..) {
            match arg {
                LocalValue::Value(v) => ret.push(v),
                LocalValue::VarRef(var) => {
                    let var = self.var.index_maybe_local_var(var.func_name, var.name, &var.idxs)?;
                    ret.push(var.1.get(var.2)?);
                }
                LocalValue::InternedStr(s) => {
                    ret.push(self.var.resolve_key(s).into());
                }
            }
        }

        Ok(ret)
    }

    pub fn take_list(&mut self, count: u32) -> impl Iterator<Item = LocalValue> + '_ {
        self.stack.drain(self.stack.len() - count as usize..)
    }

    pub fn dup(&mut self) {
        let last = self.stack.last().unwrap().clone();
        self.stack.push(last);
    }

    pub fn dup_prev(&mut self) {
        let prev = self.stack[self.stack.len() - 2].clone();
        self.stack.push(prev);
    }

    pub fn push_var_ref(&mut self, name: StrKey, func_name: StrKey, idxs: ArgVec) {
        let var_ref = VariableRef {
            func_name,
            name,
            idxs,
        };
        self.stack.push(LocalValue::VarRef(var_ref));
    }

    pub fn push_strkey(&mut self, key: StrKey) {
        self.stack.push(LocalValue::InternedStr(key));
    }

    pub fn push(&mut self, value: impl Into<Value>) {
        self.stack.push(LocalValue::Value(value.into()));
    }

    pub fn pop(&mut self) -> Result<LocalValue> {
        if let Some(last_stack) = self.call_stack.last() {
            if last_stack.stack_base >= self.stack.len() {
                bail!("다른 함수의 스택을 침범했습니다. 이전 함수 콜스택: {last_stack:?} 현재 스택 길이: {}", self.stack.len());
            }
        }

        // if this failed, it must be compiler error
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => bail!("Stack is empty"),
        }
    }

    pub fn pop_value(&mut self) -> Result<Value> {
        match self.pop()? {
            LocalValue::Value(v) => Ok(v),
            LocalValue::VarRef(var_ref) => self.read_var_ref(&var_ref),
            LocalValue::InternedStr(s) => Ok(Value::String(self.var.resolve_key(s).into())),
        }
    }

    pub fn pop_var_ref(&mut self) -> Result<VariableRef> {
        self.pop()?.try_into()
    }

    pub fn pop_str(&mut self) -> Result<String> {
        self.pop_value().and_then(|v| v.try_into_str())
    }

    pub fn pop_strkey(&mut self) -> Result<StrKey> {
        let value = match self.pop()? {
            LocalValue::InternedStr(s) => return Ok(s),
            LocalValue::Value(v) => v,
            LocalValue::VarRef(var_ref) => self.read_var_ref(&var_ref)?,
        };

        match value {
            Value::Int(_) => bail!("Value is not Str"),
            Value::String(s) => Ok(self.var.interner().get_or_intern(s)),
        }
    }

    pub fn pop_int(&mut self) -> Result<i64> {
        self.pop_value().and_then(|v| v.try_into_int())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionIdentifier {
    Normal(StrKey),
    Event(EventType, usize),
}

#[derive(Debug, Clone)]
pub struct Callstack {
    pub func_name: FunctionIdentifier,
    pub file_path: StrKey,
    pub script_position: ScriptPosition,
    pub stack_base: usize,
    pub instruction_pos: usize,
}

#[derive(Clone, Copy)]
pub struct VariableRef {
    pub name: StrKey,
    pub func_name: StrKey,
    pub idxs: ArgVec,
}

impl fmt::Debug for VariableRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.name, self.func_name)?;

        for idx in self.idxs.iter() {
            write!(f, ":{}", idx)?;
        }

        Ok(())
    }
}

impl fmt::Display for VariableRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.name)?;

        for idx in self.idxs.iter() {
            write!(f, ":{}", idx)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum LocalValue {
    Value(Value),
    InternedStr(StrKey),
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
