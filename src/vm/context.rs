use anyhow::{bail, Result};
use arrayvec::ArrayVec;
use smol_str::SmolStr;
use std::fmt;
use std::sync::Arc;

use erars_ast::{BeginType, EventType, ScriptPosition, Value, VariableInfo};
use erars_compiler::{EraConfig, HeaderInfo};

use crate::vm::{VariableStorage, VmVariable};

use super::UniformVariable;

#[derive(Clone)]
pub struct VmContext {
    pub var: VariableStorage,
    pub begin: Option<BeginType>,
    pub header_info: Arc<HeaderInfo>,
    pub config: Arc<EraConfig>,
    stack: Vec<LocalValue>,
    call_stack: Vec<Callstack>,
    current_pos: ScriptPosition,
}

impl VmContext {
    pub fn new(header_info: Arc<HeaderInfo>, config: Arc<EraConfig>) -> Self {
        let mut ret = Self {
            var: VariableStorage::new(&header_info.global_variables),
            header_info,
            begin: Some(BeginType::Title),
            stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(512),
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

        self.var.init_replace(&replace)?;

        Ok(())
    }

    pub fn set_begin(&mut self, ty: BeginType) {
        self.begin = Some(ty);
    }

    pub fn take_begin(&mut self) -> Option<BeginType> {
        self.begin.take()
    }

    pub fn var_mut(&mut self) -> &mut VariableStorage {
        &mut self.var
    }

    pub fn update_last_call_stack(&mut self) {
        if let Some(stack) = self.call_stack.last_mut() {
            stack.script_position = self.current_pos.clone();
        }
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
            LocalValue::VarRef(r) => self.read_var_ref(&r),
        }
    }

    pub fn read_var_ref(&mut self, var_ref: &VariableRef) -> Result<Value> {
        let (_, var, idx) = self.resolve_var_ref(&var_ref)?;

        var.get(idx)
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
        self.var.index_maybe_local_var(&r.func_name, &r.name, &r.idxs)
    }

    pub fn resolve_var_ref_raw<'c>(
        &'c mut self,
        r: &VariableRef,
    ) -> Result<(
        &'c mut VariableInfo,
        &'c mut UniformVariable,
        ArrayVec<usize, 4>,
    )> {
        let (info, var) = self.var.get_maybe_local_var(&r.func_name, &r.name)?;

        Ok((info, var, r.idxs.clone()))
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

    pub fn take_arg_list(&mut self, count: u32) -> Result<ArrayVec<usize, 4>> {
        self.take_value_list(count)?
            .into_iter()
            .map(usize::try_from)
            .collect()
    }

    pub fn take_value_list(&mut self, count: u32) -> Result<ArrayVec<Value, 16>> {
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

    pub fn push_var_ref(&mut self, name: String, func_name: String, idxs: ArrayVec<usize, 4>) {
        let var_ref = VariableRef {
            func_name,
            name,
            idxs,
        };
        self.stack.push(LocalValue::VarRef(var_ref));
    }

    pub fn push(&mut self, value: impl Into<Value>) {
        self.stack.push(LocalValue::Value(value.into()));
    }

    pub fn pop(&mut self) -> Result<LocalValue> {
        if let Some(last_stack) = self.call_stack.last() {
            if last_stack.stack_base >= self.stack.len() {
                bail!("다른 함수의 스택을 침범했습니다.");
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
        }
    }

    pub fn pop_var_ref(&mut self) -> Result<VariableRef> {
        self.pop()?.try_into()
    }

    pub fn pop_str(&mut self) -> Result<String> {
        self.pop_value().and_then(|v| v.try_into_str())
    }

    pub fn pop_int(&mut self) -> Result<i64> {
        self.pop_value().and_then(|v| v.try_into_int())
    }
}

#[derive(Debug, Clone)]
pub struct Callstack {
    pub func_name: Result<SmolStr, EventType>,
    pub file_path: SmolStr,
    pub script_position: ScriptPosition,
    pub stack_base: usize,
}

#[derive(Clone)]
pub struct VariableRef {
    pub name: String,
    pub func_name: String,
    pub idxs: ArrayVec<usize, 4>,
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
pub enum LocalValue {
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
