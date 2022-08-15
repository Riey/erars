use std::iter;
use std::sync::Arc;

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use smol_str::SmolStr;
use strum::Display;

use hashbrown::{HashMap, HashSet};

use erars_ast::{
    BeginType, BinaryOperator, BuiltinCommand, EventType, PrintFlags, UnaryOperator, Value,
    VariableInfo,
};
use erars_compiler::{HeaderInfo, Instruction, ParserContext};

use crate::function::{FunctionBody, FunctionDic};
use crate::ui::{ConsoleChannel, ConsoleMessage, ConsoleResult, InputRequest};

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
        macro_rules! a {
            () => {
                args.next().unwrap_or(0)
            };
        }
        match self {
            VmVariable::Int0D(s) => Ok(s),
            VmVariable::Int1D(s) => s.get_mut(a!()).ok_or_else(|| anyhow!("Index out of range")),
            VmVariable::Int2D(s) => s
                .get_mut(a!())
                .unwrap()
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index out of range")),
            VmVariable::Int3D(s) => s
                .get_mut(a!())
                .unwrap()
                .get_mut(a!())
                .unwrap()
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index out of range")),
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

    pub fn add_chara(&mut self, info: &VariableInfo) {
        match self {
            UniformVariable::Character(c) => c.push(VmVariable::new(info)),
            _ => {}
        }
    }
}

struct VariableStorage {
    character_len: usize,
    variables: HashMap<SmolStr, (VariableInfo, UniformVariable)>,
    local_variables: HashMap<SmolStr, HashMap<SmolStr, (VariableInfo, UniformVariable)>>,
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

    fn init_local(&mut self, name: &str, body: &FunctionBody) {
        self.local_variables.entry(name.into()).or_insert_with(|| {
            body.local_vars()
                .iter()
                .map(|(idx, info)| {
                    let mut var = UniformVariable::new(info);

                    if !info.init.is_empty() {
                        let var = var.assume_normal();
                        for (idx, init_var) in info.init.iter().enumerate() {
                            var.set(iter::once(idx), init_var.clone()).unwrap();
                        }
                    }

                    (idx.clone(), (info.clone(), var))
                })
                .collect()
        });
    }

    fn get_local_var(
        &mut self,
        name: &str,
        var: &str,
    ) -> Result<&mut (VariableInfo, UniformVariable)> {
        self.local_variables
            .get_mut(name)
            .unwrap()
            .get_mut(var)
            .ok_or_else(|| anyhow!("Variable {} is not exists", var))
    }

    fn get_var(&mut self, var: &str) -> Result<&mut (VariableInfo, UniformVariable)> {
        self.variables
            .get_mut(var)
            .ok_or_else(|| anyhow!("Variable {} is not exists", var))
    }

    fn get_var2(
        &mut self,
        l: &str,
        r: &str,
    ) -> Result<(
        &mut (VariableInfo, UniformVariable),
        &mut (VariableInfo, UniformVariable),
    )> {
        assert_ne!(l, r);

        match self.variables.get_many_mut([l, r]) {
            Some([l, r]) => Ok((l, r)),
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
}

#[derive(Display, Debug, Clone, Copy)]
enum Workflow {
    Return,
    Exit,
}

struct Callstack {
    stack_base: usize,
    local_idxs: HashSet<SmolStr>,
}

#[derive(Clone, Debug)]
struct VariableRef {
    name: SmolStr,
    func_extern: Option<SmolStr>,
    idxs: ArrayVec<usize, 4>,
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

impl TryFrom<LocalValue> for Value {
    type Error = anyhow::Error;

    fn try_from(value: LocalValue) -> Result<Self, Self::Error> {
        match value {
            LocalValue::Value(v) => Ok(v),
            _ => bail!("LocalValue type is not Value"),
        }
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

pub struct VmContext {
    var: VariableStorage,
    begin: Option<BeginType>,
    stack: Vec<LocalValue>,
    call_stack: Vec<Callstack>,
    color: u32,
    hl_color: u32,
    bg_color: u32,
}

impl VmContext {
    pub fn new(infos: &HashMap<SmolStr, VariableInfo>) -> Self {
        Self {
            var: VariableStorage::new(infos),
            begin: Some(BeginType::Title),
            stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(512),
            color: u32::from_le_bytes([0xFF, 0xFF, 0xFF, 0x00]),
            hl_color: u32::from_le_bytes([0xFF, 0xFF, 0x00, 0x00]),
            bg_color: u32::from_le_bytes([0x00, 0x00, 0x00, 0x00]),
        }
    }

    fn resolve_var_ref<'c>(
        &'c mut self,
        dic: &FunctionDic,
        func_name: &str,
        r: &VariableRef,
    ) -> Result<(
        &'c mut VariableInfo,
        &'c mut UniformVariable,
        arrayvec::IntoIter<usize, 4>,
    )> {
        let (info, var) = match r.func_extern.as_deref() {
            Some(ex_func) => {
                self.var.init_local(ex_func, dic.get_func(ex_func)?);
                self.var.get_local_var(ex_func, &r.name)
            }
            None => {
                if self.is_local_var(&r.name) {
                    self.var.get_local_var(func_name, &r.name)
                } else {
                    self.var.get_var(&r.name)
                }
            }
        }?;

        Ok((info, var, r.idxs.clone().into_iter()))
    }

    pub fn is_local_var(&self, name: &str) -> bool {
        self.call_stack
            .last()
            .expect("Call stack is empty")
            .local_idxs
            .contains(name)
    }

    pub fn new_func(&mut self, label: &str, body: &FunctionBody) {
        self.var.init_local(label, body);
        self.call_stack.push(Callstack {
            stack_base: self.stack.len(),
            local_idxs: body.local_vars().keys().cloned().collect(),
        });
    }

    pub fn end_func(&mut self) {
        self.call_stack.pop();
    }

    pub fn return_func(&mut self) -> impl Iterator<Item = Value> + '_ {
        let call_stack = self.call_stack.last().unwrap();
        self.stack
            .drain(call_stack.stack_base..)
            .map(|v| v.try_into().unwrap())
    }

    fn take_arg_list(&mut self, count: u32) -> Result<ArrayVec<usize, 4>> {
        self.take_value_list(count).map(usize::try_from).collect()
    }

    fn take_value_list(&mut self, count: u32) -> impl Iterator<Item = Value> + '_ {
        self.take_list(count).map(|l| l.try_into().unwrap())
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

    fn push_var_ref(
        &mut self,
        name: SmolStr,
        func_extern: Option<SmolStr>,
        idxs: ArrayVec<usize, 4>,
    ) {
        self.stack.push(LocalValue::VarRef(VariableRef {
            name,
            func_extern,
            idxs,
        }));
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

    fn pop_value(&mut self) -> Value {
        self.pop().try_into().unwrap()
    }

    fn pop_var_ref(&mut self) -> Result<VariableRef> {
        self.pop().try_into()
    }

    fn pop_str(&mut self) -> Result<String> {
        self.pop_value().try_into()
    }

    #[allow(unused)]
    fn pop_int(&mut self) -> Result<i64> {
        self.pop_value().try_into()
    }
}

pub struct TerminalVm {
    dic: FunctionDic,
    header_info: Arc<HeaderInfo>,
}

impl TerminalVm {
    pub fn new(function_dic: FunctionDic, header_info: Arc<HeaderInfo>) -> Self {
        Self {
            dic: function_dic,
            header_info,
        }
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
        // eprintln!("stack: {:?}, inst: {:?}", ctx.stack, inst);

        match inst {
            Instruction::LoadInt(n) => ctx.push(*n),
            Instruction::LoadStr(s) => ctx.push(s),
            Instruction::Nop => {}
            Instruction::Pop => drop(ctx.pop()),
            Instruction::Duplicate => ctx.dup(),
            Instruction::DuplicatePrev => ctx.dup_prev(),
            Instruction::EvalFormString => {
                let form = ctx.pop_str()?;
                let parser_ctx = ParserContext::new(self.header_info.clone());
                let expr = erars_compiler::normal_form_str(&parser_ctx)(&form)
                    .unwrap()
                    .1;
                let insts = erars_compiler::compile_expr(expr).unwrap();

                for inst in insts {
                    self.run_instruction(func_name, goto_labels, &inst, &mut 0, chan, ctx)?;
                }
            }
            Instruction::GotoLabel => {
                *cursor = goto_labels[ctx.pop_str()?.as_str()] as usize;
            }
            Instruction::LoadVarRef(var_idx, func_extern, c) => {
                let args = ctx.take_arg_list(*c)?;
                ctx.push_var_ref(var_idx.clone(), func_extern.clone(), args);
            }
            Instruction::StoreVar => {
                let target = *ctx
                    .var
                    .get_var("TARGET".into())
                    .unwrap()
                    .1
                    .assume_normal()
                    .get_int(iter::empty())?;
                let var_ref = ctx.pop_var_ref()?;
                let value = ctx.pop_value();

                let (info, var, mut args) = ctx.resolve_var_ref(&self.dic, func_name, &var_ref)?;
                match var {
                    UniformVariable::Character(c) => {
                        let no = if args.len() < info.arg_len() {
                            target as usize
                        } else {
                            args.next().unwrap()
                        };

                        c[no].set(args, value)?;
                    }
                    UniformVariable::Normal(v) => {
                        v.set(args, value)?;
                    }
                }
            }
            Instruction::LoadVar => {
                let var_ref = ctx.pop_var_ref()?;
                match var_ref.name.as_str() {
                    "GAMEBASE_VERSION" => ctx.push(0),
                    "GAMEBASE_AUTHOR" => ctx.push("Riey"),
                    "GAMEBASE_YEAR" => ctx.push(2022),
                    "GAMEBASE_TITLE" => ctx.push("eraTHYMKR"),
                    "GAMEBASE_INFO" => ctx.push(""),
                    "NO" => ctx.push(0),
                    _ => {
                        let target = *ctx
                            .var
                            .get_var("TARGET".into())
                            .unwrap()
                            .1
                            .assume_normal()
                            .get_int(iter::empty())?;

                        let (info, var, mut args) =
                            ctx.resolve_var_ref(&self.dic, func_name, &var_ref)?;
                        let value = match var {
                            UniformVariable::Character(c) => {
                                let no = if args.len() < info.arg_len() {
                                    target as usize
                                } else {
                                    args.next().unwrap()
                                };
                                c[no].get(args)?
                            }
                            UniformVariable::Normal(v) => v.get(args)?,
                        };

                        ctx.push(value);
                    }
                }
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
            Instruction::ReturnF => return Ok(Some(Workflow::Return)),
            Instruction::Return => {
                let values = ctx.return_func().collect::<ArrayVec<_, 16>>();

                let mut result_idx = 0usize;
                let mut results_idx = 0usize;

                let ((_, result), (_, results)) =
                    ctx.var.get_var2("RESULT".into(), "RESULTS".into()).unwrap();
                let result = result.assume_normal();
                let results = results.assume_normal();

                for value in values {
                    match value {
                        Value::Int(_) => {
                            result.set(iter::once(result_idx), value)?;
                            result_idx += 1;
                        }
                        Value::String(_) => {
                            results.set(iter::once(results_idx), value)?;
                            results_idx += 1;
                        }
                    }
                }

                return Ok(Some(Workflow::Return));
            }
            Instruction::CallMethod(c) => {
                let func = ctx.pop_str()?;
                let ret: Value;

                {
                    let mut args = ctx.take_value_list(*c);
                    match func.as_str() {
                        "TOSTR" => {
                            let value = args.next().unwrap().try_into_int()?;
                            let format = args.next();

                            ret = if let Some(_format) = format {
                                format!("{00}", value)
                            } else {
                                value.to_string()
                            }
                            .into();
                        }
                        "MAX" => {
                            let mut max = args.next().unwrap().try_into_int()?;

                            for arg in args {
                                max = max.max(arg.try_into_int()?);
                            }

                            ret = max.into();
                        }
                        other => bail!("TODO: CallMethod {}", other),
                    }
                }

                ctx.push(ret);
            }
            Instruction::Call(c) => {
                let func = ctx.pop_str()?;
                let args = ctx.take_value_list(*c).collect::<ArrayVec<_, 8>>();

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
                let args = ctx.take_value_list(*c);
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
                    let operand = ctx.pop_value().as_bool();
                    ctx.push(!operand);
                }
                UnaryOperator::Minus => {
                    let operand = ctx.pop_int()?;
                    ctx.push(-operand);
                }
            },
            Instruction::BinaryOperator(op) => {
                let rhs = ctx.pop_value();
                let lhs = ctx.pop_value();

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
                    _ => todo!("Todo {:?}", op),
                };

                ctx.push(ret);
            }
            Instruction::Goto(no) => {
                *cursor = *no as usize;
            }
            Instruction::GotoIfNot(no) => {
                let cond = ctx.pop_value().as_bool();
                if !cond {
                    *cursor = *no as usize;
                }
            }
            Instruction::SetAlignment(align) => {
                chan.send_msg(ConsoleMessage::Alignment(*align));
            }
            Instruction::Command(com, c) => {
                let mut args = ctx.take_list(*c).collect::<ArrayVec<_, 4>>().into_iter();
                macro_rules! pop {
                    () => {
                        match args.next() {
                            Some(v) => v,
                            None => bail!("매개변수가 부족합니다"),
                        }
                    };
                    (@opt) => {
                        args.next()
                    };
                    (@var) => {
                        VariableRef::try_from(pop!())?
                    };
                    (@value) => {
                        Value::try_from(pop!())?
                    };
                    (@opt @value) => {
                        match pop!(@opt) {
                            Some(v) => Some(Value::try_from(v)?),
                            None => None,
                        }
                    };
                    (@$t:ty) => {
                        <$t>::try_from(Value::try_from(pop!())?)?
                    };
                    (@opt @$t:ty) => {
                        match pop!(@opt) {
                            Some(v) => Some(<$t>::try_from(Value::try_from(v)?)?),
                            None => None,
                        }
                    };
                }

                match com {
                    BuiltinCommand::Varset => {
                        let var = pop!(@var);
                        let (info, var, args) = ctx.resolve_var_ref(&self.dic, func_name, &var)?;
                        let value = pop!(@opt @value);
                        let start = pop!(@opt @usize);
                        let end = pop!(@opt @usize);

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
                    BuiltinCommand::DrawLine => {
                        chan.send_msg(ConsoleMessage::DrawLine);
                        chan.request_redraw();
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
                    BuiltinCommand::Input => {
                        chan.send_msg(ConsoleMessage::Input(InputRequest::Int));
                        chan.request_redraw();
                        let ret = chan.recv_ret();
                        log::trace!("Console Recv {ret:?}");
                        match ret {
                            ConsoleResult::Quit => {
                                log::info!("User Quit");
                                return Ok(Some(Workflow::Exit));
                            }
                            ConsoleResult::Value(ret) => {
                                ctx.var
                                    .get_var("RESULT".into())
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
                    BuiltinCommand::AddDefChara => {
                        ctx.var.add_chara();
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
                        bail!("TODO: {}({:?})", com, args)
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
            log::trace!("[{func_name}] run instruction {inst:?}");
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

        ctx.new_func(label, body);

        let mut args = args.iter().cloned();

        for (var_idx, default_value, arg_indices) in body.args().iter() {
            let var = if ctx.is_local_var(var_idx) {
                ctx.var.get_local_var(label, var_idx)?
            } else {
                ctx.var.get_var(var_idx)?
            };

            var.1.assume_normal().set(
                arg_indices.iter().copied(),
                args.next()
                    .unwrap_or_else(|| default_value.clone().unwrap()),
            )?;
        }

        let ret = self.run_body(label, body.goto_labels(), body, chan, ctx);

        ctx.end_func();

        ret
    }

    fn call_event(&self, ty: EventType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        self.dic.get_event(ty).run(|body| {
            let label = ty.into();
            ctx.new_func(label, body);
            self.run_body(label, body.goto_labels(), body, chan, ctx)?;
            ctx.end_func();

            Ok(())
        })
    }

    fn begin(&self, ty: BeginType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        log::trace!("Begin {ty}");

        match ty {
            BeginType::Title => {
                self.call("SYSTEM_TITLE", &[], chan, ctx)?;
                Ok(())
            }
            BeginType::First => {
                self.call_event(EventType::First, chan, ctx)?;
                Ok(())
            }
            _ => bail!("TODO: {}", ty),
        }
    }

    pub fn start(&self, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        while let Some(begin) = ctx.begin.take() {
            self.begin(begin, chan, ctx)?;
        }

        Ok(())
    }
}
