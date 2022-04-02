use std::iter;

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use smartstring::{LazyCompact, SmartString};
use strum::Display;

use hashbrown::{HashMap, HashSet};
use serde::{Deserialize, Serialize};

use erars_compiler::{
    BeginType, BinaryOperator, BulitinVariable, EventType, Instruction, KnownVariables, PrintFlags,
    UnaryOperator, VariableIndex, VariableInterner,
};

use crate::function::{FunctionBody, FunctionDic};
use crate::ui::{ConsoleChannel, ConsoleMessage, ConsoleResult, InputRequest};
use crate::value::Value;

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct VariableInfo {
    is_chara: bool,
    is_str: bool,
    is_local: bool,
    default_int: i64,
    size: Vec<usize>,
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }
}

#[derive(Clone, Debug)]
enum Variable {
    Int0D(i64),
    Int1D(Vec<i64>),
    Int2D(Vec<Vec<i64>>),
    Int3D(Vec<Vec<Vec<i64>>>),
    Str0D(String),
    Str1D(Vec<String>),
    Str2D(Vec<Vec<String>>),
    Str3D(Vec<Vec<Vec<String>>>),
}

impl Variable {
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
            Variable::Int0D(..)
            | Variable::Int1D(..)
            | Variable::Int2D(..)
            | Variable::Int3D(..) => *self.get_int(args)? = value.try_into()?,
            Variable::Str0D(..)
            | Variable::Str1D(..)
            | Variable::Str2D(..)
            | Variable::Str3D(..) => *self.get_str(args)? = value.try_into()?,
        }

        Ok(())
    }

    pub fn get(&mut self, args: impl Iterator<Item = usize>) -> Result<Value> {
        match self {
            Variable::Int0D(..)
            | Variable::Int1D(..)
            | Variable::Int2D(..)
            | Variable::Int3D(..) => self.get_int(args).map(Value::from),
            Variable::Str0D(..)
            | Variable::Str1D(..)
            | Variable::Str2D(..)
            | Variable::Str3D(..) => self.get_str(args).map(Value::from),
        }
    }

    pub fn get_int(&mut self, mut args: impl Iterator<Item = usize>) -> Result<&mut i64> {
        macro_rules! a {
            () => {
                args.next().unwrap_or(0)
            };
        }
        match self {
            Variable::Int0D(s) => Ok(s),
            Variable::Int1D(s) => s.get_mut(a!()).ok_or_else(|| anyhow!("Index out of range")),
            Variable::Int2D(s) => s
                .get_mut(a!())
                .unwrap()
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index out of range")),
            Variable::Int3D(s) => s
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
            Variable::Str0D(s) => Ok(s),
            Variable::Str1D(s) => s.get_mut(a!()).ok_or_else(|| anyhow!("Index out of range")),
            Variable::Str2D(s) => s
                .get_mut(a!())
                .unwrap()
                .get_mut(a!())
                .ok_or_else(|| anyhow!("Index out of range")),
            Variable::Str3D(s) => s
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
    Normal(Variable),
    Character(Vec<Variable>),
    Local(HashMap<String, Variable>),
    LocalCharacter(HashMap<String, Vec<Variable>>),
}

impl UniformVariable {
    pub fn new(info: &VariableInfo) -> Self {
        match (info.is_local, info.is_chara) {
            (false, false) => UniformVariable::Normal(Variable::new(info)),
            (false, true) => UniformVariable::Character(Vec::new()),
            (true, false) => UniformVariable::Local(HashMap::new()),
            (true, true) => UniformVariable::LocalCharacter(HashMap::new()),
        }
    }

    pub fn assume_normal(&mut self) -> &mut Variable {
        match self {
            Self::Normal(v) => v,
            _ => panic!("Variable is not normal variable"),
        }
    }

    pub fn resolve(&mut self, name: &str, target: usize) -> &mut Variable {
        match self {
            UniformVariable::Normal(v) => v,
            UniformVariable::Character(c) => c.get_mut(target).unwrap(),
            UniformVariable::Local(l) => l.get_mut(name).unwrap(),
            UniformVariable::LocalCharacter(lc) => {
                lc.get_mut(name).unwrap().get_mut(target).unwrap()
            }
        }
    }

    pub fn add_chara(&mut self, name: &str, info: &VariableInfo) {
        match self {
            UniformVariable::Character(c) => c.push(Variable::new(info)),
            UniformVariable::LocalCharacter(c) => {
                c.get_mut(name).unwrap().push(Variable::new(info))
            }
            _ => {}
        }
    }
}

struct VariableStorage {
    character_len: usize,
    variable_interner: VariableInterner,
    variables: Vec<(VariableInfo, UniformVariable)>,
    inited_functions: HashSet<SmartString<LazyCompact>>,
}

impl VariableStorage {
    pub fn new(infos: &HashMap<String, VariableInfo>, variable_interner: VariableInterner) -> Self {
        let variables = variable_interner
            .idxs_without_builtin()
            .map(|idx| {
                let name = variable_interner.resolve(idx).unwrap();
                let info = infos
                    .get(name.as_str())
                    .cloned()
                    .unwrap_or_else(|| VariableInfo {
                        default_int: 0,
                        is_chara: false,
                        is_local: false,
                        is_str: false,
                        size: vec![1000],
                    });

                let var = UniformVariable::new(&info);

                (info, var)
            })
            .collect();

        Self {
            character_len: 0,
            variable_interner,
            variables,
            inited_functions: HashSet::new(),
        }
    }

    fn init_local(&mut self, name: &str, body: &FunctionBody) {
        self.inited_functions.get_or_insert_with(name, |name| {
            self.variable_interner.intern(format!("ARG@{}", name));
            let arg_info = VariableInfo {
                default_int: 0,
                size: vec![1000],
                is_local: true,
                is_str: false,
                is_chara: false,
            };
            let arg = UniformVariable::new(&arg_info);
            self.variables.push((arg_info, arg));

            self.variable_interner.intern(format!("ARGS@{}", name));
            let arg_info = VariableInfo {
                default_int: 0,
                size: vec![100],
                is_local: true,
                is_str: true,
                is_chara: false,
            };
            let arg = UniformVariable::new(&arg_info);
            self.variables.push((arg_info, arg));

            self.variable_interner.intern(format!("LOCAL@{}", name));
            let arg_info = VariableInfo {
                default_int: 0,
                size: vec![body.local_size()],
                is_local: true,
                is_str: false,
                is_chara: false,
            };
            let arg = UniformVariable::new(&arg_info);
            self.variables.push((arg_info, arg));

            self.variable_interner.intern(format!("LOCALS@{}", name));
            let arg_info = VariableInfo {
                default_int: 0,
                size: vec![body.locals_size()],
                is_local: true,
                is_str: true,
                is_chara: false,
            };
            let arg = UniformVariable::new(&arg_info);
            self.variables.push((arg_info, arg));

            name.into()
        });
    }

    fn get_var(&mut self, idx: VariableIndex) -> Result<&mut (VariableInfo, UniformVariable)> {
        self.variables
            .get_mut(idx.as_usize())
            .ok_or_else(|| anyhow!("Variable {} is not exists", idx))
    }

    fn get_var2(
        &mut self,
        l: VariableIndex,
        r: VariableIndex,
    ) -> Result<(
        &mut (VariableInfo, UniformVariable),
        &mut (VariableInfo, UniformVariable),
    )> {
        assert_ne!(l, r);

        if l.as_usize() >= self.variables.len() {
            bail!("Variable {} is not exists", l);
        }

        if r.as_usize() >= self.variables.len() {
            bail!("Variable {} is not exists", r);
        }

        unsafe {
            let ptr = self.variables.as_mut_ptr();

            let l = ptr.add(l.as_usize());
            let r = ptr.add(r.as_usize());

            Ok((l.as_mut().unwrap_unchecked(), r.as_mut().unwrap_unchecked()))
        }
    }

    pub fn reset_data(&mut self) {
        self.character_len = 0;
        for var in self.variables.iter_mut() {
            var.1 = UniformVariable::new(&var.0);
        }
    }

    pub fn add_chara(&mut self, name: &str) {
        self.character_len += 1;
        self.variables.iter_mut().for_each(|(info, var)| {
            var.add_chara(name, info);
        });
    }

    pub fn get_known2(
        &mut self,
        l: KnownVariables,
        r: KnownVariables,
    ) -> (&mut UniformVariable, &mut UniformVariable) {
        let (l, r) = self
            .get_var2(
                self.variable_interner.get_known(l),
                self.variable_interner.get_known(r),
            )
            .unwrap();

        (&mut l.1, &mut r.1)
    }

    pub fn get_known(&mut self, known: KnownVariables) -> &mut UniformVariable {
        &mut self
            .get_var(self.variable_interner.get_known(known))
            .unwrap()
            .1
    }
}

#[derive(Display, Debug, Clone, Copy)]
enum Workflow {
    Return,
    Exit,
}

pub struct VmContext {
    var: VariableStorage,
    begin: Option<BeginType>,
    stack: Vec<Value>,
    call_stack: Vec<usize>,
}

impl VmContext {
    pub fn new(infos: &HashMap<String, VariableInfo>, variable_interner: VariableInterner) -> Self {
        Self {
            var: VariableStorage::new(infos, variable_interner),
            begin: Some(BeginType::Title),
            stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(512),
        }
    }

    pub fn new_func(&mut self) {
        self.call_stack.push(self.stack.len());
    }

    pub fn end_func(&mut self) {
        self.call_stack.pop();
    }

    pub fn return_func(&mut self) -> impl Iterator<Item = Value> + '_ {
        let base = self.call_stack.last().unwrap();
        self.stack.drain(*base..)
    }

    fn take_arg_list(&mut self, count: u32) -> Result<ArrayVec<usize, 4>> {
        self.take_list(count).map(usize::try_from).collect()
    }

    fn take_list(&mut self, count: u32) -> impl Iterator<Item = Value> + '_ {
        self.stack.drain(self.stack.len() - count as usize..)
    }

    fn push(&mut self, value: impl Into<Value>) {
        self.stack.push(value.into());
    }

    fn pop(&mut self) -> Value {
        // if this failed, it must be compiler error
        self.stack
            .pop()
            .unwrap_or_else(|| unreachable!("Unknown compiler error"))
    }

    fn pop_str(&mut self) -> Result<String> {
        self.pop().try_into()
    }

    #[allow(unused)]
    fn pop_int(&mut self) -> Result<i64> {
        self.pop().try_into()
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
        inst: &Instruction,
        cursor: &mut usize,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        match inst {
            Instruction::LoadInt(n) => ctx.push(*n),
            Instruction::LoadStr(s) => ctx.push(s),
            Instruction::Nop => {}
            Instruction::StoreVar(var_idx, c) => {
                if var_idx.is_builtin() {
                    bail!("Can't edit builtin variable");
                }

                let target = *ctx
                    .var
                    .get_known(KnownVariables::Target)
                    .assume_normal()
                    .get_int(iter::empty())?;

                let mut args = ctx.take_arg_list(*c)?.into_iter();
                let value = ctx.pop();

                let (info, var) = ctx.var.get_var(*var_idx)?;

                let var = if info.is_chara {
                    let no = if args.len() < info.arg_len() {
                        target as usize
                    } else {
                        args.next().unwrap()
                    };

                    var.resolve(func_name, no)
                } else {
                    var.resolve(func_name, 0)
                };

                var.set(args, value)?;
            }
            Instruction::LoadVar(var_idx, c) => {
                let target = *ctx
                    .var
                    .get_known(KnownVariables::Target)
                    .assume_normal()
                    .get_int(iter::empty())?;

                let value = if let Some(builtin) = var_idx.to_builtin() {
                    match builtin {
                        BulitinVariable::GamebaseAuthor => "Empty".into(),
                        BulitinVariable::GamebaseYear => 2022i64.into(),
                        BulitinVariable::GamebaseTitle => "Title".into(),
                        BulitinVariable::GamebaseVersion => 1000.into(),
                        BulitinVariable::GamebaseInfo => "Info".into(),
                    }
                } else {
                    let mut args = ctx.take_arg_list(*c)?.into_iter();

                    let (info, var) = ctx.var.get_var(*var_idx)?;

                    let var = if info.is_chara {
                        let no = if args.len() < info.arg_len() {
                            target as usize
                        } else {
                            args.next().unwrap()
                        };

                        var.resolve(func_name, no)
                    } else {
                        var.resolve(func_name, 0)
                    };

                    var.get(args)?
                };

                ctx.push(value);
            }
            Instruction::ReuseLastLine => {
                chan.send_msg(ConsoleMessage::ReuseLastLine(ctx.pop_str()?));
            }
            Instruction::Print(flags) => {
                chan.send_msg(ConsoleMessage::Print(ctx.pop_str()?));
                if flags.contains(PrintFlags::NEWLINE) {
                    chan.send_msg(ConsoleMessage::NewLine);
                }

                // TODO: PRINTW
            }
            Instruction::ReturnF => return Ok(Some(Workflow::Return)),
            Instruction::Return => {
                let values = ctx.return_func().collect::<ArrayVec<_, 16>>();

                let mut result_idx = 0usize;
                let mut results_idx = 0usize;

                let (result, results) = ctx
                    .var
                    .get_known2(KnownVariables::Result, KnownVariables::ResultS);
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
                let mut args = ctx.take_list(*c);

                match func.as_str() {
                    "TOSTR" => {
                        let value = args.next().unwrap().try_into_int()?;
                        let format = args.next();

                        let ret = if let Some(_format) = format {
                            format!("{00}", value)
                        } else {
                            value.to_string()
                        };

                        drop(args);

                        ctx.push(ret);
                    }
                    other => bail!("TODO: CallMethod {}", other),
                }
            }
            Instruction::Call(c) => {
                let func = ctx.pop_str()?;
                let args = ctx.take_list(*c).collect::<ArrayVec<_, 8>>();

                match func.as_str() {
                    _ => {
                        self.call(&func, &args, chan, ctx)?;
                    }
                }
            }
            Instruction::Begin(b) => {
                ctx.begin = Some(*b);
                chan.send_msg(ConsoleMessage::Exit);
                return Ok(Some(Workflow::Exit));
            }
            Instruction::ConcatString(c) => {
                let args = ctx.take_list(*c);
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
                    let operand = ctx.pop().as_bool();
                    ctx.push(!operand);
                }
            },
            Instruction::BinaryOperator(op) => {
                let rhs = ctx.pop();
                let lhs = ctx.pop();

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
                    _ => todo!("{:?}", op),
                };

                ctx.push(ret);
            }
            Instruction::Goto(no) => {
                *cursor = *no as usize;
            }
            Instruction::GotoIfNot(no) => {
                let cond = ctx.pop().as_bool();
                if !cond {
                    *cursor = *no as usize;
                }
            }
            Instruction::SetAlignment(align) => {
                chan.send_msg(ConsoleMessage::Alignment(*align));
            }
            Instruction::Command(c) => {
                let name = ctx.pop_str()?;
                let args = ctx.take_list(*c).collect::<ArrayVec<_, 4>>();

                match name.as_str() {
                    "DRAWLINE" => {
                        chan.send_msg(ConsoleMessage::DrawLine);
                    }
                    "INPUT" => {
                        chan.send_msg(ConsoleMessage::Input(InputRequest::Int));
                        match chan.recv_ret() {
                            ConsoleResult::Quit => return Ok(Some(Workflow::Exit)),
                            ConsoleResult::Value(ret) => {
                                ctx.var
                                    .get_known(KnownVariables::Result)
                                    .assume_normal()
                                    .set(iter::empty(), ret)?;
                            }
                        }
                    }
                    "QUIT" => {
                        chan.send_msg(ConsoleMessage::Exit);
                        return Ok(Some(Workflow::Exit));
                    }
                    "ADDDEFCHARA" => {
                        ctx.var.add_chara(func_name);
                    }
                    "RESETDATA" => {
                        ctx.var.reset_data();
                    }
                    _ => {
                        bail!("{}({:?})", name, args)
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
        body: &FunctionBody,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let mut cursor = 0;

        let insts = body.body();

        while let Some(inst) = insts.get(cursor) {
            cursor += 1;
            match self.run_instruction(func_name, inst, &mut cursor, chan, ctx) {
                Ok(None) => {}
                Ok(Some(Workflow::Exit)) => return Ok(Workflow::Exit),
                Ok(Some(Workflow::Return)) => break,
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

        ctx.var.init_local(label, body);

        ctx.new_func();

        for ((var_idx, arg_indices), arg) in body.args().iter().zip(args) {
            let var = ctx.var.get_var(*var_idx).unwrap();
            var.1
                .resolve(label, 0)
                .set(arg_indices.iter().copied(), arg.clone())?;
        }

        ctx.end_func();

        self.run_body(label, body, chan, ctx)
    }

    fn call_event(&self, ty: EventType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        self.dic.get_event(ty).run(|body| {
            let label = ty.into();
            ctx.var.init_local(label, body);
            self.run_body(label, body, chan, ctx)?;

            Ok(())
        })
    }

    fn begin(&self, ty: BeginType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
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
