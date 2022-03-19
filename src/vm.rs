use std::iter;
use std::ops::Range;

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use either::Either;
use strum::{Display, EnumIter, EnumString};

use hashbrown::HashMap;
use serde::{Deserialize, Serialize};

use erars_compiler::{BeginType, BinaryOperator, EventType, Instruction, PrintFlags, UnaryOperator};

use crate::function::{FunctionDic, FunctionBody};
use crate::ui::{ConsoleChannel, ConsoleMessage, ConsoleResult, InputRequest};
use crate::value::Value;

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct VariableInfo {
    is_chara: bool,
    is_str: bool,
    default_int: i64,
    size: Vec<usize>,
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
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
    EnumString,
    Serialize,
    Deserialize,
)]
pub enum BulitinVariable {
    #[strum(to_string = "GAMEBASE_AUTHOR")]
    GamebaseAuthor,
    #[strum(to_string = "GAMEBASE_TITLE")]
    GamebaseTitle,
    #[strum(to_string = "GAMEBASE_YEAR")]
    GamebaseYear,
    #[strum(to_string = "GAMEBASE_INFO")]
    GamebaseInfo,
    #[strum(to_string = "GAMEBASE_VERSION")]
    GamebaseVersion,
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

struct VariableStorage {
    character_len: usize,
    variables: HashMap<String, (VariableInfo, Either<Variable, Vec<Variable>>)>,
}

impl VariableStorage {
    pub fn new(infos: &HashMap<String, VariableInfo>) -> Self {
        let variables = infos
            .iter()
            .map(|(name, info)| {
                let var = if info.is_chara {
                    Either::Right(Vec::new())
                } else {
                    Either::Left(Variable::new(info))
                };

                (name.clone(), (info.clone(), var))
            })
            .collect();

        Self {
            character_len: 0,
            variables,
        }
    }

    fn init_local(&mut self, name: &str, body: &FunctionBody) {
        self.variables
            .entry(format!("ARG@{}", name))
            .or_insert_with(|| {
                let arg_info = VariableInfo {
                    default_int: 0,
                    size: vec![1000],
                    is_str: false,
                    is_chara: false,
                };
                let arg = Variable::new(&arg_info);
                (arg_info, Either::Left(arg))
            });
        self.variables
            .entry(format!("ARGS@{}", name))
            .or_insert_with(|| {
                let args_info = VariableInfo {
                    default_int: 0,
                    size: vec![100],
                    is_str: true,
                    is_chara: false,
                };
                let args = Variable::new(&args_info);
                (args_info, Either::Left(args))
            });
        self.variables
            .entry(format!("LOCAL@{}", name))
            .or_insert_with(|| {
                let local_info = VariableInfo {
                    default_int: 0,
                    size: vec![body.local_size()],
                    is_str: false,
                    is_chara: false,
                };
                let local = Variable::new(&local_info);
                (local_info, Either::Left(local))
            });
        self.variables
            .entry(format!("LOCALS@{}", name))
            .or_insert_with(|| {
                let locals_info = VariableInfo {
                    default_int: 0,
                    size: vec![body.locals_size()],
                    is_str: true,
                    is_chara: false,
                };
                let locals = Variable::new(&locals_info);
                (locals_info, Either::Left(locals))
            });
    }

    pub fn target(&mut self) -> Result<usize> {
        Ok((*self
            .get_global("TARGET")
            .ok_or_else(|| anyhow!("TARGET variable is not exists"))?
            .get_int(iter::empty())?)
        .try_into()?)
    }

    fn get_var(
        &mut self,
        name: &str,
    ) -> Result<&mut (VariableInfo, Either<Variable, Vec<Variable>>)> {
        self.variables
            .get_mut(name)
            .ok_or_else(|| anyhow!("Variable {} is not exists", name))
    }

    pub fn reset_data(&mut self) {
        self.character_len = 0;
        self.variables
            .values_mut()
            .for_each(|(info, var)| match var {
                Either::Left(v) => *v = Variable::new(info),
                Either::Right(cvar) => cvar.clear(),
            });
    }

    pub fn add_chara(&mut self) {
        self.character_len += 1;
        self.variables.values_mut().for_each(|(info, var)| {
            if let Either::Right(cvar) = var {
                cvar.push(Variable::new(info));
            }
        });
    }

    pub fn get_global(&mut self, name: &str) -> Option<&mut Variable> {
        if let Either::Left(ref mut gvar) = self.variables.get_mut(name)?.1 {
            Some(gvar)
        } else {
            None
        }
    }

    #[allow(unused)]
    pub fn get_chara(&mut self, name: &str, no: usize) -> Option<&mut Variable> {
        if let Either::Right(ref mut cvar) = self.variables.get_mut(name)?.1 {
            cvar.get_mut(no)
        } else {
            None
        }
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
    list_stack: Vec<Range<usize>>,
    list_begin_stack: Vec<usize>,
}

impl VmContext {
    pub fn new(infos: &HashMap<String, VariableInfo>) -> Self {
        Self {
            var: VariableStorage::new(infos),
            begin: Some(BeginType::Title),
            stack: Vec::with_capacity(1024),
            list_begin_stack: Vec::with_capacity(16),
            list_stack: Vec::with_capacity(32),
        }
    }

    fn set_list_begin(&mut self) {
        self.list_begin_stack.push(self.stack.len());
    }

    fn set_list_end(&mut self) {
        self.list_stack
            .push(self.list_begin_stack.pop().unwrap()..self.stack.len());
    }

    fn take_arg_list(&mut self) -> Result<ArrayVec<usize, 4>> {
        self.stack
            .drain(self.list_stack.pop().unwrap())
            .map(usize::try_from)
            .collect()
    }

    fn take_list(&mut self) -> ArrayVec<Value, 8> {
        self.stack.drain(self.list_stack.pop().unwrap()).collect()
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
        inst: &Instruction,
        cursor: &mut usize,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        match inst {
            Instruction::LoadInt(n) => ctx.push(*n),
            Instruction::LoadStr(s) => ctx.push(s),
            Instruction::Nop => {}
            Instruction::ListBegin => ctx.set_list_begin(),
            Instruction::ListEnd => ctx.set_list_end(),
            Instruction::StoreVar => {
                let target = ctx.var.target()?;

                let name = ctx.pop_str()?;

                if name.parse::<BulitinVariable>().is_ok() {
                    bail!("Can't edit builtin variable");
                } else {
                    let mut args = ctx.take_arg_list()?.into_iter();
                    let value = ctx.pop();

                    let (info, var) = ctx.var.get_var(&name)?;

                    match var {
                        Either::Left(gvar) => {
                            gvar.set(args, value)?;
                        }
                        Either::Right(cvar) => {
                            let no = if args.len() < info.arg_len() {
                                target
                            } else {
                                args.next().unwrap()
                            };
                            cvar[no].set(args, value)?
                        }
                    }
                };
            }
            Instruction::LoadVar => {
                let target = ctx.var.target()?;

                let name = ctx.pop_str()?;

                let value = if let Ok(builtin) = name.parse() {
                    match builtin {
                        BulitinVariable::GamebaseAuthor => "Empty".into(),
                        BulitinVariable::GamebaseYear => 2022i64.into(),
                        BulitinVariable::GamebaseTitle => "Title".into(),
                        BulitinVariable::GamebaseVersion => 1000.into(),
                        BulitinVariable::GamebaseInfo => "Info".into(),
                    }
                } else {
                    let mut args = ctx.take_arg_list()?.into_iter();

                    let (info, var) = ctx.var.get_var(&name)?;

                    match var {
                        Either::Left(gvar) => gvar.get(args)?,
                        Either::Right(cvar) => {
                            let no = if args.len() < info.arg_len() {
                                target
                            } else {
                                args.next().unwrap()
                            };
                            cvar[no].get(args)?
                        }
                    }
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
                let values = ctx.take_list();

                let mut result_idx = 0usize;
                let mut results_idx = 0usize;

                for value in values {
                    match value {
                        Value::Int(_) => {
                            ctx.var
                                .get_global("RESULT")
                                .unwrap()
                                .set(iter::once(result_idx), value)?;
                            result_idx += 1;
                        }
                        Value::String(_) => {
                            ctx.var
                                .get_global("RESULTS")
                                .unwrap()
                                .set(iter::once(results_idx), value)?;
                            results_idx += 1;
                        }
                    }
                }

                return Ok(Some(Workflow::Return));
            }
            Instruction::Call => {
                let func = ctx.pop_str()?;
                let args = ctx.take_list();

                match func.as_str() {
                    "TOSTR" => {
                        let mut args = args.into_iter();
                        let value = args.next().unwrap().try_into_int()?;
                        let format = args.next();

                        let ret = if let Some(_format) = format {
                            format!("{00}", value)
                        } else {
                            value.to_string()
                        };

                        ctx.push(ret);
                    }
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
            Instruction::ConcatString => {
                let args = ctx.take_list();
                let ret = args
                    .into_iter()
                    .fold(String::new(), |s, l| s + &l.into_str());
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
                        Value::String(s) => Value::String(s + &rhs.into_str()),
                    },
                    BinaryOperator::Sub => Value::Int(lhs.try_into_int()? - rhs.try_into_int()?),
                    BinaryOperator::Div => Value::Int(lhs.try_into_int()? / rhs.try_into_int()?),
                    BinaryOperator::Mul => Value::Int(lhs.try_into_int()? * rhs.try_into_int()?),
                    BinaryOperator::Rem => Value::Int(lhs.try_into_int()? % rhs.try_into_int()?),
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
            Instruction::Command => {
                let name = ctx.pop_str()?;
                let args = ctx.take_list();

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
                                    .get_global("RESULT")
                                    .unwrap()
                                    .set(iter::empty(), ret)?;
                            }
                        }
                    }
                    "QUIT" => {
                        chan.send_msg(ConsoleMessage::Exit);
                        return Ok(Some(Workflow::Exit));
                    }
                    "ADDDEFCHARA" => {
                        ctx.var.add_chara();
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
        body: &FunctionBody,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let mut cursor = 0;

        let insts = body.body();

        while let Some(inst) = insts.get(cursor) {
            cursor += 1;
            match self.run_instruction(inst, &mut cursor, chan, ctx) {
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

        for ((arg_name, arg_indices), arg) in body.args().iter().zip(args) {
            let var = ctx.var.get_global(arg_name).unwrap();
            var.set(arg_indices.iter().copied(), arg.clone())?;
        }

        self.run_body(body, chan, ctx)
    }

    fn call_event(&self, ty: EventType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        self.dic.get_event(ty).run(|body| {
            ctx.var.init_local(ty.into(), body);
            self.run_body(body, chan, ctx)?;

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