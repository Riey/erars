use std::iter;
use std::ops::Range;

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use either::Either;
use strum::{Display, EnumIter, EnumString};

use hashbrown::HashMap;
use serde::{Deserialize, Serialize};

use crate::instruction::{BeginType, Instruction};
use crate::operator::BinaryOperator;
use crate::value::Value;

#[derive(Clone, Default, Debug, Serialize, Deserialize)]
#[serde(default)]
pub struct VariableInfo {
    is_chara: bool,
    is_str: bool,
    builtin: Option<BulitinVariable>,
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
pub enum Variable {
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

pub struct VariableStorage {
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

    fn get_var(
        &mut self,
        name: &str,
    ) -> Result<&mut (VariableInfo, Either<Variable, Vec<Variable>>)> {
        self.variables
            .get_mut(name)
            .ok_or_else(|| anyhow!("Variable {} is not exists", name))
    }

    pub fn add_chara(&mut self) {
        self.character_len += 1;
        for (_, (info, var)) in self.variables.iter_mut() {
            if let Either::Right(cvar) = var {
                cvar.push(Variable::new(info));
            }
        }
    }

    pub fn get_global(&mut self, name: &str) -> Option<&mut Variable> {
        if let Either::Left(ref mut gvar) = self.variables.get_mut(name)?.1 {
            Some(gvar)
        } else {
            None
        }
    }
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
    stack: Vec<Value>,
    list_stack: Vec<Range<usize>>,
    list_begin_stack: Vec<usize>,
}

impl VmContext {
    pub fn new(infos: &HashMap<String, VariableInfo>) -> Self {
        Self {
            var: VariableStorage::new(infos),
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

    fn pop_int(&mut self) -> Result<i64> {
        self.pop().try_into()
    }
}

pub struct TerminalVm {
    functions: HashMap<String, Vec<Instruction>>,
}

impl TerminalVm {
    pub fn new(functions: HashMap<String, Vec<Instruction>>) -> Self {
        Self { functions }
    }

    fn try_get_func(&self, name: &str) -> Result<&[Instruction]> {
        self.functions
            .get(name)
            .ok_or_else(|| anyhow!("Function {} is not exists", name))
            .map(|b| b.as_slice())
    }

    fn run_instruction(
        &self,
        inst: &Instruction,
        cursor: &mut usize,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        match inst {
            Instruction::LoadInt(n) => ctx.push(*n),
            Instruction::LoadStr(s) => ctx.push(s),
            Instruction::Nop => {}
            Instruction::ListBegin => ctx.set_list_begin(),
            Instruction::ListEnd => ctx.set_list_end(),
            Instruction::StoreVar => {
                let target = ctx
                    .var
                    .get_global("TARGET")
                    .unwrap()
                    .get_int(iter::empty())?
                    .clone()
                    .try_into()?;

                let name = ctx.pop_str()?;

                if name.parse::<BulitinVariable>().is_ok() {
                    return bail!("Can't edit builtin variable");
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
                let target = ctx
                    .var
                    .get_global("TARGET")
                    .unwrap()
                    .get_int(iter::empty())?
                    .clone()
                    .try_into()?;

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
            Instruction::Print(flags) => {}
            Instruction::CallMethod => {
                let name = ctx.pop_str()?;
                let args = ctx.take_list();
                panic!("{}, {:?}", name, args);
            }
            Instruction::Exit => return Ok(Some(Workflow::Exit)),
            Instruction::BinaryOperator(BinaryOperator::Rem) => {
                let rhs = ctx.pop_int()?;
                let lhs = ctx.pop_int()?;
                ctx.push(lhs % rhs);
            }
            Instruction::BinaryOperator(BinaryOperator::Div) => {
                let rhs = ctx.pop_int()?;
                let lhs = ctx.pop_int()?;
                ctx.push(lhs / rhs);
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
            _ => todo!("{:?}", inst),
        }

        Ok(None)
    }

    fn call(&self, name: &str, ctx: &mut VmContext) -> Result<Workflow> {
        let body = self.try_get_func(name)?;

        let mut cursor = 0;

        loop {
            if let Some(inst) = body.get(cursor) {
                cursor += 1;
                match self.run_instruction(inst, &mut cursor, ctx)? {
                    None => {}
                    Some(Workflow::Exit) => return Ok(Workflow::Exit),
                    Some(Workflow::Return) => break,
                }
            } else {
                break;
            }
        }

        Ok(Workflow::Return)
    }

    pub fn begin(&self, ty: BeginType, ctx: &mut VmContext) -> Result<()> {
        match ty {
            BeginType::Title => {
                self.call("SYSTEM_TITLE", ctx)?;
                Ok(())
            }
            BeginType::First => todo!(),
        }
    }
}
