use std::iter;

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use smartstring::{LazyCompact, SmartString};
use std::sync::Arc;
use strum::Display;

use hashbrown::HashMap;

use erars_compiler::{
    BeginType, BinaryOperator, BuiltinCommand, BulitinVariable, EventType, FunctionIndex,
    GlobalIndex, Instruction, KnownVariables, LocalIndex, PrintFlags, UnaryOperator, Value,
    VariableDic, VariableIndex, VariableInfo,
};

use crate::function::{FunctionBody, FunctionDic};
use crate::ui::{ConsoleChannel, ConsoleMessage};

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
            (false, []) => Self::Int0D(info.init.get(0).map(Value::expect_int).unwrap_or_default()),
            (false, [a]) => Self::Int1D(
                (0..*a)
                    .map(|i| info.init.get(i).map(Value::expect_int).unwrap_or_default())
                    .collect(),
            ),
            (false, [a, b]) => Self::Int2D(vec![vec![0; *a]; *b]),
            (false, [a, b, c]) => Self::Int3D(vec![vec![vec![0; *a]; *b]; *c]),
            (false, []) => Self::Str0D(info.init.get(0).map(Value::expect_str).unwrap_or_default()),
            (false, [a]) => Self::Str1D(
                (0..*a)
                    .map(|i| info.init.get(i).map(Value::expect_str).unwrap_or_default())
                    .collect(),
            ),
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
            | Variable::Int3D(..) => {
                *self.get_int(args)? = value.try_into().map_err(|msg| anyhow!("{}", msg))?
            }
            Variable::Str0D(..)
            | Variable::Str1D(..)
            | Variable::Str2D(..)
            | Variable::Str3D(..) => {
                *self.get_str(args)? = value.try_into().map_err(|msg| anyhow!("{}", msg))?
            }
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
    Bulitin(BulitinVariable),
}

impl UniformVariable {
    pub fn new(info: &VariableInfo) -> Self {
        match info.is_chara {
            false => UniformVariable::Normal(Variable::new(info)),
            true => UniformVariable::Character(Vec::new()),
        }
    }

    pub fn assume_normal(&mut self) -> &mut Variable {
        match self {
            Self::Normal(v) => v,
            _ => panic!("Variable is not normal variable"),
        }
    }

    pub fn add_chara(&mut self, info: &VariableInfo) {
        match self {
            UniformVariable::Character(c) => c.push(Variable::new(info)),
            _ => {}
        }
    }
}

struct VariableStorage {
    character_len: usize,
    var_dic: Arc<VariableDic>,
    variables: Vec<(VariableInfo, UniformVariable)>,
    local_variables: HashMap<FunctionIndex, Vec<(VariableInfo, UniformVariable)>>,
}

impl VariableStorage {
    pub fn new(var_dic: Arc<VariableDic>) -> Self {
        let mut variables = Vec::new();

        var_dic.iter_global(|idx, info| {
            let var = if let Some(builtin) = idx.to_builtin() {
                UniformVariable::Bulitin(builtin)
            } else {
                UniformVariable::new(info)
            };
            variables.push((info.clone(), var));
        });

        Self {
            character_len: 0,
            var_dic,
            variables,
            local_variables: HashMap::new(),
        }
    }

    fn init_local(&mut self, body: &FunctionBody) {
        let func_idx = body.idx();
        self.local_variables.entry(func_idx).or_insert_with(|| {
            let mut vars = Vec::new();
            self.var_dic.iter_local(func_idx, |local_idx, info| {
                vars.push((info.clone(), UniformVariable::new(info)));
            });
            vars
        });
    }

    fn get_var(&mut self, idx: VariableIndex) -> &mut (VariableInfo, UniformVariable) {
        match idx {
            VariableIndex::Global(g) => self.get_global_var(g),
            VariableIndex::Local(f, l) => self.get_local_var(f, l),
        }
    }

    fn get_local_var(
        &mut self,
        func: FunctionIndex,
        idx: LocalIndex,
    ) -> &mut (VariableInfo, UniformVariable) {
        self.local_variables
            .get_mut(&func)
            .unwrap()
            .get_mut(idx.as_usize())
            .unwrap()
    }

    fn get_global_var(&mut self, idx: GlobalIndex) -> &mut (VariableInfo, UniformVariable) {
        self.variables.get_mut(idx.as_usize()).unwrap()
    }

    fn get_global_var2(
        &mut self,
        l: GlobalIndex,
        r: GlobalIndex,
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

    pub fn add_chara(&mut self) {
        self.character_len += 1;
        self.variables.iter_mut().for_each(|(info, var)| {
            var.add_chara(info);
        });
    }

    pub fn get_known2(
        &mut self,
        l: KnownVariables,
        r: KnownVariables,
    ) -> (&mut UniformVariable, &mut UniformVariable) {
        let (l, r) = self
            .get_global_var2(self.var_dic.get_known(l), self.var_dic.get_known(r))
            .unwrap();

        (&mut l.1, &mut r.1)
    }

    pub fn get_known(&mut self, known: KnownVariables) -> &mut UniformVariable {
        &mut self.get_global_var(self.var_dic.get_known(known)).1
    }
}

#[derive(Display, Debug, Clone, Copy)]
enum Workflow {
    Return,
    Exit,
}

struct Callstack {
    stack_base: usize,
}

pub struct VmContext {
    var: VariableStorage,
    begin: Option<BeginType>,
    stack: Vec<Value>,
    call_stack: Vec<Callstack>,
}

impl VmContext {
    pub fn new(variable_interner: Arc<VariableDic>) -> Self {
        Self {
            var: VariableStorage::new(variable_interner),
            begin: Some(BeginType::Title),
            stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(512),
        }
    }

    pub fn new_func(&mut self, body: &FunctionBody) {
        self.var.init_local(body);
        self.call_stack.push(Callstack {
            stack_base: self.stack.len(),
        });
    }

    pub fn end_func(&mut self) {
        self.call_stack.pop();
    }

    pub fn return_func(&mut self) -> impl Iterator<Item = Value> + '_ {
        let call_stack = self.call_stack.last().unwrap();
        self.stack.drain(call_stack.stack_base..)
    }

    fn take_arg_list(&mut self, count: u32) -> Result<ArrayVec<usize, 4>> {
        self.take_list(count)
            .map(|v| Ok(usize::try_from(v)?))
            .collect()
    }

    fn take_list(&mut self, count: u32) -> impl Iterator<Item = Value> + '_ {
        self.stack.drain(self.stack.len() - count as usize..)
    }

    fn dup(&mut self) {
        let last = self.stack.last().unwrap().clone();
        self.push(last);
    }

    fn dup_prev(&mut self) {
        let prev = self.stack[self.stack.len() - 2].clone();
        self.push(prev);
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
        Ok(self.pop().try_into()?)
    }

    #[allow(unused)]
    fn pop_int(&mut self) -> Result<i64> {
        Ok(self.pop().try_into()?)
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
        goto_labels: &HashMap<SmartString<LazyCompact>, u32>,
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
            Instruction::GotoLabel => {
                *cursor = goto_labels[ctx.pop_str()?.as_str()] as usize;
            }
            Instruction::StoreVar(var_idx, c) => {
                let target = *ctx
                    .var
                    .get_known(KnownVariables::Target)
                    .assume_normal()
                    .get_int(iter::empty())?;

                let mut args = ctx.take_arg_list(*c)?.into_iter();
                let value = ctx.pop();

                let (info, var) = ctx.var.get_var(*var_idx);

                match var {
                    UniformVariable::Bulitin(_) => bail!("Builtin variables are immutable"),
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
            Instruction::LoadVar(var_idx, c) => {
                let target = *ctx
                    .var
                    .get_known(KnownVariables::Target)
                    .assume_normal()
                    .get_int(iter::empty())?;

                let mut args = ctx.take_arg_list(*c)?.into_iter();

                let (info, var) = ctx.var.get_var(*var_idx);

                let value = match var {
                    UniformVariable::Bulitin(builtin) => match builtin {
                        BulitinVariable::Gamebase_Author => "Empty".into(),
                        BulitinVariable::Gamebase_Year => 2022i64.into(),
                        BulitinVariable::Gamebase_Title => "Title".into(),
                        BulitinVariable::Gamebase_Version => 1000.into(),
                        BulitinVariable::Gamebase_Info => "Info".into(),
                        other => todo!("{other}"),
                    },
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
                let idx = ctx.var.var_dic.get_func(func).unwrap();
                let args = ctx.take_list(*c).collect::<ArrayVec<_, 8>>();

                self.call(idx, &args, chan, ctx)?;
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
                UnaryOperator::BitNot => {
                    let operand = ctx.pop_int()?;
                    ctx.push(-operand);
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
                    BinaryOperator::And => Value::Int(i64::from(lhs.as_bool() && rhs.as_bool())),
                    BinaryOperator::Or => Value::Int(i64::from(lhs.as_bool() || rhs.as_bool())),
                    BinaryOperator::Xor => Value::Int(i64::from(lhs.as_bool() ^ rhs.as_bool())),
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
            Instruction::Command(com, c) => {
                let args = ctx.take_list(*c).collect::<ArrayVec<_, 4>>();
            }
            _ => bail!("TODO: {:?}", inst),
        }

        Ok(None)
    }

    fn run_body(
        &self,
        goto_labels: &HashMap<SmartString<LazyCompact>, u32>,
        body: &FunctionBody,
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let mut cursor = 0;

        let insts = body.body();

        while let Some(inst) = insts.get(cursor) {
            cursor += 1;
            match self.run_instruction(goto_labels, inst, &mut cursor, chan, ctx) {
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
        idx: FunctionIndex,
        args: &[Value],
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Workflow> {
        let body = self.dic.get_func(idx)?;

        ctx.new_func(body);

        let mut args = args.iter().cloned();

        for (var_idx, default_value, arg_indices) in body.args().iter() {
            let var = &mut ctx.var.get_var(*var_idx).1;

            var.assume_normal().set(
                arg_indices.iter().copied(),
                args.next()
                    .unwrap_or_else(|| default_value.clone().unwrap()),
            )?;
        }

        let ret = self.run_body(body.goto_labels(), body, chan, ctx);

        ctx.end_func();

        ret
    }

    fn call_event(&self, ty: EventType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        self.dic.get_event(ty).run(|body| {
            ctx.new_func(body);
            self.run_body(body.goto_labels(), body, chan, ctx)?;
            ctx.end_func();

            Ok(())
        })
    }

    fn begin(&self, ty: BeginType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        match ty {
            BeginType::Title => {
                self.call(
                    ctx.var.var_dic.get_func("SYSTEM_TITLE").unwrap(),
                    &[],
                    chan,
                    ctx,
                )?;
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
