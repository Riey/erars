mod button_parser;

use crossbeam_channel::{bounded, Receiver, Sender};
use maplit::btreemap;
use std::iter;
use std::ops::Range;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{anyhow, bail, Result};
use arrayvec::ArrayVec;
use eframe::epi::{App, Frame};
use egui::{Color32, CtxRef, FontData, FontDefinitions, FontFamily};
use either::Either;
use strum::{Display, EnumIter, EnumString};

use hashbrown::HashMap;
use serde::{Deserialize, Serialize};

use crate::compiler::{Alignment, PrintFlags};
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

    fn pop_int(&mut self) -> Result<i64> {
        self.pop().try_into()
    }
}

const FONT: &[u8] = include_bytes!("../res/D2Coding-Ver1.3.2-20180524.ttc");

pub struct EraApp {
    console: EraConsole,
    req: Option<InputRequest>,
    chan: Arc<ConsoleChannel>,
}

impl EraApp {
    pub fn new(chan: Arc<ConsoleChannel>) -> Self {
        Self {
            console: EraConsole::default(),
            req: None,
            chan,
        }
    }
}

impl App for EraApp {
    fn setup(&mut self, ctx: &CtxRef, _frame: &Frame, _storage: Option<&dyn eframe::epi::Storage>) {
        ctx.set_fonts(FontDefinitions {
            font_data: btreemap! {
                "default".into() => FontData::from_static(FONT),
            },
            fonts_for_family: btreemap! {
                FontFamily::Monospace => vec!["default".into()],
                FontFamily::Proportional => vec!["default".into()],
            },
            ..Default::default()
        });

        let mut style = (*ctx.style()).clone();
        style.visuals.override_text_color = Some(Color32::WHITE);
        ctx.set_style(style);
    }

    fn name(&self) -> &str {
        "erars"
    }

    fn update(&mut self, ctx: &CtxRef, frame: &Frame) {
        if self.req.is_none() {
            while let Some(msg) = self.chan.recv_msg() {
                match msg {
                    ConsoleMessage::Exit => {
                        frame.quit();
                        return;
                    }
                    ConsoleMessage::Input(req) => self.req = Some(req),
                    ConsoleMessage::NewLine => self.console.new_line(),
                    ConsoleMessage::Print(s) => self.console.print(s),
                    ConsoleMessage::DrawLine => self.console.draw_line(),
                    ConsoleMessage::Alignment(align) => self.console.set_align(align),
                    ConsoleMessage::PrintButton(value, s) => self.console.print_button(value, s),
                    ConsoleMessage::ReuseLastLine(s) => self.console.reuse_last_line(s),
                }
            }
        }

        egui::CentralPanel::default().show(ctx, |ui| {
            let mut ret = |value: &Value| match (&self.req, value) {
                (Some(InputRequest::Anykey), _)
                | (Some(InputRequest::EnterKey), _)
                | (Some(InputRequest::Int), Value::Int(_))
                | (Some(InputRequest::Str), Value::String(_)) => {
                    self.req = None;
                    self.chan.send_ret(ConsoleResult::Value(value.clone()));
                }
                _ => {}
            };
            for line in self
                .console
                .lines
                .iter()
                .chain(iter::once(&self.console.last_line))
            {
                match line.align {
                    Alignment::Left => {
                        ui.horizontal(|ui| {
                            for part in line.parts.iter() {
                                part.display(ui, &mut ret);
                            }
                        });
                    }
                    Alignment::Center => {
                        ui.vertical_centered(|ui| {
                            for part in line.parts.iter() {
                                part.display(ui, &mut ret);
                            }
                        });
                    }
                    Alignment::Right => todo!(),
                }
            }
        });
    }
}

#[derive(Default)]
struct EraConsole {
    lines: Vec<ConsoleLine>,
    last_line: ConsoleLine,
}

#[derive(Default)]
struct ConsoleLine {
    parts: Vec<ConsoleLinePart>,
    align: Alignment,
    reuse: bool,
}

enum ConsoleLinePart {
    Normal(String),
    Line,
    Button(Value, String),
}

impl ConsoleLinePart {
    pub fn display(&self, ui: &mut egui::Ui, ret: impl FnOnce(&Value)) {
        match self {
            ConsoleLinePart::Normal(s) => {
                ui.label(s);
            }
            ConsoleLinePart::Line => {
                let width = ui.available_width();
                let char_width = ui.fonts().glyph_width(egui::TextStyle::Monospace, '=');
                let s = "=".repeat((width / char_width) as usize);
                ui.label(s);
            }
            ConsoleLinePart::Button(value, text) => {
                if ui.small_button(text).clicked() {
                    ret(value);
                }
            }
        }
    }
}

impl EraConsole {
    pub fn print_button(&mut self, value: Value, s: String) {
        self.last_line.parts.push(ConsoleLinePart::Button(value, s));
    }

    pub fn print_plain_text(&mut self, s: String) {
        self.last_line.parts.push(ConsoleLinePart::Normal(s));
    }

    pub fn print(&mut self, s: String) {
        dbg!(&s);
        button_parser::parse_button(s, &mut self.last_line.parts);
    }
    pub fn new_line(&mut self) {
        let prev_line = std::mem::take(&mut self.last_line);
        let prev_align = prev_line.align;
        self.lines.push(prev_line);
        self.last_line.align = prev_align;
    }
    pub fn set_align(&mut self, align: Alignment) {
        self.last_line.align = align;
    }

    pub fn draw_line(&mut self) {
        self.last_line.parts.push(ConsoleLinePart::Line);
        self.new_line();
    }

    pub fn reuse_last_line(&mut self, s: String) {
        if !self.last_line.reuse {
            self.new_line();
            self.print(s);
            self.last_line.reuse = true;
        } else {
            self.last_line.parts.clear();
            self.print(s);
        }
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
        chan: &ConsoleChannel,
        ctx: &mut VmContext,
    ) -> Result<Option<Workflow>> {
        eprintln!("Run {:?}", inst);
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
            Instruction::CallMethod => {
                let name = ctx.pop_str()?;
                let mut args = ctx.take_list().into_iter();

                match name.as_str() {
                    "TOSTR" => {
                        let value = args.next().unwrap().try_into_int()?;
                        let format = args.next();

                        let ret = if let Some(_format) = format {
                            format!("{00}", value)
                        } else {
                            value.to_string()
                        };

                        ctx.push(ret);
                    }
                    _ => bail!("Unknown method {}", name),
                }
            }
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
                    _ => {
                        bail!("{}({:?})", name, args)
                    }
                }
            }
            _ => bail!("{:?}", inst),
        }

        Ok(None)
    }

    fn call(&self, name: &str, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<Workflow> {
        let body = self.try_get_func(name)?;

        let mut cursor = 0;

        loop {
            if let Some(inst) = body.get(cursor) {
                cursor += 1;
                match self.run_instruction(inst, &mut cursor, chan, ctx) {
                    Ok(None) => {}
                    Ok(Some(Workflow::Exit)) => return Ok(Workflow::Exit),
                    Ok(Some(Workflow::Return)) => break,
                    Err(err) => {
                        eprintln!("Error occured in {}@{} ({:?})", name, cursor, inst);
                        return Err(err);
                    }
                }
            } else {
                break;
            }
        }

        Ok(Workflow::Return)
    }

    fn begin(&self, ty: BeginType, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        match ty {
            BeginType::Title => {
                self.call("SYSTEM_TITLE", chan, ctx)?;
                Ok(())
            }
            BeginType::First => todo!(),
        }
    }

    pub fn start(&self, chan: &ConsoleChannel, ctx: &mut VmContext) -> Result<()> {
        while let Some(begin) = ctx.begin.take() {
            self.begin(begin, chan, ctx)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConsoleMessage {
    Print(String),
    NewLine,
    DrawLine,
    PrintButton(Value, String),
    ReuseLastLine(String),
    Alignment(Alignment),
    Input(InputRequest),
    Exit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InputRequest {
    Anykey,
    EnterKey,
    Int,
    Str,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConsoleResult {
    Quit,
    Value(Value),
}

pub struct ConsoleChannel {
    console: (Sender<ConsoleMessage>, Receiver<ConsoleMessage>),
    ret: (Sender<ConsoleResult>, Receiver<ConsoleResult>),
}

impl ConsoleChannel {
    pub fn new() -> Self {
        Self {
            console: bounded(256),
            ret: bounded(8),
        }
    }

    pub fn take_all_msg(self) -> Vec<ConsoleMessage> {
        let mut ret = Vec::new();
        while let Ok(msg) = self.console.1.try_recv() {
            ret.push(msg);
        }
        ret
    }

    fn send_msg(&self, msg: ConsoleMessage) {
        self.console.0.send(msg).unwrap();
    }

    fn recv_msg(&self) -> Option<ConsoleMessage> {
        self.console.1.recv_timeout(Duration::from_millis(50)).ok()
    }

    fn send_ret(&self, ret: ConsoleResult) {
        self.ret.0.send(ret).unwrap()
    }

    fn recv_ret(&self) -> ConsoleResult {
        self.ret.1.recv().unwrap()
    }
}
