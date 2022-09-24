use crossbeam_channel::{bounded, Receiver, Sender};
use erars_ast::{Alignment, Value};
use pad::PadStr;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering::SeqCst};
use std::sync::Arc;
use std::time::Duration;

#[cfg(feature = "stdio-backend")]
mod stdio_backend;

#[cfg(feature = "stdio-backend")]
pub use stdio_backend::StdioBackend;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConsoleMessage {
    Print(String),
    NewLine,
    DrawLine(String),
    PrintButton(Value, String),
    ReuseLastLine(String),
    Alignment(Alignment),
    Input(InputRequest),
    SetColor(u8, u8, u8),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
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

pub trait EraApp {
    fn run(&mut self, chan: Arc<ConsoleChannel>) -> anyhow::Result<()>;
}

pub struct ConsoleSender {
    chan: Arc<ConsoleChannel>,
    line_is_empty: bool,
    printc_count: u32,
    color: u32,
    hl_color: u32,
    bg_color: u32,
    align: Alignment,
    inputs: VecDeque<Value>,
}

impl ConsoleSender {
    pub fn new(chan: Arc<ConsoleChannel>) -> Self {
        Self {
            chan,

            line_is_empty: true,
            printc_count: 0,
            color: u32::from_le_bytes([0xFF, 0xFF, 0xFF, 0x00]),
            hl_color: u32::from_le_bytes([0xFF, 0xFF, 0x00, 0x00]),
            bg_color: u32::from_le_bytes([0x00, 0x00, 0x00, 0x00]),
            align: Alignment::Left,
            inputs: VecDeque::new(),
        }
    }

    pub fn push_input(&mut self, value: Value) {
        self.inputs.push_back(value);
    }

    pub fn input(&mut self, req: InputRequest) -> ConsoleResult {
        if let Some(i) = self.inputs.pop_front() {
            ConsoleResult::Value(i)
        } else {
            self.chan.send_msg(ConsoleMessage::Input(req));
            let ret = self.chan.recv_ret();
            log::trace!("Console Recv {ret:?}");
            ret
        }
    }

    pub fn line_is_empty(&self) -> bool {
        self.line_is_empty
    }

    pub fn reuse_last_line(&mut self, s: String) {
        self.line_is_empty = false;
        self.chan.send_msg(ConsoleMessage::ReuseLastLine(s));
    }

    pub fn print(&mut self, s: String) {
        self.line_is_empty = false;
        self.chan.send_msg(ConsoleMessage::Print(s));
    }

    pub fn print_line(&mut self, s: String) {
        self.print(s);
        self.new_line();
    }

    pub fn printlc(&mut self, s: &str) {
        if self.printc_count == 3 {
            self.new_line();
        }
        self.printc_count += 1;
        self.print(s.pad_to_width_with_alignment(30, pad::Alignment::Left));
    }

    pub fn printrc(&mut self, s: &str) {
        if self.printc_count == 3 {
            self.new_line();
        }
        self.printc_count += 1;
        self.print(s.pad_to_width_with_alignment(30, pad::Alignment::Right));
    }

    pub fn new_line(&mut self) {
        self.printc_count = 0;
        self.line_is_empty = true;
        self.chan.send_msg(ConsoleMessage::NewLine);
    }

    pub fn draw_line(&mut self, s: String) {
        self.printc_count = 0;
        self.line_is_empty = true;
        self.chan.send_msg(ConsoleMessage::DrawLine(s));
    }

    pub fn set_color(&mut self, r: u8, g: u8, b: u8) {
        self.color = u32::from_le_bytes([r, g, b, 0]);
        self.chan.send_msg(ConsoleMessage::SetColor(r, g, b));
    }

    pub fn set_align(&mut self, align: Alignment) {
        self.align = align;
        self.chan.send_msg(ConsoleMessage::Alignment(align));
    }

    pub fn exit(&self) {
        self.chan.exit();
    }

    pub fn align(&self) -> Alignment {
        self.align
    }

    pub fn color(&self) -> u32 {
        self.color
    }

    pub fn hl_color(&self) -> u32 {
        self.hl_color
    }

    pub fn bg_color(&self) -> u32 {
        self.bg_color
    }

    pub fn request_redraw(&self) {
        self.chan.request_redraw();
    }

    pub fn into_chan(self) -> Arc<ConsoleChannel> {
        self.chan
    }
}

pub struct ConsoleChannel {
    redraw_fn: Mutex<Option<Box<dyn Fn() + Send + Sync>>>,
    exit_fn: Mutex<Option<Box<dyn Fn() + Send + Sync>>>,
    delay_redraw: AtomicBool,
    delay_exit: AtomicBool,
    console: (Sender<ConsoleMessage>, Receiver<ConsoleMessage>),
    ret: (Sender<ConsoleResult>, Receiver<ConsoleResult>),
}

impl ConsoleChannel {
    pub fn new() -> Self {
        Self {
            redraw_fn: Mutex::new(None),
            exit_fn: Mutex::new(None),
            delay_redraw: AtomicBool::new(false),
            delay_exit: AtomicBool::new(false),
            console: bounded(256),
            ret: bounded(8),
        }
    }

    pub fn set_redraw_fn(&self, f: impl Fn() + Send + Sync + 'static) {
        let mut redraw_fn = self.redraw_fn.lock();

        if self.delay_redraw.swap(false, SeqCst) {
            f();
        }

        *redraw_fn = Some(Box::new(f));
    }

    pub fn set_exit_fn(&self, f: impl Fn() + Send + Sync + 'static) {
        let mut exit_fn = self.exit_fn.lock();

        if self.delay_exit.swap(false, SeqCst) {
            f();
        }

        *exit_fn = Some(Box::new(f));
    }

    pub fn request_redraw(&self) {
        match self.redraw_fn.lock().as_deref() {
            Some(f) => f(),
            None => self.delay_redraw.store(true, SeqCst),
        }
    }

    pub fn exit(&self) {
        match self.exit_fn.lock().as_deref() {
            Some(f) => f(),
            None => self.delay_exit.store(true, SeqCst),
        }
    }

    pub fn take_all_msg(self) -> Vec<ConsoleMessage> {
        let mut ret = Vec::new();
        while let Ok(msg) = self.console.1.try_recv() {
            ret.push(msg);
        }
        ret
    }

    pub fn send_msg(&self, msg: ConsoleMessage) {
        self.console.0.send(msg).unwrap();
    }

    pub fn recv_msg(&self) -> Option<ConsoleMessage> {
        self.console.1.recv_timeout(Duration::from_millis(50)).ok()
    }

    pub fn send_ret(&self, ret: ConsoleResult) {
        self.ret.0.send(ret).unwrap()
    }

    pub fn recv_ret(&self) -> ConsoleResult {
        self.ret.1.recv().unwrap()
    }
}
