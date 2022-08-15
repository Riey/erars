use crossbeam_channel::{bounded, Receiver, Sender};
use erars_ast::{Alignment, Value};
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
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
    DrawLine,
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
        match self.redraw_fn.lock().as_deref() {
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
