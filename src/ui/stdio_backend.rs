use super::{ConsoleChannel, ConsoleMessage, ConsoleResult, EraApp, InputRequest};
use std::{
    io::{self, BufRead, Write},
    sync::{
        atomic::{AtomicBool, Ordering::SeqCst},
        Arc,
    },
};

use ansi_term::Color;
use erars_ast::Value;

enum ConsoleLinePart {
    Normal(String, Color),
    Line(Color),
}

impl ConsoleLinePart {
    fn draw(&self, drawline_str: &str, mut out: impl Write) -> anyhow::Result<()> {
        match self {
            ConsoleLinePart::Normal(s, c) => {
                write!(out, "{}", c.paint(s))?;
            }
            ConsoleLinePart::Line(c) => {
                write!(out, "{}", c.paint(drawline_str))?;
            }
        }

        Ok(())
    }
}

pub struct StdioBackend {
    lines: Vec<Vec<ConsoleLinePart>>,
    req: Option<InputRequest>,
    need_redraw: bool,
}

impl StdioBackend {
    pub fn new() -> Self {
        Self {
            lines: vec![Vec::new()],
            req: None,
            need_redraw: true,
        }
    }

    fn new_line(&mut self) {
        self.lines.push(Vec::new());
    }

    fn print(&mut self, s: impl Into<String>) {
        self.need_redraw = true;
        self.lines
            .last_mut()
            .unwrap()
            .push(ConsoleLinePart::Normal(s.into(), Color::White));
    }

    fn draw_line(&mut self) {
        self.need_redraw = true;
        if !self.lines.last().unwrap().is_empty() {
            self.new_line();
        }
        self.lines
            .last_mut()
            .unwrap()
            .push(ConsoleLinePart::Line(Color::White));
        self.new_line();
    }

    fn draw(&mut self, mut out: impl io::Write) -> anyhow::Result<()> {
        if self.need_redraw {
            for line in self.lines.iter() {
                for part in line.iter() {
                    part.draw("LINE", &mut out)?;
                }
                writeln!(out)?;
            }
            let no = self.lines.len();
            self.lines.drain(..no - 1);
            self.need_redraw = false;
        }

        Ok(())
    }
}

impl EraApp for StdioBackend {
    fn run(&mut self, chan: Arc<ConsoleChannel>) -> anyhow::Result<()> {
        let mut input = String::with_capacity(64);
        let mut stdin = io::stdin().lock();
        let mut lock = io::stdout().lock();
        let end = Arc::new(AtomicBool::new(false));

        let end_inner = end.clone();
        chan.set_exit_fn(move || {
            end_inner.store(true, SeqCst);
        });

        loop {
            if end.load(SeqCst) {
                break Ok(());
            }

            if self.req.is_none() {
                while let Some(msg) = chan.recv_msg() {
                    log::trace!("[UI] Recv: {msg:?}");
                    match msg {
                        ConsoleMessage::DrawLine => self.draw_line(),
                        ConsoleMessage::Print(str) => self.print(str),
                        ConsoleMessage::PrintButton(_, str) => self.print(str),
                        ConsoleMessage::NewLine => self.new_line(),
                        ConsoleMessage::Input(req) => {
                            self.req = Some(req);
                        }
                        ConsoleMessage::ReuseLastLine(str) => {
                            self.print(str);
                            self.new_line();
                        }
                        ConsoleMessage::Alignment(_) => {}
                        ConsoleMessage::SetColor(_, _, _) => {}
                    }
                }
            }

            self.draw(&mut lock)?;

            if let Some(req) = self.req {
                let size = stdin.read_line(&mut input)?;

                let s = input[..size].trim_end_matches(&['\r', '\n']);

                match req {
                    InputRequest::Int => match s.trim().parse() {
                        Ok(i) => {
                            chan.send_ret(ConsoleResult::Value(Value::Int(i)));
                            self.req = None;
                        }
                        Err(_) => {}
                    },
                    InputRequest::Str => {
                        chan.send_ret(ConsoleResult::Value(Value::String(s.into())));
                        self.req = None;
                    }
                    InputRequest::Anykey | InputRequest::EnterKey => {
                        chan.send_ret(ConsoleResult::Value(Value::Int(0)));
                        self.req = None;
                    }
                }

                input.clear();
            }
        }
    }
}
