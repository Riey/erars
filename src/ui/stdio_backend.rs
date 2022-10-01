use super::{
    ConsoleChannel, ConsoleLinePart, ConsoleResult, EraApp, FontStyle,
    InputRequest, TextStyle, VirtualConsole,
};
use std::{
    io::{self, BufRead},
    sync::{
        atomic::{AtomicBool, Ordering::SeqCst},
        Arc,
    },
};

use erars_ast::Value;

pub struct StdioBackend {
    vconsole: VirtualConsole,
    req: Option<InputRequest>,
    need_redraw: bool,
}

impl StdioBackend {
    pub fn new() -> Self {
        Self {
            vconsole: VirtualConsole::new(),
            req: None,
            need_redraw: true,
        }
    }

    fn draw(&mut self, mut out: impl io::Write) -> anyhow::Result<()> {
        if self.need_redraw {
            for line in self.vconsole.lines.drain(..) {
                for part in line.parts.iter() {
                    match part {
                        ConsoleLinePart::Text(text, style) => {
                            write!(out, "{}", paint(style, text, false))?;
                        }
                        ConsoleLinePart::Button(btns, _value) => {
                            for (text, style) in btns.iter() {
                                write!(out, "{}", paint(style, text, true))?;
                            }
                        }
                        ConsoleLinePart::Line(text, style) => {
                            write!(out, "{}", paint(style, &text.repeat(30), false))?;
                        }
                    }
                }
                writeln!(out)?;
            }
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
                    match self.vconsole.push_msg(msg) {
                        Some(req) => {
                            self.req = Some(req);
                        }
                        None => {
                            self.need_redraw = true;
                        }
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

fn paint<'a>(
    style: &TextStyle,
    text: &'a str,
    is_btn: bool,
) -> ansi_term::ANSIGenericString<'a, str> {
    let color = if is_btn {
        ansi_term::Color::Yellow
    } else {
        ansi_term::Color::RGB(style.color.0[0], style.color.0[1], style.color.0[2])
    };

    let mut s = color.paint(text);

    s.style_ref_mut().is_bold = style.font_style.contains(FontStyle::BOLD);
    s.style_ref_mut().is_italic = style.font_style.contains(FontStyle::ITALIC);
    s.style_ref_mut().is_strikethrough = style.font_style.contains(FontStyle::STRIKELINE);
    s.style_ref_mut().is_underline = style.font_style.contains(FontStyle::UNDERLINE);

    s
}
