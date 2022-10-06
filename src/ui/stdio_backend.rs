use super::{
    ConsoleChannel, ConsoleLinePart, ConsoleResult, EraApp, FontStyle, InputRequestType,
    VirtualConsole,
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
    need_redraw: bool,
}

impl StdioBackend {
    pub fn new() -> Self {
        Self {
            vconsole: VirtualConsole::new(),
            need_redraw: true,
        }
    }

    fn draw(&mut self, mut out: impl io::Write) -> anyhow::Result<()> {
        if self.need_redraw {
            for line in self.vconsole.lines.drain(..) {
                for part in line.parts.iter() {
                    match part {
                        ConsoleLinePart::Text(text, style) => {
                            write!(out, "{}", paint(style.color, style.font_style, text))?;
                        }
                        ConsoleLinePart::Button(btns, _value) => {
                            for (text, style) in btns.iter() {
                                write!(
                                    out,
                                    "{}",
                                    paint(self.vconsole.hl_color, style.font_style, text)
                                )?;
                            }
                        }
                        ConsoleLinePart::Line(text, style) => {
                            write!(
                                out,
                                "{}",
                                paint(style.color, style.font_style, &text.repeat(30))
                            )?;
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

            while let Some(msg) = chan.recv_msg() {
                self.need_redraw = true;
                log::trace!("[UI] Recv: {msg:?}");
                self.vconsole.push_msg(msg);
            }

            self.draw(&mut lock)?;

            if let Some(ty) = self.vconsole.current_req.as_ref().map(|i| i.ty) {
                let size = stdin.read_line(&mut input)?;

                let s = input[..size].trim_end_matches(&['\r', '\n']);

                match ty {
                    InputRequestType::Int => match s.trim().parse() {
                        Ok(i) => {
                            chan.send_ret(ConsoleResult::Value(Value::Int(i)));
                            self.vconsole.current_req = None;
                        }
                        Err(_) => {}
                    },
                    InputRequestType::Str => {
                        chan.send_ret(ConsoleResult::Value(Value::String(s.into())));
                        self.vconsole.current_req = None;
                    }
                    InputRequestType::AnyKey | InputRequestType::EnterKey => {
                        chan.send_ret(ConsoleResult::Value(Value::Int(0)));
                        self.vconsole.current_req = None;
                    }
                }

                input.clear();
            }
        }
    }
}

fn paint<'a>(
    color: super::Color,
    font_style: FontStyle,
    text: &'a str,
) -> ansi_term::ANSIGenericString<'a, str> {
    let color = ansi_term::Color::RGB(color.0[0], color.0[1], color.0[2]);

    let mut s = color.paint(text);

    s.style_ref_mut().is_bold = font_style.contains(FontStyle::BOLD);
    s.style_ref_mut().is_italic = font_style.contains(FontStyle::ITALIC);
    s.style_ref_mut().is_strikethrough = font_style.contains(FontStyle::STRIKELINE);
    s.style_ref_mut().is_underline = font_style.contains(FontStyle::UNDERLINE);

    s
}
