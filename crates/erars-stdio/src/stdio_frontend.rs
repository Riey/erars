use erars_ui::{ConsoleChannel, ConsoleLinePart, FontStyle, InputRequestType, VirtualConsole};
use std::{
    io,
    sync::{
        atomic::{AtomicBool, Ordering::SeqCst},
        Arc,
    },
};

use erars_ast::Value;

pub struct StdioFrontend {
    vconsole: VirtualConsole,
    need_redraw: bool,
}

impl StdioFrontend {
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
                            write!(out, "{}", paint(style.color, style.font_style, &text))?;
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

    pub fn run(&mut self, chan: Arc<ConsoleChannel>) -> anyhow::Result<()> {
        let mut input = String::with_capacity(64);
        let stdin = io::stdin();
        let lock = io::stdout();
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

            self.draw(lock.lock())?;

            if let Some(req) = self.vconsole.current_req.as_ref() {
                let size = stdin.read_line(&mut input)?;

                let s = input[..size].trim_end_matches(&['\r', '\n']);

                match req.ty {
                    InputRequestType::Int => match s.trim().parse() {
                        Ok(i) => {
                            chan.send_input(Value::Int(i), req.generation);
                            self.vconsole.current_req = None;
                        }
                        Err(_) => {}
                    },
                    InputRequestType::Str => {
                        chan.send_input(Value::String(s.into()), req.generation);
                        self.vconsole.current_req = None;
                    }
                    InputRequestType::AnyKey | InputRequestType::EnterKey => {
                        chan.send_input(Value::Int(0), req.generation);
                        self.vconsole.current_req = None;
                    }
                }

                input.clear();
            }
        }
    }
}

fn paint<'a>(
    color: erars_ui::Color,
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
