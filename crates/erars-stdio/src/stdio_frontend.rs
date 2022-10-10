use erars_ui::{ConsoleLinePart, FontStyle, InputRequestType, VirtualConsole};
use erars_vm::{SystemState, TerminalVm, VmContext, VmResult};
use std::{
    io,
    sync::{
        atomic::{AtomicBool, Ordering::SeqCst},
        Arc,
    },
};

pub struct StdioFrontend {
    vconsole: VirtualConsole,
    need_redraw: bool,
}

impl StdioFrontend {
    pub fn new(printc_width: usize) -> Self {
        Self {
            vconsole: VirtualConsole::new(printc_width),
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

    pub fn run(&mut self, vm: &TerminalVm, ctx: &mut VmContext) -> anyhow::Result<()> {
        let mut input = String::with_capacity(64);
        let stdin = io::stdin();
        let lock = io::stdout();

        loop {
            match vm.run_state(&mut state, &mut self.vconsole, ctx)? {
                VmResult::Exit => break Ok(()),
                VmResult::NeedInput { req } => {
                    self.draw(lock.lock())?;

                    loop {
                        let size = stdin.read_line(&mut input)?;

                        let s = input[..size].trim_end_matches(&['\r', '\n']);

                        match req.ty {
                            InputRequestType::Int => match s.trim().parse::<i64>() {
                                Ok(i) => {
                                    ctx.push(i);
                                    break;
                                }
                                Err(_) => {
                                    continue;
                                }
                            },
                            InputRequestType::Str => {
                                ctx.push(s);
                            }
                            InputRequestType::AnyKey | InputRequestType::EnterKey => {}
                        }
                    }
                }
                VmResult::Redraw => {
                    self.draw(lock.lock())?;
                }
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
