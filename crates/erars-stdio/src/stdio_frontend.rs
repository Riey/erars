use erars_ui::{ConsoleLinePart, FontStyle, InputRequestType, VirtualConsole};
use erars_vm::{TerminalVm, VmContext, VmResult};
use std::io::{self, BufRead};

pub struct StdioFrontend {
    vconsole: VirtualConsole,
}

impl StdioFrontend {
    pub fn new(vconsole: VirtualConsole) -> Self {
        Self { vconsole }
    }

    fn draw(&mut self, mut out: impl io::Write) -> anyhow::Result<()> {
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
        out.flush()?;

        Ok(())
    }

    pub fn run(&mut self, vm: &TerminalVm, ctx: &mut VmContext) -> anyhow::Result<()> {
        let mut input = String::with_capacity(64);
        let mut stdin = io::stdin().lock();
        let mut lock = io::stdout().lock();

        loop {
            match vm.run_state(&mut self.vconsole, ctx) {
                VmResult::Exit => break Ok(()),
                VmResult::NeedInput { req, set_result } => {
                    self.draw(&mut lock)?;

                    loop {
                        let size = stdin.read_line(&mut input)?;

                        let s = input[..size].trim_end_matches(&['\r', '\n']);

                        match req.ty {
                            InputRequestType::Int => match s.trim().parse::<i64>() {
                                Ok(i) => {
                                    log::info!("[stdio] <- {i}");
                                    if set_result {
                                        ctx.var.set_result(i);
                                    } else {
                                        ctx.push(i);
                                    }
                                    break;
                                }
                                Err(_) => {
                                    continue;
                                }
                            },
                            InputRequestType::Str => {
                                log::info!("[stdio] <- \"{s}\"");
                                if set_result {
                                    ctx.var.set_results(s.into());
                                } else {
                                    ctx.push(s);
                                }
                                break;
                            }
                            InputRequestType::AnyKey | InputRequestType::EnterKey => {
                                log::info!("[stdio] <- \"\"");
                                break;
                            }
                        }
                    }

                    input.clear();
                }
                VmResult::Redraw => {
                    self.draw(&mut lock)?;
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
