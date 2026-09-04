use erars_ast::Value;
use erars_ui::{
    win32, ConsoleLinePart, FontStyle, InputRequest, InputRequestType, MouseKeyEvent,
    VirtualConsole,
};
use erars_vm::SystemFunctions;
use std::{
    collections::VecDeque,
    io::{self, Write},
};

pub struct StdioFrontend {
    from: usize,
    input: String,
    json: bool,
    inputs: VecDeque<Value>,
    /// A `\e` was seen in the line just read; the console turns it into
    /// message skip.
    mes_skip: bool,
}

impl StdioFrontend {
    pub fn new(json: bool, inputs: VecDeque<Value>) -> Self {
        Self {
            from: 0,
            input: String::new(),
            json,
            inputs,
            mes_skip: false,
        }
    }

    fn draw(
        &mut self,
        vconsole: &mut VirtualConsole,
        mut out: impl io::Write,
    ) -> anyhow::Result<()> {
        if !self.json {
            return self.draw_stdio(out, vconsole);
        }

        if vconsole.need_rebuild {
            self.from = vconsole.top_index;
        }

        let ret = vconsole.make_serializable(self.from);

        serde_json::to_writer(&mut out, &ret)?;
        writeln!(out)?;

        self.from += ret.lines.len();
        vconsole.need_rebuild = false;

        out.flush()?;

        Ok(())
    }

    fn draw_stdio(
        &mut self,
        mut out: impl io::Write,
        vconsole: &mut VirtualConsole,
    ) -> anyhow::Result<()> {
        // `CLEARLINE` drops lines and asks for a repaint
        // (`VirtualConsole::clear_line`). Without honouring that, `self.from`
        // stays above the shrunken line count and `lines_from` returns nothing
        // — every later line, including a VM error report, was written to a
        // console nobody printed. The JSON path below already rebases the same
        // way.
        if vconsole.need_rebuild {
            self.from = vconsole.top_index;
            vconsole.need_rebuild = false;
        }

        for line in vconsole.lines_from(self.from).iter() {
            self.from += 1;
            for part in line.parts.iter() {
                match part {
                    ConsoleLinePart::Text(text, style) => {
                        write!(out, "{}", paint(style.color, style.font_style, &text))?;
                    }
                    ConsoleLinePart::Button(btns, _input_gen, _value) => {
                        for (text, style) in btns.iter() {
                            write!(out, "{}", paint(vconsole.hl_color, style.font_style, text))?;
                        }
                    }
                    ConsoleLinePart::Line(text, style) => {
                        // The terminal has no font metric of its own, so the
                        // rule is the console's configured bar — the same
                        // string `DRAWLINESTR` reports.
                        let bar = vconsole.bar_string(text).unwrap_or_default();
                        write!(out, "{}", paint(style.color, style.font_style, &bar))?;
                    }
                    // A terminal has no pixels. Emuera's own fallback for an
                    // image it cannot draw is the reconstructed `<img …>` tag
                    // (`GameView/ConsoleImagePart.cs:69-73`, `:214-220`), so
                    // print exactly that: a real Emuera string that occupies
                    // the width it describes rather than an invented
                    // placeholder, and never a silently dropped part.
                    ConsoleLinePart::Image(image) => {
                        let style = &vconsole.style;
                        write!(out, "{}", paint(style.color, style.font_style, &image.alt))?;
                    }
                    // A terminal has no coordinates either, so a positioned
                    // box prints as the tag Emuera itself builds for one
                    // (`_Library/EvilMask/ConsoleDivPart.cs:189-200`): the
                    // open tag with the attributes that were set, the box's
                    // own lines, and `</div>`. The box's content is therefore
                    // readable in a terminal, in the flow, which is the only
                    // place a terminal can put it.
                    ConsoleLinePart::Div(div) => {
                        let style = &vconsole.style;
                        write!(
                            out,
                            "{}",
                            paint(style.color, style.font_style, &div.alt_text())
                        )?;
                    }
                }
            }
            writeln!(out)?;
        }

        out.flush()?;

        Ok(())
    }
}

impl SystemFunctions for StdioFrontend {
    fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        if !self.inputs.is_empty() {
            if matches!(req.ty, InputRequestType::Int | InputRequestType::Str) {
                return Ok(self.inputs.pop_front());
            } else {
                return Ok(None);
            }
        }

        if self.json {
            let out = io::stdout();
            let mut out = out.lock();
            serde_json::to_writer(&mut out, &req)?;
            writeln!(out)?;
        }

        loop {
            self.input.clear();
            let size = io::stdin().read_line(&mut self.input)?;

            let s = self.input[..size].trim_end_matches(&['\r', '\n']);

            // Emuera `EmueraConsole.cs:1130`: `\e` anywhere in an entered line
            // starts message skip and is stripped from the input itself.
            let unescaped;
            let s = if s.contains("\\e") {
                self.mes_skip = true;
                unescaped = s.replace("\\e", "");
                unescaped.as_str()
            } else {
                s
            };

            // The one seam the debug console needs from a front end. Emuera
            // diverts a line starting with `@` to `doSystemCommand` before it
            // looks at the request type at all, and only a ONEINPUT-family
            // request takes the character literally
            // (`GameView/EmueraConsole.cs:1103-1110`). An `InputRequestType`
            // that does not otherwise carry text has to hand the line over
            // verbatim for `VmContext::input_redraw` to classify; it either
            // runs a command and re-issues this same request, or rejects the
            // line for us.
            if !req.is_one && s.starts_with('@') {
                log::info!("[stdio] <- system command \"{s}\"");
                break Ok(Some(Value::String(s.into())));
            }

            match req.ty {
                InputRequestType::Int => match s.trim().parse::<i64>() {
                    Ok(i) => {
                        log::info!("[stdio] <- {i}");
                        break Ok(Some(Value::Int(i)));
                    }
                    Err(_) => {
                        continue;
                    }
                },
                InputRequestType::Str => {
                    log::info!("[stdio] <- \"{s}\"");
                    break Ok(Some(Value::String(s.into())));
                }
                InputRequestType::AnyKey
                | InputRequestType::EnterKey
                | InputRequestType::ForceEnterKey => {
                    log::info!("[stdio] <- \"\"");
                    break Ok(None);
                }
                // Answered by `input_mouse_key`, which never routes here.
                InputRequestType::MouseKey => break Ok(None),
            }
        }
    }

    fn take_mes_skip(&mut self) -> bool {
        std::mem::take(&mut self.mes_skip)
    }

    /// INPUTMOUSEKEY over a terminal: there is no mouse and no raw key
    /// stream, but a typed line is a real key press, so the first character
    /// is reported as the virtual key that produces it. An empty line is
    /// Enter, and a replayed `--use-input` integer is taken as the key code
    /// itself.
    fn input_mouse_key(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
        painted: erars_vm::graphics::Painted<'_>,
    ) -> anyhow::Result<MouseKeyEvent> {
        self.redraw(vconsole, painted)?;

        let code = match self.inputs.pop_front() {
            Some(Value::Int(i)) => i,
            Some(Value::String(s)) => s.chars().next().and_then(win32::vk_from_char).unwrap_or(
                // A line was replayed, so a key was pressed; the terminal
                // just cannot name which one.
                win32::VK_RETURN,
            ),
            None => {
                if self.json {
                    let out = io::stdout();
                    let mut out = out.lock();
                    serde_json::to_writer(&mut out, &req)?;
                    writeln!(out)?;
                }
                self.input.clear();
                let size = io::stdin().read_line(&mut self.input)?;
                let s = self.input[..size].trim_end_matches(&['\r', '\n']);
                match s.chars().next() {
                    Some(c) => win32::vk_from_char(c).unwrap_or(win32::VK_RETURN),
                    None => win32::VK_RETURN,
                }
            }
        };

        log::info!("[stdio] <- key {code}");
        Ok(MouseKeyEvent {
            kind: 3,
            code,
            x: code,
            ..MouseKeyEvent::default()
        })
    }

    fn redraw(
        &mut self,
        vconsole: &mut VirtualConsole,
        _painted: erars_vm::graphics::Painted<'_>,
    ) -> anyhow::Result<()> {
        if !vconsole.need_rebuild && self.from == vconsole.line_count() && vconsole.line_is_empty()
        {
            // skip redraw
            return Ok(());
        }
        self.draw(vconsole, &mut io::stdout().lock())
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
