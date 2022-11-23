use erars_ast::Value;
use erars_ui::{ConsoleLinePart, FontStyle, InputRequest, InputRequestType, VirtualConsole};
use erars_vm::{
    SaveList, SerializableGlobalVariableStorage, SerializableVariableStorage, SystemFunctions,
};
use std::{
    collections::VecDeque,
    io::{self, Write},
    path::PathBuf,
};

pub struct StdioFrontend {
    sav_path: PathBuf,
    from: usize,
    input: String,
    json: bool,
    inputs: VecDeque<Value>,
}

impl StdioFrontend {
    pub fn new(sav_path: PathBuf, json: bool, inputs: VecDeque<Value>) -> Self {
        Self {
            sav_path,
            from: 0,
            input: String::new(),
            json,
            inputs,
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
}

#[async_trait::async_trait(?Send)]
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
            }
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        if !vconsole.need_rebuild && self.from == vconsole.line_count() && vconsole.line_is_empty()
        {
            // skip redraw
            return Ok(());
        }
        self.draw(vconsole, &mut io::stdout().lock())
    }
    fn load_local_list(&mut self) -> anyhow::Result<SaveList> {
        erars_saveload_fs::load_local_list(&self.sav_path)
    }
    fn load_local(
        &mut self,
        idx: u32,
    ) -> anyhow::Result<Option<SerializableVariableStorage>> {
        erars_saveload_fs::read_save_data(&self.sav_path, idx)
    }
    fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>> {
        erars_saveload_fs::read_global_data(&self.sav_path)
    }
    fn save_local(
        &mut self,
        idx: u32,
        sav: SerializableVariableStorage,
    ) -> anyhow::Result<()> {
        erars_saveload_fs::write_save_data(&self.sav_path, idx, &sav)
    }
    fn remove_local(&mut self, idx: u32) -> anyhow::Result<()> {
        erars_saveload_fs::delete_save_data(&self.sav_path, idx)
    }
    fn save_global(&mut self, sav: SerializableGlobalVariableStorage) -> anyhow::Result<()> {
        erars_saveload_fs::write_global_data(&self.sav_path, &sav)
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
