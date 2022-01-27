use crate::{compiler::Alignment, value::Value};
use crossbeam_channel::{bounded, Receiver, Sender};
use eframe::epi::{App, Frame};
use egui::{Color32, CtxRef, FontData, FontDefinitions, FontFamily};
use maplit::btreemap;
use std::iter;
use std::sync::Arc;
use std::time::Duration;

mod button_parser;

const FONT: &[u8] = include_bytes!("../res/D2Coding-Ver1.3.2-20180524.ttc");

pub struct EraApp {
    console: EraConsole,
    req: Option<InputRequest>,
    chan: Arc<ConsoleChannel>,
}

impl EraApp {
    pub fn new(chan: Arc<ConsoleChannel>) -> Self {
        Self {
            console: EraConsole::default(),
            req: None,
            chan,
        }
    }
}

impl App for EraApp {
    fn setup(&mut self, ctx: &CtxRef, _frame: &Frame, _storage: Option<&dyn eframe::epi::Storage>) {
        ctx.set_fonts(FontDefinitions {
            font_data: btreemap! {
                "default".into() => FontData::from_static(FONT),
            },
            fonts_for_family: btreemap! {
                FontFamily::Monospace => vec!["default".into()],
                FontFamily::Proportional => vec!["default".into()],
            },
            ..Default::default()
        });

        let mut style = (*ctx.style()).clone();
        style.visuals.override_text_color = Some(Color32::WHITE);
        ctx.set_style(style);
    }

    fn name(&self) -> &str {
        "erars"
    }

    fn update(&mut self, ctx: &CtxRef, frame: &Frame) {
        if self.req.is_none() {
            while let Some(msg) = self.chan.recv_msg() {
                match msg {
                    ConsoleMessage::Exit => {
                        frame.quit();
                        return;
                    }
                    ConsoleMessage::Input(req) => self.req = Some(req),
                    ConsoleMessage::NewLine => self.console.new_line(),
                    ConsoleMessage::Print(s) => self.console.print(s),
                    ConsoleMessage::DrawLine => self.console.draw_line(),
                    ConsoleMessage::Alignment(align) => self.console.set_align(align),
                    ConsoleMessage::PrintButton(value, s) => self.console.print_button(value, s),
                    ConsoleMessage::ReuseLastLine(s) => self.console.reuse_last_line(s),
                }
            }
        }

        egui::CentralPanel::default().show(ctx, |ui| {
            let mut ret = |value: &Value| match (&self.req, value) {
                (Some(InputRequest::Anykey), _)
                | (Some(InputRequest::EnterKey), _)
                | (Some(InputRequest::Int), Value::Int(_))
                | (Some(InputRequest::Str), Value::String(_)) => {
                    self.req = None;
                    self.chan.send_ret(ConsoleResult::Value(value.clone()));
                }
                _ => {}
            };
            for line in self
                .console
                .lines
                .iter()
                .chain(iter::once(&self.console.last_line))
            {
                match line.align {
                    Alignment::Left => {
                        ui.horizontal(|ui| {
                            for part in line.parts.iter() {
                                part.display(ui, &mut ret);
                            }
                        });
                    }
                    Alignment::Center => {
                        ui.vertical_centered(|ui| {
                            for part in line.parts.iter() {
                                part.display(ui, &mut ret);
                            }
                        });
                    }
                    Alignment::Right => todo!(),
                }
            }
        });
    }
}

#[derive(Default)]
struct EraConsole {
    lines: Vec<ConsoleLine>,
    last_line: ConsoleLine,
}

#[derive(Default)]
struct ConsoleLine {
    parts: Vec<ConsoleLinePart>,
    align: Alignment,
    reuse: bool,
}

enum ConsoleLinePart {
    Normal(String),
    Line,
    Button(Value, String),
}

impl ConsoleLinePart {
    pub fn display(&self, ui: &mut egui::Ui, ret: impl FnOnce(&Value)) {
        match self {
            ConsoleLinePart::Normal(s) => {
                ui.label(s);
            }
            ConsoleLinePart::Line => {
                let width = ui.available_width();
                let char_width = ui.fonts().glyph_width(egui::TextStyle::Monospace, '=');
                let s = "=".repeat((width / char_width) as usize);
                ui.label(s);
            }
            ConsoleLinePart::Button(value, text) => {
                if ui.small_button(text).clicked() {
                    ret(value);
                }
            }
        }
    }
}

impl EraConsole {
    pub fn print_button(&mut self, value: Value, s: String) {
        self.last_line.parts.push(ConsoleLinePart::Button(value, s));
    }

    // pub fn print_plain_text(&mut self, s: String) {
    //     self.last_line.parts.push(ConsoleLinePart::Normal(s));
    // }

    pub fn print(&mut self, s: String) {
        button_parser::parse_button(s, &mut self.last_line.parts);
    }
    pub fn new_line(&mut self) {
        let prev_line = std::mem::take(&mut self.last_line);
        let prev_align = prev_line.align;
        self.lines.push(prev_line);
        self.last_line.align = prev_align;
    }
    pub fn set_align(&mut self, align: Alignment) {
        self.last_line.align = align;
    }

    pub fn draw_line(&mut self) {
        self.last_line.parts.push(ConsoleLinePart::Line);
        self.new_line();
    }

    pub fn reuse_last_line(&mut self, s: String) {
        if !self.last_line.reuse {
            self.new_line();
            self.print(s);
            self.last_line.reuse = true;
        } else {
            self.last_line.parts.clear();
            self.print(s);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConsoleMessage {
    Print(String),
    NewLine,
    DrawLine,
    PrintButton(Value, String),
    ReuseLastLine(String),
    Alignment(Alignment),
    Input(InputRequest),
    Exit,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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

pub struct ConsoleChannel {
    console: (Sender<ConsoleMessage>, Receiver<ConsoleMessage>),
    ret: (Sender<ConsoleResult>, Receiver<ConsoleResult>),
}

impl ConsoleChannel {
    pub fn new() -> Self {
        Self {
            console: bounded(256),
            ret: bounded(8),
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
