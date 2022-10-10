use crossbeam_channel::{bounded, Receiver, Sender, TrySendError};
use erars_ast::{Alignment, Value};
use once_cell::sync::Lazy;
use pad::PadStr;
use parking_lot::Mutex;
use regex::Regex;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::collections::VecDeque;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::{AtomicBool, Ordering::SeqCst};
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextStyle {
    pub color: Color,
    #[serde(skip_serializing_if = "<str>::is_empty")]
    pub font_family: SmolStr,
    #[serde(skip_serializing_if = "FontStyle::is_empty")]
    pub font_style: FontStyle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Color(pub [u8; 3]);

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConsoleLinePart {
    Text(String, TextStyle),
    Line(String, TextStyle),
    Button(Vec<(String, TextStyle)>, Value),
}

impl ConsoleLinePart {
    fn as_text(&self) -> &str {
        match self {
            Self::Text(t, _) => t.as_str(),
            _ => unreachable!(),
        }
    }

    fn into_text(self) -> (String, TextStyle) {
        match self {
            Self::Text(t, s) => (t, s),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConsoleLine {
    #[serde(skip_serializing_if = "is_left_alignment")]
    pub align: Alignment,
    #[serde(skip)]
    pub button_start: Option<usize>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub parts: Vec<ConsoleLinePart>,
}

impl ConsoleLine {
    fn push_button_merge(&mut self, text: String, style: TextStyle, value: Value) {
        let len = self
            .parts
            .iter()
            .rev()
            .take_while(|part| matches!(part, ConsoleLinePart::Text(..)))
            .count();
        let mut parts = if len == 0 {
            Vec::new()
        } else {
            let from = self.parts.len() - len;
            self.parts.drain(from..).map(ConsoleLinePart::into_text).collect()
        };
        parts.push((text, style));
        self.parts.push(ConsoleLinePart::Button(parts, value));
    }
    pub fn push_text(&mut self, text: String, style: &TextStyle) {
        static BUTTON_REGEX: Lazy<Regex> =
            Lazy::new(|| Regex::new(r#"[^\[]*\[ *(\d+) *\][^\[\]]*"#).unwrap());

        if text.contains(']') {
            match self.button_start.take() {
                Some(prev_btn_part) => {
                    let mut btn_buf = String::new();

                    for part in self.parts[prev_btn_part..].iter() {
                        btn_buf.push_str(part.as_text());
                    }

                    btn_buf.push_str(&text);

                    if BUTTON_REGEX.is_match(&btn_buf) {
                        drop(self.parts.drain(prev_btn_part..));
                        // TODO: respect styles
                        for captures in BUTTON_REGEX.captures_iter(&btn_buf) {
                            let num: i64 = captures.get(1).unwrap().as_str().parse().unwrap();
                            self.push_button_merge(
                                captures.get(0).unwrap().as_str().to_string(),
                                style.clone(),
                                Value::Int(num),
                            );
                        }
                        return;
                    }
                }
                None => match BUTTON_REGEX.is_match(&text) {
                    true => {
                        for captures in BUTTON_REGEX.captures_iter(&text) {
                            let num: i64 = captures.get(1).unwrap().as_str().parse().unwrap();
                            self.push_button_merge(
                                captures.get(0).unwrap().as_str().to_string(),
                                style.clone(),
                                Value::Int(num),
                            );
                        }
                        return;
                    }
                    false => {}
                },
            }
        }

        let has_lb = text.find('[');

        match self.parts.last_mut() {
            Some(ConsoleLinePart::Text(prev_text, prev_style)) if *prev_style == *style => {
                prev_text.push_str(&text);
            }
            Some(ConsoleLinePart::Button(parts, _)) => {
                if let Some(pos) = has_lb {
                    let (left, right) = text.split_at(pos);
                    if parts.last().unwrap().1 == *style {
                        parts.last_mut().unwrap().0.push_str(left);
                    } else {
                        parts.push((left.into(), style.clone()));
                    }
                    self.parts.push(ConsoleLinePart::Text(right.into(), style.clone()));
                } else {
                    if parts.last().unwrap().1 == *style {
                        parts.last_mut().unwrap().0.push_str(&text);
                    } else {
                        parts.push((text, style.clone()));
                    }
                }
            }
            _ => {
                self.parts.push(ConsoleLinePart::Text(text, style.clone()));
            }
        }

        if has_lb.is_some() {
            self.button_start = Some(self.parts.len() - 1);
        }
    }
}

/// Used by ui backend
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VirtualConsole {
    pub current_req: Option<InputRequest>,
    pub timeout: Option<(Instant, u32, Value)>,
    pub lines: Vec<ConsoleLine>,
    pub style: TextStyle,
    pub bg_color: Color,
    pub hl_color: Color,
}

impl VirtualConsole {
    pub fn new() -> Self {
        Self {
            current_req: None,
            timeout: None,
            lines: Vec::new(),
            style: TextStyle {
                color: Color([255, 255, 255]),
                font_family: "".into(),
                font_style: FontStyle::NORMAL,
            },
            bg_color: Color([0, 0, 0]),
            hl_color: Color([255, 255, 0]),
        }
    }

    pub fn lines(&self) -> &[ConsoleLine] {
        &self.lines
    }

    pub fn push_msg(&mut self, com: ConsoleMessage) {
        if self.lines.is_empty() {
            self.lines.push(ConsoleLine::default());
        }
        match com {
            ConsoleMessage::Input(req) => {
                if let Some(timeout) = req.timeout.as_ref() {
                    self.timeout = Some((
                        Instant::now()
                            + Duration::from_nanos(
                                (timeout.timeout
                                    - time::OffsetDateTime::now_utc().unix_timestamp_nanos())
                                    as _,
                            ),
                        req.generation,
                        timeout.default_value.clone(),
                    ));
                }
                self.current_req = Some(req);
            }
            ConsoleMessage::Alignment(align) => self.lines.last_mut().unwrap().align = align,
            ConsoleMessage::DrawLine(text) => {
                self.lines
                    .last_mut()
                    .unwrap()
                    .parts
                    .push(ConsoleLinePart::Line(text, self.style.clone()));
                self.lines.push(ConsoleLine::default());
            }
            ConsoleMessage::ClearLine(c) => {
                self.lines.truncate(self.lines.len().saturating_sub(c));
            }
            ConsoleMessage::Print(text) => {
                self.lines.last_mut().unwrap().push_text(text, &self.style);
            }
            ConsoleMessage::PrintButton(value, text) => {
                let line = &mut self.lines.last_mut().unwrap();
                line.button_start = None;

                line.parts.push(ConsoleLinePart::Button(
                    vec![(text, self.style.clone())],
                    value,
                ));
            }
            ConsoleMessage::ReuseLastLine(text) => {
                let parts = &mut self.lines.last_mut().unwrap().parts;

                parts.clear();
                parts.push(ConsoleLinePart::Text(text, self.style.clone()));
            }
            ConsoleMessage::NewLine => {
                self.lines.push(ConsoleLine::default());
            }
            ConsoleMessage::SetColor(color) => {
                self.style.color = color;
            }
            ConsoleMessage::SetBgColor(color) => {
                self.bg_color = color;
            }
            ConsoleMessage::SetHlColor(color) => {
                self.hl_color = color;
            }
            ConsoleMessage::SetFont(family) => {
                self.style.font_family = family.into();
            }
            ConsoleMessage::SetStyle(font_style) => {
                self.style.font_style = font_style;
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConsoleMessage {
    Print(String),
    NewLine,
    DrawLine(String),
    ClearLine(usize),
    PrintButton(Value, String),
    ReuseLastLine(String),
    Alignment(Alignment),
    Input(InputRequest),

    SetFont(String),
    SetStyle(FontStyle),
    SetColor(Color),
    SetBgColor(Color),
    SetHlColor(Color),
}

bitflags::bitflags! {
    #[derive(Serialize, Deserialize)]
    pub struct FontStyle: u32 {
        const NORMAL = 0x0;
        const BOLD = 0x1;
        const ITALIC = 0x2;
        const STRIKELINE = 0x4;
        const UNDERLINE = 0x8;
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum InputRequestType {
    AnyKey,
    EnterKey,
    Int,
    Str,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// input timeout
pub struct Timeout {
    /// Unix timestamp in nanos
    pub timeout: i128,
    #[serde(skip)]
    pub default_value: Value,
    pub timeout_msg: Option<String>,
    pub show_timer: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct InputRequest {
    /// InputRequest generation
    pub generation: u32,
    /// type of request
    pub ty: InputRequestType,
    /// whether is ONEINPUT or not
    pub is_one: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// TINPUT
    pub timeout: Option<Timeout>,
}

impl InputRequest {
    pub fn normal(gen: u32, ty: InputRequestType) -> Self {
        Self {
            generation: gen,
            ty,
            is_one: false,
            timeout: None,
        }
    }

    pub fn oneinput(gen: u32, ty: InputRequestType) -> Self {
        Self {
            generation: gen,
            ty,
            is_one: true,
            timeout: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConsoleResult {
    Quit,
    Value(Value),
}

pub struct ConsoleSender {
    chan: Arc<ConsoleChannel>,
    printc_width: usize,
    line_count: usize,
    skipdisp: bool,
    line_is_empty: bool,
    color: u32,
    hl_color: u32,
    bg_color: u32,
    align: Alignment,
    style: FontStyle,
    font: String,
    inputs: VecDeque<Value>,
}

impl ConsoleSender {
    pub fn new(chan: Arc<ConsoleChannel>, printc_width: usize) -> Self {
        Self {
            chan,

            skipdisp: false,
            line_is_empty: true,
            line_count: 1,
            printc_width,
            color: u32::from_le_bytes([0xFF, 0xFF, 0xFF, 0x00]),
            hl_color: u32::from_le_bytes([0xFF, 0xFF, 0x00, 0x00]),
            bg_color: u32::from_le_bytes([0x00, 0x00, 0x00, 0x00]),
            align: Alignment::Left,
            font: "".into(),
            style: FontStyle::NORMAL,
            inputs: VecDeque::new(),
        }
    }

    pub fn set_skipdisp(&mut self, skipdisp: bool) {
        self.skipdisp = skipdisp;
    }

    pub fn skipdisp(&self) -> bool {
        self.skipdisp
    }

    pub fn push_input(&mut self, value: Value) {
        self.inputs.push_back(value);
    }

    /// Request Int input
    ///
    /// If response is String, try again
    ///
    /// Return `None` if console send `Quit`
    pub fn input_int(&mut self) -> Option<i64> {
        loop {
            let gen = self.input_gen();
            match self.input(InputRequest {
                generation: gen,
                ty: InputRequestType::Int,
                is_one: false,
                timeout: None,
            }) {
                ConsoleResult::Quit => break None,
                ConsoleResult::Value(Value::Int(i)) => break Some(i),
                ConsoleResult::Value(Value::String(_)) => continue,
            }
        }
    }

    pub fn input_gen(&self) -> u32 {
        self.chan.input_gen()
    }

    pub fn input(&mut self, req: InputRequest) -> ConsoleResult {
        if self.skipdisp() {
            log::error!("Can't INPUT while SKIPDISP is on.");
            self.print_line("Can't INPUT while SKIPDISP is on.".into());
            self.request_redraw();
            self.exit();
            return ConsoleResult::Quit;
        }

        self.request_redraw();

        if matches!(
            req.ty,
            InputRequestType::AnyKey | InputRequestType::EnterKey
        ) && !self.inputs.is_empty()
        {
            ConsoleResult::Value(0i64.into())
        } else if let Some(i) = self.inputs.pop_front() {
            ConsoleResult::Value(i)
        } else {
            self.chan.send_msg(ConsoleMessage::Input(req));
            let ret = self.chan.recv_ret();
            log::trace!("Console Recv {ret:?}");
            ret
        }
    }

    pub fn line_count(&self) -> usize {
        self.line_count
    }

    pub fn line_is_empty(&self) -> bool {
        self.line_is_empty
    }

    pub fn reuse_last_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.line_is_empty = false;
        self.chan.send_msg(ConsoleMessage::ReuseLastLine(s));
    }

    pub fn print(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.line_is_empty = false;
        self.chan.send_msg(ConsoleMessage::Print(s));
    }

    pub fn print_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.print(s);
        self.new_line();
    }

    pub fn printlc(&mut self, s: &str) {
        self.print(s.pad_to_width_with_alignment(self.printc_width, pad::Alignment::Left));
    }

    pub fn printrc(&mut self, s: &str) {
        self.print(s.pad_to_width_with_alignment(self.printc_width, pad::Alignment::Right));
    }

    pub fn new_line(&mut self) {
        if self.skipdisp {
            return;
        }
        self.line_count += 1;
        self.line_is_empty = true;
        self.chan.send_msg(ConsoleMessage::NewLine);
    }

    pub fn draw_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.line_count += 1;
        self.line_is_empty = true;
        self.chan.send_msg(ConsoleMessage::DrawLine(s));
    }

    pub fn clear_line(&mut self, c: usize) {
        self.line_count = self.line_count.saturating_sub(c).min(1);
        self.line_is_empty = false;
        self.chan.send_msg(ConsoleMessage::ClearLine(c));
    }

    pub fn set_color(&mut self, r: u8, g: u8, b: u8) {
        self.color = u32::from_le_bytes([r, g, b, 0]);
        self.chan.send_msg(ConsoleMessage::SetColor(Color([r, g, b])));
    }

    pub fn set_bg_color(&mut self, r: u8, g: u8, b: u8) {
        self.bg_color = u32::from_le_bytes([r, g, b, 0]);
        self.chan.send_msg(ConsoleMessage::SetBgColor(Color([r, g, b])));
    }

    pub fn set_hl_color(&mut self, r: u8, g: u8, b: u8) {
        self.hl_color = u32::from_le_bytes([r, g, b, 0]);
        self.chan.send_msg(ConsoleMessage::SetHlColor(Color([r, g, b])));
    }

    pub fn set_align(&mut self, align: Alignment) {
        self.align = align;
        self.chan.send_msg(ConsoleMessage::Alignment(align));
    }

    pub fn set_style(&mut self, style: FontStyle) {
        self.style = style;
        self.chan.send_msg(ConsoleMessage::SetStyle(style));
    }

    pub fn set_font(&mut self, font: String) {
        self.font = font.clone();
        self.chan.send_msg(ConsoleMessage::SetFont(font));
    }

    pub fn exit(&self) {
        self.chan.exit();
    }

    pub fn align(&self) -> Alignment {
        self.align
    }

    pub fn color(&self) -> u32 {
        self.color
    }

    pub fn hl_color(&self) -> u32 {
        self.hl_color
    }

    pub fn bg_color(&self) -> u32 {
        self.bg_color
    }

    pub fn font(&self) -> &str {
        &self.font
    }

    pub fn style(&self) -> FontStyle {
        self.style
    }

    pub fn request_redraw(&self) {
        self.chan.request_redraw();
    }

    pub fn into_chan(self) -> Arc<ConsoleChannel> {
        self.chan
    }
}

pub struct ConsoleChannel {
    redraw_fn: Mutex<Option<Box<dyn Fn() + Send + Sync>>>,
    exit_fn: Mutex<Option<Box<dyn Fn() + Send + Sync>>>,
    delay_redraw: AtomicBool,
    delay_exit: AtomicBool,
    input_generation: AtomicU32,
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
            input_generation: AtomicU32::new(0),
            console: bounded(64),
            ret: bounded(8),
        }
    }

    pub fn input_gen(&self) -> u32 {
        self.input_generation.load(SeqCst)
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
        match self.exit_fn.lock().as_deref() {
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
        match self.console.0.try_send(msg) {
            Ok(()) => {}
            Err(TrySendError::Full(msg)) => {
                // if draw queue is full, request frontend to flush the queue.
                self.request_redraw();
                // block until queue is available
                self.console.0.send(msg).unwrap();
            }
            _ => unreachable!(),
        }
    }

    pub fn recv_msg(&self) -> Option<ConsoleMessage> {
        self.console.1.recv_timeout(Duration::from_millis(50)).ok()
    }

    pub fn send_quit(&self) {
        self.ret.0.send(ConsoleResult::Quit).unwrap()
    }

    pub fn send_input(&self, input: Value, gen: u32) -> bool {
        if self
            .input_generation
            .compare_exchange(gen, gen.wrapping_add(1), SeqCst, SeqCst)
            .is_ok()
        {
            self.ret.0.send(ConsoleResult::Value(input)).unwrap();
            true
        } else {
            false
        }
    }

    pub fn recv_ret(&self) -> ConsoleResult {
        self.ret.1.recv().unwrap()
    }
}

fn is_left_alignment(align: &Alignment) -> bool {
    *align == Alignment::Left
}

#[test]
fn button_test() {
    let mut line = ConsoleLine::default();
    line.push_text(
        "[0] 1 [1] 2 [ 3] 3 [456 ] 745".into(),
        &TextStyle {
            color: Color([0; 3]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        },
    );
    k9::assert_equal!(line.parts.len(), 4);
    k9::snapshot!(
        &line.parts,
        r#"
[
    Button(
        [
            (
                "[0] 1 ",
                TextStyle {
                    color: Color(
                        [
                            0,
                            0,
                            0,
                        ],
                    ),
                    font_family: "",
                    font_style: NORMAL,
                },
            ),
        ],
        Int(
            0,
        ),
    ),
    Button(
        [
            (
                "[1] 2 ",
                TextStyle {
                    color: Color(
                        [
                            0,
                            0,
                            0,
                        ],
                    ),
                    font_family: "",
                    font_style: NORMAL,
                },
            ),
        ],
        Int(
            1,
        ),
    ),
    Button(
        [
            (
                "[ 3] 3 ",
                TextStyle {
                    color: Color(
                        [
                            0,
                            0,
                            0,
                        ],
                    ),
                    font_family: "",
                    font_style: NORMAL,
                },
            ),
        ],
        Int(
            3,
        ),
    ),
    Button(
        [
            (
                "[456 ] 745",
                TextStyle {
                    color: Color(
                        [
                            0,
                            0,
                            0,
                        ],
                    ),
                    font_family: "",
                    font_style: NORMAL,
                },
            ),
        ],
        Int(
            456,
        ),
    ),
]
"#
    );

    line = ConsoleLine::default();

    line.push_text(
        ">".into(),
        &TextStyle {
            color: Color([0; 3]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        },
    );

    line.push_text(
        "[ 9]".into(),
        &TextStyle {
            color: Color([0; 3]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        },
    );

    line.push_text(
        "2022年10月08日 22:52:52 1일째 [낮".into(),
        &TextStyle {
            color: Color([0; 3]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        },
    );
    k9::assert_equal!(line.parts.len(), 2);
    k9::snapshot!(
        &line.parts,
        r#"
[
    Button(
        [
            (
                ">",
                TextStyle {
                    color: Color(
                        [
                            0,
                            0,
                            0,
                        ],
                    ),
                    font_family: "",
                    font_style: NORMAL,
                },
            ),
            (
                "[ 9]2022年10月08日 22:52:52 1일째 ",
                TextStyle {
                    color: Color(
                        [
                            0,
                            0,
                            0,
                        ],
                    ),
                    font_family: "",
                    font_style: NORMAL,
                },
            ),
        ],
        Int(
            9,
        ),
    ),
    Text(
        "[낮",
        TextStyle {
            color: Color(
                [
                    0,
                    0,
                    0,
                ],
            ),
            font_family: "",
            font_style: NORMAL,
        },
    ),
]
"#
    );
}
