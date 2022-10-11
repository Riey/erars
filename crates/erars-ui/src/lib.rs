use erars_ast::{Alignment, Value};
use once_cell::sync::Lazy;
use pad::PadStr;
use regex::Regex;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::collections::VecDeque;
use std::fmt::{Debug, Display};
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

impl From<Color> for u32 {
    fn from(Color([r, g, b]): Color) -> Self {
        u32::from_le_bytes([r, g, b, 0])
    }
}

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

impl Display for ConsoleLinePart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text(arg0, _) => write!(f, "{arg0}"),
            Self::Line(arg0, _) => write!(f, "{arg0}"),
            Self::Button(arg0, _) => {
                for (text, _) in arg0 {
                    write!(f, "{text}")?;
                }

                Ok(())
            }
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ConsoleLine {
    #[serde(skip_serializing_if = "is_left_alignment")]
    pub align: Alignment,
    #[serde(skip)]
    pub button_start: Option<usize>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub parts: Vec<ConsoleLinePart>,
}

impl Debug for ConsoleLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.parts.iter()).finish()
    }
}

impl Display for ConsoleLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for part in self.parts.iter() {
            write!(f, "{}", part)?;
        }

        Ok(())
    }
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
    pub timeout: Option<(Instant, u32, Value)>,
    pub lines: Vec<ConsoleLine>,
    pub style: TextStyle,
    pub bg_color: Color,
    pub hl_color: Color,
    pub printc_width: usize,
    pub skipdisp: bool,
    pub inputs: VecDeque<Value>,
    pub input_gen: u32,
}

impl VirtualConsole {
    pub fn new(printc_width: usize) -> Self {
        Self {
            input_gen: 0,
            timeout: None,
            printc_width,
            lines: Vec::new(),
            style: TextStyle {
                color: Color([255, 255, 255]),
                font_family: "".into(),
                font_style: FontStyle::NORMAL,
            },
            bg_color: Color([0, 0, 0]),
            hl_color: Color([255, 255, 0]),
            skipdisp: false,
            inputs: VecDeque::new(),
        }
    }

    pub fn lines(&self) -> &[ConsoleLine] {
        &self.lines
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

    pub fn input_gen(&mut self) -> u32 {
        let ret = self.input_gen;
        self.input_gen += 1;
        ret
    }

    pub fn line_count(&self) -> usize {
        match self.lines.last() {
            None => 0,
            Some(line) => self.lines.len() - line.parts.is_empty() as usize,
        }
    }

    pub fn line_is_empty(&self) -> bool {
        self.lines.last().map_or(true, |l| l.parts.is_empty())
    }

    pub fn reuse_last_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        if self.lines.is_empty() {
            self.lines.push(ConsoleLine::default());
        }
        let parts = &mut self.lines.last_mut().unwrap().parts;

        parts.clear();
        parts.push(ConsoleLinePart::Text(s, self.style.clone()));
    }

    pub fn print(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        if self.lines.is_empty() {
            self.lines.push(ConsoleLine::default());
        }
        self.lines.last_mut().unwrap().push_text(s, &self.style);
    }

    pub fn print_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.print(s);
        self.new_line();
    }

    pub fn print_button(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        if self.lines.is_empty() {
            self.lines.push(ConsoleLine::default());
        }
        let line = &mut self.lines.last_mut().unwrap();
        line.button_start = None;

        line.parts.push(ConsoleLinePart::Button(
            vec![(text, self.style.clone())],
            value,
        ));
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
        self.lines.push(ConsoleLine::default());
    }

    pub fn draw_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        if self.lines.is_empty() {
            self.lines.push(ConsoleLine::default());
        }
        self.lines
            .last_mut()
            .unwrap()
            .parts
            .push(ConsoleLinePart::Line(s, self.style.clone()));
        self.lines.push(ConsoleLine::default());
    }

    pub fn clear_line(&mut self, c: usize) {
        self.lines.truncate(c);
    }

    pub fn set_color(&mut self, r: u8, g: u8, b: u8) {
        self.style.color = Color([r, g, b]);
    }

    pub fn set_bg_color(&mut self, r: u8, g: u8, b: u8) {
        self.bg_color = Color([r, g, b]);
    }

    pub fn set_hl_color(&mut self, r: u8, g: u8, b: u8) {
        self.hl_color = Color([r, g, b]);
    }

    pub fn set_align(&mut self, align: Alignment) {
        self.lines.last_mut().unwrap().align = align;
    }

    pub fn set_style(&mut self, style: FontStyle) {
        self.style.font_style = style;
    }

    pub fn set_font(&mut self, font: String) {
        self.style.font_family = font.into();
    }

    pub fn align(&self) -> Alignment {
        self.lines.last().unwrap().align
    }

    pub fn color(&self) -> u32 {
        self.style.color.into()
    }

    pub fn hl_color(&self) -> u32 {
        self.hl_color.into()
    }

    pub fn bg_color(&self) -> u32 {
        self.bg_color.into()
    }

    pub fn font(&self) -> &str {
        &self.style.font_family
    }

    pub fn style(&self) -> FontStyle {
        self.style.font_style
    }
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
