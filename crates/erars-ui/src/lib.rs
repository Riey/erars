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
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

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
    pub fn push_plain_text(&mut self, text: String, style: &TextStyle) {
        match self.parts.last_mut() {
            Some(ConsoleLinePart::Text(prev_text, prev_style)) if *prev_style == *style => {
                prev_text.push_str(&text);
            }
            _ => {
                self.parts.push(ConsoleLinePart::Text(text, style.clone()));
            }
        }
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

#[derive(Serialize)]
pub struct ConsoleSerde<'a> {
    pub current_req: Option<&'a InputRequest>,
    pub rebuild: bool,
    pub bg_color: Color,
    pub hl_color: Color,
    #[serde(skip_serializing_if = "ConsoleLine::is_empty")]
    pub last_line: &'a ConsoleLine,
    pub lines: LinesFrom<'a>,
}

/// Used by ui backend
#[derive(Clone, Debug)]
pub struct VirtualConsole {
    pub timeout: Option<(Instant, u32, Value)>,
    pub lines: VecDeque<ConsoleLine>,
    pub last_line: ConsoleLine,
    pub style: TextStyle,
    pub bg_color: Color,
    pub hl_color: Color,
    pub skipdisp: bool,
    pub need_rebuild: bool,
    pub input_gen: u32,

    max_log: usize,
    printc_width: usize,
    pub top_index: usize,
}

impl VirtualConsole {
    pub fn new(printc_width: usize, max_log: usize) -> Self {
        Self {
            input_gen: 0,
            timeout: None,
            printc_width,
            need_rebuild: false,
            lines: VecDeque::with_capacity(max_log),
            last_line: ConsoleLine::default(),
            max_log,
            style: TextStyle {
                color: Color([255, 255, 255]),
                font_family: "".into(),
                font_style: FontStyle::NORMAL,
            },
            bg_color: Color([0, 0, 0]),
            hl_color: Color([255, 255, 0]),
            skipdisp: false,
            top_index: 0,
        }
    }

    pub fn make_serializable<'a>(
        &'a self,
        req: Option<&'a InputRequest>,
        from: usize,
    ) -> ConsoleSerde<'a> {
        ConsoleSerde {
            current_req: req,
            rebuild: self.need_rebuild,
            bg_color: self.bg_color,
            hl_color: self.hl_color,
            last_line: &self.last_line,
            lines: self.lines_from(from),
        }
    }

    pub fn lines_from(&self, from: usize) -> LinesFrom<'_> {
        let from = from.saturating_sub(self.top_index).min(self.lines.len());
        LinesFrom { this: self, from }
    }

    pub fn set_skipdisp(&mut self, skipdisp: bool) {
        self.skipdisp = skipdisp;
    }

    pub fn skipdisp(&self) -> bool {
        self.skipdisp
    }

    pub fn input_gen(&mut self) -> u32 {
        let ret = self.input_gen;
        self.input_gen += 1;
        ret
    }

    pub fn line_count(&self) -> usize {
        self.lines.len() + !self.line_is_empty() as usize
    }

    pub fn line_is_empty(&self) -> bool {
        self.last_line.parts.is_empty()
    }

    pub fn reuse_last_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        let style = self.style.clone();
        let parts = &mut self.last_line.parts;

        parts.clear();
        parts.push(ConsoleLinePart::Text(s, style));
    }

    pub fn print_plain(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.last_line.push_plain_text(s, &self.style);
    }

    pub fn print(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.last_line.push_text(s, &self.style);
    }

    pub fn print_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.print(s);
        self.push_line();
    }

    pub fn print_button(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        let style = self.style.clone();
        self.last_line.button_start = None;
        self.last_line
            .parts
            .push(ConsoleLinePart::Button(vec![(text, style)], value));
    }

    pub fn printlc(&mut self, s: &str) {
        self.print(s.pad_to_width_with_alignment(self.printc_width, pad::Alignment::Left));
    }

    pub fn printrc(&mut self, s: &str) {
        self.print(s.pad_to_width_with_alignment(self.printc_width, pad::Alignment::Right));
    }

    fn push_line(&mut self) {
        if self.lines.len() == self.max_log {
            self.lines.pop_front();
            self.top_index += 1;
        }

        self.lines.push_back(std::mem::take(&mut self.last_line));
    }

    pub fn new_line(&mut self) {
        if self.skipdisp {
            return;
        }

        self.push_line();
    }

    pub fn draw_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        let style = self.style.clone();
        self.last_line.parts.push(ConsoleLinePart::Line(s, style));
        self.push_line();
    }

    pub fn clear_line(&mut self, c: usize) {
        if c == 0 {
            return;
        }

        let c = c.min(self.lines.len().saturating_sub(1));

        drop(self.lines.drain(self.lines.len() - c..));

        self.need_rebuild = true;

        if !self.line_is_empty() {
            self.last_line = self.lines.pop_back().unwrap_or_default();
        }
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
        self.last_line.align = align;
    }

    pub fn set_style(&mut self, style: FontStyle) {
        self.style.font_style = style;
    }

    pub fn set_font(&mut self, font: String) {
        self.style.font_family = font.into();
    }

    pub fn align(&self) -> Alignment {
        self.last_line.align
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
    ForceEnterKey,
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

pub struct LinesFrom<'a> {
    this: &'a VirtualConsole,
    from: usize,
}

impl<'a> LinesFrom<'a> {
    pub fn len(&self) -> usize {
        self.this.lines.len() - self.from
    }

    pub fn iter(&self) -> impl Iterator<Item = &'a ConsoleLine> + Clone + 'a {
        self.this.lines.range(self.from..self.this.lines.len())
    }
}

impl<'a> Serialize for LinesFrom<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serde_iter::seq::serialize(&self.iter(), serializer)
    }
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
