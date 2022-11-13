use erars_ast::{Alignment, Value};
use once_cell::sync::Lazy;
use pad::PadStr;
use regex::Regex;
use smol_str::SmolStr;
use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::time::Instant;

use crate::fbs::{self, Color, FontStyle};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextStyle {
    pub color: Color,
    pub font_family: SmolStr,
    pub font_style: FontStyle,
}

impl TextStyle {
    fn make_serializable<'a: 'b, 'b>(
        &self,
        fbb: &'b mut flatbuffers::FlatBufferBuilder<'a>,
    ) -> flatbuffers::WIPOffset<fbs::TextStyle<'a>> {
        let ff = fbb.create_shared_string(&self.font_family);
        let mut b = fbs::TextStyleBuilder::new(fbb);
        b.add_color(&self.color);
        b.add_font_family(ff);
        b.add_font_style(self.font_style);
        b.finish()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConsoleLinePart {
    Text(String, TextStyle),
    Line(String, TextStyle),
    Button(Vec<(String, TextStyle)>, u32, String),
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
            Self::Button(arg0, _, _) => {
                for (text, _) in arg0 {
                    write!(f, "{text}")?;
                }

                Ok(())
            }
        }
    }
}

#[derive(Clone, Default, PartialEq, Eq)]
pub struct ConsoleLine {
    pub align: Alignment,
    pub button_start: Option<usize>,
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

    fn push_button_merge(&mut self, input_gen: u32, text: String, style: TextStyle, value: String) {
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
        self.parts.push(ConsoleLinePart::Button(parts, input_gen, value));
    }
    fn append_button_text(&mut self, text: String, style: &TextStyle) {
        match self.parts.last_mut() {
            Some(ConsoleLinePart::Button(parts, _, _)) => {
                parts.last_mut().unwrap().0.push_str(&text);
            }
            _ => {
                self.parts.push(ConsoleLinePart::Text(text, style.clone()));
            }
        }
    }
    fn push_plain_text(&mut self, text: String, style: &TextStyle) {
        match self.parts.last_mut() {
            Some(ConsoleLinePart::Text(prev_text, prev_style)) if *prev_style == *style => {
                prev_text.push_str(&text);
            }
            _ => {
                self.parts.push(ConsoleLinePart::Text(text, style.clone()));
            }
        }
    }
    fn push_text(&mut self, input_gen: u32, text: String, style: &TextStyle) {
        static BUTTON_REGEX: Lazy<Regex> =
            Lazy::new(|| Regex::new(r#"[^\[]*\[\s*(\d+)\s*\][^\[\]]*"#).unwrap());

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
                        let mut start = 0;

                        while let Some(capture) = BUTTON_REGEX.captures(&btn_buf[start..]) {
                            let num = capture.get(1).unwrap().as_str();
                            let text = capture.get(0).unwrap().as_str().to_string();
                            start += text.len();
                            self.push_button_merge(input_gen, text, style.clone(), num.into());
                        }

                        if let Some(s) = btn_buf.get(start..) {
                            self.append_button_text(s.into(), style);
                        }

                        return;
                    }
                }
                None => match BUTTON_REGEX.is_match(&text) {
                    true => {
                        let mut start = 0;

                        while let Some(capture) = BUTTON_REGEX.captures(&text[start..]) {
                            let num = capture.get(1).unwrap().as_str();
                            let text = capture.get(0).unwrap().as_str().to_string();
                            start += text.len();
                            self.push_button_merge(input_gen, text, style.clone(), num.into());
                        }

                        if let Some(s) = text.get(start..) {
                            self.append_button_text(s.into(), style);
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
            Some(ConsoleLinePart::Button(parts, ..)) => {
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

    fn make_serializable<'a: 'b, 'b>(
        &self,
        fbb: &'b mut flatbuffers::FlatBufferBuilder<'a>,
    ) -> flatbuffers::WIPOffset<fbs::ConsoleLine<'a>> {
        let parts = self
            .parts
            .iter()
            .map(|part| {
                let (ty, part) = match part {
                    ConsoleLinePart::Text(text, style) => {
                        let text = fbb.create_string(text);
                        let style = style.make_serializable(fbb);
                        let mut b = fbs::ConsoleLinePartTextBuilder::new(fbb);
                        b.add_style(style);
                        b.add_text(text);
                        (
                            fbs::ConsoleLinePart::ConsoleLinePartText,
                            b.finish().as_union_value(),
                        )
                    }
                    ConsoleLinePart::Line(text, style) => {
                        let text = fbb.create_string(text);
                        let style = style.make_serializable(fbb);
                        let mut b = fbs::ConsoleLinePartLineBuilder::new(fbb);
                        b.add_style(style);
                        b.add_line_text(text);
                        (
                            fbs::ConsoleLinePart::ConsoleLinePartLine,
                            b.finish().as_union_value(),
                        )
                    }
                    ConsoleLinePart::Button(parts, gen, value) => {
                        let parts = parts
                            .iter()
                            .map(|(text, style)| {
                                let text = fbb.create_string(text);
                                let style = style.make_serializable(fbb);
                                let mut b = fbs::ConsoleLinePartTextBuilder::new(fbb);
                                b.add_style(style);
                                b.add_text(text);
                                b.finish()
                            })
                            .collect::<Vec<_>>();
                        let parts = fbb.create_vector(&parts);
                        let value = fbb.create_shared_string(value);
                        let mut b = fbs::ConsoleLinePartButtonBuilder::new(fbb);
                        b.add_value(value);
                        b.add_btn_parts(parts);
                        b.add_generation(*gen);
                        (
                            fbs::ConsoleLinePart::ConsoleLinePartButton,
                            b.finish().as_union_value(),
                        )
                    }
                };
                fbs::ConsoleLinePartWrap::create(
                    fbb,
                    &fbs::ConsoleLinePartWrapArgs {
                        part: Some(part),
                        part_type: ty,
                        ..Default::default()
                    },
                )
            })
            .collect::<Vec<_>>();

        let parts = fbb.create_vector(&parts);

        let mut b = fbs::ConsoleLineBuilder::new(fbb);

        b.add_align(match self.align {
            Alignment::Left => fbs::LineAlign::Left,
            Alignment::Center => fbs::LineAlign::Center,
            Alignment::Right => fbs::LineAlign::Right,
        });

        b.add_parts(parts);

        b.finish()
    }
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
                font_style: FontStyle::empty(),
            },
            bg_color: Color([0, 0, 0]),
            hl_color: Color([255, 255, 0]),
            skipdisp: false,
            top_index: 0,
        }
    }

    pub fn make_serializable(&self, from: usize, mut out: Vec<u8>) -> Vec<u8> {
        out.clear();
        let mut fbb = flatbuffers::FlatBufferBuilder::from_vec(out);

        let lines = self
            .lines
            .iter()
            .skip(from)
            .map(|l| l.make_serializable(&mut fbb))
            .collect::<Vec<_>>();

        let lines = fbb.create_vector(&lines);

        let b = fbs::DrawRequest::create(
            &mut fbb,
            &fbs::DrawRequestArgs {
                bg_color: Some(&self.bg_color),
                hl_color: Some(&self.hl_color),
                lines: Some(lines),
                need_rebuild: self.need_rebuild,
                last_line: None,
                ..Default::default()
            },
        );

        fbb.finish(b, None);
        fbb.collapse().0
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
        self.last_line.push_text(self.input_gen, s, &self.style);
    }

    pub fn print_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.print(s);
        self.push_line();
    }

    pub fn print_button(&mut self, text: String, value: String) {
        if self.skipdisp {
            return;
        }
        let style = self.style.clone();
        self.last_line.button_start = None;
        self.last_line.parts.push(ConsoleLinePart::Button(
            vec![(text, style)],
            self.input_gen,
            value,
        ));
    }

    pub fn print_button_lc(&mut self, text: String, value: String) {
        if self.skipdisp {
            return;
        }
        self.print_button(
            text.pad_to_width_with_alignment(self.printc_width, pad::Alignment::Left),
            value,
        );
    }

    pub fn print_button_rc(&mut self, text: String, value: String) {
        if self.skipdisp {
            return;
        }
        self.print_button(
            text.pad_to_width_with_alignment(self.printc_width, pad::Alignment::Right),
            value,
        );
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

        let new_line = std::mem::take(&mut self.last_line);
        self.last_line.align = new_line.align;
        self.lines.push_back(new_line);
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
        let [r, g, b] = self.style.color.0;
        u32::from_le_bytes([r, g, b, 0])
    }

    pub fn hl_color(&self) -> u32 {
        let [r, g, b] = self.hl_color.0;
        u32::from_le_bytes([r, g, b, 0])
    }

    pub fn bg_color(&self) -> u32 {
        let [r, g, b] = self.bg_color.0;
        u32::from_le_bytes([r, g, b, 0])
    }

    pub fn font(&self) -> &str {
        &self.style.font_family
    }

    pub fn style(&self) -> FontStyle {
        self.style.font_style
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InputRequestType {
    AnyKey,
    EnterKey,
    ForceEnterKey,
    Int,
    Str,
}

#[derive(Clone, Debug, PartialEq, Eq)]
/// input timeout
pub struct Timeout {
    /// Unix timestamp in nanos
    pub timeout: i128,
    pub default_value: Value,
    pub timeout_msg: Option<String>,
    pub show_timer: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InputRequest {
    /// InputRequest generation
    pub generation: u32,
    /// type of request
    pub ty: InputRequestType,
    /// whether is ONEINPUT or not
    pub is_one: bool,
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

#[cfg(test)]
macro_rules! make_test_line {
    ($($text:expr)*) => {{
        let mut line = ConsoleLine::default();
        $(
            line.push_text(
                0,
                String::from($text),
                &TextStyle {
                    color: Color([0; 3]),
                    font_family: "".into(),
                    font_style: FontStyle::empty(),
                },
            );
        )*
        line
    }};
}

#[test]
fn test_fbb() {
    let mut console = VirtualConsole::new(10, 500);
    console.print(format!("Hello, world!"));
    let ret = console.make_serializable(0, Vec::new());
    assert!(!ret.is_empty());
}

#[test]
fn issue_73() {
    k9::snapshot!(
        make_test_line!("[ 0] - [텍스트]").parts,
        r#"
[
    Button(
        [
            (
                "[ 0] - [텍스트]",
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
        0,
        Int(
            0,
        ),
    ),
]
"#
    );
}

#[test]
fn button_test() {
    k9::snapshot!(
        make_test_line!("[0] 1 [1] 2 [ 3] 3 [456 ] 745").parts,
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
        0,
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
        0,
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
        0,
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
        0,
        Int(
            456,
        ),
    ),
]
"#
    );

    k9::snapshot!(
        make_test_line!(">" "[ 9]" "2022年10月08日 22:52:52 1일째 [낮").parts,
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
        0,
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
