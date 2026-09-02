use erars_ast::{Alignment, Value};
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::sync::Arc;
use std::time::Instant;

pub mod width;

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

impl Default for Color {
    fn default() -> Self {
        Color([0; 3])
    }
}

impl From<Color> for u32 {
    fn from(Color([r, g, b]): Color) -> Self {
        u32::from_le_bytes([r, g, b, 0])
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConsoleLinePart {
    Text(String, TextStyle),
    Line(String, TextStyle),
    Button(Vec<(String, TextStyle)>, u32, Value),
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

    fn push_button_merge(&mut self, input_gen: u32, text: String, style: TextStyle, value: Value) {
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
                            let num: i64 = capture.get(1).unwrap().as_str().parse().unwrap();
                            let text = capture.get(0).unwrap().as_str().to_string();
                            start += text.len();
                            self.push_button_merge(input_gen, text, style.clone(), Value::Int(num));
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
                            let num: i64 = capture.get(1).unwrap().as_str().parse().unwrap();
                            let text = capture.get(0).unwrap().as_str().to_string();
                            start += text.len();
                            self.push_button_merge(input_gen, text, style.clone(), Value::Int(num));
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

    /// Emuera `PrintStringBuffer.Append(str, style, force_button = true)`
    /// (used by PRINTC/PRINTLC): the text is button-scanned on its own and
    /// appended as its own part(s). Nothing already on the line is drained
    /// into a button made from `text`, a pending `[` (`button_start`) from
    /// earlier text is forgotten, and no `[` inside `text` is left pending.
    fn push_forced_text(&mut self, input_gen: u32, text: String, style: &TextStyle) {
        let mut item = ConsoleLine::default();
        item.push_text(input_gen, text, style);
        self.parts.extend(item.parts);
        self.button_start = None;
    }
}

#[derive(Serialize)]
pub struct ConsoleSerde<'a> {
    pub rebuild: bool,
    pub bg_color: Color,
    pub hl_color: Color,
    #[serde(skip_serializing_if = "ConsoleLine::is_empty")]
    pub last_line: &'a ConsoleLine,
    pub lines: LinesFrom<'a>,
}

/// Everything `VirtualConsole::new` needs from `EraConfig`; built by
/// `erars_vm::console_config` (spec Component 2).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ConsoleConfig {
    /// PRINTC field width in half-width cells (Emuera PrintCLength, 25);
    /// PRINTLC uses `printc_width + 1`.
    pub printc_width: usize,
    /// Number of finished lines kept before the oldest is dropped.
    pub max_log: usize,
    /// Game encoding (`Language::encoding()`): decides half/full cells.
    pub encoding: &'static encoding_rs::Encoding,
    /// Default text colour (`文字色`, Emuera ForeColor 192,192,192).
    pub fore_color: Color,
    /// Background colour (`背景色`, Emuera BackColor 0,0,0).
    pub bg_color: Color,
    /// Hovered-button colour (`選択中文字色`, Emuera FocusColor 255,255,0).
    pub focus_color: Color,
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
    default_color: Color,
    widths: Arc<width::WidthTable>,
    pub top_index: usize,
}

impl VirtualConsole {
    pub fn new(cfg: &ConsoleConfig) -> Self {
        Self {
            input_gen: 0,
            timeout: None,
            printc_width: cfg.printc_width,
            need_rebuild: false,
            lines: VecDeque::with_capacity(cfg.max_log),
            last_line: ConsoleLine::default(),
            max_log: cfg.max_log,
            style: TextStyle {
                color: cfg.fore_color,
                font_family: "".into(),
                font_style: FontStyle::NORMAL,
            },
            default_color: cfg.fore_color,
            bg_color: cfg.bg_color,
            hl_color: cfg.focus_color,
            skipdisp: false,
            top_index: 0,
            widths: Arc::new(width::WidthTable::new(cfg.encoding)),
        }
    }

    /// Half-width cells of `s` in the game encoding — the one width function
    /// shared with the VM (STRLEN, PadStr) and the renderer grid.
    pub fn cells(&self, s: &str) -> usize {
        self.widths.str_cells(s)
    }

    /// Cells of one character: 0, 1 or 2.
    pub fn char_cells(&self, c: char) -> u8 {
        self.widths.char_cells(c)
    }

    /// The configured text colour (`文字色`); PRINTD and GETDEFCOLOR use it.
    pub fn default_color(&self) -> Color {
        self.default_color
    }

    /// RESETCOLOR: back to the configured text colour.
    pub fn reset_color(&mut self) {
        self.style.color = self.default_color;
    }

    /// Emuera `CreateTypeCString`: pad `s` with spaces into a field of
    /// `width` cells — after the text when `left` (PRINTLC), before it
    /// otherwise (PRINTC). Text at or beyond `width` cells is returned as is.
    fn pad_cells(&self, s: &str, width: usize, left: bool) -> String {
        let cells = self.widths.str_cells(s);
        if cells >= width {
            return s.to_owned();
        }
        let pad = width - cells;
        let mut out = String::with_capacity(s.len() + pad);
        if left {
            out.push_str(s);
            out.extend(std::iter::repeat(' ').take(pad));
        } else {
            out.extend(std::iter::repeat(' ').take(pad));
            out.push_str(s);
        }
        out
    }

    pub fn make_serializable<'a>(&'a self, from: usize) -> ConsoleSerde<'a> {
        ConsoleSerde {
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

    /// PRINT: every `\n` ends the current logical line, exactly like Emuera's
    /// `EmueraConsole.Print`, so LINECOUNT / CLEARLINE / ALIGNMENT see it.
    /// Empty segments push nothing (Emuera returns early on an empty string).
    pub fn print(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        if !s.contains('\n') {
            if !s.is_empty() {
                self.last_line.push_text(self.input_gen, s, &self.style);
            }
            return;
        }
        for (i, seg) in s.split('\n').enumerate() {
            if i > 0 {
                self.push_line();
            }
            if !seg.is_empty() {
                self.last_line.push_text(self.input_gen, seg.to_owned(), &self.style);
            }
        }
    }

    pub fn print_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.print(s);
        self.push_line();
    }

    fn push_button(&mut self, text: String, value: Value) {
        let style = self.style.clone();
        self.last_line.button_start = None;
        self.last_line.parts.push(ConsoleLinePart::Button(
            vec![(text, style)],
            self.input_gen,
            value,
        ));
    }

    /// PRINTBUTTON: `\n` is removed (Emuera Process.ScriptProc.cs:118), the
    /// rest becomes one button part.
    pub fn print_button(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        self.push_button(strip_newlines(text), value);
    }

    /// PRINTBUTTONLC: `\n` removed, then left-aligned in `printc_width + 1` cells.
    pub fn print_button_lc(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        let text = strip_newlines(text);
        let padded = self.pad_cells(&text, self.printc_width + 1, true);
        self.push_button(padded, value);
    }

    /// PRINTBUTTONC: `\n` removed, then right-aligned in `printc_width` cells.
    pub fn print_button_rc(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        let text = strip_newlines(text);
        let padded = self.pad_cells(&text, self.printc_width, false);
        self.push_button(padded, value);
    }

    /// PRINTLC: left-aligned in `printc_width + 1` cells (Emuera 26) and
    /// pushed as its own part: `\n` is kept and nothing before or inside the
    /// item merges into a button with it.
    pub fn printlc(&mut self, s: &str) {
        if self.skipdisp {
            return;
        }
        let padded = self.pad_cells(s, self.printc_width + 1, true);
        self.last_line.push_forced_text(self.input_gen, padded, &self.style);
    }

    /// PRINTC: right-aligned in `printc_width` cells (Emuera 25); see `printlc`.
    pub fn printrc(&mut self, s: &str) {
        if self.skipdisp {
            return;
        }
        let padded = self.pad_cells(s, self.printc_width, false);
        self.last_line.push_forced_text(self.input_gen, padded, &self.style);
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

    /// DRAWLINE / CUSTOMDRAWLINE: Emuera draws the rule with
    /// `FontStyle.Regular` but keeps the current colour and family.
    pub fn draw_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        let style = TextStyle {
            font_style: FontStyle::NORMAL,
            ..self.style.clone()
        };
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
    #[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
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

/// PRINTBUTTON / PRINTBUTTONC / PRINTBUTTONLC drop every `\n`
/// (Emuera Process.ScriptProc.cs:118/135).
fn strip_newlines(text: String) -> String {
    if text.contains('\n') {
        text.replace('\n', "")
    } else {
        text
    }
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
                    font_style: FontStyle::NORMAL,
                },
            );
        )*
        line
    }};
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
                    font_style: FontStyle(
                        0x0,
                    ),
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
                    font_style: FontStyle(
                        0x0,
                    ),
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
                    font_style: FontStyle(
                        0x0,
                    ),
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
                    font_style: FontStyle(
                        0x0,
                    ),
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
                    font_style: FontStyle(
                        0x0,
                    ),
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
                    font_style: FontStyle(
                        0x0,
                    ),
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
                    font_style: FontStyle(
                        0x0,
                    ),
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
            font_style: FontStyle(
                0x0,
            ),
        },
    ),
]
"#
    );
}

#[cfg(test)]
mod console_tests {
    use super::*;
    use erars_ast::{Alignment, Value};

    const FORE: Color = Color([192, 192, 192]);

    fn config(encoding: &'static encoding_rs::Encoding) -> ConsoleConfig {
        ConsoleConfig {
            printc_width: 25,
            max_log: 500,
            encoding,
            fore_color: FORE,
            bg_color: Color([0, 0, 0]),
            focus_color: Color([255, 255, 0]),
        }
    }

    fn jp() -> VirtualConsole {
        VirtualConsole::new(&config(encoding_rs::SHIFT_JIS))
    }

    fn kr() -> VirtualConsole {
        VirtualConsole::new(&config(encoding_rs::EUC_KR))
    }

    fn style() -> TextStyle {
        TextStyle {
            color: FORE,
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        }
    }

    fn spaces(n: usize) -> String {
        " ".repeat(n)
    }

    #[test]
    fn printc_pads_to_25_cells() {
        // half-width
        let mut tx = jp();
        tx.printrc("abc");
        assert_eq!(tx.last_line.to_string(), format!("{}abc", spaces(22)));
        assert_eq!(tx.cells(&tx.last_line.to_string()), 25);
        // full-width
        let mut tx = jp();
        tx.printrc("あい");
        assert_eq!(tx.last_line.to_string(), format!("{}あい", spaces(21)));
        assert_eq!(tx.cells(&tx.last_line.to_string()), 25);
        // mixed: a (1) + あ (2) + ─ U+2500 (2 in Shift_JIS)
        let mut tx = jp();
        tx.printrc("aあ─");
        assert_eq!(tx.last_line.to_string(), format!("{}aあ─", spaces(20)));
        // EUC-KR: Hangul is 2 cells
        let mut tx = kr();
        tx.printrc("한");
        assert_eq!(tx.last_line.to_string(), format!("{}한", spaces(23)));
    }

    #[test]
    fn printlc_pads_to_26_cells() {
        let mut tx = jp();
        tx.printlc("abc");
        assert_eq!(tx.last_line.to_string(), format!("abc{}", spaces(23)));
        let mut tx = jp();
        tx.printlc("あい");
        assert_eq!(tx.last_line.to_string(), format!("あい{}", spaces(22)));
        let mut tx = jp();
        tx.printlc("aあ");
        assert_eq!(tx.last_line.to_string(), format!("aあ{}", spaces(23)));
        assert_eq!(tx.cells(&tx.last_line.to_string()), 26);
    }

    #[test]
    fn printc_field_boundary() {
        // PRINTC: 24 cells -> one space, 25 -> unpadded
        let mut tx = jp();
        tx.printrc(&"a".repeat(24));
        assert_eq!(tx.last_line.to_string(), format!(" {}", "a".repeat(24)));
        let mut tx = jp();
        tx.printrc(&"a".repeat(25));
        assert_eq!(tx.last_line.to_string(), "a".repeat(25));
        // PRINTLC: 25 cells -> one space, 26 -> unpadded
        let mut tx = jp();
        tx.printlc(&"a".repeat(25));
        assert_eq!(tx.last_line.to_string(), format!("{} ", "a".repeat(25)));
        let mut tx = jp();
        tx.printlc(&"a".repeat(26));
        assert_eq!(tx.last_line.to_string(), "a".repeat(26));
    }

    #[test]
    fn printc_overlong_unpadded() {
        let mut tx = jp();
        tx.printrc(&"a".repeat(30));
        assert_eq!(tx.last_line.to_string(), "a".repeat(30));
        let mut tx = jp();
        tx.printlc(&"あ".repeat(14)); // 28 cells
        assert_eq!(tx.last_line.to_string(), "あ".repeat(14));
    }

    #[test]
    fn printc_button_variants() {
        let mut tx = jp();
        tx.print_button_rc("[1] x".into(), Value::Int(1));
        assert_eq!(
            tx.last_line.parts,
            vec![ConsoleLinePart::Button(
                vec![(format!("{}[1] x", spaces(20)), style())],
                0,
                Value::Int(1)
            )]
        );
        let mut tx = jp();
        tx.print_button_lc("[1] x".into(), Value::Int(1));
        assert_eq!(
            tx.last_line.parts,
            vec![ConsoleLinePart::Button(
                vec![(format!("[1] x{}", spaces(21)), style())],
                0,
                Value::Int(1)
            )]
        );
        // overlong button text is unpadded
        let mut tx = jp();
        tx.print_button_rc("a".repeat(25), Value::Int(1));
        assert_eq!(tx.last_line.to_string(), "a".repeat(25));
    }

    #[test]
    fn printc_item_never_merges_with_neighbours() {
        // text printed before a PRINTC item stays its own Text part
        let mut tx = jp();
        tx.print("abc".into());
        tx.printrc("[1] x");
        assert_eq!(
            tx.last_line.parts,
            vec![
                ConsoleLinePart::Text("abc".into(), style()),
                ConsoleLinePart::Button(
                    vec![(format!("{}[1] x", spaces(20)), style())],
                    0,
                    Value::Int(1)
                ),
            ]
        );
        assert_eq!(tx.last_line.button_start, None);

        // a pending '[' from earlier text does not fuse with the PRINTC item
        let mut tx = jp();
        tx.print("[".into());
        assert_eq!(tx.last_line.button_start, Some(0));
        tx.printrc("1] x");
        assert_eq!(
            tx.last_line.parts,
            vec![
                ConsoleLinePart::Text("[".into(), style()),
                ConsoleLinePart::Text(format!("{}1] x", spaces(21)), style()),
            ]
        );
        assert_eq!(tx.last_line.button_start, None);

        // train-menu shape (`{name}[{no:3}]`): every item is its own button
        let mut tx = jp();
        tx.printrc("A[  1]");
        tx.printrc("B[  2]");
        assert_eq!(
            tx.last_line.parts,
            vec![
                ConsoleLinePart::Button(
                    vec![(format!("{}A[  1]", spaces(19)), style())],
                    0,
                    Value::Int(1)
                ),
                ConsoleLinePart::Button(
                    vec![(format!("{}B[  2]", spaces(19)), style())],
                    0,
                    Value::Int(2)
                ),
            ]
        );
    }

    #[test]
    fn print_line_splits_at_newline() {
        let mut tx = jp();
        tx.print_line("a\nb".into());
        assert_eq!(tx.line_count(), 2);
        assert_eq!(tx.lines[0].to_string(), "a");
        assert_eq!(tx.lines[1].to_string(), "b");
        assert!(tx.line_is_empty());
        tx.clear_line(1);
        assert_eq!(tx.line_count(), 1);
        assert_eq!(tx.lines[0].to_string(), "a");
    }

    #[test]
    fn newline_split_keeps_alignment_per_logical_line() {
        let mut tx = jp();
        tx.print("a\nb".into());
        tx.set_align(Alignment::Right);
        tx.print_line("c".into());
        assert_eq!(tx.lines[0].align, Alignment::Left);
        assert_eq!(tx.lines[0].to_string(), "a");
        assert_eq!(tx.lines[1].align, Alignment::Right);
        assert_eq!(tx.lines[1].to_string(), "bc");
    }

    #[test]
    fn print_edge_newlines_and_empty() {
        let mut tx = jp();
        tx.print("a\n".into());
        assert_eq!(tx.lines.len(), 1);
        assert_eq!(tx.lines[0].to_string(), "a");
        assert!(tx.line_is_empty());

        let mut tx = jp();
        tx.print("\nb".into());
        assert_eq!(tx.lines.len(), 1);
        assert!(tx.lines[0].is_empty());
        assert_eq!(tx.last_line.to_string(), "b");

        let mut tx = jp();
        tx.print(String::new());
        assert!(tx.line_is_empty());
        tx.print_line(String::new());
        assert_eq!(tx.line_count(), 1);
        assert!(tx.lines[0].is_empty());
    }

    #[test]
    fn print_button_strips_newlines() {
        let mut tx = jp();
        tx.print_button("x\ny".into(), Value::Int(3));
        assert_eq!(
            tx.last_line.parts,
            vec![ConsoleLinePart::Button(
                vec![("xy".into(), style())],
                0,
                Value::Int(3)
            )]
        );
        let mut tx = jp();
        tx.print_button_lc("x\ny\n".into(), Value::Int(3));
        assert_eq!(tx.last_line.to_string(), format!("xy{}", spaces(24)));
    }

    #[test]
    fn printrc_keeps_newline_inside_part() {
        let mut tx = jp();
        tx.printrc("a\nb");
        assert_eq!(tx.lines.len(), 0);
        assert_eq!(tx.last_line.parts.len(), 1);
        let s = tx.last_line.to_string();
        assert!(s.ends_with("a\nb"), "{s:?}");
        // '\n' is a control character: 0 cells (spec Component 1 step 1),
        // so the item is 2 cells and gets 23 pad spaces
        assert_eq!(s, format!("{}a\nb", spaces(23)));
    }

    #[test]
    fn draw_line_forces_normal_style() {
        let mut tx = jp();
        tx.set_style(FontStyle::BOLD | FontStyle::UNDERLINE);
        tx.set_color(1, 2, 3);
        tx.set_font("Foo".into());
        tx.draw_line("-".into());
        assert_eq!(
            tx.lines[0].parts,
            vec![ConsoleLinePart::Line(
                "-".into(),
                TextStyle {
                    color: Color([1, 2, 3]),
                    font_family: "Foo".into(),
                    font_style: FontStyle::NORMAL,
                }
            )]
        );
        // the console's own style is untouched
        assert_eq!(tx.style(), FontStyle::BOLD | FontStyle::UNDERLINE);
    }

    #[test]
    fn reset_color_restores_configured_colour() {
        let mut tx = jp();
        assert_eq!(tx.style.color, FORE);
        assert_eq!(tx.default_color(), FORE);
        assert_eq!(tx.bg_color, Color([0, 0, 0]));
        assert_eq!(tx.hl_color, Color([255, 255, 0]));
        tx.set_color(1, 2, 3);
        assert_eq!(tx.style.color, Color([1, 2, 3]));
        tx.reset_color();
        assert_eq!(tx.style.color, FORE);
        assert_eq!(tx.color(), u32::from(FORE));
    }

    #[test]
    fn cells_follow_the_configured_encoding() {
        let tx = jp();
        assert_eq!(tx.char_cells('a'), 1);
        assert_eq!(tx.char_cells('あ'), 2);
        assert_eq!(tx.cells("aあ"), 3);
        assert_eq!(tx.cells(""), 0);
        let tx = kr();
        assert_eq!(tx.cells("한"), 2);
        // the Arc-shared table survives Clone
        let tx2 = tx.clone();
        assert_eq!(tx2.cells("한"), 2);
    }

    #[test]
    fn console_serde_json() {
        let mut tx = jp();
        tx.print_line("a\nb".into());
        tx.set_align(Alignment::Right);
        tx.print("c".into());
        tx.print_button("[1] go".into(), Value::Int(1));
        let json = serde_json::to_value(tx.make_serializable(0)).unwrap();
        assert_eq!(
            json,
            serde_json::json!({
                "rebuild": false,
                "bg_color": [0, 0, 0],
                "hl_color": [255, 255, 0],
                "last_line": {
                    "align": "Right",
                    "parts": [
                        {"Text": ["c", {"color": [192, 192, 192]}]},
                        {"Button": [[["[1] go", {"color": [192, 192, 192]}]], 0, {"Int": 1}]}
                    ]
                },
                "lines": [
                    {"parts": [{"Text": ["a", {"color": [192, 192, 192]}]}]},
                    {"parts": [{"Text": ["b", {"color": [192, 192, 192]}]}]}
                ]
            })
        );
    }
}
