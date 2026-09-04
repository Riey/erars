use erars_ast::{Alignment, Value};
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::sync::Arc;
use std::time::Instant;

pub mod cbg;
pub mod div;
pub mod image;
pub mod kana;
pub mod width;

pub use cbg::{CbgImage, CbgLayer};
pub use div::{ConsoleDiv, DivAnchor, DivBox, DivSpec};
pub use image::{ImageBitmap, ImageStore, InlineImage, MixedNum};
pub use kana::ForceKana;

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

/// `0xRRGGBB`, Emuera's `Color.ToArgb() & 0xFFFFFF` — the packing of every
/// colour integer the VM reads or writes (GETCOLOR, GETDEFCOLOR, GETCONFIG,
/// SETCOLOR's single-int form).
impl From<Color> for u32 {
    fn from(Color([r, g, b]): Color) -> Self {
        (u32::from(r) << 16) | (u32::from(g) << 8) | u32::from(b)
    }
}

/// Inverse of `u32::from(Color)`; bits above 23 are ignored.
impl From<u32> for Color {
    fn from(c: u32) -> Self {
        Color([(c >> 16) as u8, (c >> 8) as u8, c as u8])
    }
}

/// One run of a console line.
///
/// `Eq` is deliberately absent: an [`InlineImage`]'s `XsubPixel` is an `f32`,
/// exactly as in Emuera (`GameView/ConsoleImagePart.cs:90`).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ConsoleLinePart {
    Text(String, TextStyle),
    Line(String, TextStyle),
    Button(Vec<(String, TextStyle)>, u32, Value),
    /// A `<img>` / `PRINT_IMG` bitmap. Emuera's `ConsoleImagePart`, which is
    /// an inline part of the same list as the text runs
    /// (`GameView/ConsoleImagePart.cs:13`).
    ///
    /// `Arc` because `ConsoleFrame::from_vconsole` clones every line on every
    /// redraw; the payload is share-only after construction.
    Image(Arc<InlineImage>),
    /// A positioned `<div>` box. Emuera's `ConsoleDivPart`, an inline part of
    /// the line it was printed on that occupies no width and draws at a
    /// coordinate of its own (`_Library/EvilMask/ConsoleDivPart.cs:14-64`).
    Div(Arc<ConsoleDiv>),
}

impl ConsoleLinePart {
    /// The text this part contributes to `[n]` button scanning.
    ///
    /// DELIBERATE: an image contributes nothing, so a `[` … `]` pair split by
    /// an image still forms a button, and the button is cut in two around the
    /// image: both halves are clickable under the same value, the image is
    /// drawn where it was printed and is not itself clickable. Emuera scans
    /// the raw string before parts exist and swallows the image into the
    /// `ConsoleButtonString` (`GameView/PrintStringBuffer.cs:189-279`,
    /// `createButtons`), which a `ConsoleLinePart::Button` of text runs cannot
    /// hold. No call site in either corpus puts an image inside a button
    /// (`<button>` never contains `<img>` in eramegaten_p_kr). See §5 of
    /// `docs/research/2026-09-03-emuera-command-gap.md`.
    fn as_text(&self) -> &str {
        match self {
            Self::Text(t, _) => t.as_str(),
            // A positioned box contributes nothing for the same reason an
            // image does not: `[` … `]` scanning walks the built parts.
            Self::Image(_) | Self::Div(_) => "",
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
            // The reconstructed `<img …>` tag, which is what Emuera itself
            // prints when the resource is missing
            // (`GameView/ConsoleImagePart.cs:69-73`). Text-only front-ends
            // therefore show a real Emuera string, not a placeholder.
            Self::Image(img) => write!(f, "{}", img.alt),
            // `ConsoleDivPart.ToString` (`_Library/EvilMask/ConsoleDivPart.cs:177-188`):
            // the reconstructed tag around the content of the box.
            Self::Div(div) => write!(f, "{}", div.alt_text()),
        }
    }
}

#[derive(Clone, Default, PartialEq, Serialize, Deserialize)]
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

/// Append one styled run, merging into the previous one when the style is
/// identical — the rule [`ConsoleLine::push_plain_text`] applies to a line's
/// parts, applied inside a button's run list. An empty run is nothing.
fn push_run(runs: &mut Vec<(String, TextStyle)>, text: String, style: TextStyle) {
    if text.is_empty() {
        return;
    }
    match runs.last_mut() {
        Some((prev_text, prev_style)) if *prev_style == style => prev_text.push_str(&text),
        _ => runs.push((text, style)),
    }
}

impl ConsoleLine {
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    /// One button, built from `runs` and from whatever plain text already sat
    /// in front of it.
    ///
    /// Emuera builds a `ConsoleButtonString` out of the display parts it has
    /// already made (`GameView/ConsoleButtonString.cs:27-43` takes an
    /// `AConsoleDisplayPart[]`), so a button spanning a colour or font change
    /// keeps every run's own style; `ConsoleLinePart::Button` carries the same
    /// `(text, style)` list.
    fn push_button_runs(
        &mut self,
        input_gen: u32,
        runs: Vec<(String, TextStyle)>,
        value: Value,
    ) {
        let len = self
            .parts
            .iter()
            .rev()
            .take_while(|part| matches!(part, ConsoleLinePart::Text(..)))
            .count();
        let mut parts: Vec<(String, TextStyle)> = if len == 0 {
            Vec::new()
        } else {
            let from = self.parts.len() - len;
            self.parts.drain(from..).map(ConsoleLinePart::into_text).collect()
        };
        for (text, style) in runs {
            push_run(&mut parts, text, style);
        }
        self.parts.push(ConsoleLinePart::Button(parts, input_gen, value));
    }
    fn append_button_text(&mut self, text: String, style: &TextStyle) {
        match self.parts.last_mut() {
            Some(ConsoleLinePart::Button(parts, _, _)) => {
                push_run(parts, text, style.clone());
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

    /// Append a resolved inline image.
    ///
    /// Emuera's `EmueraConsole.PrintImg` appends a `ConsoleImagePart` to the
    /// print buffer with no merging or button scanning
    /// (`GameView/EmueraConsole.Print.cs:404-408`); an image can never merge
    /// with a neighbour because it is not text.
    fn push_image(&mut self, image: Arc<InlineImage>) {
        self.parts.push(ConsoleLinePart::Image(image));
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
                        let mut queue: VecDeque<ConsoleLinePart> =
                            self.parts.drain(prev_btn_part..).collect();
                        queue.push_back(ConsoleLinePart::Text(text, style.clone()));

                        let mut start = 0;

                        while let Some(capture) = BUTTON_REGEX.captures(&btn_buf[start..]) {
                            let num: i64 = capture.get(1).unwrap().as_str().parse().unwrap();
                            let mut len = capture.get(0).unwrap().as_str().len();
                            start += len;

                            let value = Value::Int(num);
                            let mut runs: Vec<(String, TextStyle)> = Vec::new();

                            while len > 0 {
                                match queue.pop_front() {
                                    // Emuera cuts a `ConsoleStyledString` at
                                    // the button boundary and keeps both
                                    // halves' styles
                                    // (`PrintStringBuffer.cs:388-397`,
                                    // `createButtons`' `DivideAt(used)`).
                                    Some(ConsoleLinePart::Text(mut text, style)) => {
                                        if text.len() > len {
                                            let rest = text.split_off(len);
                                            queue.push_front(ConsoleLinePart::Text(
                                                rest,
                                                style.clone(),
                                            ));
                                        }
                                        len -= text.len();
                                        push_run(&mut runs, text, style);
                                    }
                                    // A part that contributed no text — an
                                    // image, a positioned box — is drawn where
                                    // it was printed, so it cuts the button in
                                    // two and the text on either side stays
                                    // clickable under the same value. Emuera,
                                    // whose scan runs before any part exists,
                                    // keeps it *inside* the
                                    // `ConsoleButtonString`
                                    // (`PrintStringBuffer.cs:189-279`), which
                                    // `ConsoleLinePart::Button` cannot hold.
                                    Some(part) => {
                                        if !runs.is_empty() {
                                            self.push_button_runs(
                                                input_gen,
                                                std::mem::take(&mut runs),
                                                value.clone(),
                                            );
                                        }
                                        self.parts.push(part);
                                    }
                                    None => break,
                                }
                            }

                            if !runs.is_empty() {
                                self.push_button_runs(input_gen, runs, value);
                            }
                        }

                        // Everything past the last `]`: text joins the button
                        // it trails, a textless part just stays on the line.
                        for part in queue {
                            match part {
                                ConsoleLinePart::Text(text, style) => {
                                    self.append_button_text(text, &style)
                                }
                                part => self.parts.push(part),
                            }
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
                            self.push_button_runs(
                                input_gen,
                                vec![(text, style.clone())],
                                Value::Int(num),
                            );
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
    /// HTML_PRINT_ISLAND overlays in paint order, `[layer, lines]` per island.
    /// Unlike `lines` this is the whole current set on every frame, because an
    /// island is not appended to a log a client can follow incrementally — it
    /// exists until it is cleared. Absent while there is none, so a client
    /// that never sees an island sees the same JSON as before.
    #[serde(skip_serializing_if = "<[_]>::is_empty")]
    pub islands: &'a [(i64, Vec<ConsoleLine>)],
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
    /// Half-width cells that fit the drawable width, i.e. Emuera's
    /// `Config.DrawableWidth` (`Config/Config.cs:225`) divided by the width of
    /// one half-width cell. `getStBar` measures the bar against it
    /// (`GameView/EmueraConsole.Print.cs:632-649`).
    pub drawable_cells: usize,
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

    /// TOOLTIP_SETCOLOR: mouse-over tooltip foreground colour (`None` = default).
    pub tooltip_color: Option<Color>,
    /// TOOLTIP_SETCOLOR: mouse-over tooltip background colour (`None` = default).
    /// Emuera's `SetToolTipColor` sets both at once (`EmueraConsole.cs:1733-1737`).
    pub tooltip_bg_color: Option<Color>,
    /// TOOLTIP_SETDELAY: tooltip show delay in milliseconds.
    pub tooltip_delay_ms: u32,
    /// TOOLTIP_SETDURATION: tooltip display duration in milliseconds.
    pub tooltip_duration_ms: u32,
    /// SETANIMETIMER: the sprite-animation repaint interval in milliseconds,
    /// `None` while the timer is off. Emuera's `redrawTimer`
    /// (`GameView/EmueraConsole.cs:629-640`): a non-positive tick count
    /// disables it and anything under 10 ms is raised to 10.
    pub redraw_timer_ms: Option<u32>,
    /// FORCEKANA: the kana conversion applied to PRINTK-family output.
    force_kana: kana::ForceKana,
    /// DEBUGPRINT*/DEBUG text lines, kept off the normal console.
    debug_lines: Vec<String>,
    /// Whether the last DEBUGPRINT ended its line, so the next one starts a
    /// fresh entry rather than continuing the pending one.
    debug_line_done: bool,
    /// REDRAW: whether printing repaints the screen. Emuera's
    /// `EmueraConsole.redraw` (`ConsoleRedraw.Normal` initially), which gates
    /// every unforced `RefreshStrings`.
    redraw_enabled: bool,
    /// MESSKIP/MOUSESKIP: whether the user is currently fast-forwarding
    /// messages. Emuera's `EmueraConsole.MesSkip` \u2014 live input state set by
    /// Escape, a right click, or `\e` in an entered line, and cleared when the
    /// input transaction ends. Not `skipdisp`, which is script state (ISSKIP).
    mes_skip: bool,

    max_log: usize,
    printc_width: usize,
    drawable_cells: usize,
    default_color: Color,
    widths: Arc<width::WidthTable>,
    /// The decoded pixels behind every [`ConsoleLinePart::Image`] on these
    /// lines. Emuera's parts point straight at the live `Bitmap` because it
    /// has one thread; erars' VM publishes into this store at the redraw
    /// boundary (`erars_vm::GraphicsStore::publish`) and the front-end reads
    /// it, so a frame's text and its pixels always come from one instant.
    ///
    /// A handle, not the data: cloning a `VirtualConsole` (which `ConsoleSerde`
    /// and the save path both do) shares the store rather than copying any
    /// bitmap.
    pub images: image::ImageStore,
    /// The console-background plane the `CBG*` methods draw on, behind and in
    /// front of every line above.
    ///
    /// On the console, exactly as Emuera keeps it
    /// (`GameView/EmueraConsole.cs:101`), which is what makes it outlive
    /// `SPRITEDISPOSEALL`, a load and a new game: only `CBGCLEAR` empties it.
    /// `Arc` because [`crate::cbg::CbgLayer`] changes only when a `CBG*`
    /// method runs while the front-end takes a copy on every redraw.
    pub cbg: Arc<cbg::CbgLayer>,
    /// HTML_PRINT_ISLAND: the overlays, in paint order — sorted by layer, and
    /// within one layer in the order they were printed. Each entry is one
    /// island's markup already turned into lines and stays until a clear
    /// takes it, so printing twice to one layer leaves both islands standing:
    /// `Data/ERB/RPG/ダンジョンアタック/SYSTEM_DUNGEON.ERB:2630-2641` covers
    /// the dungeon view with a background box, waits, and then prints a
    /// second island whose text is positioned at the centre of *that* box —
    /// which only lines up if the box is still there. A layer is therefore a
    /// paint-order group, not a slot, and the layer number is what
    /// `HTML_PRINT_ISLAND_CLEAR` addresses.
    ///
    /// Off the log deliberately: an island is redrawn from this list on every
    /// frame until it is cleared, so it neither scrolls with the log nor
    /// appears in LINECOUNT / HTML_GETPRINTEDSTR / CLEARLINE.
    islands: Vec<(i64, Vec<ConsoleLine>)>,
    pub top_index: usize,
}

impl VirtualConsole {
    pub fn new(cfg: &ConsoleConfig) -> Self {
        Self {
            input_gen: 0,
            timeout: None,
            printc_width: cfg.printc_width,
            drawable_cells: cfg.drawable_cells,
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
            tooltip_color: None,
            tooltip_bg_color: None,
            tooltip_delay_ms: 500,
            tooltip_duration_ms: 3000,
            redraw_timer_ms: None,
            force_kana: kana::ForceKana::Off,
            debug_lines: Vec::new(),
            debug_line_done: false,
            redraw_enabled: true,
            mes_skip: false,
            top_index: 0,
            widths: Arc::new(width::WidthTable::new(cfg.encoding)),
            images: image::ImageStore::new(),
            cbg: Arc::default(),
            islands: Vec::new(),
        }
    }

    /// Half-width cells of `s` in the game encoding — the one width function
    /// shared with the VM (STRLEN, PadStr) and the renderer grid.
    pub fn cells(&self, s: &str) -> usize {
        self.widths.str_cells(s)
    }

    /// A console that builds lines with this one's style, colours, widths and
    /// bitmap store, but has no lines of its own: the destination for content
    /// that is printed *into* something (a `<div>`'s children, an island's
    /// markup) instead of onto the log.
    ///
    /// Emuera does this by recursing into `html2DisplayLine` with the same
    /// `HtmlAnalzeState` (`GameView/HtmlManager.cs:557-563`), so the nested
    /// content inherits the outer style exactly as this does.
    pub fn sub_console(&self) -> Self {
        Self {
            timeout: None,
            lines: VecDeque::new(),
            last_line: ConsoleLine::default(),
            islands: Vec::new(),
            debug_lines: Vec::new(),
            debug_line_done: true,
            skipdisp: false,
            need_rebuild: false,
            top_index: 0,
            input_gen: self.input_gen,
            style: self.style.clone(),
            bg_color: self.bg_color,
            hl_color: self.hl_color,
            tooltip_color: self.tooltip_color,
            tooltip_bg_color: self.tooltip_bg_color,
            tooltip_delay_ms: self.tooltip_delay_ms,
            tooltip_duration_ms: self.tooltip_duration_ms,
            redraw_timer_ms: self.redraw_timer_ms,
            force_kana: self.force_kana,
            redraw_enabled: self.redraw_enabled,
            mes_skip: self.mes_skip,
            max_log: self.max_log,
            printc_width: self.printc_width,
            drawable_cells: self.drawable_cells,
            default_color: self.default_color,
            widths: self.widths.clone(),
            images: self.images.clone(),
            cbg: self.cbg.clone(),
        }
    }

    /// Everything printed into this console as a flat line list, the pending
    /// line included when it has content — what a [`Self::sub_console`]
    /// produces. Emuera's `ConsoleDisplayLine[]` return of
    /// `html2DisplayLine` (`GameView/HtmlManager.cs:557-563`).
    pub fn into_lines(mut self) -> Vec<ConsoleLine> {
        if !self.last_line.is_empty() {
            self.lines.push_back(std::mem::take(&mut self.last_line));
        }
        self.lines.into()
    }

    /// HTML_PRINT_ISLAND: add `lines` to layer `layer`, above everything
    /// already on that layer and every lower one. Nothing is replaced — see
    /// the field's own comment for the corpus evidence — so an island lives
    /// until a `HTML_PRINT_ISLAND_CLEAR` takes its layer or all of them.
    /// Markup that produced nothing adds nothing, since an empty island can
    /// neither be seen nor be cleared separately.
    ///
    /// Under `skipdisp` nothing is stored, for the same reason no `PRINT`
    /// reaches the log: ISSKIP suppresses output, and an island is output.
    pub fn print_island(&mut self, layer: i64, lines: Vec<ConsoleLine>) {
        if self.skipdisp || lines.is_empty() {
            return;
        }
        // Keep the list sorted by layer with the prints to one layer in
        // order, so iterating it is painting it.
        let at = self.islands.partition_point(|(l, _)| *l <= layer);
        self.islands.insert(at, (layer, lines));
    }

    /// HTML_PRINT_ISLAND_CLEAR: one layer, or every layer when no layer was
    /// given. Both forms are live in the corpus — a popup clears exactly the
    /// two layers it printed so the caller's islands survive
    /// (`Data/ERB/関数/汎用組み込み関数/メッセージ/MESSAGE_POPUP.ERB:38-39`),
    /// while a screen that owns the display clears everything before
    /// reprinting (`Data/ERB/SHOW_STATUS/SHOW_STATUS_WINDOW.ERB:1111`).
    pub fn clear_islands(&mut self, layer: Option<i64>) {
        match layer {
            Some(layer) => self.islands.retain(|(l, _)| *l != layer),
            None => self.islands.clear(),
        }
    }

    /// Every island in paint order: lowest layer first, and within a layer
    /// the order it was printed in. One layer can appear more than once.
    pub fn islands(&self) -> impl Iterator<Item = (i64, &[ConsoleLine])> {
        self.islands.iter().map(|(layer, lines)| (*layer, lines.as_slice()))
    }

    /// Cells of one character: 0, 1 or 2.
    pub fn char_cells(&self, c: char) -> u8 {
        self.widths.char_cells(c)
    }

    /// Emuera `getStBar` (`GameView/EmueraConsole.Print.cs:632-649`) measured
    /// in half-width cells instead of pixels: repeat `unit` until it reaches
    /// the drawable width, then drop trailing characters while it exceeds it.
    /// `None` when `unit` has no width, where Emuera's loop never terminates.
    ///
    /// This is the bar Emuera bakes once at start-up from the configured
    /// `ウィンドウ幅` (`GameProc/Process.cs:117`), so it is what `DRAWLINESTR`
    /// reports. The GPU renderer re-fits `ConsoleLinePart::Line` to the *live*
    /// window instead (design spec §"Emuera lays out against the fixed config
    /// WindowX"), so a resized window draws a longer rule than this.
    pub fn bar_string(&self, unit: &str) -> Option<String> {
        // CUSTOMDRAWLINE may carry a `\n`; Emuera measures the raw string but
        // a newline has no width, so dropping it first is the same bar.
        let unit: String = unit.chars().filter(|&c| c != '\n').collect();
        let unit_cells = self.cells(&unit);
        if unit_cells == 0 {
            return None;
        }
        // Emuera appends one copy, then appends while the width is still under
        // the limit, so it ends on the first count whose width reaches it.
        let reps = self.drawable_cells.div_ceil(unit_cells);
        let mut bar = unit.repeat(reps);
        let mut width = unit_cells * reps;
        while width > self.drawable_cells {
            let Some(c) = bar.pop() else { break };
            width -= self.char_cells(c) as usize;
        }
        Some(bar)
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
            islands: &self.islands,
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

    /// `REDRAW <i>` — Emuera `EmueraConsole.SetRedraw`: bit 0 turns repainting
    /// on, and bit 1 additionally forces one repaint now. The caller performs
    /// that repaint when this answers `true`.
    #[must_use = "bit 1 asks for an immediate repaint the console cannot do itself"]
    pub fn set_redraw(&mut self, flags: i64) -> bool {
        self.redraw_enabled = flags & 1 != 0;
        flags & 2 != 0
    }

    /// Emuera resets this in `GotoTitle`, so `BEGIN TITLE` un-freezes a screen
    /// that `REDRAW 0` left behind.
    pub fn reset_redraw(&mut self) {
        self.redraw_enabled = true;
    }

    /// CURRENTREDRAW. `false` suppresses print-driven repaints; entering an
    /// input wait repaints anyway, as Emuera force-paints there
    /// (`EmueraConsole.cs:1184`).
    pub fn redraw_enabled(&self) -> bool {
        self.redraw_enabled
    }

    /// A front-end reports here that the user asked to fast-forward messages,
    /// and clears it when the wait it answered is over.
    pub fn set_mes_skip(&mut self, mes_skip: bool) {
        self.mes_skip = mes_skip;
    }

    /// MESSKIP / MOUSESKIP.
    pub fn mes_skip(&self) -> bool {
        self.mes_skip
    }

    /// TOOLTIP_SETCOLOR: set (or clear) the mouse-over tooltip colours. Emuera
    /// sets the foreground and background together (`EmueraConsole.cs:1733-1737`).
    pub fn set_tooltip_color(&mut self, fore: Option<Color>, back: Option<Color>) {
        self.tooltip_color = fore;
        self.tooltip_bg_color = back;
    }

    pub fn tooltip_color(&self) -> Option<Color> {
        self.tooltip_color
    }

    pub fn tooltip_bg_color(&self) -> Option<Color> {
        self.tooltip_bg_color
    }

    /// TOOLTIP_SETDELAY: tooltip show delay in milliseconds.
    pub fn set_tooltip_delay(&mut self, ms: u32) {
        self.tooltip_delay_ms = ms;
    }

    pub fn tooltip_delay(&self) -> u32 {
        self.tooltip_delay_ms
    }

    /// TOOLTIP_SETDURATION: tooltip display duration in milliseconds.
    pub fn set_tooltip_duration(&mut self, ms: u32) {
        self.tooltip_duration_ms = ms;
    }

    pub fn tooltip_duration(&self) -> u32 {
        self.tooltip_duration_ms
    }

    /// SETANIMETIMER: set the sprite-animation repaint interval.
    /// `EmueraConsole.setRedrawTimer` (`:629-640`) disables the timer for a
    /// non-positive tick count and clamps anything smaller than 10 ms up.
    pub fn set_redraw_timer(&mut self, ms: i32) {
        self.redraw_timer_ms = (ms > 0).then(|| ms.max(10) as u32);
    }

    /// FORCEKANA: the kana conversion applied to PRINTK-family output.
    pub fn set_force_kana(&mut self, mode: kana::ForceKana) {
        self.force_kana = mode;
    }

    pub fn force_kana(&self) -> kana::ForceKana {
        self.force_kana
    }

    /// DEBUGPRINT*/DEBUG output accumulation. These lines are NOT part of the
    /// normal console (`lines`/`last_line`); DEBUGCLEAR empties them.
    ///
    /// Emuera keeps the debug console in one flat `StringBuilder`: `DebugPrint`
    /// appends the text and `DebugNewLine` appends a line break
    /// (`EmueraConsole.cs:1837-1854`). `newline` therefore commits the pending
    /// line, and a bare DEBUGPRINT continues it.
    pub fn debug_print(&mut self, s: String, newline: bool) {
        match self.debug_lines.last_mut() {
            Some(last) if !self.debug_line_done => last.push_str(&s),
            _ => self.debug_lines.push(s),
        }
        self.debug_line_done = newline;
    }

    pub fn clear_debug(&mut self) {
        self.debug_lines.clear();
        self.debug_line_done = false;
    }

    pub fn debug_lines(&self) -> &[String] {
        &self.debug_lines
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

    /// PRINT_IMG / `<img>` with a resolved sprite: append the bitmap to the
    /// current line (Emuera `PrintImg`,
    /// `GameView/EmueraConsole.Print.cs:404-408`).
    ///
    /// An *unresolved* name never reaches here — Emuera prints the
    /// reconstructed tag as ordinary text instead
    /// (`GameView/ConsoleImagePart.cs:69-73`), so the caller prints
    /// [`InlineImage::alt_text`] through [`Self::print`].
    pub fn print_image(&mut self, image: Arc<InlineImage>) {
        if self.skipdisp {
            return;
        }
        self.last_line.push_image(image);
    }

    /// `<div>`: append a positioned box to the current line
    /// (`GameView/HtmlManager.cs:567`, which adds the `ConsoleDivPart` to the
    /// run being built). The box occupies no width, so nothing else about the
    /// line changes — including a pending `[`, which Emuera's own scan
    /// (running before parts exist) would also carry across it.
    pub fn print_div(&mut self, div: Arc<ConsoleDiv>) {
        if self.skipdisp {
            return;
        }
        self.last_line.parts.push(ConsoleLinePart::Div(div));
    }

    /// PRINT: every `\n` ends the current logical line, exactly like Emuera's
    /// `EmueraConsole.Print`, so LINECOUNT / CLEARLINE / ALIGNMENT see it.
    /// Empty segments push nothing (Emuera returns early on an empty string).
    ///
    /// The `\n`-free fast path and the split loop must push a segment the same
    /// way (`push_text` with the same generation and style): a one-segment
    /// string has to land identically whichever branch handled it.
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

    /// One button built from several independently styled runs, as Emuera's
    /// `ConsoleButtonString(console, css[])` (`HtmlManager.cs:743-769`) — the
    /// shape `HTML_PRINT` needs for `<button><b>ok</b>!</button>`.
    pub fn print_styled_button(&mut self, parts: Vec<(String, TextStyle)>, value: Value) {
        if self.skipdisp || parts.is_empty() {
            return;
        }
        self.last_line.button_start = None;
        self.last_line
            .parts
            .push(ConsoleLinePart::Button(parts, self.input_gen, value));
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
    /// INPUTMOUSEKEY — Emuera `InputType.PrimitiveMouseKey`. Not a value
    /// wait: it reports one raw mouse or key event and is answered with a
    /// [`MouseKeyEvent`], never a [`Value`].
    MouseKey,
}

/// The event INPUTMOUSEKEY reports, laid out exactly as Emuera's
/// `Process.InputResult5` writes it into `RESULT` (`Process.cs:249-258`).
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct MouseKeyEvent {
    /// `RESULT:0` — 1 mouse press, 2 wheel, 3 key press, 4 time limit.
    pub kind: i64,
    /// `RESULT:1` — the `MouseButtons` value, the wheel delta, or the
    /// virtual key code.
    pub code: i64,
    /// `RESULT:2` — client X for a mouse event, `KeyData` for a key.
    pub x: i64,
    /// `RESULT:3` — client Y for a mouse event, measured from the *bottom*
    /// edge as Emuera does (`EmueraConsole.cs:983`).
    pub y: i64,
    /// `RESULT:4` — the button-map mask colour under the cursor, or `-1`.
    pub mask: i64,
    /// `RESULT:5` — the integer value of the console button pressed.
    pub button: i64,
    /// `RESULTS:0` — set instead of `button` when the console button pressed
    /// carried a string (`EmueraConsole.cs:1021-1025`).
    pub button_str: Option<String>,
}

impl MouseKeyEvent {
    /// The time limit ran out: Emuera reports kind 4 and zeroes the rest
    /// (`EmueraConsole.cs:744`), printing no timeout message.
    pub const TIMEOUT: Self = Self {
        kind: 4,
        code: 0,
        x: 0,
        y: 0,
        mask: 0,
        button: 0,
        button_str: None,
    };
}

/// The live input surface `GETKEY` / `GETKEYTRIGGERED` / `MOUSEX` / `MOUSEY`
/// read. Emuera reaches straight for the OS on every call — `GetKeyState`
/// (`_Library/WinInput.cs:9`) and `Cursor.Position`
/// (`EmueraConsole.GetMousePosition`, `GameView/EmueraConsole.cs:1981-1990`) —
/// which erars cannot do from the VM thread, so the front-end answers one
/// `QueryState` with the whole surface at once.
///
/// The keyboard is reported as `GetKeyState`'s two observable bits, per
/// virtual key: `down` is its high bit and `toggled` is its low bit. Both are
/// needed because `GETKEYTRIGGERED` latches the low bit between calls
/// (`Creator.Method.cs:6725-6734`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct InputState {
    /// Cursor X in client pixels, left origin.
    pub mouse_x: i64,
    /// Cursor Y in client pixels, measured from the *bottom* edge like every
    /// other Emuera mouse coordinate (`EmueraConsole.cs:1988`).
    pub mouse_y: i64,
    /// Bit `vk` set: virtual key `vk` is down.
    down: [u64; 4],
    /// Bit `vk` set: virtual key `vk`'s OS toggle bit is on.
    toggled: [u64; 4],
}

impl InputState {
    /// Virtual key codes are a byte, exactly the range Emuera accepts
    /// (`Creator.Method.cs:6722-6723` rejects anything outside `0..=255`).
    pub const KEYS: usize = 256;

    fn bit(keys: &[u64; 4], vk: u8) -> bool {
        keys[vk as usize / 64] & (1 << (vk % 64)) != 0
    }

    fn set_bit(keys: &mut [u64; 4], vk: u8, on: bool) {
        let (w, b) = (vk as usize / 64, 1u64 << (vk % 64));
        if on {
            keys[w] |= b;
        } else {
            keys[w] &= !b;
        }
    }

    /// `GetKeyState(vk) < 0`.
    pub fn is_down(&self, vk: u8) -> bool {
        Self::bit(&self.down, vk)
    }

    /// `GetKeyState(vk) & 1`.
    pub fn is_toggled(&self, vk: u8) -> bool {
        Self::bit(&self.toggled, vk)
    }

    pub fn set_down(&mut self, vk: u8, on: bool) {
        Self::set_bit(&mut self.down, vk, on);
    }

    /// Flip `vk`'s toggle bit, which Windows does on every key press.
    pub fn flip_toggled(&mut self, vk: u8) {
        let on = !Self::bit(&self.toggled, vk);
        Self::set_bit(&mut self.toggled, vk, on);
    }

    /// Every key released, keeping the toggle bits — what the front-end does
    /// when it loses the focus that was feeding it key events.
    pub fn release_all(&mut self) {
        self.down = [0; 4];
    }
}

/// The Win32 vocabulary INPUTMOUSEKEY reports in. Emuera hands WinForms
/// values straight to the script — `(int)MouseButtons` in `RESULT:1` for a
/// press, `(int)Keys.KeyCode` and `(int)Keys.KeyData` in `RESULT:1`/`RESULT:2`
/// for a key — so a front-end must speak these numbers or the scripts that
/// compare against them break.
pub mod win32 {
    /// `System.Windows.Forms.MouseButtons`.
    pub const MOUSE_LEFT: i64 = 0x0010_0000;
    pub const MOUSE_RIGHT: i64 = 0x0020_0000;
    pub const MOUSE_MIDDLE: i64 = 0x0040_0000;
    pub const MOUSE_X1: i64 = 0x0080_0000;
    pub const MOUSE_X2: i64 = 0x0100_0000;

    /// `Keys.Shift` / `Keys.Control` / `Keys.Alt`, the bits `KeyData` adds
    /// on top of the key code.
    pub const MOD_SHIFT: i64 = 0x0001_0000;
    pub const MOD_CONTROL: i64 = 0x0002_0000;
    pub const MOD_ALT: i64 = 0x0004_0000;

    /// One notch of a Win32 `WM_MOUSEWHEEL`.
    pub const WHEEL_DELTA: i64 = 120;

    pub const VK_BACK: i64 = 8;
    pub const VK_TAB: i64 = 9;
    pub const VK_RETURN: i64 = 13;
    pub const VK_SHIFT: i64 = 16;
    pub const VK_CONTROL: i64 = 17;
    pub const VK_MENU: i64 = 18;
    pub const VK_CAPITAL: i64 = 20;
    pub const VK_ESCAPE: i64 = 27;
    pub const VK_SPACE: i64 = 32;
    pub const VK_PRIOR: i64 = 33;
    pub const VK_NEXT: i64 = 34;
    pub const VK_END: i64 = 35;
    pub const VK_HOME: i64 = 36;
    pub const VK_LEFT: i64 = 37;
    pub const VK_UP: i64 = 38;
    pub const VK_RIGHT: i64 = 39;
    pub const VK_DOWN: i64 = 40;
    pub const VK_INSERT: i64 = 45;
    pub const VK_DELETE: i64 = 46;

    /// The virtual key that produces `c` on a US layout, which is what
    /// WinForms reports as `Keys.KeyCode`: letters and digits carry their
    /// ASCII uppercase value, and the punctuation keys have their own OEM
    /// codes.
    pub fn vk_from_char(c: char) -> Option<i64> {
        Some(match c {
            '\r' | '\n' => VK_RETURN,
            '\t' => VK_TAB,
            '\u{8}' => VK_BACK,
            '\u{1b}' => VK_ESCAPE,
            ' ' => VK_SPACE,
            '0'..='9' => c as i64,
            'a'..='z' => c as i64 - 0x20,
            'A'..='Z' => c as i64,
            ';' | ':' => 186,
            '+' | '=' => 187,
            ',' | '<' => 188,
            '-' | '_' => 189,
            '.' | '>' => 190,
            '/' | '?' => 191,
            '`' | '~' => 192,
            '[' | '{' => 219,
            '\\' | '|' => 220,
            ']' | '}' => 221,
            '\'' | '"' => 222,
            _ => return None,
        })
    }
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
/// (Emuera Process.ScriptProc.cs:118/135) — and the `\r` of a CRLF label with
/// it, which would otherwise stay in the button text as a 0-cell control char.
fn strip_newlines(text: String) -> String {
    if text.contains(['\n', '\r']) {
        text.replace(['\n', '\r'], "")
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
                ">[ 9]2022年10月08日 22:52:52 1일째 ",
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
    use crate::image::{ImageGeometry, ImageSampler, InlineSprite, Rect};
    use erars_ast::{Alignment, Value};

    const FORE: Color = Color([192, 192, 192]);

    fn config(encoding: &'static encoding_rs::Encoding) -> ConsoleConfig {
        ConsoleConfig {
            printc_width: 25,
            max_log: 500,
            // Emuera's defaults: (760 − max(2, 18/6)) / (18/2) = 84 cells.
            drawable_cells: 84,
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

        // ... and the reverse: a '[' inside the PRINTC item is not left pending
        // for later text to close (the doc promise at `push_forced_text`).
        let mut tx = jp();
        tx.printrc("[1");
        assert_eq!(tx.last_line.button_start, None);
        tx.print("] x".into());
        assert_eq!(
            tx.last_line.parts,
            vec![ConsoleLinePart::Text(format!("{}[1] x", spaces(23)), style())],
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

    fn colored(r: u8, g: u8, b: u8) -> TextStyle {
        TextStyle {
            color: Color([r, g, b]),
            ..style()
        }
    }

    fn image(name: &str) -> Arc<InlineImage> {
        let sprite = InlineSprite {
            sampler: ImageSampler::Single {
                bitmap: 1,
                src: Rect::new(0, 0, 4, 4),
            },
            width: 4,
            height: 4,
            pos_x: 0,
            pos_y: 0,
        };
        Arc::new(InlineImage {
            name: name.into(),
            button: None,
            mask: None,
            sprite,
            geometry: ImageGeometry::new(18, 4, 4, None, None, None),
            alt: format!("<img src='{name}'>"),
        })
    }

    /// Emuera builds a button out of the display parts it already has
    /// (`ConsoleButtonString(EmueraConsole, AConsoleDisplayPart[] strs)`,
    /// `GameView/ConsoleButtonString.cs:27-35`), so a `[` … `]` pair that
    /// spans a colour change keeps both colours. `ConsoleLinePart::Button`
    /// carries the same `(text, style)` list, so nothing about the model
    /// forces the flattening that used to happen here.
    #[test]
    fn a_merged_button_keeps_each_runs_own_style() {
        let mut tx = jp();
        tx.set_color(255, 0, 0);
        tx.print("[".into());
        tx.set_color(0, 255, 0);
        tx.print("1] pick".into());
        assert_eq!(
            tx.last_line.parts,
            vec![ConsoleLinePart::Button(
                vec![
                    ("[".into(), colored(255, 0, 0)),
                    ("1] pick".into(), colored(0, 255, 0)),
                ],
                0,
                Value::Int(1)
            )]
        );
    }

    /// Two buttons on one line, each closed by its own `]`, with the colour
    /// changing inside the first one: the run boundary is *inside* a button
    /// and the button boundary is *inside* a run, so both splits have to work
    /// at once.
    #[test]
    fn a_run_that_spans_two_buttons_is_split_between_them() {
        let mut tx = jp();
        tx.set_color(255, 0, 0);
        tx.print("a[".into());
        tx.set_color(0, 255, 0);
        tx.print("1] b [2] c".into());
        assert_eq!(
            tx.last_line.parts,
            vec![
                ConsoleLinePart::Button(
                    vec![
                        ("a[".into(), colored(255, 0, 0)),
                        ("1] b ".into(), colored(0, 255, 0)),
                    ],
                    0,
                    Value::Int(1)
                ),
                ConsoleLinePart::Button(
                    vec![("[2] c".into(), colored(0, 255, 0))],
                    0,
                    Value::Int(2)
                ),
            ]
        );
    }

    /// An image contributes no text to the `[` … `]` scan
    /// (`ConsoleLinePart::as_text`), but it is still on the line, and it is
    /// still *between* the two halves of the button's text: the merge keeps it
    /// there and cuts the button around it, so both halves stay clickable
    /// under the same value and the image itself is not. Emuera, scanning the
    /// raw string before any part exists, keeps the image inside the
    /// `ConsoleButtonString` (`GameView/PrintStringBuffer.cs:189-279`), which
    /// a `Button` of text runs cannot hold. A positioned `<div>` box rides the
    /// same path, and so does the `HTML_PRINT` route
    /// (`erars-vm/src/html.rs`'s `flush_run`).
    #[test]
    fn an_image_inside_a_button_splits_it_and_stays_where_it_was_printed() {
        let mut tx = jp();
        tx.print("[".into());
        tx.print_image(image("QUAD"));
        tx.print("1] pick".into());
        assert_eq!(
            tx.last_line.parts,
            vec![
                ConsoleLinePart::Button(vec![("[".into(), style())], 0, Value::Int(1)),
                ConsoleLinePart::Image(image("QUAD")),
                ConsoleLinePart::Button(vec![("1] pick".into(), style())], 0, Value::Int(1)),
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

    #[test]
    fn console_state_round_trip() {
        let mut tx = jp();

        // tooltip state (TOOLTIP_SETCOLOR/DELAY/DURATION)
        assert_eq!(tx.tooltip_color(), None);
        assert_eq!(tx.tooltip_bg_color(), None);
        assert_eq!(tx.tooltip_delay(), 500);
        assert_eq!(tx.tooltip_duration(), 3000);
        tx.set_tooltip_color(Some(Color([1, 2, 3])), Some(Color([4, 5, 6])));
        tx.set_tooltip_delay(250);
        tx.set_tooltip_duration(5000);
        assert_eq!(tx.tooltip_color(), Some(Color([1, 2, 3])));
        assert_eq!(tx.tooltip_bg_color(), Some(Color([4, 5, 6])));
        assert_eq!(tx.tooltip_delay(), 250);
        assert_eq!(tx.tooltip_duration(), 5000);

        // FORCEKANA state
        assert_eq!(tx.force_kana(), ForceKana::Off);
        tx.set_force_kana(ForceKana::Hiragana);
        assert_eq!(tx.force_kana(), ForceKana::Hiragana);

        // debug buffer accumulates text that never appears on the console;
        // a bare DEBUGPRINT continues the pending line, an L form ends it
        tx.debug_print("hid".into(), false);
        tx.debug_print("den1".into(), true);
        tx.debug_print("hidden2".into(), true);
        assert_eq!(tx.debug_lines(), &["hidden1", "hidden2"]);
        assert!(tx.line_is_empty());
        tx.clear_debug();
        assert!(tx.debug_lines().is_empty());

        // CLEARTEXTBOX targets the front-end's input field, never the console
        // log; the pending line and the history both survive it.

        // REDRAW's bit 0 is the paint gate and sticks until set again;
        // bit 1 forces one paint without enabling it.
        assert!(tx.redraw_enabled());
        assert!(!tx.set_redraw(0));
        assert!(!tx.redraw_enabled());
        assert!(tx.set_redraw(2));
        assert!(!tx.redraw_enabled());
        assert!(tx.set_redraw(3));
        assert!(tx.redraw_enabled());
        assert!(!tx.set_redraw(1));
        assert!(tx.redraw_enabled());
        // BEGIN TITLE is the only thing that resets it (`GotoTitle`).
        assert!(!tx.set_redraw(0));
        tx.reset_redraw();
        assert!(tx.redraw_enabled());

        // MESSKIP tracks a live fast-forward, not SKIPDISP
        assert!(!tx.mes_skip());
        tx.set_mes_skip(true);
        assert!(tx.mes_skip());
        assert!(!tx.skipdisp());
    }

    /// A sub-console is where HTML_PRINT_ISLAND and `<div>` build their
    /// content: it inherits everything that decides how a `PRINT` looks, and
    /// nothing about where the log currently is.
    #[test]
    fn a_sub_console_inherits_the_style_and_starts_empty() {
        let mut tx = jp();
        tx.print("logged".into());
        tx.new_line();
        tx.set_color(1, 2, 3);
        tx.set_bg_color(4, 5, 6);
        tx.set_style(FontStyle::BOLD);
        tx.set_font("MS Gothic".into());
        tx.set_align(Alignment::Center);
        tx.print_button("[1]".into(), Value::Int(1));

        let sub = tx.sub_console();
        assert_eq!(sub.lines_from(0).len(), 0, "the log does not come along");
        assert!(sub.line_is_empty());
        assert_eq!(sub.color(), u32::from(Color([1, 2, 3])));
        assert_eq!(sub.bg_color(), u32::from(Color([4, 5, 6])));
        assert_eq!(sub.style(), FontStyle::BOLD);
        assert_eq!(sub.font(), "MS Gothic");
        assert_eq!(
            sub.align(),
            Alignment::Left,
            "alignment is not inherited: the parse state starts at LEFT and only \
             a `<p align>` inside the box moves it (`HtmlManager.cs:236`, `:628`)"
        );
        assert_eq!(
            sub.input_gen, tx.input_gen,
            "a button printed in the box keys the same input generation"
        );

        // A sub-console never skips, even off a skipdisp parent: the markup
        // has to be built for its tag diagnostics to fire, exactly as Emuera
        // parses it before deciding what to draw. The suppression happens
        // where the result would land — `print_div` / `print_island`.
        tx.set_skipdisp(true);
        assert!(!tx.sub_console().skipdisp());
    }

    /// HTML_PRINT_ISLAND / HTML_PRINT_ISLAND_CLEAR keep their overlays in
    /// paint order, off the log entirely.
    #[test]
    fn islands_stack_beside_the_log() {
        let mut tx = jp();
        let island = |s: &str| {
            let mut sub = jp();
            sub.print(s.into());
            sub.into_lines()
        };
        let painted = |tx: &VirtualConsole| -> Vec<(i64, String)> {
            tx.islands().map(|(l, lines)| (l, lines[0].to_string())).collect()
        };

        assert_eq!(tx.islands().count(), 0);
        tx.print_island(2, island("top"));
        tx.print_island(-1, island("under"));
        tx.print_island(0, island("mid"));

        // Ascending layer number is paint order: the popup on `L_LAYER_NO`
        // covers the dimmer on `L_LAYER_NO - 1`
        // (`関数/汎用組み込み関数/メッセージ/MESSAGE_POPUP.ERB:22-35`).
        assert_eq!(
            painted(&tx),
            vec![(-1, "under".into()), (0, "mid".into()), (2, "top".into())]
        );
        // Nothing reached the log: not a line, not the line count.
        assert_eq!(tx.lines_from(0).len(), 0);
        assert_eq!(tx.line_count(), 0);

        // A second print to a layer stacks on top of the first instead of
        // replacing it, and lands under the layers above it
        // (`RPG/ダンジョンアタック/SYSTEM_DUNGEON.ERB:2630-2641`).
        tx.print_island(0, island("mid2"));
        assert_eq!(
            painted(&tx),
            vec![
                (-1, "under".into()),
                (0, "mid".into()),
                (0, "mid2".into()),
                (2, "top".into()),
            ]
        );

        // Markup that produced no lines adds nothing.
        tx.print_island(0, Vec::new());
        assert_eq!(tx.islands().count(), 4);

        // A clear takes the whole layer, both islands on it, and leaves the
        // rest standing.
        tx.clear_islands(Some(0));
        assert_eq!(painted(&tx), vec![(-1, "under".into()), (2, "top".into())]);
        tx.clear_islands(Some(99));
        assert_eq!(tx.islands().count(), 2, "an unused layer clears nothing");
        tx.clear_islands(None);
        assert_eq!(tx.islands().count(), 0);

        // ISSKIP suppresses an island exactly as it suppresses a PRINT.
        tx.set_skipdisp(true);
        tx.print_island(0, island("hidden"));
        assert_eq!(tx.islands().count(), 0);
    }

    /// A `<div>` is a part with no width: it draws at its own coordinate and
    /// leaves the line it was printed on untouched
    /// (`_Library/EvilMask/ConsoleDivPart.cs:47`, `:172-174`).
    #[test]
    fn a_box_part_takes_no_room_on_its_line() {
        let mut tx = jp();
        let mut inner = tx.sub_console();
        inner.print("inside".into());
        let div = Arc::new(ConsoleDiv::new(
            DivSpec {
                anchor: DivAnchor::LeftBottom,
                ..DivSpec::default()
            },
            18,
            inner.into_lines(),
        ));

        tx.print("a".into());
        tx.print_div(div.clone());
        tx.print("b".into());

        assert_eq!(tx.last_line.parts.len(), 3);
        let text: String = tx.last_line.parts.iter().map(|p| p.as_text()).collect();
        assert_eq!(text, "ab", "the box contributes no text to its line");
        assert!(
            matches!(tx.last_line.parts[1], ConsoleLinePart::Div(_)),
            "the box sits between them as its own part"
        );
        assert_eq!(tx.line_count(), 1, "and neither ends nor adds a line");
        assert_eq!(tx.last_line.to_string(), "a<div>inside</div>b");

        tx.set_skipdisp(true);
        tx.print_div(div);
        assert_eq!(tx.last_line.parts.len(), 3, "ISSKIP drops the box");
    }
}
