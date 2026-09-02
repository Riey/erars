//! Row layout: `ConsoleLine` parts → rows of cell-aligned clusters, using
//! Emuera 1.824's pixel rules (design 2026-09-02, Component 5).
//!
//! * A cluster of `n` cells occupies `[x, x + n·half_w)` on its row; the font
//!   that draws it never moves it.
//! * Wrapping is character-granular at `drawable_w = content_w − shift`
//!   (Emuera `PointX + Width > DrawableWidth`, ButtonWrap=false).
//! * Alignment is Emuera's C# integer arithmetic on `content_w` (`WindowX`).
//! * DRAWLINE repeats its string until it reaches `drawable_w`, trims, and is
//!   laid out as ordinary text *after* the parts already on the line.
//! * Underline / strike rects come from the primary font's `post` / `OS/2`
//!   tables like GDI, with uEmuera's fixed rows as the fallback.
//! * Buttons are split per row into `ButtonRegion` fragments. Hover and the
//!   active input generation are not layout inputs (applied at draw time).
//!
//! All x positions are in Emuera's `PointX` space: the drawer adds
//! `shift + x0` (GDI overhang padding).

use std::sync::Arc;

use erars_ast::{Alignment, Value};
use erars_ui::{ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};
use smol_str::SmolStr;

use crate::text::{CellMetrics, Cluster, ShapedGlyph, Shaper};

/// Horizontal geometry of the console area, in physical pixels.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Geometry {
    /// Live window inner width (Emuera `WindowX`).
    pub content_w: u32,
    /// `content_w − m.shift` (Emuera `DrawableWidth`): rows wrap here.
    pub drawable_w: u32,
    pub m: CellMetrics,
}

impl Geometry {
    pub fn new(content_w: u32, m: CellMetrics) -> Self {
        Self {
            content_w,
            drawable_w: content_w.saturating_sub(m.shift),
            m,
        }
    }
}

/// Everything the drawer needs for one `ConsoleFrame` at one `Geometry`.
#[derive(Clone, Debug, Default)]
pub struct Layout {
    pub rows: Vec<Row>,
    pub buttons: Vec<ButtonRegion>,
}

/// One visual row. `line` indexes the `lines` slice given to [`layout`].
#[derive(Clone, Debug)]
pub struct Row {
    pub line: usize,
    /// `false` for a wrapped or residual-`\n` continuation row.
    pub logical_start: bool,
    /// Alignment offset in `PointX` space.
    pub x0: i32,
    /// Sum of the cluster boxes on this row.
    pub width: u32,
    pub clusters: Vec<PlacedCluster>,
    pub rects: Vec<Rect>,
}

#[derive(Clone, Debug)]
pub struct PlacedCluster {
    /// Row-relative x, before `x0`.
    pub x: i32,
    pub cells: u8,
    /// The cluster's source characters.
    pub text: SmolStr,
    pub color: [u8; 3],
    pub style: FontStyle,
    /// Index into `Layout::buttons`.
    pub button: Option<usize>,
    /// `dx` / `dy` relative to `(x, row_y)`.
    pub glyphs: Arc<[ShapedGlyph]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RectKind {
    Underline,
    Strike,
}

/// One underline / strike bar spanning one styled run on one row.
/// `x` is row-relative like `PlacedCluster::x`; `dy` is relative to the row top.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rect {
    pub kind: RectKind,
    pub x: i32,
    pub dy: i32,
    pub h: u32,
    pub w: u32,
    pub color: [u8; 3],
    pub button: Option<usize>,
}

/// One button fragment. A button part split across rows yields several
/// fragments with the same `input_gen` / `value`. `x` is row-relative, before
/// `x0`; the hit rect is `[shift + x0 + x, row_y, w + 1, min(font_px + 1, line_h)]`
/// (Emuera's inclusive test), evaluated in `app.rs`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ButtonRegion {
    pub row: usize,
    pub x: i32,
    pub w: u32,
    pub input_gen: u32,
    pub value: Value,
}

/// Emuera 1.824 `ConsoleDisplayLine.SetAlignment`: C# integer arithmetic on
/// `WindowX` (not `DrawableWidth`; Emuera.EM differs), clamped at 0.
fn align_x0(align: Alignment, content_w: u32, width: u32) -> i32 {
    let x0 = match align {
        Alignment::Left => 0,
        Alignment::Center => (content_w / 2) as i32 - (width / 2) as i32,
        Alignment::Right => content_w as i32 - width as i32,
    };
    x0.max(0)
}

/// Underline / strike placement for the current metrics, relative to the row
/// top (spec Component 5). GDI derives both from the font: underline from
/// `post.underlinePosition/Thickness`, strike from `OS/2.yStrikeoutPosition/Size`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct LineRules {
    ul_dy: i32,
    ul_h: u32,
    st_dy: i32,
    st_h: u32,
}

impl LineRules {
    /// Read the primary font's tables through the shaper's chain.
    fn from_primary(shaper: &mut Shaper) -> Self {
        let m = *shaper.metrics();
        let chain = shaper.chain();
        let primary = chain.primary();
        let font = chain.font(primary);
        // `rustybuzz::Face` derefs to `ttf_parser::Face` (underline_metrics,
        // strikeout_metrics); `units_per_em` is rustybuzz's inherent i32 accessor.
        let face = font.rustybuzz();
        let upem = face.units_per_em() as f32;
        let ul = face.underline_metrics().map(|l| (l.position, l.thickness));
        let st = face.strikeout_metrics().map(|l| (l.position, l.thickness));
        Self::compute(m, upem, ul, st)
    }

    /// `ul` / `st` are `(position, thickness)` in font units, or `None` when
    /// the table is absent. Fallbacks: underline at `font_px` (uEmuera),
    /// strike at `font_px / 2 − 1`, both 1 px.
    fn compute(m: CellMetrics, upem: f32, ul: Option<(i16, i16)>, st: Option<(i16, i16)>) -> Self {
        let px = m.font_px as f32;
        let scale = |v: i16| (v as f32 * px / upem).round();
        let (ul_dy, ul_h) = match ul {
            Some((pos, thick)) if upem > 0.0 => (
                m.baseline as i32 + (-(pos as f32) * px / upem).round() as i32,
                scale(thick).max(1.0) as u32,
            ),
            _ => (m.font_px as i32, 1),
        };
        let (st_dy, st_h) = match st {
            Some((pos, thick)) if upem > 0.0 => (
                m.baseline as i32 - scale(pos) as i32,
                scale(thick).max(1.0) as u32,
            ),
            _ => ((m.font_px / 2) as i32 - 1, 1),
        };
        Self {
            ul_dy,
            ul_h,
            st_dy,
            st_h,
        }
    }
}

/// Emuera `getStBar` (Print.cs:543-560): repeat `s` until its width reaches
/// `drawable_w`, then drop trailing characters while it still exceeds it.
/// Widths are cells·half_w, so trimming whole clusters is cell-identical to
/// Emuera's per-character trim. `None` when `s` has no width (Emuera would
/// loop forever). `\n` is removed first (CUSTOMDRAWLINE may carry one).
fn rule_string(shaper: &mut Shaper, style: &TextStyle, s: &str, g: &Geometry) -> Option<String> {
    let s: String = s.chars().filter(|&c| c != '\n').collect();
    if s.is_empty() {
        return None;
    }
    let unit_clusters = shaper.shape(&s, style);
    let half_w = g.m.half_w;
    let unit_cells: u32 = unit_clusters.iter().map(|c| c.cells as u32).sum();
    let unit = unit_cells * half_w;
    if unit == 0 {
        return None;
    }
    let reps = (g.drawable_w + unit - 1) / unit; // ceil; 0 when drawable_w == 0
    let mut pieces: Vec<(&str, u32)> = Vec::with_capacity(unit_clusters.len() * reps as usize);
    for _ in 0..reps {
        for c in unit_clusters.iter() {
            pieces.push((c.text.as_str(), c.cells as u32 * half_w));
        }
    }
    let mut width = reps * unit;
    while width > g.drawable_w {
        let Some((_, w)) = pieces.pop() else { break };
        width -= w;
    }
    Some(pieces.iter().map(|(t, _)| *t).collect())
}

/// The styled run currently being placed (for underline / strike rects).
struct RunState {
    style: FontStyle,
    color: [u8; 3],
    /// Row-relative x of the run's first cluster on the current row.
    start: Option<i32>,
}

/// Walks one `ConsoleLine` with a pixel cursor and emits rows into a `Layout`.
struct LineBuilder<'a> {
    g: &'a Geometry,
    rules: LineRules,
    line: usize,
    align: Alignment,
    logical_start: bool,
    /// Pixel cursor on the current row (sum of the boxes placed so far).
    x: i32,
    clusters: Vec<PlacedCluster>,
    rects: Vec<Rect>,
    run: Option<RunState>,
    /// The button part being walked: `(input_gen, value)`.
    button: Option<(u32, &'a Value)>,
    /// This row's fragment of that button: `(index into Layout::buttons, start x)`.
    frag: Option<(usize, i32)>,
}

impl<'a> LineBuilder<'a> {
    fn new(g: &'a Geometry, rules: LineRules, line: usize, align: Alignment) -> Self {
        Self {
            g,
            rules,
            line,
            align,
            logical_start: true,
            x: 0,
            clusters: Vec::new(),
            rects: Vec::new(),
            run: None,
            button: None,
            frag: None,
        }
    }

    /// Place one styled run. A `\n` (only reachable from the console paths
    /// that do not split: PRINTC/PRINTLC, PRINTPLAIN, PRINTSINGLE,
    /// CUSTOMDRAWLINE, REUSELASTLINE) finishes the row and continues on a
    /// continuation row; it occupies no cells and never reaches the shaper.
    fn push_run(&mut self, text: &str, style: &TextStyle, shaper: &mut Shaper, out: &mut Layout) {
        self.run = Some(RunState {
            style: style.font_style,
            color: style.color.0,
            start: None,
        });
        for (i, seg) in text.split('\n').enumerate() {
            if i > 0 {
                self.break_row(out);
            }
            if seg.is_empty() {
                continue;
            }
            let clusters = shaper.shape(seg, style);
            for c in clusters.iter() {
                self.place(c, style, out);
            }
        }
        self.flush_run_rects();
        self.run = None;
    }

    /// Character-granular wrapping: `x + w > drawable_w` with `x > 0` finishes
    /// the row and the cluster starts the next one (a full-width cluster moves
    /// whole; the first cluster of a row is always placed).
    fn place(&mut self, c: &Cluster, style: &TextStyle, out: &mut Layout) {
        let w = c.cells as u32 * self.g.m.half_w;
        if self.x > 0 && self.x as u32 + w > self.g.drawable_w {
            self.break_row(out);
        }
        let button = match (self.button, self.frag) {
            (Some(_), Some((i, _))) => Some(i),
            (Some(_), None) => {
                // The region is pushed when the fragment ends; nothing else can
                // push a region in between, so its index is known now.
                let i = out.buttons.len();
                self.frag = Some((i, self.x));
                Some(i)
            }
            (None, _) => None,
        };
        if let Some(run) = &mut self.run {
            if run.start.is_none() {
                run.start = Some(self.x);
            }
        }
        self.clusters.push(PlacedCluster {
            x: self.x,
            cells: c.cells,
            text: c.text.clone(),
            color: style.color.0,
            style: style.font_style,
            button,
            glyphs: Arc::from(&c.glyphs[..]),
        });
        self.x += w as i32;
    }

    /// One rect per styled run per row, spanning its cluster boxes.
    fn flush_run_rects(&mut self) {
        let Some(run) = self.run.as_mut() else { return };
        let Some(start) = run.start.take() else { return };
        let w = (self.x - start) as u32;
        if w == 0 {
            return;
        }
        let button = self.frag.map(|(i, _)| i);
        if run.style.contains(FontStyle::UNDERLINE) {
            self.rects.push(Rect {
                kind: RectKind::Underline,
                x: start,
                dy: self.rules.ul_dy,
                h: self.rules.ul_h,
                w,
                color: run.color,
                button,
            });
        }
        if run.style.contains(FontStyle::STRIKELINE) {
            self.rects.push(Rect {
                kind: RectKind::Strike,
                x: start,
                dy: self.rules.st_dy,
                h: self.rules.st_h,
                w,
                color: run.color,
                button,
            });
        }
    }

    fn begin_button(&mut self, input_gen: u32, value: &'a Value) {
        self.button = Some((input_gen, value));
    }

    fn end_button(&mut self, out: &mut Layout) {
        self.end_fragment(out);
        self.button = None;
    }

    /// Emit the `ButtonRegion` for this row's fragment (if any cluster landed).
    fn end_fragment(&mut self, out: &mut Layout) {
        if let (Some((i, start)), Some((input_gen, value))) = (self.frag.take(), self.button) {
            debug_assert_eq!(i, out.buttons.len());
            out.buttons.push(ButtonRegion {
                row: out.rows.len(),
                x: start,
                w: (self.x - start) as u32,
                input_gen,
                value: value.clone(),
            });
        }
    }

    /// Finish the current row (rects, button fragment, alignment) and start a
    /// continuation row.
    fn break_row(&mut self, out: &mut Layout) {
        self.flush_run_rects();
        self.end_fragment(out);
        let width = self.x.max(0) as u32;
        out.rows.push(Row {
            line: self.line,
            logical_start: self.logical_start,
            x0: align_x0(self.align, self.g.content_w, width),
            width,
            clusters: std::mem::take(&mut self.clusters),
            rects: std::mem::take(&mut self.rects),
        });
        self.logical_start = false;
        self.x = 0;
    }

    /// Every `ConsoleLine` yields at least one row (an empty line is a blank row).
    fn finish(mut self, out: &mut Layout) {
        self.break_row(out);
    }
}

/// Lay out `lines` (a `ConsoleFrame`'s lines, oldest first) at `g`.
/// Ends with `shaper.sweep()`, so the shape cache holds exactly these strings.
pub fn layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper) -> Layout {
    let rules = LineRules::from_primary(shaper);
    let mut out = Layout::default();
    for (li, line) in lines.iter().enumerate() {
        let mut b = LineBuilder::new(g, rules, li, line.align);
        for part in &line.parts {
            match part {
                ConsoleLinePart::Text(s, style) => b.push_run(s, style, shaper, &mut out),
                ConsoleLinePart::Line(s, style) => {
                    // DRAWLINE / CUSTOMDRAWLINE: Regular style, current colour
                    // (Emuera PrintBar); the console stores NORMAL too (T3).
                    let style = TextStyle {
                        font_style: FontStyle::NORMAL,
                        ..style.clone()
                    };
                    match rule_string(shaper, &style, s, g) {
                        Some(rule) => b.push_run(&rule, &style, shaper, &mut out),
                        None => log::warn!("DRAWLINE string {s:?} has no width; skipped"),
                    }
                }
                ConsoleLinePart::Button(parts, input_gen, value) => {
                    b.begin_button(*input_gen, value);
                    for (s, style) in parts {
                        b.push_run(s, style, shaper, &mut out);
                    }
                    b.end_button(&mut out);
                }
            }
        }
        b.finish(&mut out);
    }
    shaper.sweep();
    out
}

fn style_letters(style: FontStyle) -> String {
    let mut out = String::new();
    if style.contains(FontStyle::BOLD) {
        out.push('B');
    }
    if style.contains(FontStyle::ITALIC) {
        out.push('I');
    }
    if style.contains(FontStyle::UNDERLINE) {
        out.push('U');
    }
    if style.contains(FontStyle::STRIKELINE) {
        out.push('S');
    }
    out
}

/// Font-independent text form of a `Layout` (spec Component 7), one line per
/// row / cluster / rect / button, joined by `\n` without a trailing newline:
///
/// * `row <r> line <line>[+] x0=<x0> w=<width>` — `+` marks a continuation row
/// * `  <x>:<cells> "<text>" [c=RRGGBB] [s=<BIUS>] [btn=<i>]` (two-space indent)
/// * `  rect <underline|strike> x=<x> dy=<dy> h=<h> w=<w> [btn=<i>]`
/// * `btn <i> row=<r> x=<x> w=<w> gen=<gen> value=<Value as Debug>`
///
/// `c=` only when the colour differs from `default_fg`; `s=` only when the
/// style is not `NORMAL`. No font id, glyph id, `dx`, `dy` or `size_px`.
pub fn layout_snapshot(layout: &Layout, default_fg: [u8; 3]) -> String {
    use std::fmt::Write;
    let mut lines: Vec<String> = Vec::new();
    for (r, row) in layout.rows.iter().enumerate() {
        lines.push(format!(
            "row {r} line {}{} x0={} w={}",
            row.line,
            if row.logical_start { "" } else { "+" },
            row.x0,
            row.width
        ));
        for c in &row.clusters {
            let mut s = format!("  {}:{} {:?}", c.x, c.cells, c.text.as_str());
            if c.color != default_fg {
                let _ = write!(s, " c={:02X}{:02X}{:02X}", c.color[0], c.color[1], c.color[2]);
            }
            if !c.style.is_empty() {
                let _ = write!(s, " s={}", style_letters(c.style));
            }
            if let Some(b) = c.button {
                let _ = write!(s, " btn={b}");
            }
            lines.push(s);
        }
        for rect in &row.rects {
            let kind = match rect.kind {
                RectKind::Underline => "underline",
                RectKind::Strike => "strike",
            };
            let mut s = format!(
                "  rect {kind} x={} dy={} h={} w={}",
                rect.x, rect.dy, rect.h, rect.w
            );
            if let Some(b) = rect.button {
                let _ = write!(s, " btn={b}");
            }
            lines.push(s);
        }
    }
    for (i, b) in layout.buttons.iter().enumerate() {
        lines.push(format!(
            "btn {i} row={} x={} w={} gen={} value={:?}",
            b.row, b.x, b.w, b.input_gen, b.value
        ));
    }
    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Spec Testing §4: MS Gothic's 18 px geometry, pinned. The bundled Noto
    /// Sans Mono only supplies glyphs — it is Latin-only, so CJK and
    /// box-drawing characters resolve to `.notdef` — and every `x` / `cells` /
    /// `w` asserted below comes from the width classifier and these numbers,
    /// never from the font.
    fn metrics() -> CellMetrics {
        CellMetrics {
            scale: 1.0,
            font_px: 18,
            half_w: 9,
            line_h: 19,
            baseline: 15,
            shift: 3,
        }
    }

    fn geometry(content_w: u32) -> Geometry {
        Geometry::new(content_w, metrics())
    }

    use crate::font::{bundled_font_path, FontChain};
    use erars_compiler::Language;
    use erars_ui::width::WidthTable;

    /// Bundled Noto Sans Mono only (no system fonts, no locale) + the JP width table.
    fn shaper() -> Shaper {
        let chain = FontChain::from_files(&[bundled_font_path()], Language::Japanese);
        let widths = WidthTable::new(Language::Japanese.encoding());
        Shaper::new(chain, widths, metrics())
    }

    #[test]
    fn line_rules_follow_font_tables_with_fallback() {
        // MS Gothic: upem 256, post (−17, 19), OS/2 (66, 13) → rows 16 and 10 at 18 px (spec Component 5)
        assert_eq!(
            LineRules::compute(metrics(), 256.0, Some((-17, 19)), Some((66, 13))),
            LineRules { ul_dy: 16, ul_h: 1, st_dy: 10, st_h: 1 }
        );
        // bundled Noto Sans Mono: upem 1000, post (−100, 50), OS/2 (322, 50)
        assert_eq!(
            LineRules::compute(metrics(), 1000.0, Some((-100, 50)), Some((322, 50))),
            LineRules { ul_dy: 17, ul_h: 1, st_dy: 9, st_h: 1 }
        );
        // tables absent: uEmuera's font_px and font_px/2 − 1, 1 px thick
        assert_eq!(
            LineRules::compute(metrics(), 1000.0, None, None),
            LineRules { ul_dy: 18, ul_h: 1, st_dy: 8, st_h: 1 }
        );
        // the real primary (bundled font) reproduces the Noto numbers
        assert_eq!(
            LineRules::from_primary(&mut shaper()),
            LineRules { ul_dy: 17, ul_h: 1, st_dy: 9, st_h: 1 }
        );
    }

    use erars_ui::Color;

    /// Emuera's default ForeColor; clusters in this colour print no `c=`.
    const FG: [u8; 3] = [192, 192, 192];

    fn style() -> TextStyle {
        TextStyle {
            color: Color(FG),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        }
    }

    #[test]
    fn rule_string_grows_then_trims() {
        let mut sh = shaper();
        let g = geometry(760); // drawable 757
        // "-" × 85 = 765 px > 757 → trim one → 84 (spec: 84 `-` at the defaults)
        assert_eq!(rule_string(&mut sh, &style(), "-", &g).unwrap().len(), 84);
        // 5-cell unit "──-": 17 reps = 765 px → trailing "-" dropped → 16 reps + "──" = 756 px
        let r = rule_string(&mut sh, &style(), "──-", &g).unwrap();
        assert_eq!(r.chars().count(), 50);
        assert!(r.ends_with("──-──"), "{r:?}");
        // zero-width / empty rules cannot fill anything (Emuera's getStBar would loop forever)
        assert_eq!(rule_string(&mut sh, &style(), "", &g), None);
        assert_eq!(rule_string(&mut sh, &style(), "\u{0301}", &g), None);
        // a `\n` inside a CUSTOMDRAWLINE string is dropped before repeating: "ab" at drawable 27 → "abab" → "aba"
        assert_eq!(
            rule_string(&mut sh, &style(), "a\nb", &geometry(30)).as_deref(),
            Some("aba")
        );
    }

    #[test]
    fn geometry_and_alignment_use_emuera_integer_arithmetic() {
        // DrawableWidth = WindowX − max(2, FontSize/6) = 760 − 3
        assert_eq!(geometry(760).drawable_w, 757);
        assert_eq!(geometry(2).drawable_w, 0);
        // CENTER: WindowX/2 − width/2, both integer divisions (spec Component 5)
        assert_eq!(align_x0(Alignment::Center, 760, 44), 358);
        assert_eq!(align_x0(Alignment::Center, 760, 45), 358);
        assert_eq!(align_x0(Alignment::Center, 760, 46), 357);
        // RIGHT: WindowX − width
        assert_eq!(align_x0(Alignment::Right, 760, 45), 715);
        assert_eq!(align_x0(Alignment::Left, 760, 45), 0);
        // clamped at 0 when the row is wider than the window
        assert_eq!(align_x0(Alignment::Right, 30, 40), 0);
        assert_eq!(align_x0(Alignment::Center, 30, 40), 0);
    }

    fn styled(font_style: FontStyle) -> TextStyle {
        TextStyle {
            font_style,
            ..style()
        }
    }

    fn text(s: &str) -> ConsoleLinePart {
        ConsoleLinePart::Text(s.to_owned(), style())
    }

    fn rule(s: &str) -> ConsoleLinePart {
        ConsoleLinePart::Line(s.to_owned(), style())
    }

    fn button(s: &str, input_gen: u32, value: Value) -> ConsoleLinePart {
        ConsoleLinePart::Button(vec![(s.to_owned(), style())], input_gen, value)
    }

    fn line(align: Alignment, parts: Vec<ConsoleLinePart>) -> ConsoleLine {
        ConsoleLine {
            align,
            button_start: None,
            parts,
        }
    }

    fn snap(lines: &[ConsoleLine], content_w: u32) -> String {
        let mut sh = shaper();
        layout_snapshot(&layout(lines, &geometry(content_w), &mut sh), FG)
    }

    #[test]
    fn empty_line_is_one_row() {
        k9::snapshot!(snap(&[line(Alignment::Left, vec![])], 760), "row 0 line 0 x0=0 w=0");
    }

    #[test]
    fn plain_text_colour_and_style_tags() {
        let red_bold = TextStyle {
            color: Color([255, 0, 0]),
            ..styled(FontStyle::BOLD)
        };
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![text("ab"), ConsoleLinePart::Text("c".into(), red_bold)]
                )],
                760
            ),
            r#"
row 0 line 0 x0=0 w=27
  0:1 "a"
  9:1 "b"
  18:1 "c" c=FF0000 s=B
"#
        );
    }

    /// `あ` is not in the bundled font (it draws `.notdef`), but the JP width
    /// table gives it 2 cells, so `b` lands at 27 regardless of the glyph.
    #[test]
    fn full_width_cells_without_glyphs() {
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![text("aあb")])], 760),
            r#"
row 0 line 0 x0=0 w=36
  0:1 "a"
  9:2 "あ"
  27:1 "b"
"#
        );
    }
}
