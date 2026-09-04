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
use erars_ui::div::edge;
use erars_ui::image::InlineImage;
use erars_ui::{Color, ConsoleDiv, ConsoleLine, ConsoleLinePart, DivAnchor, FontStyle, TextStyle};
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
    /// Flow rows and placed rows in one list, in paint order within each
    /// group: a positioned box's rows follow the rows of the line that
    /// printed it, so a nested box's rows follow its parent's.
    pub rows: Vec<Row>,
    /// Button fragments of every row, flow and placed alike; `ButtonRegion::row`
    /// indexes `rows`.
    pub buttons: Vec<ButtonRegion>,
    /// How many of `rows` are [`RowKind::Flow`] — the row count `View`
    /// scrolls and counts. Placed rows are not part of the flow.
    pub flow_rows: usize,
    /// Island overlays appended by [`layout_island_into`], which is also the
    /// highest [`Placement::slice`] in use.
    pub islands: usize,
}

/// One visual row. `line` indexes the `lines` slice given to [`layout`], or
/// the box's own `lines` for a placed row.
#[derive(Clone, Debug)]
pub struct Row {
    pub kind: RowKind,
    pub line: usize,
    /// `false` for a wrapped or residual-`\n` continuation row.
    pub logical_start: bool,
    /// Alignment offset in `PointX` space.
    pub x0: i32,
    /// Sum of the cluster boxes on this row.
    pub width: u32,
    pub clusters: Vec<PlacedCluster>,
    pub rects: Vec<Rect>,
    /// Inline images on this row, in print order. Drawn after `clusters` so
    /// an image overlaps the text it shares a row with, which is the order
    /// Emuera gets by drawing its escaped parts in a second pass
    /// (`GameView/EmueraConsole.cs:1596`).
    pub images: Vec<PlacedImage>,
}

/// Whether a row is part of the console flow or placed at a coordinate of its
/// own — Emuera's `ConsoleDisplayLine` versus a `ConsoleDivPart`'s children
/// (`_Library/EvilMask/ConsoleDivPart.cs:161-166`).
#[derive(Clone, Debug, PartialEq)]
pub enum RowKind {
    /// Row `n` of the console flow: scrolls with the log, counted by
    /// [`crate::draw::View`].
    Flow(usize),
    /// A row of a positioned `<div>` or of an island overlay: drawn at
    /// [`Placement`], never scrolled by itself, never counted.
    Placed(Box<Placement>),
}

impl Row {
    /// The row's placement, or `None` for a flow row.
    pub fn placement(&self) -> Option<&Placement> {
        match &self.kind {
            RowKind::Flow(_) => None,
            RowKind::Placed(p) => Some(p),
        }
    }

    /// Client x that `x0` and every cluster / rect / image x is measured
    /// from. A flow row adds the GDI overhang padding (`shift`, module docs);
    /// a placed row's origin is already a client position.
    pub fn base_x(&self, shift: u32) -> i32 {
        match &self.kind {
            RowKind::Flow(_) => shift as i32 + self.x0,
            RowKind::Placed(p) => p.x + self.x0,
        }
    }
}

/// What a [`Placement`]'s `y` is measured from — `ConsoleDivPart.DrawTo`'s two
/// rect forms (`_Library/EvilMask/ConsoleDivPart.cs:141-143`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlaceAnchor {
    /// [`DivAnchor::Relative`] (`:142`, `pointY + PointY`): the top of flow
    /// row `n`, the row the box was printed on, so the box scrolls with it
    /// and vanishes with it.
    Row(usize),
    /// [`DivAnchor::LeftTop`]: the console area's top-left corner.
    Top,
    /// [`DivAnchor::LeftBottom`]: the console area's bottom edge, so `y` is
    /// negative and measures up.
    ///
    /// DELIBERATE: the fork's rect is `MainPicBox.Height - PointY - Height`
    /// (`_Library/EvilMask/ConsoleDivPart.cs:143`), a *positive* `ypos`
    /// measured down from the box's own bottom. Every corpus site passes a
    /// negative one, built by
    /// `関数/汎用組み込み関数/入力関数/CONVERT_YPOS_TOP_TO_BUTTOM.ERB` as
    /// `L_YPOS + (L_BASE_YPOS + 100)` with
    /// `L_BASE_YPOS = -GET_HEIGHTLENS() * 100`, which only lands where the
    /// game means it under `view_h + ypos`.
    Bottom,
}

/// Where one placed row is drawn. Resolved to a screen y by
/// [`crate::draw::View::place_y`], which is the only place the anchor and the
/// scroll position meet.
#[derive(Clone, Debug, PartialEq)]
pub struct Placement {
    pub anchor: PlaceAnchor,
    /// Client x of the row's origin; `x0` and the cluster offsets are added
    /// to it exactly as `shift + x0` is for a flow row.
    pub x: i32,
    /// Row top relative to `anchor`.
    pub y: i32,
    /// Overlay z-slice: 0 for every box printed in the log, `k + 1` for entry
    /// `k` of `ConsoleFrame::islands` in paint order — the same layer number
    /// may repeat (`SYSTEM_DUNGEON.ERB:2630-2641` covers the view and then
    /// centres text inside that cover), so it is the position in the list,
    /// not the layer, that decides who is on top.
    pub slice: usize,
    /// Clip of the box's content (`:159`), in the same space as `x` / `y`.
    pub clip: Clip,
    /// The box model to paint before this row, on a box's first row only.
    pub decor: Option<BoxDecor>,
}

/// A box's content clip. `None` on an axis whose `<div>` gave no size: the
/// fork always has both (`GameView/HtmlManager.cs:1166-1169`), a newer
/// EvilMask build leaves the box unbounded there.
///
/// DELIBERATE: this is the box's own rect alone, never intersected with an
/// enclosing box's. Emuera is the same on the way in — `SetClip` with the
/// default `CombineMode.Replace` (`ConsoleDivPart.cs:148`, `:159`) — but it
/// calls `graph.ResetClip()` when the box is done (`:168`), so a parent's
/// remaining children are drawn *unclipped* after a nested box. erars keeps
/// the parent's clip for its own rows throughout, which is what the nesting
/// in `DIV_MESSAGE_LOG.ERB:61-71` expects.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Clip {
    /// `[left, right)` in client px.
    pub x: Option<(i32, i32)>,
    /// `[top, bottom)` relative to [`Placement::anchor`].
    pub y: Option<(i32, i32)>,
}

/// The CSS box painted under a `<div>`'s own content —
/// `Shape.BoxBorder.DrawBorder` (`_Library/EvilMask/Shape.cs:19-107`) over
/// the rect the margin leaves (`ConsoleDivPart.cs:145-150`).
#[derive(Clone, Debug, PartialEq)]
pub struct BoxDecor {
    /// Client x of the rect after the margin inset.
    pub x: i32,
    /// Its top, relative to [`Placement::anchor`].
    pub y: i32,
    pub w: u32,
    pub h: u32,
    /// Edge widths in [`edge`] order, drawn inside the rect.
    pub border: [i32; 4],
    /// `None` is Emuera's "no `bcolor`": the frame's fore colour
    /// (`Shape.cs:63`).
    pub border_color: [Option<Color>; 4],
    pub background: Option<Color>,
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

/// One inline image placed on a row — Emuera's `ConsoleImagePart` after
/// `CalcPointX`.
///
/// An image whose box leaves the row (`InlineImage::geometry.escapes`) simply
/// draws outside it. Emuera needs `ConsoleEscapedParts`
/// (`GameView/ConsoleButtonString.cs:141-150`,
/// `GameView/EmueraConsole.Print.cs:156-169`) because it repaints one line at
/// a time under a clip; erars draws the whole frame in one pass, so the same
/// pixels come out of drawing at `row_y + geometry.top` with no per-line
/// bookkeeping. The console still counts one line either way.
#[derive(Clone, Debug)]
pub struct PlacedImage {
    /// Row-relative x, before `x0` — Emuera `PointX`.
    pub x: i32,
    /// Layout advance in pixels (Emuera `Width`, always non-negative).
    pub w: u32,
    /// Index into `Layout::buttons`, when the image sits inside a button.
    pub button: Option<usize>,
    pub image: Arc<InlineImage>,
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
    // ceil(drawable_w / unit) without the `drawable_w + unit` overflow;
    // 0 when drawable_w == 0.
    let reps = g.drawable_w.div_ceil(unit);
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

/// Where the rows of the line being walked go: the console flow, or a
/// positioned box's own coordinate space.
enum RowSink<'a> {
    /// Numbered flow rows (`Layout::flow_rows`).
    Flow,
    /// Rows of one box, one `line_h` apart from its content origin —
    /// `ConsoleDivPart.DrawTo`'s `pointY += Config.LineHeight`
    /// (`_Library/EvilMask/ConsoleDivPart.cs:161-166`).
    Placed(&'a mut BoxCtx),
}

/// The cursor a box's rows are emitted with.
struct BoxCtx {
    anchor: PlaceAnchor,
    slice: usize,
    clip: Clip,
    /// Client x of every row's origin (the box's content origin).
    x: i32,
    /// Anchor-relative y of the *next* row.
    y: i32,
    /// Widest row emitted so far: the content extent an unbounded box's
    /// decoration wraps.
    extent_w: u32,
}

/// A `<div>` met while walking a line, laid out once that line's rows exist
/// (so the box knows the index, alignment and screen origin of the row it was
/// printed on).
struct PendingDiv<'a> {
    /// Index in `Layout::rows` of the row the part sits on.
    row: usize,
    /// The part's x on that row, before `x0`.
    x: i32,
    div: &'a ConsoleDiv,
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
    images: Vec<PlacedImage>,
    sink: RowSink<'a>,
    /// The `<div>` parts met so far, laid out by [`Self::finish`].
    pending: Vec<PendingDiv<'a>>,
}

impl<'a> LineBuilder<'a> {
    fn new(
        g: &'a Geometry,
        rules: LineRules,
        line: usize,
        align: Alignment,
        sink: RowSink<'a>,
    ) -> Self {
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
            images: Vec::new(),
            sink,
            pending: Vec::new(),
        }
    }

    /// Note one positioned box. It occupies no width — `Str = string.Empty`
    /// and an empty `SetWidth` (`_Library/EvilMask/ConsoleDivPart.cs:47`,
    /// `:176-178`) — so the pen does not move and the text after it prints as
    /// if the box were not there. The box itself is laid out by
    /// [`Self::finish`], once `out.rows[row]` exists.
    fn place_div(&mut self, div: &'a ConsoleDiv, out: &Layout) {
        self.pending.push(PendingDiv {
            row: out.rows.len(),
            x: self.x,
            div,
        });
    }

    /// Place one inline image. Emuera's `ConsoleImagePart` is non-divisible
    /// (`GameView/ConsoleImagePart.cs:148`), so `getDivideIndex` skips it and
    /// returns 0 (`GameView/PrintStringBuffer.cs:522-523`): the whole part
    /// moves to the next row when something precedes it, and is placed
    /// overflowing when it is the row's first part
    /// (`GameView/PrintStringBuffer.cs:240-247`). That is exactly the wrap
    /// test [`Self::place`] already uses, only with a pixel width instead of
    /// a cell-multiple one.
    ///
    /// `geometry.x_sub_pixel` is not threaded into a running accumulator:
    /// Emuera needs one because its text advances are fractional
    /// (`PrintStringBuffer.cs:425-448`), while every erars cluster box is an
    /// exact `cells * half_w` (module docs above), so `(int)(frac + integer)`
    /// is that integer and the accumulator can never change a placement.
    /// Emuera's own image part discards the incoming fraction too
    /// (`ConsoleImagePart.cs:156-157`).
    fn place_image(&mut self, image: &Arc<InlineImage>, out: &mut Layout) {
        let w = image.geometry.width;
        if self.x > 0 && self.x as u32 + w > self.g.drawable_w {
            self.break_row(out);
        }
        // A run of text may be open around the image (`<font>` spanning it);
        // its underline must not stretch across the bitmap, so close the run
        // rect here and let the next cluster start a new one.
        self.flush_run_rects();
        let button = match (self.button, self.frag) {
            (Some(_), Some((i, _))) => Some(i),
            (Some(_), None) => {
                let i = out.buttons.len();
                self.frag = Some((i, self.x));
                Some(i)
            }
            (None, _) => None,
        };
        self.images.push(PlacedImage {
            x: self.x,
            w,
            button,
            image: Arc::clone(image),
        });
        self.x += w as i32;
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
            glyphs: Arc::clone(&c.glyphs),
        });
        self.x += w as i32;
    }

    /// One rect per styled run per row, spanning its cluster boxes.
    fn flush_run_rects(&mut self) {
        let Some(run) = self.run.as_mut() else { return };
        let Some(start) = run.start.take() else {
            return;
        };
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
        let x0 = align_x0(self.align, self.g.content_w, width);
        let kind = match &mut self.sink {
            RowSink::Flow => {
                let n = out.flow_rows;
                out.flow_rows += 1;
                RowKind::Flow(n)
            }
            RowSink::Placed(ctx) => {
                let p = Placement {
                    anchor: ctx.anchor,
                    x: ctx.x,
                    y: ctx.y,
                    slice: ctx.slice,
                    clip: ctx.clip,
                    decor: None,
                };
                ctx.y += self.g.m.line_h as i32;
                ctx.extent_w = ctx.extent_w.max((x0 + width as i32).max(0) as u32);
                RowKind::Placed(Box::new(p))
            }
        };
        out.rows.push(Row {
            kind,
            line: self.line,
            logical_start: self.logical_start,
            x0,
            width,
            clusters: std::mem::take(&mut self.clusters),
            rects: std::mem::take(&mut self.rects),
            images: std::mem::take(&mut self.images),
        });
        self.logical_start = false;
        self.x = 0;
    }

    /// Every `ConsoleLine` yields at least one row (an empty line is a blank
    /// row), and every `<div>` on it is laid out after those rows, so a box's
    /// rows follow the row that printed it.
    fn finish(mut self, shaper: &mut Shaper, out: &mut Layout) {
        self.break_row(out);
        for p in std::mem::take(&mut self.pending) {
            emit_div(&p, self.g, self.rules, shaper, out);
        }
    }
}

/// Lay out one positioned box: its origin (`ConsoleDivPart.cs:141-143`), its
/// content rows one `line_h` apart from the content origin (`:161-166`), the
/// content clip (`:159`) and the box model painted under it (`:150`).
///
/// The origin is resolved against the row the box was printed on, which is
/// why this runs after that row exists: a `Relative` box needs its x
/// (alignment included) and, when the row is itself placed, its anchor.
fn emit_div(
    p: &PendingDiv<'_>,
    g: &Geometry,
    rules: LineRules,
    shaper: &mut Shaper,
    out: &mut Layout,
) {
    let div = p.div;
    let owner = &out.rows[p.row];
    // `PointX + xOffset` (`:142`): the client x the part itself sits at.
    let part_x = owner.base_x(g.m.shift) + p.x;
    let (owner_anchor, owner_y, slice) = match &owner.kind {
        RowKind::Flow(n) => (PlaceAnchor::Row(*n), 0, 0),
        // A box inside a box anchors to its parent row, which is already an
        // absolute client position, and paints in the parent's overlay slice.
        RowKind::Placed(pl) => (pl.anchor, pl.y, pl.slice),
    };
    let (anchor, x, y) = match div.anchor {
        DivAnchor::Relative => (owner_anchor, part_x + div.x, owner_y + div.y),
        DivAnchor::LeftTop => (PlaceAnchor::Top, div.x, div.y),
        DivAnchor::LeftBottom => (PlaceAnchor::Bottom, div.x, div.y),
    };

    let (cx, cy) = div.style.content_offset();
    let (inner_w, inner_h) = (div.inner_width(), div.inner_height());
    let mut ctx = BoxCtx {
        anchor,
        slice,
        clip: Clip {
            x: inner_w.map(|w| (x + cx, x + cx + w as i32)),
            y: inner_h.map(|h| (y + cy, y + cy + h as i32)),
        },
        x: x + cx,
        y: y + cy,
        extent_w: 0,
    };
    // `ButtonsToDisplayLines(.., SubDivisionWidth)` and `SetAlignment(align,
    // SubDivisionWidth, ..)` (`GameView/HtmlManager.cs:617-620`): the inner
    // width both wraps and aligns the children, with no `shift` padding —
    // that is the window's GDI overhang, not the box's.
    //
    // DELIBERATE: with no `width` the fork throws (`HtmlManager.cs:1166-1167`),
    // so there is no behaviour to copy for the 186 corpus boxes that omit it.
    // Unbounded means unwrapped (`drawable_w = u32::MAX`) and left-aligned
    // (`content_w = 0` clamps CENTER / RIGHT to 0 in `align_x0`).
    let child_g = match inner_w {
        Some(w) => Geometry {
            content_w: w,
            drawable_w: w,
            m: g.m,
        },
        None => Geometry {
            content_w: 0,
            drawable_w: u32::MAX,
            m: g.m,
        },
    };
    let first = out.rows.len();
    layout_lines(
        &div.lines,
        &child_g,
        rules,
        shaper,
        RowSink::Placed(&mut ctx),
        out,
    );

    let style = &div.style;
    let box_w = div
        .width
        .unwrap_or_else(|| (ctx.extent_w as i32 + style.edges_w()).max(0) as u32);
    let box_h = div.height.unwrap_or_else(|| {
        let content_h = ctx.y - (y + cy);
        (content_h + style.edges_h()).max(0) as u32
    });
    if !style.is_painted() {
        return;
    }
    // `rect` after the margin inset (`:145-148`), which is what
    // `BoxBorder.DrawBorder` fills and frames.
    let decor = BoxDecor {
        x: x + style.margin[edge::LEFT],
        y: y + style.margin[edge::TOP],
        w: (box_w as i32 - style.margin[edge::LEFT] - style.margin[edge::RIGHT]).max(0) as u32,
        h: (box_h as i32 - style.margin[edge::TOP] - style.margin[edge::BOTTOM]).max(0) as u32,
        border: style.border,
        border_color: style.border_color,
        background: style.background,
    };
    if out.rows.len() == first {
        // An empty box still paints its frame: give it a row to carry it.
        out.rows.push(Row {
            kind: RowKind::Placed(Box::new(Placement {
                anchor,
                x: ctx.x,
                y: ctx.y,
                slice,
                clip: ctx.clip,
                decor: None,
            })),
            line: 0,
            logical_start: true,
            x0: 0,
            width: 0,
            clusters: Vec::new(),
            rects: Vec::new(),
            images: Vec::new(),
        });
    }
    if let RowKind::Placed(pl) = &mut out.rows[first].kind {
        pl.decor = Some(decor);
    }
}

/// Walk `lines` into `out`, emitting their rows through `sink`.
fn layout_lines(
    lines: &[ConsoleLine],
    g: &Geometry,
    rules: LineRules,
    shaper: &mut Shaper,
    mut sink: RowSink<'_>,
    out: &mut Layout,
) {
    for (li, line) in lines.iter().enumerate() {
        let sink = match &mut sink {
            RowSink::Flow => RowSink::Flow,
            RowSink::Placed(ctx) => RowSink::Placed(ctx),
        };
        let mut b = LineBuilder::new(g, rules, li, line.align, sink);
        for part in &line.parts {
            match part {
                ConsoleLinePart::Text(s, style) => b.push_run(s, style, shaper, out),
                ConsoleLinePart::Line(s, style) => {
                    // DRAWLINE / CUSTOMDRAWLINE: Regular style, current colour
                    // (Emuera PrintBar); the console stores NORMAL too (T3).
                    let style = TextStyle {
                        font_style: FontStyle::NORMAL,
                        ..style.clone()
                    };
                    match rule_string(shaper, &style, s, g) {
                        Some(rule) => b.push_run(&rule, &style, shaper, out),
                        None => log::warn!("DRAWLINE string {s:?} has no width; skipped"),
                    }
                }
                ConsoleLinePart::Button(parts, input_gen, value) => {
                    b.begin_button(*input_gen, value);
                    for (s, style) in parts {
                        b.push_run(s, style, shaper, out);
                    }
                    b.end_button(out);
                }
                ConsoleLinePart::Image(image) => b.place_image(image, out),
                ConsoleLinePart::Div(div) => b.place_div(div, out),
            }
        }
        b.finish(shaper, out);
    }
}

/// Lay out `lines` (a `ConsoleFrame`'s lines, oldest first) at `g`.
/// Ends with `shaper.sweep()`, so the shape cache holds exactly these strings.
///
/// Callers that lay out several independent line sets against one shaper (the
/// app and the headless renderer both lay out the log and the one-line input
/// strip separately) must use [`layout_no_sweep`] for all but the last of
/// them, or the strip's sweep drops every log entry.
pub fn layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper) -> Layout {
    let out = layout_no_sweep(lines, g, shaper);
    shaper.sweep();
    out
}

/// One whole frame: the log's lines as flow rows, then every island overlay
/// in `islands` order, which *is* paint order (sorted by layer, print order
/// within a layer — the same layer may hold several islands). No
/// `shaper.sweep()` — both callers lay the input strip out against the same
/// shaper afterwards.
pub fn layout_frame_no_sweep(
    lines: &[ConsoleLine],
    islands: &[(i64, Vec<ConsoleLine>)],
    g: &Geometry,
    shaper: &mut Shaper,
) -> Layout {
    let mut out = layout_no_sweep(lines, g, shaper);
    for (_, lines) in islands {
        layout_island_into(&mut out, lines, g, shaper);
    }
    out
}

/// Append one `HTML_PRINT_ISLAND` overlay to `out`: its lines are placed like
/// a `LeftTop` box at `(0, 0)` — starting at the console area's top-left and
/// advancing one `line_h` per row — with no clip and no decoration, in an
/// overlay slice of its own, so each call stacks over the previous one.
///
/// In practice every corpus island wraps its content in an absolute `<div>`,
/// so this anchor only shows for bare island text.
pub fn layout_island_into(
    out: &mut Layout,
    lines: &[ConsoleLine],
    g: &Geometry,
    shaper: &mut Shaper,
) {
    let rules = LineRules::from_primary(shaper);
    out.islands += 1;
    let mut ctx = BoxCtx {
        anchor: PlaceAnchor::Top,
        slice: out.islands,
        clip: Clip::default(),
        x: 0,
        y: 0,
        extent_w: 0,
    };
    layout_lines(lines, g, rules, shaper, RowSink::Placed(&mut ctx), out);
}

/// [`layout`] without the trailing `shaper.sweep()`: the entries it touches
/// are marked as used by the current generation, so a later `sweep()` keeps
/// them alongside the ones of any other layout done in the same generation.
pub fn layout_no_sweep(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper) -> Layout {
    let rules = LineRules::from_primary(shaper);
    let mut out = Layout::default();
    layout_lines(lines, g, rules, shaper, RowSink::Flow, &mut out);
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
/// * ` place <row=<n>|top|bottom> x=<x> y=<y> [slice=<s>] [clipx=<a>..<b>]
///   [clipy=<a>..<b>]` on the same line, for a placed row: the anchor and the
///   position it resolves to (`y` is anchor-relative)
/// * `  decor x=<x> y=<y> w=<w> h=<h> [bg=RRGGBB] [border=<t>,<r>,<b>,<l>]
///   [bcolor=<RRGGBB|fg>×4]` (two-space indent), before the row's content
/// * `  <x>:<cells> "<text>" [c=RRGGBB] [s=<BIUS>] [btn=<i>]` (two-space indent)
/// * `  rect <underline|strike> x=<x> dy=<dy> h=<h> w=<w> [btn=<i>]`
/// * `btn <i> row=<r> x=<x> w=<w> gen=<gen> value=<Value as Debug>`
///
/// `c=` only when the colour differs from `default_fg`; `s=` only when the
/// style is not `NORMAL`. No font id, glyph id, `dx`, `dy` or `size_px`.
pub fn layout_snapshot(layout: &Layout, default_fg: [u8; 3]) -> String {
    use std::fmt::Write;
    let hex = |Color([r, g, b]): Color| format!("{r:02X}{g:02X}{b:02X}");
    let mut lines: Vec<String> = Vec::new();
    for (r, row) in layout.rows.iter().enumerate() {
        let mut head = format!(
            "row {r} line {}{} x0={} w={}",
            row.line,
            if row.logical_start { "" } else { "+" },
            row.x0,
            row.width
        );
        if let Some(p) = row.placement() {
            let anchor = match p.anchor {
                PlaceAnchor::Row(n) => format!("row={n}"),
                PlaceAnchor::Top => "top".to_owned(),
                PlaceAnchor::Bottom => "bottom".to_owned(),
            };
            let _ = write!(head, " place {anchor} x={} y={}", p.x, p.y);
            if p.slice != 0 {
                let _ = write!(head, " slice={}", p.slice);
            }
            if let Some((a, b)) = p.clip.x {
                let _ = write!(head, " clipx={a}..{b}");
            }
            if let Some((a, b)) = p.clip.y {
                let _ = write!(head, " clipy={a}..{b}");
            }
        }
        lines.push(head);
        if let Some(d) = row.placement().and_then(|p| p.decor.as_ref()) {
            let mut s = format!("  decor x={} y={} w={} h={}", d.x, d.y, d.w, d.h);
            if let Some(bg) = d.background {
                let _ = write!(s, " bg={}", hex(bg));
            }
            if d.border.iter().any(|&w| w > 0) {
                let [t, ri, b, l] = d.border;
                let _ = write!(s, " border={t},{ri},{b},{l}");
            }
            if d.border_color.iter().any(Option::is_some) {
                let c: Vec<String> = d
                    .border_color
                    .iter()
                    .map(|c| c.map_or_else(|| "fg".to_owned(), hex))
                    .collect();
                let _ = write!(s, " bcolor={}", c.join(","));
            }
            lines.push(s);
        }
        for c in &row.clusters {
            let mut s = format!("  {}:{} {:?}", c.x, c.cells, c.text.as_str());
            if c.color != default_fg {
                let _ = write!(
                    s,
                    " c={:02X}{:02X}{:02X}",
                    c.color[0], c.color[1], c.color[2]
                );
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
    use erars_ui::DivBox;

    /// Bundled Noto Sans Mono only (no system fonts, no locale) + the JP width table.
    fn shaper() -> Shaper {
        let chain = FontChain::from_files(&[bundled_font_path()], Language::Japanese);
        let widths = WidthTable::new(Language::Japanese.encoding());
        Shaper::new(chain, widths, metrics())
    }

    #[test]
    fn line_rules_follow_font_tables_with_fallback() {
        // MS Gothic: upem 256, post (−17, 19), OS/2 (66, 13) → rows 16 and 10
        // at 18 px (spec Component 5)
        assert_eq!(
            LineRules::compute(metrics(), 256.0, Some((-17, 19)), Some((66, 13))),
            LineRules {
                ul_dy: 16,
                ul_h: 1,
                st_dy: 10,
                st_h: 1
            }
        );
        // bundled Noto Sans Mono: upem 1000, post (−100, 50), OS/2 (322, 50)
        assert_eq!(
            LineRules::compute(metrics(), 1000.0, Some((-100, 50)), Some((322, 50))),
            LineRules {
                ul_dy: 17,
                ul_h: 1,
                st_dy: 9,
                st_h: 1
            }
        );
        // tables absent: uEmuera's font_px and font_px/2 − 1, 1 px thick
        assert_eq!(
            LineRules::compute(metrics(), 1000.0, None, None),
            LineRules {
                ul_dy: 18,
                ul_h: 1,
                st_dy: 8,
                st_h: 1
            }
        );
        // the real primary (bundled font) reproduces the Noto numbers
        assert_eq!(
            LineRules::from_primary(&mut shaper()),
            LineRules {
                ul_dy: 17,
                ul_h: 1,
                st_dy: 9,
                st_h: 1
            }
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
        let g = geometry(760); // drawable_w = 757

        // "-" × 85 = 765 px > 757 → trim one → 84 (spec: 84 `-` at the defaults)
        assert_eq!(rule_string(&mut sh, &style(), "-", &g).unwrap().len(), 84);
        // 5-cell unit "──-": 17 reps = 765 px → trailing "-" dropped → 16 reps + "──" = 756 px
        let r = rule_string(&mut sh, &style(), "──-", &g).unwrap();
        assert_eq!(r.chars().count(), 50);
        assert!(r.ends_with("──-──"), "{r:?}");
        // zero-width / empty rules cannot fill anything (Emuera's getStBar would loop forever)
        assert_eq!(rule_string(&mut sh, &style(), "", &g), None);
        assert_eq!(rule_string(&mut sh, &style(), "\u{0301}", &g), None);
        // a `\n` inside a CUSTOMDRAWLINE string is dropped before repeating:
        // "ab" at drawable 27 → "abab" → "aba"
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
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![])], 760),
            "row 0 line 0 x0=0 w=0"
        );
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

    #[test]
    fn mid_word_wrap_is_character_granular() {
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![text("abcdefghijkl")])], 93),
            r#"
row 0 line 0 x0=0 w=90
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "d"
  36:1 "e"
  45:1 "f"
  54:1 "g"
  63:1 "h"
  72:1 "i"
  81:1 "j"
row 1 line 0+ x0=0 w=18
  0:1 "k"
  9:1 "l"
"#
        );
    }

    /// 9 cells are used; the 2-cell `あ` would end at 99 > 90, so it moves whole.
    #[test]
    fn full_width_cluster_that_does_not_fit_moves_whole() {
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![text("abcdefghiあ")])], 93),
            r#"
row 0 line 0 x0=0 w=81
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "d"
  36:1 "e"
  45:1 "f"
  54:1 "g"
  63:1 "h"
  72:1 "i"
row 1 line 0+ x0=0 w=18
  0:2 "あ"
"#
        );
    }

    /// `print_plain("a\nb")` keeps the `\n` inside one Text part (T3); the row
    /// breaks there, the `\n` occupies no cells and the next row is a
    /// continuation (`+`).
    #[test]
    fn residual_newline_from_print_plain() {
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![text("a\nb")])], 760),
            r#"
row 0 line 0 x0=0 w=9
  0:1 "a"
row 1 line 0+ x0=0 w=9
  0:1 "b"
"#
        );
    }

    /// A *trailing* `\n` still finishes its row, so the line ends with an empty
    /// continuation row (`w=0`, no clusters). T9/T10 count rows, so this is
    /// pinned: `"a\n"` is two rows, not one.
    #[test]
    fn trailing_newline_emits_an_empty_continuation_row() {
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![text("a\n")])], 760),
            r#"
row 0 line 0 x0=0 w=9
  0:1 "a"
row 1 line 0+ x0=0 w=0
"#
        );
    }

    /// The shaper (T6) expands `\t` to 1-cell `" "` clusters up to the next
    /// multiple of 8 cells, counted from the start of the part's text:
    /// `a` + 7 spaces, `b` at 72.
    #[test]
    fn tab_expands_to_eight_cell_stops() {
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![text("a\tb")])], 760),
            r#"
row 0 line 0 x0=0 w=81
  0:1 "a"
  9:1 " "
  18:1 " "
  27:1 " "
  36:1 " "
  45:1 " "
  54:1 " "
  63:1 " "
  72:1 "b"
"#
        );
    }

    /// 45 px → 380 − 22 = 358 (Center), 760 − 45 = 715 (Right); 36 px → 362; 54 px → 353.
    #[test]
    fn center_and_right_offsets_use_windowx() {
        k9::snapshot!(
            snap(
                &[
                    line(Alignment::Center, vec![text("abcde")]),
                    line(Alignment::Right, vec![text("abcde")]),
                    line(Alignment::Center, vec![text("abcd")]),
                    line(Alignment::Center, vec![text("abcdef")]),
                ],
                760
            ),
            r#"
row 0 line 0 x0=358 w=45
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "d"
  36:1 "e"
row 1 line 1 x0=715 w=45
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "d"
  36:1 "e"
row 2 line 2 x0=362 w=36
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "d"
row 3 line 3 x0=353 w=54
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "d"
  36:1 "e"
  45:1 "f"
"#
        );
    }

    /// Emuera applies the alignment to every wrapped fragment: 93 − 90 = 3, 93 − 18 = 75.
    #[test]
    fn alignment_applies_to_every_wrapped_row() {
        k9::snapshot!(
            snap(&[line(Alignment::Right, vec![text("abcdefghijkl")])], 93),
            r#"
row 0 line 0 x0=3 w=90
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "d"
  36:1 "e"
  45:1 "f"
  54:1 "g"
  63:1 "h"
  72:1 "i"
  81:1 "j"
row 1 line 0+ x0=75 w=18
  0:1 "k"
  9:1 "l"
"#
        );
    }

    /// The rule keeps the colour but is forced to NORMAL (no `s=`, no rect).
    #[test]
    fn drawline_fills_drawable_width_in_normal_style() {
        let red_bold_underline = TextStyle {
            color: Color([255, 0, 0]),
            ..styled(FontStyle::BOLD | FontStyle::UNDERLINE)
        };
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![ConsoleLinePart::Line("-".into(), red_bold_underline)]
                )],
                93
            ),
            r#"
row 0 line 0 x0=0 w=90
  0:1 "-" c=FF0000
  9:1 "-" c=FF0000
  18:1 "-" c=FF0000
  27:1 "-" c=FF0000
  36:1 "-" c=FF0000
  45:1 "-" c=FF0000
  54:1 "-" c=FF0000
  63:1 "-" c=FF0000
  72:1 "-" c=FF0000
  81:1 "-" c=FF0000
"#
        );
    }

    /// 3-character, 5-cell rule at drawable 108 (12 cells): 3 reps = 135 px,
    /// trailing "-" dropped → 108 px, so the row ends inside a repetition.
    #[test]
    fn drawline_trims_a_partial_repetition() {
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![rule("──-")])], 111),
            r#"
row 0 line 0 x0=0 w=108
  0:2 "─"
  18:2 "─"
  36:1 "-"
  45:2 "─"
  63:2 "─"
  81:1 "-"
  90:2 "─"
"#
        );
    }

    /// The rule string is computed once against drawable_w (10 dashes) and laid
    /// out after the pending `abc`, so 7 dashes fit and 3 spill (Emuera 1.824,
    /// ButtonWrap=false).
    #[test]
    fn text_then_drawline_spills_to_next_row() {
        k9::snapshot!(
            snap(&[line(Alignment::Left, vec![text("abc"), rule("-")])], 93),
            r#"
row 0 line 0 x0=0 w=90
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "-"
  36:1 "-"
  45:1 "-"
  54:1 "-"
  63:1 "-"
  72:1 "-"
  81:1 "-"
row 1 line 0+ x0=0 w=27
  0:1 "-"
  9:1 "-"
  18:1 "-"
"#
        );
    }

    /// `unit == 0` (a combining-mark-only or empty rule) is skipped with a
    /// warning; the line still takes a blank row.
    #[test]
    fn drawline_with_zero_width_rule_is_skipped() {
        k9::snapshot!(
            snap(
                &[
                    line(Alignment::Left, vec![rule("\u{0301}")]),
                    line(Alignment::Left, vec![rule("")]),
                ],
                760
            ),
            r#"
row 0 line 0 x0=0 w=0
row 1 line 1 x0=0 w=0
"#
        );
    }

    /// Spec Component 5 at the Emuera defaults (760 → 757): 84 `-`;
    /// `abc` + DRAWLINE → row 1 = `abc` + 81 `-`, row 2 = 3 `-`.
    #[test]
    fn drawline_at_emuera_defaults() {
        let mut sh = shaper();
        let g = geometry(760);
        let l = layout(&[line(Alignment::Left, vec![rule("-")])], &g, &mut sh);
        assert_eq!(l.rows.len(), 1);
        let row = &l.rows[0];
        assert_eq!((row.clusters.len(), row.width), (84, 756));
        assert!(row.clusters.iter().all(|c| c.cells == 1 && c.text.as_str() == "-"));
        assert_eq!(row.clusters[83].x, 747);

        let l = layout(
            &[line(Alignment::Left, vec![text("abc"), rule("-")])],
            &g,
            &mut sh,
        );
        assert_eq!(l.rows.len(), 2);
        assert_eq!((l.rows[0].clusters.len(), l.rows[0].width), (84, 756));
        assert_eq!(l.rows[0].clusters[3].text.as_str(), "-");
        assert_eq!(
            (
                l.rows[1].clusters.len(),
                l.rows[1].width,
                l.rows[1].logical_start
            ),
            (3, 27, false)
        );
    }

    /// Three 8-cell PRINTC columns as the console pads them by cells (T3):
    /// `aa` right-aligned in cells 0–7, `あbc` (4 cells) in 8–15, `x` in 16–23.
    #[test]
    fn printc_columns_land_on_cell_boundaries() {
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![text("      aa    あbc       x")]
                )],
                760
            ),
            r#"
row 0 line 0 x0=0 w=216
  0:1 " "
  9:1 " "
  18:1 " "
  27:1 " "
  36:1 " "
  45:1 " "
  54:1 "a"
  63:1 "a"
  72:1 " "
  81:1 " "
  90:1 " "
  99:1 " "
  108:2 "あ"
  126:1 "b"
  135:1 "c"
  144:1 " "
  153:1 " "
  162:1 " "
  171:1 " "
  180:1 " "
  189:1 " "
  198:1 " "
  207:1 "x"
"#
        );
    }

    /// A button part wraps mid-text: each row gets its own fragment with the
    /// same generation and value; clusters carry the fragment index.
    #[test]
    fn button_fragments_across_a_wrap() {
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![text("ab"), button("[1] click", 3, Value::Int(1))]
                )],
                93
            ),
            r#"
row 0 line 0 x0=0 w=90
  0:1 "a"
  9:1 "b"
  18:1 "[" btn=0
  27:1 "1" btn=0
  36:1 "]" btn=0
  45:1 " " btn=0
  54:1 "c" btn=0
  63:1 "l" btn=0
  72:1 "i" btn=0
  81:1 "c" btn=0
row 1 line 0+ x0=0 w=9
  0:1 "k" btn=1
btn 0 row=0 x=18 w=72 gen=3 value=Int(1)
btn 1 row=1 x=0 w=9 gen=3 value=Int(1)
"#
        );
    }

    #[test]
    fn two_buttons_on_one_row() {
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![
                        button("[0]", 1, Value::Int(0)),
                        text(" "),
                        button("go", 1, Value::String("go".into())),
                    ]
                )],
                760
            ),
            r#"
row 0 line 0 x0=0 w=54
  0:1 "[" btn=0
  9:1 "0" btn=0
  18:1 "]" btn=0
  27:1 " "
  36:1 "g" btn=1
  45:1 "o" btn=1
btn 0 row=0 x=0 w=27 gen=1 value=Int(0)
btn 1 row=0 x=36 w=18 gen=1 value=String("go")
"#
        );
    }

    /// One rect per styled run per row. Bundled Noto Sans Mono at 18 px:
    /// underline dy = 15 + round(100·18/1000) = 17, strike dy = 15 −
    /// round(322·18/1000) = 9, 1 px each.
    #[test]
    fn underline_and_strike_rects_span_their_runs() {
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![
                        ConsoleLinePart::Text("ab".into(), styled(FontStyle::UNDERLINE)),
                        ConsoleLinePart::Text("cd".into(), styled(FontStyle::STRIKELINE)),
                        ConsoleLinePart::Text(
                            "ef".into(),
                            styled(
                                FontStyle::BOLD
                                    | FontStyle::ITALIC
                                    | FontStyle::UNDERLINE
                                    | FontStyle::STRIKELINE
                            )
                        ),
                    ]
                )],
                760
            ),
            r#"
row 0 line 0 x0=0 w=54
  0:1 "a" s=U
  9:1 "b" s=U
  18:1 "c" s=S
  27:1 "d" s=S
  36:1 "e" s=BIUS
  45:1 "f" s=BIUS
  rect underline x=0 dy=17 h=1 w=18
  rect strike x=18 dy=9 h=1 w=18
  rect underline x=36 dy=17 h=1 w=18
  rect strike x=36 dy=9 h=1 w=18
"#
        );
    }

    #[test]
    fn underlined_button_gets_one_rect_per_row() {
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![ConsoleLinePart::Button(
                        vec![("abcdefghijkl".into(), styled(FontStyle::UNDERLINE))],
                        2,
                        Value::Int(5)
                    )]
                )],
                93
            ),
            r#"
row 0 line 0 x0=0 w=90
  0:1 "a" s=U btn=0
  9:1 "b" s=U btn=0
  18:1 "c" s=U btn=0
  27:1 "d" s=U btn=0
  36:1 "e" s=U btn=0
  45:1 "f" s=U btn=0
  54:1 "g" s=U btn=0
  63:1 "h" s=U btn=0
  72:1 "i" s=U btn=0
  81:1 "j" s=U btn=0
  rect underline x=0 dy=17 h=1 w=90 btn=0
row 1 line 0+ x0=0 w=18
  0:1 "k" s=U btn=1
  9:1 "l" s=U btn=1
  rect underline x=0 dy=17 h=1 w=18 btn=1
btn 0 row=0 x=0 w=90 gen=2 value=Int(5)
btn 1 row=1 x=0 w=18 gen=2 value=Int(5)
"#
        );
    }

    /// A button region's `x`/`w` are what `app.rs` hit-tests: draw x is
    /// `shift + x0 + x`, so with Right alignment the region moves with `x0`.
    #[test]
    fn button_regions_follow_alignment_offset() {
        let mut sh = shaper();
        let g = geometry(760);
        let l = layout(
            &[line(
                Alignment::Right,
                vec![text("AB"), button("[1] ", 7, Value::Int(1))],
            )],
            &g,
            &mut sh,
        );
        assert_eq!(l.rows[0].x0, 760 - 54);
        assert_eq!(
            l.buttons,
            vec![ButtonRegion {
                row: 0,
                x: 18,
                w: 36,
                input_gen: 7,
                value: Value::Int(1)
            }]
        );
    }

    // -----------------------------------------------------------------------
    // Positioned `<div>` boxes (`_Library/EvilMask/ConsoleDivPart.cs`)
    // -----------------------------------------------------------------------

    fn div_part(
        anchor: DivAnchor,
        (x, y): (i32, i32),
        (width, height): (Option<u32>, Option<u32>),
        style: DivBox,
        lines: Vec<ConsoleLine>,
    ) -> ConsoleLinePart {
        ConsoleLinePart::Div(std::sync::Arc::new(ConsoleDiv {
            anchor,
            x,
            y,
            width,
            height,
            style,
            lines,
            // Only `HTML_GETPRINTEDSTR` reads the alt text; the layout never does.
            alt_head: String::new(),
        }))
    }

    /// One left-aligned line of plain text inside a box.
    fn div_line(s: &str) -> ConsoleLine {
        line(Alignment::Left, vec![text(s)])
    }

    /// The corpus case: a run of blank `PRINTL`s reserves the space and the
    /// box is lifted into it with a negative `ypos`
    /// (`PRINT_EVENT_PICTURE.ERB:12-70`). The box hangs off the row it was
    /// printed on (`ConsoleDivPart.cs:142`, `rect = (PointX + xOffset,
    /// pointY + PointY, ..)`), so its rows are placed at `row=<flow index>`
    /// with a negative anchor-relative `y`, and the part itself moves no pen
    /// (`:47`): the `"gh"` after it starts at 18, right after `"ab"`.
    #[test]
    fn relative_div_hangs_off_its_own_row_with_a_negative_ypos() {
        k9::snapshot!(
            snap(
                &[
                    line(Alignment::Left, vec![text("zz")]),
                    line(
                        Alignment::Left,
                        vec![
                            text("ab"),
                            div_part(
                                DivAnchor::Relative,
                                (5, -38),
                                (Some(100), Some(38)),
                                DivBox::default(),
                                vec![div_line("cd"), div_line("ef")],
                            ),
                            text("gh"),
                        ],
                    ),
                ],
                760,
            ),
            r#"
row 0 line 0 x0=0 w=18
  0:1 "z"
  9:1 "z"
row 1 line 1 x0=0 w=36
  0:1 "a"
  9:1 "b"
  18:1 "g"
  27:1 "h"
row 2 line 0 x0=0 w=18 place row=1 x=26 y=-38 clipx=26..126 clipy=-38..0
  0:1 "c"
  9:1 "d"
row 3 line 1 x0=0 w=18 place row=1 x=26 y=-19 clipx=26..126 clipy=-38..0
  0:1 "e"
  9:1 "f"
"#
        );
    }

    /// `display: absolute-lefttop` / `absolute-leftbottom`
    /// (`GameView/HtmlManager.cs:1155-1160`) measure from the console area's
    /// corners, so neither depends on the row that printed them.
    ///
    /// DELIBERATE: the bottom anchor is `view_h + ypos`, not the fork's
    /// `MainPicBox.Height - PointY - Height` (`ConsoleDivPart.cs:143`) — see
    /// `PlaceAnchor::Bottom`.
    #[test]
    fn absolute_divs_anchor_to_the_console_corners() {
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Right,
                    vec![
                        text("ab"),
                        div_part(
                            DivAnchor::LeftTop,
                            (40, 7),
                            (Some(60), Some(19)),
                            DivBox::default(),
                            vec![div_line("cd")],
                        ),
                        div_part(
                            DivAnchor::LeftBottom,
                            (40, -57),
                            (Some(60), Some(19)),
                            DivBox::default(),
                            vec![div_line("ef")],
                        ),
                    ],
                )],
                760,
            ),
            r#"
row 0 line 0 x0=742 w=18
  0:1 "a"
  9:1 "b"
row 1 line 0 x0=0 w=18 place top x=40 y=7 clipx=40..100 clipy=7..26
  0:1 "c"
  9:1 "d"
row 2 line 0 x0=0 w=18 place bottom x=40 y=-57 clipx=40..100 clipy=-57..-38
  0:1 "e"
  9:1 "f"
"#
        );
    }

    /// No `width` / `height`: unwrapped, left-aligned and unclipped on that
    /// axis, with the decoration wrapping the content extent — the newer
    /// EvilMask behaviour the corpus needs (186 of its 369 boxes give no
    /// `width`). The frame is `margin` in from the box origin and `border`
    /// thick (`ConsoleDivPart.cs:145-150`).
    #[test]
    fn unbounded_div_wraps_its_content_and_is_never_clipped() {
        let style = DivBox {
            margin: [1, 1, 1, 1],
            border: [2, 2, 2, 2],
            padding: [3, 3, 3, 3],
            border_color: [None, Some(Color([255, 0, 0])), None, None],
            background: Some(Color([0, 0, 255])),
        };
        // content offset 1+2+3 = 6 on both axes; edges 12 wide and 12 high.
        // Widest child "cdef" = 36 → box 48 wide; two rows = 38 high → 50.
        // The decoration rect drops the 1 px margin on every side: 46 × 48
        // at (30 + 1, 6 + 1).
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![div_part(
                        DivAnchor::LeftTop,
                        (30, 6),
                        (None, None),
                        style,
                        vec![div_line("cdef"), div_line("gh")],
                    )],
                )],
                760,
            ),
            r#"
row 0 line 0 x0=0 w=0
row 1 line 0 x0=0 w=36 place top x=36 y=12
  decor x=31 y=7 w=46 h=48 bg=0000FF border=2,2,2,2 bcolor=fg,FF0000,fg,fg
  0:1 "c"
  9:1 "d"
  18:1 "e"
  27:1 "f"
row 2 line 1 x0=0 w=18 place top x=36 y=31
  0:1 "g"
  9:1 "h"
"#
        );
    }

    /// The children are wrapped and aligned in `SubDivisionWidth`
    /// (`HtmlManager.cs:532-556`, `:617-620`) — the box width minus its box
    /// model, with no `shift` deduction — and the clip is the same rect
    /// (`ConsoleDivPart.cs:159`).
    #[test]
    fn div_children_wrap_and_align_inside_the_inner_width() {
        // width 60 − (2 + 2) padding = 56 inner: 6 half-width cells per row,
        // and a Center child is offset by 56/2 − w/2.
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![div_part(
                        DivAnchor::LeftTop,
                        (0, 0),
                        (Some(60), Some(60)),
                        DivBox {
                            padding: [2, 2, 2, 2],
                            ..DivBox::default()
                        },
                        vec![
                            line(Alignment::Left, vec![text("abcdefgh")]),
                            line(Alignment::Center, vec![text("ij")]),
                        ],
                    )],
                )],
                760,
            ),
            r#"
row 0 line 0 x0=0 w=0
row 1 line 0 x0=0 w=54 place top x=2 y=2 clipx=2..58 clipy=2..58
  0:1 "a"
  9:1 "b"
  18:1 "c"
  27:1 "d"
  36:1 "e"
  45:1 "f"
row 2 line 0+ x0=0 w=18 place top x=2 y=21 clipx=2..58 clipy=2..58
  0:1 "g"
  9:1 "h"
row 3 line 1 x0=19 w=18 place top x=2 y=40 clipx=2..58 clipy=2..58
  0:1 "i"
  9:1 "j"
"#
        );
    }

    /// A box inside a box: the inner `Relative` box hangs off its own row,
    /// which is already an absolute position, so it folds into the parent's
    /// anchor (`DIV_MESSAGE_LOG.ERB:61-71` stacks three deep this way). Its
    /// clip is its own rect only — see `Placement::clip`.
    #[test]
    fn nested_relative_div_folds_into_its_parents_anchor() {
        k9::snapshot!(
            snap(
                &[line(
                    Alignment::Left,
                    vec![div_part(
                        DivAnchor::LeftBottom,
                        (10, -100),
                        (Some(200), Some(80)),
                        DivBox::default(),
                        vec![
                            div_line("ab"),
                            line(
                                Alignment::Left,
                                vec![
                                    text("cd"),
                                    div_part(
                                        DivAnchor::Relative,
                                        (4, -19),
                                        (Some(50), None),
                                        DivBox::default(),
                                        vec![div_line("ef")],
                                    ),
                                ],
                            ),
                        ],
                    )],
                )],
                760,
            ),
            r#"
row 0 line 0 x0=0 w=0
row 1 line 0 x0=0 w=18 place bottom x=10 y=-100 clipx=10..210 clipy=-100..-20
  0:1 "a"
  9:1 "b"
row 2 line 1 x0=0 w=18 place bottom x=10 y=-81 clipx=10..210 clipy=-100..-20
  0:1 "c"
  9:1 "d"
row 3 line 0 x0=0 w=18 place bottom x=32 y=-100 clipx=32..82
  0:1 "e"
  9:1 "f"
"#
        );
    }

    /// `HTML_PRINT_ISLAND`: each layer is placed like a `LeftTop` box at
    /// `(0, 0)` in an overlay slice of its own, lowest layer first, and its
    /// buttons join the one flat region list the hit test walks.
    #[test]
    fn islands_are_placed_at_the_console_top_left_in_layer_order() {
        let mut sh = shaper();
        let g = geometry(760);
        let l = layout_frame_no_sweep(
            &[line(Alignment::Left, vec![text("log")])],
            &[
                (98, vec![div_line("dim")]),
                (
                    99,
                    vec![line(Alignment::Left, vec![button("[1]", 7, Value::Int(1))])],
                ),
            ],
            &g,
            &mut sh,
        );
        sh.sweep();
        assert_eq!((l.flow_rows, l.islands), (1, 2));
        k9::snapshot!(
            layout_snapshot(&l, FG),
            r#"
row 0 line 0 x0=0 w=27
  0:1 "l"
  9:1 "o"
  18:1 "g"
row 1 line 0 x0=0 w=27 place top x=0 y=0 slice=1
  0:1 "d"
  9:1 "i"
  18:1 "m"
row 2 line 0 x0=0 w=27 place top x=0 y=0 slice=2
  0:1 "[" btn=0
  9:1 "1" btn=0
  18:1 "]" btn=0
btn 0 row=2 x=0 w=27 gen=7 value=Int(1)
"#
        );
    }

    /// The corpus's own island shape: the island line holds nothing but an
    /// `absolute-leftbottom` box
    /// (`女神転生/ＭＡＧ/MAG_PORTRAIT.ERB:373`, `HTML_PRINT_ISLAND
    /// "<div display='absolute-leftbottom' xpos='0' ypos='…'>" + DRAWLINESTR +
    /// "</div>", MAIN_LAYER_NO`). The island decides only which overlay slice
    /// the content paints in; the box keeps its own anchor, so its row is
    /// `place bottom`, not the island's `place top`. Were the box's content
    /// dropped here, every island in eramegaten_p_kr would render empty.
    #[test]
    fn a_box_inside_an_island_keeps_its_own_anchor_and_the_islands_slice() {
        let mut sh = shaper();
        let g = geometry(760);
        let l = layout_frame_no_sweep(
            &[line(Alignment::Left, vec![text("log")])],
            &[(
                12,
                vec![line(
                    Alignment::Left,
                    vec![div_part(
                        DivAnchor::LeftBottom,
                        (0, -200),
                        (None, None),
                        DivBox::default(),
                        vec![div_line("ab")],
                    )],
                )],
            )],
            &g,
            &mut sh,
        );
        sh.sweep();
        assert_eq!((l.flow_rows, l.islands), (1, 1));
        k9::snapshot!(
            layout_snapshot(&l, FG),
            r#"
row 0 line 0 x0=0 w=27
  0:1 "l"
  9:1 "o"
  18:1 "g"
row 1 line 0 x0=0 w=0 place top x=0 y=0 slice=1
row 2 line 0 x0=0 w=18 place bottom x=0 y=-200 slice=1
  0:1 "a"
  9:1 "b"
"#
        );
    }
}

