//! Turn a [`Layout`] into GPU quads (spec Component 6, `draw.rs`).
//!
//! Hover is applied here, at draw time: every cluster and rect whose `button`
//! equals `hover` is drawn in `hl`; nothing moves (Emuera
//! `ConsoleStyledString.DrawTo(.., isSelecting, ..)` swaps the brush only).
//! Glyph quads sit on integer pixels at `(shift + x0 + x + dx + left,
//! row_y + dy − top)`; underline/strike rects use shader mode 0 at
//! `(shift + x0 + rect.x, row_y + rect.dy)`.
//!
//! Region lookup goes through [`RegionSource`], so bucketing, colouring and
//! the view arithmetic are unit-tested without a GPU via
//! [`build_instances_with`]; [`build_instances`] is the production entry point
//! backed by [`GlyphRaster`].

use std::sync::LazyLock;

use erars_ast::Alignment;
use erars_ui::cbg::{CbgImage, CbgLayer};
use erars_ui::div::edge;
use erars_ui::image::{BitmapId, ImageStore, Rect};
use erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};

use crate::gpu::Instance;
use crate::layout::{BoxDecor, Clip, Layout, PlaceAnchor, Placement, PlacedImage, Row, RowKind};
use crate::raster::{AtlasRegion, GlyphRaster, RasterKey};
use crate::text::{CellMetrics, ShapedGlyph, Shaper};

/// The input strip's line: `> {input}_` in the console's default colour
/// (spec Component 5 "View state"). Shared by the window and the headless
/// renderer so both strips are laid out from the same text.
pub fn input_line(input: &str, fg: [u8; 3]) -> ConsoleLine {
    ConsoleLine {
        align: Alignment::Left,
        button_start: None,
        parts: vec![ConsoleLinePart::Text(
            format!("> {input}_"),
            TextStyle {
                color: Color(fg),
                font_family: "".into(),
                font_style: FontStyle::NORMAL,
            },
        )],
    }
}

/// One frame's quads in paint order: the console-background plane behind the
/// text, the glyph and rect quads bucketed per atlas page, the inline images,
/// the plane's negative depths in front of everything, and the placed
/// overlays above them all.
///
/// The first four *are* Emuera's merged depth loop
/// (`GameView/EmueraConsole.cs:1557-1599`), which walks the CBG list and the
/// escaped parts together in descending depth and runs the whole text loop at
/// the dummy depth 0.
#[derive(Debug, Default)]
pub struct Quads {
    /// CBG entries with a positive `zdepth`, back to front.
    pub under: Vec<ImageBatch>,
    /// `glyphs[p]` samples atlas page `p`.
    pub glyphs: Vec<Vec<Instance>>,
    /// Inline `<img>` / `PRINT_IMG` batches, drawn after every glyph page:
    /// Emuera runs the whole `displayLineList` text loop for `depth == 0` and
    /// only then walks the escaped parts (`:1576-1598`).
    pub images: Vec<ImageBatch>,
    /// CBG entries with a negative `zdepth`: in front of the text.
    pub over: Vec<ImageBatch>,
    /// Positioned `<div>` boxes and island overlays, one z-slice per
    /// [`Placement::slice`], drawn in index order after everything above.
    ///
    /// DELIBERATE: Emuera draws a box while painting its own line
    /// (`ConsoleDivPart.DrawTo`, `_Library/EvilMask/ConsoleDivPart.cs:139`),
    /// so a *later* log line covers an earlier box, and a negative-depth CBG
    /// entry covers both. erars cannot: glyph instances are bucketed per atlas
    /// page, so their order is not row order. Drawing every placed box above
    /// the log instead is safe for the corpus, which always reserves the blank
    /// lines its boxes occupy (`PRINT_EVENT_PICTURE.ERB:12-70`), and is what
    /// an island overlay needs anyway.
    pub overlays: Vec<OverlayQuads>,
}

/// One overlay z-slice: its rect and glyph quads bucketed per atlas page, and
/// its image quads on top of them.
#[derive(Debug, Default)]
pub struct OverlayQuads {
    pub glyphs: Vec<Vec<Instance>>,
    pub images: Vec<ImageBatch>,
}

/// Consecutive image quads that sample the same bitmap, so a run of sprites
/// cropped from one sheet costs one bind group while keeping paint order.
#[derive(Debug, Clone, PartialEq)]
pub struct ImageBatch {
    pub bitmap: BitmapId,
    pub instances: Vec<Instance>,
}

impl Quads {
    /// Append `from`'s buckets to this frame's (growing the page list): how
    /// the input strip's quads join the log's.
    pub fn merge(&mut self, from: Quads) {
        merge_pages(&mut self.glyphs, from.glyphs);
        self.under.extend(from.under);
        self.images.extend(from.images);
        self.over.extend(from.over);
        for (i, slice) in from.overlays.into_iter().enumerate() {
            if self.overlays.len() <= i {
                self.overlays.resize_with(i + 1, OverlayQuads::default);
            }
            merge_pages(&mut self.overlays[i].glyphs, slice.glyphs);
            self.overlays[i].images.extend(slice.images);
        }
    }

    /// Every bitmap any image quad samples, once each, in first-use order —
    /// the upload list for `ImageTextures::sync`. The button map is not here:
    /// it is sampled on the CPU and never drawn.
    pub fn bitmaps(&self) -> Vec<BitmapId> {
        let mut out: Vec<BitmapId> = Vec::with_capacity(self.images.len());
        let overlays = self.overlays.iter().flat_map(|s| s.images.iter());
        for batch in self
            .under
            .iter()
            .chain(&self.images)
            .chain(&self.over)
            .chain(overlays)
        {
            if !out.contains(&batch.bitmap) {
                out.push(batch.bitmap);
            }
        }
        out
    }

    /// Push a quad onto one of the image layers, extending its last batch
    /// when that batch samples the same bitmap.
    fn push_image(layer: &mut Vec<ImageBatch>, bitmap: BitmapId, instance: Instance) {
        match layer.last_mut() {
            Some(last) if last.bitmap == bitmap => last.instances.push(instance),
            _ => layer.push(ImageBatch {
                bitmap,
                instances: vec![instance],
            }),
        }
    }

    /// The slice `i`'s buckets, creating it (and its rect bucket 0) on demand.
    fn slice(&mut self, i: usize, pages: usize) -> OverlayTarget<'_> {
        if self.overlays.len() <= i {
            self.overlays.resize_with(i + 1, OverlayQuads::default);
        }
        let slice = &mut self.overlays[i];
        if slice.glyphs.is_empty() {
            slice.glyphs.resize_with(pages.max(1), Vec::new);
        }
        OverlayTarget {
            glyphs: &mut slice.glyphs,
            images: &mut slice.images,
        }
    }

    /// Grow every glyph bucket list — the frame's and each overlay slice's —
    /// to `pages`, so all of them index the atlas pages 1:1. A slice created
    /// only to keep the z-order (or one whose page was added after it was
    /// built) would otherwise be short, and `GlyphRaster::pages_with`
    /// requires exactly one bucket per page.
    pub fn fit_pages(&mut self, pages: usize) {
        if self.glyphs.len() < pages {
            self.glyphs.resize_with(pages, Vec::new);
        }
        for slice in &mut self.overlays {
            if slice.glyphs.len() < pages {
                slice.glyphs.resize_with(pages, Vec::new);
            }
        }
    }
}

/// Append `from`'s per-page buckets to `into`, growing the page list.
fn merge_pages(into: &mut Vec<Vec<Instance>>, from: Vec<Vec<Instance>>) {
    for (page, list) in from.into_iter().enumerate() {
        if into.len() <= page {
            into.resize_with(page + 1, Vec::new);
        }
        into[page].extend(list);
    }
}

/// The buckets one row's quads are pushed into: the frame's own, or one
/// overlay slice's.
struct OverlayTarget<'a> {
    glyphs: &'a mut Vec<Vec<Instance>>,
    images: &'a mut Vec<ImageBatch>,
}

/// The image inputs of one frame: the published pixels, and the clock the
/// animated sprites are sampled at.
///
/// DELIBERATE: Emuera latches an animation's start time on its *first draw*
/// and measures from there (`Content/CroppedImage.cs:216-240`), so frame 0 is
/// always shown first. erars snapshots sprite geometry into the console line
/// at print time, so a printed image carries no identity a start time could be
/// latched against; `now_ms` is a monotonic clock instead. Frame *order* and
/// *timing* are identical — only the phase at the very first draw differs.
#[derive(Clone, Copy)]
pub struct ImageCtx<'a> {
    pub store: &'a ImageStore,
    pub now_ms: u64,
}

/// The store a front-end with no image layer draws from.
static NO_IMAGES: LazyLock<ImageStore> = LazyLock::new(ImageStore::new);

impl ImageCtx<'_> {
    /// No published pixels: every image quad resolves to nothing. What the
    /// GPU-free layout tests draw against.
    pub fn empty() -> Self {
        Self {
            store: &NO_IMAGES,
            now_ms: 0,
        }
    }
}

/// Which rows are on screen (spec Component 5, "View state").
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct View {
    /// Whole rows hidden below the bottom of the row area (0 = stuck to the bottom).
    pub scroll_rows: usize,
    /// Height of the row area in physical px (`window_h − strip_h`).
    pub view_h: u32,
    /// Height of the input strip below the row area (`line_h`; 0 when there is none).
    pub strip_h: u32,
}

impl View {
    /// Whole rows that fit in the row area (`view_h / line_h`).
    pub fn visible_rows(&self, line_h: u32) -> usize {
        if line_h == 0 {
            0
        } else {
            (self.view_h / line_h) as usize
        }
    }

    /// Screen y of row `r` of a layout with `rows` rows, or `None` when the row
    /// is off screen. Rows are bottom-anchored: `bottom_row = rows − 1 −
    /// scroll_rows` is drawn at `view_h − line_h`, row `r` at
    /// `view_h − (bottom_row − r + 1)·line_h` for `r ∈ (bottom_row − visible, bottom_row]`,
    /// so slack appears at the top.
    pub fn row_y(&self, rows: usize, r: usize, line_h: u32) -> Option<i32> {
        if rows == 0 || r >= rows {
            return None;
        }
        let bottom_row = rows - 1 - self.scroll_rows.min(rows - 1);
        if r > bottom_row {
            return None;
        }
        let below = bottom_row - r;
        if below >= self.visible_rows(line_h) {
            return None;
        }
        Some(self.view_h as i32 - (below as i32 + 1) * line_h as i32)
    }

    /// The view that draws a one-row layout inside the input strip: with
    /// `strip_h == line_h` its single row lands at `y = view_h`.
    pub fn strip(&self) -> View {
        View {
            scroll_rows: 0,
            view_h: self.view_h + self.strip_h,
            strip_h: 0,
        }
    }

    /// Screen y of a placed row: its anchor's base plus the row's own offset
    /// (`_Library/EvilMask/ConsoleDivPart.cs:141-143`). `None` when a
    /// `Relative` box hangs off a flow row that is not on screen — Emuera
    /// draws the box from that row's paint, so it is gone with it.
    pub fn place_y(&self, flow_rows: usize, p: &Placement, line_h: u32) -> Option<i32> {
        Some(self.anchor_y(flow_rows, p.anchor, line_h)? + p.y)
    }

    /// Screen y an anchor measures from.
    fn anchor_y(&self, flow_rows: usize, anchor: PlaceAnchor, line_h: u32) -> Option<i32> {
        match anchor {
            PlaceAnchor::Row(n) => self.row_y(flow_rows, n, line_h),
            PlaceAnchor::Top => Some(0),
            PlaceAnchor::Bottom => Some(self.view_h as i32),
        }
    }
}

/// Source of atlas regions for shaped glyphs — the seam that lets
/// [`build_instances_with`] run without a GPU.
pub trait RegionSource {
    /// Atlas pages that exist right now; buckets are pre-sized to it.
    fn page_count(&self) -> usize;
    /// Region for one glyph (rasterizing/uploading on demand); `None` for blank glyphs.
    fn region(&mut self, glyph: &ShapedGlyph) -> Option<AtlasRegion>;
}

/// The production [`RegionSource`]: a [`GlyphRaster`] fed by the shaper's font chain.
pub struct GpuRegions<'a> {
    pub raster: &'a mut GlyphRaster,
    pub device: &'a wgpu::Device,
    pub queue: &'a wgpu::Queue,
    pub shaper: &'a mut Shaper,
}

impl RegionSource for GpuRegions<'_> {
    fn page_count(&self) -> usize {
        self.raster.page_count()
    }

    fn region(&mut self, g: &ShapedGlyph) -> Option<AtlasRegion> {
        let key = RasterKey::new(g.font, g.glyph, g.size_px, g.flags);
        // Cache hit: no `Arc<Font>` lookup for the common case.
        if let Some(hit) = self.raster.lookup(&key) {
            return hit;
        }
        // Key the atlas on the face that is really rasterized: `font()`
        // substitutes the primary for a face that fails to load, and caching
        // that raster under the requested id would pin a wrong glyph there for
        // good. A substituted face costs one `font_with_id` per glyph per
        // frame; the raster itself is still cached under the primary.
        let (font, face) = self.shaper.chain().font_with_id(g.font);
        let key = RasterKey::new(face, g.glyph, g.size_px, g.flags);
        self.raster.get(self.device, self.queue, &font, key)
    }
}

fn rgba(c: [u8; 3]) -> [f32; 4] {
    [
        c[0] as f32 / 255.0,
        c[1] as f32 / 255.0,
        c[2] as f32 / 255.0,
        1.0,
    ]
}

/// Build one frame's quads for the rows of `layout` that `view` shows.
/// `hover` is an index into `layout.buttons`; its clusters and rects are drawn
/// in `hl` and its images swap to their `srcb` sprite. `fg` is the frame's
/// fore colour, which a box border with no `bcolor` is painted in
/// (`_Library/EvilMask/Shape.cs:63`). Feeds `raster.pages_with(&quads.glyphs)`
/// and `textures.pages_with(&quads.images)`.
#[allow(clippy::too_many_arguments)]
pub fn build_instances(
    layout: &Layout,
    view: &View,
    hover: Option<usize>,
    hl: [u8; 3],
    fg: [u8; 3],
    raster: &mut GlyphRaster,
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    shaper: &mut Shaper,
    images: ImageCtx<'_>,
) -> Quads {
    let m = *shaper.metrics();
    let mut src = GpuRegions {
        raster,
        device,
        queue,
        shaper,
    };
    build_instances_with(layout, view, hover, hl, fg, &m, &mut src, images)
}

/// GPU-agnostic core of [`build_instances`]. Solid rects (mode 0) go to
/// bucket 0 (page 0 always exists) and are pushed before the row's glyphs so
/// glyphs draw over their underline; a region on a page beyond
/// `src.page_count()` (a page created while building) grows the bucket list.
///
/// A cluster's glyph offsets (`dx`, `dy`) are relative to the cluster's own
/// box, not to a base glyph: the shaper re-places a merged 0-cell (combining)
/// cluster with a fresh pen inside its predecessor's box, so every glyph of a
/// cluster is positioned from `c.x` alike.
///
/// The flow rows come first, then every placed row in layout order — the
/// paint order [`Quads::overlays`] documents.
#[allow(clippy::too_many_arguments)]
pub fn build_instances_with(
    layout: &Layout,
    view: &View,
    hover: Option<usize>,
    hl: [u8; 3],
    fg: [u8; 3],
    m: &CellMetrics,
    src: &mut dyn RegionSource,
    images: ImageCtx<'_>,
) -> Quads {
    let pages = src.page_count().max(1);
    let mut out = Quads {
        glyphs: (0..pages).map(|_| Vec::new()).collect(),
        ..Quads::default()
    };
    for row in &layout.rows {
        let RowKind::Flow(n) = row.kind else { continue };
        let Some(row_y) = view.row_y(layout.flow_rows, n, m.line_h) else {
            continue;
        };
        let mut target = OverlayTarget {
            glyphs: &mut out.glyphs,
            images: &mut out.images,
        };
        row_quads(
            row,
            row.base_x(m.shift),
            row_y,
            &ScreenClip::NONE,
            hover,
            hl,
            src,
            images,
            &mut target,
        );
    }
    for row in &layout.rows {
        let Some(p) = row.placement() else { continue };
        let Some(row_y) = view.place_y(layout.flow_rows, p, m.line_h) else {
            continue;
        };
        // Everything else in the placement is anchor-relative like `p.y`.
        let anchor_y = row_y - p.y;
        let clip = ScreenClip::of(&p.clip, anchor_y);
        let mut target = out.slice(p.slice, pages);
        if let Some(d) = &p.decor {
            decor_quads(d, anchor_y, fg, target.glyphs);
        }
        row_quads(
            row,
            row.base_x(m.shift),
            row_y,
            &clip,
            hover,
            hl,
            src,
            images,
            &mut target,
        );
    }
    out
}

/// One row's rects, clusters and images at `(base_x, row_y)`, clipped to
/// `clip`.
#[allow(clippy::too_many_arguments)]
fn row_quads(
    row: &Row,
    base_x: i32,
    row_y: i32,
    clip: &ScreenClip,
    hover: Option<usize>,
    hl: [u8; 3],
    src: &mut dyn RegionSource,
    images: ImageCtx<'_>,
    out: &mut OverlayTarget<'_>,
) {
    for rect in &row.rects {
        let color = if hover.is_some() && rect.button == hover {
            hl
        } else {
            rect.color
        };
        let quad = Instance {
            rect: [
                (base_x + rect.x) as f32,
                (row_y + rect.dy) as f32,
                rect.w as f32,
                rect.h as f32,
            ],
            uv: [0.0; 4],
            color: rgba(color),
            mode: 0,
            _pad: [0; 3],
        };
        if let Some(quad) = clip.apply(quad) {
            out.glyphs[0].push(quad);
        }
    }
    for c in &row.clusters {
        let color = if hover.is_some() && c.button == hover {
            hl
        } else {
            c.color
        };
        for g in c.glyphs.iter() {
            let Some(reg) = src.region(g) else {
                continue;
            };
            let quad = Instance {
                rect: [
                    (base_x + c.x + g.dx + reg.left) as f32,
                    (row_y + g.dy - reg.top) as f32,
                    reg.size[0] as f32,
                    reg.size[1] as f32,
                ],
                uv: reg.uv,
                color: rgba(color),
                mode: if reg.color { 2 } else { 1 },
                _pad: [0; 3],
            };
            let Some(quad) = clip.apply(quad) else { continue };
            if reg.page >= out.glyphs.len() {
                out.glyphs.resize_with(reg.page + 1, Vec::new);
            }
            out.glyphs[reg.page].push(quad);
        }
    }
    for p in &row.images {
        let Some((bitmap, quad)) =
            image_quad(p, base_x, row_y, hover.is_some() && p.button == hover, images)
        else {
            continue;
        };
        if let Some(quad) = clip.apply(quad) {
            Quads::push_image(out.images, bitmap, quad);
        }
    }
}

/// A box's decoration as solid quads at `anchor_y`: the background over the
/// rect the margin leaves, then the four border edges inside it
/// (`ConsoleDivPart.cs:150` → `Shape.BoxBorder.DrawBorder`,
/// `_Library/EvilMask/Shape.cs:19-107`). It is painted before the box's own
/// content and needs no clip: the content clip is strictly inside this rect.
///
/// DELIBERATE: Emuera builds each edge from two polygons that meet the
/// adjacent edge diagonally (`Shape.cs:60-105`), so two borders of different
/// colours miter at the corner; these are full bands, left and right drawn
/// over top and bottom. The corner triangles are the only difference, and
/// only when adjacent edges differ. Rounded corners (`radius`, `Shape.cs:108`)
/// are not reproduced either: `DivBox` carries no radius, because nothing in
/// the corpus sets one.
fn decor_quads(d: &BoxDecor, anchor_y: i32, fg: [u8; 3], out: &mut [Vec<Instance>]) {
    let y = anchor_y + d.y;
    let solid = |x: i32, y: i32, w: i32, h: i32, c: [u8; 3]| Instance {
        rect: [x as f32, y as f32, w as f32, h as f32],
        uv: [0.0; 4],
        color: rgba(c),
        mode: 0,
        _pad: [0; 3],
    };
    let (w, h) = (d.w as i32, d.h as i32);
    if w <= 0 || h <= 0 {
        return;
    }
    if let Some(Color(bg)) = d.background {
        out[0].push(solid(d.x, y, w, h, bg));
    }
    let color = |i: usize| d.border_color[i].map_or(fg, |Color(c)| c);
    let bands = [
        (edge::TOP, [d.x, y, w, d.border[edge::TOP]]),
        (
            edge::BOTTOM,
            [d.x, y + h - d.border[edge::BOTTOM], w, d.border[edge::BOTTOM]],
        ),
        (edge::LEFT, [d.x, y, d.border[edge::LEFT], h]),
        (
            edge::RIGHT,
            [d.x + w - d.border[edge::RIGHT], y, d.border[edge::RIGHT], h],
        ),
    ];
    for (i, [bx, by, bw, bh]) in bands {
        if bw > 0 && bh > 0 {
            out[0].push(solid(bx, by, bw, bh, color(i)));
        }
    }
}


/// One `ConsoleImagePart.DrawTo` as a mode-2 quad
/// (`GameView/ConsoleImagePart.cs:194-215`): the destination box is
/// `destRect` offset by the part's `PointX + DrawingParam_ShapePositionShift`
/// (`base_x + p.x`) and by the row top, and the source window becomes the UV
/// rect.
fn image_quad(
    p: &PlacedImage,
    base_x: i32,
    row_y: i32,
    selecting: bool,
    images: ImageCtx<'_>,
) -> Option<(BitmapId, Instance)> {
    let (bitmap, dest, src) = p.image.draw_rects(selecting, images.now_ms)?;
    sprite_quad(bitmap, dest, src, base_x + p.x, row_y, images)
}

/// The console-background plane's two layers as image quads, in client pixels
/// — `OnPaint`'s CBG arm (`GameView/EmueraConsole.cs:1566-1576`).
///
/// `client_height` is the console area the plane's bottom-left origin is
/// measured against, and `selecting` is the button value under the cursor
/// (`-1` for none), which decides per entry whether `ImgB` is drawn.
/// Returns a [`Quads`] with only `under` and `over` filled, ready to
/// [`Quads::merge`] into the frame.
pub fn cbg_quads(
    cbg: &CbgLayer,
    client_height: i32,
    selecting: i32,
    images: ImageCtx<'_>,
) -> Quads {
    let mut out = Quads::default();
    cbg_layer_quads(cbg.background(), client_height, selecting, images, &mut out.under);
    cbg_layer_quads(cbg.foreground(), client_height, selecting, images, &mut out.over);
    out
}

/// One depth side of the plane, in list order (back to front).
fn cbg_layer_quads(
    entries: &[CbgImage],
    client_height: i32,
    selecting: i32,
    images: ImageCtx<'_>,
    out: &mut Vec<ImageBatch>,
) {
    for entry in entries {
        // `isButton && buttonValue == selectingCBGButtonInt` (`:1570`).
        let hit = entry.button.is_some_and(|b| b as i32 == selecting);
        let Some((bitmap, dest, src)) = entry.draw_rects(hit, images.now_ms, client_height) else {
            continue;
        };
        let Some((bitmap, instance)) = sprite_quad(bitmap, dest, src, 0, 0, images) else {
            continue;
        };
        if let Some(instance) = ScreenClip::below(client_height as f32).apply(instance) {
            Quads::push_image(out, bitmap, instance);
        }
    }
}

/// A rectangular clip on a quad, open on any side it does not name.
///
/// Emuera clips with `Graphics.SetClip` — the console-background plane by the
/// `MainPicBox` control it paints on, whose height *is* `ClientHeight`
/// (`GameView/EmueraConsole.cs:238`), and a positioned box by its own rect
/// (`_Library/EvilMask/ConsoleDivPart.cs:148`, `:159`). erars draws the whole
/// frame into one surface, so both edges have to be applied to the quads
/// themselves.
///
/// The clip is geometric: the destination-to-source mapping is affine, so
/// cutting the box and the UV window by the same fraction keeps every
/// surviving pixel sampling exactly what it sampled before. It is exact for
/// every quad kind here — solid rects (mode 0, no UV), glyph blits and image
/// blits are all unrotated.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ScreenClip {
    /// Inclusive left / top edge.
    pub min: [f32; 2],
    /// Exclusive right / bottom edge.
    pub max: [f32; 2],
}

impl ScreenClip {
    /// Open on all four sides.
    pub const NONE: ScreenClip = ScreenClip {
        min: [f32::NEG_INFINITY; 2],
        max: [f32::INFINITY; 2],
    };

    /// Only `y < client_height` — the console area's bottom edge.
    pub fn below(client_height: f32) -> ScreenClip {
        ScreenClip {
            max: [f32::INFINITY, client_height],
            ..ScreenClip::NONE
        }
    }

    /// A box's content clip at `anchor_y`: an axis the `<div>` gave no size
    /// for stays open.
    fn of(clip: &Clip, anchor_y: i32) -> ScreenClip {
        let mut out = ScreenClip::NONE;
        if let Some((a, b)) = clip.x {
            out.min[0] = a as f32;
            out.max[0] = b as f32;
        }
        if let Some((a, b)) = clip.y {
            out.min[1] = (anchor_y + a) as f32;
            out.max[1] = (anchor_y + b) as f32;
        }
        out
    }

    /// The part of `instance` inside the clip, with its UV window moved to
    /// match, or `None` when nothing of it survives.
    pub fn apply(&self, mut instance: Instance) -> Option<Instance> {
        for axis in 0..2 {
            let (pos, size) = (instance.rect[axis], instance.rect[axis + 2]);
            if size <= 0.0 {
                return None;
            }
            let lo = pos.max(self.min[axis]);
            let hi = (pos + size).min(self.max[axis]);
            if hi <= lo {
                return None;
            }
            if lo == pos && hi == pos + size {
                continue;
            }
            let (u, du) = (instance.uv[axis], instance.uv[axis + 2]);
            instance.rect[axis] = lo;
            instance.rect[axis + 2] = hi - lo;
            instance.uv[axis] = u + du * (lo - pos) / size;
            instance.uv[axis + 2] = du * (hi - lo) / size;
        }
        Some(instance)
    }
}

/// One sprite draw as a mode-2 quad: `dest` shifted by `(dx, dy)`, with
/// `src` as its UV rect.
///
/// A negative extent mirrors, which is why Emuera keeps `destRect` signed:
/// the quad is drawn on the normalised box with the UV axis reversed. A
/// negative *source* extent reverses it again, so the two cancel — the same
/// reading `Rect::normalized` gives the CPU blitter.
fn sprite_quad(
    bitmap: BitmapId,
    dest: Rect,
    src: Rect,
    dx: i32,
    dy: i32,
    images: ImageCtx<'_>,
) -> Option<(BitmapId, Instance)> {
    let bmp = images.store.get(bitmap)?;
    if bmp.width == 0 || bmp.height == 0 {
        return None;
    }

    let d = dest.normalized();
    let s = src.normalized();
    if d.width == 0 || d.height == 0 || s.width == 0 || s.height == 0 {
        return None;
    }
    let flip_x = (dest.width < 0) != (src.width < 0);
    let flip_y = (dest.height < 0) != (src.height < 0);

    let (tw, th) = (bmp.width as f32, bmp.height as f32);
    let (mut u, mut du) = (s.x as f32 / tw, s.width as f32 / tw);
    let (mut v, mut dv) = (s.y as f32 / th, s.height as f32 / th);
    if flip_x {
        u += du;
        du = -du;
    }
    if flip_y {
        v += dv;
        dv = -dv;
    }

    Some((
        bitmap,
        Instance {
            rect: [
                (dx + d.x) as f32,
                (dy + d.y) as f32,
                d.width as f32,
                d.height as f32,
            ],
            uv: [u, v, du, dv],
            // Mode 2 samples the texture straight; `color` is unused, and
            // white keeps it that way if the shader ever tints.
            color: [1.0; 4],
            mode: 2,
            _pad: [0; 3],
        },
    ))
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use cosmic_text::fontdb;
    use erars_ast::Value;
    use erars_ui::FontStyle;

    use super::*;
    use crate::flags::RasterFlags;
    use crate::layout::{ButtonRegion, PlacedCluster, Rect, RectKind, Row};

    const WHITE: [u8; 3] = [255, 255, 255];
    const HL: [u8; 3] = [255, 255, 0];

    /// Every glyph gets a 9×18 region whose page is `glyph % pages`.
    struct FakeRegions {
        pages: usize,
    }

    impl RegionSource for FakeRegions {
        fn page_count(&self) -> usize {
            self.pages
        }
        fn region(&mut self, g: &ShapedGlyph) -> Option<AtlasRegion> {
            Some(AtlasRegion {
                page: g.glyph as usize % self.pages,
                uv: [0.0, 0.0, 9.0 / 2048.0, 18.0 / 2048.0],
                size: [9, 18],
                left: 0,
                top: 15,
                color: false,
            })
        }
    }

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

    /// The glyph buckets of a build with no image layer, which is what every
    /// placement test asserts on.
    fn glyph_quads(
        layout: &Layout,
        view: &View,
        hover: Option<usize>,
        src: &mut dyn RegionSource,
    ) -> Vec<Vec<Instance>> {
        build_instances_with(
            layout,
            view,
            hover,
            HL,
            WHITE,
            &metrics(),
            src,
            ImageCtx::empty(),
        )
        .glyphs
    }

    fn glyph(id: u16) -> ShapedGlyph {
        ShapedGlyph {
            font: fontdb::ID::dummy(),
            glyph: id,
            dx: 0,
            dy: 15,
            size_px: 18.0,
            flags: RasterFlags::empty(),
        }
    }

    fn cluster(x: i32, text: &str, glyph_id: u16, button: Option<usize>) -> PlacedCluster {
        PlacedCluster {
            x,
            cells: 1,
            text: text.into(),
            color: WHITE,
            style: FontStyle::NORMAL,
            button,
            glyphs: Arc::from(vec![glyph(glyph_id)]),
        }
    }

    /// Row 0: `a` plain, `b` = button 0 with an underline rect; row 1: `c` = button 1.
    fn fake_layout() -> Layout {
        Layout {
            flow_rows: 2,
            islands: 0,
            rows: vec![
                Row {
                    kind: RowKind::Flow(0),
                    line: 0,
                    logical_start: true,
                    x0: 0,
                    width: 18,
                    clusters: vec![cluster(0, "a", 1, None), cluster(9, "b", 2, Some(0))],
                    rects: vec![Rect {
                        kind: RectKind::Underline,
                        x: 9,
                        dy: 16,
                        h: 1,
                        w: 9,
                        color: WHITE,
                        button: Some(0),
                    }],
                    images: Vec::new(),
                },
                Row {
                    kind: RowKind::Flow(1),
                    line: 1,
                    logical_start: true,
                    x0: 0,
                    width: 9,
                    clusters: vec![cluster(0, "c", 3, Some(1))],
                    rects: vec![],
                    images: Vec::new(),
                },
            ],
            buttons: vec![
                ButtonRegion {
                    row: 0,
                    x: 9,
                    w: 9,
                    input_gen: 1,
                    value: Value::Int(1),
                },
                ButtonRegion {
                    row: 1,
                    x: 0,
                    w: 9,
                    input_gen: 1,
                    value: Value::Int(2),
                },
            ],
        }
    }

    fn flat(pages: &[Vec<Instance>]) -> Vec<Instance> {
        pages.iter().flatten().copied().collect()
    }

    fn rgb(c: [u8; 3]) -> [f32; 4] {
        [
            c[0] as f32 / 255.0,
            c[1] as f32 / 255.0,
            c[2] as f32 / 255.0,
            1.0,
        ]
    }

    #[test]
    fn view_rows_are_bottom_anchored() {
        let v = View {
            scroll_rows: 0,
            view_h: 38,
            strip_h: 19,
        };
        assert_eq!(v.visible_rows(19), 2);
        assert_eq!(v.row_y(2, 0, 19), Some(0));
        assert_eq!(v.row_y(2, 1, 19), Some(19));
        // One row in a two-row area: slack at the top.
        assert_eq!(v.row_y(1, 0, 19), Some(19));
        // Three rows: the oldest is off screen.
        assert_eq!(v.row_y(3, 0, 19), None);
        assert_eq!(v.row_y(3, 1, 19), Some(0));
        assert_eq!(v.row_y(3, 2, 19), Some(19));
        assert_eq!(v.row_y(0, 0, 19), None);
        assert_eq!(v.row_y(2, 5, 19), None);
        assert_eq!(v.row_y(2, 0, 0), None, "line_h 0 shows nothing");
    }

    #[test]
    fn view_scroll_rows_hides_the_bottom() {
        let v = View {
            scroll_rows: 1,
            view_h: 38,
            strip_h: 19,
        };
        assert_eq!(v.row_y(3, 2, 19), None, "the newest row is scrolled out");
        assert_eq!(v.row_y(3, 1, 19), Some(19));
        assert_eq!(v.row_y(3, 0, 19), Some(0));
        let clamped = View {
            scroll_rows: 99,
            view_h: 38,
            strip_h: 19,
        };
        assert_eq!(
            clamped.row_y(3, 0, 19),
            Some(19),
            "scroll_rows is clamped to rows − 1"
        );
    }

    #[test]
    fn view_strip_places_one_row_below_the_row_area() {
        let v = View {
            scroll_rows: 0,
            view_h: 38,
            strip_h: 19,
        };
        assert_eq!(
            v.strip(),
            View {
                scroll_rows: 0,
                view_h: 57,
                strip_h: 0
            }
        );
        assert_eq!(v.strip().row_y(1, 0, 19), Some(38));
    }

    #[test]
    fn quads_use_the_spec_origin_formula() {
        let view = View {
            scroll_rows: 0,
            view_h: 38,
            strip_h: 19,
        };
        let mut src = FakeRegions { pages: 1 };
        let pages = glyph_quads(&fake_layout(), &view, None, &mut src);
        assert_eq!(pages.len(), 1);
        let inst = &pages[0];
        assert_eq!(inst.len(), 4, "underline, a, b, c");
        // Row 0 rect first (mode 0): shift + x0 + x = 3 + 9, row_y + dy = 0 + 16.
        assert_eq!(
            inst[0],
            Instance {
                rect: [12.0, 16.0, 9.0, 1.0],
                uv: [0.0; 4],
                color: rgb(WHITE),
                mode: 0,
                _pad: [0; 3]
            }
        );
        // `a`: shift + x0 + x + dx + left = 3, row_y + dy − top = 0 + 15 − 15 = 0.
        assert_eq!(inst[1].rect, [3.0, 0.0, 9.0, 18.0]);
        assert_eq!(inst[1].mode, 1);
        assert_eq!(inst[2].rect, [12.0, 0.0, 9.0, 18.0]);
        // `c` on row 1: y = 19.
        assert_eq!(inst[3].rect, [3.0, 19.0, 9.0, 18.0]);
        assert!(inst.iter().all(|i| i.color == rgb(WHITE)));
    }

    /// `layout_snapshot` does not print rect colour, so this is the only place
    /// a rect's own (non-default) colour is checked to reach the instance.
    #[test]
    fn rects_keep_their_own_colour() {
        const RED: [u8; 3] = [200, 0, 0];
        let view = View {
            scroll_rows: 0,
            view_h: 38,
            strip_h: 19,
        };
        let mut layout = fake_layout();
        layout.rows[0].rects[0].color = RED;
        let mut src = FakeRegions { pages: 1 };
        let pages = glyph_quads(&layout, &view, None, &mut src);
        assert_eq!(pages[0][0].mode, 0);
        assert_eq!(pages[0][0].color, rgb(RED));
        // Hovering the rect's button still overrides it with `hl`.
        let hovered = glyph_quads(&layout, &view, Some(0), &mut src);
        assert_eq!(hovered[0][0].color, rgb(HL));
    }

    #[test]
    fn hover_recolours_exactly_the_hovered_button() {
        let view = View {
            scroll_rows: 0,
            view_h: 38,
            strip_h: 19,
        };
        let layout = fake_layout();
        let mut src = FakeRegions { pages: 1 };
        let plain = flat(&glyph_quads(&layout, &view, None, &mut src));
        let hover0 = flat(&glyph_quads(&layout, &view, Some(0), &mut src));
        let hover1 = flat(&glyph_quads(&layout, &view, Some(1), &mut src));
        assert_eq!(plain.len(), hover0.len());
        for (p, h) in plain.iter().zip(&hover0) {
            assert_eq!((p.rect, p.uv, p.mode), (h.rect, h.uv, h.mode), "nothing moves");
        }
        let colors = |v: &[Instance]| v.iter().map(|i| i.color).collect::<Vec<_>>();
        assert_eq!(colors(&plain), vec![rgb(WHITE); 4]);
        assert_eq!(
            colors(&hover0),
            vec![rgb(HL), rgb(WHITE), rgb(HL), rgb(WHITE)],
            "underline + b"
        );
        assert_eq!(
            colors(&hover1),
            vec![rgb(WHITE), rgb(WHITE), rgb(WHITE), rgb(HL)],
            "c only"
        );
    }

    #[test]
    fn instances_are_bucketed_per_page_with_rects_on_page_zero() {
        let view = View {
            scroll_rows: 0,
            view_h: 38,
            strip_h: 19,
        };
        let mut src = FakeRegions { pages: 2 };
        let pages = glyph_quads(&fake_layout(), &view, None, &mut src);
        assert_eq!(pages.len(), 2);
        // glyph 2 (`b`) → page 0, glyphs 1 and 3 → page 1; the rect → page 0.
        assert_eq!(pages[0].len(), 2);
        assert_eq!(pages[0][0].mode, 0);
        assert_eq!(pages[0][1].rect, [12.0, 0.0, 9.0, 18.0]);
        assert_eq!(pages[1].len(), 2);
        assert!(pages[1].iter().all(|i| i.mode == 1));
    }

    #[test]
    fn scrolled_out_rows_produce_no_instances() {
        let view = View {
            scroll_rows: 0,
            view_h: 19,
            strip_h: 19,
        };
        let mut src = FakeRegions { pages: 1 };
        let only_last = flat(&glyph_quads(&fake_layout(), &view, None, &mut src));
        assert_eq!(only_last.len(), 1, "one visible row: `c`");
        assert_eq!(only_last[0].rect, [3.0, 0.0, 9.0, 18.0]);
        let view = View {
            scroll_rows: 1,
            view_h: 19,
            strip_h: 19,
        };
        let first = flat(&glyph_quads(&fake_layout(), &view, None, &mut src));
        assert_eq!(first.len(), 3, "row 0: rect, a, b");
        assert_eq!(first[1].rect, [3.0, 0.0, 9.0, 18.0]);
    }

    #[test]
    fn a_page_created_mid_build_extends_the_buckets() {
        /// Reports one page but hands out page 1 for glyph 3.
        struct Growing;
        impl RegionSource for Growing {
            fn page_count(&self) -> usize {
                1
            }
            fn region(&mut self, g: &ShapedGlyph) -> Option<AtlasRegion> {
                Some(AtlasRegion {
                    page: if g.glyph == 3 { 1 } else { 0 },
                    uv: [0.0; 4],
                    size: [9, 18],
                    left: 0,
                    top: 15,
                    color: false,
                })
            }
        }
        let view = View {
            scroll_rows: 0,
            view_h: 38,
            strip_h: 19,
        };
        let pages = glyph_quads(&fake_layout(), &view, None, &mut Growing);
        assert_eq!(pages.len(), 2);
        assert_eq!(pages[1].len(), 1);
    }

    /// Real chain → shaper → layout → raster → instances for `abc` with the
    /// bundled font: one quad per glyph, each inside its cell (±2 px bearing
    /// slack — measured: bundled `a`/`c` span 0..9, `b` 0..10 at 15 px, and
    /// 0..11 at 18 px in an 11 px cell) and inside row 0, pen x increasing.
    #[test]
    fn build_instances_gpu_smoke() {
        use std::path::PathBuf;

        use erars_ast::Alignment;
        use erars_compiler::Language;
        use erars_ui::width::WidthTable;
        use erars_ui::{Color, ConsoleLine, ConsoleLinePart, TextStyle};

        use crate::font::FontChain;
        use crate::layout::{layout, Geometry};

        let _gpu = crate::test_support::gpu_lock();
        let Some((device, queue)) = crate::test_support::gpu_device() else {
            return;
        };
        const BUNDLED: &str =
            concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf");
        let mut chain = FontChain::from_files(&[PathBuf::from(BUNDLED)], Language::Korean);
        let m = {
            let primary = chain.font(chain.primary());
            CellMetrics::from_primary(&primary, 18, 19, 1.0)
        };
        let mut shaper = Shaper::new(chain, WidthTable::new(Language::Korean.encoding()), m);
        let g = Geometry {
            content_w: 760,
            drawable_w: 760 - m.shift,
            m,
        };
        let line = ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Text(
                "abc".into(),
                TextStyle {
                    color: Color(WHITE),
                    font_family: "".into(),
                    font_style: FontStyle::NORMAL,
                },
            )],
        };
        let laid = layout(&[line], &g, &mut shaper);
        let mut raster = GlyphRaster::new(&device, true);
        let view = View {
            scroll_rows: 0,
            view_h: m.line_h,
            strip_h: 0,
        };
        let quads = build_instances(
            &laid,
            &view,
            None,
            HL,
            WHITE,
            &mut raster,
            &device,
            &queue,
            &mut shaper,
            ImageCtx::empty(),
        );
        assert!(quads.images.is_empty(), "no image parts in this line");
        let pages = quads.glyphs;
        assert_eq!(pages.len(), raster.page_count());
        let inst = flat(&pages);
        assert_eq!(inst.len(), 3, "one quad per glyph");
        assert!(inst.iter().all(|i| i.mode == 1 && i.color == rgb(WHITE)));
        for (k, i) in inst.iter().enumerate() {
            let cell_left = (m.shift + k as u32 * m.half_w) as f32;
            let cell_right = cell_left + m.half_w as f32;
            assert!(
                i.rect[0] >= cell_left - 2.0 && i.rect[0] + i.rect[2] <= cell_right + 2.0,
                "glyph {k} quad x={}..{} outside cell {cell_left}..{cell_right} (±2 px bearing)",
                i.rect[0],
                i.rect[0] + i.rect[2]
            );
            assert!(
                i.rect[1] >= 0.0 && i.rect[1] < m.line_h as f32,
                "quad top inside row 0"
            );
        }
        let pen: Vec<f32> = inst.iter().map(|i| i.rect[0]).collect();
        assert!(pen[0] < pen[1] && pen[1] < pen[2]);
        assert_eq!(raster.pages_with(&pages).len(), pages.len());
    }
}
