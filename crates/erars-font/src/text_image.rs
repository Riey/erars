//! GDI+ text on a bitmap: `GDRAWTEXT`'s rasteriser.
//!
//! This is *not* the console's cell grid. Emuera's `GDRAWTEXT` goes through
//! `GraphicsImage.GDrawString` (`Content/GraphicsImage.cs:120-142`), which
//! builds one `GraphicsPath` with
//! `AddString(text, family, style, usingFont.Height, Point(x, y), GenericTypographic)`,
//! fills it with the brush at `SmoothingMode.AntiAlias` and then strokes it
//! with the pen. So every glyph sits at the font's own advance, not in a
//! `cells × half_width` box, and nothing is quantised to half-width cells.
//!
//! Four GDI+ properties are reproduced deliberately, because scripts measure
//! against them:
//!
//! 1. **The em size is the font's `Height`, not its size.**
//!    `GraphicsImage.cs:130` passes `usingFont.Height` as `AddString`'s
//!    `emSize`. `Font.Height` is `(int)ceil(GetHeight())`, and for a
//!    `GraphicsUnit.Pixel` font `GetHeight()` is the design *line spacing*
//!    scaled to the requested size. Text is therefore drawn taller than the
//!    nominal font size. See [`em_for_drawing`].
//! 2. **The origin is the top-left of the layout box.** `AddString` takes a
//!    `Point`, and with `StringFormat.GenericTypographic` there is no padding,
//!    so the first baseline lands at `y + ascent` with `ascent` scaled to the
//!    em size of (1).
//! 3. **The measurement uses a different size.**
//!    `GraphicsDrawStringMethod` (`GameData/Function/Creator.Method.cs:5549-5564`)
//!    measures with `MeasureString(text, font, int.MaxValue, GenericTypographic)`
//!    — the font at *its own* size, i.e. 100 px by default. `GDRAWTEXT` reports
//!    that in `RESULT:1`/`RESULT:2`, not the size of the ink it just drew.
//! 4. **The path is filled *and* stroked.** `GraphicsImage.cs:131-141`:
//!    `FillPath(brush ?? SolidBrush(Config.ForeColor))` then
//!    `DrawPath(pen ?? new Pen(Config.ForeColor))`. Two coverage masks come
//!    back for that reason: `GSETBRUSH` and `GSETPEN` are separate colours.
//!
//! Underline and strikeout are style bits of the font handed to `AddString`
//! (`Creator.Method.cs:5415-5427` decodes them from `GSETFONT`'s 4th
//! argument), and GDI+ puts those bars *into* the path — so they are filled
//! and stroked with everything else. Their geometry comes from the same tables
//! the console text uses: `post.underlinePosition/Thickness` and
//! `OS/2.yStrikeoutPosition/Size`.

use cosmic_text::{fontdb, ttf_parser};
use rustybuzz::{BufferClusterLevel, Direction, UnicodeBuffer};
use smol_str::SmolStr;
use swash::scale::ScaleContext;

use crate::flags::RasterFlags;
use crate::font::{FontChain, FontConfig, StyleKey};
use crate::raster::{outline_image, outline_stroke_image, GlyphImage};
use crate::text::{features, resolve_spans};

/// Emuera's `GSETFONT` / `GGETTEXTSIZE` style bits
/// (`GameData/Function/Creator.Method.cs:5415-5427`), which are GDI+
/// `FontStyle` in everything but name — and the same four bits as
/// `erars_ui::FontStyle`, so the console's current style can be passed
/// straight through when the bitmap has no font of its own.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct TextStyleBits(pub i64);

impl TextStyleBits {
    pub const BOLD: i64 = 1;
    pub const ITALIC: i64 = 2;
    pub const STRIKEOUT: i64 = 4;
    pub const UNDERLINE: i64 = 8;

    pub fn bold(self) -> bool {
        self.0 & Self::BOLD != 0
    }

    pub fn italic(self) -> bool {
        self.0 & Self::ITALIC != 0
    }

    pub fn strikeout(self) -> bool {
        self.0 & Self::STRIKEOUT != 0
    }

    pub fn underline(self) -> bool {
        self.0 & Self::UNDERLINE != 0
    }
}

/// The font `GDRAWTEXT` draws with: `GSETFONT`'s state, or the fallback
/// `new Font(Config.FontName, 100, Console.StringStyle.FontStyle, GraphicsUnit.Pixel)`
/// that `GraphicsImage.cs:126-127` builds when the bitmap never had one.
#[derive(Clone, Copy, Debug)]
pub struct TextFont<'a> {
    /// GDI+ family name; empty = the configured chain. An unknown name falls
    /// back exactly as it does for console text (`FontChain::resolve`).
    pub family: &'a str,
    /// `GraphicsUnit.Pixel` em size. Emuera's default is 100.
    pub size_px: f64,
    pub style: TextStyleBits,
}

/// One coverage layer: 8-bit alpha, row-major, `width × height`, positioned
/// by `(left, top)` **relative to the layout origin** that was handed to
/// `AddString`.
///
/// `left`/`top` grow right and down, like the ARGB bitmap the caller owns, so
/// compositing is `dst[(y + top + row) * stride + x + left + col]`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Coverage {
    pub left: i32,
    pub top: i32,
    pub width: u32,
    pub height: u32,
    pub data: Vec<u8>,
}

impl Coverage {
    fn empty() -> Self {
        Self {
            left: 0,
            top: 0,
            width: 0,
            height: 0,
            data: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.width == 0 || self.height == 0
    }

    /// Coverage at `(x, y)` in the layer's own pixel space, 0 outside it.
    pub fn at(&self, x: u32, y: u32) -> u8 {
        if x >= self.width || y >= self.height {
            return 0;
        }
        self.data[y as usize * self.width as usize + x as usize]
    }
}

/// What [`TextRasterizer::draw`] hands back: the two GDI+ passes as coverage
/// masks, plus the extent `MeasureString` would have reported.
#[derive(Clone, Debug)]
pub struct TextImage {
    /// `FillPath(brush, path)` — paint with the brush colour first.
    pub fill: Coverage,
    /// `DrawPath(pen, path)` — paint with the pen colour, *over* the fill.
    pub stroke: Coverage,
    /// `MeasureString(text, font, int.MaxValue, GenericTypographic).Width`:
    /// the widest line's advance sum at the font's own `size_px`.
    pub measured_width: f64,
    /// The same call's `.Height`: the line count times the font's line
    /// spacing at its own `size_px`.
    pub measured_height: f64,
}

/// Design metrics of one face in font units, as GDI+ `FontFamily` exposes
/// them.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct FaceMetrics {
    /// `FontFamily.GetEmHeight`.
    upem: u16,
    /// `FontFamily.GetCellAscent`.
    ascent: i16,
    /// `FontFamily.GetLineSpacing` = cell ascent + cell descent + leading.
    line_spacing: i32,
}

impl FaceMetrics {
    /// `ttf_parser`'s `ascender`/`descender`/`line_gap` are hhea, or the OS/2
    /// typographic pair when `USE_TYPO_METRICS` is set — the same accessors
    /// the console's cell metrics take their baseline from, so no font is
    /// measured two different ways inside erars.
    ///
    /// Checked against GDI+'s published Arial numbers (upem 2048, CellAscent
    /// 1854, CellDescent 434, LineSpacing 2355): Arial's hhea is
    /// `ascender 1854`, `descender −434`, `lineGap 67`, and
    /// `1854 + 434 + 67 = 2355`.
    fn of(face: &ttf_parser::Face) -> Self {
        let ascent = face.ascender();
        let descent = face.descender();
        Self {
            upem: face.units_per_em(),
            ascent,
            line_spacing: i32::from(ascent) - i32::from(descent) + i32::from(face.line_gap()),
        }
    }

    /// `Font.GetHeight()` for a `GraphicsUnit.Pixel` font: the design line
    /// spacing scaled to `size_px`.
    fn line_height(&self, size_px: f64) -> f64 {
        f64::from(self.line_spacing) * size_px / f64::from(self.upem)
    }
}

/// `Font.Height`, the em size `GDrawString` hands to `AddString`
/// (`Content/GraphicsImage.cs:130`).
///
/// .NET's `Font.Height` is `(int)Math.Ceiling(GetHeight())`, so a 100 px
/// Noto Sans Mono (upem 1000, ascender 1069, descender −293, lineGap 0) draws
/// at `ceil(136.2) = 137` px.
fn em_for_drawing(m: &FaceMetrics, size_px: f64) -> f64 {
    m.line_height(size_px).ceil().max(1.0)
}

/// One glyph as shaped: which face, which id, and where its origin sits in
/// layout pixels at the drawing em.
struct Placed {
    font: fontdb::ID,
    glyph: u16,
    flags: RasterFlags,
    x: f64,
    /// Baseline y, already including the glyph's `y_offset`.
    y: f64,
}

/// Accumulates coverage over a growing bounding box.
///
/// GDI+ fills *one* path for the whole string, so overlapping coverage must
/// not add up: two anti-aliased edges meeting inside one filled path do not
/// double-darken it. The combine is therefore `max`, not a blend.
struct Accum {
    /// Half-open box in layout coordinates; `None` until the first non-empty
    /// image lands.
    bounds: Option<(i32, i32, i32, i32)>,
    parts: Vec<(i32, i32, u32, u32, Vec<u8>)>,
}

impl Accum {
    fn new() -> Self {
        Self {
            bounds: None,
            parts: Vec::new(),
        }
    }

    fn push(&mut self, left: i32, top: i32, width: u32, height: u32, data: Vec<u8>) {
        if width == 0 || height == 0 {
            return;
        }
        let (x1, y1) = (left + width as i32, top + height as i32);
        self.bounds = Some(match self.bounds {
            None => (left, top, x1, y1),
            Some((ax, ay, bx, by)) => (ax.min(left), ay.min(top), bx.max(x1), by.max(y1)),
        });
        self.parts.push((left, top, width, height, data));
    }

    fn finish(self) -> Coverage {
        let Some((x0, y0, x1, y1)) = self.bounds else {
            return Coverage::empty();
        };
        let (w, h) = ((x1 - x0) as u32, (y1 - y0) as u32);
        let mut data = vec![0u8; w as usize * h as usize];
        for (left, top, pw, ph, part) in self.parts {
            let (dx, dy) = ((left - x0) as usize, (top - y0) as usize);
            for row in 0..ph as usize {
                let src = &part[row * pw as usize..(row + 1) * pw as usize];
                let base = (dy + row) * w as usize + dx;
                for (i, &a) in src.iter().enumerate() {
                    let dst = &mut data[base + i];
                    *dst = (*dst).max(a);
                }
            }
        }
        Coverage {
            left: x0,
            top: y0,
            width: w,
            height: h,
            data,
        }
    }
}

/// Upper bound on the em a script may ask a glyph to be rasterised at.
///
/// DELIBERATE divergence from `Content/GraphicsImage.cs:128-130`: GDI+ has no
/// such cap. `AddString` takes any em size, and `GSETFONT id, "", 100000`
/// followed by `GDRAWTEXT` makes .NET throw `OutOfMemoryException` out of the
/// rasteriser rather than draw. There is no way to reproduce "an allocation
/// the size of the address space fails somewhere inside GDI+" that is more
/// faithful than refusing to grow the mask past a size no bitmap can show, so
/// the em is clamped here and the glyphs are still drawn.
pub const MAX_EM_PX: f64 = 4096.0;

/// The em size of the font GDI+ falls back on when a bitmap never had a
/// `GSETFONT`: `new Font(Config.FontName, 100, …, GraphicsUnit.Pixel)`
/// (`Content/GraphicsImage.cs:127`, and again for the measurement at
/// `GameData/Function/Creator.Method.cs:5555`).
pub const DEFAULT_FONT_SIZE_PX: f64 = 100.0;

/// The whole font stack `GDRAWTEXT` needs, and the only thing outside this
/// crate has to hold on to: the same per-character chain console text
/// resolves through, plus swash's scaler cache.
///
/// Both are caches over immutable font data, so keeping one per VM only
/// decides how much work is repeated — and `FontChain::new` reads the system
/// font database, which is far too expensive to redo per call.
pub struct TextRasterizer {
    chain: FontChain,
    scaler: ScaleContext,
}

impl TextRasterizer {
    /// Build the chain from the game's configuration — the identical
    /// `FontConfig` the renderer lays console text out with, so `GDRAWTEXT`
    /// cannot pick a different face than the console for the same family.
    pub fn new(cfg: &FontConfig) -> Self {
        Self {
            chain: FontChain::new(cfg),
            scaler: ScaleContext::new(),
        }
    }

    /// Draw `text` as GDI+ would, with the layout box's top-left at `(0, 0)`.
    ///
    /// `stroke_px` is the pen width: `Pen`'s default is 1 and GDI+ draws a
    /// width-0 pen as one device pixel, so a caller with a `GSETPEN` width
    /// passes `max(width, 1)`; `0.0` skips the `DrawPath` pass entirely.
    ///
    /// The returned coverage is positioned relative to that origin, so a
    /// caller drawing at `(x, y)` composites `fill` at
    /// `(x + fill.left, y + fill.top)`.
    pub fn draw(&mut self, font: &TextFont, text: &str, stroke_px: f64) -> TextImage {
        render(&mut self.chain, &mut self.scaler, font, text, stroke_px)
    }
}

/// [`TextRasterizer::draw`]'s body, over borrowed halves so the tests can
/// drive it with a chain built from a file instead of the system fonts.
fn render(
    chain: &mut FontChain,
    ctx: &mut ScaleContext,
    font: &TextFont,
    text: &str,
    stroke_px: f64,
) -> TextImage {
    // `MeasureString("")` is `(0, 0)` and an empty path fills nothing, so the
    // C# still returns 1 having drawn no pixel (`Creator.Method.cs:5542`
    // passes any string straight through).
    let Some(first) = text.chars().next() else {
        return TextImage {
            fill: Coverage::empty(),
            stroke: Coverage::empty(),
            measured_width: 0.0,
            measured_height: 0.0,
        };
    };

    let key = StyleKey {
        family: SmolStr::new(font.family),
        bold: font.style.bold(),
        italic: font.style.italic(),
    };
    // `AddString` takes ONE `FontFamily` for the whole string and lays every
    // line out on its line spacing, so the metrics are the primary face's —
    // the face the first character resolves to.
    let primary = chain.resolve(first, &key).0;
    let metrics = FaceMetrics::of(chain.font(primary).rustybuzz());

    let size_px = font.size_px.clamp(1.0, MAX_EM_PX);
    let em = em_for_drawing(&metrics, size_px).min(MAX_EM_PX);
    let scale = em / f64::from(metrics.upem);
    let ascent = f64::from(metrics.ascent) * scale;
    let line_h = metrics.line_height(em);

    let ul = font
        .style
        .underline()
        .then(|| bar_metrics(chain, primary, Bar::Underline))
        .flatten();
    let st = font
        .style
        .strikeout()
        .then(|| bar_metrics(chain, primary, Bar::Strikeout))
        .flatten();

    let mut placed: Vec<Placed> = Vec::new();
    // Underline / strikeout rectangles in layout pixels: (x, y, w, h).
    let mut bars: Vec<(f64, f64, f64, f64)> = Vec::new();
    // `MeasureString` is done at the font's *own* size (property 3 above),
    // which is why each span accumulates a second, differently scaled sum.
    let mut measured_width = 0.0f64;
    let mut lines = 0u32;

    let mut buf = UnicodeBuffer::new();
    let feats = features();
    for line in text.split('\n') {
        // GDI+ treats CR as part of the break, so a CRLF script does not draw
        // a .notdef box at the end of every line.
        let line = line.strip_suffix('\r').unwrap_or(line);
        let baseline = ascent + f64::from(lines) * line_h;
        // Pen position in *layout pixels*: each span advances by its own
        // face's units / upem.
        //
        // DELIBERATE divergence from `Content/GraphicsImage.cs:130`: GDI+'s
        // `GraphicsPath.AddString` takes one `FontFamily` and draws that
        // family's .notdef box for anything it lacks — it does not fall back
        // per character the way `DrawString` does. Emuera can rely on that
        // because Windows ships the font its config names (MS Gothic); erars
        // runs where the named family may not exist at all, so `GDRAWTEXT`
        // resolves each character through the same chain console text uses.
        // A face swapped in mid-string carries its own metrics, hence the
        // per-span scaling here.
        let mut pen = 0.0f64;
        let mut measured = 0.0f64;
        for span in resolve_spans(chain, line, &key) {
            let face_font = chain.font(span.font);
            let face = face_font.rustybuzz();
            let upem = f64::from(face.units_per_em());
            buf.push_str(&line[span.start..span.end]);
            buf.set_direction(Direction::LeftToRight);
            buf.set_cluster_level(BufferClusterLevel::MonotoneGraphemes);
            let shaped = rustybuzz::shape(face, &feats, buf);
            {
                let infos = shaped.glyph_infos();
                for (info, pos) in infos.iter().zip(shaped.glyph_positions()) {
                    placed.push(Placed {
                        font: span.font,
                        glyph: info.glyph_id as u16,
                        flags: span.flags,
                        x: pen + f64::from(pos.x_offset) * em / upem,
                        y: baseline - f64::from(pos.y_offset) * em / upem,
                    });
                    pen += f64::from(pos.x_advance) * em / upem;
                    measured += f64::from(pos.x_advance) * size_px / upem;
                }
            }
            buf = shaped.clear();
        }
        measured_width = measured_width.max(measured);
        // The bars span the line's own advance width — GDI+ adds them per
        // line, so an empty line has none.
        if pen > 0.0 {
            if let Some((pos, thick)) = ul {
                bars.push((0.0, baseline - pos * scale, pen, thick * scale));
            }
            if let Some((pos, thick)) = st {
                bars.push((0.0, baseline - pos * scale, pen, thick * scale));
            }
        }
        lines += 1;
        // Guard against a runaway `\n`-only string turning into gigabytes of
        // bounding box; nothing is drawn past the point a bitmap could hold.
        if f64::from(lines) * line_h > f64::from(u16::MAX) {
            break;
        }
    }

    let mut fill = Accum::new();
    let mut stroke = Accum::new();
    let em_f32 = em as f32;
    for p in &placed {
        let face = chain.font(p.font);
        if let Some(img) = outline_image(ctx, &face, p.glyph, em_f32, p.flags) {
            add_glyph(&mut fill, &img, p);
        }
        if stroke_px > 0.0 {
            if let Some(img) =
                outline_stroke_image(ctx, &face, p.glyph, em_f32, p.flags, stroke_px as f32)
            {
                add_glyph(&mut stroke, &img, p);
            }
        }
    }
    for &(x, y, w, h) in &bars {
        // A bar is a subpath like any other: filled solid, and its border
        // stroked by `DrawPath`.
        add_bar(&mut fill, x, y, w, h, None);
        if stroke_px > 0.0 {
            add_bar(&mut stroke, x, y, w, h, Some(stroke_px));
        }
    }

    TextImage {
        fill: fill.finish(),
        stroke: stroke.finish(),
        measured_width,
        measured_height: f64::from(lines) * metrics.line_height(size_px),
    }
}

/// Blit one glyph image into `acc`. [`GlyphImage`]'s `top` is the distance
/// from the baseline *up* to the image's first row, so its top in layout
/// coordinates is `baseline − top`.
fn add_glyph(acc: &mut Accum, img: &GlyphImage, p: &Placed) {
    if img.is_empty() {
        return;
    }
    let left = (p.x.round() as i32).saturating_add(img.left);
    let top = (p.y.round() as i32).saturating_sub(img.top);
    // A colour glyph (emoji) has no single coverage; GDI+ would fill the
    // *outline* path, and the alpha channel is the closest honest reading of
    // "what that path covers".
    let mask: Vec<u8> = img.rgba.chunks_exact(4).map(|px| px[3]).collect();
    acc.push(left, top, img.width, img.height, mask);
}

/// An axis-aligned bar (underline / strikeout), anti-aliased on its edges the
/// way a filled rectangle inside an `AntiAlias` path is. With `stroke` set the
/// bar's *border* is drawn instead, `stroke` px wide and centred on the edge.
fn add_bar(acc: &mut Accum, x: f64, y: f64, w: f64, h: f64, stroke: Option<f64>) {
    if w <= 0.0 {
        return;
    }
    // A sub-pixel bar still paints a row: GDI+ anti-aliases it to a partial
    // coverage rather than dropping it, and `post.underlineThickness` is
    // frequently under 1 px at small sizes.
    let h = h.max(1.0);
    let (x0, y0, x1, y1) = (x, y, x + w, y + h);
    let cover = |a0: f64, a1: f64, i: i32| -> f64 {
        let (lo, hi) = (f64::from(i), f64::from(i) + 1.0);
        (a1.min(hi) - a0.max(lo)).clamp(0.0, 1.0)
    };
    let mut blit = |x0: f64, y0: f64, x1: f64, y1: f64| {
        let (ix0, iy0) = (x0.floor() as i32, y0.floor() as i32);
        let (ix1, iy1) = (x1.ceil() as i32, y1.ceil() as i32);
        let (bw, bh) = ((ix1 - ix0) as u32, (iy1 - iy0) as u32);
        if bw == 0 || bh == 0 {
            return;
        }
        let mut data = vec![0u8; bw as usize * bh as usize];
        for row in 0..bh as i32 {
            let cy = cover(y0, y1, iy0 + row);
            for col in 0..bw as i32 {
                let a = cy * cover(x0, x1, ix0 + col);
                data[row as usize * bw as usize + col as usize] = (a * 255.0).round() as u8;
            }
        }
        acc.push(ix0, iy0, bw, bh, data);
    };
    match stroke {
        None => blit(x0, y0, x1, y1),
        Some(sw) => {
            let r = sw / 2.0;
            blit(x0 - r, y0 - r, x1 + r, y0 + r); // top edge
            blit(x0 - r, y1 - r, x1 + r, y1 + r); // bottom edge
            blit(x0 - r, y0 - r, x0 + r, y1 + r); // left edge
            blit(x1 - r, y0 - r, x1 + r, y1 + r); // right edge
        }
    }
}

enum Bar {
    Underline,
    Strikeout,
}

/// `(position, thickness)` in font units for one bar, from the tables GDI
/// reads: `post.underlinePosition/Thickness` and
/// `OS/2.yStrikeoutPosition/Size`. `position` is measured up from the baseline
/// (an underline's is negative). `None` when the table is absent: there is
/// nothing to derive it from and GDI+ would draw nothing either.
fn bar_metrics(chain: &mut FontChain, id: fontdb::ID, bar: Bar) -> Option<(f64, f64)> {
    let font = chain.font(id);
    let face = font.rustybuzz();
    let m = match bar {
        Bar::Underline => face.underline_metrics(),
        Bar::Strikeout => face.strikeout_metrics(),
    }?;
    Some((f64::from(m.position), f64::from(m.thickness)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::bundled_font_path;
    use erars_compiler::Language;

    /// Bundled Noto Sans Mono only: every number below is derived from its own
    /// tables, so no assertion reads back this module's output.
    fn bundled() -> FontChain {
        FontChain::from_files(&[bundled_font_path()], Language::Japanese)
    }

    fn plain(size_px: f64) -> TextFont<'static> {
        TextFont {
            family: "",
            size_px,
            style: TextStyleBits(0),
        }
    }

    /// `(upem, ascender, descender, line_gap)` of the bundled face.
    fn hhea() -> (u16, i16, i16, i16) {
        let mut chain = bundled();
        let id = chain.primary();
        let font = chain.font(id);
        let face = font.rustybuzz();
        (
            // rustybuzz's inherent accessor is `i32`; the table field is `u16`.
            face.units_per_em().try_into().expect("upem fits u16"),
            face.ascender(),
            face.descender(),
            face.line_gap(),
        )
    }

    fn line_spacing_units() -> i32 {
        let (_, asc, desc, gap) = hhea();
        i32::from(asc) - i32::from(desc) + i32::from(gap)
    }

    /// The em `AddString` is called with, recomputed from the raw tables.
    fn em_at(size_px: f64) -> f64 {
        let (upem, _, _, _) = hhea();
        (f64::from(line_spacing_units()) * size_px / f64::from(upem)).ceil()
    }

    fn baseline_at(size_px: f64) -> f64 {
        let (upem, asc, _, _) = hhea();
        f64::from(asc) * em_at(size_px) / f64::from(upem)
    }

    /// Sum of `hmtx` advances for `s` in the bundled face, in font units.
    fn advance_units(s: &str) -> i64 {
        let mut chain = bundled();
        let id = chain.primary();
        let font = chain.font(id);
        let face = font.rustybuzz();
        s.chars()
            .map(|c| {
                let g = face.glyph_index(c).expect("glyph present");
                i64::from(face.glyph_hor_advance(g).expect("hmtx entry"))
            })
            .sum()
    }

    #[test]
    fn style_bits_decode_like_creator_method() {
        // `Creator.Method.cs:5420-5427`: 1 bold, 2 italic, 4 strikeout, 8 underline.
        let none = TextStyleBits(0);
        assert!(!none.bold() && !none.italic() && !none.strikeout() && !none.underline());
        assert!(TextStyleBits(1).bold());
        assert!(TextStyleBits(2).italic());
        assert!(TextStyleBits(4).strikeout());
        assert!(TextStyleBits(8).underline());
        let all = TextStyleBits(15);
        assert!(all.bold() && all.italic() && all.strikeout() && all.underline());
        // Bits outside the four are ignored, as C#'s `|=` of those four is.
        let high = TextStyleBits(16);
        assert!(!high.bold() && !high.italic() && !high.strikeout() && !high.underline());
        let mixed = TextStyleBits(1 | 8);
        assert!(mixed.bold() && mixed.underline() && !mixed.italic() && !mixed.strikeout());
    }

    #[test]
    fn measure_width_is_the_hmtx_advance_sum() {
        let mut chain = bundled();
        let mut ctx = ScaleContext::new();
        let (upem, ..) = hhea();
        for (text, size) in [("A", 100.0), ("ABC", 100.0), ("Hello, world!", 37.0)] {
            let units = advance_units(text);
            let expected = units as f64 * size / f64::from(upem);
            let img = render(&mut chain, &mut ctx, &plain(size), text, 1.0);
            assert!(
                (img.measured_width - expected).abs() < 1e-9,
                "{text:?} @{size}px: {} != {expected} ({units} units / upem {upem})",
                img.measured_width
            );
        }
        // Noto Sans Mono advances 600/1000 em, so "ABC" is exactly 180 px at
        // the default 100 px font Emuera builds (`GraphicsImage.cs:126`).
        assert_eq!(advance_units("ABC"), 1800);
        let img = render(&mut chain, &mut ctx, &plain(100.0), "ABC", 1.0);
        assert_eq!(img.measured_width, 180.0);
    }

    #[test]
    fn measure_height_is_line_spacing_per_line() {
        let mut chain = bundled();
        let mut ctx = ScaleContext::new();
        let (upem, ..) = hhea();
        let one = f64::from(line_spacing_units()) * 100.0 / f64::from(upem);
        for (text, lines) in [("A", 1u32), ("A\nB", 2), ("A\nB\nC", 3), ("A\n", 2)] {
            let img = render(&mut chain, &mut ctx, &plain(100.0), text, 1.0);
            assert!(
                (img.measured_height - one * f64::from(lines)).abs() < 1e-9,
                "{text:?}: {} != {lines} x {one}",
                img.measured_height
            );
        }
        // The widest line wins, not the last one.
        let img = render(&mut chain, &mut ctx, &plain(100.0), "ABC\nA", 1.0);
        assert_eq!(img.measured_width, 180.0);
        // `MeasureString("")` is (0, 0) and nothing is drawn.
        let empty = render(&mut chain, &mut ctx, &plain(100.0), "", 1.0);
        assert_eq!((empty.measured_width, empty.measured_height), (0.0, 0.0));
        assert!(empty.fill.is_empty() && empty.stroke.is_empty());
    }

    /// The em handed to `AddString` is `Font.Height`, not the font size
    /// (`GraphicsImage.cs:130`), so the ink is taller than `size_px`.
    #[test]
    fn drawing_em_is_the_ceiled_line_height() {
        let (upem, asc, ..) = hhea();
        let m = FaceMetrics {
            upem,
            ascent: asc,
            line_spacing: line_spacing_units(),
        };
        let exact = f64::from(m.line_spacing) * 100.0 / f64::from(upem);
        assert_eq!(em_for_drawing(&m, 100.0), exact.ceil());
        assert!(
            em_for_drawing(&m, 100.0) > 100.0,
            "the bundled face's line spacing exceeds its em"
        );
        // GDI+'s documented Arial numbers: upem 2048, LineSpacing 2355.
        let arial = FaceMetrics {
            upem: 2048,
            ascent: 1854,
            line_spacing: 2355,
        };
        assert_eq!(arial.line_height(2048.0), 2355.0);
        assert_eq!(
            em_for_drawing(&arial, 100.0),
            (2355.0_f64 * 100.0 / 2048.0).ceil()
        );
    }

    /// Ink lands inside the ascender band below the origin: a capital `H` sits
    /// between the cap height and the baseline, never below it.
    #[test]
    fn glyph_has_ink_in_the_ascender_band() {
        let mut chain = bundled();
        let mut ctx = ScaleContext::new();
        let img = render(&mut chain, &mut ctx, &plain(100.0), "H", 0.0);
        assert!(!img.fill.is_empty(), "H must rasterise");
        assert!(
            img.fill.data.iter().any(|&a| a > 0),
            "coverage must be non-zero"
        );
        assert!(
            img.fill.top >= 0,
            "H's first row {} is above the layout box",
            img.fill.top
        );
        let baseline = baseline_at(100.0);
        let bottom = img.fill.top + img.fill.height as i32;
        assert!(
            bottom <= baseline.ceil() as i32,
            "H's last row {bottom} is below the baseline {baseline}"
        );
        // An `H` is inked across its whole height on both stems.
        for row in 0..img.fill.height {
            assert!(
                (0..img.fill.width).any(|x| img.fill.at(x, row) > 0),
                "row {row} of H's box is blank"
            );
        }
    }

    /// The stroke is a second, different coverage: `DrawPath` after `FillPath`
    /// (`GraphicsImage.cs:131-141`).
    #[test]
    fn stroke_is_a_separate_outline_layer() {
        let mut chain = bundled();
        let mut ctx = ScaleContext::new();
        let img = render(&mut chain, &mut ctx, &plain(100.0), "H", 4.0);
        assert!(!img.stroke.is_empty(), "the pen must paint something");
        // A 4 px pen straddles the outline, so its box grows on both axes.
        assert!(
            img.stroke.width > img.fill.width && img.stroke.height > img.fill.height,
            "stroke {}x{} vs fill {}x{}",
            img.stroke.width,
            img.stroke.height,
            img.fill.width,
            img.fill.height
        );
        let none = render(&mut chain, &mut ctx, &plain(100.0), "H", 0.0);
        assert!(none.stroke.is_empty(), "a zero-width pen draws no stroke");
        assert_eq!(
            none.fill.data, img.fill.data,
            "the fill does not depend on the pen"
        );
    }

    /// Underline and strikeout are in the path, so they extend the fill box and
    /// sit where `post` / `OS/2` say.
    #[test]
    fn underline_and_strikeout_come_from_the_font_tables() {
        let mut chain = bundled();
        let mut ctx = ScaleContext::new();
        let (upem, ..) = hhea();
        let em = em_at(100.0);
        let scale = em / f64::from(upem);
        let baseline = baseline_at(100.0);
        let primary = chain.primary();
        let (ul_pos, _) = bar_metrics(&mut chain, primary, Bar::Underline)
            .expect("the bundled face has a post table");
        let (st_pos, _) = bar_metrics(&mut chain, primary, Bar::Strikeout)
            .expect("the bundled face has an OS/2 table");
        assert!(ul_pos < 0.0, "an underline sits below the baseline");
        assert!(st_pos > 0.0, "a strike sits above the baseline");

        let styled = |bits: i64| TextFont {
            family: "",
            size_px: 100.0,
            style: TextStyleBits(bits),
        };
        let bare = render(&mut chain, &mut ctx, &plain(100.0), "x", 0.0);
        let underlined = render(
            &mut chain,
            &mut ctx,
            &styled(TextStyleBits::UNDERLINE),
            "x",
            0.0,
        );
        let struck = render(
            &mut chain,
            &mut ctx,
            &styled(TextStyleBits::STRIKEOUT),
            "x",
            0.0,
        );

        // `x` has no descender, so the underline is what pushes the box down.
        let bare_bottom = bare.fill.top + bare.fill.height as i32;
        let ul_row = (baseline - ul_pos * scale).floor() as i32;
        assert!(ul_row >= bare_bottom, "the underline is below `x`'s ink");
        assert!(
            underlined.fill.top + underlined.fill.height as i32 > ul_row,
            "the underline must extend the box to row {ul_row}"
        );
        assert!(
            underlined.fill.at(0, (ul_row - underlined.fill.top) as u32) > 0,
            "the underline reaches the left edge of the advance"
        );
        // The bar spans the advance, which `x` itself does not.
        assert_eq!(bare.fill.at(0, 0), 0);
        // The strike sits inside the x-height, and only when asked for.
        let st_row = (baseline - st_pos * scale).floor() as i32;
        assert!(
            struck.fill.at(0, (st_row - struck.fill.top) as u32) > 0,
            "the strike bar reaches the left edge of the advance"
        );
        assert_eq!(
            underlined.fill.at(0, (st_row - underlined.fill.top) as u32),
            0,
            "an underlined `x` has no ink on the strike row's left edge"
        );
    }

    /// A family nobody has resolves through the chain, exactly as console text
    /// does — the same `FontChain::resolve` call, so the fallback cannot drift.
    #[test]
    fn unknown_family_falls_back_to_the_chain() {
        let mut chain = bundled();
        let mut ctx = ScaleContext::new();
        let known = render(&mut chain, &mut ctx, &plain(50.0), "A", 1.0);
        let unknown = render(
            &mut chain,
            &mut ctx,
            &TextFont {
                family: "No Such Family At All",
                size_px: 50.0,
                style: TextStyleBits(0),
            },
            "A",
            1.0,
        );
        assert_eq!(known.measured_width, unknown.measured_width);
        assert_eq!(known.fill.data, unknown.fill.data);
    }

    /// A newline puts the next line's ink one line height lower.
    #[test]
    fn newline_starts_a_second_line_of_ink() {
        let mut chain = bundled();
        let mut ctx = ScaleContext::new();
        let one = render(&mut chain, &mut ctx, &plain(40.0), "H", 0.0);
        let two = render(&mut chain, &mut ctx, &plain(40.0), "H\nH", 0.0);
        let (upem, ..) = hhea();
        let line_h = f64::from(line_spacing_units()) * em_at(40.0) / f64::from(upem);
        assert_eq!(two.fill.top, one.fill.top, "the first line is unmoved");
        assert_eq!(two.fill.width, one.fill.width, "both lines are one `H` wide");
        let grown = f64::from(two.fill.height) - f64::from(one.fill.height);
        assert!(
            (grown - line_h).abs() <= 1.0,
            "the box grew by {grown}, expected the {line_h} px line height"
        );
    }
}
