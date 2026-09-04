//! Glyph rasterization (spec Component 6).
//!
//! Two raster paths:
//! 1. Embedded 1-bit strikes (MS Gothic EBDT), read with ttf-parser through
//!    `Font::rustybuzz()`. Used only when `use_bitmap_strikes` is on, the glyph
//!    carries no synthetic-style flag, `size_px` is an integer and the strike's
//!    `pixels_per_em` equals `size_px` — ttf-parser returns the *nearest*
//!    strike (22 ppem for a 23 px request), which must be rejected.
//! 2. Everything else through swash on `Font::as_swash()`: hinted outlines,
//!    colour outlines and colour bitmaps, with synthetic bold (embolden) and
//!    synthetic italic (12° skew). swash's `Source::Bitmap` is never used: its
//!    EBLC locator cannot read MS Gothic's index-format-5 subtables, so it
//!    would mix crisp ASCII with anti-aliased kana in one row.
//!
//! Every output here is a CPU image: white + coverage-in-alpha for a mask
//! glyph, straight RGBA for a colour glyph. `erars-renderer`'s atlas uploads
//! them to `PAGE_SIZE`² textures; `erars-vm`'s `GDRAWTEXT` composites them
//! straight into an ARGB bitmap. [`place`] is the atlas *allocation* rule,
//! which is GPU-free (etagere shelf packing) and therefore lives here with
//! [`PAGE_SIZE`], the size limit it enforces.

use cosmic_text::{fontdb, ttf_parser, Font};
use etagere::{size2, AtlasAllocator};
use swash::scale::image::Content;
use swash::scale::{Render, ScaleContext, Source, StrikeWith};
use swash::zeno::{Angle, Format, Stroke, Style, Transform};

use crate::flags::RasterFlags;

/// Side length of one atlas page in texels
/// (= `Limits::downlevel_defaults().max_texture_dimension_2d`).
pub const PAGE_SIZE: u32 = 2048;

/// Identity of one rasterized glyph image.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct RasterKey {
    pub font: fontdb::ID,
    pub glyph: u16,
    /// `size_px.to_bits()` — keeps the key hashable; see [`RasterKey::size_px`].
    pub size_bits: u32,
    pub flags: RasterFlags,
}

impl RasterKey {
    pub fn new(font: fontdb::ID, glyph: u16, size_px: f32, flags: RasterFlags) -> Self {
        Self {
            font,
            glyph,
            size_bits: size_px.to_bits(),
            flags,
        }
    }

    pub fn size_px(&self) -> f32 {
        f32::from_bits(self.size_bits)
    }
}

/// Where a glyph lives in the atlas and how to place its quad.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AtlasRegion {
    pub page: usize,
    /// UV rect in [0,1]: u, v, uw, vh.
    pub uv: [f32; 4],
    /// Bitmap size in px.
    pub size: [u32; 2],
    /// Left bearing from the pen origin (px, +x right).
    pub left: i32,
    /// Distance from the baseline up to the bitmap's top row (px, +y up).
    pub top: i32,
    /// `true` = RGBA colour glyph (shader mode 2); `false` = alpha mask (mode 1).
    pub color: bool,
}

/// Raster path 1 output: an 8-bit mask (0 / 255) decoded from an embedded
/// 1-bit strike, with the same placement convention as [`AtlasRegion`].
#[derive(Clone, Debug, PartialEq)]
pub struct StrikeMask {
    pub width: u32,
    pub height: u32,
    pub left: i32,
    pub top: i32,
    pub data: Vec<u8>,
}

/// A CPU-side glyph image: straight RGBA8, row-major, no row padding.
#[derive(Clone, Debug, PartialEq)]
pub struct GlyphImage {
    pub width: u32,
    pub height: u32,
    pub left: i32,
    pub top: i32,
    pub color: bool,
    pub rgba: Vec<u8>,
}

impl GlyphImage {
    /// A blank glyph (space): takes no atlas space and draws nothing.
    pub fn blank() -> Self {
        Self {
            width: 0,
            height: 0,
            left: 0,
            top: 0,
            color: false,
            rgba: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.width == 0 || self.height == 0
    }

    /// White + coverage-in-alpha RGBA from an 8-bit mask of `width × height`.
    fn from_mask(width: u32, height: u32, left: i32, top: i32, mask: &[u8]) -> Self {
        let mut rgba = vec![0u8; width as usize * height as usize * 4];
        for (px, a) in rgba.chunks_exact_mut(4).zip(mask) {
            px[0] = 255;
            px[1] = 255;
            px[2] = 255;
            px[3] = *a;
        }
        Self {
            width,
            height,
            left,
            top,
            color: false,
            rgba,
        }
    }
}

/// Decode a 1-bit-per-pixel bitmap into an 8-bit mask (set bit → 255).
/// `packed` = ttf-parser `BitmapMonoPacked` (rows are bit-continuous);
/// otherwise every row starts on a byte boundary (`BitmapMono`). The most
/// significant bit of the first byte is the top-left pixel. `None` when `data`
/// is shorter than `width × height` needs.
pub fn decode_mono(data: &[u8], width: u32, height: u32, packed: bool) -> Option<Vec<u8>> {
    let (w, h) = (width as usize, height as usize);
    let row_bits = if packed { w } else { (w + 7) / 8 * 8 };
    if data.len() < (row_bits * h + 7) / 8 {
        return None;
    }
    let mut out = vec![0u8; w * h];
    for y in 0..h {
        for x in 0..w {
            let bit = y * row_bits + x;
            if (data[bit >> 3] >> (7 - (bit & 7))) & 1 == 1 {
                out[y * w + x] = 255;
            }
        }
    }
    Some(out)
}

/// A fresh empty page allocator (`erars-renderer`'s atlas creates page 0
/// with one, and [`place`] appends further pages).
pub fn new_allocator() -> AtlasAllocator {
    AtlasAllocator::new(size2(PAGE_SIZE as i32, PAGE_SIZE as i32))
}

/// Find room for a `w × h` image plus a 1 px gutter on an existing page, or on
/// a page appended to `allocs`. Returns `(page, x, y)`; `None` when the image
/// plus its gutter cannot fit on any page (`w >= PAGE_SIZE` or `h >= PAGE_SIZE`).
pub fn place(allocs: &mut Vec<AtlasAllocator>, w: u32, h: u32) -> Option<(usize, u32, u32)> {
    if w >= PAGE_SIZE || h >= PAGE_SIZE {
        return None;
    }
    let want = size2(w as i32 + 1, h as i32 + 1);
    for (i, alloc) in allocs.iter_mut().enumerate() {
        if let Some(a) = alloc.allocate(want) {
            return Some((i, a.rectangle.min.x as u32, a.rectangle.min.y as u32));
        }
    }
    let mut alloc = new_allocator();
    let a = alloc.allocate(want)?;
    allocs.push(alloc);
    Some((
        allocs.len() - 1,
        a.rectangle.min.x as u32,
        a.rectangle.min.y as u32,
    ))
}

/// Raster path 1: the font's embedded monochrome strike at exactly `size_px`
/// ppem, decoded to an 8-bit mask. `None` when the font has no strike, the
/// nearest strike has a different `pixels_per_em` (ttf-parser returns the
/// nearest one), the image is not 1-bit, or `size_px` does not fit a `u16`.
/// Placement: `left = image.x`; ttf-parser's `y` is the bitmap's *bottom* edge
/// relative to the baseline, so `top = y + height` (MS Gothic 18 px: `y = −3`,
/// `height = 18` → `top = 15` = the baseline row).
pub fn strike_mask(font: &Font, glyph: u16, size_px: u32) -> Option<StrikeMask> {
    let ppem = u16::try_from(size_px).ok()?;
    let img = font
        .rustybuzz()
        .glyph_raster_image(ttf_parser::GlyphId(glyph), ppem)?;
    if img.pixels_per_em != ppem {
        return None;
    }
    let packed = match img.format {
        ttf_parser::RasterImageFormat::BitmapMono => false,
        ttf_parser::RasterImageFormat::BitmapMonoPacked => true,
        _ => return None,
    };
    let (width, height) = (u32::from(img.width), u32::from(img.height));
    let data = decode_mono(img.data, width, height, packed)?;
    Some(StrikeMask {
        width,
        height,
        left: i32::from(img.x),
        top: i32::from(img.y) + height as i32,
        data,
    })
}

/// [`strike_mask`] as an atlas-ready image. A strike with no set bit (the
/// space) yields [`GlyphImage::blank`], so no atlas space is spent and path 2
/// is not tried.
pub fn strike_image(font: &Font, glyph: u16, size_px: u32) -> Option<GlyphImage> {
    let m = strike_mask(font, glyph, size_px)?;
    if m.data.iter().all(|&a| a == 0) {
        return Some(GlyphImage::blank());
    }
    Some(GlyphImage::from_mask(
        m.width, m.height, m.left, m.top, &m.data,
    ))
}

/// swash sources for path 2, in priority order. `Source::Bitmap` is deliberately absent.
const SOURCES: [Source; 3] = [
    Source::ColorBitmap(StrikeWith::BestFit),
    Source::ColorOutline(0),
    Source::Outline,
];

/// Just the outline source: what a *stroke* can be taken from. A colour
/// bitmap has no path to stroke, and `SOURCES`' bitmap-first order would
/// silently return an unstroked image.
const OUTLINE_ONLY: [Source; 1] = [Source::Outline];

/// Raster path 2: hinted outlines / colour glyphs through swash, with
/// synthetic bold (`embolden(size_px / 24)`) and synthetic italic (12° skew).
/// swash applies both only to `Source::Outline`. `Image.placement` is
/// baseline-relative with y up, the same convention as [`AtlasRegion`].
pub fn outline_image(
    ctx: &mut ScaleContext,
    font: &Font,
    glyph: u16,
    size_px: f32,
    flags: RasterFlags,
) -> Option<GlyphImage> {
    render_glyph(ctx, font, glyph, size_px, flags, &SOURCES, Style::default())
}

/// [`outline_image`] with the outline **stroked** `width_px` wide instead of
/// filled — GDI+ `Graphics.DrawPath(pen, path)`, which is what
/// `GraphicsImage.GDrawString` does after `FillPath`
/// (`Content/GraphicsImage.cs:137-141`).
///
/// zeno's default miter limit is 4; GDI+ `Pen.MiterLimit` defaults to 10, so
/// that is what is set here. Caps never apply: glyph subpaths are closed.
pub fn outline_stroke_image(
    ctx: &mut ScaleContext,
    font: &Font,
    glyph: u16,
    size_px: f32,
    flags: RasterFlags,
    width_px: f32,
) -> Option<GlyphImage> {
    let mut stroke = Stroke::new(width_px);
    stroke.miter_limit = 10.0;
    render_glyph(
        ctx,
        font,
        glyph,
        size_px,
        flags,
        &OUTLINE_ONLY,
        Style::Stroke(stroke),
    )
}

/// The shared swash render: pick the first usable `sources` entry, apply the
/// synthetic styles, fill or stroke it with `style`, and convert the result to
/// a [`GlyphImage`].
fn render_glyph(
    ctx: &mut ScaleContext,
    font: &Font,
    glyph: u16,
    size_px: f32,
    flags: RasterFlags,
    sources: &[Source],
    style: Style,
) -> Option<GlyphImage> {
    let mut scaler = ctx.builder(font.as_swash()).size(size_px).hint(true).build();
    let mut render = Render::new(sources);
    render.format(Format::Alpha);
    render.style(style);
    if flags.contains(RasterFlags::BOLD_SYNTH) {
        render.embolden(size_px / 24.0);
    }
    if flags.contains(RasterFlags::ITALIC_SYNTH) {
        render.transform(Some(Transform::skew(
            Angle::from_degrees(12.0),
            Angle::ZERO,
        )));
    }
    let image = render.render(&mut scaler, glyph)?;
    let p = image.placement;
    if p.width == 0 || p.height == 0 {
        return Some(GlyphImage::blank());
    }
    let n = (p.width * p.height) as usize;
    let out = match image.content {
        // `Mask` is sliced to `n` below, so a longer buffer is harmless;
        // `Color` moves `image.data` in whole, so its length must match exactly
        // or the image would not be `width × height`.
        Content::Mask => {
            if image.data.len() < n {
                return None;
            }
            GlyphImage::from_mask(p.width, p.height, p.left, p.top, &image.data[..n])
        }
        Content::Color => {
            if image.data.len() != n * 4 {
                return None;
            }
            GlyphImage {
                width: p.width,
                height: p.height,
                left: p.left,
                top: p.top,
                color: true,
                rgba: image.data,
            }
        }
        Content::SubpixelMask => {
            // Never produced with `Format::Alpha`; keep the coverage channel if it ever is.
            if image.data.len() != n * 4 {
                return None;
            }
            let mask: Vec<u8> = image.data.chunks_exact(4).map(|c| c[3]).collect();
            GlyphImage::from_mask(p.width, p.height, p.left, p.top, &mask)
        }
    };
    Some(out)
}

/// Choose the raster path for `key` (spec Component 6): the embedded strike
/// when allowed and exact, otherwise swash.
pub fn rasterize(
    ctx: &mut ScaleContext,
    font: &Font,
    key: RasterKey,
    use_bitmap_strikes: bool,
) -> Option<GlyphImage> {
    let size_px = key.size_px();
    let integer = size_px.fract() == 0.0 && size_px > 0.0 && size_px <= f32::from(u16::MAX);
    if use_bitmap_strikes && key.flags.is_empty() && integer {
        if let Some(img) = strike_image(font, key.glyph, size_px as u32) {
            return Some(img);
        }
    }
    outline_image(ctx, font, key.glyph, size_px, key.flags)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decode_mono_packed_rows() {
        // 3×2, rows `101` and `011`, bit-continuous: 1 0 1 0 1 1 0 0 = 0xAC.
        assert_eq!(
            decode_mono(&[0xAC], 3, 2, true),
            Some(vec![255, 0, 255, 0, 255, 255])
        );
    }

    #[test]
    fn decode_mono_byte_padded_rows() {
        // Same image, each row padded to a byte: 0b1010_0000, 0b0110_0000.
        assert_eq!(
            decode_mono(&[0xA0, 0x60], 3, 2, false),
            Some(vec![255, 0, 255, 0, 255, 255])
        );
    }

    #[test]
    fn decode_mono_rejects_short_data() {
        assert_eq!(decode_mono(&[0xA0], 3, 2, false), None);
        assert_eq!(decode_mono(&[], 0, 0, true), Some(vec![]));
    }

    #[test]
    fn place_opens_a_new_page_when_full() {
        let mut allocs = Vec::new();
        // 1025² (+1 px gutter) fits once per 2048² page.
        let p: Vec<(usize, u32, u32)> = (0..3)
            .map(|_| place(&mut allocs, 1025, 1025).unwrap())
            .collect();
        assert_eq!(p, vec![(0, 0, 0), (1, 0, 0), (2, 0, 0)]);
        assert_eq!(allocs.len(), 3);
        assert_eq!(place(&mut allocs, PAGE_SIZE, 4), None, "wider than a page");
        assert!(place(&mut allocs, 9, 18).is_some(), "small glyphs still fit");
    }

    #[test]
    fn raster_key_round_trips_size() {
        let k = RasterKey::new(fontdb::ID::dummy(), 7, 15.0, RasterFlags::BOLD_SYNTH);
        assert_eq!(k.size_px(), 15.0);
        assert_eq!(
            k,
            RasterKey::new(fontdb::ID::dummy(), 7, 15.0, RasterFlags::BOLD_SYNTH)
        );
        assert_ne!(
            k,
            RasterKey::new(fontdb::ID::dummy(), 7, 18.0, RasterFlags::BOLD_SYNTH)
        );
    }

    use std::path::{Path, PathBuf};
    use std::sync::Arc;

    use cosmic_text::FontSystem;

    const BUNDLED: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf");

    /// Load one face of a font file straight through fontdb/cosmic-text — no
    /// system fonts, no locale, independent of `FontChain`.
    fn load_face(path: &Path, index: u32) -> Arc<Font> {
        let mut db = fontdb::Database::new();
        db.load_font_data(std::fs::read(path).expect("read font file"));
        let id = db
            .faces()
            .find(|f| f.index == index)
            .expect("face index present")
            .id;
        let mut fs = FontSystem::new_with_locale_and_db("en-US".to_owned(), db);
        fs.get_font(id).expect("face loads")
    }

    fn gid(font: &Font, c: char) -> u16 {
        font.rustybuzz().glyph_index(c).expect("glyph present").0
    }

    /// `msgothic.ttc` inside `ERARS_FONT_DIR` (opt-in, never in CI); prints a
    /// `SKIP {test}` line and returns `None` otherwise.
    fn msgothic(test: &str) -> Option<PathBuf> {
        let found = std::env::var_os("ERARS_FONT_DIR").and_then(|dir| {
            std::fs::read_dir(dir)
                .ok()?
                .flatten()
                .map(|e| e.path())
                .find(|p| {
                    p.file_name()
                        .and_then(|n| n.to_str())
                        .is_some_and(|n| n.eq_ignore_ascii_case("msgothic.ttc"))
                })
        });
        if found.is_none() {
            eprintln!("SKIP {test}: ERARS_FONT_DIR does not contain msgothic.ttc");
        }
        found
    }

    fn only_black_or_white(img: &GlyphImage) -> bool {
        img.rgba
            .chunks_exact(4)
            .all(|p| p[..3] == [255, 255, 255] && (p[3] == 0 || p[3] == 255))
    }

    fn has_grey(img: &GlyphImage) -> bool {
        img.rgba.chunks_exact(4).any(|p| p[3] != 0 && p[3] != 255)
    }

    fn set_bits(m: &StrikeMask) -> usize {
        m.data.iter().filter(|&&v| v == 255).count()
    }

    #[test]
    fn outline_image_renders_bundled_a_antialiased() {
        let font = load_face(Path::new(BUNDLED), 0);
        let mut ctx = ScaleContext::new();
        let a = outline_image(&mut ctx, &font, gid(&font, 'A'), 18.0, RasterFlags::empty())
            .expect("A renders");
        assert!(!a.is_empty());
        assert!(!a.color);
        assert_eq!(a.rgba.len(), (a.width * a.height * 4) as usize);
        assert!(has_grey(&a), "hinted outlines are anti-aliased");
        assert!(
            a.top > 0 && a.top <= 18,
            "top {} is above the baseline, inside the em",
            a.top
        );
        // Blank glyph: swash returns a 2×0 image for the space.
        let sp = outline_image(&mut ctx, &font, gid(&font, ' '), 18.0, RasterFlags::empty())
            .expect("space renders");
        assert!(sp.is_empty());
    }

    #[test]
    fn outline_image_applies_synthetic_styles() {
        let font = load_face(Path::new(BUNDLED), 0);
        let mut ctx = ScaleContext::new();
        let g = gid(&font, 'A');
        let plain = outline_image(&mut ctx, &font, g, 18.0, RasterFlags::empty()).unwrap();
        let bold = outline_image(&mut ctx, &font, g, 18.0, RasterFlags::BOLD_SYNTH).unwrap();
        let italic = outline_image(&mut ctx, &font, g, 18.0, RasterFlags::ITALIC_SYNTH).unwrap();
        assert!(
            bold.width > plain.width,
            "embolden widens: {} vs {}",
            bold.width,
            plain.width
        );
        assert_ne!(italic.rgba, plain.rgba, "the 12° skew changes the pixels");
        assert!(!bold.is_empty() && !italic.is_empty());
    }

    #[test]
    fn bundled_font_has_no_strikes_so_rasterize_uses_outlines() {
        let font = load_face(Path::new(BUNDLED), 0);
        let g = gid(&font, 'A');
        assert_eq!(strike_mask(&font, g, 18), None);
        let mut ctx = ScaleContext::new();
        let key = RasterKey::new(font.id(), g, 18.0, RasterFlags::empty());
        let img = rasterize(&mut ctx, &font, key, true).expect("outline fallback");
        assert!(
            has_grey(&img),
            "no strike → hinted outline even with strikes enabled"
        );
    }

    /// GPU-free companion of the spec's Testing §5 strike tests: the raw
    /// ttf-parser result for `あ` at 18 ppem, the nearest-strike behaviour at
    /// 23 ppem, and the decoded masks.
    #[test]
    fn strike_mask_decodes_ms_gothic_at_18px() {
        let Some(path) = msgothic("strike_mask_decodes_ms_gothic_at_18px") else {
            return;
        };
        let font = load_face(&path, 0); // face 0 = MS Gothic (the monospace face)
        let face = font.rustybuzz();
        let a = face.glyph_index('あ').expect("あ in cmap");
        let raw = face.glyph_raster_image(a, 18).expect("18 ppem strike");
        assert_eq!(raw.pixels_per_em, 18);
        assert_eq!((raw.width, raw.height, raw.x, raw.y), (18, 18, 0, -3));
        assert_eq!(raw.format, ttf_parser::RasterImageFormat::BitmapMonoPacked);
        let near = face.glyph_raster_image(a, 23).expect("nearest strike");
        assert_eq!(near.pixels_per_em, 22, "ttf-parser picks the nearest strike");

        assert!(
            strike_mask(&font, a.0, 23).is_none(),
            "the 22 ppem strike must be rejected for 23 px"
        );
        let m = strike_mask(&font, a.0, 18).expect("exact strike accepted");
        assert_eq!((m.width, m.height, m.left, m.top), (18, 18, 0, 15));
        assert!(m.data.iter().all(|&v| v == 0 || v == 255));
        assert_eq!(set_bits(&m), 61, "あ @18 has 61 set bits");

        let latin = strike_mask(&font, gid(&font, 'A'), 18).unwrap();
        assert_eq!((latin.width, latin.height, latin.top), (9, 18, 15));
        assert_eq!(set_bits(&latin), 28);
        let dbl = strike_mask(&font, gid(&font, '═'), 18).unwrap();
        assert_eq!(dbl.width, 9, "═ is a half-width strike in MS Gothic");
        assert_eq!(set_bits(&dbl), 18);

        let img = strike_image(&font, a.0, 18).unwrap();
        assert!(only_black_or_white(&img));
        assert_eq!((img.width, img.height, img.left, img.top), (18, 18, 0, 15));
        let space = strike_image(&font, gid(&font, ' '), 18).expect("space has a strike");
        assert!(space.is_empty(), "an all-zero strike is blank");
    }

    #[test]
    fn rasterize_prefers_strikes_only_when_allowed() {
        let Some(path) = msgothic("rasterize_prefers_strikes_only_when_allowed") else {
            return;
        };
        let font = load_face(&path, 0);
        let mut ctx = ScaleContext::new();
        let g = gid(&font, 'あ');
        let key = RasterKey::new(font.id(), g, 18.0, RasterFlags::empty());
        let strike = rasterize(&mut ctx, &font, key, true).unwrap();
        assert!(only_black_or_white(&strike));
        assert_eq!((strike.width, strike.height), (18, 18));
        let outline = rasterize(&mut ctx, &font, key, false).unwrap();
        assert!(has_grey(&outline), "--no-bitmap-strikes renders outlines");
        let bold = rasterize(
            &mut ctx,
            &font,
            RasterKey::new(font.id(), g, 18.0, RasterFlags::BOLD_SYNTH),
            true,
        )
        .unwrap();
        assert!(has_grey(&bold), "synthetic styles bypass the strike path");
        let frac = rasterize(
            &mut ctx,
            &font,
            RasterKey::new(font.id(), g, 17.5, RasterFlags::empty()),
            true,
        )
        .unwrap();
        assert!(has_grey(&frac), "non-integer size_px uses outlines");
        let big = rasterize(
            &mut ctx,
            &font,
            RasterKey::new(font.id(), g, 23.0, RasterFlags::empty()),
            true,
        )
        .unwrap();
        assert!(has_grey(&big), "23 px has no exact strike → outlines");
    }
}
