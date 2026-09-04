//! Off-screen bitmap and sprite store backing Emuera's `G*` / `SPRITE*` commands.
//!
//! The store owns the *mutable* side of the image layer: scripts create, draw
//! into, blit between and query bitmaps here, and every query answers from the
//! real pixel data. The *immutable* side — the geometry rules and the decoded
//! snapshot every front-end draws from — lives in [`erars_ui::image`], and this
//! module re-uses those types rather than defining a second set, so a sprite
//! the VM stores and a sprite the renderer draws are literally the same value.
//!
//! Semantics mirror Emuera (`Emuera/Content/{GraphicsImage,CroppedImage,AppContents}.cs`
//! and `GameData/Function/Creator.Method.cs`):
//!
//! * a `Graphics` is a 32bpp ARGB bitmap, zero-filled on creation, i.e. every
//!   pixel starts at `0x00000000` (transparent black);
//! * pen / brush / font are per-bitmap state that lives until `GDISPOSE`;
//! * a sprite stores only its parent's *id* plus a source rectangle, so
//!   mutating the parent mutates the sprite and disposing the parent makes the
//!   sprite report "not created";
//! * sprite names are matched case-insensitively (Emuera upper-cases them).
//!
//! Emuera's renderer reads the live `Bitmap` because it has a single thread.
//! erars' renderer is a separate thread, so a mutation only marks its bitmap
//! dirty ([`GraphicsStore::publish`] then hands the changed pixels to an
//! [`ImageStore`] at the redraw boundary). Nothing in the blit hot paths
//! copies or allocates for this: `touch` inserts one `u32` into a set.

use erars_compiler::Language;
use erars_font::font::FontConfig;
use erars_font::text_image::{Coverage, TextFont, TextRasterizer, TextStyleBits};
use erars_ui::image::{
    BitmapId, ImageBitmap, ImageFrame, ImageGeometry, ImageSampler, ImageStore, InlineImage,
    InlineSprite, MixedNum,
};
use hashbrown::{HashMap, HashSet};
use std::borrow::Cow;
use std::marker::PhantomData;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub use erars_ui::image::Rect;

/// Emuera `AbstractImage.MAX_IMAGESIZE`.
pub const MAX_IMAGE_SIZE: i64 = 8192;

/// `GSETPEN` state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pen {
    /// `0xAARRGGBB`
    pub color: u32,
    pub width: i64,
}

/// `GSETFONT` state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Font {
    pub name: String,
    pub size: i64,
    /// Emuera `SETFONT` style bits: 1 bold, 2 italic, 4 strikeout, 8 underline.
    pub style: i64,
}

/// One created `Graphics`.
pub struct Bitmap {
    width: u32,
    height: u32,
    /// `0xAARRGGBB` per pixel, row-major, exactly `width * height` entries.
    pixels: Vec<u32>,
    brush: Option<u32>,
    pen: Option<Pen>,
    font: Option<Font>,
}

/// Everything `GDRAWTEXT` needs from outside the store: the configuration
/// defaults GDI+ falls back on and where the fonts come from.
///
/// Passing it per call keeps the store from holding a clone of `EraConfig`
/// while still building the font chain exactly once (see [`TextStack`]).
pub struct TextEnv<'a> {
    /// `Config.FontName` — the family of the default 100 px font
    /// (`GraphicsImage.cs:127`).
    pub family: &'a str,
    /// `GlobalStatic.Console.StringStyle.FontStyle` (`GraphicsImage.cs:127`):
    /// the console's *current* style, which the default font inherits.
    pub console_style: i64,
    /// `Config.ForeColor` as `0xAARRGGBB` — the brush and the pen when the
    /// bitmap has neither (`GraphicsImage.cs:135,140`).
    pub fore_color: u32,
    /// Game directory; `<game>/font/*` joins the chain (Emuera.EM behaviour).
    pub game_dir: &'a Path,
    pub lang: Language,
}

impl TextEnv<'_> {
    /// The font chain for this game: configured family → `<game>/font` →
    /// `ERARS_FONT_DIR` → the language's fixed-pitch CJK list → the bundled
    /// font. Identical to the renderer's `shaper_for`, so `GDRAWTEXT` and the
    /// console cannot resolve one family to two different faces.
    fn rasterizer(&self) -> TextRasterizer {
        TextRasterizer::new(&FontConfig {
            family: self.family,
            game_dir: self.game_dir,
            extra_dir: std::env::var_os("ERARS_FONT_DIR").map(PathBuf::from),
            lang: self.lang,
        })
    }
}

impl Bitmap {
    fn new(width: u32, height: u32) -> Self {
        Self {
            width,
            height,
            pixels: vec![0; width as usize * height as usize],
            brush: None,
            pen: None,
            font: None,
        }
    }

    #[inline]
    fn idx(&self, x: u32, y: u32) -> usize {
        y as usize * self.width as usize + x as usize
    }

    #[inline]
    fn in_bounds(&self, x: i64, y: i64) -> bool {
        x >= 0 && y >= 0 && x < self.width as i64 && y < self.height as i64
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn pixels(&self) -> &[u32] {
        &self.pixels
    }

    pub fn brush(&self) -> Option<u32> {
        self.brush
    }

    pub fn pen(&self) -> Option<Pen> {
        self.pen
    }

    pub fn font(&self) -> Option<&Font> {
        self.font.as_ref()
    }

    /// Fill the whole bitmap, replacing (not blending) every pixel — GDI+
    /// `Graphics.Clear`.
    fn clear(&mut self, color: u32) {
        self.pixels.fill(color);
    }

    /// Composite one anti-aliased coverage layer in `color`, with the layer's
    /// layout origin at `(x, y)` — GDI+ `FillPath` / `DrawPath` with a solid
    /// brush or pen at `SmoothingMode.AntiAlias` (`GraphicsImage.cs:131-141`).
    ///
    /// The coverage scales the paint's *alpha*; the result is composited with
    /// `CompositingMode.SourceOver`, the default every Emuera draw uses. So a
    /// half-covered pixel of an opaque colour lands as that colour at alpha
    /// 128, which is what makes `GDRAWTEXT` onto a transparent bitmap keep the
    /// text colour exact and put the anti-aliasing in the alpha channel.
    fn blend_coverage(&mut self, cov: &Coverage, x: i64, y: i64, color: u32) {
        let ca = color >> 24;
        if cov.is_empty() || ca == 0 {
            return;
        }
        let rgb = color & 0x00FF_FFFF;
        let left = x + i64::from(cov.left);
        let top = y + i64::from(cov.top);
        // The horizontal window is the same for every row, so it is clipped
        // once instead of bounds-checking each pixel.
        let cols = i64::from(cov.width);
        let c0 = (-left).clamp(0, cols) as usize;
        let c1 = (i64::from(self.width) - left).clamp(0, cols) as usize;
        if c0 >= c1 {
            return;
        }
        let stride = cov.width as usize;
        for row in 0..cov.height as usize {
            let py = top + row as i64;
            if py < 0 {
                continue;
            }
            if py >= i64::from(self.height) {
                break;
            }
            let dst = py as usize * self.width as usize + (left + c0 as i64) as usize;
            for i in c0..c1 {
                let a = u32::from(cov.data[row * stride + i]);
                if a == 0 {
                    continue;
                }
                // `(ca · a + 127) / 255`: the exact 8-bit product, rounded.
                let sa = (ca * a + 127) / 255;
                let p = &mut self.pixels[dst + i - c0];
                *p = blend_over(sa << 24 | rgb, *p);
            }
        }
    }

    /// `rect` clipped to the bitmap, as an `x` range by a `y` range.
    ///
    /// Both ends are clamped, not just the far one: a rect starting past the
    /// bitmap's own edge has to come back as an *empty* range, and clamping
    /// only the end would leave `start > end`.
    fn clip(&self, rect: Rect) -> (Range<u32>, Range<u32>) {
        let rect = rect.normalized();
        let clamp = |v: i32, max: u32| v.clamp(0, max as i32) as u32;
        (
            clamp(rect.x, self.width)..clamp(rect.x.saturating_add(rect.width), self.width),
            clamp(rect.y, self.height)..clamp(rect.y.saturating_add(rect.height), self.height),
        )
    }

    /// The clipped pixels of one row.
    fn row(&mut self, y: u32, xs: &Range<u32>) -> &mut [u32] {
        let row = self.idx(0, y);
        &mut self.pixels[row + xs.start as usize..row + xs.end as usize]
    }

    /// `GCLEAR` with the EM 6-argument rect form: clip to the bitmap, then
    /// replace.
    fn clear_rect(&mut self, color: u32, rect: Rect) {
        let (xs, ys) = self.clip(rect);
        for y in ys {
            self.row(y, &xs).fill(color);
        }
    }

    /// `GFILLRECTANGLE` (`Content/GraphicsImage.cs:188-203`): clip to the
    /// bitmap, then composite **source-over**.
    ///
    /// GDI+ `FillRectangle` with a `SolidBrush` blends, where the
    /// `Graphics.Clear` behind `GCLEAR` replaces — so a translucent brush
    /// tints what is already on the bitmap instead of overwriting it.
    fn fill_rect(&mut self, color: u32, rect: Rect) {
        let (xs, ys) = self.clip(rect);
        for y in ys {
            for px in self.row(y, &xs) {
                *px = blend_over(color, *px);
            }
        }
    }
}

/// A `SPRITECREATE`d or `SPRITEANIMECREATE`d sprite.
///
/// This is [`erars_ui::image::InlineSprite`] itself, not a copy of it: the
/// fields Emuera's `ASprite` exposes (`sampler` for `ASpriteSingle` vs
/// `SpriteAnime`, `DestBaseSize` as `width`/`height`, `DestBasePosition` as
/// `pos_x`/`pos_y`, `Content/CroppedImage.cs:21-49`) are exactly the fields a
/// front-end needs to draw one, so `SPRITE*` mutates and `PRINT_IMG` reads the
/// same struct with no conversion step to drift.
pub type Sprite = InlineSprite;

/// What [`GraphicsStore::sprite_get_color`] found under a point.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpriteColor {
    /// No such sprite, its parent bitmap is gone, or the point is outside the
    /// sprite's box — all three of Emuera's `-1` exits.
    Missing,
    /// An animated sprite: `SpriteAnime.SpriteGetColor` is nothing but
    /// `throw new NotSupportedException()` (`CroppedImage.cs:273-286`).
    Unsupported,
    /// `0xAARRGGBB`.
    Color(u32),
}

/// VM-wide graphics state: `id -> bitmap` and `NAME -> sprite`.
#[derive(Default)]
pub struct GraphicsStore {
    bitmaps: HashMap<u32, Bitmap>,
    sprites: HashMap<String, Sprite>,
    /// `resources/` parent images already decoded, keyed by resolved path —
    /// Emuera's `resourceDic` (`Content/AppContents.cs:18`), which is what
    /// stops one shared sheet being decoded once per sprite that crops it.
    resources: HashMap<String, u32>,
    /// Bitmap ids whose pixels changed, or which were created or disposed,
    /// since the last [`GraphicsStore::publish`].
    dirty: HashSet<u32>,
    /// Monotonic publish counter. Generations must never repeat for an id, so
    /// a `GDISPOSE`+`GCREATE` cannot make a renderer's `(id, generation)`
    /// texture cache believe stale pixels are current; one global counter is
    /// the cheapest way to guarantee it.
    generation: u64,
    /// The `GDRAWTEXT` font stack, built on first use.
    text: Option<TextRasterizer>,
}

/// Upper-case a sprite name the way Emuera's `AppContents` does, in place when
/// the name is ASCII.
fn normalize_sprite_name(mut name: String) -> String {
    if name.is_ascii() {
        name.make_ascii_uppercase();
        name
    } else {
        name.to_uppercase()
    }
}

/// The map key for `name`, borrowed when the name is already upper-case —
/// which it always is for names that came back out of the store.
fn sprite_key(name: &str) -> Cow<'_, str> {
    if name.chars().any(char::is_lowercase) {
        Cow::Owned(normalize_sprite_name(name.to_owned()))
    } else {
        Cow::Borrowed(name)
    }
}

impl GraphicsStore {
    pub fn get(&self, id: u32) -> Option<&Bitmap> {
        self.bitmaps.get(&id)
    }

    /// Mark `id`'s pixels as needing republication. Cheap enough to call from
    /// every mutation: one `u32` into a set that is drained per redraw.
    #[inline]
    fn touch(&mut self, id: u32) {
        self.dirty.insert(id);
    }

    /// Look up a sprite by its case-insensitive name.
    pub fn sprite(&self, name: &str) -> Option<&Sprite> {
        self.sprites.get(sprite_key(name).as_ref())
    }

    /// `GCREATED`
    pub fn created(&self, id: u32) -> bool {
        self.bitmaps.contains_key(&id)
    }

    /// `GCREATE`. `false` when `id` already exists (Emuera requires an explicit
    /// `GDISPOSE` first). Caller validates `width` / `height` ranges.
    pub fn create(&mut self, id: u32, width: u32, height: u32) -> bool {
        if self.bitmaps.contains_key(&id) {
            return false;
        }
        self.bitmaps.insert(id, Bitmap::new(width, height));
        self.touch(id);
        true
    }

    /// `GDISPOSE`. `false` when `id` was not created.
    ///
    /// Sprites are *not* removed: Emuera's sprite holds the `GraphicsImage`
    /// object, which merely reports `IsCreated == false` afterwards, so a later
    /// `GCREATE` on the same id revives every sprite that referenced it.
    pub fn dispose(&mut self, id: u32) -> bool {
        let existed = self.bitmaps.remove(&id).is_some();
        if existed {
            // Publishing a dirty-but-absent id retracts it from the
            // `ImageStore`, so a disposed bitmap stops being drawn.
            self.touch(id);
        }
        existed
    }

    /// `GWIDTH` — 0 when not created.
    pub fn width(&self, id: u32) -> u32 {
        self.bitmaps.get(&id).map_or(0, |b| b.width)
    }

    /// `GHEIGHT` — 0 when not created.
    pub fn height(&self, id: u32) -> u32 {
        self.bitmaps.get(&id).map_or(0, |b| b.height)
    }

    /// `GGETCOLOR` — `None` when not created or out of bounds (the caller turns
    /// that into Emuera's `-1`).
    pub fn get_color(&self, id: u32, x: i64, y: i64) -> Option<u32> {
        let b = self.bitmaps.get(&id)?;
        if !b.in_bounds(x, y) {
            return None;
        }
        Some(b.pixels[b.idx(x as u32, y as u32)])
    }

    /// `GSETCOLOR` — replaces the pixel outright, alpha included.
    pub fn set_color(&mut self, id: u32, color: u32, x: i64, y: i64) -> bool {
        let Some(b) = self.bitmaps.get_mut(&id) else {
            return false;
        };
        if !b.in_bounds(x, y) {
            return false;
        }
        let idx = b.idx(x as u32, y as u32);
        b.pixels[idx] = color;
        self.touch(id);
        true
    }

    /// `GCLEAR id, color`
    pub fn clear(&mut self, id: u32, color: u32) -> bool {
        match self.bitmaps.get_mut(&id) {
            Some(b) => {
                b.clear(color);
                self.touch(id);
                true
            }
            None => false,
        }
    }

    /// `GCLEAR id, color, x, y, w, h`
    pub fn clear_rect(&mut self, id: u32, color: u32, rect: Rect) -> bool {
        match self.bitmaps.get_mut(&id) {
            Some(b) => {
                b.clear_rect(color, rect);
                self.touch(id);
                true
            }
            None => false,
        }
    }

    /// `GFILLRECTANGLE id, x, y, w, h` — fill with this bitmap's `GSETBRUSH`
    /// colour, or `no_brush` when it never had one.
    ///
    /// The colour is *not* an argument: `GraphicsFillRectangleMethod` takes
    /// five integers and hands the rect straight to `GFillRectangle`, which
    /// picks `brush ?? new SolidBrush(Config.BackColor)`
    /// (`Creator.Method.cs:6157-6168`, `Content/GraphicsImage.cs:190-203`).
    /// The doc comment above the method claims a `cARGB` second argument; the
    /// `argumentTypeArray` beside it has five entries and no colour, and that
    /// is what actually runs.
    pub fn fill_rect(&mut self, id: u32, rect: Rect, no_brush: u32) -> bool {
        match self.bitmaps.get_mut(&id) {
            Some(b) => {
                let color = b.brush.unwrap_or(no_brush);
                b.fill_rect(color, rect);
                self.touch(id);
                true
            }
            None => false,
        }
    }

    /// `GSETBRUSH`
    pub fn set_brush(&mut self, id: u32, color: u32) -> bool {
        match self.bitmaps.get_mut(&id) {
            Some(b) => {
                b.brush = Some(color);
                true
            }
            None => false,
        }
    }

    /// `GSETPEN`
    pub fn set_pen(&mut self, id: u32, pen: Pen) -> bool {
        match self.bitmaps.get_mut(&id) {
            Some(b) => {
                b.pen = Some(pen);
                true
            }
            None => false,
        }
    }

    /// `GSETFONT`
    pub fn set_font(&mut self, id: u32, font: Font) -> bool {
        match self.bitmaps.get_mut(&id) {
            Some(b) => {
                b.font = Some(font);
                true
            }
            None => false,
        }
    }

    /// `GDRAWTEXT` — draw `text` with this bitmap's GDI+ state, the top-left
    /// corner of its layout box at `(x, y)`, and report the extent
    /// `MeasureString` would have returned (`Creator.Method.cs:5549-5564`).
    ///
    /// `None` when `id` was never created: `GraphicsDrawStringMethod` returns
    /// 0 before it measures anything (`Creator.Method.cs:5537-5538`), so the
    /// caller must leave `RESULT:1`/`:2` untouched.
    ///
    /// Both GDI+ passes run: the path is filled with the `GSETBRUSH` colour
    /// (or `Config.ForeColor`) and then stroked with the `GSETPEN` colour (or
    /// a default `Pen(Config.ForeColor)`, width 1) —
    /// `Content/GraphicsImage.cs:131-141`.
    pub fn draw_text(
        &mut self,
        id: u32,
        text: &str,
        x: i64,
        y: i64,
        env: &TextEnv,
    ) -> Option<(f64, f64)> {
        let (brush, pen, font) = {
            let b = self.bitmaps.get(&id)?;
            (b.brush, b.pen, b.font.clone())
        };

        let face = match &font {
            Some(f) => TextFont {
                family: &f.name,
                size_px: f.size as f64,
                style: TextStyleBits(f.style),
            },
            // `usingFont == null` → `new Font(Config.FontName, 100,
            // Console.StringStyle.FontStyle, GraphicsUnit.Pixel)`
            // (`GraphicsImage.cs:126-127`).
            None => TextFont {
                family: env.family,
                size_px: erars_font::text_image::DEFAULT_FONT_SIZE_PX,
                style: TextStyleBits(env.console_style),
            },
        };
        // `new Pen(Color)` is 1 px wide; `GSETPEN` passes its own width
        // (`Creator.Method.cs:5467`), and GDI+ draws a 0-width pen as exactly
        // one device pixel — a negative width has no defined rendering, so it
        // takes the same path.
        let stroke_px = match pen {
            Some(p) if p.width > 1 => p.width as f64,
            _ => 1.0,
        };

        // `FontChain::new` reads the system font database, so the stack is
        // built on the first `GDRAWTEXT` and never for a game that draws none.
        let image = self
            .text
            .get_or_insert_with(|| env.rasterizer())
            .draw(&face, text, stroke_px);

        let b = self.bitmaps.get_mut(&id)?;
        b.blend_coverage(&image.fill, x, y, brush.unwrap_or(env.fore_color));
        b.blend_coverage(&image.stroke, x, y, pen.map_or(env.fore_color, |p| p.color));
        self.touch(id);
        Some((image.measured_width, image.measured_height))
    }

    /// `GDRAWG` — blit `src_rect` of `src_id` into `dest_rect` of `dest_id`,
    /// source-over, nearest-neighbour when the extents differ, optionally
    /// recolouring each source pixel through `cm`. `false` when either bitmap
    /// is missing.
    pub fn draw_g(
        &mut self,
        dest_id: u32,
        src_id: u32,
        dest_rect: Rect,
        src_rect: Rect,
        cm: Option<&ColorMatrix>,
    ) -> bool {
        if !self.bitmaps.contains_key(&dest_id) {
            return false;
        }

        if dest_id == src_id {
            // Self-blit: snapshot once so overlapping rects read pre-blit
            // pixels, as GDI+ does.
            let Some(mut bmp) = self.bitmaps.remove(&dest_id) else {
                return false;
            };
            let snapshot = bmp.pixels.clone();
            let (w, h) = (bmp.width, bmp.height);
            blit(&mut bmp, &snapshot, w, h, dest_rect, src_rect, cm);
            self.bitmaps.insert(dest_id, bmp);
            self.touch(dest_id);
            return true;
        }

        // Move the source out so both halves can be borrowed; this moves a
        // `Bitmap` header, never the pixels.
        let Some(src) = self.bitmaps.remove(&src_id) else {
            return false;
        };
        let drawn = match self.bitmaps.get_mut(&dest_id) {
            Some(dest) => {
                blit(
                    dest,
                    &src.pixels,
                    src.width,
                    src.height,
                    dest_rect,
                    src_rect,
                    cm,
                );
                true
            }
            None => false,
        };
        self.bitmaps.insert(src_id, src);
        if drawn {
            self.touch(dest_id);
        }
        drawn
    }

    /// `GDRAWGWITHMASK` — 1:1 blit of `src_id` at `(dest_x, dest_y)` using the
    /// **blue** channel of `mask_id` as per-pixel opacity.
    ///
    /// Emuera requires the source and mask to match exactly and the blit to fit
    /// inside the destination; anything else returns `false`.
    pub fn draw_g_with_mask(
        &mut self,
        dest_id: u32,
        src_id: u32,
        mask_id: u32,
        dest_x: i32,
        dest_y: i32,
    ) -> bool {
        let Some((dw, dh)) = self.bitmaps.get(&dest_id).map(|b| (b.width, b.height)) else {
            return false;
        };
        let Some((sw, sh)) = self.bitmaps.get(&src_id).map(|b| (b.width, b.height)) else {
            return false;
        };
        let Some((mw, mh)) = self.bitmaps.get(&mask_id).map(|b| (b.width, b.height)) else {
            return false;
        };

        if sw != mw || sh != mh {
            return false;
        }
        // Emuera checks only the far edges and would index out of range on a
        // negative offset; reject those instead of panicking.
        if dest_x < 0 || dest_y < 0 {
            return false;
        }
        if dest_x as i64 + sw as i64 > dw as i64 || dest_y as i64 + sh as i64 > dh as i64 {
            return false;
        }

        // `src_id` and `mask_id` may name the same bitmap, and either may be
        // `dest_id`; snapshot both rather than juggling three borrows.
        let src = self.bitmaps[&src_id].pixels.clone();
        let mask = if mask_id == src_id {
            None
        } else {
            Some(self.bitmaps[&mask_id].pixels.clone())
        };
        let mask = mask.as_deref().unwrap_or(&src);

        let dest = self.bitmaps.get_mut(&dest_id).unwrap();
        for y in 0..sh {
            let src_row = y as usize * sw as usize;
            let dest_row = (dest_y as u32 + y) as usize * dw as usize + dest_x as usize;
            for x in 0..sw as usize {
                dest.pixels[dest_row + x] = blend_mask(
                    src[src_row + x],
                    dest.pixels[dest_row + x],
                    mask[src_row + x],
                );
            }
        }
        self.touch(dest_id);
        true
    }

    /// `GDRAWSPRITE` — `dest_rect` is the caller's destination rect *before*
    /// Emuera's `DestBasePosition` adjustment, which this applies.
    pub fn draw_sprite(
        &mut self,
        dest_id: u32,
        name: &str,
        dest_rect: Rect,
        cm: Option<&ColorMatrix>,
    ) -> bool {
        let Some(sprite) = self.sprite(name) else {
            return false;
        };
        let ImageSampler::Single { bitmap: gid, src } = sprite.sampler else {
            // `SpriteAnime.GraphicsDraw` picks its frame from the wall clock
            // (`CroppedImage.cs:219-254`); see `sprite_anime_create`.
            return false;
        };
        if !self.bitmaps.contains_key(&gid) {
            return false;
        }

        let mut dest_rect = dest_rect;
        // `ASpriteSingle.GraphicsDraw`: shift the destination by the sprite
        // position, scaled by the destination/source extent ratio.
        if sprite.pos_x != 0 || sprite.pos_y != 0 {
            if src.width != 0 {
                dest_rect.x =
                    dest_rect.x.saturating_add(sprite.pos_x * dest_rect.width / src.width);
            }
            if src.height != 0 {
                dest_rect.y = dest_rect
                    .y
                    .saturating_add(sprite.pos_y * dest_rect.height / src.height);
            }
        }

        self.draw_g(dest_id, gid, dest_rect, src, cm)
    }

    /// `SPRITECREATE`. `Ok(false)` on Emuera's soft failures (empty name, name
    /// already live, parent not created); `Err(())` when an explicit rect misses
    /// the parent entirely, which Emuera reports as a script error.
    pub fn sprite_create(
        &mut self,
        name: String,
        gid: u32,
        rect: Option<Rect>,
    ) -> Result<bool, ()> {
        if name.is_empty() {
            return Ok(false);
        }
        let name = normalize_sprite_name(name);
        if self.sprite_created(&name) {
            return Ok(false);
        }
        let Some((gw, gh)) = self.bitmaps.get(&gid).map(|b| (b.width, b.height)) else {
            return Ok(false);
        };

        let src = match rect {
            Some(rect) => {
                if !rect.intersects_size(gw, gh) {
                    return Err(());
                }
                rect
            }
            None => Rect::new(0, 0, gw as i32, gh as i32),
        };

        self.sprites.insert(
            name,
            Sprite {
                sampler: ImageSampler::Single { bitmap: gid, src },
                width: src.width.unsigned_abs(),
                height: src.height.unsigned_abs(),
                pos_x: 0,
                pos_y: 0,
            },
        );
        Ok(true)
    }

    /// The anonymous sprite `CBG_SetGraphics` wraps around a whole bitmap:
    /// `new SpriteG("", gra, new Rectangle(0, 0, gra.Width, gra.Height))`
    /// (`GameView/EmueraConsole.cs:190`). `None` when the bitmap does not
    /// exist, which is that method's `!gra.IsCreated` arm.
    ///
    /// Not registered in the sprite map — it has no name to be found by, and
    /// Emuera disposes it when the plane drops the entry.
    pub fn bitmap_sprite(&self, gid: u32) -> Option<Sprite> {
        let (gw, gh) = self.bitmaps.get(&gid).map(|b| (b.width, b.height))?;
        Some(Sprite {
            sampler: ImageSampler::Single {
                bitmap: gid,
                src: Rect::new(0, 0, gw as i32, gh as i32),
            },
            width: gw,
            height: gh,
            pos_x: 0,
            pos_y: 0,
        })
    }

    /// A `resources/` CSV sprite: `new SpriteF(name, parentImage, rect, pos)`
    /// (`Content/AppContents.cs:311`).
    ///
    /// Unlike [`GraphicsStore::sprite_create`] this carries a
    /// `DestBasePosition` and never fails hard — the CSV loader has already
    /// warned about a bad rect and a taken name is its caller's diagnostic, so
    /// `false` only means "the name was already defined; the first wins".
    pub fn sprite_create_at(
        &mut self,
        name: String,
        gid: u32,
        src: Rect,
        pos_x: i32,
        pos_y: i32,
    ) -> bool {
        if name.is_empty() {
            return false;
        }
        let name = normalize_sprite_name(name);
        // The raw map, not `sprite_created`: at load time nothing has been
        // disposed, and Emuera tests `resourceImageDictionary.ContainsKey`.
        if self.sprites.contains_key(&name) {
            return false;
        }
        if !self.bitmaps.contains_key(&gid) {
            return false;
        }

        self.sprites.insert(
            name,
            Sprite {
                sampler: ImageSampler::Single { bitmap: gid, src },
                width: src.width.unsigned_abs(),
                height: src.height.unsigned_abs(),
                pos_x,
                pos_y,
            },
        );
        true
    }

    /// Decode a `resources/` parent image once and return its bitmap id —
    /// Emuera's `resourceDic` cache (`Content/AppContents.cs:220-256`).
    ///
    /// `name` is the CSV's second column, upper-cased and possibly written
    /// with `\` separators, resolved against the CSV's own directory. The id
    /// is allocated downward from `u32::MAX`, above every script-reachable id
    /// (`graphics_id` caps those at `i32::MAX`), because Emuera keeps resource
    /// parents in a dictionary `GCREATE` cannot address.
    ///
    /// An oversize image is installed *and* reported: Emuera stopped rejecting
    /// those when a shipped game turned out to contain one (`:236-243`).
    pub fn resource_bitmap(
        &mut self,
        dir: &Path,
        name: &str,
    ) -> Result<u32, crate::resources::ResourceImageError> {
        use crate::resources::ResourceImageError as E;

        let Some(path) = crate::resources::resolve_path(dir, name) else {
            return Err(E::NotFound);
        };
        let key = path.to_string_lossy().into_owned();
        if let Some(id) = self.resources.get(&key) {
            return Ok(*id);
        }

        // Same reader as `GCREATEFROMFILE`: the codec comes from the magic
        // bytes, never the extension.
        let Ok(img) = image::ImageReader::open(&path)
            .and_then(image::ImageReader::with_guessed_format)
            .map_err(image::ImageError::IoError)
            .and_then(|r| r.decode())
        else {
            return Err(E::Undecodable);
        };
        let img = img.to_rgba8();
        let (width, height) = (img.width(), img.height());
        if width == 0 || height == 0 {
            return Err(E::Undecodable);
        }

        let id = u32::MAX - self.resources.len() as u32;
        let mut bmp = Bitmap::new(width, height);
        for (dst, px) in bmp.pixels.iter_mut().zip(img.pixels()) {
            let [r, g, b, a] = px.0;
            *dst = (a as u32) << 24 | (r as u32) << 16 | (g as u32) << 8 | b as u32;
        }
        self.bitmaps.insert(id, bmp);
        self.touch(id);
        self.resources.insert(key, id);

        if width as i64 > MAX_IMAGE_SIZE || height as i64 > MAX_IMAGE_SIZE {
            return Err(E::TooLarge(id));
        }
        Ok(id)
    }

    /// `SPRITECREATED` — a `SPRITECREATE` sprite counts as created only while
    /// its parent bitmap is; a `SPRITEANIMECREATE` sprite always does
    /// (`CroppedImage.cs:256-259`).
    pub fn sprite_created(&self, name: &str) -> bool {
        self.live_sprite(name).is_some()
    }

    fn live_sprite(&self, name: &str) -> Option<&Sprite> {
        self.sprite(name).filter(|s| match s.sampler {
            ImageSampler::Single { bitmap, .. } => self.bitmaps.contains_key(&bitmap),
            ImageSampler::Anime { .. } => true,
        })
    }

    /// `SPRITEANIMECREATE(name, width, height)` — create an empty animated
    /// sprite (`Creator.Method.cs:6404-6431`). `false` on Emuera's soft
    /// failures: an empty name, or a name that is already live. The caller
    /// range-checks `width` / `height`, which Emuera reports as script errors.
    pub fn sprite_anime_create(&mut self, name: String, width: u32, height: u32) -> bool {
        if name.is_empty() {
            return false;
        }
        let name = normalize_sprite_name(name);
        if self.sprite_created(&name) {
            return false;
        }
        self.sprites.insert(
            name,
            Sprite {
                sampler: ImageSampler::Anime { frames: Vec::new(), total_ms: 0 },
                width,
                height,
                pos_x: 0,
                pos_y: 0,
            },
        );
        true
    }

    /// `SPRITEANIMEADDFRAME(name, gid, x, y, w, h, offsetX, offsetY, delay)`
    /// (`Creator.Method.cs:6446-6471`). `false` on every soft failure Emuera
    /// has here: an empty or unknown name, a name that is not an animated
    /// sprite, an uncreated parent, a non-positive source rect or one that
    /// leaves the parent, and a non-positive delay.
    pub fn sprite_anime_add_frame(
        &mut self,
        name: &str,
        gid: u32,
        src: Rect,
        offset_x: i32,
        offset_y: i32,
        delay: i64,
    ) -> bool {
        if name.is_empty() || delay <= 0 || delay > i32::MAX as i64 {
            return false;
        }
        let Some((gw, gh)) = self.bitmaps.get(&gid).map(|b| (b.width, b.height)) else {
            return false;
        };
        if src.width <= 0
            || src.height <= 0
            || src.x < 0
            || src.y < 0
            || src.x as i64 + src.width as i64 > gw as i64
            || src.y as i64 + src.height as i64 > gh as i64
        {
            return false;
        }
        let Some(sprite) = self.sprites.get_mut(sprite_key(name).as_ref()) else {
            return false;
        };
        let (dest_w, dest_h) = (sprite.width, sprite.height);
        let ImageSampler::Anime { frames, total_ms } = &mut sprite.sampler else {
            return false;
        };

        // `AnimeFrame.Normalize` (`CroppedImage.cs:166-178`): clip the frame's
        // destination box against the sprite's own box. `Rectangle.Intersect`
        // of disjoint rects is empty, which nulls the frame's image — the
        // frame stays in the list and still spends its delay.
        let x0 = offset_x.max(0);
        let y0 = offset_y.max(0);
        let x1 = (offset_x.saturating_add(src.width)).min(dest_w as i32);
        let y1 = (offset_y.saturating_add(src.height)).min(dest_h as i32);
        let empty = x1 <= x0 || y1 <= y0;

        let frame = if empty {
            ImageFrame {
                bitmap: gid,
                src,
                offset_x,
                offset_y,
                delay_ms: delay as u32,
                empty: true,
            }
        } else {
            ImageFrame {
                bitmap: gid,
                src: Rect::new(src.x, src.y, x1 - x0, y1 - y0),
                offset_x: x0,
                offset_y: y0,
                delay_ms: delay as u32,
                empty: false,
            }
        };
        *total_ms += frame.delay_ms as u64;
        frames.push(frame);
        true
    }

    /// `SPRITEWIDTH` — 0 when not created.
    pub fn sprite_width(&self, name: &str) -> u32 {
        self.live_sprite(name).map_or(0, |s| s.width)
    }

    /// `SPRITEHEIGHT` — 0 when not created.
    pub fn sprite_height(&self, name: &str) -> u32 {
        self.live_sprite(name).map_or(0, |s| s.height)
    }

    /// `SPRITEPOSX` — 0 when not created (indistinguishable from a real 0, as
    /// in Emuera).
    pub fn sprite_pos_x(&self, name: &str) -> i32 {
        self.live_sprite(name).map_or(0, |s| s.pos_x)
    }

    /// `SPRITEPOSY` — see [`Self::sprite_pos_x`].
    pub fn sprite_pos_y(&self, name: &str) -> i32 {
        self.live_sprite(name).map_or(0, |s| s.pos_y)
    }

    /// `SPRITESETPOS`
    pub fn sprite_set_pos(&mut self, name: &str, x: i32, y: i32) -> bool {
        if !self.sprite_created(name) {
            return false;
        }
        let sprite = self.sprites.get_mut(sprite_key(name).as_ref()).unwrap();
        sprite.pos_x = x;
        sprite.pos_y = y;
        true
    }

    /// `SPRITEMOVE`
    pub fn sprite_move(&mut self, name: &str, dx: i32, dy: i32) -> bool {
        if !self.sprite_created(name) {
            return false;
        }
        let sprite = self.sprites.get_mut(sprite_key(name).as_ref()).unwrap();
        sprite.pos_x = sprite.pos_x.saturating_add(dx);
        sprite.pos_y = sprite.pos_y.saturating_add(dy);
        true
    }

    /// `SPRITEDISPOSE`
    pub fn sprite_dispose(&mut self, name: &str) -> bool {
        if !self.sprite_created(name) {
            return false;
        }
        self.sprites.remove(sprite_key(name).as_ref());
        true
    }

    /// `GSAVE` — write `id` to `path` as PNG. `false` when `id` is not created
    /// or the write fails.
    pub fn save_image(&self, id: u32, path: &Path) -> bool {
        let Some(b) = self.bitmaps.get(&id) else {
            return false;
        };

        if let Some(dir) = path.parent() {
            if !dir.as_os_str().is_empty() && std::fs::create_dir_all(dir).is_err() {
                return false;
            }
        }

        let mut rgba = Vec::with_capacity(b.pixels.len() * 4);
        for &p in &b.pixels {
            rgba.extend_from_slice(&[(p >> 16) as u8, (p >> 8) as u8, p as u8, (p >> 24) as u8]);
        }

        match image::RgbaImage::from_raw(b.width, b.height, rgba) {
            Some(img) => img.save_with_format(path, image::ImageFormat::Png).is_ok(),
            None => false,
        }
    }

    /// `GLOAD` and `GCREATEFROMFILE` — decode `path` and create `id` from it.
    /// `false` when `id` already exists, the file is missing/undecodable, or it
    /// is larger than [`MAX_IMAGE_SIZE`] on either axis. Emuera shares one
    /// `Image.FromFile` path between the two
    /// (`Creator.Method.cs:5903-5962` and the `GLOAD` arm above it).
    pub fn load_image(&mut self, id: u32, path: &Path) -> bool {
        if self.bitmaps.contains_key(&id) {
            return false;
        }
        // GDI+ (and therefore Emuera) picks the codec from the file's magic
        // bytes, never from its name; `image::open` would trust the extension.
        let Ok(img) = image::ImageReader::open(path)
            .and_then(image::ImageReader::with_guessed_format)
            .map_err(image::ImageError::IoError)
            .and_then(|r| r.decode())
        else {
            return false;
        };
        let img = img.to_rgba8();
        let (width, height) = (img.width(), img.height());
        if width == 0
            || height == 0
            || width as i64 > MAX_IMAGE_SIZE
            || height as i64 > MAX_IMAGE_SIZE
        {
            return false;
        }

        let mut bmp = Bitmap::new(width, height);
        for (dst, px) in bmp.pixels.iter_mut().zip(img.pixels()) {
            let [r, g, b, a] = px.0;
            *dst = (a as u32) << 24 | (r as u32) << 16 | (g as u32) << 8 | b as u32;
        }
        self.bitmaps.insert(id, bmp);
        self.touch(id);
        true
    }

    /// The geometry `PRINT_IMG` and `<img>` need for `name`, or `None` when
    /// the name is not in the sprite dictionary at all — Emuera's
    /// `AppContents.GetSprite` (`AppContents.cs:41-49`), whose null result is
    /// what makes the caller print the alt text instead
    /// (`ConsoleImagePart.cs:69-73`).
    ///
    /// Deliberately *not* [`Self::live_sprite`]: `ConsoleImagePart` has the
    /// `IsCreated` filter commented out (`ConsoleImagePart.cs:67-68`, and
    /// again for `srcb` at `:124-125`), so a sprite whose parent was
    /// `GDISPOSE`d still counts as resolved and simply has nothing to draw.
    /// Filtering here would turn that into printed alt text, which is a
    /// visible difference.
    ///
    /// Returns a clone. It is a ~40-byte struct plus, for an animated sprite,
    /// its frame list; the pixels stay in the store and are never copied.
    ///
    /// DELIBERATE divergence from `Emuera/GameView/ConsoleImagePart.cs:66`,
    /// which keeps the `ASprite` *object*, so a later `SPRITEMOVE` shifts an
    /// already-printed image there and does not here. Sharing the sprite would
    /// make every `SPRITE*` mutation visible to the renderer thread and so
    /// force the whole sprite map behind a lock, for a behaviour no corpus
    /// script uses: `SPRITEPOSX`, `SPRITEPOSY`, `SPRITESETPOS` and
    /// `SPRITEMOVE` have zero call sites across eraTHYMKR and
    /// eramegaten_p_kr. Recorded in
    /// `docs/research/2026-09-03-emuera-command-gap.md` §5.
    pub fn sprite_geometry(&self, name: &str) -> Option<Sprite> {
        self.sprite(name).cloned()
    }

    /// `SPRITEGETCOLOR` — the pixel at a sprite-local `(x, y)`.
    ///
    /// `Creator.Method.cs:5826-5841` rejects a missing or uncreated sprite and
    /// an out-of-`DestBaseSize` point with `-1`; anything else reads
    /// `SpriteGetColor`, which offsets the point by the crop's origin and
    /// answers **transparent** — not `-1` — for a point that lands outside the
    /// parent bitmap (`CroppedImage.cs:78-89`).
    ///
    /// The bounds test runs before the read, and `SpriteAnime.IsCreated` is
    /// hard-coded `true` (`CroppedImage.cs:257-260`), so an animated sprite
    /// still answers `-1` outside its box and only an *inside* point reaches
    /// the `NotSupportedException` at `CroppedImage.cs:274`.
    pub fn sprite_get_color(&self, name: &str, x: i32, y: i32) -> SpriteColor {
        let Some(sprite) = self.sprite(name) else {
            return SpriteColor::Missing;
        };
        let single = match &sprite.sampler {
            // `ASpriteSingle.IsCreated` is its parent's
            // (`CroppedImage.cs:74-77`), so a `GDISPOSE`d parent is `-1`.
            ImageSampler::Single { bitmap, src } => match self.bitmaps.get(bitmap) {
                Some(parent) => Some((parent, *src)),
                None => return SpriteColor::Missing,
            },
            ImageSampler::Anime { .. } => None,
        };
        // Before the read, and against the always-positive `DestBaseSize`.
        if x < 0 || y < 0 || x as u32 >= sprite.width || y as u32 >= sprite.height {
            return SpriteColor::Missing;
        }
        let Some((parent, src)) = single else {
            return SpriteColor::Unsupported;
        };
        // The crop's extents may be negative (a mirrored sprite) and Emuera
        // still only adds the origin: `bmpX = x + SrcRectangle.X`.
        let px = x as i64 + src.x as i64;
        let py = y as i64 + src.y as i64;
        match parent.in_bounds(px, py) {
            true => SpriteColor::Color(parent.pixels[parent.idx(px as u32, py as u32)]),
            // `Color.Transparent`, which is `0x00FFFFFF` and not zero.
            false => SpriteColor::Color(0x00FF_FFFF),
        }
    }

    /// Hand every bitmap changed since the last call to `store`, and retract
    /// the ones that were disposed, then mint the [`Painted`] token that
    /// `SystemFunctions::redraw` demands. Called once per redraw, so text and
    /// pixels always come from the same instant and a frame cannot tear.
    ///
    /// Steady state does nothing but mint: an untouched bitmap is not in
    /// `dirty`, and the renderer keys its GPU texture on `(id, generation)` so
    /// it does not re-upload what it already has.
    pub fn publish(&mut self, store: &ImageStore) -> Painted<'_> {
        for id in self.dirty.drain() {
            match self.bitmaps.get(&id) {
                Some(b) => {
                    self.generation += 1;
                    store.publish(
                        id as BitmapId,
                        Arc::new(ImageBitmap::new(
                            b.width,
                            b.height,
                            b.pixels.clone().into_boxed_slice(),
                            self.generation,
                        )),
                    );
                }
                // `GDISPOSE`d between two redraws.
                None => store.remove(id as BitmapId),
            }
        }
        Painted(PhantomData)
    }
}

/// Proof that [`GraphicsStore::publish`] has already run for the frame about
/// to be painted.
///
/// `SystemFunctions::redraw` and its three input siblings take one by value,
/// and the only thing that can build one is `publish` — the field is private
/// to this module, so no front-end crate can forge it. `Painted` is neither
/// `Clone` nor `Copy`, so a caller cannot publish once and repaint forever:
/// every repaint consumes a token and so needs its own publish.
///
/// The lifetime keeps the store borrowed for as long as the token lives, which
/// is what rules out the remaining hole — publishing, mutating a bitmap, and
/// *then* painting. That is a borrow error, not a review comment. This is why
/// `context.rs` needs no "do not call `system.redraw` directly" rule: a direct
/// call does not compile.
pub struct Painted<'a>(PhantomData<&'a ()>);

/// Turns the arguments of an `<img>` tag or a `PRINT_IMG` into the inline part
/// the console stores — Emuera's `ConsoleImagePart` constructor
/// (`GameView/ConsoleImagePart.cs:17-131`), which is the same code for both
/// surfaces because both build a `ConsoleImagePart`.
///
/// Exists so `html.rs` needs no view of the graphics store beyond this, and so
/// there is exactly one place where a sprite name becomes pixels.
#[derive(Clone, Copy)]
pub struct ImageResolver<'a> {
    store: &'a GraphicsStore,
    /// `Config.FontSize`. Not the line height: an image is measured against
    /// the font and is allowed to overflow its line
    /// (`ConsoleImagePart.cs:19-20`).
    font_size: i32,
}

impl<'a> ImageResolver<'a> {
    pub fn new(store: &'a GraphicsStore, font_size: i32) -> Self {
        Self { store, font_size }
    }

    /// `Ok(part)` when `src` names a sprite, `Err(alt_text)` when it does not.
    ///
    /// The error *is* the output in that case: Emuera assigns
    /// `Str = AltText` and returns (`ConsoleImagePart.cs:69-73`), i.e. the
    /// reconstructed tag is printed as ordinary text. Callers print it rather
    /// than raising, because a missing resource is not a script error.
    pub fn resolve(
        &self,
        src: &str,
        button: Option<&str>,
        mask: Option<&str>,
        width: Option<MixedNum>,
        height: Option<MixedNum>,
        ypos: Option<MixedNum>,
    ) -> Result<InlineImage, String> {
        let alt = InlineImage::alt_text(src, button, mask, width, height, ypos, self.font_size);

        let Some(sprite) = self.store.sprite_geometry(src) else {
            return Err(alt);
        };

        let geometry =
            ImageGeometry::new(self.font_size, sprite.width, sprite.height, width, height, ypos);

        Ok(InlineImage {
            // `ResourceName = resName ?? ""` (`:22`) keeps the name as
            // written; the dictionary lookup is what upper-cases.
            name: src.into(),
            button: button.and_then(|b| self.store.sprite_geometry(b)),
            mask: mask
                // `MappingGraphName` is only looked up when non-empty
                // (`:127-130`).
                .filter(|m| !m.is_empty())
                .and_then(|m| self.store.sprite_geometry(m)),
            sprite,
            geometry,
            alt,
        })
    }

    /// `Config.FontSize` — the same value `<img>` geometry resolves against
    /// (`Utils.cs:19-22`), which a `<div>`'s box model also needs.
    pub fn font_size(&self) -> i32 {
        self.font_size
    }
}

/// Straight-alpha source-over, the GDI+ `CompositingMode.SourceOver` default
/// that every Emuera `DrawImage` call uses.
#[inline]
fn blend_over(src: u32, dst: u32) -> u32 {
    let sa = src >> 24;
    if sa == 0xFF {
        return src;
    }
    if sa == 0 {
        return dst;
    }

    let da = dst >> 24;
    let inv = 255 - sa;
    let ra = sa * 255 + da * inv;
    if ra == 0 {
        return 0;
    }

    let channel = |shift: u32| {
        let s = (src >> shift) & 0xFF;
        let d = (dst >> shift) & 0xFF;
        (s * sa * 255 + d * da * inv) / ra
    };

    (ra / 255) << 24 | channel(16) << 16 | channel(8) << 8 | channel(0)
}

/// Emuera's `GDrawGWithMask` inner loop, byte for byte: fully opaque copies,
/// fully transparent skips, and otherwise a `(mask + 1) / 256` blend applied to
/// all four channels including alpha.
#[inline]
fn blend_mask(src: u32, dst: u32, mask: u32) -> u32 {
    let m = mask & 0xFF;
    if m == 0xFF {
        return src;
    }
    if m == 0 {
        return dst;
    }

    let m = m + 1;
    let inv = 256 - m;
    let channel = |shift: u32| (((src >> shift) & 0xFF) * m + ((dst >> shift) & 0xFF) * inv) >> 8;

    channel(24) << 24 | channel(16) << 16 | channel(8) << 8 | channel(0)
}

/// GDI+ `ColorMatrix`, the optional trailing argument of `GDRAWG` and
/// `GDRAWSPRITE`.
///
/// Emuera reads a 5x5 block of an integer array and divides every entry by
/// 256, then hands it to GDI+, which multiplies the row vector
/// `[r, g, b, a, 1]` (components normalised to `0.0..=1.0`) by the matrix and
/// clamps the result.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ColorMatrix(pub [[f32; 5]; 5]);

impl ColorMatrix {
    /// Build from Emuera's raw integer entries, each scaled by `1 / 256`.
    pub fn from_scaled_ints(raw: [[i64; 5]; 5]) -> Self {
        let mut m = [[0.0f32; 5]; 5];
        for (row, raw) in m.iter_mut().zip(raw) {
            for (cell, raw) in row.iter_mut().zip(raw) {
                *cell = raw as f32 / 256.0;
            }
        }
        Self(m)
    }

    /// Transform one `0xAARRGGBB` pixel.
    pub fn apply(&self, argb: u32) -> u32 {
        let m = &self.0;
        let c = [
            ((argb >> 16) & 0xFF) as f32 / 255.0,
            ((argb >> 8) & 0xFF) as f32 / 255.0,
            (argb & 0xFF) as f32 / 255.0,
            (argb >> 24) as f32 / 255.0,
        ];

        let out = |j: usize| {
            let v = c[0] * m[0][j] + c[1] * m[1][j] + c[2] * m[2][j] + c[3] * m[3][j] + m[4][j];
            (v.clamp(0.0, 1.0) * 255.0 + 0.5) as u32
        };

        out(3) << 24 | out(0) << 16 | out(1) << 8 | out(2)
    }
}

/// Nearest-neighbour source-over blit of `src` (`src_w * src_h`) into `dest`.
///
/// Both rects are clipped: destination pixels outside `dest` are dropped, and so
/// are pixels whose sampled source coordinate falls outside the source bitmap.
fn blit(
    dest: &mut Bitmap,
    src: &[u32],
    src_w: u32,
    src_h: u32,
    dest_rect: Rect,
    src_rect: Rect,
    cm: Option<&ColorMatrix>,
) {
    let dest_rect = dest_rect.normalized();
    let src_rect = src_rect.normalized();
    if dest_rect.width <= 0 || dest_rect.height <= 0 || src_rect.width <= 0 || src_rect.height <= 0
    {
        return;
    }

    // Destination rows/columns that actually land on the bitmap.
    let j0 = (-dest_rect.y).max(0);
    let j1 = dest_rect.height.min(dest.height as i32 - dest_rect.y).max(j0);
    let i0 = (-dest_rect.x).max(0);
    let i1 = dest_rect.width.min(dest.width as i32 - dest_rect.x).max(i0);

    let one_to_one = dest_rect.width == src_rect.width && dest_rect.height == src_rect.height;

    for j in j0..j1 {
        let sy = if one_to_one {
            src_rect.y + j
        } else {
            src_rect.y + (j as i64 * src_rect.height as i64 / dest_rect.height as i64) as i32
        };
        if sy < 0 || sy >= src_h as i32 {
            continue;
        }

        let src_row = sy as usize * src_w as usize;
        let dest_row = (dest_rect.y + j) as usize * dest.width as usize;

        for i in i0..i1 {
            let sx = if one_to_one {
                src_rect.x + i
            } else {
                src_rect.x + (i as i64 * src_rect.width as i64 / dest_rect.width as i64) as i32
            };
            if sx < 0 || sx >= src_w as i32 {
                continue;
            }

            let dest_idx = dest_row + (dest_rect.x + i) as usize;
            let s = src[src_row + sx as usize];
            let s = match cm {
                Some(cm) => cm.apply(s),
                None => s,
            };
            dest.pixels[dest_idx] = blend_over(s, dest.pixels[dest_idx]);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const RED: u32 = 0xFFFF0000;
    const BLUE: u32 = 0xFF0000FF;
    const GREEN: u32 = 0xFF00FF00;

    fn store() -> GraphicsStore {
        GraphicsStore::default()
    }

    fn row(store: &GraphicsStore, id: u32, y: u32) -> Vec<u32> {
        let b = store.get(id).unwrap();
        b.pixels()[y as usize * b.width() as usize..][..b.width() as usize].to_vec()
    }

    #[test]
    fn create_is_zero_filled_and_not_repeatable() {
        let mut s = store();
        assert!(s.create(0, 3, 2));
        // Emuera: creating an existing id fails and changes nothing.
        assert!(!s.create(0, 9, 9));
        assert_eq!(s.width(0), 3);
        assert_eq!(s.height(0), 2);
        assert_eq!(s.get(0).unwrap().pixels(), &[0; 6]);
        assert_eq!(s.get_color(0, 0, 0), Some(0));
    }

    #[test]
    fn queries_on_missing_id_use_emuera_sentinels() {
        let s = store();
        assert!(!s.created(7));
        assert_eq!(s.width(7), 0);
        assert_eq!(s.height(7), 0);
        // GGETCOLOR is the one query that reports -1 rather than 0.
        assert_eq!(s.get_color(7, 0, 0), None);

        let mut s = store();
        assert!(!s.dispose(7));
        assert!(!s.clear(7, RED));
        assert!(!s.set_color(7, RED, 0, 0));
        assert!(!s.set_brush(7, RED));
        assert!(!s.set_pen(
            7,
            Pen {
                color: RED,
                width: 2
            }
        ));
        assert!(!s.set_font(
            7,
            Font {
                name: "x".into(),
                size: 12,
                style: 0
            }
        ));
    }

    #[test]
    fn get_set_color_reject_out_of_range_coordinates() {
        let mut s = store();
        s.create(0, 2, 2);
        assert!(s.set_color(0, RED, 1, 1));
        assert_eq!(s.get_color(0, 1, 1), Some(RED));

        for (x, y) in [(-1, 0), (0, -1), (2, 0), (0, 2), (i64::MAX, 0)] {
            assert!(!s.set_color(0, BLUE, x, y), "set {x},{y}");
            assert_eq!(s.get_color(0, x, y), None, "get {x},{y}");
        }
        // Nothing leaked into the bitmap.
        assert_eq!(s.get(0).unwrap().pixels(), &[0, 0, 0, RED]);
    }

    #[test]
    fn dispose_frees_the_id_and_orphans_its_sprites() {
        let mut s = store();
        s.create(1, 4, 4);
        s.sprite_create("S".into(), 1, None).unwrap();
        assert!(s.sprite_created("S"));

        assert!(s.dispose(1));
        assert!(!s.created(1));
        // Emuera's sprite keeps the parent id, so it reports "not created"...
        assert!(!s.sprite_created("S"));
        assert_eq!(s.sprite_width("S"), 0);
        assert!(!s.sprite_set_pos("S", 1, 1));
        assert!(!s.sprite_dispose("S"));
        // ...and comes back to life when the id is created again.
        assert!(s.create(1, 4, 4));
        assert!(s.sprite_created("S"));
    }

    #[test]
    fn clear_rect_clips_to_the_bitmap() {
        let mut s = store();
        s.create(0, 4, 3);
        assert!(s.clear_rect(0, RED, Rect::new(-2, -1, 4, 3)));
        assert_eq!(row(&s, 0, 0), vec![RED, RED, 0, 0]);
        assert_eq!(row(&s, 0, 1), vec![RED, RED, 0, 0]);
        assert_eq!(row(&s, 0, 2), vec![0, 0, 0, 0]);

        // Fully outside: no panic, no change.
        assert!(s.clear_rect(0, BLUE, Rect::new(100, 100, 5, 5)));
        assert_eq!(row(&s, 0, 0), vec![RED, RED, 0, 0]);

        // Negative extents fold back onto the origin: (3,2,-2,-2) covers
        // x in 1..3, y in 0..2.
        s.clear(0, 0);
        assert!(s.clear_rect(0, GREEN, Rect::new(3, 2, -2, -2)));
        assert_eq!(row(&s, 0, 0), vec![0, GREEN, GREEN, 0]);
        assert_eq!(row(&s, 0, 1), vec![0, GREEN, GREEN, 0]);
        assert_eq!(row(&s, 0, 2), vec![0, 0, 0, 0]);
    }

    #[test]
    fn a_rect_that_starts_past_the_far_edge_clips_to_nothing() {
        // The y range overlaps and the x range does not: clamping only the
        // range's end would leave `start > end` and panic on the row slice.
        let mut s = store();
        s.create(0, 4, 3);
        assert!(s.clear_rect(0, RED, Rect::new(100, 0, 5, 2)));
        assert!(s.fill_rect(0, Rect::new(100, 0, 5, 2), RED));
        assert_eq!(row(&s, 0, 0), vec![0, 0, 0, 0]);
    }

    #[test]
    fn fill_rectangle_takes_its_colour_from_the_brush_and_composites() {
        let mut s = store();
        s.create(0, 4, 1);

        // With no `GSETBRUSH`, the fallback the caller derives from
        // `Config.BackColor`.
        assert!(s.fill_rect(0, Rect::new(0, 0, 2, 1), BLUE));
        assert_eq!(row(&s, 0, 0), vec![BLUE, BLUE, 0, 0]);

        // A brush wins over the fallback, and a translucent one *blends*:
        // GDI+ `FillRectangle`, not the `Graphics.Clear` behind `GCLEAR`.
        assert!(s.set_brush(0, 0x80FF_0000));
        assert!(s.fill_rect(0, Rect::new(0, 0, 3, 1), GREEN));
        assert_eq!(
            row(&s, 0, 0),
            vec![0xFF80_007F, 0xFF80_007F, 0x80FF_0000, 0],
            "half-alpha red over blue, over blue, over nothing"
        );

        assert!(!s.fill_rect(9, Rect::new(0, 0, 1, 1), BLUE), "no such bitmap");
    }

    #[test]
    fn sprite_get_color_reads_through_the_crop() {
        let mut s = store();
        s.create(0, 4, 2);
        s.set_color(0, RED, 1, 0);
        s.set_color(0, GREEN, 2, 1);
        // A 2x2 window at (1,0), so sprite (0,0) is parent (1,0).
        s.sprite_create("A".into(), 0, Some(Rect::new(1, 0, 2, 2))).unwrap();

        assert_eq!(s.sprite_get_color("A", 0, 0), SpriteColor::Color(RED));
        assert_eq!(s.sprite_get_color("A", 1, 1), SpriteColor::Color(GREEN));
        assert_eq!(s.sprite_get_color("A", 0, 1), SpriteColor::Color(0));
        // Names are matched the way every other sprite method matches them.
        assert_eq!(s.sprite_get_color("a", 0, 0), SpriteColor::Color(RED));

        // Outside `DestBaseSize` is Emuera's -1 …
        for (x, y) in [(2, 0), (0, 2), (-1, 0), (0, -1)] {
            assert_eq!(s.sprite_get_color("A", x, y), SpriteColor::Missing, "({x},{y})");
        }
        assert_eq!(s.sprite_get_color("NOPE", 0, 0), SpriteColor::Missing);

        // … while inside the sprite but outside the *parent* is transparent,
        // which is a colour and not a failure. A crop reaching past the
        // parent's right edge is legal: `SPRITECREATE` only demands overlap.
        let mut s = store();
        s.create(0, 2, 1);
        s.clear(0, RED);
        s.sprite_create("B".into(), 0, Some(Rect::new(1, 0, 3, 1))).unwrap();
        assert_eq!(s.sprite_get_color("B", 0, 0), SpriteColor::Color(RED));
        assert_eq!(s.sprite_get_color("B", 1, 0), SpriteColor::Color(0x00FF_FFFF));

        // `IsCreated` is the parent's: disposing it takes the sprite's colour
        // with it, and reports -1 rather than transparent.
        assert!(s.dispose(0));
        assert_eq!(s.sprite_get_color("B", 0, 0), SpriteColor::Missing);
    }

    #[test]
    fn sprite_get_color_bounds_check_precedes_the_animation_refusal() {
        let mut s = store();
        s.create(0, 2, 2);
        assert!(s.sprite_anime_create("ANI".into(), 2, 2));
        assert!(s.sprite_anime_add_frame("ANI", 0, Rect::new(0, 0, 2, 2), 0, 0, 50));

        // `SpriteAnime.IsCreated` is hard-coded true, so the -1 exits still
        // fire; only a point inside the box reaches the unsupported read.
        assert_eq!(s.sprite_get_color("ANI", 2, 0), SpriteColor::Missing);
        assert_eq!(s.sprite_get_color("ANI", -1, 1), SpriteColor::Missing);
        assert_eq!(s.sprite_get_color("ANI", 1, 1), SpriteColor::Unsupported);
    }

    #[test]
    fn draw_g_one_to_one_copies_and_clips() {
        let mut s = store();
        s.create(0, 4, 1);
        s.create(1, 2, 1);
        s.clear(1, RED);

        // Straddling the right edge: only the in-bounds column lands.
        assert!(s.draw_g(0, 1, Rect::new(3, 0, 2, 1), Rect::new(0, 0, 2, 1), None));
        assert_eq!(row(&s, 0, 0), vec![0, 0, 0, RED]);

        // Straddling the left edge.
        s.clear(0, 0);
        assert!(s.draw_g(0, 1, Rect::new(-1, 0, 2, 1), Rect::new(0, 0, 2, 1), None));
        assert_eq!(row(&s, 0, 0), vec![RED, 0, 0, 0]);

        // A source rect reaching past the source bitmap drops the missing
        // pixels instead of reading garbage.
        s.clear(0, 0);
        assert!(s.draw_g(0, 1, Rect::new(0, 0, 4, 1), Rect::new(0, 0, 4, 1), None));
        assert_eq!(row(&s, 0, 0), vec![RED, RED, 0, 0]);
    }

    #[test]
    fn draw_g_scales_nearest_neighbour() {
        let mut s = store();
        s.create(0, 4, 1);
        s.create(1, 2, 1);
        s.set_color(1, RED, 0, 0);
        s.set_color(1, BLUE, 1, 0);

        // 2 -> 4: each source pixel doubles.
        assert!(s.draw_g(0, 1, Rect::new(0, 0, 4, 1), Rect::new(0, 0, 2, 1), None));
        assert_eq!(row(&s, 0, 0), vec![RED, RED, BLUE, BLUE]);

        // 2 -> 1: takes the first source pixel.
        s.create(2, 1, 1);
        assert!(s.draw_g(2, 1, Rect::new(0, 0, 1, 1), Rect::new(0, 0, 2, 1), None));
        assert_eq!(row(&s, 2, 0), vec![RED]);
    }

    #[test]
    fn draw_g_reports_missing_ids() {
        let mut s = store();
        s.create(0, 2, 2);
        assert!(!s.draw_g(0, 5, Rect::new(0, 0, 2, 2), Rect::new(0, 0, 2, 2), None));
        assert!(!s.draw_g(5, 0, Rect::new(0, 0, 2, 2), Rect::new(0, 0, 2, 2), None));
        // Same id for source and destination is legal in Emuera.
        s.set_color(0, RED, 0, 0);
        assert!(s.draw_g(0, 0, Rect::new(1, 1, 1, 1), Rect::new(0, 0, 1, 1), None));
        assert_eq!(s.get_color(0, 1, 1), Some(RED));
    }

    #[test]
    fn draw_g_composites_source_over() {
        let mut s = store();
        s.create(0, 1, 1);
        s.create(1, 1, 1);
        s.clear(0, 0xFF000000); // opaque black
        s.clear(1, 0x80FFFFFF); // half-transparent white

        assert!(s.draw_g(0, 1, Rect::new(0, 0, 1, 1), Rect::new(0, 0, 1, 1), None));
        let c = s.get_color(0, 0, 0).unwrap();
        assert_eq!(c >> 24, 0xFF, "opaque destination stays opaque");
        // 0x80 = 128/255 of white over black.
        assert_eq!(c & 0xFF, 128);

        // A fully transparent source leaves the destination alone.
        s.clear(1, 0x00FF0000);
        assert!(s.draw_g(0, 1, Rect::new(0, 0, 1, 1), Rect::new(0, 0, 1, 1), None));
        assert_eq!(s.get_color(0, 0, 0).unwrap(), c);
    }

    #[test]
    fn color_matrix_recolours_the_source() {
        // Identity: every entry Emuera reads is divided by 256, so 256 is 1.0.
        let mut identity = [[0i64; 5]; 5];
        for i in 0..4 {
            identity[i][i] = 256;
        }
        let cm = ColorMatrix::from_scaled_ints(identity);
        assert_eq!(cm.apply(0x8010FF40), 0x8010FF40);

        // Halve the alpha, swap red and blue, and add a green bias of 0.25.
        let mut raw = [[0i64; 5]; 5];
        raw[0][2] = 256; // r -> b
        raw[2][0] = 256; // b -> r
        raw[3][3] = 128; // a * 0.5
        raw[4][1] = 64; // g += 0.25
        let cm = ColorMatrix::from_scaled_ints(raw);
        assert_eq!(cm.apply(0xFF11FF22), 0x80224011);

        // Out-of-gamut results clamp instead of wrapping.
        let mut raw = [[0i64; 5]; 5];
        raw[4] = [1024, -1024, 1024, 1024, 0];
        let cm = ColorMatrix::from_scaled_ints(raw);
        assert_eq!(cm.apply(0), 0xFFFF00FF);
    }

    #[test]
    fn draw_g_applies_the_color_matrix_before_compositing() {
        let mut s = store();
        s.create(0, 1, 1);
        s.create(1, 1, 1);
        s.clear(1, 0xFFFF0000);

        // Zero out the alpha: the source must then leave the destination alone.
        let mut raw = [[0i64; 5]; 5];
        raw[0][0] = 256;
        raw[1][1] = 256;
        raw[2][2] = 256;
        let cm = ColorMatrix::from_scaled_ints(raw);
        assert!(s.draw_g(
            0,
            1,
            Rect::new(0, 0, 1, 1),
            Rect::new(0, 0, 1, 1),
            Some(&cm)
        ));
        assert_eq!(s.get_color(0, 0, 0), Some(0));
    }

    #[test]
    fn draw_g_with_mask_uses_the_blue_channel() {
        let mut s = store();
        s.create(0, 2, 1); // dest
        s.create(1, 2, 1); // src
        s.create(2, 2, 1); // mask
        s.clear(0, 0xFF000000);
        s.clear(1, 0xFFFFFFFF);
        // Blue 0xFF -> copy, blue 0x00 -> skip. Red/green are ignored.
        s.set_color(2, 0x00FF00FF, 0, 0);
        s.set_color(2, 0xFFFFFF00, 1, 0);

        assert!(s.draw_g_with_mask(0, 1, 2, 0, 0));
        assert_eq!(row(&s, 0, 0), vec![0xFFFFFFFF, 0xFF000000]);
    }

    #[test]
    fn draw_g_with_mask_blends_partial_opacity() {
        let mut s = store();
        s.create(0, 1, 1);
        s.create(1, 1, 1);
        s.create(2, 1, 1);
        s.clear(0, 0x00000000);
        s.clear(1, 0xFFFFFFFF);
        s.clear(2, 0x00000080);

        assert!(s.draw_g_with_mask(0, 1, 2, 0, 0));
        // Emuera: (src * (m + 1) + dst * (256 - m - 1)) >> 8, per channel.
        let expect = (0xFFu32 * 129) >> 8;
        assert_eq!(expect, 128);
        assert_eq!(s.get_color(0, 0, 0), Some(0x80808080));
    }

    #[test]
    fn draw_g_with_mask_enforces_emuera_preconditions() {
        let mut s = store();
        s.create(0, 4, 4);
        s.create(1, 2, 2);
        s.create(2, 2, 2);
        s.create(3, 3, 3);

        assert!(!s.draw_g_with_mask(9, 1, 2, 0, 0), "missing dest");
        assert!(!s.draw_g_with_mask(0, 9, 2, 0, 0), "missing src");
        assert!(!s.draw_g_with_mask(0, 1, 9, 0, 0), "missing mask");
        assert!(!s.draw_g_with_mask(0, 1, 3, 0, 0), "mask size mismatch");
        assert!(
            !s.draw_g_with_mask(0, 1, 2, 3, 0),
            "runs off the right edge"
        );
        assert!(
            !s.draw_g_with_mask(0, 1, 2, 0, 3),
            "runs off the bottom edge"
        );
        assert!(!s.draw_g_with_mask(0, 1, 2, -1, 0), "negative offset");
        assert!(s.draw_g_with_mask(0, 1, 2, 2, 2), "exactly fits");
    }

    #[test]
    fn sprite_create_defaults_to_the_whole_parent() {
        let mut s = store();
        s.create(3, 6, 5);
        assert_eq!(s.sprite_create("Chara".into(), 3, None), Ok(true));
        // Names are case-insensitive, upper-cased like Emuera.
        assert!(s.sprite_created("CHARA"));
        assert_eq!(s.sprite_width("CHARA"), 6);
        assert_eq!(s.sprite_height("CHARA"), 5);
        assert_eq!(s.sprite_pos_x("CHARA"), 0);
        assert_eq!(s.sprite_pos_y("CHARA"), 0);

        // Re-creating a live sprite fails.
        assert_eq!(s.sprite_create("CHARA".into(), 3, None), Ok(false));
        // Empty name and missing parent fail softly.
        assert_eq!(s.sprite_create(String::new(), 3, None), Ok(false));
        assert_eq!(s.sprite_create("OTHER".into(), 99, None), Ok(false));
    }

    #[test]
    fn sprite_create_rect_must_touch_the_parent() {
        let mut s = store();
        s.create(0, 4, 4);
        assert_eq!(
            s.sprite_create("A".into(), 0, Some(Rect::new(2, 2, 8, 8))),
            Ok(true)
        );
        assert_eq!(s.sprite_width("A"), 8);
        // Negative extents are allowed; DestBaseSize is their absolute value.
        assert_eq!(
            s.sprite_create("B".into(), 0, Some(Rect::new(3, 3, -2, -2))),
            Ok(true)
        );
        assert_eq!((s.sprite_width("B"), s.sprite_height("B")), (2, 2));
        // Entirely outside is a script error in Emuera.
        assert_eq!(
            s.sprite_create("C".into(), 0, Some(Rect::new(10, 10, 2, 2))),
            Err(())
        );
        assert!(!s.sprite_created("C"));
    }

    #[test]
    fn sprite_queries_on_missing_name_return_zero() {
        let mut s = store();
        assert!(!s.sprite_created("NOPE"));
        assert_eq!(s.sprite_width("NOPE"), 0);
        assert_eq!(s.sprite_height("NOPE"), 0);
        assert_eq!(s.sprite_pos_x("NOPE"), 0);
        assert_eq!(s.sprite_pos_y("NOPE"), 0);
        assert!(!s.sprite_set_pos("NOPE", 1, 2));
        assert!(!s.sprite_move("NOPE", 1, 2));
        assert!(!s.sprite_dispose("NOPE"));
    }

    #[test]
    fn sprite_set_pos_and_move() {
        let mut s = store();
        s.create(0, 4, 4);
        s.sprite_create("A".into(), 0, None).unwrap();
        assert!(s.sprite_set_pos("A", 3, -4));
        assert_eq!((s.sprite_pos_x("A"), s.sprite_pos_y("A")), (3, -4));
        assert!(s.sprite_move("A", -1, 2));
        assert_eq!((s.sprite_pos_x("A"), s.sprite_pos_y("A")), (2, -2));
        assert!(s.sprite_dispose("A"));
        assert!(!s.sprite_created("A"));
    }

    #[test]
    fn draw_sprite_uses_the_crop_and_position() {
        let mut s = store();
        s.create(1, 4, 1);
        for (x, c) in [(0, RED), (1, BLUE), (2, GREEN), (3, 0xFF123456)] {
            s.set_color(1, c, x, 0);
        }
        // Crop the middle two pixels.
        s.sprite_create("MID".into(), 1, Some(Rect::new(1, 0, 2, 1))).unwrap();

        s.create(0, 4, 1);
        assert!(s.draw_sprite(0, "MID", Rect::new(0, 0, 2, 1), None));
        assert_eq!(row(&s, 0, 0), vec![BLUE, GREEN, 0, 0]);

        // SPRITESETPOS shifts the blit by pos * destExtent / srcExtent.
        s.clear(0, 0);
        assert!(s.sprite_set_pos("MID", 2, 0));
        assert!(s.draw_sprite(0, "MID", Rect::new(0, 0, 2, 1), None));
        assert_eq!(row(&s, 0, 0), vec![0, 0, BLUE, GREEN]);

        assert!(!s.draw_sprite(0, "NOPE", Rect::new(0, 0, 1, 1), None));
        assert!(!s.draw_sprite(9, "MID", Rect::new(0, 0, 1, 1), None));
    }

    #[test]
    fn save_and_load_round_trip_png() {
        let dir = std::env::temp_dir().join(format!(
            "erars-graphics-test-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        let path = dir.join("img0003.png");

        let mut s = store();
        assert!(!s.save_image(0, &path), "nothing to save yet");
        assert!(!s.load_image(0, &path), "nothing to load yet");

        s.create(0, 2, 2);
        s.set_color(0, 0x11223344, 0, 0);
        s.set_color(0, 0xFF00FF00, 1, 0);
        s.set_color(0, 0x00000000, 0, 1);
        s.set_color(0, 0xFFFFFFFF, 1, 1);
        assert!(s.save_image(0, &path), "GSAVE creates the directory");

        // GLOAD refuses to overwrite a live id.
        assert!(!s.load_image(0, &path));
        assert!(s.load_image(5, &path));
        assert_eq!(s.width(5), 2);
        assert_eq!(s.height(5), 2);
        assert_eq!(s.get(5).unwrap().pixels(), s.get(0).unwrap().pixels());

        let _ = std::fs::remove_dir_all(&dir);
    }

    /// Emuera hands the file to GDI+, which sniffs the content rather than the
    /// extension, so a BMP living under the `imgNNNN.png` name still loads.
    #[test]
    fn load_sniffs_the_content_not_the_extension() {
        let dir = std::env::temp_dir().join(format!(
            "erars-graphics-bmp-{}-{:?}",
            std::process::id(),
            std::thread::current().id()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("img0007.png");

        let mut src = image::RgbaImage::new(2, 1);
        src.put_pixel(0, 0, image::Rgba([0x12, 0x34, 0x56, 0xFF]));
        src.put_pixel(1, 0, image::Rgba([0xAB, 0xCD, 0xEF, 0xFF]));
        src.save_with_format(&path, image::ImageFormat::Bmp).unwrap();

        let mut s = store();
        assert!(s.load_image(2, &path));
        assert_eq!(row(&s, 2, 0), vec![0xFF123456, 0xFFABCDEF]);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn pen_brush_font_survive_until_dispose() {
        let mut s = store();
        s.create(0, 1, 1);
        assert!(s.set_brush(0, RED));
        assert!(s.set_pen(
            0,
            Pen {
                color: BLUE,
                width: 3
            }
        ));
        assert!(s.set_font(
            0,
            Font {
                name: "MS Gothic".into(),
                size: 18,
                style: 3
            }
        ));

        let b = s.get(0).unwrap();
        assert_eq!(b.brush(), Some(RED));
        assert_eq!(
            b.pen(),
            Some(Pen {
                color: BLUE,
                width: 3
            })
        );
        assert_eq!(
            b.font().map(|f| (f.name.as_str(), f.size, f.style)),
            Some(("MS Gothic", 18, 3))
        );

        s.dispose(0);
        s.create(0, 1, 1);
        let b = s.get(0).unwrap();
        assert_eq!(b.brush(), None);
        assert_eq!(b.pen(), None);
        assert_eq!(b.font(), None);
    }

    /// `Config.FontName` empty, `Config.ForeColor` at Emuera's default, and no
    /// game directory — the font chain then ends at the bundled face, so these
    /// assertions hold on a machine with no Japanese fonts installed.
    fn text_env() -> TextEnv<'static> {
        TextEnv {
            family: "",
            console_style: 0,
            fore_color: 0xFFC0_C0C0,
            game_dir: Path::new(""),
            lang: Language::Japanese,
        }
    }

    /// Every pixel that got any ink, as `(x, y, colour)`.
    fn inked(store: &GraphicsStore, id: u32) -> Vec<(u32, u32, u32)> {
        let b = store.get(id).unwrap();
        b.pixels()
            .iter()
            .enumerate()
            .filter(|(_, &p)| p >> 24 != 0)
            .map(|(i, &p)| {
                let i = i as u32;
                (i % b.width(), i / b.width(), p)
            })
            .collect()
    }

    #[test]
    fn draw_text_paints_the_brush_colour_where_the_glyph_covers() {
        let mut s = store();
        assert!(s.create(7, 120, 80));
        assert!(s.set_brush(7, GREEN));
        // Pen in the same colour: `GDrawString` always strokes as well as
        // fills (`GraphicsImage.cs:137-140`), so a different pen colour would
        // put two colours in the bitmap.
        assert!(s.set_pen(7, Pen { color: GREEN, width: 1 }));
        assert!(s.set_font(
            7,
            Font {
                name: String::new(),
                size: 24,
                style: 0,
            }
        ));

        let extent = s
            .draw_text(7, "H", 10, 10, &text_env())
            .expect("bitmap 7 exists");
        assert!(extent.0 > 0.0 && extent.1 > 0.0, "measured {extent:?}");

        let ink = inked(&s, 7);
        assert!(!ink.is_empty(), "GDRAWTEXT drew nothing");
        // Source-over onto transparent black keeps the paint's RGB exactly and
        // puts the anti-aliasing in the alpha channel.
        for &(x, y, p) in &ink {
            assert_eq!(
                p & 0x00FF_FFFF,
                GREEN & 0x00FF_FFFF,
                "pixel ({x}, {y}) = {p:#010X} is not the brush colour"
            );
        }
        // Drawn at (10, 10) with the box's top-left there: nothing above or
        // left of the origin, and at least one pixel fully covered.
        assert!(
            ink.iter().all(|&(x, y, _)| x >= 10 && y >= 10),
            "ink escaped the layout origin"
        );
        assert!(
            ink.iter().any(|&(_, _, p)| p >> 24 == 0xFF),
            "an `H` at 24 px must cover some pixel completely"
        );
        // The bitmap is dirty for the renderer.
        assert!(s.dirty.contains(&7));
    }

    #[test]
    fn draw_text_falls_back_to_fore_color_and_the_console_style() {
        let mut s = store();
        assert!(s.create(1, 120, 80));
        // No `GSETBRUSH`, no `GSETPEN`, no `GSETFONT`: GDI+ uses
        // `Config.ForeColor` for both passes and a 100 px font
        // (`GraphicsImage.cs:126-140`).
        let env = TextEnv {
            fore_color: 0xFF12_3456,
            ..text_env()
        };
        let extent = s.draw_text(1, "-", 0, 0, &env).expect("bitmap 1 exists");
        assert!(extent.0 > 0.0 && extent.1 > 0.0, "measured {extent:?}");
        let ink = inked(&s, 1);
        assert!(!ink.is_empty(), "the default font drew nothing");
        for &(x, y, p) in &ink {
            assert_eq!(
                p & 0x00FF_FFFF,
                0x0012_3456,
                "pixel ({x}, {y}) = {p:#010X} is not Config.ForeColor"
            );
        }
    }

    #[test]
    fn draw_text_strokes_the_path_with_the_pen() {
        let mut s = store();
        assert!(s.create(2, 160, 160));
        // A fully transparent brush paints nothing (alpha 0 source-over is a
        // no-op), which isolates the `DrawPath` pass from the `FillPath` one
        // that always precedes it (`GraphicsImage.cs:131-141`). Overlaying
        // them instead would say nothing: a wide pen covers a thin stem
        // completely, so "the brush colour survives" depends on the face.
        assert!(s.set_brush(2, 0x0000_0000));
        assert!(s.set_pen(2, Pen { color: RED, width: 3 }));
        assert!(s.set_font(
            2,
            Font {
                name: String::new(),
                size: 40,
                style: 0,
            }
        ));
        s.draw_text(2, "H", 20, 10, &text_env()).expect("bitmap 2 exists");

        let ink = inked(&s, 2);
        assert!(!ink.is_empty(), "the stroke pass painted nothing");
        for &(x, y, p) in &ink {
            assert_eq!(
                p & 0x00FF_FFFF,
                RED & 0x00FF_FFFF,
                "pixel ({x}, {y}) = {p:#010X} is not the pen colour"
            );
        }
        assert!(
            ink.iter().any(|&(_, _, p)| p >> 24 == 0xFF),
            "a 3 px pen must cover some pixel completely"
        );
    }

    #[test]
    fn draw_text_on_a_missing_bitmap_reports_nothing() {
        let mut s = store();
        // `if (!g.IsCreated) return 0` before the measurement
        // (`Creator.Method.cs:5537-5538`), so the caller must not touch
        // `RESULT:1`/`:2`.
        assert!(s.draw_text(9, "x", 0, 0, &text_env()).is_none());
        assert!(s.dirty.is_empty());
    }

    #[test]
    fn draw_text_advance_is_linear_in_the_glyph_count() {
        let mut s = store();
        assert!(s.create(3, 8, 8));
        assert!(s.set_font(
            3,
            Font {
                name: String::new(),
                size: 30,
                style: 0,
            }
        ));
        // The advance sum is linear in the glyph count with kerning and
        // ligatures off, whichever face the chain picked.
        let one = s.draw_text(3, "H", 0, 0, &text_env()).unwrap();
        let two = s.draw_text(3, "HH", 0, 0, &text_env()).unwrap();
        assert_eq!(two.0, one.0 * 2.0, "two glyphs advance twice as far");
        assert_eq!(two.1, one.1, "one line either way");
        // A 30 px glyph does not fit an 8×8 bitmap: it is clipped, never
        // wrapped, and never written out of bounds.
        assert_eq!(s.get(3).unwrap().pixels().len(), 64);

        // Drawn far outside: every pixel is clipped away, no panic.
        let mut s2 = store();
        assert!(s2.create(4, 4, 4));
        assert!(s2.set_brush(4, GREEN));
        assert!(s2.draw_text(4, "H", -500, -500, &text_env()).is_some());
        assert!(s2.draw_text(4, "H", 500, 500, &text_env()).is_some());
        assert!(s2.get(4).unwrap().pixels().iter().all(|&p| p == 0));
    }

    /// Clipping is exactly windowing: a bitmap too small for the text holds
    /// the same pixels a big one holds in that corner.
    #[test]
    fn draw_text_clips_without_shifting_the_glyphs() {
        let draw = |width: u32, height: u32| {
            let mut s = store();
            assert!(s.create(6, width, height));
            assert!(s.set_brush(6, GREEN));
            assert!(s.set_pen(6, Pen { color: GREEN, width: 1 }));
            assert!(s.set_font(
                6,
                Font {
                    name: String::new(),
                    size: 24,
                    style: 0,
                }
            ));
            s.draw_text(6, "HHH", 10, 10, &text_env()).unwrap();
            s
        };

        let full = draw(200, 200);
        let ink = inked(&full, 6);
        assert!(!ink.is_empty(), "GDRAWTEXT drew nothing");
        let (min_x, max_x) = (
            ink.iter().map(|&(x, ..)| x).min().unwrap(),
            ink.iter().map(|&(x, ..)| x).max().unwrap(),
        );
        let (min_y, max_y) = (
            ink.iter().map(|&(_, y, _)| y).min().unwrap(),
            ink.iter().map(|&(_, y, _)| y).max().unwrap(),
        );
        // Halfway through the ink box: past the first inked pixel (so the
        // window really holds ink) and short of the last (so it really clips).
        let (w, h) = ((min_x + max_x) / 2 + 1, (min_y + max_y) / 2 + 1);
        let cut = draw(w, h);
        assert!(
            inked(&cut, 6).iter().any(|&(_, _, p)| p >> 24 != 0),
            "the {w}x{h} window should still hold ink"
        );
        for y in 0..h {
            for x in 0..w {
                assert_eq!(
                    cut.get_color(6, i64::from(x), i64::from(y)),
                    full.get_color(6, i64::from(x), i64::from(y)),
                    "({x}, {y}) differs between a {w}x{h} and a 200x200 bitmap"
                );
            }
        }
    }

    #[test]
    fn draw_text_of_an_empty_string_measures_zero_and_draws_nothing() {
        let mut s = store();
        assert!(s.create(5, 16, 16));
        assert!(s.set_brush(5, GREEN));
        // `MeasureString("")` is `(0, 0)` and an empty `GraphicsPath` fills
        // nothing, but the method still returns 1 (`Creator.Method.cs:5565`).
        assert_eq!(s.draw_text(5, "", 0, 0, &text_env()), Some((0.0, 0.0)));
        assert!(s.get(5).unwrap().pixels().iter().all(|&p| p == 0));
    }
}
