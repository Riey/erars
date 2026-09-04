//! The inline image layer: the pixel content a `<img>` tag or `PRINT_IMG`
//! puts on a console line, plus the decoded-bitmap store shared between the
//! VM and every front-end.
//!
//! Emuera has no separate image layer. `ConsoleImagePart`
//! (`GameView/ConsoleImagePart.cs`) is an ordinary inline display part that
//! happens to draw a bitmap instead of glyphs, it resolves its sprite out of
//! the global `AppContents` dictionary, and it measures itself in *pixels*
//! against `Config.FontSize`. This module is the same thing:
//!
//! * [`ImageGeometry`] is `ConsoleImagePart`'s constructor arithmetic
//!   (`ConsoleImagePart.cs:74-116`) — the one implementation of the rule, so
//!   the VM, the GPU renderer and the text front-ends cannot disagree;
//! * [`InlineImage`] is the part itself, carrying the resolved geometry and
//!   enough of the sprite to sample it;
//! * [`ImageStore`] holds the decoded pixels. Emuera reads them straight out
//!   of the live `Bitmap` because it is single-threaded; erars' VM and
//!   renderer are separate threads (`erars-renderer/src/main.rs`), so the VM
//!   publishes a snapshot at the redraw boundary and the renderer reads that.
//!   A frame therefore can never tear between its text and its images.
//!
//! Sprite *pixels* stay live: [`InlineImage`] names a [`BitmapId`], so a
//! `GDRAWG` into the parent bitmap after the image was printed shows up on the
//! next redraw exactly as it does in Emuera (`CroppedImage.cs:139-146`, where
//! `SpriteF`/`SpriteG` hold their parent by reference).

use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::collections::HashMap;
use std::fmt::Write as _;
use std::sync::Arc;

use parking_lot::RwLock;

/// Identifies one bitmap in an [`ImageStore`] — the `GCREATE` id for a script
/// bitmap, or a synthetic id handed out to a `resources/` CSV image.
pub type BitmapId = u32;

/// An inclusive-origin, exclusive-extent rectangle in bitmap pixels.
///
/// Emuera passes `System.Drawing.Rectangle`s straight through and explicitly
/// allows negative extents for sprite source rects, so [`Rect::normalized`]
/// folds a negative extent back onto its origin.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
}

impl Rect {
    pub const fn new(x: i32, y: i32, width: i32, height: i32) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }

    /// Same area with non-negative extents.
    pub fn normalized(self) -> Self {
        let (x, width) = if self.width < 0 {
            (self.x.saturating_add(self.width), -self.width)
        } else {
            (self.x, self.width)
        };
        let (y, height) = if self.height < 0 {
            (self.y.saturating_add(self.height), -self.height)
        } else {
            (self.y, self.height)
        };

        Self {
            x,
            y,
            width,
            height,
        }
    }

    /// Whether the normalized rect shares at least one pixel with `w * h`
    /// anchored at the origin. Emuera's `SPRITECREATE` range check.
    pub fn intersects_size(self, w: u32, h: u32) -> bool {
        let r = self.normalized();
        r.width > 0
            && r.height > 0
            && r.x < w as i32
            && r.y < h as i32
            && r.x.saturating_add(r.width) > 0
            && r.y.saturating_add(r.height) > 0
    }
}

/// A length written either as a percentage of the font size or as literal
/// pixels. Emuera's `MixedNum` (`_Library/EvilMask/Utils.cs:17-40`), produced
/// by `ParseMixedNum` for HTML attributes (`:126-139`) and by the `px` keyword
/// in `PRINT_IMG`'s argument list (`GameProc/Function/ArgumentBuilder.cs:265`).
///
/// The wiki documents only the percentage form; `px` is an EM private-build
/// extension that the source of record implements, so both are supported.
/// `Default` is C#'s `default(MixedNum)` — `0`, no `px` — which is also the
/// "attribute absent" value the tag reconstruction drops.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct MixedNum {
    pub num: i32,
    pub is_px: bool,
}

impl MixedNum {
    pub const fn percent(num: i32) -> Self {
        Self { num, is_px: false }
    }

    pub const fn px(num: i32) -> Self {
        Self { num, is_px: true }
    }

    /// `MixedNum.ToPixel(num, def)` (`Utils.cs:19-22`): `None` yields `def`,
    /// a `px` value is literal, otherwise it is a percentage of `font_size`.
    pub fn to_pixel(this: Option<Self>, font_size: i32, def: i32) -> i32 {
        match this {
            Some(n) if n.is_px => n.num,
            Some(n) => n.num.saturating_mul(font_size) / 100,
            None => def,
        }
    }

    /// `MixedNum.BuilderString` (`Utils.cs:44-48`): a `px` value keeps its
    /// suffix, a percentage is written **resolved against the font size** and
    /// without a unit. The reconstructed tag therefore does not round-trip —
    /// re-parsing `ypos='-7'` reads a percentage again — but that asymmetry is
    /// the source's, and this text is what Emuera shows for a part it cannot
    /// draw.
    pub(crate) fn write_value(self, font_size: i32, out: &mut String) {
        if self.is_px {
            let _ = write!(out, "{}px", self.num);
        } else {
            let _ = write!(out, "{}", self.num.saturating_mul(font_size) / 100);
        }
    }

    /// `Utils.AddTagMixedNumArg` (`Utils.cs:140-147`): a zero or absent value
    /// contributes nothing, which is why the reconstructed tag of an
    /// `<img src='X' width='0'>` has no `width`. The test is on the written
    /// number, so a percentage that *resolves* to zero is still written.
    pub(crate) fn write_tag_arg(this: Option<Self>, name: &str, font_size: i32, out: &mut String) {
        match this {
            Some(n) if n.num != 0 => {
                let _ = write!(out, " {name}='");
                n.write_value(font_size, out);
                out.push('\'');
            }
            _ => {}
        }
    }
}

/// One frame of a `SPRITEANIMECREATE` sprite (Emuera `AnimeFrame`,
/// `Content/CroppedImage.cs:150-180`), already normalized by
/// `SPRITEANIMEADDFRAME`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImageFrame {
    /// Parent bitmap this frame samples.
    pub bitmap: BitmapId,
    pub src: Rect,
    pub offset_x: i32,
    pub offset_y: i32,
    pub delay_ms: u32,
    /// The frame's destination box missed the sprite entirely, so it draws
    /// nothing but still spends its delay (`Rectangle.Intersect` of disjoint
    /// rects is empty, `CroppedImage.cs:166-178`).
    pub empty: bool,
}

/// Where an [`InlineImage`] takes its pixels from — Emuera's `ASpriteSingle`
/// versus `SpriteAnime` (`Content/CroppedImage.cs:52-63`, `:182-262`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ImageSampler {
    /// One window onto one bitmap.
    Single { bitmap: BitmapId, src: Rect },
    /// A timed frame list. `total_ms` is the sum of the frame delays; zero
    /// means the animation has no frames and draws nothing.
    Anime { frames: Vec<ImageFrame>, total_ms: u64 },
}

/// The part of a sprite an [`InlineImage`] needs: how to sample it and the
/// `DestBaseSize` / `DestBasePosition` that Emuera's `ASprite` exposes
/// (`Content/CroppedImage.cs:21-49`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InlineSprite {
    pub sampler: ImageSampler,
    /// `DestBaseSize.Width`, always positive.
    pub width: u32,
    /// `DestBaseSize.Height`, always positive.
    pub height: u32,
    /// `DestBasePosition.X`, mutated by `SPRITEPOS`/`SPRITEMOVE`.
    pub pos_x: i32,
    /// `DestBasePosition.Y`.
    pub pos_y: i32,
}

impl InlineSprite {
    /// The frame to draw `elapsed_ms` into the animation, or the single
    /// window for a still sprite. `None` when there is nothing to draw.
    ///
    /// `SpriteAnime.GraphicsDraw` (`CroppedImage.cs:219-254`) walks the frame
    /// list subtracting delays from `elapsed % total`, so the frame boundary
    /// is exclusive on the left and the list order is the play order.
    pub fn frame_at(&self, elapsed_ms: u64) -> Option<(BitmapId, Rect, i32, i32)> {
        match &self.sampler {
            ImageSampler::Single { bitmap, src } => Some((*bitmap, *src, 0, 0)),
            ImageSampler::Anime { frames, total_ms } => {
                if *total_ms == 0 {
                    return None;
                }
                let mut t = elapsed_ms % *total_ms;
                for f in frames {
                    if t < f.delay_ms as u64 {
                        return (!f.empty).then_some((f.bitmap, f.src, f.offset_x, f.offset_y));
                    }
                    t -= f.delay_ms as u64;
                }
                // `total_ms` is the exact sum of the delays, so the loop
                // always returns; keep the arm honest rather than panicking.
                None
            }
        }
    }
}

/// `ConsoleImagePart`'s resolved pixel geometry (`ConsoleImagePart.cs:74-116`).
///
/// All values are pixels relative to the part's own origin: `x` advances from
/// the part's `PointX`, `y` from the top of the console line.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct ImageGeometry {
    /// Layout advance, i.e. Emuera `Width` after the negative-extent
    /// normalisation. Always non-negative.
    pub width: u32,
    /// Emuera `XsubPixel`: the fraction dropped by the integer division that
    /// produced `width`, handed to the part that follows.
    pub x_sub_pixel: f32,
    /// Emuera `top`: the first line-relative row the image covers. Negative
    /// for a negative `ypos`.
    pub top: i32,
    /// Emuera `bottom` = `top + |height|`.
    pub bottom: i32,
    /// `destRect.X`. `0` normally, `|width|` when the width was negative, so
    /// that a negative `dest_width` mirrors in place.
    pub dest_x: i32,
    /// `destRect.Y`. `top` normally, `bottom` when the height was negative.
    pub dest_y: i32,
    /// `destRect.Width`, *signed*: GDI+ mirrors horizontally on a negative
    /// extent and Emuera deliberately leaves it negative (`:106-110`).
    pub dest_width: i32,
    /// `destRect.Height`, signed; negative mirrors vertically (`:111-115`).
    pub dest_height: i32,
}

impl ImageGeometry {
    /// `ConsoleImagePart.cs:74-116` verbatim, for a sprite whose
    /// `DestBaseSize` is `sprite_w * sprite_h`.
    ///
    /// `font_size` is `Config.FontSize` (erars `EraConfig::font_size`). Note
    /// that it is *not* `LineHeight`: an image is measured against the font
    /// and may overflow the line, which the caller handles as an escaped part.
    pub fn new(
        font_size: i32,
        sprite_w: u32,
        sprite_h: u32,
        width: Option<MixedNum>,
        height: Option<MixedNum>,
        ypos: Option<MixedNum>,
    ) -> Self {
        // `:76-83` — an absent or zero height is the font size, not a
        // percentage of it.
        let mut height_px = match height {
            None => font_size,
            Some(h) if h.num == 0 => font_size,
            Some(h) if h.is_px => h.num,
            Some(h) => h.num.saturating_mul(font_size) / 100,
        };

        // `:87-102` — an absent or zero width keeps the sprite's aspect
        // ratio and records the dropped fraction; a percentage does the same
        // against the font size; `px` is exact.
        let (mut width_px, x_sub_pixel) = match width {
            None | Some(MixedNum { num: 0, .. }) => {
                if sprite_h == 0 {
                    (0, 0.0)
                } else {
                    let exact = sprite_w as f32 * height_px as f32 / sprite_h as f32;
                    let w = (sprite_w as i64 * height_px as i64 / sprite_h as i64) as i32;
                    (w, exact - w as f32)
                }
            }
            Some(w) if w.is_px => (w.num, 0.0),
            Some(w) => {
                let n = w.num.saturating_mul(font_size);
                let px = n / 100;
                (px, n as f32 / 100.0 - px as f32)
            }
        };

        // `:104`
        let top = MixedNum::to_pixel(ypos, font_size, 0);

        // `:105-116` — the destination extents stay signed so GDI+ mirrors,
        // while the layout scalars are made positive.
        let mut dest_x = 0;
        let mut dest_y = top;
        let dest_width = width_px;
        let dest_height = height_px;
        if dest_width < 0 {
            dest_x = -dest_width;
            width_px = -dest_width;
        }
        if dest_height < 0 {
            dest_y = dest_y.saturating_sub(dest_height);
            height_px = -dest_height;
        }

        Self {
            width: width_px.max(0) as u32,
            x_sub_pixel,
            top,
            bottom: top.saturating_add(height_px),
            dest_x,
            dest_y,
            dest_width,
            dest_height,
        }
    }

    /// Whether the image leaves its console line and must be drawn as an
    /// escaped part instead. `ConsoleButtonString.FilterEscaped`
    /// (`GameView/ConsoleButtonString.cs:141-150`).
    pub fn escapes(&self, line_height: i32) -> bool {
        self.top < 0 || self.bottom > line_height
    }

    /// The inclusive line offsets an escaped part spans, relative to the line
    /// it was printed on. `EmueraConsole.Print.cs:164-166`.
    pub fn escaped_line_span(&self, line_height: i32) -> (i32, i32) {
        debug_assert!(line_height > 0);
        let first = div_ceil(self.top, line_height);
        let last = (self.bottom - 1).max(0) / line_height;
        (first, last)
    }
}

/// C#'s `(int)Math.Ceiling((double)a / b)` for a positive `b`.
fn div_ceil(a: i32, b: i32) -> i32 {
    let q = a / b;
    if a % b > 0 {
        q + 1
    } else {
        q
    }
}

/// One inline image on a console line — Emuera's `ConsoleImagePart` for the
/// case where the sprite resolved. When it does *not* resolve, Emuera prints
/// the reconstructed tag as ordinary text (`ConsoleImagePart.cs:69-73`) and so
/// does erars, which is why an unresolved image never becomes an
/// `InlineImage`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InlineImage {
    /// `ResourceName`, already upper-cased by the sprite lookup.
    pub name: SmolStr,
    /// The `srcb` sprite, drawn instead of `sprite` while the containing
    /// button is hovered (`ConsoleImagePart.cs:203-204`). `None` falls back to
    /// `sprite`, which is exactly what Emuera does — the wiki's claim that an
    /// omitted `srcb` "uses the same image" is true in effect, but there is no
    /// copy.
    pub button: Option<InlineSprite>,
    /// The `srcm` hit-mask sprite, sampled by `GETMAPPINGCOLOR`
    /// (`ConsoleImagePart.cs:176-196`). Never drawn.
    pub mask: Option<InlineSprite>,
    pub sprite: InlineSprite,
    pub geometry: ImageGeometry,
    /// Emuera `AltText`: the reconstructed `<img …>` tag
    /// (`ConsoleImagePart.cs:25-65`). It is what Emuera itself prints when the
    /// resource is missing, so text-only front-ends show it verbatim rather
    /// than inventing a placeholder.
    pub alt: String,
}

impl InlineImage {
    /// `Utils.AddTagArg` / `AddTagMixedNumArg` over the constructor's
    /// arguments (`ConsoleImagePart.cs:25-65`). `srcb` is emitted whenever it
    /// was given, `srcm` only when non-empty — the asymmetry is in the source.
    pub fn alt_text(
        name: &str,
        button: Option<&str>,
        mask: Option<&str>,
        width: Option<MixedNum>,
        height: Option<MixedNum>,
        ypos: Option<MixedNum>,
        font_size: i32,
    ) -> String {
        let mut out = String::with_capacity(name.len() + 16);
        out.push_str("<img src='");
        out.push_str(name);
        out.push('\'');
        if let Some(b) = button {
            let _ = write!(out, " srcb='{b}'");
        }
        if let Some(m) = mask.filter(|m| !m.is_empty()) {
            let _ = write!(out, " srcm='{m}'");
        }
        // The source appends height before width.
        MixedNum::write_tag_arg(height, "height", font_size, &mut out);
        MixedNum::write_tag_arg(width, "width", font_size, &mut out);
        MixedNum::write_tag_arg(ypos, "ypos", font_size, &mut out);
        out.push('>');
        out
    }

    /// The sprite to draw: `srcb` while hovered, otherwise `src`.
    pub fn draw_sprite(&self, selecting: bool) -> &InlineSprite {
        match &self.button {
            Some(b) if selecting => b,
            _ => &self.sprite,
        }
    }

    /// The bitmap, destination rect and source window for one draw of this
    /// image, in the space of the part's own origin: `x` from the part's
    /// `PointX`, `y` from the top of its console line.
    ///
    /// `ConsoleImagePart.DrawTo` (`GameView/ConsoleImagePart.cs:194-215`)
    /// hands `destRect` to `ASprite.GraphicsDraw(Graphics, Rectangle)`, and
    /// the two sprite kinds implement that differently: a still sprite only
    /// shifts the box by its `DestBasePosition`
    /// (`Content/CroppedImage.cs:100-107`), while an animation *also* scales
    /// the box down to the current frame's window (`:290-297`), which is how a
    /// frame smaller than `DestBaseSize` lands inside the sprite's box.
    ///
    /// Extents stay signed: GDI+ mirrors a negative one, and Emuera leaves
    /// `destRect` signed on purpose (`ConsoleImagePart.cs:106-115`).
    /// `None` when there is nothing to draw — an animation with no frames, a
    /// frame whose box missed the sprite, or a degenerate sprite box.
    pub fn draw_rects(&self, selecting: bool, elapsed_ms: u64) -> Option<(BitmapId, Rect, Rect)> {
        let sprite = self.draw_sprite(selecting);
        let g = &self.geometry;
        let mut dest = Rect::new(g.dest_x, g.dest_y, g.dest_width, g.dest_height);
        let (dw, dh) = (dest.width, dest.height);

        match &sprite.sampler {
            ImageSampler::Single { bitmap, src } => {
                // `if (!DestBasePosition.IsEmpty)` — the guard is on the pair,
                // so a sprite at (0, 0) skips both divisions.
                if sprite.pos_x != 0 || sprite.pos_y != 0 {
                    if src.width != 0 {
                        dest.x = dest.x.saturating_add(scale(sprite.pos_x, dw, src.width));
                    }
                    if src.height != 0 {
                        dest.y = dest.y.saturating_add(scale(sprite.pos_y, dh, src.height));
                    }
                }
                Some((*bitmap, dest, *src))
            }
            ImageSampler::Anime { .. } => {
                let (bitmap, src, offset_x, offset_y) = sprite.frame_at(elapsed_ms)?;
                // `DestBaseSize`, which `SPRITEANIMECREATE` forces positive.
                let (bw, bh) = (sprite.width as i32, sprite.height as i32);
                if bw == 0 || bh == 0 {
                    return None;
                }
                dest.x = dest
                    .x
                    .saturating_add(scale(sprite.pos_x.saturating_add(offset_x), dw, bw));
                dest.y = dest
                    .y
                    .saturating_add(scale(sprite.pos_y.saturating_add(offset_y), dh, bh));
                dest.width = scale(src.width, dw, bw);
                dest.height = scale(src.height, dh, bh);
                Some((bitmap, dest, src))
            }
        }
    }
}

/// `a * b / c` in C# `int` arithmetic, widened so an intermediate cannot
/// overflow and saturating instead of wrapping at the ends.
fn scale(a: i32, b: i32, c: i32) -> i32 {
    debug_assert_ne!(c, 0);
    let v = a as i64 * b as i64 / c as i64;
    v.clamp(i32::MIN as i64, i32::MAX as i64) as i32
}

/// A decoded, immutable bitmap snapshot. `0xAARRGGBB`, row-major — the same
/// packing the VM's `GraphicsStore` uses, so publishing is one memcpy and the
/// renderer's upload is a straight reinterpretation.
#[derive(Debug)]
pub struct ImageBitmap {
    pub width: u32,
    pub height: u32,
    pub pixels: Box<[u32]>,
    /// Bumped on every publish, so a renderer can keep a GPU texture keyed by
    /// `(id, generation)` and re-upload only when the pixels really changed.
    pub generation: u64,
}

impl ImageBitmap {
    pub fn new(width: u32, height: u32, pixels: Box<[u32]>, generation: u64) -> Self {
        debug_assert_eq!(pixels.len() as u64, width as u64 * height as u64);
        Self {
            width,
            height,
            pixels,
            generation,
        }
    }

    /// `0xAARRGGBB` at `(x, y)`, or fully transparent outside the bitmap.
    /// Emuera `ASpriteSingle.SpriteGetColor` returns `Color.Transparent` out
    /// of bounds (`Content/CroppedImage.cs:78-89`).
    pub fn pixel(&self, x: i32, y: i32) -> u32 {
        if x < 0 || y < 0 || x >= self.width as i32 || y >= self.height as i32 {
            return 0;
        }
        self.pixels[y as usize * self.width as usize + x as usize]
    }
}

/// The decoded pixels every front-end draws from, shared by cheap clone.
///
/// Emuera reads the live `Bitmap` because it has one thread; erars publishes
/// into this store at the redraw boundary so the renderer thread always sees a
/// complete frame. Steady state costs nothing: a bitmap nobody drew into is
/// not republished, and the renderer only re-uploads when `generation` moves.
#[derive(Clone, Default)]
pub struct ImageStore {
    inner: Arc<RwLock<HashMap<BitmapId, Arc<ImageBitmap>>>>,
}

impl ImageStore {
    pub fn new() -> Self {
        Self::default()
    }

    /// Replace `id`'s pixels. The previous `Arc` stays alive for whichever
    /// frame is still holding it.
    pub fn publish(&self, id: BitmapId, bitmap: Arc<ImageBitmap>) {
        self.inner.write().insert(id, bitmap);
    }

    /// `GDISPOSE`. Front-ends holding the old `Arc` keep drawing it until
    /// their next frame, which is the same one-frame lag as any other
    /// publish.
    pub fn remove(&self, id: BitmapId) {
        self.inner.write().remove(&id);
    }

    pub fn clear(&self) {
        self.inner.write().clear();
    }

    /// One `Arc` clone under a read lock — cheap enough to call per image per
    /// frame, which is the only place it is called.
    pub fn get(&self, id: BitmapId) -> Option<Arc<ImageBitmap>> {
        self.inner.read().get(&id).cloned()
    }

    /// The generation currently published for `id`, for cache validation
    /// without cloning the `Arc`.
    pub fn generation(&self, id: BitmapId) -> Option<u64> {
        self.inner.read().get(&id).map(|b| b.generation)
    }

    pub fn len(&self) -> usize {
        self.inner.read().len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.read().is_empty()
    }
}

impl std::fmt::Debug for ImageStore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ImageStore").field("len", &self.len()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const FS: i32 = 18;

    /// `:76-77` + `:87-91`: no attributes at all means "font size tall, aspect
    /// ratio wide", with the dropped fraction kept for the next part.
    #[test]
    fn defaults_to_font_size_and_aspect_ratio() {
        let g = ImageGeometry::new(FS, 100, 40, None, None, None);
        // height = 18, width = 100*18/40 = 45 exactly.
        assert_eq!(g.width, 45);
        assert_eq!(g.x_sub_pixel, 0.0);
        assert_eq!((g.top, g.bottom), (0, 18));
        assert_eq!((g.dest_x, g.dest_y), (0, 0));
        assert_eq!((g.dest_width, g.dest_height), (45, 18));
    }

    #[test]
    fn aspect_ratio_keeps_the_dropped_fraction() {
        // 7*18/40 = 3.15 -> Width 3, XsubPixel 0.15
        let g = ImageGeometry::new(FS, 7, 40, None, None, None);
        assert_eq!(g.width, 3);
        assert!((g.x_sub_pixel - 0.15).abs() < 1e-4, "{}", g.x_sub_pixel);
    }

    /// `:76` — an explicit zero is the same as absent, not a zero-height box.
    #[test]
    fn zero_height_is_the_font_size() {
        let g = ImageGeometry::new(FS, 10, 10, None, Some(MixedNum::percent(0)), None);
        assert_eq!(g.bottom, 18);
        assert_eq!(g.width, 18);
    }

    /// `:82-83` / `:100-101`: percentages are of the font size, and the width
    /// percentage keeps its own fraction.
    #[test]
    fn percentages_are_of_the_font_size() {
        let g = ImageGeometry::new(
            FS,
            10,
            10,
            Some(MixedNum::percent(250)),
            Some(MixedNum::percent(200)),
            None,
        );
        assert_eq!(g.bottom, 36); // 18*200/100
        assert_eq!(g.width, 45); // 18*250/100
        assert_eq!(g.x_sub_pixel, 0.0);

        let g = ImageGeometry::new(FS, 10, 10, Some(MixedNum::percent(133)), None, None);
        assert_eq!(g.width, 23); // 18*133/100 = 23.94
        assert!((g.x_sub_pixel - 0.94).abs() < 1e-4, "{}", g.x_sub_pixel);
    }

    /// `:80-81` / `:92-95`: `px` is literal and contributes no fraction.
    #[test]
    fn px_is_literal() {
        let g =
            ImageGeometry::new(FS, 10, 10, Some(MixedNum::px(300)), Some(MixedNum::px(200)), None);
        assert_eq!(g.width, 300);
        assert_eq!(g.bottom, 200);
        assert_eq!(g.x_sub_pixel, 0.0);
    }

    /// `:104`: `ypos` shifts the box, and `bottom` follows it. A negative
    /// `ypos` lifts the image above its line.
    #[test]
    fn ypos_shifts_the_box() {
        let g = ImageGeometry::new(FS, 10, 10, None, None, Some(MixedNum::px(-30)));
        assert_eq!((g.top, g.bottom), (-30, -12));
        assert_eq!(g.dest_y, -30);

        let g = ImageGeometry::new(FS, 10, 10, None, None, Some(MixedNum::percent(-100)));
        assert_eq!((g.top, g.bottom), (-18, 0));
    }

    /// `:106-110`: a negative width mirrors horizontally — `dest_width` stays
    /// negative while the advance is positive and `dest_x` moves right.
    #[test]
    fn negative_width_mirrors_in_place() {
        let g = ImageGeometry::new(FS, 10, 10, Some(MixedNum::px(-40)), None, None);
        assert_eq!(g.width, 40);
        assert_eq!(g.dest_x, 40);
        assert_eq!(g.dest_width, -40);
    }

    /// `:111-115`: same for height, vertically.
    #[test]
    fn negative_height_mirrors_in_place() {
        let g = ImageGeometry::new(FS, 10, 10, Some(MixedNum::px(10)), Some(MixedNum::px(-40)), None);
        assert_eq!((g.top, g.bottom), (0, 40));
        assert_eq!(g.dest_y, 40);
        assert_eq!(g.dest_height, -40);
    }

    /// `ConsoleButtonString.cs:141-150` + `EmueraConsole.Print.cs:164-166`.
    #[test]
    fn escaped_span_covers_the_overlapped_lines() {
        let line_h = 20;

        // Fits exactly: not escaped.
        let g = ImageGeometry::new(20, 10, 10, None, None, None);
        assert!(!g.escapes(line_h));

        // 60px tall from the top of its line spans lines 0..=2.
        let g = ImageGeometry::new(20, 10, 10, None, Some(MixedNum::px(60)), None);
        assert!(g.escapes(line_h));
        assert_eq!(g.escaped_line_span(line_h), (0, 2));

        // Lifted a whole line: starts one line above, ends on its own line.
        let g = ImageGeometry::new(20, 10, 10, None, Some(MixedNum::px(40)), Some(MixedNum::px(-20)));
        assert_eq!((g.top, g.bottom), (-20, 20));
        assert_eq!(g.escaped_line_span(line_h), (-1, 0));
    }

    /// `ConsoleImagePart.cs:25-65` via `Utils.AddTagArg`/`AddTagMixedNumArg`:
    /// height before width, zeros dropped, `srcm` only when non-empty, and a
    /// percentage written resolved against the font size while a `px` value
    /// keeps its suffix (`MixedNum.BuilderString`, `Utils.cs:44-48`).
    #[test]
    fn alt_text_matches_emuera() {
        assert_eq!(InlineImage::alt_text("A", None, None, None, None, None, 18), "<img src='A'>");
        assert_eq!(
            InlineImage::alt_text("A", Some("B"), Some(""), None, None, None, 18),
            "<img src='A' srcb='B'>"
        );
        assert_eq!(
            InlineImage::alt_text(
                "A",
                None,
                Some("M"),
                Some(MixedNum::percent(0)),
                Some(MixedNum::px(200)),
                Some(MixedNum::percent(-50)),
                18,
            ),
            "<img src='A' srcm='M' height='200px' ypos='-9'>"
        );
        // The zero test is on the written number, so a percentage too small to
        // resolve to a pixel still appears — as `0`.
        assert_eq!(
            InlineImage::alt_text("A", None, None, Some(MixedNum::percent(4)), None, None, 18, ),
            "<img src='A' width='0'>"
        );
    }

    #[test]
    fn anime_frame_picks_by_elapsed_time() {
        let sprite = InlineSprite {
            sampler: ImageSampler::Anime {
                frames: vec![
                    ImageFrame {
                        bitmap: 1,
                        src: Rect::new(0, 0, 4, 4),
                        offset_x: 0,
                        offset_y: 0,
                        delay_ms: 100,
                        empty: false,
                    },
                    ImageFrame {
                        bitmap: 2,
                        src: Rect::new(4, 0, 4, 4),
                        offset_x: 1,
                        offset_y: 2,
                        delay_ms: 50,
                        empty: false,
                    },
                ],
                total_ms: 150,
            },
            width: 4,
            height: 4,
            pos_x: 0,
            pos_y: 0,
        };

        assert_eq!(sprite.frame_at(0).unwrap().0, 1);
        assert_eq!(sprite.frame_at(99).unwrap().0, 1);
        assert_eq!(sprite.frame_at(100).unwrap().0, 2);
        assert_eq!(sprite.frame_at(149).unwrap().0, 2);
        // Wraps.
        assert_eq!(sprite.frame_at(150).unwrap().0, 1);
        assert_eq!(sprite.frame_at(100).unwrap().2, 1);
    }

    #[test]
    fn empty_anime_draws_nothing() {
        let sprite = InlineSprite {
            sampler: ImageSampler::Anime { frames: Vec::new(), total_ms: 0 },
            width: 4,
            height: 4,
            pos_x: 0,
            pos_y: 0,
        };
        assert!(sprite.frame_at(0).is_none());
    }

    #[test]
    fn store_publishes_and_bumps_generation() {
        let store = ImageStore::new();
        assert!(store.is_empty());

        store.publish(3, Arc::new(ImageBitmap::new(2, 1, vec![0xFF00FF00, 0].into(), 1)));
        let first = store.get(3).unwrap();
        assert_eq!(first.pixel(0, 0), 0xFF00FF00);
        assert_eq!(first.pixel(5, 0), 0);
        assert_eq!(store.generation(3), Some(1));

        store.publish(3, Arc::new(ImageBitmap::new(2, 1, vec![0xFF0000FF, 0].into(), 2)));
        assert_eq!(store.generation(3), Some(2));
        // The frame that grabbed the old Arc still sees the old pixels.
        assert_eq!(first.pixel(0, 0), 0xFF00FF00);

        store.remove(3);
        assert!(store.get(3).is_none());
    }
}
