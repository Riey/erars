//! The console-background plane: the *second* image surface, the one the
//! `CBG*` methods draw on.
//!
//! It is not a list of sprite commands. Emuera keeps one `cbgList` on the
//! console itself (`GameView/EmueraConsole.cs:101-130`), sorted by descending
//! `zdepth`, and `OnPaint` walks it and the text in **one merged loop**
//! (`:1557-1599`): entries with a depth above 0 are drawn, then the whole text
//! log at the dummy depth 0, then the entries below it. So a negative `zdepth`
//! puts an image *in front of* the text, which no other erars surface can do,
//! and a positive one puts it behind — a wallpaper the log floats on.
//!
//! Three more properties follow from where the plane lives rather than from
//! any single command, and they are the reason this is a plane and not eight
//! methods:
//!
//! * **Coordinates are client pixels from the bottom-left corner and do not
//!   scroll.** `OnPaint` draws each entry at
//!   `(x, y + ClientHeight - DestBaseSize.Height)` (`:1573`), outside the
//!   `pointY` cursor the text loop advances, so scrolling the log leaves the
//!   plane where it is.
//! * **It outlives the game.** `CBG_Clear`'s only caller is the console
//!   constructor (`:93`), so `SPRITEDISPOSEALL`, loading a save and starting a
//!   new game all leave the plane standing. Only `CBGCLEAR` empties it.
//! * **Hit testing is a pixel lookup, not a rectangle test.** A separate
//!   button-map bitmap ([`CbgLayer::button_map`]) is never drawn; the colour
//!   under the cursor *is* the button value (`MoveMouse`, `:2009-2025`).
//!
//! [`CbgImage`] holds an [`InlineSprite`] — the same resolved-sprite snapshot
//! `ConsoleLinePart::Image` carries — so `CBGSETSPRITE` and `<img>` share one
//! notion of "how to sample a sprite", and one animation clock.

use serde::{Deserialize, Serialize};

use crate::image::{BitmapId, ImageSampler, ImageStore, InlineSprite, Rect};

/// One `ClientBackGroundImage` (`GameView/EmueraConsole.cs:107-129`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CbgImage {
    /// `Img`. `None` only for a button entry: `CBG_SetButtonImage` accepts an
    /// unresolvable sprite name and stores the null (`:220-235`), unlike
    /// `CBG_SetImage`, which rejects it.
    pub image: Option<InlineSprite>,
    /// `ImgB`, drawn in place of `image` while this entry's button is the one
    /// under the cursor.
    pub image_b: Option<InlineSprite>,
    pub x: i32,
    pub y: i32,
    /// Draw order, descending: higher is further back, and 0 is reserved for
    /// the text (`:109-111`), so no entry can hold it.
    pub zdepth: i32,
    /// `buttonValue` when `isButton`. The value the button map's pixel colour
    /// must equal for `image_b` to be drawn, so it is a 24-bit colour.
    pub button: Option<u32>,
}

impl CbgImage {
    /// The sprite to draw: `ImgB` while this entry's button is selected,
    /// otherwise `Img` (`:1569-1571`). `None` when that sprite is absent, in
    /// which case the entry draws nothing but keeps its place in the list.
    pub fn draw_sprite(&self, selecting: bool) -> Option<&InlineSprite> {
        match (&self.button, selecting) {
            (Some(_), true) => self.image_b.as_ref(),
            _ => self.image.as_ref(),
        }
    }

    /// The bitmap, destination box and source window for one draw, in client
    /// pixels from the **top-left** corner — the space the renderer draws in.
    ///
    /// `OnPaint` calls `ASprite.GraphicsDraw(Graphics, Point)`
    /// (`EmueraConsole.cs:1573`), the *point* overload, which is not the one
    /// `ConsoleImagePart` uses: it offsets by `DestBasePosition` without
    /// scaling anything and draws at the sprite's natural `DestBaseSize`
    /// (`Content/CroppedImage.cs:96-100`), or at the current frame's own size
    /// for an animation (`:289-298`). The bottom-left origin is folded in
    /// here, against `DestBaseSize.Height` even when an animation frame is
    /// smaller, exactly as `:1573` computes it.
    ///
    /// `None` when there is nothing to draw: no sprite, or an animation with
    /// no frames.
    pub fn draw_rects(
        &self,
        selecting: bool,
        elapsed_ms: u64,
        client_height: i32,
    ) -> Option<(BitmapId, Rect, Rect)> {
        let sprite = self.draw_sprite(selecting)?;

        // `new Point(x, y + ClientHeight - DestBaseSize.Height)`, then
        // `offset.Offset(DestBasePosition)`.
        let x = self.x.saturating_add(sprite.pos_x);
        let y = self
            .y
            .saturating_add(client_height)
            .saturating_sub(sprite.height as i32)
            .saturating_add(sprite.pos_y);

        match &sprite.sampler {
            ImageSampler::Single { bitmap, src } => Some((
                *bitmap,
                Rect::new(x, y, sprite.width as i32, sprite.height as i32),
                *src,
            )),
            ImageSampler::Anime { .. } => {
                let (bitmap, src, offset_x, offset_y) = sprite.frame_at(elapsed_ms)?;
                Some((
                    bitmap,
                    // `Rectangle(offset + frame.Offset, frame.SrcRectangle.Size)`
                    // — the frame's window, not the sprite's box.
                    Rect::new(
                        x.saturating_add(offset_x),
                        y.saturating_add(offset_y),
                        src.width,
                        src.height,
                    ),
                    src,
                ))
            }
        }
    }
}

/// The plane: `cbgList` plus `cbgButtonMap`
/// (`GameView/EmueraConsole.cs:101-102`).
///
/// `selectingCBGButtonInt` is deliberately *not* here. It is not game state:
/// Emuera recomputes it from the cursor on every `MouseMove` and clears it
/// whenever the map goes away, so in erars it belongs to the front-end that
/// owns the cursor, and a plane crossing the channel carries no stale hover.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct CbgLayer {
    /// Sorted by descending `zdepth`, which is draw order: back to front.
    ///
    /// Emuera's `List.Sort` is introsort and so unstable, leaving the order of
    /// equal depths unspecified; erars sorts stably, which keeps the later
    /// `CBGSET*` of two entries at the same depth on top — the one order a
    /// script could rely on.
    images: Vec<CbgImage>,
    /// The bitmap `CBGSETBMAPG` handed over. Never drawn, only sampled by
    /// [`CbgLayer::hit_test`].
    button_map: Option<BitmapId>,
}

impl CbgLayer {
    /// `CBG_Clear` (`:131-142`): drop every entry and the button map.
    ///
    /// Emuera also disposes the anonymous `SpriteG` that `CBG_SetGraphics`
    /// wrapped around a bitmap; erars stores a [`CbgImage`] by value and holds
    /// the bitmap by id, so dropping the entry *is* the dispose.
    pub fn clear(&mut self) {
        self.images.clear();
        self.clear_button_map();
    }

    /// `CBG_ClearRange` (`:144-160`): drop the entries inside an inclusive
    /// depth range.
    pub fn clear_range(&mut self, zmin: i32, zmax: i32) {
        if zmin > zmax {
            return;
        }
        // The dummy at depth 0 is skipped there; no entry can hold 0 here.
        self.images
            .retain(|c| c.zdepth < zmin || c.zdepth > zmax);
    }

    /// `CBG_ClearButton` (`:162-177`): drop every button entry, and the map
    /// with them.
    pub fn clear_button(&mut self) {
        self.images.retain(|c| c.button.is_none());
        self.clear_button_map();
    }

    /// `CBG_ClearBMap` (`:179-184`).
    pub fn clear_button_map(&mut self) {
        self.button_map = None;
    }

    /// `CBG_SetImage` (`:192-205`). `zdepth == 0` is the caller's error to
    /// raise — both methods that reach here reject it first.
    pub fn set_image(&mut self, image: InlineSprite, x: i32, y: i32, zdepth: i32) {
        self.push(CbgImage {
            image: Some(image),
            image_b: None,
            x,
            y,
            zdepth,
            button: None,
        });
    }

    /// `CBG_SetButtonImage` (`:220-235`). Either sprite may be absent, and
    /// the entry is registered regardless.
    pub fn set_button_image(
        &mut self,
        button: u32,
        image: Option<InlineSprite>,
        image_b: Option<InlineSprite>,
        x: i32,
        y: i32,
        zdepth: i32,
    ) {
        self.push(CbgImage {
            image,
            image_b,
            x,
            y,
            zdepth,
            button: Some(button),
        });
    }

    /// `cbgList.Add` then `cbgList.Sort` (`:203-204`).
    fn push(&mut self, image: CbgImage) {
        self.images.push(image);
        self.images.sort_by(|a, b| b.zdepth.cmp(&a.zdepth));
    }

    /// `CBG_SetButtonMap` (`:208-218`): `false` when this bitmap is already
    /// the map, which is Emuera's "nothing changed, do not repaint".
    pub fn set_button_map(&mut self, bitmap: BitmapId) -> bool {
        if self.button_map == Some(bitmap) {
            return false;
        }
        self.button_map = Some(bitmap);
        true
    }

    /// The entries drawn *behind* the text: `zdepth > 0`, back to front.
    pub fn background(&self) -> &[CbgImage] {
        &self.images[..self.split()]
    }

    /// The entries drawn *in front of* the text: `zdepth < 0`, back to front.
    pub fn foreground(&self) -> &[CbgImage] {
        &self.images[self.split()..]
    }

    /// Where the descending list crosses the text's depth 0.
    fn split(&self) -> usize {
        self.images.partition_point(|c| c.zdepth > 0)
    }

    /// The button value under a cursor at client position `(x, y)`, measured
    /// from the **bottom-left** corner as [`crate::MouseKeyEvent`] reports it,
    /// or `-1` for no button.
    ///
    /// `MoveMouse` (`GameView/EmueraConsole.cs:2009-2025`) and `MouseDown`
    /// (`:1000-1014`) run the same three steps: shift the point into the map
    /// bitmap's own top-left space by adding the map's height, reject anything
    /// outside it, and read the pixel. Alpha must be **exactly** 255 — a
    /// feathered edge is not a button — and the value is the colour's low 24
    /// bits.
    pub fn hit_test(&self, store: &ImageStore, x: i64, y: i64) -> i32 {
        let Some(bitmap) = self.button_map else {
            return -1;
        };
        let Some(map) = store.get(bitmap) else {
            return -1;
        };
        let map_y = y + map.height as i64;
        if x < 0 || map_y < 0 || x >= map.width as i64 || map_y >= map.height as i64 {
            return -1;
        }
        let argb = map.pixel(x as i32, map_y as i32);
        if argb >> 24 != 0xFF {
            return -1;
        }
        (argb & 0xFF_FFFF) as i32
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::image::ImageBitmap;

    fn sprite(width: u32, height: u32) -> InlineSprite {
        InlineSprite {
            sampler: ImageSampler::Single {
                bitmap: 1,
                src: Rect::new(0, 0, width as i32, height as i32),
            },
            width,
            height,
            pos_x: 0,
            pos_y: 0,
        }
    }

    #[test]
    fn depth_orders_the_plane_back_to_front_and_splits_it_at_the_text() {
        let mut layer = CbgLayer::default();
        for z in [1, -5, 7, -1] {
            layer.set_image(sprite(4, 4), 0, 0, z);
        }

        assert_eq!(
            layer.background().iter().map(|c| c.zdepth).collect::<Vec<_>>(),
            [7, 1]
        );
        assert_eq!(
            layer.foreground().iter().map(|c| c.zdepth).collect::<Vec<_>>(),
            [-1, -5]
        );
    }

    #[test]
    fn equal_depths_keep_insertion_order_so_the_later_call_is_on_top() {
        let mut layer = CbgLayer::default();
        layer.set_image(sprite(4, 4), 10, 0, 3);
        layer.set_image(sprite(4, 4), 20, 0, 3);
        layer.set_image(sprite(4, 4), 30, 0, 3);

        assert_eq!(
            layer.background().iter().map(|c| c.x).collect::<Vec<_>>(),
            [10, 20, 30]
        );
    }

    #[test]
    fn the_bottom_left_origin_is_measured_against_the_sprite_box() {
        let mut layer = CbgLayer::default();
        // A sprite 30 px tall at y = 0 sits flush with the bottom of a
        // 480 px client: 0 + 480 - 30.
        layer.set_image(sprite(40, 30), 12, 0, 1);
        let (bitmap, dest, src) = layer.background()[0].draw_rects(false, 0, 480).unwrap();

        assert_eq!(bitmap, 1);
        assert_eq!(dest, Rect::new(12, 450, 40, 30));
        assert_eq!(src, Rect::new(0, 0, 40, 30));

        // y counts upward from there.
        layer.clear();
        layer.set_image(sprite(40, 30), 0, 100, 1);
        let (_, dest, _) = layer.background()[0].draw_rects(false, 0, 480).unwrap();
        assert_eq!(dest.y, 550);
    }

    #[test]
    fn an_animation_frame_draws_at_its_own_size_inside_the_sprite_box() {
        let frames = vec![
            crate::image::ImageFrame {
                bitmap: 2,
                src: Rect::new(0, 0, 8, 8),
                offset_x: 1,
                offset_y: 2,
                delay_ms: 100,
                empty: false,
            },
            crate::image::ImageFrame {
                bitmap: 2,
                src: Rect::new(8, 0, 4, 4),
                offset_x: 0,
                offset_y: 0,
                delay_ms: 100,
                empty: false,
            },
        ];
        let mut layer = CbgLayer::default();
        layer.set_image(
            InlineSprite {
                sampler: ImageSampler::Anime {
                    frames,
                    total_ms: 200,
                },
                width: 16,
                height: 16,
                pos_x: 0,
                pos_y: 0,
            },
            0,
            0,
            1,
        );
        let cbg = &layer.background()[0];

        // The flip uses DestBaseSize.Height (16), never the frame's.
        let (bitmap, dest, src) = cbg.draw_rects(false, 0, 100).unwrap();
        assert_eq!(bitmap, 2);
        assert_eq!(dest, Rect::new(1, 100 - 16 + 2, 8, 8));
        assert_eq!(src, Rect::new(0, 0, 8, 8));

        let (_, dest, src) = cbg.draw_rects(false, 150, 100).unwrap();
        assert_eq!(dest, Rect::new(0, 84, 4, 4));
        assert_eq!(src, Rect::new(8, 0, 4, 4));
    }

    #[test]
    fn a_selected_button_swaps_in_its_second_sprite() {
        let mut layer = CbgLayer::default();
        layer.set_button_image(0x40, Some(sprite(4, 4)), Some(sprite(9, 9)), 0, 0, 2);
        let cbg = &layer.background()[0];

        assert_eq!(cbg.draw_rects(false, 0, 50).unwrap().1.width, 4);
        assert_eq!(cbg.draw_rects(true, 0, 50).unwrap().1.width, 9);

        // A button with no `ImgB` disappears while selected — Emuera's
        // `img = ImgB` then `if (img != null …)`.
        layer.clear();
        layer.set_button_image(0x40, Some(sprite(4, 4)), None, 0, 0, 2);
        assert!(layer.background()[0].draw_rects(true, 0, 50).is_none());
    }

    #[test]
    fn clear_range_is_inclusive_and_clear_button_takes_the_map_with_it() {
        let mut layer = CbgLayer::default();
        for z in [-2, -1, 1, 2, 3] {
            layer.set_image(sprite(4, 4), 0, 0, z);
        }
        layer.clear_range(-1, 2);
        assert_eq!(
            layer.background().iter().map(|c| c.zdepth).collect::<Vec<_>>(),
            [3]
        );
        assert_eq!(
            layer.foreground().iter().map(|c| c.zdepth).collect::<Vec<_>>(),
            [-2]
        );

        // `zmin > zmax` is a no-op, not an empty range that clears nothing
        // by accident.
        layer.clear_range(5, -5);
        assert_eq!(layer.background().len() + layer.foreground().len(), 2);

        layer.set_button_image(1, Some(sprite(4, 4)), None, 0, 0, 4);
        assert!(layer.set_button_map(7));
        assert!(!layer.set_button_map(7));
        layer.clear_button();
        // The map went with the button entry: setting the *same* bitmap again
        // is a change now, where a moment ago it was not.
        assert!(layer.set_button_map(7));
        assert_eq!(layer.background().len() + layer.foreground().len(), 2);
    }

    #[test]
    fn the_button_map_is_sampled_bottom_up_and_only_where_it_is_opaque() {
        // 4 x 2 map: opaque red at (0,0), opaque 0x123456 at (3,1),
        // a nearly-opaque pixel at (1,0).
        let mut pixels = vec![0u32; 8];
        pixels[0] = 0xFF00_0000 | 0x00FF_0000;
        pixels[1] = 0xFE12_3456;
        pixels[7] = 0xFF12_3456;
        let store = ImageStore::new();
        store.publish(9, Arc::new(ImageBitmap::new(4, 2, pixels.into(), 1)));

        let mut layer = CbgLayer::default();
        assert_eq!(layer.hit_test(&store, 0, -2), -1, "no map, no button");
        layer.set_button_map(9);

        // Bottom-origin y = -2 is the map's top row, y = -1 its bottom row.
        assert_eq!(layer.hit_test(&store, 0, -2), 0xFF0000);
        assert_eq!(layer.hit_test(&store, 3, -1), 0x123456);
        // Alpha 0xFE is not 0xFF.
        assert_eq!(layer.hit_test(&store, 1, -2), -1);
        // Transparent, and outside.
        assert_eq!(layer.hit_test(&store, 2, -2), -1);
        assert_eq!(layer.hit_test(&store, 4, -1), -1);
        assert_eq!(layer.hit_test(&store, 0, -3), -1);
        assert_eq!(layer.hit_test(&store, 0, 0), -1);
        assert_eq!(layer.hit_test(&store, -1, -1), -1);
    }
}
