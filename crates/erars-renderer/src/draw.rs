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

use crate::gpu::Instance;
use crate::layout::Layout;
use crate::raster::{AtlasRegion, GlyphRaster, RasterKey};
use crate::text::{CellMetrics, ShapedGlyph, Shaper};

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
        let font = self.shaper.chain().font(g.font);
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

/// Build per-page instance lists for the rows of `layout` that `view` shows.
/// `hover` is an index into `layout.buttons`; its clusters and rects are drawn
/// in `hl`. Returns one bucket per atlas page (`buckets[p]` samples page `p`),
/// ready for `raster.pages_with(&buckets)` → `GpuContext::render`.
#[allow(clippy::too_many_arguments)]
pub fn build_instances(
    layout: &Layout,
    view: &View,
    hover: Option<usize>,
    hl: [u8; 3],
    raster: &mut GlyphRaster,
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    shaper: &mut Shaper,
) -> Vec<Vec<Instance>> {
    let m = *shaper.metrics();
    let mut src = GpuRegions {
        raster,
        device,
        queue,
        shaper,
    };
    build_instances_with(layout, view, hover, hl, &m, &mut src)
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
pub fn build_instances_with(
    layout: &Layout,
    view: &View,
    hover: Option<usize>,
    hl: [u8; 3],
    m: &CellMetrics,
    src: &mut dyn RegionSource,
) -> Vec<Vec<Instance>> {
    let mut pages: Vec<Vec<Instance>> = (0..src.page_count().max(1)).map(|_| Vec::new()).collect();
    let rows = layout.rows.len();
    for (r, row) in layout.rows.iter().enumerate() {
        let Some(row_y) = view.row_y(rows, r, m.line_h) else {
            continue;
        };
        let base_x = m.shift as i32 + row.x0;
        for rect in &row.rects {
            let color = if hover.is_some() && rect.button == hover {
                hl
            } else {
                rect.color
            };
            pages[0].push(Instance {
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
            });
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
                if reg.page >= pages.len() {
                    pages.resize_with(reg.page + 1, Vec::new);
                }
                pages[reg.page].push(Instance {
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
                });
            }
        }
    }
    pages
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
            rows: vec![
                Row {
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
                },
                Row {
                    line: 1,
                    logical_start: true,
                    x0: 0,
                    width: 9,
                    clusters: vec![cluster(0, "c", 3, Some(1))],
                    rects: vec![],
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
        let pages = build_instances_with(&fake_layout(), &view, None, HL, &metrics(), &mut src);
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
        let pages = build_instances_with(&layout, &view, None, HL, &metrics(), &mut src);
        assert_eq!(pages[0][0].mode, 0);
        assert_eq!(pages[0][0].color, rgb(RED));
        // Hovering the rect's button still overrides it with `hl`.
        let hovered = build_instances_with(&layout, &view, Some(0), HL, &metrics(), &mut src);
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
        let plain = flat(&build_instances_with(
            &layout,
            &view,
            None,
            HL,
            &metrics(),
            &mut src,
        ));
        let hover0 = flat(&build_instances_with(
            &layout,
            &view,
            Some(0),
            HL,
            &metrics(),
            &mut src,
        ));
        let hover1 = flat(&build_instances_with(
            &layout,
            &view,
            Some(1),
            HL,
            &metrics(),
            &mut src,
        ));
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
        let pages = build_instances_with(&fake_layout(), &view, None, HL, &metrics(), &mut src);
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
        let only_last = flat(&build_instances_with(
            &fake_layout(),
            &view,
            None,
            HL,
            &metrics(),
            &mut src,
        ));
        assert_eq!(only_last.len(), 1, "one visible row: `c`");
        assert_eq!(only_last[0].rect, [3.0, 0.0, 9.0, 18.0]);
        let view = View {
            scroll_rows: 1,
            view_h: 19,
            strip_h: 19,
        };
        let first = flat(&build_instances_with(
            &fake_layout(),
            &view,
            None,
            HL,
            &metrics(),
            &mut src,
        ));
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
        let pages = build_instances_with(&fake_layout(), &view, None, HL, &metrics(), &mut Growing);
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
        let pages = build_instances(
            &laid, &view, None, HL, &mut raster, &device, &queue, &mut shaper,
        );
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
