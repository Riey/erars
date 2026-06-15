use cosmic_text::{Attrs, Buffer, CacheKey, Family, Metrics, Shaping};
use erars_ui::TextStyle;
use unicode_width::UnicodeWidthStr;

use crate::font::FontCtx;

/// One glyph placed at an integer grid column, ready to rasterize/draw.
#[derive(Clone, Debug, PartialEq)]
pub struct PlacedGlyph {
    /// Starting grid column (0-based) of the cluster this glyph belongs to.
    pub col: usize,
    /// Number of cells the cluster occupies (1 narrow, 2 wide).
    pub cell_span: usize,
    /// cosmic-text physical glyph cache key (font + glyph id + subpixel).
    pub cache_key: CacheKey,
    /// Pixel x of the glyph pen origin (col * cell_w + intra-cluster offset).
    pub x_px: f32,
    /// Pixel y of the run baseline relative to the line top.
    pub y_px: f32,
    /// RGB color from the style.
    pub color: [u8; 3],
}

/// Result of shaping a single styled run.
pub struct ShapedRun {
    pub glyphs: Vec<PlacedGlyph>,
    /// Total columns consumed by the run.
    pub cols: usize,
}

pub struct CellShaper;

impl CellShaper {
    /// Shape `text` with `style`, starting at grid column `start_col`.
    /// Returns grid-positioned glyphs and the total column count.
    ///
    /// cosmic-text shapes the run (applying font fallback per cluster); we
    /// ignore its advances and re-bin each cluster onto integer grid columns
    /// using unicode-width, so the result is a true monospace cell grid.
    pub fn shape_run(
        ctx: &mut FontCtx,
        text: &str,
        style: &TextStyle,
        start_col: usize,
    ) -> ShapedRun {
        if text.is_empty() {
            return ShapedRun {
                glyphs: Vec::new(),
                cols: 0,
            };
        }

        let font_size = ctx.font_size;
        let cell_w = ctx.cell_w;
        let metrics = Metrics::new(font_size, ctx.cell_h);
        // Empty style family means "use the configured default font".
        let family = if style.font_family.is_empty() {
            ctx.default_family.as_str()
        } else {
            style.font_family.as_str()
        };
        let attrs = if family.is_empty() {
            Attrs::new().family(Family::Monospace)
        } else {
            Attrs::new().family(Family::Name(family))
        };

        let mut buffer = Buffer::new(&mut ctx.font_system, metrics);
        buffer.set_text(&mut ctx.font_system, text, attrs, Shaping::Advanced);
        buffer.shape_until_scroll(&mut ctx.font_system, false);

        let color = style.color.0;
        let mut glyphs = Vec::new();
        let mut col = start_col;

        for run in buffer.layout_runs() {
            let baseline = run.line_y;
            let g = run.glyphs;
            // Group glyphs by their source cluster (byte offset `start`).
            let mut i = 0;
            while i < g.len() {
                let cluster_start = g[i].start;
                let mut j = i;
                while j < g.len() && g[j].start == cluster_start {
                    j += 1;
                }
                let cluster_end = g[i..j].iter().map(|x| x.end).max().unwrap_or(cluster_start);
                let cluster_str = &text[cluster_start..cluster_end.min(text.len())];
                // ERA games are CJK: East-Asian "Ambiguous" characters (…, ※,
                // Greek/Cyrillic, box-drawing, …) are rendered full-width by CJK
                // monospace fonts, so use the CJK width table (ambiguous = 2).
                // Using the narrow table would allot 1 cell and the wide glyph
                // would overflow into its neighbour (e.g. merged ellipsis dots).
                let cells = cluster_str.width_cjk().max(1);

                // Anchor the whole cluster at its cell origin. We deliberately
                // ignore cosmic-text's absolute layout x (which accumulates the
                // font's own, possibly proportional, advances) so the grid pitch
                // is exactly `cell_w` regardless of the resolved fallback font —
                // this is what keeps text columns aligned. Only the offset of a
                // glyph *within* its cluster (combining marks) is preserved.
                let cell_x = col as f32 * cell_w;
                let base_x = g[i].physical((0.0, 0.0), 1.0).x as f32;
                for glyph in &g[i..j] {
                    let physical = glyph.physical((0.0, 0.0), 1.0);
                    glyphs.push(PlacedGlyph {
                        col,
                        cell_span: cells,
                        cache_key: physical.cache_key,
                        x_px: cell_x + (physical.x as f32 - base_x),
                        y_px: baseline,
                        color,
                    });
                }
                col += cells;
                i = j;
            }
        }

        ShapedRun {
            glyphs,
            cols: col - start_col,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use erars_ui::{Color, FontStyle};

    fn style() -> TextStyle {
        TextStyle {
            color: Color([255, 255, 255]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        }
    }

    fn cols_of(text: &str) -> Vec<usize> {
        let mut ctx = FontCtx::new("", 18, 19);
        let run = CellShaper::shape_run(&mut ctx, text, &style(), 0);
        // distinct cluster starting columns in order
        let mut cols = Vec::new();
        for g in &run.glyphs {
            if cols.last() != Some(&g.col) {
                cols.push(g.col);
            }
        }
        cols
    }

    #[test]
    fn ascii_is_one_cell_each() {
        let mut ctx = FontCtx::new("", 18, 19);
        let run = CellShaper::shape_run(&mut ctx, "abc", &style(), 0);
        assert_eq!(run.cols, 3);
        assert_eq!(cols_of("abc"), vec![0, 1, 2]);
    }

    #[test]
    fn cjk_is_two_cells_each() {
        let mut ctx = FontCtx::new("", 18, 19);
        let run = CellShaper::shape_run(&mut ctx, "한글", &style(), 0);
        assert_eq!(run.cols, 4);
        assert_eq!(cols_of("한글"), vec![0, 2]);
    }

    #[test]
    fn ambiguous_width_chars_are_two_cells() {
        // U+2026 HORIZONTAL ELLIPSIS is East-Asian "Ambiguous": CJK fonts draw
        // it full-width, so it must occupy two cells (else the dots overlap).
        let mut ctx = FontCtx::new("", 18, 19);
        assert_eq!(CellShaper::shape_run(&mut ctx, "…", &style(), 0).cols, 2);
        assert_eq!(CellShaper::shape_run(&mut ctx, "……", &style(), 0).cols, 4);
        assert_eq!(cols_of("……"), vec![0, 2]);
    }

    #[test]
    fn mixed_scripts_align_to_grid() {
        let mut ctx = FontCtx::new("", 18, 19);
        let run = CellShaper::shape_run(&mut ctx, "a한b", &style(), 0);
        assert_eq!(run.cols, 4);
        assert_eq!(cols_of("a한b"), vec![0, 1, 3]);
    }

    #[test]
    fn start_col_offsets_columns() {
        let mut ctx = FontCtx::new("", 18, 19);
        let run = CellShaper::shape_run(&mut ctx, "ab", &style(), 5);
        assert_eq!(run.cols, 2);
        assert_eq!(run.glyphs[0].col, 5);
    }

    /// Alignment guarantee: each single-glyph cluster sits exactly on its grid
    /// column (`col * cell_w`), so the font's advances never shift columns.
    /// Runs with whatever fallback fonts FontCtx loads (incl. proportional
    /// Windows CJK fonts), which is the case the grid must survive.
    #[test]
    fn glyphs_land_exactly_on_grid() {
        let mut ctx = FontCtx::new("", 18, 19);
        let cell_w = ctx.cell_w;
        for text in ["abcdef", "한글한글", "a한b글c", "2022年10月08日"] {
            let run = CellShaper::shape_run(&mut ctx, text, &style(), 0);
            for g in &run.glyphs {
                let expected = g.col as f32 * cell_w;
                assert!(
                    (g.x_px - expected).abs() < 0.01,
                    "{text:?}: glyph at col {} has x_px={} expected {}",
                    g.col,
                    g.x_px,
                    expected
                );
            }
        }
    }
}
