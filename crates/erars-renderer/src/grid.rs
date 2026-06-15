use erars_ast::{Alignment, Value};
use erars_ui::{ConsoleLine, ConsoleLinePart};

use crate::font::FontCtx;
use crate::text::{CellShaper, PlacedGlyph};

/// A clickable region produced for a Button part.
#[derive(Clone, Debug)]
pub struct ButtonRegion {
    /// Pixel rectangle (x, y, w, h) in content space (before scroll).
    pub rect: [f32; 4],
    pub input_gen: u32,
    pub value: Value,
}

/// The full draw model for one ConsoleFrame at a given grid width.
pub struct Grid {
    pub glyphs: Vec<PlacedGlyph>,
    pub buttons: Vec<ButtonRegion>,
    /// Total content height in pixels.
    pub content_h: f32,
    pub grid_cols: usize,
}

impl Grid {
    /// Compute starting column for a line of `line_cols` cells under `align`.
    fn align_offset(align: Alignment, line_cols: usize, grid_cols: usize) -> usize {
        match align {
            Alignment::Left => 0,
            Alignment::Center => grid_cols.saturating_sub(line_cols) / 2,
            Alignment::Right => grid_cols.saturating_sub(line_cols),
        }
    }

    /// Number of times a fill char of `char_cells` repeats to fill grid_cols.
    fn fill_count(grid_cols: usize, char_cells: usize) -> usize {
        if char_cells == 0 {
            0
        } else {
            grid_cols / char_cells
        }
    }

    /// First pass over a line: total column count (used for alignment).
    /// A Line part is treated as filling the whole grid width.
    fn line_cols(ctx: &mut FontCtx, line: &ConsoleLine, grid_cols: usize) -> usize {
        let mut cols = 0;
        for part in &line.parts {
            match part {
                ConsoleLinePart::Text(s, style) => {
                    cols += CellShaper::shape_run(ctx, s, style, 0).cols;
                }
                ConsoleLinePart::Line(..) => {
                    cols = grid_cols.max(cols);
                }
                ConsoleLinePart::Button(parts, _, _) => {
                    for (s, style) in parts {
                        cols += CellShaper::shape_run(ctx, s, style, 0).cols;
                    }
                }
            }
        }
        cols
    }

    /// Build the full draw model for `lines` at the given grid width.
    /// `active_gen` enables buttons of that input generation; the button at
    /// `hovered_button` (index into the produced button list) is recolored
    /// with `hl_color`.
    pub fn build(
        ctx: &mut FontCtx,
        lines: &[ConsoleLine],
        grid_cols: usize,
        active_gen: Option<u32>,
        hovered_button: Option<usize>,
        hl_color: [u8; 3],
    ) -> Grid {
        let cell_w = ctx.cell_w;
        let cell_h = ctx.cell_h;
        let mut glyphs = Vec::new();
        let mut buttons = Vec::new();
        let mut y = 0.0_f32;

        for line in lines {
            let total = Self::line_cols(ctx, line, grid_cols);
            let mut col = Self::align_offset(line.align, total, grid_cols);

            for part in &line.parts {
                match part {
                    ConsoleLinePart::Text(s, style) => {
                        let run = CellShaper::shape_run(ctx, s, style, col);
                        for mut g in run.glyphs {
                            g.y_px += y;
                            glyphs.push(g);
                        }
                        col += run.cols;
                    }
                    ConsoleLinePart::Line(s, style) => {
                        let char_cells =
                            unicode_width::UnicodeWidthStr::width(s.as_str()).max(1);
                        let count = Self::fill_count(grid_cols, char_cells);
                        let filled = s.repeat(count);
                        let run = CellShaper::shape_run(ctx, &filled, style, 0);
                        for mut g in run.glyphs {
                            g.y_px += y;
                            glyphs.push(g);
                        }
                        col = grid_cols;
                    }
                    ConsoleLinePart::Button(parts, input_gen, value) => {
                        let start_col = col;
                        let enabled = active_gen == Some(*input_gen);
                        let btn_index = buttons.len();
                        let is_hover = enabled && hovered_button == Some(btn_index);
                        for (s, style) in parts {
                            let mut bstyle = style.clone();
                            if is_hover {
                                bstyle.color = erars_ui::Color(hl_color);
                            }
                            let run = CellShaper::shape_run(ctx, s, &bstyle, col);
                            for mut g in run.glyphs {
                                g.y_px += y;
                                glyphs.push(g);
                            }
                            col += run.cols;
                        }
                        let span = col - start_col;
                        buttons.push(ButtonRegion {
                            rect: [
                                start_col as f32 * cell_w,
                                y,
                                span as f32 * cell_w,
                                cell_h,
                            ],
                            input_gen: *input_gen,
                            value: value.clone(),
                        });
                    }
                }
            }

            y += cell_h;
        }

        Grid {
            glyphs,
            buttons,
            content_h: y,
            grid_cols,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use erars_ast::Value;
    use erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};

    fn plain_style() -> TextStyle {
        TextStyle {
            color: Color([255, 255, 255]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        }
    }

    fn text_line(s: &str) -> ConsoleLine {
        ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Text(s.to_string(), plain_style())],
        }
    }

    fn button_line() -> ConsoleLine {
        ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Button(
                vec![("[1]".to_string(), plain_style())],
                7,
                Value::Int(1),
            )],
        }
    }

    #[test]
    fn align_left() {
        assert_eq!(Grid::align_offset(Alignment::Left, 4, 30), 0);
    }

    #[test]
    fn align_center() {
        assert_eq!(Grid::align_offset(Alignment::Center, 4, 30), 13);
    }

    #[test]
    fn align_right() {
        assert_eq!(Grid::align_offset(Alignment::Right, 4, 30), 26);
    }

    #[test]
    fn align_clamps_when_overfull() {
        assert_eq!(Grid::align_offset(Alignment::Right, 40, 30), 0);
        assert_eq!(Grid::align_offset(Alignment::Center, 40, 30), 0);
    }

    #[test]
    fn fill_count_narrow() {
        assert_eq!(Grid::fill_count(30, 1), 30);
    }

    #[test]
    fn fill_count_wide() {
        assert_eq!(Grid::fill_count(30, 2), 15);
    }

    #[test]
    fn build_stacks_lines_vertically() {
        let mut ctx = FontCtx::new("", 18, 19);
        let lines = vec![text_line("ab"), text_line("cd")];
        let grid = Grid::build(&mut ctx, &lines, 30, None, None, [255, 255, 0]);
        assert_eq!(grid.content_h, 38.0);
        // second line glyphs are shifted down by at least one cell height
        let max_y = grid.glyphs.iter().map(|g| g.y_px).fold(0.0_f32, f32::max);
        assert!(max_y >= 19.0);
    }

    #[test]
    fn build_emits_button_region() {
        let mut ctx = FontCtx::new("", 18, 19);
        let lines = vec![button_line()];
        let grid = Grid::build(&mut ctx, &lines, 30, Some(7), None, [255, 255, 0]);
        assert_eq!(grid.buttons.len(), 1);
        assert_eq!(grid.buttons[0].input_gen, 7);
        // "[1]" is 3 narrow cells wide
        assert_eq!(grid.buttons[0].rect[2], 3.0 * ctx.cell_w);
    }
}
