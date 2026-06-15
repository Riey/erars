# erars-renderer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `erars-renderer`, a GPU-powered, OS-independent erars frontend that renders the console as a true fixed-width (monospace) cell grid with cross-script font fallback, replacing `erars-iced`.

**Architecture:** A standalone crate using winit (window/input), wgpu (GPU), and cosmic-text (shaping + fontdb fallback + rasterization). Text is shaped per run by cosmic-text (where fallback happens), then each grapheme cluster is re-binned onto integer grid columns via `unicode-width` (wide chars = 2 cells). A custom wgpu glyph atlas draws everything as one batched instanced-quad pass. The VM thread wakes the render loop through the proxy `notify` callback wired to a winit `EventLoopProxy`.

**Tech Stack:** Rust, winit 0.30, wgpu 0.19, cosmic-text 0.12, unicode-width 0.1, etagere 0.2, bytemuck 1, pollster 0.3, fontdb (via cosmic-text); existing crates erars-ast/ui/vm/loader/proxy-system/compiler.

**Reference:** Design spec at `docs/superpowers/specs/2026-06-15-erars-renderer-design.md`.

---

## File Structure

- `crates/erars-renderer/Cargo.toml` — crate manifest.
- `crates/erars-renderer/assets/NotoSansMono-Regular.ttf` — bundled Latin monospace fallback (OFL).
- `crates/erars-renderer/src/main.rs` — CLI, logging, VM thread bootstrap, event loop.
- `crates/erars-renderer/src/font.rs` — `FontCtx`: FontSystem + bundled fallback + cell metrics.
- `crates/erars-renderer/src/text.rs` — `CellShaper`, `PlacedGlyph`: run → grid-positioned glyphs.
- `crates/erars-renderer/src/grid.rs` — `Grid`, draw-list + button-region builder.
- `crates/erars-renderer/src/atlas.rs` — `GlyphAtlas`: wgpu texture + etagere packer + SwashCache raster.
- `crates/erars-renderer/src/gpu.rs` — `GpuContext`: wgpu device/pipeline/render.
- `crates/erars-renderer/src/app.rs` — `App: ApplicationHandler`: controller wiring it all together.
- `crates/erars-renderer/src/shader.wgsl` — instanced quad shader.
- Workspace `Cargo.toml` — add member, remove `erars-iced`.

Modules are introduced bottom-up (leaf logic first, GPU/app last) so each task compiles and tests on its own.

---

## Task 1: Scaffold the crate and wire it into the workspace

**Files:**
- Create: `crates/erars-renderer/Cargo.toml`
- Create: `crates/erars-renderer/src/main.rs`
- Modify: `Cargo.toml` (workspace members)

- [ ] **Step 1: Create the crate manifest**

`crates/erars-renderer/Cargo.toml`:

```toml
[package]
name = "erars-renderer"
version = "0.5.0"
edition = "2021"
license = "GPL-3.0-or-later"

[dependencies]
erars-ast = { path = "../erars-ast" }
erars-compiler = { path = "../erars-compiler" }
erars-ui = { path = "../erars-ui" }
erars-vm = { path = "../erars-vm" }
erars-loader = { path = "../erars-loader", features = ["multithread"] }
erars-proxy-system = { path = "../erars-proxy-system" }

flume.workspace = true
clap = { version = "4", features = ["derive"] }
flexi_logger = "0.29.0"
log-panics = "2.1.0"
log = "0.4.17"
mimalloc = { version = "0.1.31", default-features = false }

winit = "0.30"
wgpu = "0.19"
pollster = "0.3"
bytemuck = { version = "1", features = ["derive"] }
etagere = "0.2"
unicode-width = "0.1"

[dependencies.cosmic-text]
version = "0.12.1"
features = ["monospace_fallback", "shape-run-cache"]
```

- [ ] **Step 2: Create a minimal `main.rs` that compiles**

`crates/erars-renderer/src/main.rs`:

```rust
fn main() {
    println!("erars-renderer");
}
```

- [ ] **Step 3: Register the crate, drop erars-iced from the workspace**

In `Cargo.toml`, change the `members` list: replace the `"./crates/erars-iced",` line with `"./crates/erars-renderer",` (keep all other members).

- [ ] **Step 4: Verify it builds**

Run: `cargo build -p erars-renderer`
Expected: compiles successfully (downloads winit/wgpu/etc on first run).

- [ ] **Step 5: Commit**

```bash
git add crates/erars-renderer/Cargo.toml crates/erars-renderer/src/main.rs Cargo.toml
git commit -m "feat(renderer): scaffold erars-renderer crate"
```

---

## Task 2: Bundle the fallback font and build `FontCtx` with cell metrics

**Files:**
- Create: `crates/erars-renderer/assets/NotoSansMono-Regular.ttf`
- Create: `crates/erars-renderer/src/font.rs`
- Modify: `crates/erars-renderer/src/main.rs`

- [ ] **Step 1: Vendor the bundled font**

```bash
mkdir -p crates/erars-renderer/assets
cp /usr/share/fonts/noto/NotoSansMono-Regular.ttf crates/erars-renderer/assets/NotoSansMono-Regular.ttf
```

(If that path is absent, copy any OFL/redistributable monospace .ttf to that exact destination path; Noto Sans Mono is OFL-licensed and safe to redistribute.)

- [ ] **Step 2: Write the failing test for cell metrics**

`crates/erars-renderer/src/font.rs`:

```rust
use cosmic_text::{fontdb, Attrs, Buffer, Family, FontSystem, Metrics, Shaping};

/// Owns the cosmic-text FontSystem plus the bundled fallback, and the
/// fixed cell size derived from the default monospace font.
pub struct FontCtx {
    pub font_system: FontSystem,
    /// Width of one grid cell in pixels (advance of an ASCII glyph).
    pub cell_w: f32,
    /// Height of one grid cell in pixels (config line_height).
    pub cell_h: f32,
    /// Font pixel size.
    pub font_size: f32,
    /// Default family name string.
    pub default_family: String,
}

/// Bundled Latin monospace fallback, always available.
const BUNDLED_FONT: &[u8] = include_bytes!("../assets/NotoSansMono-Regular.ttf");

impl FontCtx {
    pub fn new(default_family: &str, font_size: u32, line_height: u32) -> Self {
        let mut db = fontdb::Database::new();
        db.load_system_fonts();
        db.load_font_data(BUNDLED_FONT.to_vec());

        let locale = sys_locale::get_locale().unwrap_or_else(|| String::from("en-US"));
        let mut font_system = FontSystem::new_with_locale_and_db(locale, db);

        let font_size = font_size as f32;
        let cell_w = measure_cell_w(&mut font_system, default_family, font_size);

        Self {
            font_system,
            cell_w,
            cell_h: line_height as f32,
            font_size,
            default_family: default_family.to_string(),
        }
    }
}

/// Measure the advance of a representative ASCII glyph ("0") at this size.
fn measure_cell_w(font_system: &mut FontSystem, family: &str, font_size: f32) -> f32 {
    let mut buffer = Buffer::new(font_system, Metrics::new(font_size, font_size));
    let attrs = if family.is_empty() {
        Attrs::new().family(Family::Monospace)
    } else {
        Attrs::new().family(Family::Name(family))
    };
    buffer.set_text(font_system, "0", attrs, Shaping::Advanced);
    buffer.shape_until_scroll(font_system, false);
    let mut w = 0.0_f32;
    for run in buffer.layout_runs() {
        for glyph in run.glyphs.iter() {
            w += glyph.w;
        }
    }
    if w <= 0.0 {
        font_size * 0.6 // fallback guess
    } else {
        w
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cell_metrics_are_positive() {
        let ctx = FontCtx::new("", 18, 19);
        assert!(ctx.cell_w > 0.0, "cell_w must be positive, got {}", ctx.cell_w);
        assert_eq!(ctx.cell_h, 19.0);
        assert_eq!(ctx.font_size, 18.0);
    }
}
```

- [ ] **Step 3: Add the `sys-locale` dependency**

In `crates/erars-renderer/Cargo.toml` under `[dependencies]` add:

```toml
sys-locale = "0.3"
```

And declare the module + test entry in `main.rs` by adding at the top:

```rust
mod font;
```

(Temporarily add `#[allow(dead_code)]` above `mod font;` so the unused warnings don't block; it will be used in later tasks.)

- [ ] **Step 4: Run the test (expect fail → then pass)**

Run: `cargo test -p erars-renderer font::tests::cell_metrics_are_positive`
Expected first run before font.rs existed: compile error. After adding the code: PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/erars-renderer/assets crates/erars-renderer/src/font.rs crates/erars-renderer/src/main.rs crates/erars-renderer/Cargo.toml
git commit -m "feat(renderer): FontCtx with bundled fallback and cell metrics"
```

---

## Task 3: `CellShaper` — map a styled run onto grid columns

**Files:**
- Create: `crates/erars-renderer/src/text.rs`
- Modify: `crates/erars-renderer/src/main.rs`

- [ ] **Step 1: Write the failing tests**

`crates/erars-renderer/src/text.rs`:

```rust
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
    pub fn shape_run(
        ctx: &mut FontCtx,
        text: &str,
        style: &TextStyle,
        start_col: usize,
    ) -> ShapedRun {
        let font_size = ctx.font_size;
        let cell_w = ctx.cell_w;
        let metrics = Metrics::new(font_size, ctx.cell_h);
        let family = style.font_family.as_str();
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
            // Group glyphs by their source cluster (byte offset `start`).
            let mut i = 0;
            let g = run.glyphs;
            while i < g.len() {
                let cluster_start = g[i].start;
                let mut j = i;
                while j < g.len() && g[j].start == cluster_start {
                    j += 1;
                }
                let cluster_end = g[i..j].iter().map(|x| x.end).max().unwrap_or(cluster_start);
                let cluster_str = &text[cluster_start..cluster_end.min(text.len())];
                let cells = cluster_str.width().max(1);

                let cell_x = col as f32 * cell_w;
                for glyph in &g[i..j] {
                    let physical = glyph.physical((0.0, 0.0), 1.0);
                    glyphs.push(PlacedGlyph {
                        col,
                        cell_span: cells,
                        cache_key: physical.cache_key,
                        x_px: cell_x + physical.x as f32,
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
}
```

- [ ] **Step 2: Declare the module**

In `main.rs` add `mod text;` (keep the temporary `#[allow(dead_code)]` pattern on new modules until they are wired in).

- [ ] **Step 3: Run the tests, expect fail then pass**

Run: `cargo test -p erars-renderer text::`
Expected: the four tests PASS. If `한글` measures as 1 cell, confirm `unicode-width` is treating Hangul as Wide (it does for syllables U+AC00–U+D7A3).

- [ ] **Step 4: Commit**

```bash
git add crates/erars-renderer/src/text.rs crates/erars-renderer/src/main.rs
git commit -m "feat(renderer): CellShaper maps runs to fixed grid columns"
```

---

## Task 4: `Grid` — build per-line draw lists with alignment, Line-fill, buttons

**Files:**
- Create: `crates/erars-renderer/src/grid.rs`
- Modify: `crates/erars-renderer/src/main.rs`

- [ ] **Step 1: Write the failing tests**

`crates/erars-renderer/src/grid.rs`:

```rust
use erars_ast::{Alignment, Value};
use erars_ui::{ConsoleLine, ConsoleLinePart, TextStyle};

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
    fn line_cols(ctx: &mut FontCtx, line: &ConsoleLine, grid_cols: usize) -> usize {
        let mut cols = 0;
        for part in &line.parts {
            match part {
                ConsoleLinePart::Text(s, style) => {
                    cols += CellShaper::shape_run(ctx, s, style, 0).cols;
                }
                ConsoleLinePart::Line(s, _) => {
                    let char_cells = unicode_width::UnicodeWidthStr::width(s.as_str()).max(1);
                    cols = grid_cols.max(cols);
                    let _ = char_cells;
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
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
```

- [ ] **Step 2: Declare the module**

In `main.rs` add `mod grid;`.

- [ ] **Step 3: Run the tests**

Run: `cargo test -p erars-renderer grid::`
Expected: all six PASS.

- [ ] **Step 4: Commit**

```bash
git add crates/erars-renderer/src/grid.rs crates/erars-renderer/src/main.rs
git commit -m "feat(renderer): grid alignment and fill-count logic"
```

---

## Task 5: `Grid::build` — assemble glyphs and button regions for a frame

**Files:**
- Modify: `crates/erars-renderer/src/grid.rs`

- [ ] **Step 1: Write the failing test**

Add to `grid.rs` inside `impl Grid` (above the `#[cfg(test)]` block):

```rust
    /// Build the full draw model for `lines` at the given grid width and
    /// cell height. `hl_color` highlights the enabled button under the cursor.
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
```

Add this test to the `tests` module:

```rust
    use erars_ast::Value;
    use erars_ui::{Color, FontStyle};

    fn text_line(s: &str) -> ConsoleLine {
        ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Text(
                s.to_string(),
                TextStyle {
                    color: Color([255, 255, 255]),
                    font_family: "".into(),
                    font_style: FontStyle::NORMAL,
                },
            )],
        }
    }

    fn button_line() -> ConsoleLine {
        ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Button(
                vec![(
                    "[1]".to_string(),
                    TextStyle {
                        color: Color([255, 255, 255]),
                        font_family: "".into(),
                        font_style: FontStyle::NORMAL,
                    },
                )],
                7,
                Value::Int(1),
            )],
        }
    }

    #[test]
    fn build_stacks_lines_vertically() {
        let mut ctx = FontCtx::new("", 18, 19);
        let lines = vec![text_line("ab"), text_line("cd")];
        let grid = Grid::build(&mut ctx, &lines, 30, None, None, [255, 255, 0]);
        assert_eq!(grid.content_h, 38.0);
        // second line glyphs are shifted down by one cell height
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
```

- [ ] **Step 2: Run the tests**

Run: `cargo test -p erars-renderer grid::`
Expected: all tests PASS.

- [ ] **Step 3: Commit**

```bash
git add crates/erars-renderer/src/grid.rs
git commit -m "feat(renderer): Grid::build assembles glyphs and button regions"
```

---

## Task 6: `GlyphAtlas` — rasterize via SwashCache into a wgpu texture

**Files:**
- Create: `crates/erars-renderer/src/atlas.rs`
- Modify: `crates/erars-renderer/src/main.rs`

This task needs a wgpu device. The unit test creates a headless device.

- [ ] **Step 1: Write the atlas with a device-backed smoke test**

`crates/erars-renderer/src/atlas.rs`:

```rust
use std::collections::HashMap;

use cosmic_text::{CacheKey, FontSystem, SwashCache, SwashContent};
use etagere::{size2, AllocId, AtlasAllocator};

/// UV + placement info for a rasterized glyph.
#[derive(Clone, Copy, Debug)]
pub struct AtlasRegion {
    /// UV rect in [0,1]: x, y, w, h.
    pub uv: [f32; 4],
    /// Glyph bitmap size in px.
    pub size: [f32; 2],
    /// Left/top bearing from the pen origin (placement offsets).
    pub offset: [f32; 2],
    /// true if RGBA (color) glyph, false if alpha mask.
    pub color: bool,
    #[allow(dead_code)]
    alloc: AllocId,
}

const ATLAS_SIZE: u32 = 2048;

/// A single-page growable glyph atlas backed by an RGBA wgpu texture.
/// Alpha-mask glyphs are stored as white with the coverage in the alpha
/// channel, so one texture serves both mask and color glyphs.
pub struct GlyphAtlas {
    allocator: AtlasAllocator,
    map: HashMap<CacheKey, Option<AtlasRegion>>,
    pub texture: wgpu::Texture,
    pub view: wgpu::TextureView,
    size: u32,
}

impl GlyphAtlas {
    pub fn new(device: &wgpu::Device) -> Self {
        let size = ATLAS_SIZE;
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("glyph-atlas"),
            size: wgpu::Extent3d {
                width: size,
                height: size,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8Unorm,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
        Self {
            allocator: AtlasAllocator::new(size2(size as i32, size as i32)),
            map: HashMap::new(),
            texture,
            view,
            size,
        }
    }

    /// Get (rasterizing on demand) the atlas region for a glyph.
    /// Returns None for empty glyphs (e.g. space).
    pub fn get(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        font_system: &mut FontSystem,
        swash: &mut SwashCache,
        key: CacheKey,
    ) -> Option<AtlasRegion> {
        if let Some(cached) = self.map.get(&key) {
            return *cached;
        }
        let region = self.rasterize(device, queue, font_system, swash, key);
        self.map.insert(key, region);
        region
    }

    fn rasterize(
        &mut self,
        _device: &wgpu::Device,
        queue: &wgpu::Queue,
        font_system: &mut FontSystem,
        swash: &mut SwashCache,
        key: CacheKey,
    ) -> Option<AtlasRegion> {
        let image = swash.get_image_uncached(font_system, key)?;
        let w = image.placement.width;
        let h = image.placement.height;
        if w == 0 || h == 0 {
            return None;
        }
        let is_color = matches!(image.content, SwashContent::Color);

        // Convert to RGBA8.
        let mut rgba = vec![0u8; (w * h * 4) as usize];
        match image.content {
            SwashContent::Mask => {
                for (i, a) in image.data.iter().enumerate() {
                    rgba[i * 4] = 255;
                    rgba[i * 4 + 1] = 255;
                    rgba[i * 4 + 2] = 255;
                    rgba[i * 4 + 3] = *a;
                }
            }
            SwashContent::Color => {
                rgba.copy_from_slice(&image.data);
            }
            SwashContent::SubpixelMask => {
                for (i, chunk) in image.data.chunks_exact(4).enumerate() {
                    rgba[i * 4] = 255;
                    rgba[i * 4 + 1] = 255;
                    rgba[i * 4 + 2] = 255;
                    rgba[i * 4 + 3] = chunk[3];
                }
            }
        }

        let alloc = self
            .allocator
            .allocate(size2(w as i32 + 1, h as i32 + 1))?;
        let rect = alloc.rectangle;
        let (x, y) = (rect.min.x as u32, rect.min.y as u32);

        queue.write_texture(
            wgpu::ImageCopyTexture {
                texture: &self.texture,
                mip_level: 0,
                origin: wgpu::Origin3d { x, y, z: 0 },
                aspect: wgpu::TextureAspect::All,
            },
            &rgba,
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(w * 4),
                rows_per_image: Some(h),
            },
            wgpu::Extent3d {
                width: w,
                height: h,
                depth_or_array_layers: 1,
            },
        );

        let s = self.size as f32;
        Some(AtlasRegion {
            uv: [x as f32 / s, y as f32 / s, w as f32 / s, h as f32 / s],
            size: [w as f32, h as f32],
            offset: [image.placement.left as f32, image.placement.top as f32],
            color: is_color,
            alloc: alloc.id,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::FontCtx;
    use crate::text::CellShaper;
    use erars_ui::{Color, FontStyle, TextStyle};

    fn headless_device() -> Option<(wgpu::Device, wgpu::Queue)> {
        let instance = wgpu::Instance::default();
        let adapter = pollster::block_on(instance.request_adapter(
            &wgpu::RequestAdapterOptions::default(),
        ))?;
        pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor::default(), None)).ok()
    }

    #[test]
    fn rasterizes_a_glyph() {
        let Some((device, queue)) = headless_device() else {
            eprintln!("no GPU adapter; skipping");
            return;
        };
        let mut ctx = FontCtx::new("", 18, 19);
        let mut swash = SwashCache::new();
        let mut atlas = GlyphAtlas::new(&device);
        let style = TextStyle {
            color: Color([255, 255, 255]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        };
        let run = CellShaper::shape_run(&mut ctx, "A", &style, 0);
        let key = run.glyphs[0].cache_key;
        let region = atlas.get(&device, &queue, &mut ctx.font_system, &mut swash, key);
        assert!(region.is_some(), "glyph 'A' should rasterize to a region");
    }
}
```

- [ ] **Step 2: Declare the module**

In `main.rs` add `mod atlas;`.

- [ ] **Step 3: Run the test**

Run: `cargo test -p erars-renderer atlas::`
Expected: PASS (or a printed "no GPU adapter; skipping" on a headless box without any adapter — llvmpipe usually provides one).

- [ ] **Step 4: Commit**

```bash
git add crates/erars-renderer/src/atlas.rs crates/erars-renderer/src/main.rs
git commit -m "feat(renderer): glyph atlas with SwashCache rasterization"
```

---

## Task 7: WGSL shader + `GpuContext` rendering pipeline

**Files:**
- Create: `crates/erars-renderer/src/shader.wgsl`
- Create: `crates/erars-renderer/src/gpu.rs`
- Modify: `crates/erars-renderer/src/main.rs`

- [ ] **Step 1: Write the shader**

`crates/erars-renderer/src/shader.wgsl`:

```wgsl
struct Globals {
    screen: vec2<f32>,
    _pad: vec2<f32>,
};
@group(0) @binding(0) var<uniform> globals: Globals;
@group(0) @binding(1) var atlas_tex: texture_2d<f32>;
@group(0) @binding(2) var atlas_smp: sampler;

struct Instance {
    @location(0) rect: vec4<f32>,   // x, y, w, h in pixels
    @location(1) uv: vec4<f32>,     // u, v, uw, vh
    @location(2) color: vec4<f32>,  // rgba 0..1
    @location(3) mode: u32,         // 0 solid, 1 alpha-mask, 2 rgba
};

struct VsOut {
    @builtin(position) pos: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) color: vec4<f32>,
    @location(2) @interpolate(flat) mode: u32,
};

@vertex
fn vs_main(@builtin(vertex_index) vid: u32, inst: Instance) -> VsOut {
    // Two triangles for a unit quad.
    var corners = array<vec2<f32>, 6>(
        vec2<f32>(0.0, 0.0), vec2<f32>(1.0, 0.0), vec2<f32>(0.0, 1.0),
        vec2<f32>(0.0, 1.0), vec2<f32>(1.0, 0.0), vec2<f32>(1.0, 1.0),
    );
    let c = corners[vid];
    let px = inst.rect.xy + c * inst.rect.zw;
    // pixel -> NDC (y down to y up)
    let ndc = vec2<f32>(
        px.x / globals.screen.x * 2.0 - 1.0,
        1.0 - px.y / globals.screen.y * 2.0,
    );
    var out: VsOut;
    out.pos = vec4<f32>(ndc, 0.0, 1.0);
    out.uv = inst.uv.xy + c * inst.uv.zw;
    out.color = inst.color;
    out.mode = inst.mode;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    if (in.mode == 0u) {
        return in.color;
    } else if (in.mode == 1u) {
        let a = textureSample(atlas_tex, atlas_smp, in.uv).a;
        return vec4<f32>(in.color.rgb, in.color.a * a);
    } else {
        return textureSample(atlas_tex, atlas_smp, in.uv);
    }
}
```

- [ ] **Step 2: Write `GpuContext`**

`crates/erars-renderer/src/gpu.rs`:

```rust
use bytemuck::{Pod, Zeroable};
use wgpu::util::DeviceExt;

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
pub struct Instance {
    pub rect: [f32; 4],
    pub uv: [f32; 4],
    pub color: [f32; 4],
    pub mode: u32,
    pub _pad: [u32; 3],
}

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
struct Globals {
    screen: [f32; 2],
    _pad: [f32; 2],
}

pub struct GpuContext {
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
    surface: wgpu::Surface<'static>,
    config: wgpu::SurfaceConfiguration,
    pipeline: wgpu::RenderPipeline,
    globals_buf: wgpu::Buffer,
    sampler: wgpu::Sampler,
    bind_group_layout: wgpu::BindGroupLayout,
}

impl GpuContext {
    pub fn new(
        instance: &wgpu::Instance,
        surface: wgpu::Surface<'static>,
        width: u32,
        height: u32,
    ) -> Self {
        let adapter = pollster::block_on(instance.request_adapter(
            &wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::LowPower,
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
            },
        ))
        .expect("no suitable GPU adapter found");

        let (device, queue) = pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                label: Some("erars-renderer"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::downlevel_defaults(),
            },
            None,
        ))
        .expect("failed to create wgpu device");

        let caps = surface.get_capabilities(&adapter);
        let format = caps
            .formats
            .iter()
            .copied()
            .find(|f| f.is_srgb())
            .unwrap_or(caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format,
            width: width.max(1),
            height: height.max(1),
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("quad-shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("shader.wgsl").into()),
        });

        let bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("globals-bgl"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::VERTEX,
                        ty: wgpu::BindingType::Buffer {
                            ty: wgpu::BufferBindingType::Uniform,
                            has_dynamic_offset: false,
                            min_binding_size: None,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 2,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
            });

        let pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("pl"),
                bind_group_layouts: &[&bind_group_layout],
                push_constant_ranges: &[],
            });

        let instance_layout = wgpu::VertexBufferLayout {
            array_stride: std::mem::size_of::<Instance>() as wgpu::BufferAddress,
            step_mode: wgpu::VertexStepMode::Instance,
            attributes: &wgpu::vertex_attr_array![
                0 => Float32x4, // rect
                1 => Float32x4, // uv
                2 => Float32x4, // color
                3 => Uint32,    // mode
            ],
        };

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("quad-pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[instance_layout],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            multiview: None,
        });

        let globals_buf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("globals"),
            contents: bytemuck::bytes_of(&Globals {
                screen: [width as f32, height as f32],
                _pad: [0.0; 2],
            }),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        Self {
            device,
            queue,
            surface,
            config,
            pipeline,
            globals_buf,
            sampler,
            bind_group_layout,
        }
    }

    pub fn resize(&mut self, width: u32, height: u32) {
        self.config.width = width.max(1);
        self.config.height = height.max(1);
        self.surface.configure(&self.device, &self.config);
        self.queue.write_buffer(
            &self.globals_buf,
            0,
            bytemuck::bytes_of(&Globals {
                screen: [self.config.width as f32, self.config.height as f32],
                _pad: [0.0; 2],
            }),
        );
    }

    pub fn size(&self) -> (u32, u32) {
        (self.config.width, self.config.height)
    }

    /// Render one frame: clear to `bg`, draw `instances` against `atlas_view`.
    pub fn render(
        &mut self,
        atlas_view: &wgpu::TextureView,
        instances: &[Instance],
        bg: [f32; 3],
    ) {
        let frame = match self.surface.get_current_texture() {
            Ok(f) => f,
            Err(wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated) => {
                self.surface.configure(&self.device, &self.config);
                return;
            }
            Err(e) => {
                log::error!("surface error: {e:?}");
                return;
            }
        };
        let view = frame
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("bg"),
            layout: &self.bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: self.globals_buf.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::TextureView(atlas_view),
                },
                wgpu::BindGroupEntry {
                    binding: 2,
                    resource: wgpu::BindingResource::Sampler(&self.sampler),
                },
            ],
        });

        let instance_buf =
            self.device
                .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("instances"),
                    contents: bytemuck::cast_slice(instances),
                    usage: wgpu::BufferUsages::VERTEX,
                });

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
        {
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("main-pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &view,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(wgpu::Color {
                            r: bg[0] as f64 / 255.0,
                            g: bg[1] as f64 / 255.0,
                            b: bg[2] as f64 / 255.0,
                            a: 1.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            if !instances.is_empty() {
                pass.set_pipeline(&self.pipeline);
                pass.set_bind_group(0, &bind_group, &[]);
                pass.set_vertex_buffer(0, instance_buf.slice(..));
                pass.draw(0..6, 0..instances.len() as u32);
            }
        }
        self.queue.submit(Some(encoder.finish()));
        frame.present();
    }
}
```

- [ ] **Step 3: Declare the module**

In `main.rs` add `mod gpu;`.

- [ ] **Step 4: Verify it builds**

Run: `cargo build -p erars-renderer`
Expected: compiles. (No unit test here; the pipeline is exercised by the app in Task 9.)

- [ ] **Step 5: Commit**

```bash
git add crates/erars-renderer/src/gpu.rs crates/erars-renderer/src/shader.wgsl crates/erars-renderer/src/main.rs
git commit -m "feat(renderer): wgpu quad pipeline and shader"
```

---

## Task 8: Instance assembly — turn a `Grid` + atlas into draw instances

**Files:**
- Create: `crates/erars-renderer/src/draw.rs`
- Modify: `crates/erars-renderer/src/main.rs`

- [ ] **Step 1: Write the assembler with a headless test**

`crates/erars-renderer/src/draw.rs`:

```rust
use cosmic_text::{FontSystem, SwashCache};

use crate::atlas::GlyphAtlas;
use crate::grid::Grid;
use crate::gpu::Instance;

/// Build GPU instances for a grid: optional button-hover backgrounds plus
/// all glyph quads. `scroll_y` is subtracted from content-space y.
pub fn build_instances(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    font_system: &mut FontSystem,
    swash: &mut SwashCache,
    atlas: &mut GlyphAtlas,
    grid: &Grid,
    scroll_y: f32,
) -> Vec<Instance> {
    let mut out = Vec::with_capacity(grid.glyphs.len() + grid.buttons.len());

    for g in &grid.glyphs {
        let Some(region) =
            atlas.get(device, queue, font_system, swash, g.cache_key)
        else {
            continue;
        };
        let mode = if region.color { 2u32 } else { 1u32 };
        out.push(Instance {
            rect: [
                g.x_px + region.offset[0],
                g.y_px - region.offset[1] - scroll_y,
                region.size[0],
                region.size[1],
            ],
            uv: region.uv,
            color: [
                g.color[0] as f32 / 255.0,
                g.color[1] as f32 / 255.0,
                g.color[2] as f32 / 255.0,
                1.0,
            ],
            mode,
            _pad: [0; 3],
        });
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::FontCtx;
    use erars_ast::Alignment;
    use erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};

    fn headless() -> Option<(wgpu::Device, wgpu::Queue)> {
        let instance = wgpu::Instance::default();
        let adapter = pollster::block_on(
            instance.request_adapter(&wgpu::RequestAdapterOptions::default()),
        )?;
        pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor::default(), None)).ok()
    }

    #[test]
    fn produces_one_instance_per_visible_glyph() {
        let Some((device, queue)) = headless() else {
            eprintln!("no GPU; skipping");
            return;
        };
        let mut ctx = FontCtx::new("", 18, 19);
        let mut swash = SwashCache::new();
        let mut atlas = GlyphAtlas::new(&device);
        let line = ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Text(
                "abc".into(),
                TextStyle {
                    color: Color([255, 255, 255]),
                    font_family: "".into(),
                    font_style: FontStyle::NORMAL,
                },
            )],
        };
        let grid = Grid::build(&mut ctx, &[line], 30, None, None, [255, 255, 0]);
        let instances = build_instances(
            &device, &queue, &mut ctx.font_system, &mut swash, &mut atlas, &grid, 0.0,
        );
        assert_eq!(instances.len(), 3);
    }
}
```

- [ ] **Step 2: Declare the module**

In `main.rs` add `mod draw;`.

- [ ] **Step 3: Run the test**

Run: `cargo test -p erars-renderer draw::`
Expected: PASS (or skip without GPU).

- [ ] **Step 4: Commit**

```bash
git add crates/erars-renderer/src/draw.rs crates/erars-renderer/src/main.rs
git commit -m "feat(renderer): assemble GPU instances from grid + atlas"
```

---

## Task 9: `App` controller and `main.rs` bootstrap — the running GUI

**Files:**
- Create: `crates/erars-renderer/src/app.rs`
- Rewrite: `crates/erars-renderer/src/main.rs`

- [ ] **Step 1: Write `app.rs`**

`crates/erars-renderer/src/app.rs`:

```rust
use std::sync::Arc;

use cosmic_text::SwashCache;
use erars_ast::Value;
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest, SystemResponse};
use erars_ui::{InputRequest, InputRequestType};
use winit::application::ApplicationHandler;
use winit::event::{ElementState, MouseScrollDelta, WindowEvent};
use winit::event_loop::ActiveEventLoop;
use winit::keyboard::{Key, NamedKey};
use winit::window::{Window, WindowId};

use crate::atlas::GlyphAtlas;
use crate::draw::build_instances;
use crate::font::FontCtx;
use crate::gpu::GpuContext;
use crate::grid::Grid;

/// User event used to wake the loop when the VM sends a request.
#[derive(Debug, Clone, Copy)]
pub struct Wake;

pub struct App {
    font: FontCtx,
    swash: SwashCache,
    receiver: ProxyReceiver,
    window: Option<Arc<Window>>,
    gpu: Option<GpuContext>,
    atlas: Option<GlyphAtlas>,

    frame: ConsoleFrame,
    current_req: Option<InputRequest>,
    input: String,
    scroll_y: f32,
    stick_bottom: bool,
    hovered_button: Option<usize>,
    cursor: (f32, f32),
    buttons_cache: Vec<crate::grid::ButtonRegion>,
    init_size: (u32, u32),
}

impl App {
    pub fn new(font: FontCtx, receiver: ProxyReceiver, init_size: (u32, u32)) -> Self {
        Self {
            font,
            swash: SwashCache::new(),
            receiver,
            window: None,
            gpu: None,
            atlas: None,
            frame: ConsoleFrame::default(),
            current_req: None,
            input: String::new(),
            scroll_y: 0.0,
            stick_bottom: true,
            hovered_button: None,
            cursor: (0.0, 0.0),
            buttons_cache: Vec::new(),
            init_size,
        }
    }

    fn send(&mut self, resp: SystemResponse) {
        let _ = self.receiver.res_tx.send(resp);
        self.current_req = None;
    }

    /// Drain all pending VM requests.
    fn drain_requests(&mut self, event_loop: &ActiveEventLoop) {
        while let Ok(req) = self.receiver.req_rx.try_recv() {
            match req {
                SystemRequest::Quit => event_loop.exit(),
                SystemRequest::Redraw(frame) => {
                    self.frame = frame;
                    self.stick_bottom = true;
                }
                SystemRequest::Input(req) => {
                    self.current_req = Some(req);
                }
            }
        }
        if let Some(w) = &self.window {
            w.request_redraw();
        }
    }

    fn grid_cols(&self) -> usize {
        let (w, _) = self.gpu.as_ref().map(|g| g.size()).unwrap_or(self.init_size);
        ((w as f32 / self.font.cell_w).floor() as usize).max(1)
    }

    fn render(&mut self) {
        let (Some(gpu), Some(atlas)) = (self.gpu.as_mut(), self.atlas.as_mut()) else {
            return;
        };
        let (_, win_h) = gpu.size();
        let cols = {
            let (w, _) = gpu.size();
            ((w as f32 / self.font.cell_w).floor() as usize).max(1)
        };

        let active_gen = self.current_req.as_ref().map(|r| r.generation);
        // input line appended as a synthetic bottom line
        let mut lines = self.frame.lines.clone();
        if self.current_req.is_some() {
            lines.push(self.input_line());
        }
        let grid = Grid::build(
            &mut self.font,
            &lines,
            cols,
            active_gen,
            self.hovered_button,
            self.frame.hl_color.0,
        );
        self.buttons_cache = grid.buttons.clone();

        if self.stick_bottom {
            self.scroll_y = (grid.content_h - win_h as f32).max(0.0);
            self.stick_bottom = false;
        }

        let instances = build_instances(
            &gpu.device,
            &gpu.queue,
            &mut self.font.font_system,
            &mut self.swash,
            atlas,
            &grid,
            self.scroll_y,
        );
        gpu.render(&atlas.view, &instances, self.frame.bg_color.0);
    }

    fn input_line(&self) -> erars_ui::ConsoleLine {
        use erars_ui::{Color, ConsoleLinePart, FontStyle, TextStyle};
        erars_ui::ConsoleLine {
            align: erars_ast::Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Text(
                format!("> {}_", self.input),
                TextStyle {
                    color: Color([200, 200, 200]),
                    font_family: "".into(),
                    font_style: FontStyle::NORMAL,
                },
            )],
        }
    }

    fn submit(&mut self) {
        let Some(req) = self.current_req.clone() else { return };
        match req.ty {
            InputRequestType::Int => {
                if let Ok(i) = self.input.trim().parse::<i64>() {
                    self.input.clear();
                    self.send(SystemResponse::Input(Value::Int(i)));
                }
            }
            InputRequestType::Str => {
                let s = std::mem::take(&mut self.input);
                self.send(SystemResponse::Input(Value::String(s)));
            }
            InputRequestType::AnyKey
            | InputRequestType::EnterKey
            | InputRequestType::ForceEnterKey => {
                self.input.clear();
                self.send(SystemResponse::Empty);
            }
        }
    }

    fn on_click(&mut self) {
        let Some(active) = self.current_req.as_ref().map(|r| r.generation) else {
            return;
        };
        let (mx, my) = self.cursor;
        let my_content = my + self.scroll_y;
        // collect first to avoid borrow conflict
        let hit = self.buttons_cache.iter().find(|b| {
            b.input_gen == active
                && mx >= b.rect[0]
                && mx <= b.rect[0] + b.rect[2]
                && my_content >= b.rect[1]
                && my_content <= b.rect[1] + b.rect[3]
        });
        if let Some(b) = hit {
            let value = b.value.clone();
            self.send(SystemResponse::Input(value));
        }
    }

    fn update_hover(&mut self) {
        let active = self.current_req.as_ref().map(|r| r.generation);
        let (mx, my) = self.cursor;
        let my_content = my + self.scroll_y;
        self.hovered_button = self
            .buttons_cache
            .iter()
            .position(|b| {
                active == Some(b.input_gen)
                    && mx >= b.rect[0]
                    && mx <= b.rect[0] + b.rect[2]
                    && my_content >= b.rect[1]
                    && my_content <= b.rect[1] + b.rect[3]
            });
    }
}

impl ApplicationHandler<Wake> for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }
        let attrs = Window::default_attributes().with_title("erars");
        let window = Arc::new(event_loop.create_window(attrs).unwrap());
        let size = window.inner_size();
        let instance = wgpu::Instance::default();
        let surface = instance.create_surface(window.clone()).unwrap();
        let gpu = GpuContext::new(&instance, surface, size.width.max(1), size.height.max(1));
        let atlas = GlyphAtlas::new(&gpu.device);
        self.atlas = Some(atlas);
        self.gpu = Some(gpu);
        self.window = Some(window);
        self.drain_requests(event_loop);
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, _: Wake) {
        self.drain_requests(event_loop);
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                if let Some(gpu) = self.gpu.as_mut() {
                    gpu.resize(size.width, size.height);
                }
                self.stick_bottom = true;
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            WindowEvent::RedrawRequested => self.render(),
            WindowEvent::CursorMoved { position, .. } => {
                self.cursor = (position.x as f32, position.y as f32);
                self.update_hover();
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: winit::event::MouseButton::Left,
                ..
            } => {
                // AnyKey/Enter accept any click
                if let Some(req) = self.current_req.clone() {
                    match req.ty {
                        InputRequestType::AnyKey
                        | InputRequestType::EnterKey
                        | InputRequestType::ForceEnterKey => self.submit(),
                        _ => self.on_click(),
                    }
                }
            }
            WindowEvent::MouseWheel { delta, .. } => {
                let dy = match delta {
                    MouseScrollDelta::LineDelta(_, y) => y * self.font.cell_h,
                    MouseScrollDelta::PixelDelta(p) => p.y as f32,
                };
                self.scroll_y = (self.scroll_y - dy).max(0.0);
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            WindowEvent::KeyboardInput { event, .. }
                if event.state == ElementState::Pressed =>
            {
                let Some(req) = self.current_req.clone() else { return };
                match &event.logical_key {
                    Key::Named(NamedKey::Enter) => self.submit(),
                    Key::Named(NamedKey::Backspace) => {
                        self.input.pop();
                    }
                    Key::Named(NamedKey::Space)
                        if matches!(req.ty, InputRequestType::AnyKey) =>
                    {
                        self.submit();
                    }
                    Key::Character(s) => match req.ty {
                        InputRequestType::Int => {
                            if s.chars().all(|c| c.is_ascii_digit()) {
                                self.input.push_str(s);
                            }
                        }
                        InputRequestType::Str => self.input.push_str(s),
                        InputRequestType::AnyKey => self.submit(),
                        _ => {}
                    },
                    _ => {
                        if matches!(req.ty, InputRequestType::AnyKey) {
                            self.submit();
                        }
                    }
                }
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            _ => {}
        }
    }
}
```

- [ ] **Step 2: Rewrite `main.rs`**

`crates/erars-renderer/src/main.rs`:

```rust
#![windows_subsystem = "windows"]

mod app;
mod atlas;
mod draw;
mod font;
mod gpu;
mod grid;
mod text;

use std::{path::Path, sync::Arc};

use app::{App, Wake};
use erars_loader::{load_config, load_script, run_script};
use erars_vm::get_interner;
use winit::event_loop::EventLoop;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(clap::Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(value_parser, default_value = ".", help = "ERA game path")]
    target_path: String,
    #[clap(long, default_value = "info", help = "Log level")]
    log_level: String,
    #[clap(long, help = "Don't print logs")]
    quite: bool,
    #[clap(long, help = "Load bytecode")]
    load: bool,
    #[clap(long, help = "Turn off ERB lint")]
    lint_off: bool,
}

fn main() {
    use flexi_logger::*;
    let args: Args = clap::Parser::parse();

    let _handle = if args.quite {
        None
    } else {
        Some(
            Logger::try_with_str(format!(
                "warn,wgpu_hal=off,erars={level},erars-renderer={level}",
                level = &args.log_level
            ))
            .unwrap()
            .log_to_file(
                FileSpec::default()
                    .directory(Path::new(&args.target_path).join("logs"))
                    .basename("erars"),
            )
            .write_mode(WriteMode::BufferAndFlush)
            .start()
            .unwrap(),
        )
    };
    log_panics::init();

    let config = load_config(&args.target_path);
    let event_loop = EventLoop::<Wake>::with_user_event().build().unwrap();
    let proxy = event_loop.create_proxy();

    let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(move || {
        let _ = proxy.send_event(Wake);
    }));

    let font_family = config.font_family.clone();
    let font_size = config.font_size;
    let line_height = config.line_height;
    let init_size = (config.window_width, config.window_height);

    let target_path = args.target_path.clone();
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .name("erars-runtime".into())
        .spawn(move || {
            let system_back = system.clone();
            let system = Box::new(system);
            let ret = if args.load {
                unsafe { load_script(&target_path, system, config) }
            } else {
                run_script(&target_path, system, config, false, !args.lint_off)
            };
            let normal = match ret {
                Ok((vm, mut ctx, mut tx)) => vm.start(&mut tx, &mut ctx),
                Err(err) => {
                    log::error!("Game loading failed: {err}");
                    false
                }
            };
            if normal {
                system_back.send_quit();
            }
        })
        .unwrap();

    let _ = get_interner();
    let font = font::FontCtx::new(&font_family, font_size, line_height);
    let mut app = App::new(font, receiver, init_size);
    event_loop.run_app(&mut app).unwrap();
}
```

Note: confirm the exact import path for `get_interner` — in `erars-iced` it came from `erars_ast::get_interner`. If `erars_vm::get_interner` does not resolve, use `erars_ast::get_interner`. Remove the `get_interner` call entirely if neither is needed (it was used in iced only to pass a resolved font-name pointer to iced; here we pass the family string directly to cosmic-text, so the call can be dropped).

- [ ] **Step 3: Build**

Run: `cargo build -p erars-renderer`
Expected: compiles. Fix any import path issues flagged by the compiler (notably `get_interner` per the note above; drop it if unused).

- [ ] **Step 4: Commit**

```bash
git add crates/erars-renderer/src/app.rs crates/erars-renderer/src/main.rs
git commit -m "feat(renderer): App controller and event-loop bootstrap"
```

---

## Task 10: TINPUT timeout support

**Files:**
- Modify: `crates/erars-renderer/src/app.rs`

- [ ] **Step 1: Add a deadline field and arm it on input**

In `App` add field `timeout_deadline: Option<std::time::Instant>` and a `timeout_value: Value` (default value to send). Initialize both to `None`/`Value::Int(0)` in `new`.

In `drain_requests`, when handling `SystemRequest::Input(req)`, set:

```rust
SystemRequest::Input(req) => {
    self.timeout_deadline = req.timeout.as_ref().map(|t| {
        let now = std::time::Instant::now();
        let secs = ((t.timeout - current_unix_nanos()).max(0)) as f64 / 1e9;
        now + std::time::Duration::from_secs_f64(secs)
    });
    if let Some(t) = req.timeout.as_ref() {
        self.timeout_value = t.default_value.clone();
    }
    self.current_req = Some(req);
}
```

Add a helper:

```rust
fn current_unix_nanos() -> i128 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos() as i128
}
```

- [ ] **Step 2: Fire the timeout in `about_to_wait`**

Implement the `about_to_wait` method on the `ApplicationHandler` impl:

```rust
fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
    if let Some(deadline) = self.timeout_deadline {
        if std::time::Instant::now() >= deadline {
            self.timeout_deadline = None;
            let v = self.timeout_value.clone();
            self.send(SystemResponse::Input(v));
            return;
        }
        // keep polling while a timeout is armed
        event_loop.set_control_flow(winit::event_loop::ControlFlow::wait_timeout(
            std::time::Duration::from_millis(50),
        ));
        if let Some(w) = &self.window {
            w.request_redraw();
        }
    }
}
```

Clear `self.timeout_deadline = None;` inside `send` so a normal answer cancels the timer.

- [ ] **Step 3: Build**

Run: `cargo build -p erars-renderer`
Expected: compiles.

- [ ] **Step 4: Commit**

```bash
git add crates/erars-renderer/src/app.rs
git commit -m "feat(renderer): TINPUT timeout countdown and default value"
```

---

## Task 11: Delete erars-iced and finalize

**Files:**
- Delete: `crates/erars-iced/`
- Modify: any workspace references

- [ ] **Step 1: Remove the old crate**

```bash
git rm -r crates/erars-iced
```

- [ ] **Step 2: Confirm nothing references it**

Run: `grep -rn "erars-iced\|erars_iced" --include=*.toml --include=*.rs --include=*.nix --include=*.yml .`
Expected: no remaining references (update `flake.nix`/CI if any appear).

- [ ] **Step 3: Full workspace build + tests**

Run: `cargo build --workspace && cargo test -p erars-renderer`
Expected: workspace builds; renderer tests pass.

- [ ] **Step 4: Run the GUI against a sample game**

Run: `cargo run -p erars-renderer -- ./tests` (or a known ERA game directory)
Expected: a window opens showing console text on a fixed-width grid; CJK and Latin align to columns; clicking a button advances the game. On macOS the same command runs on the Metal backend.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "chore: remove erars-iced in favor of erars-renderer"
```

---

## Self-Review Notes

- **Spec coverage:** new crate replacing iced (Tasks 1, 11); system fonts + bundled fallback (Task 2); Approach A cluster→column mapping (Task 3); alignment + Line-fill + buttons (Tasks 4–5); custom atlas via SwashCache (Task 6); single instanced pipeline (Task 7); fallback through cosmic-text shaping (Task 3); wake bridge via `notify`→`EventLoopProxy` (Task 9); TINPUT timeout (Task 10); GPU-free unit tests for column logic (Tasks 2–5, 8).
- **API risk points to verify during execution:** cosmic-text 0.12 `LayoutGlyph` fields (`start`, `end`, `physical()`, `w`, `line_y`) and `SwashCache::get_image_uncached` signature; `get_interner` import path (see Task 9 note); winit 0.30 `ControlFlow::wait_timeout` exact name (may be `ControlFlow::WaitUntil(Instant)` — adjust in Task 10).
