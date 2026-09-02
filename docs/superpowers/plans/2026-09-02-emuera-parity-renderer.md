# Emuera-Parity Text Renderer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make erars-renderer lay out and draw the ERA console exactly like Emuera 1.824 / uEmuera (1- or 2-half-cell boxes decided by code point + game language, Emuera wrapping/alignment/DRAWLINE pixel rules, MS Gothic bitmap strikes) with one width function shared by the VM and the renderer.

**Architecture:** A language-encoding width classifier in erars-ui feeds VM string functions/padding and the renderer. The renderer keeps cosmic-text only as a font database, resolves a font per character from an ordered chain, shapes spans with rustybuzz, snaps clusters to integer-pixel cells, lays out rows with Emuera's pixel arithmetic, rasterizes via ttf-parser bitmap strikes or swash outlines into a multi-page wgpu atlas, and renders with the existing instanced-quad pipeline.

**Tech Stack:** Rust 1.98, winit 0.30, wgpu 0.19, cosmic-text 0.12.1 (fontdb 0.16.2), rustybuzz 0.14.1, ttf-parser 0.21.1, swash 0.1.18, encoding_rs 0.8.33, unicode-width 0.1.11, etagere 0.2, flate2 + crc32fast, k9 snapshots.

**Spec:** docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md

## Global Constraints

- All pinned crate versions come from Cargo.lock; do not upgrade wgpu/winit/cosmic-text. New direct deps allowed: encoding_rs 0.8, unicode-width 0.1 (erars-ui), swash 0.1.18, smol_str 0.2, bitflags 2, flate2 1.0, crc32fast 1.3 (erars-renderer). Remove pad and unicode-width from erars-vm, pad from erars-ui, unicode-width and the cosmic-text features monospace_fallback/shape-run-cache from erars-renderer.
- Every pixel quantity (font_px, half_w, line_h, baseline, shift, glyph origins) is an integer physical pixel; shift = max(2, font_px / 6) with integer division; drawable_w = content_w − shift; Center x0 = content_w/2 − width/2 and Right x0 = content_w − width with integer division.
- Width rule: 0 for controls/combining/format (+U+00AD, U+D7B0–D7FF); else byte count in the game encoding; else JP best-fit overrides (U+00A2, U+00A3, U+00AC, U+2014, U+2016, U+301C, U+E000–E757 → 2); else 2 for unicode-width==Some(2) or U+1F1E6–1F1FF; else 1.
- Config defaults: printc_width 25, printc_count 3, window 760×480, font_family "", fore (192,192,192), bg (0,0,0), focus (255,255,0).
- msgothic.ttc (repo root) is proprietary: never commit it; tests that need it are gated on ERARS_FONT_DIR and skip (or panic under ERARS_REQUIRE_CJK_FONT=1) otherwise.
- GPU tests hold test_support::gpu_lock() and obtain devices via test_support::gpu_device(); they print "SKIP <test>: no wgpu adapter" and pass without an adapter unless ERARS_REQUIRE_GPU=1.
- Commit after every task with conventional messages (feat/fix/test/docs(scope)).

---

## File Structure

Paths are relative to the repo root. `Tn` names the task that touches the file; the workspace compiles at the end of every task.

### Created

| File | Responsibility | Task |
|---|---|---|
| `crates/erars-ui/src/width.rs` | `WidthTable`: per-encoding 0/1/2-cell classifier (2-bit BMP table), `str_cells` with 8-cell tab stops, `TAB_CELLS` | T1 |
| `crates/erars-vm/src/terminal_vm/cells.rs` | Cell-walk string helpers for STRLEN/SUBSTRING/STRFIND and `PadStr` (`uft_index`, `substring_cells`, `strfind_cells`, `pad_str_cells`) | T4 |
| `crates/erars-vm/tests/train_menu.rs` | `printc_count` gate of the TRAIN menu driven by a scripted `SystemFunctions` | T4 |
| `tests/run_tests/jp/emuera.config` | Per-directory config: JAPANESE fixtures + colour keys | T4 |
| `tests/run_tests/jp/{lang,strlen_cells,substring_cells,strfind_cells,padstr,printc,getdefcolor}.{erb,out}` | JAPANESE VM fixtures (cell widths, cp932 overrides, PadStr, PRINTC, colours) | T4 |
| `tests/run_tests/basic/{strlen_cells,substring_cells,strfind_cells,padstr,getdefcolor,printc,drawline,alignment,print_newline,printbutton_newline}.{erb,out}` | KOREAN VM fixtures for the string functions, padding, colours, PRINTC/PRINTLC, DRAWLINE, ALIGNMENT and `\n` splitting | T4 |
| `crates/erars-renderer/src/flags.rs` | `RasterFlags` (BOLD_SYNTH / ITALIC_SYNTH) shared by shaper, layout and raster | T5 |
| `crates/erars-renderer/src/layout.rs` | Rows/clusters/rects/button fragments with Emuera pixel rules; `Geometry`, `layout()`, `layout_snapshot()` (replaces `grid.rs`) | T7 |
| `crates/erars-renderer/src/raster.rs` | Strike (ttf-parser) and outline (swash) rasterization, `RasterKey`, multi-page 2048² atlas `GlyphRaster` (replaces `atlas.rs`) | T8 |
| `crates/erars-renderer/src/lib.rs` | Library target exposing `app, draw, flags, font, gpu, headless, layout, raster, test_support, text` so `tests/tui.rs` can use them | T10 |
| `crates/erars-renderer/tests/games/tui/{ERB/TUI.ERB, CSV/GAMEBASE.CSV, emuera.config, emuera.jp.config}` | Synthetic fixture game (UTF-8 BOM): box map, PRINTC/PRINTLC tables, centred title, DRAWLINE, buttons | T11 |
| `crates/erars-renderer/tests/tui.rs` | Integration: run the fixture headlessly, snapshot `layout_snapshot` for KOREAN/JAPANESE, GPU PNG dump | T11 |

### Modified

| File | Responsibility of the change | Task |
|---|---|---|
| `Cargo.toml` (workspace) | `encoding_rs = "0.8"` as a workspace dependency | T1 |
| `Cargo.lock` | Regenerated dependency edges (committed with each task) | T1–T11 |
| `crates/erars-ui/Cargo.toml` | `+encoding_rs`, `+unicode-width 0.1`, `−pad` (T1); `+serde_json` dev-dep (T3) | T1, T3 |
| `crates/erars-ui/src/lib.rs` | `pub mod width`, `pad` removed (T1); `ConsoleConfig`, `VirtualConsole::new(&ConsoleConfig)`, `cells`/`char_cells`/`default_color`/`reset_color`, cell-based PRINTC/PRINTLC, `\n` splitting, `\n`-stripping PRINTBUTTON*, NORMAL-style `draw_line` (T3); `From<Color> for u32` → `0xRRGGBB`, new `From<u32> for Color` (T4) | T1, T3, T4 |
| `crates/erars-compiler/Cargo.toml` | `+encoding_rs` | T2 |
| `crates/erars-compiler/src/parser.rs` | `Language::encoding()`, CHINESE_HANS/HANT label fix, colour config keys `文字色/背景色/選択中文字色`, defaults 25/3/760×480/"" | T2 |
| `crates/erars-compiler/src/compiler.rs` | `PadStr` default alignment Right | T2 |
| `tests/run_tests/basic/builtin_methods.out` | `GETCONFIG 19 ` (empty `フォント名` default) | T2 |
| `crates/erars-loader/src/lib.rs` | Temporary local `console_config` helper + `VirtualConsole::new(&console_config(&config))` (T3); import `erars_vm::console_config`, helper deleted (T4) | T3, T4 |
| `tests/run_tests.rs` | Temporary helper (T3); rewritten: per-directory `emuera.config` via `fixture_config`, `console_config` (T4) | T3, T4 |
| `crates/erars-vm/Cargo.toml` | `encoding_rs.workspace`, `−pad`, `−unicode-width`, `−twoway`, `+serde_yaml` dev-dep | T4 |
| `crates/erars-vm/src/lib.rs` | `pub fn console_config(&EraConfig) -> ConsoleConfig` | T4 |
| `crates/erars-vm/src/context.rs` | `VmContext::encoding` delegates to `Language::encoding` | T4 |
| `crates/erars-vm/src/terminal_vm.rs` | `mod cells;` | T4 |
| `crates/erars-vm/src/terminal_vm/executor.rs` | STRLEN/SUBSTRING/STRFIND/PadStr via cells; PRINTD/RESETCOLOR/GETDEFCOLOR/GETDEFBGCOLOR via the configured colours; SETCOLOR/SETBGCOLOR single-int form and the PRINTD restore decode `0xRRGGBB` | T4 |
| `crates/erars-proxy-system/src/lib.rs` | `ConsoleFrame.fore_color` from `VirtualConsole::default_color()` | T9 |
| `crates/erars-renderer/Cargo.toml` | cosmic-text default features only, `+smol_str`, `+bitflags` (T5); `+rustybuzz` (T6); `[dev-dependencies] k9` (T7); `+swash` (T8); `+flate2`, `+crc32fast` (T9); `−unicode-width`, `−sys-locale`, `[lib] doctest = false` (T10); `+anyhow` dev-dep (T11) | T5–T11 |
| `crates/erars-renderer/src/main.rs` | Module list / `font_candidates` sourced from `font::language_candidates` (T5–T8); PNG `--headless-shot` shim over `headless::shaper_for` + `render_frame` (T9); thin CLI over the library with `--no-bitmap-strikes` (T10) | T5–T10 |
| `crates/erars-renderer/src/font.rs` | `FontChain`, `FontConfig`, `StyleKey`, `language_candidates`, coverage/loading helpers beside the legacy `FontCtx` (T5); legacy `FontCtx` deleted (T10) | T5, T10 |
| `crates/erars-renderer/src/text.rs` | `CellMetrics`, `ShapedGlyph`, `Cluster`, rustybuzz `Shaper` with per-layout cache above a legacy block (T6); legacy `CellShaper` deleted (T10) | T6, T10 |
| `crates/erars-renderer/src/draw.rs` | `View`, `RegionSource`, `build_instances`/`build_instances_with` over `Layout` with draw-time hover; old path kept as `build_instances_legacy` (T8); legacy path deleted (T10) | T8, T10 |
| `crates/erars-renderer/src/gpu.rs` | `Instance: Debug + PartialEq`, `nearest_sampler`, `FrameDraw`, per-page `GpuContext::render` | T8 |
| `crates/erars-renderer/src/app.rs` | One-line patches for the renamed legacy path (T8); rewritten: `AppConfig`, row-anchored `View`, scroll/wheel/hover/hit-test helpers, input strip, `Shaper`/`Layout`/`GlyphRaster` plumbing (T10) | T8, T10 |
| `crates/erars-renderer/src/headless.rs` | Legacy call renamed (T8); rewritten: `Rendered` helpers, `request_device`, `shaper_for`, `render_frame`/`_opts`/`_on`, `encode_png`/`write_png`, pixel tests (T9) | T8, T9 |
| `crates/erars-renderer/src/test_support.rs` | `test_name`, `gpu_device` (T8); rewritten with font gates (`require_cjk_font`, `msgothic_font`), `test_shaper`, console fixtures (T9); exported `pub mod` (T10) | T8, T9, T10 |
| `.github/workflows/check.yml` | lavapipe install + `ERARS_REQUIRE_GPU=1`, maintained action versions | T11 |
| `justfile` | `test-align` (goldens + pixel tests + tui), `headless-shot` writes PNG | T11 |
| `.gitignore` | `msgothic.ttc`, `.DS_Store` | T11 |
| `README.md` | "GUI renderer (erars-renderer)" usage section | T11 |

### Deleted

| File | Replaced by | Task |
|---|---|---|
| `crates/erars-renderer/src/grid.rs` | `layout.rs` | T10 |
| `crates/erars-renderer/src/atlas.rs` | `raster.rs` | T10 |

---

## Tasks

### Task 1: Width classifier

Spec: `docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md` Component 1 (lines 160-214), Testing §1 (599-602), Workspace changes (682-684). Everything below was verified by a bare-`rustc` probe against encoding_rs 0.8.33 + unicode-width 0.1.11 (`scratchpad/probe-plan-T1/probe.rs`, `probe2.rs`); the numbers in the tests are the probe's output.

**Files:**
- Create: `crates/erars-ui/src/width.rs`
- Modify: `/home/riey/repos/erars/Cargo.toml` lines 23-32 (`[workspace.dependencies]`) — add `encoding_rs`
- Modify: `/home/riey/repos/erars/crates/erars-ui/Cargo.toml` lines 7-22 (`[dependencies]`) — `+encoding_rs` (workspace), `+unicode-width = "0.1"`, `−pad` (line 16)
- Modify: `/home/riey/repos/erars/crates/erars-ui/src/lib.rs` line 3 (`use pad::PadStr;`), insert `pub mod width;` after line 9, lines 363-389 (`print_button_lc`, `print_button_rc`, `printlc`, `printrc`), append a helper after line 550 and a test after line 799
- Modify: `/home/riey/repos/erars/Cargo.lock` (regenerated by cargo; commit it)
- Test: `crates/erars-ui/src/width.rs` (`#[cfg(test)] mod tests`), one helper test appended to `crates/erars-ui/src/lib.rs`

**Interfaces:**
- Consumes: nothing (first task).
- Produces (used by T3 `VirtualConsole`, T4 VM string functions, T6 `Shaper`):
  - `erars_ui::width::WidthTable` — `Clone + Debug + Send + Sync`
  - `impl WidthTable { pub fn new(encoding: &'static encoding_rs::Encoding) -> Self; pub fn encoding(&self) -> &'static encoding_rs::Encoding; pub fn char_cells(&self, c: char) -> u8; pub fn str_cells(&self, s: &str) -> usize; }`
  - `erars_ui::width::TAB_CELLS: usize = 8` (tab stop used by `str_cells`; T6 expands `\t` with the same constant)
  - `erars_ui` now depends on `encoding_rs` (workspace) and `unicode-width 0.1`; `pad` is gone from `erars-ui` (still used by `erars-vm` until T4).
  - Transitional (private, deleted by T3): `fn pad_to_width(s: &str, width: usize, right: bool) -> String` in `lib.rs`, behaviour-identical to `pad::PadStr::pad_to_width_with_alignment`.

Rules the code implements (evaluated in order, per code point; `width` = `unicode_width::UnicodeWidthChar::width`, the non-CJK table):
1. U+00AD → 0; U+D7B0–U+D7FF → 0; `width(c) == None | Some(0)` → 0.
2. `Encoder::encode_from_utf8_without_replacement` of the single char (4-byte output buffer, `last = false`) returns `InputEmpty` with 1 or 2 bytes written → that byte count. `Unmappable(_)` → continue. (`OutputFull` cannot occur with a 4-byte buffer: the four legacy encoders emit at most 2 bytes; the probe showed `OutputFull, read 0, written 0` only for buffers shorter than 2 bytes. encoding_rs's `GBK` encoder is `Gb18030Encoder { extended: false }` — `variant.rs:380` — so it reports `Unmappable` instead of 4-byte GB18030 sequences.)
3. Only when `encoding` is `SHIFT_JIS` (pointer identity): U+00A2, U+00A3, U+00AC, U+2014, U+2016, U+301C, U+E000–U+E757 → 2.
4. `width(c) == Some(2)` or U+1F1E6–U+1F1FF → 2; else 1.

The BMP is precomputed into a 16 384-byte 2-bit table at `new()` (probe, `-O`: Shift_JIS 12.7 ms, EUC-KR 9.1 ms, GBK 11.4 ms, Big5 55 ms; no code point ever classifies to 3). Astral code points run the rule directly with a fresh encoder.

- [ ] **Step 1: Add `encoding_rs` to the workspace dependencies**

Edit `/home/riey/repos/erars/Cargo.toml` (currently lines 23-32):

```diff
 [workspace.dependencies]
 flume = "0.11.0"
 serde = { version = "1", features = ["derive"] }
 serde_json = "1"
 strum = { version = "0.26", features = ["derive", "phf"] }
 thiserror = "1"
 anyhow = "1"
 logos = { version = "0.14", features = ["logos-derive"] }
 hashbrown = { version = "0.15", features = ["serde"] }
 enum-map = { version = "2", features = ["serde"] }
+encoding_rs = "0.8"
```

(`Cargo.lock` already holds encoding_rs 0.8.33 via `erars-vm`'s `encoding_rs = "0.8.31"`; the workspace entry resolves to the same version.)

- [ ] **Step 2: Add `encoding_rs` and `unicode-width` to `erars-ui`**

Edit `/home/riey/repos/erars/crates/erars-ui/Cargo.toml` (lines 7-22). Keep `pad` for now — it is removed in Step 8 after its call sites are rewritten.

```diff
 [dependencies]
 erars-ast = { path = "../erars-ast" }

 anyhow.workspace = true
 serde.workspace = true
+encoding_rs.workspace = true

 parking_lot = "0.12.1"
 once_cell = "1.15.0"
 regex = "1.6.0"
 pad = "0.1.6"
 smol_str = "0.2.0"
 log = "0.4.17"
 time = "0.3.15"
 bitflags = "2.3.1"
 crossbeam-channel = "0.5.6"
 serde_iter = "0.1.1"
+unicode-width = "0.1"
```

- [ ] **Step 3: Register the module in `lib.rs`**

Edit `/home/riey/repos/erars/crates/erars-ui/src/lib.rs`: after line 9 (`use std::time::Instant;`) insert

```rust

pub mod width;
```

- [ ] **Step 4: Create `width.rs` with the API skeleton and the full test module**

Create `/home/riey/repos/erars/crates/erars-ui/src/width.rs` with exactly this content (the bodies are `todo!()` stubs so the tests compile and fail; Step 6 replaces everything above `#[cfg(test)]`):

```rust
//! Cell-width classifier shared by the VM (`STRLEN`, `SUBSTRING`, `PadStr`,
//! PRINTC padding) and the renderer (cluster boxes).
//!
//! Every code point occupies 0, 1 or 2 half-width cells, decided by the code
//! point and the game *encoding* alone — never by the font that draws it. See
//! `docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md`,
//! Component 1, for the rule and the accepted deviations from Emuera.

use encoding_rs::{Encoder, EncoderResult, Encoding, SHIFT_JIS};
use std::fmt;
use unicode_width::UnicodeWidthChar;

/// Bytes in the packed BMP table: 65 536 code points × 2 bits.
const BMP_TABLE_LEN: usize = 0x1_0000 / 4;

/// A `\t` advances to the next multiple of this many cells (uEmuera /
/// GRAPHICS-mode behaviour). The renderer expands tabs with the same stop.
pub const TAB_CELLS: usize = 8;

/// Per-encoding cell widths: 2 bits per BMP code point, built once.
#[derive(Clone)]
pub struct WidthTable {
    encoding: &'static Encoding,
    bmp: Box<[u8]>,
}

impl WidthTable {
    /// Build the table for one game encoding (`SHIFT_JIS`, `EUC_KR`, `GBK`,
    /// `BIG5`). About 10-50 ms; build once per console / shaper.
    pub fn new(encoding: &'static Encoding) -> Self {
        let _ = encoding;
        todo!("WidthTable::new (Task 1 step 6)")
    }

    /// The encoding this table was built for.
    pub fn encoding(&self) -> &'static Encoding {
        self.encoding
    }

    /// Cells occupied by `c`: 0, 1 or 2. Controls (including `\n` and `\t`)
    /// are 0 — the console splits `\n` and `str_cells` expands `\t` itself.
    pub fn char_cells(&self, c: char) -> u8 {
        let _ = c;
        todo!("WidthTable::char_cells (Task 1 step 6)")
    }

    /// Sum of `char_cells` over `s`, with `\t` advancing to the next multiple
    /// of [`TAB_CELLS`].
    pub fn str_cells(&self, s: &str) -> usize {
        let _ = s;
        todo!("WidthTable::str_cells (Task 1 step 6)")
    }
}

impl fmt::Debug for WidthTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WidthTable")
            .field("encoding", &self.encoding.name())
            .finish_non_exhaustive()
    }
}

/// Windows cp932 best-fit code points that WHATWG Shift_JIS cannot encode but
/// Emuera's `STRLEN` counts as 2 bytes and MS Gothic draws full-width
/// (`¢ £ ¬ — ‖ 〜` and the user-defined area).
#[inline]
fn jp_best_fit(cp: u32) -> bool {
    let _ = cp;
    todo!("jp_best_fit (Task 1 step 6)")
}

/// The rule of Component 1 for one code point. `encoder` must be an encoder
/// of `encoding`; it is reused across calls (`last = false`, stateless
/// legacy encoders), so a table build performs no allocation per char.
fn classify(c: char, encoding: &'static Encoding, encoder: &mut Encoder) -> u8 {
    let _ = (c, encoding, encoder);
    todo!("classify (Task 1 step 6)")
}

#[cfg(test)]
mod tests {
    use super::{classify, WidthTable, TAB_CELLS};
    use encoding_rs::{Encoding, BIG5, EUC_KR, GBK, SHIFT_JIS};
    use once_cell::sync::Lazy;

    static JP: Lazy<WidthTable> = Lazy::new(|| WidthTable::new(SHIFT_JIS));
    static KR: Lazy<WidthTable> = Lazy::new(|| WidthTable::new(EUC_KR));
    static HANS: Lazy<WidthTable> = Lazy::new(|| WidthTable::new(GBK));
    static HANT: Lazy<WidthTable> = Lazy::new(|| WidthTable::new(BIG5));

    fn check(table: &WidthTable, want: u8, chars: &[char]) {
        for &c in chars {
            assert_eq!(
                table.char_cells(c),
                want,
                "U+{:04X} {:?} in {}",
                c as u32,
                c,
                table.encoding().name()
            );
        }
    }

    // Spec Component 1, "Expected values (tested)", row JP.
    #[test]
    fn japanese_shift_jis() {
        check(&JP, 1, &['A', 'ｱ', '═', '║', '░', '█', '▶', 'é', '♥', '¥']);
        check(
            &JP,
            2,
            &[
                'あ', '─', '°', '※', '★', 'α', 'А', '①', '〜', '‖', '¢', '−', '\u{E000}', '한',
                '😀',
            ],
        );
        check(&JP, 0, &['\u{0301}', '\u{200D}', '\u{00AD}']);
    }

    // Row KR: KS X 1001 has the single/mixed-weight box glyphs and `▒`, but
    // not `═ ░ █`; `¢` U+00A2 is not in WHATWG EUC-KR (0xA1CB is U+FFE0).
    #[test]
    fn korean_euc_kr() {
        check(&KR, 1, &['A', 'ｱ', '═', '░', '█', '¢']);
        check(&KR, 2, &['한', 'あ', '─', '▒', '★', '①', '😀']);
        check(&KR, 0, &['\u{0301}', '\u{1160}']);
    }

    // Row ZH: GBK and Big5 encode the double-line box characters and `█`.
    #[test]
    fn chinese_gbk_and_big5() {
        for table in [&*HANS, &*HANT] {
            check(table, 1, &['A', 'ｱ', '░']);
            check(table, 2, &['═', '║', '█', '中', '한']);
            check(table, 0, &['\u{0301}']);
        }
        assert_eq!(HANS.str_cells("╔══╗"), 8);
        assert_eq!(HANT.str_cells("╔══╗"), 8);
        assert_eq!(JP.str_cells("╔══╗"), 4);
        assert_eq!(KR.str_cells("╔══╗"), 4);
    }

    // Rule 2b applies to Shift_JIS only; elsewhere the same code points follow
    // the plain rule (unmappable → EAW: `〜` is W, the rest are A/N → 1).
    #[test]
    fn jp_best_fit_overrides_and_eudc() {
        for c in ['\u{00A2}', '\u{00A3}', '\u{00AC}', '\u{2014}', '\u{2016}', '\u{301C}'] {
            assert_eq!(JP.char_cells(c), 2, "JP U+{:04X}", c as u32);
        }
        check(&KR, 1, &['\u{00A2}', '\u{00A3}', '\u{00AC}', '\u{2014}', '\u{2016}']);
        assert_eq!(KR.char_cells('\u{301C}'), 2);
        assert_eq!(HANT.char_cells('\u{2016}'), 1);
        // User-defined area U+E000–U+E757 (cp932 gaiji): 2 in Japanese only.
        check(&JP, 2, &['\u{E000}', '\u{E3FF}', '\u{E757}']);
        check(&JP, 1, &['\u{E758}', '\u{F8FF}']);
        check(&KR, 1, &['\u{E000}', '\u{E757}']);
        check(&HANT, 1, &['\u{E000}']);
        // Encodable code points keep their WHATWG byte count: `−` is 0x817C in
        // Shift_JIS (2), unmappable in EUC-KR (1); `¥` / `‾` / `ｱ` are 1 byte.
        assert_eq!(JP.char_cells('−'), 2);
        assert_eq!(KR.char_cells('−'), 1);
        check(&JP, 1, &['¥', '\u{203E}', 'ｱ', '\u{FF9E}']);
        check(&JP, 2, &['\u{FF0D}', '\u{2225}', '\u{FF5E}', '\u{3000}', '→']);
    }

    // Rule 1: overrides, controls, combining marks, format characters.
    #[test]
    fn zero_width_and_controls() {
        check(&JP, 0, &['\u{00AD}', '\u{D7B0}', '\u{D7FF}']);
        check(&JP, 0, &['\0', '\t', '\n', '\r', '\u{1B}', '\u{7F}', '\u{80}', '\u{9F}']);
        check(
            &JP,
            0,
            &[
                '\u{0301}', '\u{3099}', '\u{200C}', '\u{200D}', '\u{FE0E}', '\u{FE0F}', '\u{FEFF}',
                '\u{2060}', '\u{2064}', '\u{1160}', '\u{11FF}', '\u{E0001}', '\u{E0100}',
            ],
        );
        // Spacing characters that look like the above are not zero.
        check(&JP, 1, &['\u{00A0}', '\u{2028}']);
        assert_eq!(JP.char_cells('\u{309B}'), 2); // spacing voiced mark, in JIS
        for table in [&*KR, &*HANS, &*HANT] {
            check(table, 0, &['\u{00AD}', '\u{D7B0}', '\u{0301}', '\u{200D}', '\u{1160}', '\n']);
        }
    }

    // Astral code points bypass the table; regional indicators are forced to 2.
    #[test]
    fn astral_and_regional_indicators() {
        for table in [&*JP, &*KR, &*HANS, &*HANT] {
            check(
                table,
                2,
                &['😀', '\u{1F1E6}', '\u{1F1F0}', '\u{1F1FF}', '\u{1F468}', '\u{1F3FD}', '\u{20000}'],
            );
            check(table, 1, &['\u{1D400}', '\u{10400}', '\u{1F170}', '\u{10FFFF}']);
            assert_eq!(table.str_cells("🇰🇷"), 4);
            assert_eq!(table.str_cells("👨\u{200D}👩\u{200D}👧"), 6);
        }
    }

    // `str_cells` of mixed strings and tab expansion to 8-cell stops.
    #[test]
    fn str_cells_mixed_and_tabs() {
        assert_eq!(TAB_CELLS, 8);
        assert_eq!(JP.str_cells(""), 0);
        assert_eq!(JP.str_cells("A한あ"), 5);
        assert_eq!(KR.str_cells("A한あ"), 5);
        assert_eq!(JP.str_cells("e\u{0301}"), 1);
        assert_eq!(JP.str_cells("❤\u{FE0F}"), 1);
        assert_eq!(JP.str_cells("┌──┐"), 8);
        assert_eq!(JP.str_cells("[ 0] 텍스트"), 11);
        assert_eq!(JP.str_cells("abc\ndef"), 6);
        assert_eq!(JP.str_cells("\u{00AD}"), 0);
        assert_eq!(JP.str_cells("\t"), 8);
        assert_eq!(JP.str_cells("a\tb"), 9);
        assert_eq!(JP.str_cells("1234567\tX"), 9);
        assert_eq!(JP.str_cells("12345678\t"), 16);
        assert_eq!(JP.str_cells("あ\tb"), 9);
        assert_eq!(JP.str_cells("\t\t"), 16);
    }

    // The packed table must reproduce the direct rule for every BMP code
    // point (checks the 2-bit packing and that no value is 3).
    #[test]
    fn bmp_table_matches_direct_rule() {
        for (table, encoding) in [(&*JP, SHIFT_JIS), (&*KR, EUC_KR), (&*HANS, GBK), (&*HANT, BIG5)]
        {
            let encoding: &'static Encoding = encoding;
            let mut encoder = encoding.new_encoder();
            for cp in 0u32..0x1_0000 {
                let Some(c) = char::from_u32(cp) else { continue };
                let direct = classify(c, encoding, &mut encoder);
                assert!(direct <= 2, "U+{cp:04X} classified {direct}");
                assert_eq!(table.char_cells(c), direct, "U+{cp:04X} in {}", encoding.name());
            }
        }
    }

    #[test]
    fn debug_and_clone() {
        assert_eq!(format!("{:?}", *JP), "WidthTable { encoding: \"Shift_JIS\", .. }");
        assert_eq!(format!("{:?}", *KR), "WidthTable { encoding: \"EUC-KR\", .. }");
        let copy: WidthTable = (*JP).clone();
        assert_eq!(copy.char_cells('あ'), 2);
        assert!(std::ptr::eq(copy.encoding(), SHIFT_JIS));
    }
}
```

- [ ] **Step 5: Run the tests and watch them fail**

```
cargo test -p erars-ui width::
```

Expected: the crate compiles (there may be `unused` warnings for `BMP_TABLE_LEN`, `EncoderResult`, `UnicodeWidthChar`, `SHIFT_JIS` — fine), then 9 failures such as

```
thread 'width::tests::japanese_shift_jis' panicked at crates/erars-ui/src/width.rs:...:
not yet implemented: WidthTable::new (Task 1 step 6)
...
test result: FAILED. 0 passed; 9 failed; 0 ignored
```

(The tests share `Lazy` tables, so after the first `todo!` panic the other tests in the same table report `Lazy instance has previously been poisoned` instead — still 9 failures, which is the point.)

- [ ] **Step 6: Implement the classifier**

In `/home/riey/repos/erars/crates/erars-ui/src/width.rs`, replace everything from the start of the file to the line before `#[cfg(test)]` with:

```rust
//! Cell-width classifier shared by the VM (`STRLEN`, `SUBSTRING`, `PadStr`,
//! PRINTC padding) and the renderer (cluster boxes).
//!
//! Every code point occupies 0, 1 or 2 half-width cells, decided by the code
//! point and the game *encoding* alone — never by the font that draws it. See
//! `docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md`,
//! Component 1, for the rule and the accepted deviations from Emuera.
//!
//! Rule, in order:
//! 1. U+00AD and U+D7B0–U+D7FF → 0; `unicode_width::UnicodeWidthChar::width`
//!    (non-CJK table) `None` (controls) or `Some(0)` (combining marks, format
//!    characters, Hangul V/T jamo) → 0.
//! 2. Encodable in the game encoding (WHATWG encoder, `Unmappable` = not
//!    encodable) → the byte count, 1 or 2.
//! 2b. Shift_JIS only: Windows cp932 best-fit code points `¢ £ ¬ — ‖ 〜` and
//!    the user-defined area U+E000–U+E757 → 2.
//! 3. `width == Some(2)` or Regional_Indicator U+1F1E6–U+1F1FF → 2; else 1.

use encoding_rs::{Encoder, EncoderResult, Encoding, SHIFT_JIS};
use std::fmt;
use unicode_width::UnicodeWidthChar;

/// Bytes in the packed BMP table: 65 536 code points × 2 bits.
const BMP_TABLE_LEN: usize = 0x1_0000 / 4;

/// A `\t` advances to the next multiple of this many cells (uEmuera /
/// GRAPHICS-mode behaviour). The renderer expands tabs with the same stop.
pub const TAB_CELLS: usize = 8;

/// Per-encoding cell widths: 2 bits per BMP code point, built once.
#[derive(Clone)]
pub struct WidthTable {
    encoding: &'static Encoding,
    bmp: Box<[u8]>,
}

impl WidthTable {
    /// Build the table for one game encoding (`SHIFT_JIS`, `EUC_KR`, `GBK`,
    /// `BIG5`). About 10-50 ms; build once per console / shaper.
    pub fn new(encoding: &'static Encoding) -> Self {
        let mut bmp = vec![0u8; BMP_TABLE_LEN].into_boxed_slice();
        let mut encoder = encoding.new_encoder();
        for cp in 0u32..0x1_0000 {
            // Surrogates are not chars; their slots stay 0 and are never read.
            let Some(c) = char::from_u32(cp) else { continue };
            let cells = classify(c, encoding, &mut encoder);
            debug_assert!(cells <= 2, "U+{cp:04X} classified {cells}");
            bmp[(cp >> 2) as usize] |= cells << ((cp & 3) * 2);
        }
        Self { encoding, bmp }
    }

    /// The encoding this table was built for.
    pub fn encoding(&self) -> &'static Encoding {
        self.encoding
    }

    /// Cells occupied by `c`: 0, 1 or 2. Controls (including `\n` and `\t`)
    /// are 0 — the console splits `\n` and `str_cells` expands `\t` itself.
    #[inline]
    pub fn char_cells(&self, c: char) -> u8 {
        let cp = c as u32;
        if cp < 0x1_0000 {
            (self.bmp[(cp >> 2) as usize] >> ((cp & 3) * 2)) & 3
        } else {
            // Astral: no table; the four legacy encoders never map these, so
            // this is rule 1 / rule 3 only, but run the full rule for clarity.
            classify(c, self.encoding, &mut self.encoding.new_encoder())
        }
    }

    /// Sum of `char_cells` over `s`, with `\t` advancing to the next multiple
    /// of [`TAB_CELLS`].
    pub fn str_cells(&self, s: &str) -> usize {
        let mut cells = 0usize;
        for c in s.chars() {
            if c == '\t' {
                cells = (cells / TAB_CELLS + 1) * TAB_CELLS;
            } else {
                cells += usize::from(self.char_cells(c));
            }
        }
        cells
    }
}

impl fmt::Debug for WidthTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WidthTable")
            .field("encoding", &self.encoding.name())
            .finish_non_exhaustive()
    }
}

/// Windows cp932 best-fit code points that WHATWG Shift_JIS cannot encode but
/// Emuera's `STRLEN` counts as 2 bytes and MS Gothic draws full-width
/// (`¢ £ ¬ — ‖ 〜` and the user-defined area).
#[inline]
fn jp_best_fit(cp: u32) -> bool {
    matches!(cp, 0x00A2 | 0x00A3 | 0x00AC | 0x2014 | 0x2016 | 0x301C)
        || (0xE000..=0xE757).contains(&cp)
}

/// The rule of Component 1 for one code point. `encoder` must be an encoder
/// of `encoding`; it is reused across calls (`last = false`, stateless
/// legacy encoders), so a table build performs no allocation per char.
fn classify(c: char, encoding: &'static Encoding, encoder: &mut Encoder) -> u8 {
    let cp = c as u32;

    // Step 1: zero width. Explicit overrides first (unicode-width 0.1.11
    // returns Some(1) for U+00AD and the V/T jamo extensions).
    if cp == 0x00AD || (0xD7B0..=0xD7FF).contains(&cp) {
        return 0;
    }
    let width = UnicodeWidthChar::width(c);
    if matches!(width, None | Some(0)) {
        return 0;
    }

    // Step 2: encodable in the game encoding → byte count (1 or 2).
    let mut utf8 = [0u8; 4];
    let src: &str = c.encode_utf8(&mut utf8);
    let mut dst = [0u8; 4];
    let (result, _read, written) = encoder.encode_from_utf8_without_replacement(src, &mut dst, false);
    match result {
        EncoderResult::InputEmpty if (1..=2).contains(&written) => return written as u8,
        // Neither can happen for Shift_JIS / EUC-KR / GBK / Big5 (≤ 2 bytes per
        // code point, 4-byte buffer); treat like "not encodable" if it ever does.
        EncoderResult::InputEmpty | EncoderResult::OutputFull => {}
        EncoderResult::Unmappable(_) => {}
    }

    // Step 2b: Windows cp932 best-fit, Japanese only.
    if std::ptr::eq(encoding, SHIFT_JIS) && jp_best_fit(cp) {
        return 2;
    }

    // Step 3: East-Asian wide / fullwidth, emoji presentation, regional indicators.
    if width == Some(2) || (0x1F1E6..=0x1F1FF).contains(&cp) {
        return 2;
    }
    1
}
```

- [ ] **Step 7: Run the tests, then commit the classifier**

```
cargo test -p erars-ui width::
```

Expected: `test result: ok. 9 passed; 0 failed; 0 ignored` (the `bmp_table_matches_direct_rule` test takes a few hundred ms under the test profile). Then:

```
cd /home/riey/repos/erars && git add Cargo.toml Cargo.lock crates/erars-ui/Cargo.toml crates/erars-ui/src/lib.rs crates/erars-ui/src/width.rs && git commit -m "feat(ui): WidthTable cell-width classifier per game encoding

Encoding byte count (WHATWG encoder) with unicode-width for zero/wide,
cp932 best-fit and EUDC overrides for Japanese, 2-bit BMP table."
```

- [ ] **Step 8: Replace the `pad` call sites with a transitional helper**

Edit `/home/riey/repos/erars/crates/erars-ui/src/lib.rs`:

(a) Delete line 3:

```diff
 use erars_ast::{Alignment, Value};
 use once_cell::sync::Lazy;
-use pad::PadStr;
 use regex::Regex;
```

(b) Replace the four methods at (original) lines 363-389 — `print_button_lc`, `print_button_rc`, `printlc`, `printrc` — with:

```rust
    pub fn print_button_lc(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        self.print_button(pad_to_width(&text, self.printc_width, false), value);
    }

    pub fn print_button_rc(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        self.print_button(pad_to_width(&text, self.printc_width, true), value);
    }

    pub fn printlc(&mut self, s: &str) {
        self.print(pad_to_width(s, self.printc_width, false));
    }

    pub fn printrc(&mut self, s: &str) {
        self.print(pad_to_width(s, self.printc_width, true));
    }
```

(c) After `fn is_left_alignment` (original lines 548-550) insert:

```rust

/// Transitional PRINTC/PRINTLC padding by Unicode display width — identical
/// to the former `pad` crate (`pad_to_width_with_alignment`): a string at or
/// beyond `width` columns is returned unchanged, otherwise spaces are added
/// before (`right == true`) or after it. Task 3 replaces this with cell-based
/// padding through `WidthTable`.
fn pad_to_width(s: &str, width: usize, right: bool) -> String {
    let cols = unicode_width::UnicodeWidthStr::width(s);
    if cols >= width {
        return s.to_owned();
    }
    let fill = width - cols;
    let mut out = String::with_capacity(s.len() + fill);
    if right {
        out.extend(std::iter::repeat(' ').take(fill));
        out.push_str(s);
    } else {
        out.push_str(s);
        out.extend(std::iter::repeat(' ').take(fill));
    }
    out
}
```

(d) Append at the end of the file (after the closing `}` of `button_test`, original line 799):

```rust

#[test]
fn pad_to_width_matches_former_pad_crate() {
    assert_eq!(pad_to_width("한", 4, true), "  한");
    assert_eq!(pad_to_width("한", 4, false), "한  ");
    assert_eq!(pad_to_width("abcdef", 4, true), "abcdef");
    assert_eq!(pad_to_width("", 2, false), "  ");
    assert_eq!(pad_to_width("ab", 2, true), "ab");
}
```

(e) Remove `pad` from `/home/riey/repos/erars/crates/erars-ui/Cargo.toml`:

```diff
 regex = "1.6.0"
-pad = "0.1.6"
 smol_str = "0.2.0"
```

- [ ] **Step 9: Run the whole `erars-ui` test suite and commit**

```
cargo test -p erars-ui
```

Expected: `test result: ok. 12 passed; 0 failed` (`issue_73`, `button_test`, `pad_to_width_matches_former_pad_crate`, and the 9 `width::` tests) and no warnings about `pad`. Verify the lock entry: `sed -n '/^name = "erars-ui"/,/^$/p' Cargo.lock` lists `"encoding_rs"` and `"unicode-width"` and no longer lists `"pad"` (`pad` itself stays in `Cargo.lock` for `erars-vm` until Task 4). Then:

```
cd /home/riey/repos/erars && git add Cargo.lock crates/erars-ui/Cargo.toml crates/erars-ui/src/lib.rs && git commit -m "refactor(ui): drop pad crate, keep PRINTC padding via unicode-width until cell padding lands"
```

Do not run `cargo test --all` here; `erars-vm` still uses `pad` and `VirtualConsole::new(printc_width, max_log)` unchanged, so the workspace still builds, but the full run belongs to Task 11.

---

### Task 2: Compiler — `Language::encoding()`, label fix, colour config keys, Emuera defaults, `PadStr` default Right

Spec: Component 2 bullets "`Language::encoding`", "Colours", "Config defaults", "`PadStr`" and the Findings bullet on the swapped `CHINESE_HANS`/`CHINESE_HANT` labels (`docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md:136-139, 226-228, 238-245, 255-267`). Emuera references: `ConfigData.cs:47-64` (defaults), `ConfigItem.cs:279-293` (`tryStringsToColor`: split at `,`, need ≥ 3 tokens, each trimmed `Int32` in 0..=255, else error), `ConfigData.GetConfigValueInERB` (GETCONFIG returns a colour as `((R*256)+G)*256+B`), `StrForm.cs:128` (`//標準RIGHT`).

**Files:**
- Modify `/home/riey/repos/erars/Cargo.toml` — `[workspace.dependencies]` block, lines 23-32 (only if T1 has not already added `encoding_rs = "0.8"`).
- Modify `/home/riey/repos/erars/crates/erars-compiler/Cargo.toml` — `[dependencies]`, lines 15-18 (workspace-dep block).
- Modify `/home/riey/repos/erars/crates/erars-compiler/src/parser.rs`:
  - lines 178-210 `EraConfigKey` (+3 variants),
  - lines 212-235 `EraConfig` (defaults, +3 fields),
  - lines 237-251 `get_config` (+3 arms),
  - lines 253-346 `from_text` (+3 arms; new helpers inserted before `impl EraConfig`),
  - lines 349-365 `Language` (+`PartialEq, Eq`, label fix, `encoding()`),
  - append two `#[cfg(test)]` modules at the end of the file (after line 1950).
- Modify `/home/riey/repos/erars/crates/erars-compiler/src/compiler.rs` — lines 2-5 (`use erars_ast::{…}`), line 253 (`pad_str`), append a `#[cfg(test)]` module after line 772 (end of file).
- Modify `/home/riey/repos/erars/Cargo.lock` — updated automatically by cargo when erars-compiler gains the dependency; commit it.
- Modify `/home/riey/repos/erars/tests/run_tests/basic/builtin_methods.out` line 21 (`GETCONFIG 19 D2Coding`): the `フォント名` default becomes the empty string (Step 14).
- Test: unit tests in-module (`parser::language_tests`, `parser::config_tests`, `compiler::tests`); `cargo test --test parser_test --test run_tests` as a regression check.

**Interfaces:**
- Consumes: workspace dependency line `encoding_rs = "0.8"` in `/home/riey/repos/erars/Cargo.toml` `[workspace.dependencies]` (added by T1; Step 1 verifies and adds it if missing). Nothing else from earlier tasks.
- Produces (used by T3/T4/T5/T10/T11):
  - `erars_compiler::Language::encoding(&self) -> &'static encoding_rs::Encoding` (Japanese → `SHIFT_JIS`, Korean → `EUC_KR`, ChineseHans → `GBK`, ChineseHant → `BIG5`); `Language` now also derives `PartialEq, Eq`.
  - `erars_compiler::EraConfig { fore_color: [u8; 3], bg_color: [u8; 3], focus_color: [u8; 3], … }` with defaults `[192,192,192]`, `[0,0,0]`, `[255,255,0]`; `printc_width` 25, `printc_count` 3, `window_width` 760, `window_height` 480, `font_family` `""`.
  - `erars_compiler::EraConfigKey::{ForeColor, BgColor, FocusColor}` (labels `文字色`, `背景色`, `選択中文字色`); `EraConfig::get_config` returns them as `Value::Int(0xRRGGBB)`.
  - `Instruction::pad_str(Alignment::Right)` is emitted for `{x, w}` / `%s, w%` without an explicit alignment.
  - Module-private helpers in `parser.rs`: `fn parse_color(&str) -> Option<[u8; 3]>`, `fn color_to_int([u8; 3]) -> i64`, `fn parse_color_or_default(&str, EraConfigKey, [u8; 3]) -> [u8; 3]`. `color_to_int` packs `0xRRGGBB` (`((R * 256) + G) * 256 + B`) — the spec's VM-wide colour-int convention (Component 2, "Colour integers are `0xRRGGBB`"). Today's `u32::from(Color)`, GETCOLOR/GETDEFCOLOR and SETCOLOR's single-int form use the opposite (little-endian, R low) packing; T4 Step 12 switches all of them to `0xRRGGBB` so `SETCOLOR GETCONFIG("文字色")` round-trips.

Verified against the pinned crates before writing (probe in `scratchpad/probe-plan-t2/`): encoding_rs 0.8.33 exposes `SHIFT_JIS`/`EUC_KR`/`GBK`/`BIG5` as `&'static Encoding`, and `Encoding: PartialEq + Eq + Debug` so `assert_eq!` on the references compiles; derivative 2.2.0 accepts `#[derivative(Default(value = "[192, 192, 192]"))]` on a `[u8; 3]` field; the `parse_color`/`color_to_int` bodies below ran with the listed expected values.

---

- [ ] **Step 1: Make `encoding_rs` a workspace dependency of erars-compiler**

Check whether T1 already added the workspace line:

```
grep -n 'encoding_rs' /home/riey/repos/erars/Cargo.toml
```

If it prints nothing, edit `/home/riey/repos/erars/Cargo.toml` `[workspace.dependencies]` (lines 23-32) so it reads:

```toml
[workspace.dependencies]
flume = "0.11.0"
serde = { version = "1", features = ["derive"] }
serde_json = "1"
strum = { version = "0.26", features = ["derive", "phf"] }
thiserror = "1"
anyhow = "1"
logos = { version = "0.14", features = ["logos-derive"] }
hashbrown = { version = "0.15", features = ["serde"] }
enum-map = { version = "2", features = ["serde"] }
encoding_rs = "0.8"
```

Then edit `/home/riey/repos/erars/crates/erars-compiler/Cargo.toml` lines 15-18 so the workspace-dep block reads:

```toml
serde.workspace = true
strum.workspace = true
thiserror.workspace = true
anyhow.workspace = true
encoding_rs.workspace = true
```

(Cargo.lock resolves `encoding_rs` to the already-locked 0.8.33; no new crate is downloaded.)

- [ ] **Step 2: Write the failing `Language` tests**

Append to the end of `/home/riey/repos/erars/crates/erars-compiler/src/parser.rs` (after the closing `}` of the last `impl<'p> ParserContext<'p>` block, currently line 1950):

```rust
#[cfg(test)]
mod language_tests {
    use super::{EraConfig, Language};

    #[test]
    fn labels_round_trip() {
        for (label, lang) in [
            ("JAPANESE", Language::Japanese),
            ("KOREAN", Language::Korean),
            ("CHINESE_HANS", Language::ChineseHans),
            ("CHINESE_HANT", Language::ChineseHant),
        ] {
            assert_eq!(label.parse::<Language>().unwrap(), lang, "{label}");
            assert_eq!(lang.to_string(), label);
        }
        assert!("ENGLISH".parse::<Language>().is_err());
    }

    #[test]
    fn encoding_per_language() {
        assert_eq!(Language::Japanese.encoding(), encoding_rs::SHIFT_JIS);
        assert_eq!(Language::Korean.encoding(), encoding_rs::EUC_KR);
        assert_eq!(Language::ChineseHans.encoding(), encoding_rs::GBK);
        assert_eq!(Language::ChineseHant.encoding(), encoding_rs::BIG5);
    }

    #[test]
    fn chinese_config_labels_select_the_right_code_page() {
        // Regression: the CHINESE_HANS / CHINESE_HANT strum labels were swapped,
        // so a simplified-Chinese game got Big5.
        let hans = EraConfig::from_text("内部で使用する東アジア言語:CHINESE_HANS\n").unwrap();
        assert_eq!(hans.lang, Language::ChineseHans);
        assert_eq!(hans.lang.encoding(), encoding_rs::GBK);

        let hant = EraConfig::from_text("内部で使用する東アジア言語:CHINESE_HANT\n").unwrap();
        assert_eq!(hant.lang, Language::ChineseHant);
        assert_eq!(hant.lang.encoding(), encoding_rs::BIG5);
    }
}
```

- [ ] **Step 3: Run the `Language` tests and watch them fail to compile**

```
cargo test -p erars-compiler language_tests
```

Expected: compilation fails with `error[E0599]: no method named `encoding` found for enum `Language`` and `error[E0369]: binary operation `==` cannot be applied to type `Language`` (no `PartialEq` yet).

- [ ] **Step 4: Fix the labels, add `PartialEq`/`Eq` and `Language::encoding()`**

Replace lines 349-365 of `/home/riey/repos/erars/crates/erars-compiler/src/parser.rs` (from `#[derive(Clone, Copy, Debug, EnumString, Display, Serialize, Deserialize)]` through the end of `impl Default for Language`) with:

```rust
/// The game's East-Asian language (`内部で使用する東アジア言語`). It decides the
/// legacy code page Emuera uses for its byte-counting string functions and,
/// through it, the console's cell-width table.
#[derive(Clone, Copy, Debug, PartialEq, Eq, EnumString, Display, Serialize, Deserialize)]
pub enum Language {
    #[strum(to_string = "JAPANESE")]
    Japanese,
    #[strum(to_string = "KOREAN")]
    Korean,
    #[strum(to_string = "CHINESE_HANS")]
    ChineseHans,
    #[strum(to_string = "CHINESE_HANT")]
    ChineseHant,
}

impl Default for Language {
    fn default() -> Self {
        Self::Japanese
    }
}

impl Language {
    /// Emuera's code page for this language — cp932 / cp949 / cp936 / cp950,
    /// i.e. WHATWG Shift_JIS / EUC-KR / GBK / Big5 in encoding_rs.
    pub fn encoding(&self) -> &'static encoding_rs::Encoding {
        match self {
            Language::Japanese => encoding_rs::SHIFT_JIS,
            Language::Korean => encoding_rs::EUC_KR,
            Language::ChineseHans => encoding_rs::GBK,
            Language::ChineseHant => encoding_rs::BIG5,
        }
    }
}
```

(serde uses the variant *names*, so swapping the variant order changes nothing on disk; only the strum labels were wrong.)

- [ ] **Step 5: Run the `Language` tests — PASS**

```
cargo test -p erars-compiler language_tests
```

Expected: `test parser::language_tests::labels_round_trip ... ok`, `... encoding_per_language ... ok`, `... chinese_config_labels_select_the_right_code_page ... ok`; `test result: ok. 3 passed`.

- [ ] **Step 6: Commit**

```
cd /home/riey/repos/erars && cargo fmt -p erars-compiler && git add Cargo.toml Cargo.lock crates/erars-compiler/Cargo.toml crates/erars-compiler/src/parser.rs && git commit -m "feat(compiler): Language::encoding() and fix swapped CHINESE_HANS/HANT labels"
```

- [ ] **Step 7: Write the failing config tests (defaults, colour keys, GETCONFIG packing)**

Append to the end of `/home/riey/repos/erars/crates/erars-compiler/src/parser.rs` (after the `language_tests` module):

```rust
#[cfg(test)]
mod config_tests {
    use super::{color_to_int, parse_color, EraConfig, EraConfigKey, Language};
    use erars_ast::Value;

    #[test]
    fn defaults_match_emuera() {
        // Emuera ConfigData.cs:47-64
        let c = EraConfig::default();
        assert_eq!(c.lang, Language::Japanese);
        assert_eq!(c.max_log, 500);
        assert_eq!(c.printc_count, 3);
        assert_eq!(c.printc_width, 25);
        assert_eq!(c.font_family, "");
        assert_eq!(c.font_size, 18);
        assert_eq!(c.line_height, 19);
        assert_eq!(c.window_width, 760);
        assert_eq!(c.window_height, 480);
        assert_eq!(c.fore_color, [192, 192, 192]);
        assert_eq!(c.bg_color, [0, 0, 0]);
        assert_eq!(c.focus_color, [255, 255, 0]);
    }

    #[test]
    fn from_text_parses_colour_keys() {
        let text = "\u{feff}内部で使用する東アジア言語:KOREAN\r\n\
                    文字色:255, 200,100\r\n\
                    背景色:16,16,16\r\n\
                    選択中文字色:0,255,255\r\n\
                    PRINTCの文字数:30\r\n";
        let c = EraConfig::from_text(text).unwrap();
        assert_eq!(c.lang, Language::Korean);
        assert_eq!(c.fore_color, [255, 200, 100]);
        assert_eq!(c.bg_color, [16, 16, 16]);
        assert_eq!(c.focus_color, [0, 255, 255]);
        assert_eq!(c.printc_width, 30);
    }

    #[test]
    fn invalid_colour_warns_and_keeps_default() {
        let c = EraConfig::from_text("文字色:300,0,0\n背景色:1,2\n選択中文字色:red\n").unwrap();
        assert_eq!(c.fore_color, [192, 192, 192]);
        assert_eq!(c.bg_color, [0, 0, 0]);
        assert_eq!(c.focus_color, [255, 255, 0]);
    }

    #[test]
    fn parse_color_follows_emuera_try_strings_to_color() {
        assert_eq!(parse_color("192,192,192"), Some([192, 192, 192]));
        assert_eq!(parse_color(" 1 , 2 , 3 "), Some([1, 2, 3]));
        // Emuera ignores tokens after the third.
        assert_eq!(parse_color("1,2,3,4"), Some([1, 2, 3]));
        assert_eq!(parse_color("1,2"), None);
        assert_eq!(parse_color("256,0,0"), None);
        assert_eq!(parse_color("-1,0,0"), None);
        assert_eq!(parse_color("red"), None);
        assert_eq!(parse_color(""), None);
    }

    #[test]
    fn get_config_packs_colours_as_rrggbb() {
        // Emuera ConfigData.GetConfigValueInERB: ((R * 256) + G) * 256 + B
        assert_eq!(color_to_int([192, 192, 192]), 0xC0C0C0);
        let c = EraConfig::default();
        assert_eq!(c.get_config(EraConfigKey::ForeColor), Value::Int(0xC0C0C0));
        assert_eq!(c.get_config(EraConfigKey::BgColor), Value::Int(0));
        assert_eq!(c.get_config(EraConfigKey::FocusColor), Value::Int(0xFFFF00));
        assert_eq!(c.get_config(EraConfigKey::PrintcWidth), Value::Int(25));
        assert_eq!(c.get_config(EraConfigKey::PrintcCount), Value::Int(3));
        assert_eq!(c.get_config(EraConfigKey::FontFamily), Value::String(String::new()));
    }

    #[test]
    fn colour_keys_parse_from_their_japanese_labels() {
        assert!(matches!("文字色".parse::<EraConfigKey>(), Ok(EraConfigKey::ForeColor)));
        assert!(matches!("背景色".parse::<EraConfigKey>(), Ok(EraConfigKey::BgColor)));
        assert!(matches!("選択中文字色".parse::<EraConfigKey>(), Ok(EraConfigKey::FocusColor)));
        assert_eq!(EraConfigKey::ForeColor.to_string(), "文字色");
    }
}
```

- [ ] **Step 8: Run the config tests and watch them fail to compile**

```
cargo test -p erars-compiler config_tests
```

Expected: compilation fails — rustc stops at name resolution with `error[E0432]: unresolved imports `super::color_to_int`, `super::parse_color`` (the later errors, `no variant or associated item named `ForeColor` found for enum `EraConfigKey`` and `no field `fore_color` on type `EraConfig``, appear once the helpers exist but the fields do not).

- [ ] **Step 9: Add the three colour keys to `EraConfigKey`**

Replace lines 178-210 of `/home/riey/repos/erars/crates/erars-compiler/src/parser.rs` (the whole `EraConfigKey` enum) with:

```rust
#[derive(Clone, Copy, Debug, Display, EnumString)]
#[strum(use_phf)]
pub enum EraConfigKey {
    #[strum(to_string = "内部で使用する東アジア言語")]
    Lang,

    #[strum(to_string = "表示するセーブデータ数")]
    SaveNos,

    #[strum(to_string = "フォント名")]
    FontFamily,

    #[strum(to_string = "フォントサイズ")]
    FontSize,

    #[strum(to_string = "一行の高さ")]
    LineHeight,

    #[strum(to_string = "PRINTCを並べる数")]
    PrintcCount,

    #[strum(to_string = "PRINTCの文字数")]
    PrintcWidth,

    #[strum(to_string = "履歴ログの行数")]
    MaxLog,

    #[strum(to_string = "ウィンドウ幅")]
    WindowWidth,

    #[strum(to_string = "ウィンドウ高さ")]
    WindowHeight,

    /// Emuera `ForeColor` — default text colour, `r,g,b`.
    #[strum(to_string = "文字色")]
    ForeColor,

    /// Emuera `BackColor` — console background, `r,g,b`.
    #[strum(to_string = "背景色")]
    BgColor,

    /// Emuera `FocusColor` — hovered-button text colour, `r,g,b`.
    #[strum(to_string = "選択中文字色")]
    FocusColor,
}
```

- [ ] **Step 10: Change `EraConfig` defaults and add the colour fields**

Replace lines 212-235 (the `EraConfig` struct, from `#[derive(Clone, Debug, derivative::Derivative, Serialize, Deserialize)]` through its closing `}`) with:

```rust
#[derive(Clone, Debug, derivative::Derivative, Serialize, Deserialize)]
#[derivative(Default)]
pub struct EraConfig {
    pub lang: Language,
    pub save_nos: usize,
    #[derivative(Default(value = "500"))]
    pub max_log: usize,
    /// `PRINTCを並べる数` — Emuera PrintCPerLine.
    #[derivative(Default(value = "3"))]
    pub printc_count: usize,
    /// `PRINTCの文字数` — Emuera PrintCLength (PRINTLC pads to this + 1).
    #[derivative(Default(value = "25"))]
    pub printc_width: usize,

    /// `フォント名`. Empty means "no configured family": the renderer's
    /// per-language font chain applies, and `SETFONT` without an argument
    /// resets to it.
    pub font_family: String,
    #[derivative(Default(value = "18"))]
    pub font_size: u32,
    #[derivative(Default(value = "19"))]
    pub line_height: u32,

    /// `ウィンドウ幅` — Emuera WindowX.
    #[derivative(Default(value = "760"))]
    pub window_width: u32,
    /// `ウィンドウ高さ` — Emuera WindowY (includes the input strip).
    #[derivative(Default(value = "480"))]
    pub window_height: u32,

    /// `文字色` — Emuera ForeColor.
    #[derivative(Default(value = "[192, 192, 192]"))]
    pub fore_color: [u8; 3],
    /// `背景色` — Emuera BackColor.
    #[derivative(Default(value = "[0, 0, 0]"))]
    pub bg_color: [u8; 3],
    /// `選択中文字色` — Emuera FocusColor.
    #[derivative(Default(value = "[255, 255, 0]"))]
    pub focus_color: [u8; 3],
}
```

(`font_family` loses its `#[derivative(Default(value = "String::from(\"D2Coding\")"))]` attribute; `String::default()` is `""`.)

- [ ] **Step 11: Add the colour helpers and the `get_config` arms**

Insert the three helpers directly above `impl EraConfig {` (currently line 237, now shifted by the previous edits):

```rust
/// Parse an Emuera colour value `r,g,b`: split at `,`, at least three tokens,
/// each trimmed and in 0..=255; extra tokens are ignored
/// (Emuera `ConfigItem.tryStringsToColor`).
fn parse_color(s: &str) -> Option<[u8; 3]> {
    let mut tokens = s.split(',');
    let mut out = [0u8; 3];
    for slot in out.iter_mut() {
        *slot = tokens.next()?.trim().parse::<u8>().ok()?;
    }
    Some(out)
}

/// `0xRRGGBB`, the form Emuera's `GETCONFIG` returns for colour items
/// (`ConfigData.GetConfigValueInERB`: `((R * 256) + G) * 256 + B`).
fn color_to_int(c: [u8; 3]) -> i64 {
    ((c[0] as i64) << 16) | ((c[1] as i64) << 8) | (c[2] as i64)
}

/// The parsed colour, or — on an invalid value — a warning and `default`
/// (Emuera aborts loading here; we keep the game runnable).
fn parse_color_or_default(value: &str, key: EraConfigKey, default: [u8; 3]) -> [u8; 3] {
    match parse_color(value) {
        Some(c) => c,
        None => {
            log::warn!("Invalid colour {value:?} for {key} (expected r,g,b); using {default:?}");
            default
        }
    }
}
```

Then replace the body of `get_config` (the `match key { … }`, currently lines 239-250) with:

```rust
        match key {
            EraConfigKey::PrintcCount => self.printc_count.into(),
            EraConfigKey::MaxLog => self.max_log.into(),
            EraConfigKey::PrintcWidth => self.printc_width.into(),
            EraConfigKey::Lang => self.lang.to_string().into(),
            EraConfigKey::SaveNos => self.save_nos.into(),
            EraConfigKey::FontFamily => self.font_family.clone().into(),
            EraConfigKey::FontSize => self.font_size.into(),
            EraConfigKey::LineHeight => self.line_height.into(),
            EraConfigKey::WindowWidth => self.window_width.into(),
            EraConfigKey::WindowHeight => self.window_height.into(),
            EraConfigKey::ForeColor => color_to_int(self.fore_color).into(),
            EraConfigKey::BgColor => color_to_int(self.bg_color).into(),
            EraConfigKey::FocusColor => color_to_int(self.focus_color).into(),
        }
```

- [ ] **Step 12: Parse the colour keys in `from_text`**

In `from_text`, inside `match key { … }`, directly after the `EraConfigKey::WindowHeight => { … }` arm (currently lines 330-337) and before the match's closing `}`, add:

```rust
                            EraConfigKey::ForeColor => {
                                ret.fore_color =
                                    parse_color_or_default(value, key, ret.fore_color);
                            }
                            EraConfigKey::BgColor => {
                                ret.bg_color = parse_color_or_default(value, key, ret.bg_color);
                            }
                            EraConfigKey::FocusColor => {
                                ret.focus_color =
                                    parse_color_or_default(value, key, ret.focus_color);
                            }
```

(`value: &str` and `key: EraConfigKey` are the bindings already in scope from `Ok(ConfigToken::Line((key, value)))` / `if let Ok(key) = key.parse()`.)

- [ ] **Step 13: Run the config tests — PASS — then the whole crate**

```
cargo test -p erars-compiler config_tests
cargo test -p erars-compiler
```

Expected: `test result: ok. 6 passed` for the filter, then all erars-compiler unit tests pass (language_tests 3 + config_tests 6 + any pre-existing).

- [ ] **Step 14: Regression check of the workspace tests that read `EraConfig`; update the one fixture that prints the font default**

```
cargo test --test parser_test --test run_tests 2>&1 | grep -E "^\[x\]|test result"
```

Expected: `parser_test` passes unchanged (its snapshots are AST-only). `run_tests` reports exactly one failing fixture, `[x] tests/run_tests/basic/builtin_methods.erb`: its line 64 prints `%GETCONFIGS("フォント名")%`, and the `.out` (line 21, `GETCONFIG 19 D2Coding`) still carries the old `"D2Coding"` default. No fixture uses PRINTC, width padding, GETDEFCOLOR or the train menu, so nothing else changes. Update that one line by pattern (the trailing space after `19` is significant — the family is now empty):

```
cd /home/riey/repos/erars && sed -i 's/^GETCONFIG 19 D2Coding$/GETCONFIG 19 /' tests/run_tests/basic/builtin_methods.out && grep -n '^GETCONFIG 19 ' tests/run_tests/basic/builtin_methods.out | cat -A
```

Expected: `21:GETCONFIG 19 $`. Re-run `cargo test --test run_tests 2>&1 | grep -E "^\[x\]|test result"` → no `[x]` line, `test result: ok. 1 passed`. If any *other* fixture fails, stop and report — it would mean a fixture depends on `printc_width`/`printc_count`, contradicting the spec.

- [ ] **Step 15: Commit**

```
cd /home/riey/repos/erars && cargo fmt -p erars-compiler && git add crates/erars-compiler/src/parser.rs tests/run_tests/basic/builtin_methods.out && git commit -m "feat(compiler): colour config keys (文字色/背景色/選択中文字色) and Emuera defaults 25/3/760x480"
```

- [ ] **Step 16: Write the failing `PadStr` default-alignment test**

Append to the end of `/home/riey/repos/erars/crates/erars-compiler/src/compiler.rs` (after `default_arg_command`, currently line 772):

```rust
#[cfg(test)]
mod tests {
    use super::compile_stmt;
    use crate::{HeaderInfo, ParserContext};
    use erars_ast::{Alignment, StrKey};
    use std::sync::Arc;

    /// Alignments of every `PadStr` instruction the statement compiles to.
    fn pad_aligns(src: &str) -> Vec<Alignment> {
        erars_ast::init_interner();
        let ctx = ParserContext::new(Arc::new(HeaderInfo::default()), StrKey::new("pad_test"));
        let body = ctx
            .parse_body_str(&format!("{src}\n"))
            .unwrap_or_else(|(err, span)| panic!("parse {src:?}: {err} at {span:?}"));
        let stmt = body.into_iter().next().expect("one statement");
        let insts = compile_stmt(stmt.0).unwrap();
        insts.iter().filter_map(|inst| inst.as_pad_str()).collect()
    }

    #[test]
    fn form_padding_defaults_to_right() {
        // Emuera StrForm.cs:128 `//標準RIGHT`: `{x, w}` / `%s, w%` pad on the left.
        assert_eq!(pad_aligns("PRINTFORM {12, 5}"), vec![Alignment::Right]);
        assert_eq!(pad_aligns("PRINTFORM %\"x\", 5%"), vec![Alignment::Right]);
    }

    #[test]
    fn form_padding_keeps_explicit_alignment() {
        assert_eq!(pad_aligns("PRINTFORM {12, 5, LEFT}"), vec![Alignment::Left]);
        assert_eq!(pad_aligns("PRINTFORM %\"x\", 5, CENTER%"), vec![Alignment::Center]);
        assert_eq!(pad_aligns("PRINTFORM {12, 5, RIGHT}"), vec![Alignment::Right]);
    }

    #[test]
    fn form_without_width_emits_no_pad_str() {
        assert_eq!(pad_aligns("PRINTFORM {12}"), Vec::<Alignment>::new());
        assert_eq!(pad_aligns("PRINTFORM %\"x\"%"), Vec::<Alignment>::new());
    }
}
```

- [ ] **Step 17: Run it and watch the default-alignment test fail**

```
cargo test -p erars-compiler compiler::tests
```

Expected: `form_padding_defaults_to_right` fails with `assertion `left == right` failed` / `left: [Left]` / `right: [Right]`; the other two pass.

- [ ] **Step 18: Default the alignment to Right in `push_form`**

In `/home/riey/repos/erars/crates/erars-compiler/src/compiler.rs` replace the import at lines 2-5 with:

```rust
use erars_ast::{
    Alignment, BinaryOperator, BuiltinCommand, BuiltinMethod, BuiltinVariable, Expr, FormExpr,
    FormText, Function, FunctionHeader, ScriptPosition, SelectCaseCond, Stmt, StmtWithPos, StrKey,
    Variable,
};
```

and replace line 253 (`self.push(Instruction::pad_str(align.unwrap_or_default()));`) with:

```rust
                    // Emuera pads on the left (right-aligns) when no LEFT/RIGHT is
                    // written (StrForm.cs:128 `//標準RIGHT`); CENTER is an erars extension.
                    self.push(Instruction::pad_str(align.unwrap_or(Alignment::Right)));
```

- [ ] **Step 19: Run the compiler tests — PASS**

```
cargo test -p erars-compiler
```

Expected: all pass, including `compiler::tests::form_padding_defaults_to_right ... ok`.

- [ ] **Step 20: Commit**

```
cd /home/riey/repos/erars && cargo fmt -p erars-compiler && git add crates/erars-compiler/src/compiler.rs && git commit -m "fix(compiler): default form-string padding alignment to Right like Emuera"
```

Notes for later tasks (no action here):
- T4 replaces the body of `VmContext::encoding` (`crates/erars-vm/src/context.rs:61-74`) with `self.config.lang.encoding()`, and reads `cfg.fore_color`/`bg_color`/`focus_color` in `console_config`.
- `SETFONT` without an argument (`crates/erars-vm/src/terminal_vm/executor.rs:2113-2116`) now passes `""` to `set_font`, which is the intended "reset to the default chain" (spec Component 2, config defaults).
- `crates/erars-renderer/src/main.rs:176-181` keeps reading `window_width`/`window_height`/`font_family`; the empty family is handled by T5's `FontChain::new`.

---

### Task 3: Console — `ConsoleConfig`, cell-based PRINTC/PRINTLC, `\n` splitting, default colour (`erars-ui`)

Spec: Component 2 (`erars-ui` bullets) and Testing §2 of
`docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md`. Emuera
references: `EmueraConsole.Print.cs` `Print` (split at `\n`, lines 311-328),
`PrintC`/`CreateTypeCString` (355-427: pad to PrintCLength 25 / 26, forced
button), `Process.ScriptProc.cs:118/135` (PRINTBUTTON* strip `\n`),
`PrintBar` (FontStyle.Regular for DRAWLINE).

**Files:**
- Modify `crates/erars-ui/Cargo.toml` — `[dependencies]` lines 7-22, `[dev-dependencies]` lines 24-25.
- Modify `crates/erars-loader/src/lib.rs` (line 22 insert, lines 77 and 120) and `tests/run_tests.rs` (line 8 insert, line 74) — temporary `console_config` helpers so the workspace compiles (Step 15; T4 replaces them).
- Modify `crates/erars-ui/src/lib.rs` (line numbers are from the pre-T1 file, 799 lines; T1 adds one `pub mod width;` line near the imports — add that offset):
  - lines 1-9 imports (drop `use pad::PadStr;`, add `use std::sync::Arc;`),
  - line 227/228 end of `impl ConsoleLine` (insert `push_forced_text`),
  - after line 238 (`ConsoleSerde`) insert `ConsoleConfig`,
  - lines 240-278 `VirtualConsole` struct + `new` (replace), then insert accessors + `pad_cells`,
  - lines 328-389 `print_plain` … `printrc` (replace),
  - lines 410-417 `draw_line` (replace),
  - after line 550 (`is_left_alignment`) insert `strip_newlines`,
  - append `mod console_tests` after line 799.
- Test: `crates/erars-ui/src/lib.rs` — `#[cfg(test)] mod console_tests` (in-module). The existing `make_test_line!` macro and `issue_73` / `button_test` (lines 576-799) build a `ConsoleLine` directly and need **no** change.
- Do not touch `crates/erars-ui/src/width.rs` beyond Step 2.

**Interfaces:**
- Consumes (T1): `erars_ui::width::WidthTable { new(&'static Encoding), char_cells(char)->u8, str_cells(&str)->usize }` declared by `pub mod width;` in `crates/erars-ui/src/lib.rs`; `WidthTable` implements `Clone` (derive) and `Debug` (T1's manual `impl fmt::Debug for WidthTable`, checked in Step 2) and is `Send + Sync` (it holds `&'static Encoding` + a byte table). Cargo: T1 added `encoding_rs.workspace = true` and `unicode-width = "0.1"` to `erars-ui` and `encoding_rs = "0.8"` to the workspace.
- Produces (used by T4, T9, T10, T11):
  - `erars_ui::ConsoleConfig { printc_width: usize, max_log: usize, encoding: &'static Encoding, fore_color: Color, bg_color: Color, focus_color: Color }` (`Clone, Copy, Debug, PartialEq, Eq`)
  - `VirtualConsole::new(&ConsoleConfig)`
  - `VirtualConsole::{cells(&str)->usize, char_cells(char)->u8, reset_color(), default_color()->Color}`
  - `Color(pub [u8; 3])` unchanged; `u32::from(Color)` is left as today's little-endian packing in this task — T4 Step 12 switches it to `0xRRGGBB` and adds `From<u32> for Color` (spec Component 2, "Colour integers"); T4 uses `u32::from(tx.default_color())` for GETDEFCOLOR.
  - Behaviour later tasks rely on: `print`/`print_line` never leave `\n` inside a part; `printlc`/`printrc`/`print_plain`/`draw_line`/`reuse_last_line` may; `ConsoleLinePart::Button` text never contains `\n`; `draw_line` parts always carry `FontStyle::NORMAL`.
- Internal helpers (private, named here so reviewers can find them): `ConsoleLine::push_forced_text`, `VirtualConsole::pad_cells`, `VirtualConsole::push_button`, free fn `strip_newlines`.

**Workspace note:** the `VirtualConsole::new(&ConsoleConfig)` signature change breaks the three remaining callers — `crates/erars-loader/src/lib.rs:77`, `:120` and `tests/run_tests.rs:74`. Steps 1–14 only run `cargo test -p erars-ui …` (which builds only `erars-ui` and its dependencies); Step 15 then gives each caller a small **temporary** `console_config(&EraConfig) -> ConsoleConfig` helper so the whole workspace compiles at this task's commit. T4 replaces those helpers with `erars_vm::console_config` (the spec's home for it) — the call sites already have the final shape `VirtualConsole::new(&console_config(&config))`.

- [ ] **Step 1: Cargo manifest — drop `pad`, add `serde_json` for the JSON test**

Edit `crates/erars-ui/Cargo.toml` so the two sections read exactly (T1 already added `encoding_rs`/`unicode-width` and removed `pad`; normally only the dev-dependency line is new):

```toml
[dependencies]
erars-ast = { path = "../erars-ast" }

anyhow.workspace = true
serde.workspace = true
encoding_rs.workspace = true

parking_lot = "0.12.1"
once_cell = "1.15.0"
regex = "1.6.0"
smol_str = "0.2.0"
log = "0.4.17"
time = "0.3.15"
bitflags = "2.3.1"
crossbeam-channel = "0.5.6"
serde_iter = "0.1.1"
unicode-width = "0.1"

[dev-dependencies]
k9 = "0.11.5"
serde_json.workspace = true
```

Diff against the pre-T1 file: `-pad = "0.1.6"`, `+encoding_rs.workspace = true`, `+unicode-width = "0.1"` (both from T1), `+serde_json.workspace = true` (this task). (`serde_json = "1"` is already a `[workspace.dependencies]` entry in the root `Cargo.toml:27`; `serde_json 1.0.112` is in `Cargo.lock`.)

- [ ] **Step 2: Confirm `WidthTable` is `Clone + Debug` (needed by `#[derive(Clone, Debug)]` on `VirtualConsole`)**

Run `grep -n "derive(Clone)\]\|impl fmt::Debug for WidthTable" crates/erars-ui/src/width.rs`. Expected: two hits — T1 derives `Clone` and writes `Debug` by hand (`impl fmt::Debug for WidthTable`, printing `WidthTable { encoding: "Shift_JIS", .. }`, pinned by T1's `debug_and_clone` test). **Do not add `Debug` to the derive list** — a derive next to the manual impl is a conflicting-implementations error. If the manual impl were missing (it is not, per T1), add `Debug` to the derive and nothing else. `width.rs` is otherwise untouched by this task.

- [ ] **Step 3: Write the failing console tests**

Append to the end of `crates/erars-ui/src/lib.rs` (after the existing `button_test`, line 799):

```rust
#[cfg(test)]
mod console_tests {
    use super::*;
    use erars_ast::{Alignment, Value};

    const FORE: Color = Color([192, 192, 192]);

    fn config(encoding: &'static encoding_rs::Encoding) -> ConsoleConfig {
        ConsoleConfig {
            printc_width: 25,
            max_log: 500,
            encoding,
            fore_color: FORE,
            bg_color: Color([0, 0, 0]),
            focus_color: Color([255, 255, 0]),
        }
    }

    fn jp() -> VirtualConsole {
        VirtualConsole::new(&config(encoding_rs::SHIFT_JIS))
    }

    fn kr() -> VirtualConsole {
        VirtualConsole::new(&config(encoding_rs::EUC_KR))
    }

    fn style() -> TextStyle {
        TextStyle {
            color: FORE,
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        }
    }

    fn spaces(n: usize) -> String {
        " ".repeat(n)
    }

    #[test]
    fn printc_pads_to_25_cells() {
        // half-width
        let mut tx = jp();
        tx.printrc("abc");
        assert_eq!(tx.last_line.to_string(), format!("{}abc", spaces(22)));
        assert_eq!(tx.cells(&tx.last_line.to_string()), 25);
        // full-width
        let mut tx = jp();
        tx.printrc("あい");
        assert_eq!(tx.last_line.to_string(), format!("{}あい", spaces(21)));
        assert_eq!(tx.cells(&tx.last_line.to_string()), 25);
        // mixed: a (1) + あ (2) + ─ U+2500 (2 in Shift_JIS)
        let mut tx = jp();
        tx.printrc("aあ─");
        assert_eq!(tx.last_line.to_string(), format!("{}aあ─", spaces(20)));
        // EUC-KR: Hangul is 2 cells
        let mut tx = kr();
        tx.printrc("한");
        assert_eq!(tx.last_line.to_string(), format!("{}한", spaces(23)));
    }

    #[test]
    fn printlc_pads_to_26_cells() {
        let mut tx = jp();
        tx.printlc("abc");
        assert_eq!(tx.last_line.to_string(), format!("abc{}", spaces(23)));
        let mut tx = jp();
        tx.printlc("あい");
        assert_eq!(tx.last_line.to_string(), format!("あい{}", spaces(22)));
        let mut tx = jp();
        tx.printlc("aあ");
        assert_eq!(tx.last_line.to_string(), format!("aあ{}", spaces(23)));
        assert_eq!(tx.cells(&tx.last_line.to_string()), 26);
    }

    #[test]
    fn printc_field_boundary() {
        // PRINTC: 24 cells -> one space, 25 -> unpadded
        let mut tx = jp();
        tx.printrc(&"a".repeat(24));
        assert_eq!(tx.last_line.to_string(), format!(" {}", "a".repeat(24)));
        let mut tx = jp();
        tx.printrc(&"a".repeat(25));
        assert_eq!(tx.last_line.to_string(), "a".repeat(25));
        // PRINTLC: 25 cells -> one space, 26 -> unpadded
        let mut tx = jp();
        tx.printlc(&"a".repeat(25));
        assert_eq!(tx.last_line.to_string(), format!("{} ", "a".repeat(25)));
        let mut tx = jp();
        tx.printlc(&"a".repeat(26));
        assert_eq!(tx.last_line.to_string(), "a".repeat(26));
    }

    #[test]
    fn printc_overlong_unpadded() {
        let mut tx = jp();
        tx.printrc(&"a".repeat(30));
        assert_eq!(tx.last_line.to_string(), "a".repeat(30));
        let mut tx = jp();
        tx.printlc(&"あ".repeat(14)); // 28 cells
        assert_eq!(tx.last_line.to_string(), "あ".repeat(14));
    }

    #[test]
    fn printc_button_variants() {
        let mut tx = jp();
        tx.print_button_rc("[1] x".into(), Value::Int(1));
        assert_eq!(
            tx.last_line.parts,
            vec![ConsoleLinePart::Button(
                vec![(format!("{}[1] x", spaces(20)), style())],
                0,
                Value::Int(1)
            )]
        );
        let mut tx = jp();
        tx.print_button_lc("[1] x".into(), Value::Int(1));
        assert_eq!(
            tx.last_line.parts,
            vec![ConsoleLinePart::Button(
                vec![(format!("[1] x{}", spaces(21)), style())],
                0,
                Value::Int(1)
            )]
        );
        // overlong button text is unpadded
        let mut tx = jp();
        tx.print_button_rc("a".repeat(25), Value::Int(1));
        assert_eq!(tx.last_line.to_string(), "a".repeat(25));
    }

    #[test]
    fn printc_item_never_merges_with_neighbours() {
        // text printed before a PRINTC item stays its own Text part
        let mut tx = jp();
        tx.print("abc".into());
        tx.printrc("[1] x");
        assert_eq!(
            tx.last_line.parts,
            vec![
                ConsoleLinePart::Text("abc".into(), style()),
                ConsoleLinePart::Button(
                    vec![(format!("{}[1] x", spaces(20)), style())],
                    0,
                    Value::Int(1)
                ),
            ]
        );
        assert_eq!(tx.last_line.button_start, None);

        // a pending '[' from earlier text does not fuse with the PRINTC item
        let mut tx = jp();
        tx.print("[".into());
        assert_eq!(tx.last_line.button_start, Some(0));
        tx.printrc("1] x");
        assert_eq!(
            tx.last_line.parts,
            vec![
                ConsoleLinePart::Text("[".into(), style()),
                ConsoleLinePart::Text(format!("{}1] x", spaces(21)), style()),
            ]
        );
        assert_eq!(tx.last_line.button_start, None);

        // train-menu shape (`{name}[{no:3}]`): every item is its own button
        let mut tx = jp();
        tx.printrc("A[  1]");
        tx.printrc("B[  2]");
        assert_eq!(
            tx.last_line.parts,
            vec![
                ConsoleLinePart::Button(
                    vec![(format!("{}A[  1]", spaces(19)), style())],
                    0,
                    Value::Int(1)
                ),
                ConsoleLinePart::Button(
                    vec![(format!("{}B[  2]", spaces(19)), style())],
                    0,
                    Value::Int(2)
                ),
            ]
        );
    }

    #[test]
    fn print_line_splits_at_newline() {
        let mut tx = jp();
        tx.print_line("a\nb".into());
        assert_eq!(tx.line_count(), 2);
        assert_eq!(tx.lines[0].to_string(), "a");
        assert_eq!(tx.lines[1].to_string(), "b");
        assert!(tx.line_is_empty());
        tx.clear_line(1);
        assert_eq!(tx.line_count(), 1);
        assert_eq!(tx.lines[0].to_string(), "a");
    }

    #[test]
    fn newline_split_keeps_alignment_per_logical_line() {
        let mut tx = jp();
        tx.print("a\nb".into());
        tx.set_align(Alignment::Right);
        tx.print_line("c".into());
        assert_eq!(tx.lines[0].align, Alignment::Left);
        assert_eq!(tx.lines[0].to_string(), "a");
        assert_eq!(tx.lines[1].align, Alignment::Right);
        assert_eq!(tx.lines[1].to_string(), "bc");
    }

    #[test]
    fn print_edge_newlines_and_empty() {
        let mut tx = jp();
        tx.print("a\n".into());
        assert_eq!(tx.lines.len(), 1);
        assert_eq!(tx.lines[0].to_string(), "a");
        assert!(tx.line_is_empty());

        let mut tx = jp();
        tx.print("\nb".into());
        assert_eq!(tx.lines.len(), 1);
        assert!(tx.lines[0].is_empty());
        assert_eq!(tx.last_line.to_string(), "b");

        let mut tx = jp();
        tx.print(String::new());
        assert!(tx.line_is_empty());
        tx.print_line(String::new());
        assert_eq!(tx.line_count(), 1);
        assert!(tx.lines[0].is_empty());
    }

    #[test]
    fn print_button_strips_newlines() {
        let mut tx = jp();
        tx.print_button("x\ny".into(), Value::Int(3));
        assert_eq!(
            tx.last_line.parts,
            vec![ConsoleLinePart::Button(
                vec![("xy".into(), style())],
                0,
                Value::Int(3)
            )]
        );
        let mut tx = jp();
        tx.print_button_lc("x\ny\n".into(), Value::Int(3));
        assert_eq!(tx.last_line.to_string(), format!("xy{}", spaces(24)));
    }

    #[test]
    fn printrc_keeps_newline_inside_part() {
        let mut tx = jp();
        tx.printrc("a\nb");
        assert_eq!(tx.lines.len(), 0);
        assert_eq!(tx.last_line.parts.len(), 1);
        let s = tx.last_line.to_string();
        assert!(s.ends_with("a\nb"), "{s:?}");
        // '\n' is a control character: 0 cells (spec Component 1 step 1),
        // so the item is 2 cells and gets 23 pad spaces
        assert_eq!(s, format!("{}a\nb", spaces(23)));
    }

    #[test]
    fn draw_line_forces_normal_style() {
        let mut tx = jp();
        tx.set_style(FontStyle::BOLD | FontStyle::UNDERLINE);
        tx.set_color(1, 2, 3);
        tx.set_font("Foo".into());
        tx.draw_line("-".into());
        assert_eq!(
            tx.lines[0].parts,
            vec![ConsoleLinePart::Line(
                "-".into(),
                TextStyle {
                    color: Color([1, 2, 3]),
                    font_family: "Foo".into(),
                    font_style: FontStyle::NORMAL,
                }
            )]
        );
        // the console's own style is untouched
        assert_eq!(tx.style(), FontStyle::BOLD | FontStyle::UNDERLINE);
    }

    #[test]
    fn reset_color_restores_configured_colour() {
        let mut tx = jp();
        assert_eq!(tx.style.color, FORE);
        assert_eq!(tx.default_color(), FORE);
        assert_eq!(tx.bg_color, Color([0, 0, 0]));
        assert_eq!(tx.hl_color, Color([255, 255, 0]));
        tx.set_color(1, 2, 3);
        assert_eq!(tx.style.color, Color([1, 2, 3]));
        tx.reset_color();
        assert_eq!(tx.style.color, FORE);
        assert_eq!(tx.color(), u32::from(FORE));
    }

    #[test]
    fn cells_follow_the_configured_encoding() {
        let tx = jp();
        assert_eq!(tx.char_cells('a'), 1);
        assert_eq!(tx.char_cells('あ'), 2);
        assert_eq!(tx.cells("aあ"), 3);
        assert_eq!(tx.cells(""), 0);
        let tx = kr();
        assert_eq!(tx.cells("한"), 2);
        // the Arc-shared table survives Clone
        let tx2 = tx.clone();
        assert_eq!(tx2.cells("한"), 2);
    }

    #[test]
    fn console_serde_json() {
        let mut tx = jp();
        tx.print_line("a\nb".into());
        tx.set_align(Alignment::Right);
        tx.print("c".into());
        tx.print_button("[1] go".into(), Value::Int(1));
        let json = serde_json::to_value(tx.make_serializable(0)).unwrap();
        assert_eq!(
            json,
            serde_json::json!({
                "rebuild": false,
                "bg_color": [0, 0, 0],
                "hl_color": [255, 255, 0],
                "last_line": {
                    "align": "Right",
                    "parts": [
                        {"Text": ["c", {"color": [192, 192, 192]}]},
                        {"Button": [[["[1] go", {"color": [192, 192, 192]}]], 0, {"Int": 1}]}
                    ]
                },
                "lines": [
                    {"parts": [{"Text": ["a", {"color": [192, 192, 192]}]}]},
                    {"parts": [{"Text": ["b", {"color": [192, 192, 192]}]}]}
                ]
            })
        );
    }
}
```

- [ ] **Step 4: Run the tests and watch them fail to compile**

`cargo test -p erars-ui console_tests`

Expected: compilation errors from the new module, e.g. `error[E0422]: cannot find struct, variant or union type \`ConsoleConfig\` in this scope`, `error[E0061]: this function takes 2 arguments but 1 argument was supplied` (at `VirtualConsole::new(&config(...))`), `error[E0599]: no method named \`cells\` found`, `no method named \`default_color\``, `no method named \`reset_color\``. (If `pad` was removed in Step 1 you also see `error[E0432]: unresolved import \`pad\`` — expected until Step 5.)

- [ ] **Step 5: Imports**

Replace lines 1-9 of `crates/erars-ui/src/lib.rs` (keep T1's `pub mod width;` wherever it is — do not add a second one):

```rust
use erars_ast::{Alignment, Value};
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;
use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::sync::Arc;
use std::time::Instant;
```

(`use pad::PadStr;` is gone; `WidthTable` is referred to as `width::WidthTable` below, so no import is needed and there is no clash with any `pub use` T1 may have added.)

- [ ] **Step 6: Add `ConsoleConfig`**

Insert after the `ConsoleSerde` struct (after line 238, before the `/// Used by ui backend` comment):

```rust
/// Everything `VirtualConsole::new` needs from `EraConfig`; built by
/// `erars_vm::console_config` (spec Component 2).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ConsoleConfig {
    /// PRINTC field width in half-width cells (Emuera PrintCLength, 25);
    /// PRINTLC uses `printc_width + 1`.
    pub printc_width: usize,
    /// Number of finished lines kept before the oldest is dropped.
    pub max_log: usize,
    /// Game encoding (`Language::encoding()`): decides half/full cells.
    pub encoding: &'static encoding_rs::Encoding,
    /// Default text colour (`文字色`, Emuera ForeColor 192,192,192).
    pub fore_color: Color,
    /// Background colour (`背景色`, Emuera BackColor 0,0,0).
    pub bg_color: Color,
    /// Hovered-button colour (`選択中文字色`, Emuera FocusColor 255,255,0).
    pub focus_color: Color,
}
```

(`&'static encoding_rs::Encoding` is `Copy`, `Debug` (prints `Encoding { Shift_JIS }`), `PartialEq`, `Eq` — `encoding_rs-0.8.33/src/lib.rs:3427-3460`.)

- [ ] **Step 7: Add `ConsoleLine::push_forced_text`**

Inside `impl ConsoleLine`, directly after the end of `push_text` (after line 227, before the impl's closing brace on line 228):

```rust
    /// Emuera `PrintStringBuffer.Append(str, style, force_button = true)`
    /// (used by PRINTC/PRINTLC): the text is button-scanned on its own and
    /// appended as its own part(s). Nothing already on the line is drained
    /// into a button made from `text`, a pending `[` (`button_start`) from
    /// earlier text is forgotten, and no `[` inside `text` is left pending.
    fn push_forced_text(&mut self, input_gen: u32, text: String, style: &TextStyle) {
        let mut item = ConsoleLine::default();
        item.push_text(input_gen, text, style);
        self.parts.extend(item.parts);
        self.button_start = None;
    }
```

- [ ] **Step 8: Replace the `VirtualConsole` struct and `new`**

Replace lines 240-278 (`/// Used by ui backend` … the closing brace of `new`) with:

```rust
/// Used by ui backend
#[derive(Clone, Debug)]
pub struct VirtualConsole {
    pub timeout: Option<(Instant, u32, Value)>,
    pub lines: VecDeque<ConsoleLine>,
    pub last_line: ConsoleLine,
    pub style: TextStyle,
    pub bg_color: Color,
    pub hl_color: Color,
    pub skipdisp: bool,
    pub need_rebuild: bool,
    pub input_gen: u32,

    max_log: usize,
    printc_width: usize,
    default_color: Color,
    widths: Arc<width::WidthTable>,
    pub top_index: usize,
}

impl VirtualConsole {
    pub fn new(cfg: &ConsoleConfig) -> Self {
        Self {
            input_gen: 0,
            timeout: None,
            printc_width: cfg.printc_width,
            need_rebuild: false,
            lines: VecDeque::with_capacity(cfg.max_log),
            last_line: ConsoleLine::default(),
            max_log: cfg.max_log,
            style: TextStyle {
                color: cfg.fore_color,
                font_family: "".into(),
                font_style: FontStyle::NORMAL,
            },
            default_color: cfg.fore_color,
            bg_color: cfg.bg_color,
            hl_color: cfg.focus_color,
            skipdisp: false,
            top_index: 0,
            widths: Arc::new(width::WidthTable::new(cfg.encoding)),
        }
    }

    /// Half-width cells of `s` in the game encoding — the one width function
    /// shared with the VM (STRLEN, PadStr) and the renderer grid.
    pub fn cells(&self, s: &str) -> usize {
        self.widths.str_cells(s)
    }

    /// Cells of one character: 0, 1 or 2.
    pub fn char_cells(&self, c: char) -> u8 {
        self.widths.char_cells(c)
    }

    /// The configured text colour (`文字色`); PRINTD and GETDEFCOLOR use it.
    pub fn default_color(&self) -> Color {
        self.default_color
    }

    /// RESETCOLOR: back to the configured text colour.
    pub fn reset_color(&mut self) {
        self.style.color = self.default_color;
    }

    /// Emuera `CreateTypeCString`: pad `s` with spaces into a field of
    /// `width` cells — after the text when `left` (PRINTLC), before it
    /// otherwise (PRINTC). Text at or beyond `width` cells is returned as is.
    fn pad_cells(&self, s: &str, width: usize, left: bool) -> String {
        let cells = self.widths.str_cells(s);
        if cells >= width {
            return s.to_owned();
        }
        let pad = width - cells;
        let mut out = String::with_capacity(s.len() + pad);
        if left {
            out.push_str(s);
            out.extend(std::iter::repeat(' ').take(pad));
        } else {
            out.extend(std::iter::repeat(' ').take(pad));
            out.push_str(s);
        }
        out
    }
```

(The struct field order is otherwise unchanged; `make_serializable`, `lines_from`, `set_skipdisp`, … that followed `new` stay as they are.)

- [ ] **Step 9: Replace the print family (`print_plain` … `printrc`)**

Replace lines 328-389 (from `pub fn print_plain` through the closing brace of `printrc`; `reuse_last_line` above and `push_line` below stay) with:

```rust
    pub fn print_plain(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.last_line.push_plain_text(s, &self.style);
    }

    /// PRINT: every `\n` ends the current logical line, exactly like Emuera's
    /// `EmueraConsole.Print`, so LINECOUNT / CLEARLINE / ALIGNMENT see it.
    /// Empty segments push nothing (Emuera returns early on an empty string).
    pub fn print(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        if !s.contains('\n') {
            if !s.is_empty() {
                self.last_line.push_text(self.input_gen, s, &self.style);
            }
            return;
        }
        for (i, seg) in s.split('\n').enumerate() {
            if i > 0 {
                self.push_line();
            }
            if !seg.is_empty() {
                self.last_line.push_text(self.input_gen, seg.to_owned(), &self.style);
            }
        }
    }

    pub fn print_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        self.print(s);
        self.push_line();
    }

    fn push_button(&mut self, text: String, value: Value) {
        let style = self.style.clone();
        self.last_line.button_start = None;
        self.last_line.parts.push(ConsoleLinePart::Button(
            vec![(text, style)],
            self.input_gen,
            value,
        ));
    }

    /// PRINTBUTTON: `\n` is removed (Emuera Process.ScriptProc.cs:118), the
    /// rest becomes one button part.
    pub fn print_button(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        self.push_button(strip_newlines(text), value);
    }

    /// PRINTBUTTONLC: `\n` removed, then left-aligned in `printc_width + 1` cells.
    pub fn print_button_lc(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        let text = strip_newlines(text);
        let padded = self.pad_cells(&text, self.printc_width + 1, true);
        self.push_button(padded, value);
    }

    /// PRINTBUTTONC: `\n` removed, then right-aligned in `printc_width` cells.
    pub fn print_button_rc(&mut self, text: String, value: Value) {
        if self.skipdisp {
            return;
        }
        let text = strip_newlines(text);
        let padded = self.pad_cells(&text, self.printc_width, false);
        self.push_button(padded, value);
    }

    /// PRINTLC: left-aligned in `printc_width + 1` cells (Emuera 26) and
    /// pushed as its own part: `\n` is kept and nothing before or inside the
    /// item merges into a button with it.
    pub fn printlc(&mut self, s: &str) {
        if self.skipdisp {
            return;
        }
        let padded = self.pad_cells(s, self.printc_width + 1, true);
        self.last_line.push_forced_text(self.input_gen, padded, &self.style);
    }

    /// PRINTC: right-aligned in `printc_width` cells (Emuera 25); see `printlc`.
    pub fn printrc(&mut self, s: &str) {
        if self.skipdisp {
            return;
        }
        let padded = self.pad_cells(s, self.printc_width, false);
        self.last_line.push_forced_text(self.input_gen, padded, &self.style);
    }
```

- [ ] **Step 10: Replace `draw_line`**

Replace lines 410-417 with:

```rust
    /// DRAWLINE / CUSTOMDRAWLINE: Emuera draws the rule with
    /// `FontStyle.Regular` but keeps the current colour and family.
    pub fn draw_line(&mut self, s: String) {
        if self.skipdisp {
            return;
        }
        let style = TextStyle {
            font_style: FontStyle::NORMAL,
            ..self.style.clone()
        };
        self.last_line.parts.push(ConsoleLinePart::Line(s, style));
        self.push_line();
    }
```

- [ ] **Step 11: Add `strip_newlines`**

Insert after `fn is_left_alignment` (after line 550):

```rust
/// PRINTBUTTON / PRINTBUTTONC / PRINTBUTTONLC drop every `\n`
/// (Emuera Process.ScriptProc.cs:118/135).
fn strip_newlines(text: String) -> String {
    if text.contains('\n') {
        text.replace('\n', "")
    } else {
        text
    }
}
```

- [ ] **Step 12: Confirm nothing references `pad` any more**

`grep -n "pad::\|PadStr\|pad_to_width" crates/erars-ui/src/lib.rs` — expected: no output.

- [ ] **Step 13: Run the console tests**

`cargo test -p erars-ui console_tests`

Expected: `test result: ok. 15 passed; 0 failed` — the 15 tests are `printc_pads_to_25_cells`, `printlc_pads_to_26_cells`, `printc_field_boundary`, `printc_overlong_unpadded`, `printc_button_variants`, `printc_item_never_merges_with_neighbours`, `print_line_splits_at_newline`, `newline_split_keeps_alignment_per_logical_line`, `print_edge_newlines_and_empty`, `print_button_strips_newlines`, `printrc_keeps_newline_inside_part`, `draw_line_forces_normal_style`, `reset_color_restores_configured_colour`, `cells_follow_the_configured_encoding`, `console_serde_json`.

If `printrc_keeps_newline_inside_part` fails with 24 spaces instead of 23, T1's `char_cells('\n')` is not 0 — fix `width.rs` (spec Component 1 step 1: `width(c) == None` → 0), not this test.

- [ ] **Step 14: Run the whole crate (existing k9 button tests + T1 width tests must still pass)**

`cargo test -p erars-ui`

Expected: `test result: ok.` with 0 failed; `issue_73` and `button_test` unchanged (their `make_test_line!` macro builds a `ConsoleLine` directly), plus T1's `width::` tests.

- [ ] **Step 15: Keep the workspace compiling — temporary `console_config` helpers at the three call sites**

`crates/erars-loader/src/lib.rs` and `tests/run_tests.rs` still call `VirtualConsole::new(printc_width, max_log)`. Give each file a private helper with the exact body `erars_vm::console_config` will have (T4 moves it there and deletes these copies), so the call sites already take their final form.

(a) `crates/erars-loader/src/lib.rs`: directly after the `use` block (after line 22, `use hashbrown::HashMap;`) insert

```rust

/// Console construction parameters from `emuera.config`. Temporary copy —
/// the next task provides `erars_vm::console_config` and deletes this one.
fn console_config(config: &EraConfig) -> erars_ui::ConsoleConfig {
    erars_ui::ConsoleConfig {
        printc_width: config.printc_width,
        max_log: config.max_log,
        encoding: config.lang.encoding(),
        fore_color: erars_ui::Color(config.fore_color),
        bg_color: erars_ui::Color(config.bg_color),
        focus_color: erars_ui::Color(config.focus_color),
    }
}
```

then replace line 77 (`let vconsole = VirtualConsole::new(config.printc_width, config.max_log);`) with

```rust
    let vconsole = VirtualConsole::new(&console_config(&config));
```

and line 120 (`let mut tx = VirtualConsole::new(config.printc_width, config.max_log);`, where `config` is an `Arc<EraConfig>` — `&config` deref-coerces) with

```rust
    let mut tx = VirtualConsole::new(&console_config(&config));
```

(b) `tests/run_tests.rs`: after line 8 (`mod test_util;`) insert the same helper (this file is replaced wholesale by T4, which drops it again):

```rust

/// Temporary copy of the console-config mapping; T4 replaces this file and
/// uses `erars_vm::console_config`.
fn console_config(config: &EraConfig) -> erars_ui::ConsoleConfig {
    erars_ui::ConsoleConfig {
        printc_width: config.printc_width,
        max_log: config.max_log,
        encoding: config.lang.encoding(),
        fore_color: erars_ui::Color(config.fore_color),
        bg_color: erars_ui::Color(config.bg_color),
        focus_color: erars_ui::Color(config.focus_color),
    }
}
```

and replace line 74 (`let mut tx = VirtualConsole::new(ctx.config.printc_width, ctx.config.max_log);`) with

```rust
    let mut tx = VirtualConsole::new(&console_config(&ctx.config));
```

Run: `cargo check --all --all-targets 2>&1 | grep -E "^error|Finished"` → only `Finished …` (no `error`). Then `cargo test --test run_tests 2>&1 | grep -E "^\[x\]|test result"` → no `[x]`, `test result: ok. 1 passed` — the existing fixtures print the same text through the new console (no fixture uses PRINTC, embedded `\n` or GETDEFCOLOR; the default text colour is not printed).

- [ ] **Step 16: Commit**

```
git add crates/erars-ui/Cargo.toml crates/erars-ui/src/lib.rs crates/erars-loader/src/lib.rs tests/run_tests.rs Cargo.lock && git commit -m "feat(ui): ConsoleConfig, cell-based PRINTC/PRINTLC padding, newline splitting

VirtualConsole::new(&ConsoleConfig) holds an Arc<WidthTable> built from the
game encoding and exposes cells()/char_cells(); PRINTC/PRINTLC pad to 25/26
cells and are pushed as forced parts; print() splits at \\n into logical
lines; PRINTBUTTON* strip \\n; draw_line stores FontStyle::NORMAL;
default_color()/reset_color() carry the configured 文字色. Drops the pad
crate. erars-loader and tests/run_tests build the ConsoleConfig through a
temporary local helper until erars_vm::console_config lands.

Claude-Session: https://claude.ai/code/session_01XEtVTsN59k1K3cegBL8mfx"
```

(`Cargo.lock` changes only if `pad` was still listed; include it when `git status` shows it modified.)

---

### Task 4: VM + harness

**Context for the executor.** T1–T3 have landed: `erars_ui::width::WidthTable`
exists, `EraConfig` has `fore_color`/`bg_color`/`focus_color` (`[u8; 3]`) and
`Language::encoding()`, and `VirtualConsole::new` now takes
`&erars_ui::ConsoleConfig`. T3 kept the workspace compiling by giving
`crates/erars-loader/src/lib.rs` and `tests/run_tests.rs` a private, temporary
`fn console_config(&EraConfig) -> ConsoleConfig`; their call sites already read
`VirtualConsole::new(&console_config(&config))`. This task moves that function
to its spec'd home, `erars_vm::console_config`, and deletes the two copies.
Line numbers are from the files **before** this task (the loader's are shifted
by T3's 14 inserted lines — match on the quoted text); inside one file apply
the edits bottom-up.

**Files:**
- Modify `crates/erars-vm/src/lib.rs` — line 8 (`use erars_ui::{…}`), line 11 (`use pad::PadStr;`), append after line 26 (`pub use erars_compiler::{…};`)
- Modify `crates/erars-vm/src/context.rs` — lines 61–74 (`pub fn encoding`)
- Modify `crates/erars-vm/src/terminal_vm.rs` — line 1 (`mod executor;`)
- Create `crates/erars-vm/src/terminal_vm/cells.rs`
- Modify `crates/erars-vm/src/terminal_vm/executor.rs` — line 10 (imports); 195–202 (PRINTD `DEFAULT_COLOR`); 339–352 (`as_pad_str`); 1046–1059 (`StrFind`); 1073–1077 (`StrLenS`); 1319–1355 (`SubString`); 1434–1441 (`GetDefColor`/`GetDefBgColor`); 2181–2186 (`ResetColor`/`ResetBgColor`)
- Modify `crates/erars-vm/Cargo.toml` — lines 22–24 (`unicode-width`/`encoding_rs`/`pad`), line 30 (`twoway`), lines 41–42 (`[dev-dependencies]`)
- Modify `Cargo.lock` — regenerated by cargo (deps dropped/added); commit it
- Modify `crates/erars-loader/src/lib.rs` — line 21 (import) and T3's temporary `fn console_config` (deleted)
- Modify `crates/erars-ui/src/lib.rs` — `impl From<Color> for u32` (lines 29–33 of the pre-T1 file; locate by content) becomes `0xRRGGBB`, and a new `impl From<u32> for Color` is added next to it (Step 12)
- Modify `tests/run_tests.rs` — whole file replaced (drops T3's temporary helper)
- Verify `tests/run_tests/basic/builtin_methods.out` line 21 (`GETCONFIG 19 ` — updated by T2 Step 14; Step 6 only checks it)
- Create `tests/run_tests/jp/emuera.config`, `tests/run_tests/jp/lang.erb` + `.out`
- Create `tests/run_tests/basic/{strlen_cells,substring_cells,strfind_cells,padstr,getdefcolor,printc,drawline,alignment,print_newline,printbutton_newline}.erb` + `.out`
- Create `tests/run_tests/jp/{strlen_cells,substring_cells,strfind_cells,padstr,getdefcolor,printc}.erb` + `.out`
- Create `crates/erars-vm/tests/train_menu.rs`
- Test: `cargo test -p erars-vm` (unit tests in `lib.rs` and `terminal_vm/cells.rs`, integration test `train_menu`), `cargo test --test run_tests` (fixtures), `cargo check -p erars-loader`

**Interfaces:**
- Consumes (T1, in unit tests only): `erars_ui::width::WidthTable { new(&'static Encoding), char_cells(char)->u8, str_cells(&str)->usize }`
- Consumes (T2): `Language::encoding(&self)->&'static encoding_rs::Encoding`; `EraConfig.{fore_color,bg_color,focus_color}: [u8;3]`; config keys `文字色`/`背景色`/`選択中文字色` (`r,g,b`); defaults `printc_width 25 / printc_count 3 / font_family ""`; `Instruction::pad_str(Alignment::Right)` emitted for `{x, w}` / `%s, w%` without an alignment
- Consumes (T3): `erars_ui::ConsoleConfig { printc_width: usize, max_log: usize, encoding: &'static Encoding, fore_color: Color, bg_color: Color, focus_color: Color }`; `VirtualConsole::new(&ConsoleConfig)`; `VirtualConsole::{cells(&str)->usize, char_cells(char)->u8, reset_color(), default_color()->Color}`; `Color(pub [u8;3])` + `impl From<Color> for u32` (little-endian until Step 12 flips it to `0xRRGGBB`); `print()` splits at `\n`; `printrc`/`printlc` pad to 25/26 cells and push their own part; `print_button*` strip `\n`; `draw_line` text unchanged
- Produces:
  - `erars_vm::console_config(cfg: &EraConfig) -> erars_ui::ConsoleConfig` (used by `erars-loader`, `tests/run_tests.rs`, T11's `tests/tui.rs`)
  - `erars_vm::VmContext::encoding(&self) -> &'static encoding_rs::Encoding` (same signature; now delegates to `Language::encoding`)
  - `u32::from(erars_ui::Color)` = `0xRRGGBB` (`(r << 16) | (g << 8) | b`) and `erars_ui::Color::from(u32)` decodes the same way (Step 12, spec Component 2). Every colour int the VM reads or writes — GETCOLOR/GETBGCOLOR/GETFOCUSCOLOR/GETDEFCOLOR/GETDEFBGCOLOR/`GETCONFIG("文字色")`, SETCOLOR/SETBGCOLOR's single-int form, the PRINTD restore — uses this packing; `SETCOLOR r, g, b` and SETCOLORBYNAME are unaffected. Nothing outside `erars-ui`/`erars-vm` calls `u32::from(Color)` (checked: `grep -rn 'u32::from(' crates` hits only `VirtualConsole::{color,bg_color,hl_color}`).
  - `pub(crate) erars_vm::terminal_vm::cells::{uft_index(&str, i64, impl Fn(char)->u8) -> usize, substring_cells(&str, i64, Option<i64>, impl Fn(char)->u8) -> String, strfind_cells(&str, &str, Option<i64>, impl Fn(char)->u8) -> i64, pad_str_cells(String, i64, erars_ast::Alignment, usize) -> String}` (crate-private helpers)
  - Harness convention (T11 relies on it): `tests/run_tests/<dir>/emuera.config` overrides the repo-root `emuera.config` for every fixture in `<dir>`; `tests/run_tests/jp/` runs JAPANESE. `tests/run_tests.rs::fixture_config(erb_file: &Path) -> EraConfig`.
  - Test conventions: fixture `.erb` files have **no** UTF-8 BOM (59 of the 61 existing fixtures have none; the parser strips one anyway); `.out` is the exact console text, ending in `\n` exactly when the script's last output ended a line.

---

- [ ] **Step 1: Write the failing unit test for `console_config`**

In `crates/erars-vm/src/lib.rs` replace line 8

```rust
use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
```

with

```rust
use erars_ui::{Color, ConsoleConfig, InputRequest, InputRequestType, VirtualConsole};
```

and append at the end of the file:

```rust

#[cfg(test)]
mod console_config_tests {
    use super::*;

    #[test]
    fn console_config_uses_language_encoding_and_colours() {
        let cfg = EraConfig {
            lang: Language::Japanese,
            printc_width: 25,
            max_log: 7,
            fore_color: [1, 2, 3],
            bg_color: [4, 5, 6],
            focus_color: [7, 8, 9],
            ..Default::default()
        };
        let c = console_config(&cfg);
        assert_eq!(c.encoding, encoding_rs::SHIFT_JIS);
        assert_eq!(c.printc_width, 25);
        assert_eq!(c.max_log, 7);
        assert_eq!(c.fore_color, Color([1, 2, 3]));
        assert_eq!(c.bg_color, Color([4, 5, 6]));
        assert_eq!(c.focus_color, Color([7, 8, 9]));

        let kr = EraConfig {
            lang: Language::Korean,
            ..Default::default()
        };
        assert_eq!(console_config(&kr).encoding, encoding_rs::EUC_KR);
    }
}
```

Run: `cargo test -p erars-vm --lib console_config 2>&1 | grep -E "^error"` → expected
`error[E0425]: cannot find function `console_config` in this scope`.

- [ ] **Step 2: Implement `console_config` — PASS**

In `crates/erars-vm/src/lib.rs` insert after line 26 (`pub use erars_compiler::{EraConfig, HeaderInfo, Instruction, Language};`):

```rust

/// Console construction parameters derived from `emuera.config`
/// (spec Component 2): the PRINTC field width, the backlog size, the
/// game encoding that decides half/full cells, and the three colours.
/// Used by `erars-loader`, `tests/run_tests.rs` and the renderer tests.
pub fn console_config(cfg: &EraConfig) -> ConsoleConfig {
    ConsoleConfig {
        printc_width: cfg.printc_width,
        max_log: cfg.max_log,
        encoding: cfg.lang.encoding(),
        fore_color: Color(cfg.fore_color),
        bg_color: Color(cfg.bg_color),
        focus_color: Color(cfg.focus_color),
    }
}
```

Run: `cargo test -p erars-vm --lib console_config` → expected
`test console_config_tests::console_config_uses_language_encoding_and_colours ... ok`.

- [ ] **Step 3: `VmContext::encoding` delegates to `Language::encoding`**

In `crates/erars-vm/src/context.rs` replace lines 61–74

```rust
    pub fn encoding(&self) -> &'static encoding_rs::Encoding {
        use erars_compiler::Language;

        match self.config.lang {
            // 949
            Language::Korean => encoding_rs::EUC_KR,
            // 932
            Language::Japanese => encoding_rs::SHIFT_JIS,
            // 936
            Language::ChineseHans => encoding_rs::GBK,
            // 950
            Language::ChineseHant => encoding_rs::BIG5,
        }
    }
```

with

```rust
    /// The game language's legacy encoding (`Language::encoding`) — the
    /// same encoding the console's `WidthTable` was built from.
    pub fn encoding(&self) -> &'static encoding_rs::Encoding {
        self.config.lang.encoding()
    }
```

Run: `cargo check -p erars-vm 2>&1 | tail -1` → `Finished …`.

- [ ] **Step 4: `erars-loader` uses `erars_vm::console_config`; delete T3's temporary copy**

In `crates/erars-loader/src/lib.rs` replace line 21

```rust
use erars_vm::{FunctionDic, SystemFunctions, TerminalVm, VmContext};
```

with

```rust
use erars_vm::{console_config, FunctionDic, SystemFunctions, TerminalVm, VmContext};
```

and delete the temporary helper T3 inserted after the `use` block — the whole item from its doc comment `/// Console construction parameters from `emuera.config`. Temporary copy —` through the closing `}` of `fn console_config(config: &EraConfig) -> erars_ui::ConsoleConfig`. The two call sites (`let vconsole = VirtualConsole::new(&console_config(&config));` and `let mut tx = VirtualConsole::new(&console_config(&config));`) stay exactly as they are — they now resolve to the imported function (at the second site `config` is `Arc<EraConfig>`; `&config` deref-coerces).

Run: `grep -c "fn console_config" crates/erars-loader/src/lib.rs; cargo check -p erars-loader 2>&1 | tail -1` → `0`, then `Finished …`.

- [ ] **Step 5: Harness loads `<fixture dir>/emuera.config`; a `jp/` fixture proves it**

Create `tests/run_tests/jp/emuera.config` (no BOM; the config lexer skips one anyway; the colour lines exercise T2's parser and are used by `jp/getdefcolor` in Step 15):

```
内部で使用する東アジア言語:JAPANESE
文字色:200,100,50
背景色:10,20,30
```

Create `tests/run_tests/jp/lang.erb`:

```
@SYSTEM_TITLE
PRINTFORML %GETCONFIGS("内部で使用する東アジア言語")%
```

Create `tests/run_tests/jp/lang.out` (one line, ends with `\n`):

```
JAPANESE
```

Replace the whole of `tests/run_tests.rs` with (this drops T3's temporary `fn console_config`; `console_config` now comes from `use erars_vm::*;`):

```rust
use std::path::Path;
use std::sync::Arc;

use erars_compiler::{compile, EraConfig, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::*;
use flexi_logger::*;

mod test_util;

/// `<fixture dir>/emuera.config` when present, otherwise the repo-root
/// `emuera.config` (KOREAN). `tests/run_tests/jp/emuera.config` switches
/// that directory to JAPANESE.
fn fixture_config(erb_file: &Path) -> EraConfig {
    let local = erb_file.parent().unwrap().join("emuera.config");
    let text = match std::fs::read_to_string(&local) {
        Ok(text) => text,
        Err(_) => include_str!("../emuera.config").to_owned(),
    };
    EraConfig::from_text(&text).unwrap()
}

#[test]
fn run_test() {
    let _handle = Logger::try_with_str("trace")
        .unwrap()
        .rotate(
            Criterion::AgeOrSize(Age::Day, 1024 * 1024),
            Naming::Numbers,
            Cleanup::KeepLogFiles(5),
        )
        .log_to_file(FileSpec::default().directory("logs").basename("erars_test"))
        .write_mode(WriteMode::BufferAndFlush)
        .use_utc()
        .create_symlink("last_test_log.log")
        .start()
        .unwrap();

    erars_ast::init_interner();

    let erb_files = glob::glob("tests/run_tests/**/*.erb").unwrap();
    let header = test_util::get_ctx("").header.try_as_arc().unwrap();

    for erb_file in erb_files {
        let erb_file = erb_file.unwrap();
        let mut ctx = VmContext::new(
            header.clone(),
            Arc::new(fixture_config(&erb_file)),
            Box::new(NullSystemFunctions),
            "sav".into(),
        );
        let out_file = erb_file.parent().unwrap().join(format!(
            "{}.out",
            erb_file.file_stem().unwrap().to_str().unwrap()
        ));

        log::info!("Run {}", erb_file.display());

        let expected_ret = std::fs::read_to_string(out_file).unwrap();

        let program =
            test_util::do_test(erb_file.to_str().unwrap(), ParserContext::parse_program_str);
        let mut dic = FunctionDic::new();

        for func in program {
            dic.insert_compiled_func(
                &mut ctx.var,
                &ctx.header_info.default_local_size,
                compile(func).unwrap(),
            );
        }

        log::info!("FunctionDic: {dic:#?}");
        let ret = test_runner(dic, ctx);

        if ret != expected_ret {
            eprintln!("[x] {}", erb_file.display());
            k9::assert_equal!(ret, expected_ret);
        } else {
            eprintln!("[o] {}", erb_file.display());
        }
    }
}

fn test_runner(dic: FunctionDic, mut ctx: VmContext) -> String {
    let vm = TerminalVm::new(dic, ctx.header_info.clone());
    let mut tx = VirtualConsole::new(&console_config(&ctx.config));

    let ok = vm.start(&mut tx, &mut ctx);

    // Check stack is empty if return success
    if ok {
        let leftover = ctx.return_func().unwrap().collect::<Vec<_>>();
        if !leftover.is_empty() {
            panic!("Function stack is not cleared: {leftover:?}");
        }
    }

    let mut out = String::new();

    use std::fmt::Write;
    for line in tx.lines_from(0).iter() {
        writeln!(out, "{}", line).unwrap();
    }

    writeln!(out, "{}", tx.last_line).unwrap();

    // Remove lastest newline
    out.pop();

    out
}
```

Run: `cargo test --test run_tests 2>&1 | grep -E "^\[|test result|GETCONFIG"` → expected: `[o] tests/run_tests/jp/lang.erb` and `[o]` for every other fixture, `test result: ok. 1 passed`. (`basic/builtin_methods.out` line 21 was already updated to `GETCONFIG 19 ` by T2 Step 14 — Step 6 double-checks it.)

Negative check that the per-directory config is what makes `jp/lang` pass:
`mv tests/run_tests/jp/emuera.config tests/run_tests/jp/emuera.config.off && cargo test --test run_tests 2>&1 | grep "jp/lang"; mv tests/run_tests/jp/emuera.config.off tests/run_tests/jp/emuera.config` → `[x] tests/run_tests/jp/lang.erb` (the diff shows `KOREAN`; the fallback root config is KOREAN). Confirm the rename was undone: `test -f tests/run_tests/jp/emuera.config && echo restored`.

- [ ] **Step 6: Confirm the `GETCONFIG` snapshot carries the empty font default; commit**

`grep -n '^GETCONFIG 19' tests/run_tests/basic/builtin_methods.out | cat -A` → expected `21:GETCONFIG 19 $` (trailing space: the fixture prints `{GETCONFIG("一行の高さ")} %GETCONFIGS("フォント名")%` and the family is empty since T2). Only if it still prints `GETCONFIG 19 D2Coding` (T2 Step 14 was skipped) run `sed -i 's/^GETCONFIG 19 D2Coding$/GETCONFIG 19 /' tests/run_tests/basic/builtin_methods.out` and add the file to the commit below.

Run: `cargo test --test run_tests 2>&1 | grep -E "^\[x\]|test result"` → only `test result: ok. 1 passed`.

Commit:

```
git add crates/erars-vm/src/lib.rs crates/erars-vm/src/context.rs crates/erars-loader/src/lib.rs tests/run_tests.rs tests/run_tests/jp && git commit -m "feat(vm): console_config from EraConfig; run_tests loads per-directory emuera.config"
```

- [ ] **Step 7: Write the failing unit tests for the cell-walk string helpers**

In `crates/erars-vm/src/terminal_vm.rs` replace line 1

```rust
mod executor;
```

with

```rust
mod cells;
mod executor;
```

Create `crates/erars-vm/src/terminal_vm/cells.rs` containing **only** this (the functions come in Step 8):

```rust
//! Cell-width string functions shared by `STRLEN`/`STRLENS`/`STRLENFORM`,
//! `SUBSTRING`, `STRFIND` and `{x, width}` / `%s, width%` padding.
//!
//! Ports Emuera's `LangManager` (`GetStrlenLang` / `GetUFTIndex` /
//! `GetSubStringLang`) and `StrForm.FormatPercent` with the console's
//! `char_cells` in place of `Encoding.GetByteCount`, so the VM measures with
//! the same cell function the renderer lays out with (spec Component 2).
//! The helpers take the cell function as a closure because `VirtualConsole`
//! exposes `char_cells(char)` / `cells(&str)` rather than its `WidthTable`.

use erars_ast::Alignment;

#[cfg(test)]
mod tests {
    use super::*;
    use erars_ui::width::WidthTable;

    fn jp() -> WidthTable {
        WidthTable::new(encoding_rs::SHIFT_JIS)
    }

    fn kr() -> WidthTable {
        WidthTable::new(encoding_rs::EUC_KR)
    }

    #[test]
    fn uft_index_maps_cells_to_chars() {
        let t = jp();
        let c = |ch| t.char_cells(ch);
        assert_eq!(uft_index("abc", 0, c), 0);
        assert_eq!(uft_index("abc", -5, c), 0);
        assert_eq!(uft_index("abc", 1, c), 1);
        assert_eq!(uft_index("abc", 3, c), 3);
        assert_eq!(uft_index("abc", 99, c), 3);
        assert_eq!(uft_index("", 1, c), 0);
        // an offset inside a 2-cell character skips that character whole
        assert_eq!(uft_index("한글a", 1, c), 1);
        assert_eq!(uft_index("한글a", 2, c), 1);
        assert_eq!(uft_index("한글a", 3, c), 2);
    }

    #[test]
    fn substring_walks_whole_characters() {
        let t = jp();
        let c = |ch| t.char_cells(ch);
        assert_eq!(substring_cells("한글abc", 2, Some(3), c), "글a");
        assert_eq!(substring_cells("한글abc", 1, Some(2), c), "글");
        assert_eq!(substring_cells("한글abc", 4, None, c), "abc");
        assert_eq!(substring_cells("─═║x", 0, Some(3), c), "─═");
        assert_eq!(substring_cells("😀xy", 2, Some(1), c), "x");
        assert_eq!(substring_cells("abc", -1, Some(2), c), "ab");
        assert_eq!(substring_cells("abc", 0, Some(-1), c), "abc");
        assert_eq!(substring_cells("abc", 0, Some(0), c), "");
        assert_eq!(substring_cells("abc", 3, Some(1), c), "");
        assert_eq!(substring_cells("abc", 0, Some(99), c), "abc");
        assert_eq!(substring_cells("", 0, None, c), "");
        // Hangul in a Japanese game: 2 cells each, no `&#NNNN;` inflation
        assert_eq!(substring_cells("정음x", 0, Some(4), c), "정음");
    }

    #[test]
    fn substring_depends_on_language() {
        let jp = jp();
        let kr = kr();
        // ‖ U+2016: 2 cells in JP (cp932 best-fit override), 1 cell in EUC-KR
        assert_eq!(substring_cells("‖ab", 2, Some(1), |ch| jp.char_cells(ch)), "a");
        assert_eq!(substring_cells("‖ab", 2, Some(1), |ch| kr.char_cells(ch)), "b");
        // tests/run_tests/sqn/substring.erb: "정음, " is 6 cells, SUBSTRING(s, 0, 4)
        assert_eq!(substring_cells("정음, ", 0, Some(4), |ch| kr.char_cells(ch)), "정음");
    }

    #[test]
    fn strfind_returns_cell_offsets() {
        let t = kr();
        let c = |ch| t.char_cells(ch);
        // tests/run_tests/basic/strfind.erb
        assert_eq!(strfind_cells("가나다", "다", None, c), 4);
        assert_eq!(strfind_cells("한글abc", "a", None, c), 4);
        assert_eq!(strfind_cells("─═║x", "x", None, c), 4);
        assert_eq!(strfind_cells("😀x", "x", None, c), 2);
        assert_eq!(strfind_cells("abcabc", "a", Some(1), c), 3);
        assert_eq!(strfind_cells("abc", "a", Some(1), c), -1);
        assert_eq!(strfind_cells("한글한", "한", Some(1), c), 4);
        assert_eq!(strfind_cells("abc", "z", None, c), -1);
        assert_eq!(strfind_cells("abc", "c", Some(5), c), -1);
        assert_eq!(strfind_cells("abc", "c", Some(-2), c), 2);
        assert_eq!(strfind_cells("abc", "", None, c), 0);
        // Emuera: `UFTstart >= target.Length` → -1, even for an empty needle
        assert_eq!(strfind_cells("", "", None, c), -1);
        assert_eq!(strfind_cells("정음x", "x", None, c), 4);
        assert_eq!(strfind_cells("‖ab", "b", None, c), 2);
        let j = jp();
        assert_eq!(strfind_cells("‖ab", "b", None, |ch| j.char_cells(ch)), 3);
    }

    #[test]
    fn pad_str_pads_by_cells() {
        let t = jp();
        let n = |s: &str| t.str_cells(s);
        assert_eq!(pad_str_cells("★●①".into(), 8, Alignment::Left, n("★●①")), "★●①  ");
        assert_eq!(pad_str_cells("★●①".into(), 6, Alignment::Left, n("★●①")), "★●①");
        assert_eq!(pad_str_cells("12".into(), 5, Alignment::Right, n("12")), "   12");
        assert_eq!(pad_str_cells("あ".into(), 1, Alignment::Right, n("あ")), "あ");
        assert_eq!(pad_str_cells("1".into(), -3, Alignment::Right, n("1")), "1");
        assert_eq!(pad_str_cells("7".into(), 4, Alignment::Center, n("7")), " 7  ");
        assert_eq!(pad_str_cells("한a".into(), 6, Alignment::Left, n("한a")), "한a   ");
        assert_eq!(pad_str_cells("‖".into(), 3, Alignment::Left, n("‖")), "‖ ");
    }
}
```

Run: `cargo test -p erars-vm --lib cells:: 2>&1 | grep -E "^error" | sort -u` → expected
`error[E0425]: cannot find function `uft_index` in this scope` (and the same for `substring_cells`, `strfind_cells`, `pad_str_cells`).

- [ ] **Step 8: Implement the helpers — PASS**

Insert between `use erars_ast::Alignment;` and `#[cfg(test)]` in `crates/erars-vm/src/terminal_vm/cells.rs` (verified against the pinned toolchain in `scratchpad/probe-plan-t4/verify.rs`; every assertion of Step 7 and every fixture value of Steps 9–15 was computed with exactly this code):

```rust

/// `LangManager.GetStrlenLang`: the cell count of `s`.
fn total_cells(s: &str, cells: &impl Fn(char) -> u8) -> i64 {
    s.chars().map(|c| i64::from(cells(c))).sum()
}

/// `LangManager.GetUFTIndex`: how many leading characters to skip so that the
/// skipped cells reach `lang_index` (whole characters only). `≤ 0` → 0;
/// at or beyond the string's total → the character count.
pub(crate) fn uft_index(s: &str, lang_index: i64, cells: impl Fn(char) -> u8) -> usize {
    if lang_index <= 0 {
        return 0;
    }
    if lang_index >= total_cells(s, &cells) {
        return s.chars().count();
    }
    let mut utf = 0;
    let mut jis = 0;
    for c in s.chars() {
        jis += i64::from(cells(c));
        utf += 1;
        if jis >= lang_index {
            break;
        }
    }
    utf
}

/// `LangManager.GetSubStringLang`: skip characters until the running cell
/// count reaches `start`, then append characters until the running count
/// reaches `length` (`None` or negative = to the end). Never splits a
/// character.
pub(crate) fn substring_cells(
    s: &str,
    start: i64,
    length: Option<i64>,
    cells: impl Fn(char) -> u8,
) -> String {
    let total = total_cells(s, &cells);
    if start >= total || length == Some(0) {
        return String::new();
    }
    let length = match length {
        Some(l) if l >= 0 && l <= total => l,
        _ => total,
    };

    let mut chars = s.chars().peekable();
    if start <= 0 {
        if length == total {
            return s.to_owned();
        }
    } else {
        let mut jis = 0;
        while let Some(c) = chars.next() {
            jis += i64::from(cells(c));
            if jis >= start {
                break;
            }
        }
        if chars.peek().is_none() {
            return String::new();
        }
    }

    let mut ret = String::new();
    let mut jis = 0;
    for c in chars {
        ret.push(c);
        jis += i64::from(cells(c));
        if jis >= length {
            break;
        }
    }
    ret
}

/// `STRFIND` (Emuera `StrFindMethod`, non-unicode branch): `start` is a cell
/// offset mapped through [`uft_index`]; the result is the cell count of the
/// text before the match (measured from the start of `target`), or `-1`.
pub(crate) fn strfind_cells(
    target: &str,
    word: &str,
    start: Option<i64>,
    cells: impl Fn(char) -> u8,
) -> i64 {
    let uft_start = start.map_or(0, |js| uft_index(target, js, &cells));
    if uft_start >= target.chars().count() {
        return -1;
    }
    let byte_start = target
        .char_indices()
        .nth(uft_start)
        .map_or(target.len(), |(b, _)| b);
    match target[byte_start..].find(word) {
        Some(rel) => total_cells(&target[..byte_start + rel], &cells),
        None => -1,
    }
}

/// `StrForm.FormatPercent`: pad `text` (occupying `text_cells` cells) with
/// spaces to `width` cells — Left → after, Right → before, Center (erars
/// extension) → `n/2` before and the rest after. Unchanged when it already
/// fills the field or `width` is smaller (including negative).
pub(crate) fn pad_str_cells(
    text: String,
    width: i64,
    align: Alignment,
    text_cells: usize,
) -> String {
    let n = width - text_cells as i64;
    if n <= 0 {
        return text;
    }
    let n = n as usize;
    let (before, after) = match align {
        Alignment::Left => (0, n),
        Alignment::Right => (n, 0),
        Alignment::Center => (n / 2, n - n / 2),
    };
    let mut ret = String::with_capacity(text.len() + n);
    ret.extend(std::iter::repeat(' ').take(before));
    ret.push_str(&text);
    ret.extend(std::iter::repeat(' ').take(after));
    ret
}
```

Run: `cargo test -p erars-vm --lib cells::` → expected `test result: ok. 5 passed` (until Step 10 the functions are unused outside the tests, so `dead_code` warnings appear; they disappear in Step 10).

- [ ] **Step 9: KOREAN fixtures for STRLEN / SUBSTRING (failing)**

Write the following files without a BOM. Every `.out` below ends with a single `\n` after its last line and contains no trailing spaces except where noted.

`tests/run_tests/basic/strlen_cells.erb`:

```
@SYSTEM_TITLE
PRINTFORML {STRLENS("한글abc")} {STRLENS("─═║")} {STRLENS("〜")} {STRLENS("😀x")} {STRLENS("‖")} {STRLENS("▒")} {STRLENS("¢")}
LOCALS = 정음
STRLENS LOCALS
PRINTVL RESULT
STRLENFORM %LOCALS%あ
PRINTVL RESULT
```

`tests/run_tests/basic/strlen_cells.out` (EUC-KR: `─` 2, `═` `║` 1, `〜` 2, `😀` 2, `‖` 1, `▒` 2, `¢` 1):

```
7 4 2 3 1 2 1
4
6
```

`tests/run_tests/basic/substring_cells.erb`:

```
@SYSTEM_TITLE
PRINTFORML <%SUBSTRING("한글abc", 2, 3)%><%SUBSTRING("한글abc", 1, 2)%><%SUBSTRING("한글abc", 4)%><%SUBSTRING("─═║x", 0, 3)%><%SUBSTRING("😀xy", 2, 1)%><%SUBSTRING("abc", -1, 2)%><%SUBSTRING("abc", 3, 1)%>
PRINTFORML <%SUBSTRING("‖ab", 2, 1)%><%SUBSTRING("a〜b", 1, 1)%>
LOCALS = 정음x
STRLENS LOCALS
PRINTFORML <%SUBSTRING(LOCALS, 0, RESULT - 1)%>
```

`tests/run_tests/basic/substring_cells.out`:

```
<글a><글><abc><─═><x><ab><>
<b><〜>
<정음>
```

Run: `cargo test --test run_tests 2>&1 | grep -E "^\[x\]|^-|^\+" | head -8` → expected the first failing fixture `[x] tests/run_tests/basic/strlen_cells.erb` with a k9 diff whose first differing line is `7 16 8 10 7 2 6` (today's byte count with `&#NNNN;` inflation for `═ ║ 〜 😀 ‖ ¢`) against the expected `7 4 2 3 1 2 1`. (Fixtures run in `glob` order — alphabetical per directory — and the harness stops at the first k9 failure, so `substring_cells` is not reached. The `strfind_cells` and `padstr` fixtures are written in Step 10, *after* the executor change: today's code cannot even produce a diff for them — `STRFIND("abc", "c", 5)` panics on `bytes[5..]` (`range start index 5 out of range for slice of length 3`, executor.rs:1056) and `{1, -3}` makes the `pad` crate push `usize::MAX - 2` spaces (`size as usize`) until the allocator gives up; either aborts the whole harness before any `[x]` line, and `padstr` sorts before `strlen_cells`.)

- [ ] **Step 10: Executor uses the cell walk; drop `pad`/`unicode-width`/`twoway`; STRFIND / PadStr fixtures — PASS**

In `crates/erars-vm/src/terminal_vm/executor.rs` apply bottom-up:

(a) lines 1319–1355 — the whole `BuiltinMethod::SubString => { … }` arm (the `match usize::try_from(start) { … };` ends on line 1354 and the arm's closing `}` is line 1355) →

```rust
        BuiltinMethod::SubString => {
            check_arg_count!(1, 3);
            let text = get_arg!(@String: args, ctx);
            let start = get_arg!(@opt @i64: args, ctx).unwrap_or(0);
            let length = get_arg!(@opt @i64: args, ctx);

            ctx.push(cells::substring_cells(&text, start, length, |c| tx.char_cells(c)));
        }
```

(b) lines 1073–1077 (`BuiltinMethod::StrLenS`) →

```rust
        BuiltinMethod::StrLenS => {
            check_arg_count!(1);
            let s = get_arg!(@String: args, ctx);
            ctx.push(tx.cells(&s) as i64);
        }
```

(c) lines 1046–1059 (`BuiltinMethod::StrFind`) →

```rust
        BuiltinMethod::StrFind => {
            check_arg_count!(2, 3);
            let s = get_arg!(@String: args, ctx);
            let find = get_arg!(@String: args, ctx);
            let start = get_arg!(@opt @i64: args, ctx);

            ctx.push(cells::strfind_cells(&s, &find, start, |c| tx.char_cells(c)));
        }
```

(d) lines 339–352 (`} else if let Some(align) = inst.as_pad_str() {` … `ctx.push(text.pad_to_width_with_alignment(size as usize, align));`) →

```rust
    } else if let Some(align) = inst.as_pad_str() {
        let width = ctx.pop_int()?;
        let text = match ctx.pop_value()? {
            Value::String(s) => s,
            Value::Int(i) => i.to_string(),
        };
        let text_cells = tx.cells(&text);

        ctx.push(cells::pad_str_cells(text, width, align, text_cells));
```

(e) after line 10 (`use crate::{context::VariableRef, variable::KnownVariableNames as Var};`) add:

```rust
use super::cells;
```

(`use erars_ast::Alignment;` on line 2 stays — still used at line ~1705.)

Then drop the dead dependencies. In `crates/erars-vm/src/lib.rs` delete line 11 (`use pad::PadStr;`). In `crates/erars-vm/Cargo.toml` replace lines 22–24

```toml
unicode-width = "0.1.9"
encoding_rs = "0.8.31"
pad = "0.1.6"
```

with

```toml
encoding_rs.workspace = true
```

and delete line 30 (`twoway = "0.2.2"` — its only user was the byte-based `STRFIND`; `unicode-width` had no user in `erars-vm/src` at all).

Now write the two fixtures the old code could not run (see Step 9), without a BOM:

`tests/run_tests/basic/strfind_cells.erb`:

```
@SYSTEM_TITLE
PRINTFORML {STRFIND("한글abc", "a")} {STRFIND("─═║x", "x")} {STRFIND("😀x", "x")} {STRFIND("abcabc", "a", 1)} {STRFIND("abc", "a", 1)} {STRFIND("한글한", "한", 1)} {STRFIND("abc", "z")} {STRFIND("abc", "c", 5)} {STRFIND("정음x", "x")} {STRFIND("‖ab", "b")}
```

`tests/run_tests/basic/strfind_cells.out`:

```
4 4 2 3 -1 4 -1 -1 4 2
```

`tests/run_tests/basic/padstr.erb` (`{12, 5}` is Right by default; `%"あ", 1%` overlong; `{1, -3}` negative width):

```
@SYSTEM_TITLE
PRINTFORML <%"★●①", 8, LEFT%><%"★●①", 6, LEFT%><{12, 5}><%"あ", 1%><{1, -3}><{7, 4, CENTER}><%"한a", 6, LEFT%><{12, 5, RIGHT}><%"‖", 3, LEFT%>
```

`tests/run_tests/basic/padstr.out` — one line: `<★●①` + 2 spaces + `>`, `<★●①>`, `<` + 3 spaces + `12>`, `<あ>`, `<1>`, `< 7` + 2 spaces + `>`, `<한a` + 3 spaces + `>`, `<` + 3 spaces + `12>`, `<‖` + 2 spaces + `>`:

```
<★●①  ><★●①><   12><あ><1>< 7  ><한a   ><   12><‖  >
```

Run: `cargo test -p erars-vm --lib 2>&1 | grep -E "test result|^warning" ; cargo test --test run_tests 2>&1 | grep -E "^\[x\]|test result"` → `test result: ok.` for both, no `warning` lines from erars-vm, no `[x]` — `strlen_cells`, `substring_cells`, `strfind_cells` and `padstr` all print `[o]`. The pre-existing `basic/strfind.erb` (`4 1`) and `sqn/substring.erb` (`정음`) keep passing: EUC-KR-encodable text has identical byte and cell counts.

Commit:

```
git add crates/erars-vm/Cargo.toml Cargo.lock crates/erars-vm/src/lib.rs crates/erars-vm/src/terminal_vm.rs crates/erars-vm/src/terminal_vm/cells.rs crates/erars-vm/src/terminal_vm/executor.rs tests/run_tests/basic/strlen_cells.erb tests/run_tests/basic/strlen_cells.out tests/run_tests/basic/substring_cells.erb tests/run_tests/basic/substring_cells.out tests/run_tests/basic/strfind_cells.erb tests/run_tests/basic/strfind_cells.out tests/run_tests/basic/padstr.erb tests/run_tests/basic/padstr.out && git commit -m "feat(vm): STRLEN/SUBSTRING/STRFIND and form padding measure in cells"
```

- [ ] **Step 11: Colour fixture (failing)**

`tests/run_tests/basic/getdefcolor.erb` (the root `emuera.config` sets no colours → fore (192,192,192), bg (0,0,0); colour ints are `0xRRGGBB` like Emuera — spec Component 2 — so `SETCOLOR 255, 0, 0` reads back as 16711680 and `SETCOLOR 0, 0, 255` as 255; today's little-endian packing would swap the two; the `SETCOLOR 0x0000FF` line must read back the same `255` as `SETCOLOR 0, 0, 255`, which pins the single-int form to the same `0xRRGGBB` decoding):

```
@SYSTEM_TITLE
PRINTFORML {GETDEFCOLOR()}
SETCOLOR 255, 0, 0
PRINTFORML {GETCOLOR()}
RESETCOLOR
PRINTFORML {GETCOLOR()}
SETCOLOR 0, 0, 255
PRINTDL x
PRINTFORML {GETCOLOR()}
SETCOLOR 0x0000FF
PRINTFORML {GETCOLOR()}
PRINTFORML {GETDEFBGCOLOR()}
```

`tests/run_tests/basic/getdefcolor.out` (`0xC0C0C0` = 12632256, `0xFF0000` = 16711680, `0x0000FF` = 255):

```
12632256
16711680
12632256
x
255
255
0
```

Run: `cargo test --test run_tests 2>&1 | grep -E "^\[x\]|^-|^\+" | head -4` → `[x] tests/run_tests/basic/getdefcolor.erb`; the diff's first line is `16777215` (today's hard-coded `0xFFFFFF`) against `12632256` (further down, output lines 2 and 5 print today's little-endian `255`/`16711680` where `16711680`/`255` are expected).

- [ ] **Step 12: PRINTD / RESETCOLOR / GETDEFCOLOR use the configured colours; colour ints become `0xRRGGBB` — PASS; commit**

First `crates/erars-ui/src/lib.rs`: replace the `impl From<Color> for u32` block (pre-T1 lines 29–33; locate by content — T1/T3 shifted it)

```rust
impl From<Color> for u32 {
    fn from(Color([r, g, b]): Color) -> Self {
        u32::from_le_bytes([r, g, b, 0])
    }
}
```

→

```rust
/// `0xRRGGBB`, Emuera's `Color.ToArgb() & 0xFFFFFF` — the packing of every
/// colour integer the VM reads or writes (GETCOLOR, GETDEFCOLOR, GETCONFIG,
/// SETCOLOR's single-int form).
impl From<Color> for u32 {
    fn from(Color([r, g, b]): Color) -> Self {
        (u32::from(r) << 16) | (u32::from(g) << 8) | u32::from(b)
    }
}

/// Inverse of `u32::from(Color)`; bits above 23 are ignored.
impl From<u32> for Color {
    fn from(c: u32) -> Self {
        Color([(c >> 16) as u8, (c >> 8) as u8, c as u8])
    }
}
```

`VirtualConsole::{color, bg_color, hl_color}` (`self.style.color.into()` etc.) pick the new packing up unchanged.

Then `crates/erars-vm/src/terminal_vm/executor.rs` (bottom-up; `Color` is not imported there — the snippets spell `erars_ui::Color`):

(a) lines 2181–2186

```rust
        BuiltinCommand::ResetColor => {
            tx.set_color(0xFF, 0xFF, 0xFF);
        }
        BuiltinCommand::ResetBgColor => {
            tx.set_bg_color(0, 0, 0);
        }
```

→

```rust
        BuiltinCommand::ResetColor => {
            tx.reset_color();
        }
        BuiltinCommand::ResetBgColor => {
            let [r, g, b] = ctx.config.bg_color;
            tx.set_bg_color(r, g, b);
        }
```

(b) line 2174, inside `BuiltinCommand::SetBgColor` — `let [r, g, b, _] = (c as u32).to_le_bytes();` →

```rust
                    let erars_ui::Color([r, g, b]) = erars_ui::Color::from(c as u32);
```

(c) line 2136, inside `BuiltinCommand::SetColor` — the identical `let [r, g, b, _] = (c as u32).to_le_bytes();` → the same replacement as (b).

(d) lines 1434–1441

```rust
        BuiltinMethod::GetDefColor => {
            check_arg_count!(0);
            ctx.push(0xFFFFFFu32);
        }
        BuiltinMethod::GetDefBgColor => {
            check_arg_count!(0);
            ctx.push(0i64);
        }
```

→

```rust
        BuiltinMethod::GetDefColor => {
            check_arg_count!(0);
            ctx.push(u32::from(tx.default_color()));
        }
        BuiltinMethod::GetDefBgColor => {
            check_arg_count!(0);
            ctx.push(u32::from(erars_ui::Color(ctx.config.bg_color)));
        }
```

(e) lines 213–216 (the PRINTD colour restore)

```rust
        if let Some(prev_color) = prev_color {
            let [r, g, b, _] = prev_color.to_le_bytes();
            tx.set_color(r, g, b);
        }
```

→

```rust
        if let Some(prev_color) = prev_color {
            let erars_ui::Color([r, g, b]) = erars_ui::Color::from(prev_color);
            tx.set_color(r, g, b);
        }
```

(f) lines 195–202

```rust
        let prev_color = if flags.contains(PrintFlags::DEFAULT_COLOR) {
            let c = tx.color();
            // TODO: respect config
            tx.set_color(0xFF, 0xFF, 0xFF);
            Some(c)
        } else {
            None
        };
```

→

```rust
        let prev_color = if flags.contains(PrintFlags::DEFAULT_COLOR) {
            let c = tx.color();
            tx.reset_color();
            Some(c)
        } else {
            None
        };
```

Check that no little-endian colour packing survives: `grep -n "to_le_bytes\|from_le_bytes" crates/erars-ui/src/lib.rs crates/erars-vm/src/terminal_vm/executor.rs; echo "exit=$? (1 = clean)"` → no output, `exit=1`.

Run: `cargo test -p erars-ui 2>&1 | grep "test result"; cargo test --test run_tests 2>&1 | grep -E "^\[x\]|test result"` → every `test result: ok.`, no `[x]` (T3's `console_tests` compare `tx.color()` against `u32::from(FORE)`, so they follow the new packing; `basic/builtin_methods` does not print a colour).

Commit:

```
git add crates/erars-ui/src/lib.rs crates/erars-vm/src/terminal_vm/executor.rs tests/run_tests/basic/getdefcolor.erb tests/run_tests/basic/getdefcolor.out && git commit -m "fix(vm): configured default colours for PRINTD/RESETCOLOR/GETDEFCOLOR; colour ints are 0xRRGGBB like Emuera"
```

- [ ] **Step 13: KOREAN fixtures for PRINTC/PRINTLC, DRAWLINE, ALIGNMENT and the `\n` split**

These exercise T3's console behaviour end-to-end through the VM; they pass as soon as they are written.

`tests/run_tests/basic/printc.erb`:

```
@SYSTEM_TITLE
PRINTC abc
PRINTC あいう
PRINTL |
PRINTLC abc
PRINTLC 한글ab
PRINTL |
PRINTC abcdefghijklmnopqrstuvwxy
PRINTL |
PRINTLC abcdefghijklmnopqrstuvwxy
PRINTL |
PRINTLC abcdefghijklmnopqrstuvwxyz
PRINTL |
PRINTBUTTONC "x", 1
PRINTBUTTONLC "y", 2
PRINTL |
PRINTFORMC {1 + 1}
PRINTFORMLC %"z"%
PRINTL |
```

`tests/run_tests/basic/printc.out` — PRINTC pads to 25 cells (spaces before), PRINTLC to 26 (spaces after), strings at or beyond the field are unpadded. The exact space runs are 22, 19 / 23, 20 / – / 1 / – / 24, 25 / 24, 25, so generate the file rather than typing it:

```
printf '%22sabc%19sあいう|\nabc%23s한글ab%20s|\nabcdefghijklmnopqrstuvwxy|\nabcdefghijklmnopqrstuvwxy |\nabcdefghijklmnopqrstuvwxyz|\n%24sxy%25s|\n%24s2z%25s|\n' '' '' '' '' '' '' '' '' > tests/run_tests/basic/printc.out
```

Check: `python3 -c "import sys; [print(len(l.rstrip('\n'))) for l in open('tests/run_tests/basic/printc.out', encoding='utf-8')]"` → `48 51 26 27 27 52 52` (one number per line; characters, not cells).

`tests/run_tests/basic/drawline.erb` (DRAWLINE's text form is `ReplaceInfo::drawline_str`, default `-`; a rule issued after pending text stays on that line):

```
@SYSTEM_TITLE
PRINTL a
DRAWLINE
PRINT b
DRAWLINE
CUSTOMDRAWLINE =*
PRINTL c
```

`tests/run_tests/basic/drawline.out`:

```
a
-
b-
=*
c
```

`tests/run_tests/basic/alignment.erb` (`print("a\nb")` → `a` finished, `b` pending; `ALIGNMENT RIGHT` applies to the pending line, so `PRINTL c` yields `bc`; the harness compares text only, alignment itself is covered by T3's unit tests):

```
@SYSTEM_TITLE
PRINTS "a\nb"
ALIGNMENT RIGHT
PRINTL c
ALIGNMENT CENTER
PRINTL d
ALIGNMENT LEFT
PRINTL e
```

`tests/run_tests/basic/alignment.out`:

```
a
bc
d
e
```

`tests/run_tests/basic/print_newline.erb` (`\n` from a string literal and from `UNICODE(10)` each start a new logical line, so CLEARLINE and LINECOUNT see them):

```
@SYSTEM_TITLE
PRINTSL "a\nb"
CLEARLINE 1
PRINTVL LINECOUNT
PRINTFORML x%UNICODE(10)%y
PRINTVL LINECOUNT
```

`tests/run_tests/basic/print_newline.out`:

```
a
1
x
y
4
```

`tests/run_tests/basic/printbutton_newline.erb` (PRINTBUTTON strips `\n`; the button is one line):

```
@SYSTEM_TITLE
PRINTBUTTON "x\ny", 1
PRINTL
PRINTVL LINECOUNT
```

`tests/run_tests/basic/printbutton_newline.out`:

```
xy
1
```

Run: `cargo test --test run_tests 2>&1 | grep -E "^\[|test result"` → every fixture `[o]` (including the five new ones), `test result: ok`.

- [ ] **Step 14: Commit the T3 acceptance fixtures**

```
git add tests/run_tests/basic/printc.erb tests/run_tests/basic/printc.out tests/run_tests/basic/drawline.erb tests/run_tests/basic/drawline.out tests/run_tests/basic/alignment.erb tests/run_tests/basic/alignment.out tests/run_tests/basic/print_newline.erb tests/run_tests/basic/print_newline.out tests/run_tests/basic/printbutton_newline.erb tests/run_tests/basic/printbutton_newline.out && git commit -m "test(vm): run_tests fixtures for PRINTC/PRINTLC, DRAWLINE, ALIGNMENT and newline splitting"
```

- [ ] **Step 15: JAPANESE fixtures (`tests/run_tests/jp/`)**

`tests/run_tests/jp/strlen_cells.erb`: identical to `basic/strlen_cells.erb` (`cp tests/run_tests/basic/strlen_cells.erb tests/run_tests/jp/`).
`tests/run_tests/jp/strlen_cells.out` (Shift_JIS + cp932 overrides: `‖` 2, `▒` 1, `¢` 2; Hangul 2 by East-Asian-Width — no `&#NNNN;` inflation):

```
7 4 2 3 2 1 2
4
6
```

`tests/run_tests/jp/substring_cells.erb`: identical to `basic/substring_cells.erb`.
`tests/run_tests/jp/substring_cells.out` (`‖` is 2 cells, so `SUBSTRING("‖ab", 2, 1)` skips only `‖`):

```
<글a><글><abc><─═><x><ab><>
<a><〜>
<정음>
```

`tests/run_tests/jp/strfind_cells.erb`: identical to `basic/strfind_cells.erb`.
`tests/run_tests/jp/strfind_cells.out` (last value: `‖` is 2 cells):

```
4 4 2 3 -1 4 -1 -1 4 3
```

`tests/run_tests/jp/padstr.erb`: identical to `basic/padstr.erb`.
`tests/run_tests/jp/padstr.out` (last field: `‖` is 2 cells → one space):

```
<★●①  ><★●①><   12><あ><1>< 7  ><한a   ><   12><‖ >
```

`tests/run_tests/jp/printc.erb` and `.out`: identical to the `basic/` pair (`cp tests/run_tests/basic/printc.erb tests/run_tests/basic/printc.out tests/run_tests/jp/`) — `あいう` and `한글ab` are 6 cells in both languages.

`tests/run_tests/jp/getdefcolor.erb`: identical to `basic/getdefcolor.erb`.
`tests/run_tests/jp/getdefcolor.out` (`jp/emuera.config`: fore [200,100,50] → 200·65536 + 100·256 + 50 = 13132850; bg [10,20,30] → 10·65536 + 20·256 + 30 = 660510):

```
13132850
16711680
13132850
x
255
255
660510
```

Run: `cargo test --test run_tests 2>&1 | grep -E "^\[|test result"` → `[o] tests/run_tests/jp/…` for all seven `jp/` fixtures, `test result: ok. 1 passed`.

Commit:

```
git add tests/run_tests/jp && git commit -m "test(vm): JAPANESE run_tests fixtures (cell widths, PadStr, PRINTC, colours)"
```

- [ ] **Step 16: Train-menu `printc_count` gate with a scripted `SystemFunctions`**

In `crates/erars-vm/Cargo.toml` replace lines 41–42

```toml
[dev-dependencies]
k9 = "0.11.5"
```

with

```toml
[dev-dependencies]
k9 = "0.11.5"
serde_yaml = "0.9"
```

(`serde_yaml 0.9.30` is already in `Cargo.lock` via the root crate and `erars-loader`.)

Create `crates/erars-vm/tests/train_menu.rs`:

```rust
//! `printc_count` (emuera.config `PRINTCを並べる数`, Emuera `PrintCPerLine`):
//! the TRAIN command menu breaks the line after every `printc_count` PRINTC
//! items and `0` disables the break (`terminal_vm/executor.rs`,
//! `run_begin`, `BeginType::Train`). Drives the menu with a scripted
//! `SystemFunctions` that answers the one input request with `0` (COM0,
//! which QUITs).

use std::sync::Arc;

use erars_ast::{StrKey, Value};
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::{InputRequest, VirtualConsole};
use erars_vm::{console_config, FunctionDic, SystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = "@SYSTEM_TITLE\nBEGIN TRAIN\n\n@COM0\nQUIT\n";
const TRAIN_CSV: &str = "0,A\n1,B\n2,C\n3,D\n4,E\n";

/// Answers each input request with the next scripted value, then `None`.
struct Scripted(Vec<i64>);

impl SystemFunctions for Scripted {
    fn input(&mut self, _req: InputRequest) -> anyhow::Result<Option<Value>> {
        Ok(if self.0.is_empty() {
            None
        } else {
            Some(Value::Int(self.0.remove(0)))
        })
    }

    fn redraw(&mut self, _vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        Ok(())
    }
}

/// Runs the script with the given `printc_count` and returns the finished
/// console lines as text.
fn run_train_menu(printc_count: usize) -> Vec<String> {
    let mut info = HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    };
    info.merge_name_csv("TRAIN", TRAIN_CSV).unwrap();
    // every command is COM_ABLE without a COM_ABLEn function
    info.merge_replace_csv("COM_ABLE初期値,1").unwrap();
    let header = Arc::new(info);

    let config = EraConfig {
        printc_width: 25,
        printc_count,
        ..Default::default()
    };
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(Scripted(vec![0])),
        "sav".into(),
    );

    let parser = ParserContext::new(header.clone(), StrKey::new("TRAIN_MENU.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser.parse_program_str(SCRIPT).unwrap() {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    let vm = TerminalVm::new(dic, header);
    let ok = vm.start(&mut tx, &mut ctx);
    let lines: Vec<String> = tx.lines.iter().map(ToString::to_string).collect();
    assert!(ok, "VM error:\n{}", lines.join("\n"));
    lines
}

/// One `#[test]` for all three cases: `erars_ast::init_interner()` is
/// process-global, so the cases must not run on separate test threads.
#[test]
fn train_menu_breaks_lines_every_printc_count_items() {
    erars_ast::init_interner();
    // PRINTC item `{name}[{no:3}]`, right-aligned in a 25-cell field
    let item = |s: &str| format!("{s:>25}");

    // 2 per line: A B / C D / E
    k9::assert_equal!(
        run_train_menu(2),
        vec![
            item("A[  0]") + &item("B[  1]"),
            item("C[  2]") + &item("D[  3]"),
            item("E[  4]"),
        ]
    );

    // Emuera default 3 per line: A B C / D E
    k9::assert_equal!(
        run_train_menu(3),
        vec![
            item("A[  0]") + &item("B[  1]") + &item("C[  2]"),
            item("D[  3]") + &item("E[  4]"),
        ]
    );

    // 0 disables the break: one line
    k9::assert_equal!(
        run_train_menu(0),
        vec![["A[  0]", "B[  1]", "C[  2]", "D[  3]", "E[  4]"]
            .into_iter()
            .map(item)
            .collect::<String>()]
    );
}
```

How it flows through the VM (so failures can be read): `SYSTEM_TITLE` → `BEGIN TRAIN` → `run_begin(Train)` → `reset_train_data` sets `NEXTCOM = -1` → the menu branch prints one `printrc` item per TRAIN entry (COM_ABLE via `comable_init = 1`), breaking the line whenever `printc_count` items are on it, then `new_line()`, then `input_int_redraw` → `Scripted` answers `0` → `COM0` → `QUIT` → `start` returns `true`. Each padded item contains `[  n]`, so the console turns it into a `Button` part; `ConsoleLine`'s `Display` prints button text verbatim, which is why the expected strings are plain text.

Run: `cargo test -p erars-vm --test train_menu` → `test train_menu_breaks_lines_every_printc_count_items ... ok`.
(If it fails with `VM error: Value is empty`, the scripted answer was not consumed by `input_int_redraw` — check that `Scripted(vec![0])` reached `VmContext::new`; if it panics on `var_name_var[&train_key]`, `merge_name_csv("TRAIN", …)` did not run before `Arc::new(info)`.)

Commit:

```
git add crates/erars-vm/Cargo.toml Cargo.lock crates/erars-vm/tests/train_menu.rs && git commit -m "test(vm): train menu printc_count gate with a scripted SystemFunctions"
```

- [ ] **Step 17: Verify the whole VM side together**

Run:

```
cargo test -p erars-vm 2>&1 | grep -E "test result|^warning" ; cargo check -p erars-loader 2>&1 | tail -1 ; cargo test --test run_tests 2>&1 | grep -E "^\[x\]|test result" ; grep -rn "pad::\|PadStr\|twoway\|unicode_width" crates/erars-vm/src ; git status --short
```

Expected: every `test result: ok.` (lib unit tests incl. `console_config_tests` and `cells::tests`, the `train_menu` integration test), no `warning` lines from erars-vm, `Finished` for erars-loader, no `[x]` fixture, no grep hits, and `git status --short` shows nothing but the pre-existing untracked `msgothic.ttc` / `.DS_Store` / `2026-09-02-*.txt`. Also `grep -rn "fn console_config" crates tests` must list only `crates/erars-vm/src/lib.rs` (T3's temporary copies are gone).

---

### Task 5: Font chain — `crates/erars-renderer/src/font.rs` (FontChain), `flags.rs` (RasterFlags), Cargo features

**Context for the executor.** `erars-renderer` is a *binary* crate (`src/main.rs`, no `lib.rs`); its unit tests run with `cargo test -p erars-renderer`. Today `font.rs` holds a legacy `FontCtx` (cosmic-text `Buffer`/`Attrs` based) that `text.rs`, `grid.rs`, `draw.rs`, `atlas.rs`, `app.rs` and `headless.rs` still use. **This task keeps `FontCtx` compiling untouched** (Task 10 deletes it) and *adds* the new `FontChain` API beside it, so the crate builds and every existing test still passes after each step. Spec: `docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md`, Component 3 (lines 299-354), the cosmic-text Findings bullet (lines 103-111) and Error handling (lines 594-605).

Verified facts this task relies on (bare-`rustc` probes against the workspace's built rlibs in `target/debug/deps`, `scratchpad/probe-plan-T5/probe_v2.rs` — the exact code of Steps 2 and 5-10 with shims for `Language`/`TextStyle`; 22/22 tests pass, both against the current cosmic-text build and against cosmic-text 0.12.1 rebuilt **without** `monospace_fallback`/`shape-run-cache` (`probe-plan-T5/nofeat/`); the legacy `font.rs`+`text.rs` tests also pass 8/8 against that feature-less build, `probe-plan-T5/legacy/`):
- `cosmic_text::{fontdb, ttf_parser, Font, FontSystem}` are reachable from the crate root; `FontSystem::new_with_locale_and_db(String, fontdb::Database)`, `db()`, `db_mut()`, `get_font(id) -> Option<Arc<Font>>`; `Font::id()`, `Font::rustybuzz()` derefs to `ttf_parser::Face` (0.21.1 — the same crate version rustybuzz uses, so `GlyphId` types unify), hence `.glyph_index(char) -> Option<GlyphId>` and `.units_per_em()`.
- `get_font` calls `db.make_shared_face_data(id)` (fontdb `memmap` feature, enabled by cosmic-text's default `std`) before `Font::new`, so `fontdb::Source::File` faces load (mmap, no copy) — `Font::new` alone would reject them. `FontSystem::new_with_locale_and_db` also eagerly materialises every face fontdb flags `monospaced` (`cache_fonts`, rayon-parallel) — same cost as the legacy `FontCtx`, ~0.5 s with 1 800 system faces on the dev box.
- `fontdb::Database::load_font_source(Source) -> TinyVec<[ID; 8]>` returns the ids of every face in the file (a `.ttc` gives one id per face; unparsable data → empty vec + `log::warn`); `faces()` iterates in load order; `with_face_data(id, |data, index| ..)` gives the raw bytes (mmap for `File`) without materialising a `Font`; `FaceInfo { id, source, index, families: Vec<(String, Language)>, post_script_name, style: Style, weight: Weight, stretch, monospaced }` — all fields `pub`; `Weight::NORMAL = 400`, `SEMIBOLD = 600`; `Style::{Normal, Italic, Oblique}`; `ID: Copy + Eq + Hash + Ord + Display`, `ID::dummy()`.
- `Iterator::min_by_key` returns the *first* minimum, so ties fall to load order.
- The bundled Noto Sans Mono answers `glyph_index('\u{FFFF}') == Some(GlyphId(0))` — the `g.0 != 0` guard is required; it covers `A ┏ ━ ═ ░ █` but not `あ 한 漢`; upem 1000.
- `msgothic.ttc` face 0 exposes both families `"MS Gothic"` and `"ＭＳ ゴシック"` (name ID 1, no ID 16), `monospaced == true`, weight 400, `Style::Normal`; faces 1/2 are MS UI Gothic / MS PGothic; none of its faces has Hangul.
- Dev box system fonts include upright regular + bold faces of DejaVu Sans Mono and Liberation Mono (used by the real-bold test; CI's ubuntu image has DejaVu too).
- libtest only shows `eprintln!` output of *passing* tests with `--nocapture` (`cargo test … -- --nocapture`); the `SKIP` lines below are therefore invisible in a plain run.

**Files:**
- Modify `crates/erars-renderer/Cargo.toml` lines 22-32 (the `winit … cosmic-text` dependency block).
- Create `crates/erars-renderer/src/flags.rs`.
- Modify `crates/erars-renderer/src/main.rs` lines 3-13 (module list) and lines 22-62 (`font_candidates`).
- Modify `crates/erars-renderer/src/font.rs`: line 1 (imports), line 27 (`BUNDLED_FONT` visibility), insert the new code immediately before line 141 (`#[cfg(test)]` of the legacy `mod tests`). Legacy lines 3-139 and 141-169 stay byte-identical.
- Test: in-module `#[cfg(test)] mod chain_tests` in `font.rs` (19 tests), `mod tests` in `flags.rs` (1 test).

**Interfaces:**
- Consumes: `erars_compiler::Language` (variants `Japanese, Korean, ChineseHans, ChineseHant`; `Clone + Copy`, no `Eq`); `erars_ui::TextStyle { color: Color, font_family: SmolStr, font_style: FontStyle }`, `erars_ui::FontStyle::{BOLD, ITALIC}` (bitflags u32), `erars_ui::Color(pub [u8; 3])`. Nothing from Tasks 1-4.
- Produces (used by T6 shaper, T7 layout goldens, T8 raster, T9 headless, T10 app):
  - `crate::flags::RasterFlags` — `bitflags! { pub struct RasterFlags: u8 { const BOLD_SYNTH = 1; const ITALIC_SYNTH = 2; } }` deriving `Clone, Copy, Debug, Default, PartialEq, Eq, Hash` (T6's `text.rs` re-exports it with `pub use crate::flags::RasterFlags;`).
  - `crate::font::FontConfig<'a> { pub family: &'a str, pub game_dir: &'a Path, pub extra_dir: Option<PathBuf>, pub lang: Language }`
  - `crate::font::StyleKey { pub family: SmolStr, pub bold: bool, pub italic: bool }` (`Clone, Debug, Hash, PartialEq, Eq`), `StyleKey::from(style: &TextStyle) -> StyleKey`, `StyleKey::plain() -> StyleKey`
  - `crate::font::FontChain` with `pub fn new(cfg: &FontConfig) -> Self`, `pub fn from_files(files: &[PathBuf], lang: Language) -> Self`, `pub fn primary(&self) -> fontdb::ID`, `pub fn db(&self) -> &fontdb::Database`, `pub fn find_family(&self, name: &str) -> Option<fontdb::ID>`, `pub fn resolve(&mut self, c: char, style: &StyleKey) -> (fontdb::ID, RasterFlags)`, `pub fn font(&mut self, id: fontdb::ID) -> Arc<cosmic_text::Font>`
  - `crate::font::language_candidates(lang: Language) -> &'static [&'static str]`
  - `crate::font::bundled_font_path() -> PathBuf`, `pub const crate::font::BUNDLED_FONT: &[u8]`
  - free helpers `crate::font::{family_eq(&str, &str) -> bool, find_family(&fontdb::Database, &str) -> Option<fontdb::ID>, font_covers(&Font, char) -> bool, face_covers(&fontdb::Database, fontdb::ID, char) -> bool, load_dir(&mut fontdb::Database, &Path) -> Vec<fontdb::ID>}` (private: `is_bold`, `is_italic`, `first_regular`, `style_score`, `load_bundled`)
  - Cargo: `erars-renderer` gains `smol_str = "0.2"`, `bitflags = "2"`; `cosmic-text = "0.12.1"` with default features only.

Decisions taken where the spec leaves room (also listed in open_questions): the chain's tail "every other face in load order" is realised lazily by a database-wide `with_face_data` scan over faces not already in the chain (nothing is materialised until chosen; a face that then fails to load is skipped); `from_files` treats its files as `ERARS_FONT_DIR` faces; inside a directory the *primary* is the first upright weight-400 face in sorted path order (else the first face); "bold" means fontdb weight ≥ 600 (SEMIBOLD), "italic" means `Style != Normal` (Italic or Oblique); a face is "of the same family" when any of its family names equals any of the regular face's (case-insensitive); with `bold+italic` requested and no bold-italic face, a bold (or italic) face is used and only the missing attribute is synthesised — a face carrying a style the part did not ask for is never used; the locale passed to `FontSystem` is always `"en-US"` (only cosmic-text's unused `Buffer` fallback reads it); a database-wide miss costs one mmap+parse per system face (measured 366 ms for `😀` over 1826 faces, then cached per `(char, StyleKey)`).

- [ ] **Step 1: Cargo — drop the two cosmic-text features, add `smol_str` and `bitflags`.**

Replace `crates/erars-renderer/Cargo.toml` lines 22-32 (from `winit = "0.30"` through `features = ["monospace_fallback", "shape-run-cache"]`) with:

```toml
winit = "0.30"
wgpu = "0.19"
pollster = "0.3"
bytemuck = { version = "1", features = ["derive"] }
etagere = "0.2"
unicode-width = "0.1"
sys-locale = "0.3"
smol_str = "0.2"
bitflags = "2"
cosmic-text = "0.12.1"
```

(`unicode-width` and `sys-locale` stay: the legacy `text.rs`/`font.rs` code still uses them; Task 10 removes them. `smol_str` resolves to the locked 0.2.1, `bitflags` to the locked 2.6.0 — `Cargo.lock` only gains the two edges, no new crate versions, so `cargo` works offline. The feature drop is verified harmless for the legacy tests — see the facts above.)

Run: `cargo build -p erars-renderer 2>&1 | tail -3` → expected `Finished` with no errors (warnings allowed).

- [ ] **Step 2: Create `crates/erars-renderer/src/flags.rs` with `RasterFlags` and its test.**

```rust
//! Synthetic-style flags shared by the shaper (`text.rs`), the layout and the
//! rasteriser. Lives in its own module so the GPU-free layers never import
//! the GPU module (spec Component 4, critique R34).

bitflags::bitflags! {
    /// Set by `FontChain::resolve` when no real bold / italic face of the
    /// resolved family exists; the rasteriser then emboldens / skews the
    /// outline instead (spec Component 6).
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
    pub struct RasterFlags: u8 {
        const BOLD_SYNTH = 1;
        const ITALIC_SYNTH = 2;
    }
}

#[cfg(test)]
mod tests {
    use super::RasterFlags;

    #[test]
    fn flags_are_independent_bits() {
        assert_eq!(RasterFlags::BOLD_SYNTH.bits(), 1);
        assert_eq!(RasterFlags::ITALIC_SYNTH.bits(), 2);
        assert!(RasterFlags::empty().is_empty());
        assert_eq!(RasterFlags::default(), RasterFlags::empty());
        let both = RasterFlags::BOLD_SYNTH | RasterFlags::ITALIC_SYNTH;
        assert!(both.contains(RasterFlags::BOLD_SYNTH));
        assert!(both.contains(RasterFlags::ITALIC_SYNTH));
        assert_eq!(both.bits(), 3);
    }
}
```

- [ ] **Step 3: Register the module in `main.rs` and silence dead-code noise until Task 10.**

Replace `crates/erars-renderer/src/main.rs` lines 3-13 (`mod app;` … `mod text;`) with:

```rust
mod app;
mod atlas;
mod draw;
#[allow(dead_code)] // RasterFlags is consumed by the shaper/raster rewrite (T6/T8)
mod flags;
#[allow(dead_code)] // FontChain is wired into the app in T10; FontCtx stays until then
mod font;
mod gpu;
mod grid;
#[allow(dead_code)]
mod headless;
#[cfg(test)]
mod test_support;
mod text;
```

Run: `cargo test -p erars-renderer flags::` → expected

```
test flags::tests::flags_are_independent_bits ... ok
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; N filtered out
```

Then run the whole crate once to prove the feature drop is harmless: `cargo test -p erars-renderer 2>&1 | grep 'test result'` → every line `ok`, `0 failed` (GPU tests may print "no GPU adapter; skipping" and still count as ok).

- [ ] **Step 4: Commit the build/flags change.**

```
git add crates/erars-renderer/Cargo.toml Cargo.lock crates/erars-renderer/src/flags.rs crates/erars-renderer/src/main.rs && git commit -m "build(renderer): drop cosmic-text fallback features, add RasterFlags module"
```

- [ ] **Step 5: `font.rs` — new imports and public bundled-font constant.**

Replace `crates/erars-renderer/src/font.rs` line 1 (`use cosmic_text::{fontdb, Attrs, Buffer, Family, FontSystem, Metrics, Shaping};`) with:

```rust
//! Font loading and per-character fallback.
//!
//! `FontChain` (spec Component 3) is the new API: an ordered list of faces,
//! a per-`(char, StyleKey)` resolution cache and real-vs-synthetic bold/italic
//! selection. `FontCtx` below it is the legacy cosmic-text `Buffer` path that
//! the old grid renderer still uses; Task 10 deletes it.

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};

use cosmic_text::{fontdb, ttf_parser, Attrs, Buffer, Family, Font, FontSystem, Metrics, Shaping};
use erars_compiler::Language;
use erars_ui::{FontStyle, TextStyle};
use smol_str::SmolStr;

use crate::flags::RasterFlags;
```

Change line 27 (now shifted; the line reading `const BUNDLED_FONT: &[u8] = include_bytes!("../assets/NotoSansMono-Regular.ttf");`) to:

```rust
pub const BUNDLED_FONT: &[u8] = include_bytes!("../assets/NotoSansMono-Regular.ttf");
```

Run: `cargo build -p erars-renderer 2>&1 | grep -E '^(error|warning: unused)' | head` → only `unused import` warnings for the new names (no errors).

- [ ] **Step 6: Write the failing `chain_tests` first.**

Insert the following block immediately **before** the line `#[cfg(test)]` that starts the legacy `mod tests` (currently the last `#[cfg(test)]` in the file, right after `measure_cell_w`). Every later step inserts its code *above this test module* (i.e. between `measure_cell_w` and `mod chain_tests`).

```rust
#[cfg(test)]
mod chain_tests {
    use super::*;
    use erars_ui::Color;

    fn bundled_chain() -> FontChain {
        FontChain::from_files(&[bundled_font_path()], Language::Japanese)
    }

    fn key(family: &str, bold: bool, italic: bool) -> StyleKey {
        StyleKey { family: SmolStr::new(family), bold, italic }
    }

    /// Fresh per-test scratch directory (tests run in parallel).
    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("erars-font-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn source_path(db: &fontdb::Database, id: fontdb::ID) -> PathBuf {
        match &db.face(id).expect("face exists").source {
            fontdb::Source::File(p) | fontdb::Source::SharedFile(p, _) => p.clone(),
            fontdb::Source::Binary(_) => panic!("expected a file-backed face"),
        }
    }

    /// `$ERARS_FONT_DIR/msgothic.ttc` when present (opt-in, never in CI).
    /// Otherwise prints a SKIP line, or panics when `ERARS_REQUIRE_CJK_FONT=1`
    /// insists on the font being there (spec Testing §5 gating).
    fn msgothic(test: &str) -> Option<PathBuf> {
        let path = std::env::var_os("ERARS_FONT_DIR")
            .map(|dir| PathBuf::from(dir).join("msgothic.ttc"))
            .filter(|p| p.is_file());
        if path.is_none() {
            let msg = format!("SKIP {test}: $ERARS_FONT_DIR/msgothic.ttc not found");
            if std::env::var_os("ERARS_REQUIRE_CJK_FONT").is_some_and(|v| v == "1") {
                panic!("{msg} (ERARS_REQUIRE_CJK_FONT=1)");
            }
            eprintln!("{msg}");
        }
        path
    }

    fn face_info(weight: u16, style: fontdb::Style) -> fontdb::FaceInfo {
        fontdb::FaceInfo {
            id: fontdb::ID::dummy(),
            source: fontdb::Source::Binary(Arc::new(Vec::<u8>::new())),
            index: 0,
            families: vec![("Test".to_string(), fontdb::Language::English_UnitedStates)],
            post_script_name: "Test".to_string(),
            style,
            weight: fontdb::Weight(weight),
            stretch: fontdb::Stretch::Normal,
            monospaced: true,
        }
    }

    #[test]
    fn bundled_is_primary_and_family_matches_case_insensitively() {
        let chain = bundled_chain();
        let primary = chain.primary();
        let info = chain.db().face(primary).unwrap();
        assert_eq!(info.families[0].0, "Noto Sans Mono");
        assert_eq!(chain.find_family("noto sans mono"), Some(primary));
        assert_eq!(chain.find_family("NOTO SANS MONO"), Some(primary));
        assert_eq!(chain.find_family("No Such Font"), None);
    }

    #[test]
    fn covered_chars_resolve_to_primary_without_flags() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        for c in ['A', ' ', '┏', '━', '═', '░', '█'] {
            assert_eq!(chain.resolve(c, &StyleKey::plain()), (primary, RasterFlags::empty()), "{c:?}");
            assert!(font_covers(&chain.font(primary), c), "{c:?} must be in the bundled cmap");
        }
    }

    #[test]
    fn uncovered_chars_fall_back_to_primary_notdef() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        for c in ['あ', '한', '漢'] {
            let (id, flags) = chain.resolve(c, &StyleKey::plain());
            assert_eq!((id, flags), (primary, RasterFlags::empty()), "{c:?}");
            assert!(!font_covers(&chain.font(id), c), "{c:?} is not in Noto Sans Mono");
        }
    }

    #[test]
    fn glyph_id_zero_is_not_coverage() {
        let mut chain = bundled_chain();
        let font = chain.font(chain.primary());
        // format-4 delta segment: ttf-parser answers Some(GlyphId(0)) for U+FFFF
        assert_eq!(font.rustybuzz().glyph_index('\u{FFFF}'), Some(ttf_parser::GlyphId(0)));
        assert!(!font_covers(&font, '\u{FFFF}'));
        assert!(!face_covers(chain.db(), chain.primary(), '\u{FFFF}'));
        assert!(face_covers(chain.db(), chain.primary(), 'A'));
    }

    #[test]
    fn missing_bold_italic_faces_are_synthesised() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        assert_eq!(chain.resolve('A', &key("", true, false)), (primary, RasterFlags::BOLD_SYNTH));
        assert_eq!(chain.resolve('A', &key("", false, true)), (primary, RasterFlags::ITALIC_SYNTH));
        assert_eq!(
            chain.resolve('A', &key("", true, true)),
            (primary, RasterFlags::BOLD_SYNTH | RasterFlags::ITALIC_SYNTH)
        );
        // an uncovered character still reports the requested synthesis
        assert_eq!(chain.resolve('あ', &key("", true, false)), (primary, RasterFlags::BOLD_SYNTH));
    }

    #[test]
    fn unknown_setfont_family_uses_default_chain() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        assert_eq!(chain.resolve('A', &key("Nope Sans", false, false)), (primary, RasterFlags::empty()));
        assert_eq!(chain.resolve('A', &key("Nope Sans", true, false)), (primary, RasterFlags::BOLD_SYNTH));
        // a SETFONT family that exists is honoured (here it is the primary itself)
        assert_eq!(chain.resolve('A', &key("noto sans mono", false, false)), (primary, RasterFlags::empty()));
    }

    #[test]
    fn resolve_is_cached_per_char_and_style() {
        let mut chain = bundled_chain();
        assert_eq!(chain.cache_len(), 0);
        chain.resolve('A', &StyleKey::plain());
        chain.resolve('A', &StyleKey::plain());
        assert_eq!(chain.cache_len(), 1);
        chain.resolve('A', &key("", true, false));
        chain.resolve('B', &StyleKey::plain());
        assert_eq!(chain.cache_len(), 3);
    }

    #[test]
    fn font_returns_the_requested_face() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        let font = chain.font(primary);
        assert_eq!(font.id(), primary);
        assert_eq!(font.rustybuzz().units_per_em(), 1000);
    }

    #[test]
    fn style_key_from_text_style_ignores_colour_underline_strike() {
        let style = TextStyle {
            color: Color([1, 2, 3]),
            font_family: SmolStr::new("MS Gothic"),
            font_style: FontStyle::BOLD | FontStyle::UNDERLINE | FontStyle::STRIKELINE,
        };
        assert_eq!(StyleKey::from(&style), key("MS Gothic", true, false));
        let italic = TextStyle {
            color: Color([9, 9, 9]),
            font_family: SmolStr::default(),
            font_style: FontStyle::ITALIC,
        };
        assert_eq!(StyleKey::from(&italic), key("", false, true));
        assert_eq!(StyleKey::plain(), key("", false, false));
    }

    #[test]
    fn language_candidates_start_with_the_emuera_defaults() {
        assert_eq!(language_candidates(Language::Japanese)[0], "MS Gothic");
        assert_eq!(language_candidates(Language::Korean)[0], "D2Coding");
        assert_eq!(language_candidates(Language::ChineseHans)[0], "NSimSun");
        assert_eq!(language_candidates(Language::ChineseHant)[0], "NSimSun");
        assert!(language_candidates(Language::Korean).contains(&"GulimChe"));
        assert!(language_candidates(Language::Japanese).contains(&"Noto Sans Mono CJK JP"));
    }

    #[test]
    fn family_eq_is_unicode_case_insensitive() {
        assert!(family_eq("MS Gothic", "ms gothic"));
        assert!(family_eq("ＭＳ ゴシック", "ＭＳ ゴシック"));
        assert!(family_eq("Sarasa Mono K", "SARASA MONO K"));
        assert!(!family_eq("MS Gothic", "MS PGothic"));
        assert!(!family_eq("MS Gothic", "MS Gothic "));
    }

    #[test]
    fn style_score_counts_matches_and_rejects_unrequested_styles() {
        let regular = face_info(400, fontdb::Style::Normal);
        let bold = face_info(700, fontdb::Style::Normal);
        let semibold = face_info(600, fontdb::Style::Normal);
        let medium = face_info(500, fontdb::Style::Normal);
        let italic = face_info(400, fontdb::Style::Italic);
        let oblique = face_info(400, fontdb::Style::Oblique);
        let bold_italic = face_info(700, fontdb::Style::Italic);
        let b = key("", true, false);
        let i = key("", false, true);
        let bi = key("", true, true);
        assert_eq!(style_score(&regular, &b), Some(0));
        assert_eq!(style_score(&bold, &b), Some(1));
        assert_eq!(style_score(&semibold, &b), Some(1));
        assert_eq!(style_score(&medium, &b), Some(0), "500 is not bold");
        assert_eq!(style_score(&italic, &b), None, "italic not requested");
        assert_eq!(style_score(&bold_italic, &b), None);
        assert_eq!(style_score(&italic, &i), Some(1));
        assert_eq!(style_score(&oblique, &i), Some(1));
        assert_eq!(style_score(&bold, &i), None);
        assert_eq!(style_score(&bold_italic, &bi), Some(2));
        assert_eq!(style_score(&bold, &bi), Some(1));
        assert_eq!(style_score(&italic, &bi), Some(1));
        assert_eq!(style_score(&regular, &bi), Some(0));
    }

    #[test]
    fn load_dir_is_recursive_sorted_and_extension_filtered() {
        let dir = scratch("load-dir");
        std::fs::create_dir_all(dir.join("sub")).unwrap();
        std::fs::write(dir.join("b.TTF"), BUNDLED_FONT).unwrap();
        std::fs::write(dir.join("sub").join("a.otf"), BUNDLED_FONT).unwrap();
        std::fs::write(dir.join("readme.txt"), b"not a font").unwrap();
        std::fs::write(dir.join("broken.ttf"), b"garbage").unwrap();
        let mut db = fontdb::Database::new();
        let ids = load_dir(&mut db, &dir);
        let names: Vec<String> = ids
            .iter()
            .map(|id| source_path(&db, *id).strip_prefix(&dir).unwrap().to_string_lossy().into_owned())
            .collect();
        assert_eq!(names, vec!["b.TTF", "sub/a.otf"]);
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn from_files_empty_falls_back_to_bundled() {
        let mut chain = FontChain::from_files(&[], Language::Korean);
        let primary = chain.primary();
        assert_eq!(chain.db().face(primary).unwrap().families[0].0, "Noto Sans Mono");
        assert_eq!(chain.resolve('A', &StyleKey::plain()), (primary, RasterFlags::empty()));
    }

    /// `<game>/font/` faces precede the language list, so a game-shipped font
    /// is the primary even with system fonts loaded.
    #[test]
    fn game_font_dir_face_becomes_primary() {
        let game_dir = scratch("game-dir");
        std::fs::create_dir_all(game_dir.join("font")).unwrap();
        std::fs::write(game_dir.join("font").join("zz.ttf"), BUNDLED_FONT).unwrap();
        let mut chain = FontChain::new(&FontConfig {
            family: "",
            game_dir: &game_dir,
            extra_dir: None,
            lang: Language::Korean,
        });
        let primary = chain.primary();
        assert_eq!(source_path(chain.db(), primary), game_dir.join("font").join("zz.ttf"));
        assert_eq!(chain.resolve('A', &StyleKey::plain()), (primary, RasterFlags::empty()));
        std::fs::remove_dir_all(&game_dir).unwrap();
    }

    #[test]
    fn extra_dir_precedes_language_list_and_configured_family_wins() {
        let extra = scratch("extra-dir");
        std::fs::write(extra.join("extra.ttf"), BUNDLED_FONT).unwrap();
        let game_dir = scratch("extra-dir-game");
        let chain = FontChain::new(&FontConfig {
            family: "",
            game_dir: &game_dir,
            extra_dir: Some(extra.clone()),
            lang: Language::Japanese,
        });
        assert_eq!(source_path(chain.db(), chain.primary()), extra.join("extra.ttf"));
        // a configured family that is present outranks the directories
        let chain = FontChain::new(&FontConfig {
            family: "noto sans mono",
            game_dir: &game_dir,
            extra_dir: Some(extra.clone()),
            lang: Language::Japanese,
        });
        let info = chain.db().face(chain.primary()).unwrap();
        assert_eq!(info.families[0].0, "Noto Sans Mono");
        std::fs::remove_dir_all(&extra).unwrap();
        std::fs::remove_dir_all(&game_dir).unwrap();
    }

    /// Needs an installed family with upright regular and bold faces (DejaVu
    /// Sans Mono, Liberation Mono, …); prints SKIP otherwise.
    #[test]
    fn real_bold_face_is_preferred_over_synthesis() {
        let game_dir = scratch("real-bold");
        let mut chain = FontChain::new(&FontConfig {
            family: "",
            game_dir: &game_dir,
            extra_dir: None,
            lang: Language::Korean,
        });
        // lower-cased first family name → (upright regular ids, upright bold ids)
        let mut by_family: std::collections::BTreeMap<String, (Vec<fontdb::ID>, Vec<fontdb::ID>)> =
            Default::default();
        for f in chain.db().faces() {
            let Some((name, _)) = f.families.first() else { continue };
            if is_italic(f) {
                continue;
            }
            let entry = by_family.entry(name.to_lowercase()).or_default();
            if f.weight == fontdb::Weight::NORMAL {
                entry.0.push(f.id);
            } else if is_bold(f) {
                entry.1.push(f.id);
            }
        }
        let pick = by_family.iter().find_map(|(name, (regular, bold))| {
            let r = regular.iter().copied().find(|&id| face_covers(chain.db(), id, 'A'))?;
            let b = bold.iter().copied().find(|&id| face_covers(chain.db(), id, 'A'))?;
            Some((name.clone(), r, b))
        });
        let Some((name, _regular, _bold)) = pick else {
            eprintln!("SKIP real_bold_face_is_preferred_over_synthesis: no installed family has upright regular + bold faces");
            std::fs::remove_dir_all(&game_dir).unwrap();
            return;
        };
        let (plain_id, plain_flags) = chain.resolve('A', &key(&name, false, false));
        let plain = chain.db().face(plain_id).unwrap();
        assert!(!is_bold(plain) && !is_italic(plain), "{name}: SETFONT regular");
        assert_eq!(plain_flags, RasterFlags::empty());

        let (bold_id, bold_flags) = chain.resolve('A', &key(&name, true, false));
        let bold = chain.db().face(bold_id).unwrap();
        assert!(bold.families.iter().any(|(n, _)| family_eq(n, &name)), "{name}: same family");
        assert!(is_bold(bold) && !is_italic(bold), "{name}: real upright bold face");
        assert_eq!(bold_flags, RasterFlags::empty(), "{name}: nothing synthesised");
        assert_ne!(bold_id, plain_id);

        // bold + italic: a bold face is used even when no bold-italic exists;
        // only the missing posture is synthesised.
        let (bi_id, bi_flags) = chain.resolve('A', &key(&name, true, true));
        let bi = chain.db().face(bi_id).unwrap();
        assert!(is_bold(bi), "{name}: bold face for bold+italic");
        assert!(!bi_flags.contains(RasterFlags::BOLD_SYNTH));
        assert_eq!(bi_flags.contains(RasterFlags::ITALIC_SYNTH), !is_italic(bi));
        std::fs::remove_dir_all(&game_dir).unwrap();
    }

    #[test]
    fn msgothic_both_family_names_match() {
        let Some(ms) = msgothic("msgothic_both_family_names_match") else { return };
        let mut chain = FontChain::from_files(&[ms, bundled_font_path()], Language::Japanese);
        let primary = chain.primary();
        let info = chain.db().face(primary).unwrap();
        assert_eq!(info.index, 0, "face 0 of msgothic.ttc is MS Gothic");
        assert!(info.monospaced);
        let names: Vec<&str> = info.families.iter().map(|f| f.0.as_str()).collect();
        assert!(names.contains(&"MS Gothic") && names.contains(&"ＭＳ ゴシック"), "{names:?}");
        assert_eq!(chain.find_family("MS Gothic"), Some(primary));
        assert_eq!(chain.find_family("ＭＳ ゴシック"), Some(primary));
        assert_eq!(chain.find_family("ms gothic"), Some(primary));
        assert_ne!(chain.find_family("MS PGothic"), Some(primary));
        for c in ['A', 'あ', '漢', '─', '═', '║', '░'] {
            assert_eq!(chain.resolve(c, &StyleKey::plain()), (primary, RasterFlags::empty()), "{c:?}");
        }
        // nothing in this database covers Hangul: primary .notdef
        let (id, _) = chain.resolve('한', &StyleKey::plain());
        assert_eq!(id, primary);
        assert!(!font_covers(&chain.font(id), '한'));
        assert_eq!(chain.resolve('あ', &key("", true, false)), (primary, RasterFlags::BOLD_SYNTH));
    }

    /// With system fonts: あ stays on MS Gothic, 한 goes to whatever face
    /// covers it (a chain CJK font or the database-wide fallback), never MS Gothic.
    #[test]
    fn msgothic_resolves_kana_and_hangul_falls_elsewhere() {
        let Some(ms) = msgothic("msgothic_resolves_kana_and_hangul_falls_elsewhere") else { return };
        let game_dir = scratch("msgothic-game");
        std::fs::create_dir_all(game_dir.join("font")).unwrap();
        std::fs::copy(&ms, game_dir.join("font").join("msgothic.ttc")).unwrap();
        let mut chain = FontChain::new(&FontConfig {
            family: "",
            game_dir: &game_dir,
            extra_dir: None,
            lang: Language::Japanese,
        });
        let ms_id = chain.find_family("MS Gothic").expect("game font dir loaded");
        assert_eq!(chain.primary(), ms_id);
        for c in ['A', 'あ', '─', '═'] {
            assert_eq!(chain.resolve(c, &StyleKey::plain()), (ms_id, RasterFlags::empty()), "{c:?}");
        }
        let (id, flags) = chain.resolve('한', &StyleKey::plain());
        let any_hangul = chain.db().faces().map(|f| f.id).any(|id| face_covers(chain.db(), id, '한'));
        if any_hangul {
            assert_ne!(id, ms_id, "Hangul must not be drawn with MS Gothic's .notdef");
            assert!(font_covers(&chain.font(id), '한'));
            assert_eq!(flags, RasterFlags::empty());
        } else {
            assert_eq!(id, ms_id);
        }
        std::fs::remove_dir_all(&game_dir).unwrap();
    }
}
```

Run: `cargo test -p erars-renderer font::chain_tests 2>&1 | grep -E '^error' | sort | uniq -c | head` → expected compile errors only, e.g. `error[E0433]: failed to resolve: use of undeclared type `FontChain`` / `error[E0412]: cannot find type `StyleKey`` / `error[E0425]: cannot find function `bundled_font_path``.

- [ ] **Step 7: Add `bundled_font_path`, `FontConfig`, `StyleKey` and `language_candidates`.**

Insert above `mod chain_tests` (after `measure_cell_w`):

```rust
/// Path of the bundled font on disk (for `FontChain::from_files` in tests
/// and layout goldens; never calls `load_system_fonts`).
pub fn bundled_font_path() -> PathBuf {
    PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf"))
}

/// Where the fonts come from (spec Component 3).
pub struct FontConfig<'a> {
    /// `emuera.config` フォント名; may be empty (no configured family).
    pub family: &'a str,
    /// `<game>/font/*.ttf|ttc|otf|otc` is loaded (Emuera.EM behaviour).
    pub game_dir: &'a Path,
    /// `ERARS_FONT_DIR` (the caller reads the environment).
    pub extra_dir: Option<PathBuf>,
    pub lang: Language,
}

/// The shaping-relevant part of a `TextStyle`: colour, underline and strike
/// are not resolution inputs. `family` is the part's SETFONT family ("" =
/// the configured chain).
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StyleKey {
    pub family: SmolStr,
    pub bold: bool,
    pub italic: bool,
}

impl StyleKey {
    pub fn from(style: &TextStyle) -> Self {
        Self {
            family: style.font_family.clone(),
            bold: style.font_style.contains(FontStyle::BOLD),
            italic: style.font_style.contains(FontStyle::ITALIC),
        }
    }

    /// No SETFONT family, regular weight and posture.
    pub fn plain() -> Self {
        Self {
            family: SmolStr::default(),
            bold: false,
            italic: false,
        }
    }
}

/// Fixed-pitch CJK families tried (in order) for a game language, after the
/// configured family and the game / extra font directories (spec Component 3;
/// the Windows-localised names are aliases fontdb also exposes).
pub fn language_candidates(lang: Language) -> &'static [&'static str] {
    match lang {
        Language::Japanese => &[
            "MS Gothic",
            "ＭＳ ゴシック",
            "Sarasa Mono J",
            "Noto Sans Mono CJK JP",
        ],
        Language::Korean => &[
            "D2Coding",
            "NanumGothicCoding",
            "GulimChe",
            "굴림체",
            "DotumChe",
            "돋움체",
            "Sarasa Mono K",
            "Noto Sans Mono CJK KR",
        ],
        Language::ChineseHans => &[
            "NSimSun",
            "Sarasa Mono SC",
            "Sarasa Mono TC",
            "Noto Sans Mono CJK SC",
            "Noto Sans Mono CJK TC",
        ],
        Language::ChineseHant => &[
            "NSimSun",
            "Sarasa Mono TC",
            "Sarasa Mono SC",
            "Noto Sans Mono CJK TC",
            "Noto Sans Mono CJK SC",
        ],
    }
}
```

Run: `cargo test -p erars-renderer font::chain_tests 2>&1 | grep -c 'FontChain'` → still compile errors (count > 0) mentioning `FontChain`, `family_eq`, `style_score`, … only.

- [ ] **Step 8: Add the free helpers (family match, style classification, coverage, loading).**

Insert above `mod chain_tests`:

```rust
/// Case-insensitive family comparison (Unicode lowercase, so `ＭＳ ゴシック`
/// compares byte-exact while `ms gothic` matches `MS Gothic`).
pub fn family_eq(a: &str, b: &str) -> bool {
    a.chars()
        .flat_map(char::to_lowercase)
        .eq(b.chars().flat_map(char::to_lowercase))
}

/// Regular face (upright, weight nearest 400, then load order) of the family
/// `name`, scanning `FaceInfo.families` (name ID 16 in every language, or ID 1
/// when the font has no ID 16). `Database::query` is not used: it is exact
/// and case-sensitive.
pub fn find_family(db: &fontdb::Database, name: &str) -> Option<fontdb::ID> {
    db.faces()
        .filter(|f| f.families.iter().any(|(n, _)| family_eq(n, name)))
        .min_by_key(|f| {
            (
                f.style != fontdb::Style::Normal,
                f.weight.0.abs_diff(fontdb::Weight::NORMAL.0),
            )
        })
        .map(|f| f.id)
}

/// Weight ≥ 600 (SemiBold) counts as a real bold face.
fn is_bold(f: &fontdb::FaceInfo) -> bool {
    f.weight >= fontdb::Weight::SEMIBOLD
}

/// Italic or Oblique counts as a real italic face.
fn is_italic(f: &fontdb::FaceInfo) -> bool {
    f.style != fontdb::Style::Normal
}

/// How many of the requested attributes `f` satisfies, or `None` when it is
/// bold / italic although the part did not ask for it.
fn style_score(f: &fontdb::FaceInfo, style: &StyleKey) -> Option<u8> {
    let (b, i) = (is_bold(f), is_italic(f));
    if (b && !style.bold) || (i && !style.italic) {
        return None;
    }
    Some(u8::from(b && style.bold) + u8::from(i && style.italic))
}

/// First upright weight-400 face of `ids` (load order), else the first id.
fn first_regular(db: &fontdb::Database, ids: &[fontdb::ID]) -> Option<fontdb::ID> {
    ids.iter()
        .copied()
        .find(|&id| {
            db.face(id)
                .map_or(false, |f| !is_italic(f) && f.weight == fontdb::Weight::NORMAL)
        })
        .or_else(|| ids.first().copied())
}

/// cmap coverage through a loaded `Font` (`rustybuzz::Face` derefs to
/// `ttf_parser::Face`). The `!= 0` guard matters: format-4 delta segments can
/// yield `GlyphId(0)` (e.g. U+FFFF in the bundled font).
pub fn font_covers(font: &Font, c: char) -> bool {
    font.rustybuzz().glyph_index(c).map_or(false, |g| g.0 != 0)
}

/// cmap coverage without materialising a `Font` (database-wide scans; faces
/// are only turned into `Font`s once chosen — critique R20).
pub fn face_covers(db: &fontdb::Database, id: fontdb::ID, c: char) -> bool {
    db.with_face_data(id, |data, index| {
        ttf_parser::Face::parse(data, index)
            .ok()
            .and_then(|face| face.glyph_index(c))
            .map_or(false, |g| g.0 != 0)
    })
    .unwrap_or(false)
}

/// Register the bundled Noto Sans Mono (zero-copy, static bytes).
fn load_bundled(db: &mut fontdb::Database) -> Vec<fontdb::ID> {
    let source = fontdb::Source::Binary(Arc::new(BUNDLED_FONT) as Arc<dyn AsRef<[u8]> + Send + Sync>);
    db.load_font_source(source).to_vec()
}

/// Load every `ttf|ttc|otf|otc` under `dir` (recursive, sorted by path so the
/// order is deterministic) and return the face ids in load order. Unlike
/// `Database::load_fonts_dir` this reports which ids came from the directory.
pub fn load_dir(db: &mut fontdb::Database, dir: &Path) -> Vec<fontdb::ID> {
    let mut ids = Vec::new();
    let Ok(read_dir) = std::fs::read_dir(dir) else {
        return ids;
    };
    let mut entries: Vec<PathBuf> = read_dir.flatten().map(|e| e.path()).collect();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            ids.extend(load_dir(db, &path));
            continue;
        }
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.to_ascii_lowercase());
        if matches!(ext.as_deref(), Some("ttf" | "ttc" | "otf" | "otc")) {
            let loaded = db.load_font_source(fontdb::Source::File(path.clone()));
            if loaded.is_empty() {
                log::warn!("No font faces loaded from {}", path.display());
            }
            ids.extend(loaded);
        }
    }
    ids
}
```

Run: `cargo test -p erars-renderer font::chain_tests 2>&1 | grep -E '^error' | head -3` → remaining errors name only `FontChain`.

- [ ] **Step 9: Add the `FontChain` struct and its constructors.**

Insert above `mod chain_tests`:

```rust
/// Ordered per-character font fallback over a fontdb database.
///
/// Chain order: SETFONT family of the part (per `resolve` call) → configured
/// family → faces from `<game>/font/` → faces from `ERARS_FONT_DIR` →
/// per-language fixed-pitch CJK candidates → bundled Noto Sans Mono →
/// (lazily) every other face in load order.
pub struct FontChain {
    /// fontdb + `Font` loading only; `Buffer`/`Attrs` are never used.
    font_system: FontSystem,
    /// Ordered candidates, primary first (regular faces only, deduplicated).
    chain: Vec<fontdb::ID>,
    /// Metrics source; verified loadable at construction.
    primary: fontdb::ID,
    cache: HashMap<(char, StyleKey), (fontdb::ID, RasterFlags)>,
    /// SETFONT families already reported as missing (warn once).
    warned_families: HashSet<SmolStr>,
    /// Faces that failed to load (warn once).
    failed_faces: HashSet<fontdb::ID>,
}

/// Faces grouped by origin, in chain order.
#[derive(Default)]
struct Seeds {
    configured: Vec<fontdb::ID>,
    game_dir: Vec<fontdb::ID>,
    extra_dir: Vec<fontdb::ID>,
    bundled: Vec<fontdb::ID>,
}

impl FontChain {
    /// System fonts + `<game>/font/` + `ERARS_FONT_DIR` + the bundled font.
    pub fn new(cfg: &FontConfig) -> Self {
        let mut db = fontdb::Database::new();
        db.load_system_fonts();
        let mut seeds = Seeds::default();
        let game_font_dir = cfg.game_dir.join("font");
        if game_font_dir.is_dir() {
            seeds.game_dir = load_dir(&mut db, &game_font_dir);
            log::info!(
                "Loaded {} face(s) from {}",
                seeds.game_dir.len(),
                game_font_dir.display()
            );
        }
        if let Some(dir) = &cfg.extra_dir {
            seeds.extra_dir = load_dir(&mut db, dir);
            log::info!(
                "Loaded {} face(s) from ERARS_FONT_DIR={}",
                seeds.extra_dir.len(),
                dir.display()
            );
        }
        seeds.bundled = load_bundled(&mut db);
        if !cfg.family.is_empty() {
            match find_family(&db, cfg.family) {
                Some(id) => seeds.configured.push(id),
                None => log::warn!(
                    "Configured font family {:?} is not installed; using the per-language chain",
                    cfg.family
                ),
            }
        }
        Self::build(db, seeds, cfg.lang)
    }

    /// Tests and goldens: exactly these files (in order), no system fonts, no
    /// locale. The files play the role of `ERARS_FONT_DIR` faces; with an
    /// empty list the bundled font is loaded so the chain is never empty.
    pub fn from_files(files: &[PathBuf], lang: Language) -> Self {
        let mut db = fontdb::Database::new();
        let mut seeds = Seeds::default();
        for path in files {
            let ids = db.load_font_source(fontdb::Source::File(path.clone()));
            if ids.is_empty() {
                log::warn!("No font faces loaded from {}", path.display());
            }
            seeds.extra_dir.extend(ids);
        }
        Self::build(db, seeds, lang)
    }

    fn build(db: fontdb::Database, seeds: Seeds, lang: Language) -> Self {
        let lang_faces: Vec<fontdb::ID> = language_candidates(lang)
            .iter()
            .filter_map(|name| find_family(&db, name))
            .collect();
        let mut chain: Vec<fontdb::ID> = Vec::new();
        for id in seeds
            .configured
            .iter()
            .chain(&seeds.game_dir)
            .chain(&seeds.extra_dir)
            .chain(&lang_faces)
            .chain(&seeds.bundled)
        {
            if !chain.contains(id) {
                chain.push(*id);
            }
        }
        // Primary: first present of configured → game dir → extra dir →
        // language list → bundled (regular face preferred inside a directory).
        let primary_candidates: Vec<fontdb::ID> = seeds
            .configured
            .iter()
            .copied()
            .chain(first_regular(&db, &seeds.game_dir))
            .chain(first_regular(&db, &seeds.extra_dir))
            .chain(lang_faces.iter().copied())
            .chain(seeds.bundled.iter().copied())
            .collect();
        // The locale only feeds cosmic-text's Buffer fallback, which is unused.
        let mut font_system = FontSystem::new_with_locale_and_db(String::from("en-US"), db);
        let mut primary = primary_candidates
            .iter()
            .copied()
            .find(|id| font_system.get_font(*id).is_some());
        if primary.is_none() {
            // Nothing loadable (e.g. `from_files(&[])`): fall back to the bundled font.
            let ids = load_bundled(font_system.db_mut());
            primary = ids
                .iter()
                .copied()
                .find(|id| font_system.get_font(*id).is_some());
            chain.extend(ids);
        }
        let primary = primary.expect("bundled font always loads");
        if let Some(info) = font_system.db().face(primary) {
            log::info!(
                "Primary font: {:?} (face {})",
                info.families.first().map(|f| f.0.as_str()).unwrap_or(""),
                info.index
            );
        }
        Self {
            font_system,
            chain,
            primary,
            cache: HashMap::new(),
            warned_families: HashSet::new(),
            failed_faces: HashSet::new(),
        }
    }

    pub fn primary(&self) -> fontdb::ID {
        self.primary
    }

    pub fn db(&self) -> &fontdb::Database {
        self.font_system.db()
    }

    /// Regular face of a family present in the database (case-insensitive).
    pub fn find_family(&self, name: &str) -> Option<fontdb::ID> {
        find_family(self.font_system.db(), name)
    }

    #[cfg(test)]
    pub(crate) fn cache_len(&self) -> usize {
        self.cache.len()
    }
}
```

Run: `cargo test -p erars-renderer font::chain_tests 2>&1 | grep -E '^error' | head -3` → only `no method named `resolve``/`font` errors remain.

- [ ] **Step 10: Add `resolve`, `font` and the real-vs-synthetic style selection.**

Insert above `mod chain_tests` (a second `impl FontChain` block):

```rust
impl FontChain {
    /// First chain font whose cmap covers `c`. With `bold`/`italic` set, a real
    /// bold/italic face of that family is preferred; if none exists the regular
    /// face is returned with BOLD_SYNTH / ITALIC_SYNTH set. If no chain font
    /// covers `c`, the first face in the whole database that does; else the
    /// primary (renders .notdef). Results are cached per `(char, StyleKey)`.
    pub fn resolve(&mut self, c: char, style: &StyleKey) -> (fontdb::ID, RasterFlags) {
        let key = (c, style.clone());
        if let Some(hit) = self.cache.get(&key) {
            return *hit;
        }
        let regular = self.resolve_regular(c, &style.family);
        let result = self.apply_style(regular, c, style);
        self.cache.insert(key, result);
        result
    }

    /// Owned `Arc` so callers can keep borrowing the chain mutably. A face
    /// that fails to load is drawn with the primary font (warned once per face).
    pub fn font(&mut self, id: fontdb::ID) -> Arc<Font> {
        if let Some(font) = self.font_system.get_font(id) {
            return font;
        }
        if self.failed_faces.insert(id) {
            log::warn!("Font face {id} failed to load; drawing with the primary font");
        }
        self.font_system
            .get_font(self.primary)
            .expect("primary font verified at construction")
    }

    /// The regular face for `c`: SETFONT family → chain → database-wide scan
    /// in load order → primary.
    fn resolve_regular(&mut self, c: char, family: &str) -> fontdb::ID {
        if !family.is_empty() {
            match find_family(self.font_system.db(), family) {
                Some(id) => {
                    if self.loaded_covers(id, c) {
                        return id;
                    }
                }
                None => {
                    if self.warned_families.insert(SmolStr::new(family)) {
                        log::warn!(
                            "SETFONT family {family:?} is not installed; using the default chain"
                        );
                    }
                }
            }
        }
        for i in 0..self.chain.len() {
            let id = self.chain[i];
            if self.loaded_covers(id, c) {
                return id;
            }
        }
        // Database-wide fallback: coverage is read from the raw face data
        // (`with_face_data`); only the chosen face is materialised, and one
        // that fails to load is skipped.
        let others: Vec<fontdb::ID> = self
            .font_system
            .db()
            .faces()
            .map(|f| f.id)
            .filter(|id| !self.chain.contains(id))
            .collect();
        for id in others {
            if face_covers(self.font_system.db(), id, c) && self.font_system.get_font(id).is_some() {
                return id;
            }
        }
        self.primary
    }

    /// Coverage through the materialised `Font` (chain fonts are few and are
    /// needed for shaping anyway).
    fn loaded_covers(&mut self, id: fontdb::ID, c: char) -> bool {
        self.font_system
            .get_font(id)
            .map_or(false, |font| font_covers(&font, c))
    }

    /// Real bold / italic selection. Among the other faces that share a
    /// family name with `regular`, cover `c`, load, and carry no bold/italic
    /// the part did not ask for, the one satisfying most requested attributes
    /// wins (ties: load order); whatever it still lacks is flagged synthetic.
    /// With no such face the regular face carries the flags.
    fn apply_style(
        &mut self,
        regular: fontdb::ID,
        c: char,
        style: &StyleKey,
    ) -> (fontdb::ID, RasterFlags) {
        if !style.bold && !style.italic {
            return (regular, RasterFlags::empty());
        }
        let db = self.font_system.db();
        let family: Vec<String> = db
            .face(regular)
            .map(|f| f.families.iter().map(|(n, _)| n.clone()).collect())
            .unwrap_or_default();
        let mut candidates: Vec<(u8, fontdb::ID)> = db
            .faces()
            .filter(|f| f.id != regular)
            .filter(|f| {
                f.families
                    .iter()
                    .any(|(n, _)| family.iter().any(|m| family_eq(n, m)))
            })
            .filter_map(|f| style_score(f, style).map(|s| (s, f.id)))
            .filter(|(s, _)| *s > 0)
            .collect();
        // stable: equal scores keep load order
        candidates.sort_by(|a, b| b.0.cmp(&a.0));
        let mut chosen = regular;
        for (_, id) in candidates {
            if face_covers(self.font_system.db(), id, c) && self.font_system.get_font(id).is_some() {
                chosen = id;
                break;
            }
        }
        let mut flags = RasterFlags::empty();
        if let Some(info) = self.font_system.db().face(chosen) {
            if style.bold && !is_bold(info) {
                flags |= RasterFlags::BOLD_SYNTH;
            }
            if style.italic && !is_italic(info) {
                flags |= RasterFlags::ITALIC_SYNTH;
            }
        }
        (chosen, flags)
    }
}
```

Run: `cargo test -p erars-renderer font::chain_tests` → expected

```
running 19 tests
... 19 lines ending in `ok` ...
test result: ok. 19 passed; 0 failed; 0 ignored; 0 measured
```

(`msgothic_*` return early and `real_bold_face_is_preferred_over_synthesis` may return early on a box without a regular+bold system family; their `SKIP …` lines are only visible with `cargo test -p erars-renderer font::chain_tests -- --nocapture`.)

- [ ] **Step 11: `main.rs` — source the language list from `font::language_candidates`.**

Replace `crates/erars-renderer/src/main.rs` lines 22-62 (the whole `fn font_candidates` incl. its doc comment) with:

```rust
/// Build the ordered default-font candidate list for the legacy `FontCtx`:
/// the configured family first, then the per-language fixed-pitch CJK
/// families from `font::language_candidates`, then generic monospace
/// baselines. (Task 10 replaces this with `font::FontChain::new`.)
fn font_candidates(lang: Language, configured: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    if !configured.is_empty() {
        out.push(configured.to_string());
    }
    out.extend(
        font::language_candidates(lang)
            .iter()
            .map(|s| s.to_string()),
    );
    out.extend(
        ["DejaVu Sans Mono", "Noto Sans Mono"]
            .iter()
            .map(|s| s.to_string()),
    );
    out
}
```

Run: `cargo test -p erars-renderer font:: 2>&1 | grep 'test result'` → `test result: ok. 21 passed` (19 `chain_tests` + 2 legacy `font::tests`). Then `cargo test -p erars-renderer 2>&1 | grep 'test result'` → all `ok`, `0 failed`.

- [ ] **Step 12: Run the MS Gothic-gated tests (opt-in; `msgothic.ttc` sits untracked at the repo root and must never be committed).**

Run from the repo root:

```
ERARS_FONT_DIR=$PWD cargo test -p erars-renderer font::chain_tests::msgothic -- --nocapture
```

→ expected

```
running 2 tests
test font::chain_tests::msgothic_both_family_names_match ... ok
test font::chain_tests::msgothic_resolves_kana_and_hangul_falls_elsewhere ... ok
test result: ok. 2 passed; 0 failed
```

with no `SKIP` line. (The tests read `$ERARS_FONT_DIR/msgothic.ttc` as a file and copy it into a scratch `font/` dir; they never recurse the repo root.) On this box `한` resolves to a Hangul-covering system face (Sarasa / Noto CJK) — any such face is accepted. Also check the strict gate: `ERARS_REQUIRE_CJK_FONT=1 cargo test -p erars-renderer font::chain_tests::msgothic` (without `ERARS_FONT_DIR`) → both tests **fail** with `panicked … SKIP msgothic_…: $ERARS_FONT_DIR/msgothic.ttc not found (ERARS_REQUIRE_CJK_FONT=1)`; without either variable they pass as skips.

- [ ] **Step 13: Commit.**

```
git add crates/erars-renderer/src/font.rs crates/erars-renderer/src/main.rs && git commit -m "feat(renderer): FontChain — ordered per-character font fallback with real/synthetic bold-italic"
```

---

### Task 6: Shaper — `CellMetrics`, rustybuzz shaping, cluster cache

Implements spec Component 4 (`crates/erars-renderer/src/text.rs`) and the `text.rs` unit tests of spec Testing §4 (last sentence). No GPU, no system fonts: every test uses only `assets/NotoSansMono-Regular.ttf` through `FontChain::from_files`, except the two opt-in MS Gothic tests of Step 9 (`ERARS_FONT_DIR/msgothic.ttc`, `SKIP` otherwise).

Verification done while planning (2026-09-02): the code of Steps 4, 6, 7 and every test of Steps 3, 5, 8, 9 were compiled **verbatim** with `rustc --edition 2021 --test` against the locked rlibs in `target/debug/deps` (rustybuzz 0.14.1, cosmic-text 0.12.1, smol_str 0.2.1, bitflags 2.6.0), first with the T1/T5 types stubbed (`scratchpad/probe-plan-06/probe2.rs`, 17/17) and then with **T5's real `FontChain`/`StyleKey`/`RasterFlags` code** spliced in from `scratchpad/probe-plan-T5/probe_plan.rs` and only `WidthTable` stubbed (`scratchpad/probe-plan-06/probe3.rs`: `test result: ok. 22 passed; 0 failed`, the two MS Gothic-gated tests run for real with `ERARS_FONT_DIR=/home/riey/repos/erars` and print `SKIP …` without it). The numbers asserted in the tests (half_w 11, baseline 19/25/9, `.notdef` advance 600, GPOS offsets −300/230 for the second acute, cluster byte offsets, size 15.0 at half_w 9, dx 3 for a 2-cell `.notdef`, MS Gothic `{18, 9, 19, 15, 3}`, U+0180 absent from MS Gothic but present in Noto Sans Mono) were measured on the fonts in `probe.rs`/`probe3.rs`; `unicode-width 0.1.11` gives U+1160 → 0, U+0180 → 1, U+200D → 0 (`probe-plan-06/uw.rs`), which the real T1 table turns into the cell counts the tests assume.

**Files:**
- Modify: `crates/erars-renderer/Cargo.toml` — insert one line after line 26 (`etagere = "0.2"`): direct `rustybuzz` dependency (cosmic-text 0.12.1 re-exports it as `cosmic_text::rustybuzz`, but a direct dependency keeps `text.rs` independent of that re-export; cargo unifies both on the locked 0.14.1, and `Font::rustybuzz()` returns `&rustybuzz::Face`).
- Modify: `crates/erars-renderer/src/text.rs` — full rewrite. Lines 1–125 (imports, `PlacedGlyph`, `ShapedRun`, `CellShaper`) are kept **verbatim** in a legacy block at the bottom because `grid.rs:5,51,58,91,103,120` and `atlas.rs:158,184` still call `CellShaper::shape_run` until T10 deletes them; lines 127–217 (the old `mod tests`, which encode the `width_cjk` rule this task replaces) are deleted.
- Modify: `crates/erars-renderer/src/main.rs` — one line: after T5's Step 3 the module list occupies lines 3–16 and line 16 reads `mod text;`; prefix it with `#[allow(dead_code)]` (Step 10) so the crate stays warning-free until T7/T10 wire the shaper in.
- Test: in-module `#[cfg(test)] mod tests` in `text.rs` (21 tests: 19 bundled-font tests + 2 MS Gothic-gated tests that print `SKIP …` and pass when `ERARS_FONT_DIR/msgothic.ttc` is absent).
- Also touched by `cargo`: `Cargo.lock` (new dependency edge only; `rustybuzz 0.14.1` is already locked via cosmic-text).

**Precondition:** the crate compiles after T5 (T5 keeps the legacy `crate::font::FontCtx` alive for `grid.rs`/`app.rs`/`headless.rs`/`main.rs` until T10). If `crate::font::FontCtx` is gone, stop and report — do not delete `grid.rs`/`atlas.rs` here (that is T10).

**Interfaces:**
- Consumes (verbatim from the task map / spec):
  - T1: `erars_ui::width::WidthTable { new(&'static Encoding), char_cells(char)->u8, str_cells(&str)->usize }` (`WidthTable::new(encoding: &'static encoding_rs::Encoding) -> Self`, `char_cells(&self, c: char) -> u8`).
  - T2: `Language::encoding(&self) -> &'static encoding_rs::Encoding` on `erars_compiler::Language`.
  - T5: `crates/erars-renderer/src/flags.rs`: `bitflags! { pub struct RasterFlags: u8 { const BOLD_SYNTH = 1; const ITALIC_SYNTH = 2; } }` — must derive `Clone, Copy, Debug, PartialEq, Eq, Hash` (T8's `RasterKey` and this task's `ShapedGlyph: Copy` need it).
  - T5: `crates/erars-renderer/src/font.rs`: `#[derive(Clone, Hash, PartialEq, Eq)] pub struct StyleKey { pub family: SmolStr, pub bold: bool, pub italic: bool }`, `impl StyleKey { pub fn from(style: &TextStyle) -> Self; }`, `FontChain::from_files(files: &[PathBuf], lang: Language) -> Self`, `FontChain::primary(&self) -> fontdb::ID`, `FontChain::find_family(&self, name: &str) -> Option<fontdb::ID>` (gated tests only), `FontChain::resolve(&mut self, c: char, style: &StyleKey) -> (fontdb::ID, RasterFlags)`, `FontChain::font(&mut self, id: fontdb::ID) -> Arc<cosmic_text::Font>`. T5's `from_files` puts every face of every listed file into the chain in file order and makes the first regular face of the first file the primary — the MS Gothic tests rely on `from_files(&[msgothic.ttc, bundled])` giving primary = MS Gothic face 0 and `find_family("Noto Sans Mono")` = the bundled face.
  - Crates (pinned in `Cargo.lock`): rustybuzz 0.14.1 (`UnicodeBuffer::{new, push_str, set_direction, set_cluster_level}`, `shape(&Face, &[Feature], UnicodeBuffer) -> GlyphBuffer`, `GlyphBuffer::{glyph_infos, glyph_positions, clear}`, `GlyphInfo { glyph_id: u32, cluster: u32 }`, `GlyphPosition { x_advance, y_advance, x_offset, y_offset: i32 }` in **font units**, `Feature::new(Tag, u32, impl RangeBounds<usize>)`, `Face::units_per_em(&self) -> i32`, `Face: Deref<Target = ttf_parser::Face>`); ttf-parser 0.21.1 (`Face::{ascender() -> i16, units_per_em() -> u16, glyph_index(char) -> Option<GlyphId>, glyph_hor_advance(GlyphId) -> Option<u16>}`, `Tag::from_bytes(&[u8; 4])`); cosmic-text 0.12.1 (`Font::rustybuzz(&self) -> &rustybuzz::Face<'_>`, `Font::id() -> fontdb::ID`, `cosmic_text::fontdb`); smol_str 0.2.1 (`SmolStr::from(String)`, `SmolStr::as_str`).
- Produces (used by T7 layout, T8 raster/draw, T9 headless, T10 app):
  - `crates/erars-renderer/src/text.rs`:
    - `pub use crate::flags::RasterFlags;`
    - `#[derive(Clone, Copy, Debug, PartialEq)] pub struct CellMetrics { pub scale: f32, pub font_px: u32, pub half_w: u32, pub line_h: u32, pub baseline: u32, pub shift: u32 }`
    - `impl CellMetrics { pub fn from_primary(font: &cosmic_text::Font, font_size: u32, line_height: u32, scale: f32) -> Self }`
    - `#[derive(Clone, Copy, Debug, PartialEq)] pub struct ShapedGlyph { pub font: fontdb::ID, pub glyph: u16, pub dx: i32, pub dy: i32, pub size_px: f32, pub flags: RasterFlags }`
    - `#[derive(Clone, Debug, PartialEq)] pub struct Cluster { pub cells: u8, pub text: SmolStr, pub glyphs: Vec<ShapedGlyph> }`
    - `pub struct Shaper` with `pub fn new(chain: FontChain, widths: WidthTable, m: CellMetrics) -> Self`, `pub fn metrics(&self) -> &CellMetrics`, `pub fn chain(&mut self) -> &mut FontChain`, `pub fn widths(&self) -> &WidthTable` (helper for T7's DRAWLINE `unit = cells(s)·half_w`), `pub fn shape(&mut self, text: &str, style: &TextStyle) -> Arc<[Cluster]>`, `pub fn set_metrics(&mut self, m: CellMetrics)`, `pub fn sweep(&mut self)`.
  - Facts later tasks may rely on (verified with a bare-rustc probe against the locked rlibs, `scratchpad/probe-plan-06/probe.rs`): bundled Noto Sans Mono has upem 1000, ascender 1069, space/`.notdef`/letter advance 600, no CJK, no U+1160, no U+00AD glyph; `CellMetrics::from_primary(bundled, 18, 19, 1.0) == { 1.0, 18, 11, 19, 19, 3 }`; the pinned test metrics for T7/T9 are `CellMetrics { scale: 1.0, font_px: 18, half_w: 9, line_h: 19, baseline: 15, shift: 3 }` (with the bundled font every Latin glyph then takes the `a > w` path: `size_px == 15.0` exactly, `dx == 0`; a 2-cell cluster gets `dx == 3`). `CellMetrics::from_primary(msgothic face 0, 18, 19, 1.0) == { 1.0, 18, 9, 19, 15, 3 }` and every MS Gothic glyph at 18 px has `a == w` (dx 0, size 18).

Rules implemented (spec Component 4), with the choices this task fixes:
- Tabs: `\t` → spaces to the next multiple of 8 cells, counted **from the start of the string passed to `shape`** (a part), not from the row; the cache key stays the original text.
- Text must not contain `\n` (T7 splits before shaping) — `debug_assert!`.
- Spans = maximal runs of chars with equal `(fontdb::ID, RasterFlags)` from `FontChain::resolve`; each span is shaped once with direction forced `LeftToRight`, cluster level `MonotoneGraphemes`, features `liga clig calt kern` = 0.
- Clusters = groups of glyphs with equal `GlyphInfo::cluster` (byte offset into the span, from `push_str`); `cells = Σ char_cells` over the cluster's chars (saturating at 255); `text` = those chars.
- Placement per cluster: `w = cells·half_w`, `a = Σ x_advance · font_px / upem`; `a ≤ w` → `size_px = font_px`, `dx0 = floor((w − a)/2)`; `a > w` → `size_px = w · upem / Σ x_advance` (algebraically `font_px · w / a`, computed this way so an integral result is exact, e.g. 15.0), `dx0 = 0`. Glyph `dx = dx0 + round((pen + x_offset) · size_px / upem)`, `dy = baseline − round(y_offset · size_px / upem)`, `pen` accumulating `x_advance` in font units.
- A 0-cell cluster with a predecessor is merged into the predecessor: its glyphs are placed with the **predecessor's** box width `w` under the same rule and appended, its text appended. A 0-cell cluster at the start of the string stays a `cells: 0` cluster with `w = 0`; its glyphs are drawn at `font_px` from `dx = 0` (the `a > w` shrink would otherwise give `size_px = 0` for a `.notdef` with an advance, e.g. U+1160 in a font without it).
- Direction is forced `LeftToRight` for every span (Emuera/GDI draws ERA text LTR; `guess_segment_properties` would reverse a Hebrew/Arabic span and break the monotone-cluster walk).
- Cache: `HashMap<StyleKey, HashMap<String, (u32, Arc<[Cluster]>)>>`; hit ⇒ stamp `layout_gen`, return the same `Arc`; `sweep()` drops entries whose stamp ≠ `layout_gen`, then bumps `layout_gen`; `set_metrics` replaces `m` and clears everything. Colour, underline and strike are not in `StyleKey`, so they never miss the cache.

---

- [ ] **Step 1: Add the direct `rustybuzz` dependency**

Edit `crates/erars-renderer/Cargo.toml`; after line 26 (`etagere = "0.2"`) insert:

```toml
rustybuzz = "0.14"
```

Resulting block (lines 22–29):

```toml
winit = "0.30"
wgpu = "0.19"
pollster = "0.3"
bytemuck = { version = "1", features = ["derive"] }
etagere = "0.2"
rustybuzz = "0.14"
unicode-width = "0.1"
sys-locale = "0.3"
```

(`smol_str` and `bitflags` were added by T5; if `smol_str = "0.2"` is missing from this `[dependencies]` block, add it on the line after `rustybuzz`.)

Run: `cargo check -p erars-renderer` — expected: `Finished` with no new warnings (only the lock file gains an `erars-renderer → rustybuzz` edge; `rustybuzz 0.14.1` is already locked).

- [ ] **Step 2: Replace `text.rs` with the new header + the legacy block (no new code yet)**

Overwrite `crates/erars-renderer/src/text.rs` with exactly this content. The legacy block is the current file's lines 1–125 with `use erars_ui::TextStyle;` removed (it is imported in the header) — copy it from the file you just read, do not retype it:

```rust
//! Cell-grid text shaping (spec Component 4).
//!
//! Every cluster is boxed into `cells × half_w` px decided by [`WidthTable`]
//! alone; the font only decides which glyphs are drawn inside that box.
//! Shaping is done once per `(StyleKey, text)` with rustybuzz in font units
//! (size-independent), then scaled / centred into the cell box in integer
//! physical pixels. Results are cached per layout generation.

use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

use cosmic_text::fontdb;
use erars_ui::width::WidthTable;
use erars_ui::TextStyle;
use rustybuzz::ttf_parser::{self, Tag};
use rustybuzz::{BufferClusterLevel, Direction, Feature, UnicodeBuffer};
use smol_str::SmolStr;

use crate::font::{FontChain, StyleKey};
pub use crate::flags::RasterFlags;

// ---------------------------------------------------------------------------
// Legacy grid shaper — still called by grid.rs and atlas.rs tests; T10 deletes
// it together with grid.rs / atlas.rs. Do not extend.
// ---------------------------------------------------------------------------

use cosmic_text::{Attrs, Buffer, CacheKey, Family, Metrics, Shaping};
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
```

Run: `cargo check -p erars-renderer` — expected: compiles; warnings `unused import` for `Cow`, `HashMap`, `Arc`, `fontdb`, `WidthTable`, `ttf_parser`, `Tag`, `BufferClusterLevel`, `Direction`, `Feature`, `UnicodeBuffer`, `SmolStr`, `FontChain`, `StyleKey` (all consumed by Steps 4–6).

- [ ] **Step 3: Write the metrics tests (failing)**

Append to the end of `crates/erars-renderer/src/text.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use erars_compiler::Language;
    use erars_ui::{Color, FontStyle};
    use std::path::PathBuf;

    fn bundled() -> PathBuf {
        PathBuf::from(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/assets/NotoSansMono-Regular.ttf"
        ))
    }

    fn chain() -> FontChain {
        FontChain::from_files(&[bundled()], Language::Japanese)
    }

    fn widths() -> WidthTable {
        WidthTable::new(Language::Japanese.encoding())
    }

    /// The pinned metrics of spec Testing §4 (MS Gothic-like 9 px half cell),
    /// parameterised on `half_w` for the placement tests.
    fn pinned(half_w: u32) -> CellMetrics {
        CellMetrics {
            scale: 1.0,
            font_px: 18,
            half_w,
            line_h: 19,
            baseline: 15,
            shift: 3,
        }
    }

    fn shaper(half_w: u32) -> Shaper {
        Shaper::new(chain(), widths(), pinned(half_w))
    }

    fn style() -> TextStyle {
        TextStyle {
            color: Color([255, 255, 255]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        }
    }

    fn styled(font_style: FontStyle) -> TextStyle {
        TextStyle {
            font_style,
            ..style()
        }
    }

    fn cells(clusters: &[Cluster]) -> Vec<u8> {
        clusters.iter().map(|c| c.cells).collect()
    }

    fn texts(clusters: &[Cluster]) -> Vec<&str> {
        clusters.iter().map(|c| c.text.as_str()).collect()
    }

    #[test]
    fn metrics_from_bundled_font() {
        let mut ch = chain();
        let id = ch.primary();
        let font = ch.font(id);
        // Noto Sans Mono: upem 1000, space advance 600, ascender 1069.
        assert_eq!(
            CellMetrics::from_primary(&font, 18, 19, 1.0),
            CellMetrics {
                scale: 1.0,
                font_px: 18,
                half_w: 11,
                line_h: 19,
                baseline: 19,
                shift: 3
            }
        );
    }

    #[test]
    fn metrics_scale_rounds_and_clamps() {
        let mut ch = chain();
        let id = ch.primary();
        let font = ch.font(id);
        // 18 * 1.25 = 22.5 -> 23 px; half 600*23/1000 = 13.8 -> 14; line 23.75 -> 24;
        // baseline 24.6 -> 25; shift 23/6 = 3.
        assert_eq!(
            CellMetrics::from_primary(&font, 18, 19, 1.25),
            CellMetrics {
                scale: 1.25,
                font_px: 23,
                half_w: 14,
                line_h: 24,
                baseline: 25,
                shift: 3
            }
        );
        // Zero config values clamp to Emuera's minimum: font 8 px, line >= font.
        assert_eq!(
            CellMetrics::from_primary(&font, 0, 0, 1.0),
            CellMetrics {
                scale: 1.0,
                font_px: 8,
                half_w: 5,
                line_h: 8,
                baseline: 9,
                shift: 2
            }
        );
        // A non-positive / non-finite scale is treated as 1.0.
        assert_eq!(
            CellMetrics::from_primary(&font, 18, 19, 0.0),
            CellMetrics::from_primary(&font, 18, 19, 1.0)
        );
        assert_eq!(
            CellMetrics::from_primary(&font, 18, 19, f32::NAN),
            CellMetrics::from_primary(&font, 18, 19, 1.0)
        );
    }
}
```

Run: `cargo test -p erars-renderer text::` — expected failure: `error[E0412]: cannot find type `CellMetrics` in this scope` (plus `Shaper`, `Cluster` not found).

- [ ] **Step 4: Add `CellMetrics` and `from_primary`**

Insert directly after the line `pub use crate::flags::RasterFlags;` (before the legacy banner):

```rust
/// Integer physical-pixel cell geometry derived from the primary font.
///
/// All fields except `scale` are whole physical pixels (spec Invariants).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CellMetrics {
    /// winit scale factor the other fields were computed with.
    pub scale: f32,
    /// `round(font_size · scale).max(8)` — an integer pixel size, like Emuera's
    /// `new Font(.., FontSize, .., GraphicsUnit.Pixel)`.
    pub font_px: u32,
    /// `round(primary space advance · font_px / upem)`; for a 0.5 em primary
    /// this is `ceil(font_px / 2)`, so the primary is never rescaled.
    pub half_w: u32,
    /// `round(line_height · scale).max(font_px)` — the row pitch.
    pub line_h: u32,
    /// `round(ascender · font_px / upem)` from the row top, where `ascender`
    /// is `ttf_parser::Face::ascender()` (hhea, or OS/2 typo when
    /// USE_TYPO_METRICS is set). Never clamped to `line_h`.
    pub baseline: u32,
    /// `max(2, font_px / 6)` (integer division) — Emuera's
    /// `DrawingParam_ShapePositionShift`; text is drawn this far right of its
    /// logical x and `drawable_w = content_w − shift`.
    pub shift: u32,
}

impl CellMetrics {
    /// Derive the grid from the primary font at `font_size` / `line_height`
    /// logical px and the winit `scale` factor.
    pub fn from_primary(
        font: &cosmic_text::Font,
        font_size: u32,
        line_height: u32,
        scale: f32,
    ) -> Self {
        let scale = if scale.is_finite() && scale > 0.0 {
            scale
        } else {
            1.0
        };
        if font_size == 0 || line_height == 0 {
            log::warn!(
                "emuera.config: font size {font_size} px / line height {line_height} px; \
                 clamping to Emuera's minimum (font 8 px, line height >= font size)"
            );
        }
        let font_px = (font_size as f32 * scale).round().max(8.0) as u32;
        let line_h = (line_height as f32 * scale).round().max(font_px as f32) as u32;

        // `Font::rustybuzz()` derefs to the ttf-parser face; name the target
        // type so the ttf-parser `units_per_em`/`ascender` are used.
        let face: &ttf_parser::Face = font.rustybuzz();
        let upem = face.units_per_em() as f32;
        let half_w = match face
            .glyph_index(' ')
            .and_then(|g| face.glyph_hor_advance(g))
        {
            Some(adv) if adv > 0 => (adv as f32 * font_px as f32 / upem).round().max(1.0) as u32,
            _ => {
                log::warn!(
                    "primary font {:?} has no space glyph; assuming a 0.5 em cell",
                    font.id()
                );
                (font_px + 1) / 2
            }
        };
        let baseline = (face.ascender() as f32 * font_px as f32 / upem)
            .round()
            .max(0.0) as u32;
        let shift = 2u32.max(font_px / 6);

        Self {
            scale,
            font_px,
            half_w,
            line_h,
            baseline,
            shift,
        }
    }
}
```

Run: `cargo test -p erars-renderer text::metrics` — expected: `test text::tests::metrics_from_bundled_font ... ok`, `test text::tests::metrics_scale_rounds_and_clamps ... ok`, `test result: ok. 2 passed`.

- [ ] **Step 5: Write the shaping tests (failing)**

Append inside `mod tests` (after `metrics_scale_rounds_and_clamps`):

```rust
    #[test]
    fn ascii_is_one_cell_each() {
        let mut s = shaper(11);
        let primary = s.chain().primary();
        let cl = s.shape("abc", &style());
        assert_eq!(cells(&cl), [1, 1, 1]);
        assert_eq!(texts(&cl), ["a", "b", "c"]);
        for c in cl.iter() {
            assert_eq!(c.glyphs.len(), 1, "{:?}", c);
            let g = c.glyphs[0];
            assert_eq!(g.font, primary);
            assert_ne!(g.glyph, 0);
            // a = 600*18/1000 = 10.8 <= w = 11: size stays 18, dx = floor(0.2/2) = 0.
            assert_eq!(g.size_px, 18.0);
            assert_eq!(g.dx, 0);
            // y_offset 0 -> the glyph origin sits on the shared baseline.
            assert_eq!(g.dy, 15);
            assert!(g.flags.is_empty());
        }
    }

    #[test]
    fn cjk_is_two_cells_even_without_a_glyph() {
        // The bundled font has no CJK: resolve() falls back to the primary and
        // rustybuzz maps the char to .notdef (glyph 0); the box is still 2 cells
        // because the width comes from the classifier, not the font.
        let mut s = shaper(11);
        let cl = s.shape("あ", &style());
        assert_eq!(cells(&cl), [2]);
        assert_eq!(texts(&cl), ["あ"]);
        assert_eq!(cl[0].glyphs.len(), 1);
        assert_eq!(cl[0].glyphs[0].glyph, 0);
    }

    #[test]
    fn mixed_script_cells() {
        let mut s = shaper(11);
        let cl = s.shape("a한b─x", &style());
        assert_eq!(cells(&cl), [1, 2, 1, 2, 1]);
        assert_eq!(texts(&cl), ["a", "한", "b", "─", "x"]);
    }

    #[test]
    fn tab_expands_to_eight_cell_stops() {
        let mut s = shaper(11);
        let cl = s.shape("a\tb", &style());
        assert_eq!(cells(&cl).iter().map(|&c| c as usize).sum::<usize>(), 9);
        assert_eq!(texts(&cl), ["a", " ", " ", " ", " ", " ", " ", " ", "b"]);

        let cl = s.shape("\t", &style());
        assert_eq!(cl.len(), 8);
        assert!(cl.iter().all(|c| c.cells == 1 && c.text == " "));

        // A tab exactly at a stop advances a full 8 cells.
        let cl = s.shape("abcdefgh\ti", &style());
        assert_eq!(cells(&cl).iter().map(|&c| c as usize).sum::<usize>(), 17);

        // Full-width text counts 2 cells per char towards the stop.
        let cl = s.shape("あ\tb", &style());
        assert_eq!(cells(&cl), [2, 1, 1, 1, 1, 1, 1, 1]);
        assert_eq!(texts(&cl).last(), Some(&"b"));
    }

    #[test]
    fn combining_mark_joins_its_base() {
        let mut s = shaper(11);
        // rustybuzz composes e + U+0301 into one glyph; either way one cluster.
        let cl = s.shape("e\u{0301}", &style());
        assert_eq!(cells(&cl), [1]);
        assert_eq!(texts(&cl), ["e\u{0301}"]);
        assert!(!cl[0].glyphs.is_empty());

        // a + acute + acute: the second acute stays a separate glyph in the
        // same cluster, positioned by GPOS (x_offset -300, y_offset 230 in
        // font units): dx = 0 + round((600 - 300) * 18/1000) = 5,
        // dy = 15 - round(230 * 18/1000) = 11.
        let cl = s.shape("a\u{0301}\u{0301}", &style());
        assert_eq!(cells(&cl), [1]);
        assert_eq!(cl[0].glyphs.len(), 2);
        assert_eq!(cl[0].glyphs[0].dx, 0);
        assert_eq!(cl[0].glyphs[1].dx, 5);
        assert_eq!(cl[0].glyphs[1].dy, 11);
    }

    #[test]
    fn zero_cell_cluster_merges_into_previous() {
        let mut s = shaper(11);
        // U+00AD is 0 cells (classifier override) and its own rustybuzz cluster
        // (default-ignorable -> hidden zero-advance glyph): merged into "a".
        let cl = s.shape("a\u{00AD}b", &style());
        assert_eq!(cells(&cl), [1, 1]);
        assert_eq!(texts(&cl), ["a\u{00AD}", "b"]);
        assert_eq!(cl[0].glyphs.len(), 2);

        // U+1160 (Hangul V filler) is 0 cells and not a mark, so rustybuzz keeps
        // it as its own cluster; the bundled font lacks it -> .notdef, merged and
        // centred in the previous 11 px box (a = 10.8 <= 11 -> dx 0).
        let cl = s.shape("a\u{1160}", &style());
        assert_eq!(cells(&cl), [1]);
        assert_eq!(texts(&cl), ["a\u{1160}"]);
        assert_eq!(cl[0].glyphs.len(), 2);
        assert_eq!(cl[0].glyphs[1].glyph, 0);
        assert_eq!(cl[0].glyphs[1].dx, 0);
    }

    #[test]
    fn zwj_joins_the_previous_cluster() {
        // U+200D is a grapheme extender: rustybuzz keeps it in `a`'s cluster
        // (hidden zero-advance glyph). Cluster text comes from byte offsets, so
        // "a\u{200d}" is one cluster and `b` (offset 4) the next.
        let mut s = shaper(11);
        let cl = s.shape("a\u{200D}b", &style());
        assert_eq!(cells(&cl), [1, 1]);
        assert_eq!(texts(&cl), ["a\u{200D}", "b"]);
        assert_eq!(cl[0].glyphs.len(), 2);
    }

    #[test]
    fn leading_zero_cell_cluster_stands_alone() {
        let mut s = shaper(11);
        let cl = s.shape("\u{0301}a", &style());
        assert_eq!(cells(&cl), [0, 1]);
        assert_eq!(texts(&cl), ["\u{0301}", "a"]);
        assert_eq!(cl[0].glyphs.len(), 1);
        assert_eq!(cl[0].glyphs[0].dx, 0);
    }

    #[test]
    fn leading_zero_cell_cluster_with_an_advance_keeps_font_size() {
        // U+1160 is 0 cells but the bundled font lacks it, so rustybuzz emits
        // `.notdef` (advance 600). Its box is 0 px wide: the `a > w` rule would
        // shrink it to size 0; a 0-px box keeps `font_px` and `dx = 0` instead.
        let mut s = shaper(11);
        let cl = s.shape("\u{1160}a", &style());
        assert_eq!(cells(&cl), [0, 1]);
        assert_eq!(cl[0].glyphs[0].glyph, 0);
        assert_eq!((cl[0].glyphs[0].dx, cl[0].glyphs[0].size_px), (0, 18.0));
        assert_eq!((cl[1].glyphs[0].dx, cl[1].glyphs[0].size_px), (0, 18.0));
    }

    #[test]
    fn placement_a_le_w_centres() {
        // Spec Testing §4: half_w 11 -> a = 10.8 <= 11 -> dx 0, size 18.
        let mut s = shaper(11);
        let g = s.shape("a", &style())[0].glyphs[0];
        assert_eq!((g.dx, g.size_px), (0, 18.0));

        // half_w 13 -> w = 13, dx = floor(2.2 / 2) = 1, size unchanged.
        let mut s = shaper(13);
        let g = s.shape("a", &style())[0].glyphs[0];
        assert_eq!((g.dx, g.size_px), (1, 18.0));
    }

    #[test]
    fn placement_a_gt_w_rescales() {
        // Spec Testing §4: half_w 9 -> a = 10.8 > 9 -> size_px = 9*1000/600 = 15
        // exactly, dx 0 (no second shaping call: the same advances are scaled).
        let mut s = shaper(9);
        let g = s.shape("a", &style())[0].glyphs[0];
        assert_eq!((g.dx, g.size_px), (0, 15.0));

        // A 2-cell box (w = 18) holding .notdef (a = 10.8): centred, dx = 3.
        let cl = s.shape("あ", &style());
        let g = cl[0].glyphs[0];
        assert_eq!((g.dx, g.size_px), (3, 18.0));
    }

    #[test]
    fn bold_without_a_bold_face_sets_synth_flag() {
        let mut s = shaper(11);
        let g = s.shape("a", &styled(FontStyle::BOLD))[0].glyphs[0];
        assert_eq!(g.flags, RasterFlags::BOLD_SYNTH);
        let g = s.shape("a", &styled(FontStyle::ITALIC))[0].glyphs[0];
        assert_eq!(g.flags, RasterFlags::ITALIC_SYNTH);
        let g = s.shape("a", &styled(FontStyle::BOLD | FontStyle::ITALIC))[0].glyphs[0];
        assert_eq!(g.flags, RasterFlags::BOLD_SYNTH | RasterFlags::ITALIC_SYNTH);
        let g = s.shape("a", &style())[0].glyphs[0];
        assert!(g.flags.is_empty());
    }

    #[test]
    fn empty_text_has_no_clusters() {
        let mut s = shaper(11);
        assert!(s.shape("", &style()).is_empty());
    }
```

Run: `cargo test -p erars-renderer text::` — expected failure: `error[E0433]: failed to resolve: use of undeclared type `Shaper`` (and `Cluster`).

- [ ] **Step 6: Add `ShapedGlyph`, `Cluster`, `Shaper` scaffolding (cache, sweep, tabs, features)**

Insert directly after the `impl CellMetrics { … }` block:

```rust
/// One positioned glyph. `dx`/`dy` are integer px relative to the cluster's
/// box origin `(x, row_y)`; `size_px` is the raster size (== `font_px` unless
/// the cluster had to be shrunk to fit its box).
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ShapedGlyph {
    pub font: fontdb::ID,
    pub glyph: u16,
    pub dx: i32,
    pub dy: i32,
    pub size_px: f32,
    pub flags: RasterFlags,
}

/// A grapheme cluster boxed into `cells × half_w` px.
#[derive(Clone, Debug, PartialEq)]
pub struct Cluster {
    /// 0, 1 or 2 (a 0-cell cluster only occurs at the start of a string).
    pub cells: u8,
    /// The cluster's source characters.
    pub text: SmolStr,
    pub glyphs: Vec<ShapedGlyph>,
}

type StyleCache = HashMap<String, (u32, Arc<[Cluster]>)>;

/// Owns the font chain, the width table, the current cell metrics and the
/// per-layout shaping cache.
pub struct Shaper {
    chain: FontChain,
    widths: WidthTable,
    m: CellMetrics,
    layout_gen: u32,
    /// Keyed by `StyleKey` then text so `shape` can look up by `&str`.
    /// The `u32` is the `layout_gen` the entry was last used in.
    cache: HashMap<StyleKey, StyleCache>,
}

impl Shaper {
    pub fn new(chain: FontChain, widths: WidthTable, m: CellMetrics) -> Self {
        Self {
            chain,
            widths,
            m,
            layout_gen: 0,
            cache: HashMap::new(),
        }
    }

    pub fn metrics(&self) -> &CellMetrics {
        &self.m
    }

    /// The font chain, e.g. for `GlyphRaster::get(.., &chain.font(id), ..)`.
    pub fn chain(&mut self) -> &mut FontChain {
        &mut self.chain
    }

    /// The width table shared with the console (`cells(s)` for DRAWLINE etc.).
    pub fn widths(&self) -> &WidthTable {
        &self.widths
    }

    /// Replace the metrics (scale factor / font size / line height change).
    /// `dx` and `size_px` depend on `half_w` / `font_px`, so the cache is cleared.
    pub fn set_metrics(&mut self, m: CellMetrics) {
        self.m = m;
        self.cache.clear();
    }

    /// Called at the end of `layout()`: drop every entry the layout that just
    /// finished did not use, then start a new generation. The cache therefore
    /// holds exactly the strings of the lines last laid out.
    pub fn sweep(&mut self) {
        let gen = self.layout_gen;
        self.cache.retain(|_, inner| {
            inner.retain(|_, (used, _)| *used == gen);
            !inner.is_empty()
        });
        self.layout_gen = self.layout_gen.wrapping_add(1);
    }

    /// Shape `text` (one console part, no `\n`) in `style`. Cached per
    /// `(StyleKey, text)` for the current metrics; colour, underline and strike
    /// are not shaping inputs and never miss the cache.
    pub fn shape(&mut self, text: &str, style: &TextStyle) -> Arc<[Cluster]> {
        debug_assert!(
            !text.contains('\n'),
            "layout must split parts at '\\n' before shaping: {text:?}"
        );
        let key = StyleKey::from(style);
        let gen = self.layout_gen;
        if let Some(entry) = self.cache.get_mut(&key).and_then(|inner| inner.get_mut(text)) {
            entry.0 = gen;
            return Arc::clone(&entry.1);
        }
        let clusters: Arc<[Cluster]> = Arc::from(self.shape_uncached(text, &key));
        self.cache
            .entry(key)
            .or_default()
            .insert(text.to_owned(), (gen, Arc::clone(&clusters)));
        clusters
    }

    #[cfg(test)]
    fn cache_len(&self) -> usize {
        self.cache.values().map(|inner| inner.len()).sum()
    }
}

/// OpenType features turned off so one character stays one glyph and advances
/// are unkerned, as GDI's TextRenderer draws (value 0 overrides rustybuzz's
/// default global `liga`/`clig`/`calt`/`kern`).
fn features() -> [Feature; 4] {
    [
        Feature::new(Tag::from_bytes(b"liga"), 0, ..),
        Feature::new(Tag::from_bytes(b"clig"), 0, ..),
        Feature::new(Tag::from_bytes(b"calt"), 0, ..),
        Feature::new(Tag::from_bytes(b"kern"), 0, ..),
    ]
}

/// `\t` → spaces up to the next multiple of 8 cells, counted from the start
/// of `text` (uEmuera / GRAPHICS-mode behaviour; MS Gothic has no U+0009 glyph).
fn expand_tabs<'a>(text: &'a str, widths: &WidthTable) -> Cow<'a, str> {
    if !text.contains('\t') {
        return Cow::Borrowed(text);
    }
    let mut out = String::with_capacity(text.len() + 8);
    let mut col = 0usize;
    for c in text.chars() {
        if c == '\t' {
            let n = 8 - col % 8;
            out.extend(std::iter::repeat(' ').take(n));
            col += n;
        } else {
            out.push(c);
            col += widths.char_cells(c) as usize;
        }
    }
    Cow::Owned(out)
}

/// Sum of `char_cells` over `s`, saturating at `u8::MAX`.
fn cells_of(s: &str, widths: &WidthTable) -> u8 {
    s.chars()
        .map(|c| widths.char_cells(c) as u32)
        .sum::<u32>()
        .min(u8::MAX as u32) as u8
}
```

Run: `cargo check -p erars-renderer` — expected failure: `error[E0599]: no method named `shape_uncached` found for mutable reference `&mut Shaper``.

- [ ] **Step 7: Add the shaping core (`shape_uncached`) and the placement helper**

Insert directly after `fn cells_of(…) { … }`:

```rust
/// One shaped glyph in font units, before placement.
struct RawGlyph {
    gid: u16,
    x_adv: i32,
    x_off: i32,
    y_off: i32,
}

/// A maximal run of characters that resolved to the same `(font, flags)`.
struct Span {
    start: usize,
    end: usize,
    font: fontdb::ID,
    flags: RasterFlags,
}

/// Place one shaped run (font units, `upem`) into a box `w` px wide.
///
/// `a = Σ x_advance · font_px / upem` is the natural (linear, unhinted) width.
/// `a ≤ w` (or `w == 0`): keep `font_px`, centre with `dx0 = max(0, floor((w − a) / 2))`.
/// `a > w`: shrink to `size_px = w · upem / Σ x_advance` (== `font_px · w / a`,
/// computed so an integral result is exact) and start at `dx0 = 0`; rustybuzz
/// output is size-independent, so the same advances are just rescaled.
/// Every glyph origin: `dx = dx0 + round((pen + x_offset) · size_px / upem)`,
/// `dy = baseline − round(y_offset · size_px / upem)`.
fn place(
    out: &mut Vec<ShapedGlyph>,
    font: fontdb::ID,
    flags: RasterFlags,
    upem: u32,
    m: &CellMetrics,
    run: &[RawGlyph],
    w: u32,
) {
    let sum_adv: i32 = run.iter().map(|g| g.x_adv).sum::<i32>().max(0);
    let a = sum_adv as f32 * m.font_px as f32 / upem as f32;
    // `w == 0` (a leading 0-cell cluster whose glyph still has an advance,
    // e.g. `.notdef` for U+1160) would give `size_px = 0`; draw it at
    // `font_px` from the box origin instead.
    let (size_px, dx0) = if w == 0 || a <= w as f32 {
        (m.font_px as f32, ((w as f32 - a) / 2.0).floor().max(0.0) as i32)
    } else {
        (w as f32 * upem as f32 / sum_adv as f32, 0)
    };
    let k = size_px / upem as f32;
    let mut pen = 0i32;
    for g in run {
        let x = ((pen + g.x_off) as f32 * k).round() as i32;
        let y = (g.y_off as f32 * k).round() as i32;
        out.push(ShapedGlyph {
            font,
            glyph: g.gid,
            dx: dx0 + x,
            dy: m.baseline as i32 - y,
            size_px,
            flags,
        });
        pen += g.x_adv;
    }
}

impl Shaper {
    fn shape_uncached(&mut self, text: &str, key: &StyleKey) -> Vec<Cluster> {
        let m = self.m;
        let expanded = expand_tabs(text, &self.widths);
        if expanded.is_empty() {
            return Vec::new();
        }

        // 1. Split into maximal spans of equal (font, flags).
        let mut spans: Vec<Span> = Vec::new();
        for (i, c) in expanded.char_indices() {
            let (font, flags) = self.chain.resolve(c, key);
            let end = i + c.len_utf8();
            match spans.last_mut() {
                Some(last) if last.font == font && last.flags == flags => last.end = end,
                _ => spans.push(Span {
                    start: i,
                    end,
                    font,
                    flags,
                }),
            }
        }

        // 2. Shape each span once (font units) and cut it into clusters.
        //    Working form: (text, cells, glyphs) so 0-cell clusters can be
        //    merged into their predecessor before the SmolStr is built.
        let feats = features();
        let mut buf = UnicodeBuffer::new();
        let mut work: Vec<(String, u8, Vec<ShapedGlyph>)> = Vec::new();
        for span in &spans {
            let font = self.chain.font(span.font); // owned Arc: borrow-friendly
            let face = font.rustybuzz();
            let upem = face.units_per_em() as u32;
            let s = &expanded[span.start..span.end];

            buf.push_str(s);
            buf.set_direction(Direction::LeftToRight);
            buf.set_cluster_level(BufferClusterLevel::MonotoneGraphemes);
            let gb = rustybuzz::shape(face, &feats, buf);
            {
                let infos = gb.glyph_infos();
                let poss = gb.glyph_positions();
                let mut i = 0;
                while i < infos.len() {
                    // Cluster values are byte offsets into `s` (push_str), and
                    // with LTR + MonotoneGraphemes they are non-decreasing.
                    let cstart = (infos[i].cluster as usize).min(s.len());
                    let mut j = i + 1;
                    while j < infos.len() && infos[j].cluster as usize == cstart {
                        j += 1;
                    }
                    let cend = if j < infos.len() {
                        (infos[j].cluster as usize).clamp(cstart, s.len())
                    } else {
                        s.len()
                    };
                    let ctext = &s[cstart..cend];
                    let run: Vec<RawGlyph> = (i..j)
                        .map(|k| RawGlyph {
                            gid: infos[k].glyph_id as u16,
                            x_adv: poss[k].x_advance,
                            x_off: poss[k].x_offset,
                            y_off: poss[k].y_offset,
                        })
                        .collect();
                    let cells = cells_of(ctext, &self.widths);

                    match work.last_mut() {
                        // 0-cell cluster (combining-only / ignorable): draw it
                        // inside the previous cluster's box.
                        Some((ptext, pcells, pglyphs)) if cells == 0 => {
                            let w = *pcells as u32 * m.half_w;
                            place(pglyphs, span.font, span.flags, upem, &m, &run, w);
                            ptext.push_str(ctext);
                        }
                        _ => {
                            let w = cells as u32 * m.half_w;
                            let mut glyphs = Vec::with_capacity(run.len());
                            place(&mut glyphs, span.font, span.flags, upem, &m, &run, w);
                            work.push((ctext.to_owned(), cells, glyphs));
                        }
                    }
                    i = j;
                }
            }
            buf = gb.clear(); // reuse the allocation for the next span
        }

        work.into_iter()
            .map(|(text, cells, glyphs)| Cluster {
                cells,
                text: SmolStr::from(text),
                glyphs,
            })
            .collect()
    }
}
```

Run: `cargo test -p erars-renderer text::` — expected: all 15 tests so far `ok` (`test result: ok. 15 passed; 0 failed`). If `bold_without_a_bold_face_sets_synth_flag` fails, the defect is in T5's `resolve` (it must return `BOLD_SYNTH`/`ITALIC_SYNTH` when no real bold/italic face of the family exists) — report it, do not work around it here.

- [ ] **Step 8: Add the cache / sweep / metrics-change tests**

Append inside `mod tests`:

```rust
    #[test]
    fn cache_hit_returns_the_same_arc() {
        let mut s = shaper(11);
        let a1 = s.shape("abc", &style());
        let a2 = s.shape("abc", &style());
        assert!(Arc::ptr_eq(&a1, &a2));
        assert_eq!(s.cache_len(), 1);
        // A different string is a different entry; a different family too.
        s.shape("abd", &style());
        let other_family = TextStyle {
            font_family: "No Such Family".into(),
            ..style()
        };
        s.shape("abc", &other_family);
        assert_eq!(s.cache_len(), 3);
    }

    #[test]
    fn sweep_drops_entries_unused_by_the_last_layout() {
        let mut s = shaper(11);
        let a1 = s.shape("a", &style());
        s.shape("b", &style());
        assert_eq!(s.cache_len(), 2);

        s.sweep(); // both were used in generation 0 -> both survive
        assert_eq!(s.cache_len(), 2);

        s.shape("a", &style()); // generation 1 uses only "a"
        s.sweep();
        assert_eq!(s.cache_len(), 1);
        let a3 = s.shape("a", &style());
        assert!(Arc::ptr_eq(&a1, &a3), "survivor must be the cached Arc");

        s.sweep(); // "a" used in gen 2 -> kept
        s.sweep(); // nothing used in gen 3 -> dropped
        assert_eq!(s.cache_len(), 0);
    }

    #[test]
    fn set_metrics_clears_the_cache() {
        let mut s = shaper(11);
        let before = s.shape("a", &style());
        assert_eq!(before[0].glyphs[0].size_px, 18.0);
        assert_eq!(s.cache_len(), 1);

        s.set_metrics(pinned(9));
        assert_eq!(s.cache_len(), 0);
        assert_eq!(s.metrics().half_w, 9);
        let after = s.shape("a", &style());
        assert!(!Arc::ptr_eq(&before, &after));
        assert_eq!(after[0].glyphs[0].size_px, 15.0);
    }

    #[test]
    fn colour_and_decoration_are_not_shaping_inputs() {
        let mut s = shaper(11);
        let white = s.shape("abc", &style());
        let red = s.shape(
            "abc",
            &TextStyle {
                color: Color([255, 0, 0]),
                ..style()
            },
        );
        let underlined = s.shape("abc", &styled(FontStyle::UNDERLINE | FontStyle::STRIKELINE));
        assert!(Arc::ptr_eq(&white, &red));
        assert!(Arc::ptr_eq(&white, &underlined));
        assert_eq!(s.cache_len(), 1);
        // Bold is a shaping input (different face / synth flag) -> own entry.
        let bold = s.shape("abc", &styled(FontStyle::BOLD));
        assert!(!Arc::ptr_eq(&white, &bold));
        assert_eq!(s.cache_len(), 2);
    }
```

Run: `cargo test -p erars-renderer text::` — expected: `test result: ok. 19 passed; 0 failed`.

- [ ] **Step 9: MS Gothic-gated tests (opt-in; `msgothic.ttc` sits untracked at the repo root and must never be committed)**

Append inside `mod tests` (after `colour_and_decoration_are_not_shaping_inputs`). They exercise the two paths the bundled font cannot: a 1:2 primary whose `a == w` at 18 px, and a string whose characters resolve to **two different fonts** (span splitting):

```rust
    /// `ERARS_FONT_DIR/msgothic.ttc` when present (opt-in, never in CI).
    /// Prints a SKIP line, or panics when `ERARS_REQUIRE_CJK_FONT=1`.
    fn msgothic(test: &str) -> Option<PathBuf> {
        let path = std::env::var_os("ERARS_FONT_DIR")
            .map(|d| PathBuf::from(d).join("msgothic.ttc"))
            .filter(|p| p.is_file());
        if path.is_none() {
            let msg = format!("SKIP {test}: ERARS_FONT_DIR does not contain msgothic.ttc");
            if std::env::var_os("ERARS_REQUIRE_CJK_FONT").is_some_and(|v| v == "1") {
                panic!("{msg} (ERARS_REQUIRE_CJK_FONT=1)");
            }
            eprintln!("{msg}");
        }
        path
    }

    #[test]
    fn msgothic_metrics_and_grid_are_exact() {
        let Some(ms) = msgothic("msgothic_metrics_and_grid_are_exact") else { return };
        let mut ch = FontChain::from_files(&[ms, bundled()], Language::Japanese);
        let primary = ch.primary();
        assert_eq!(ch.find_family("MS Gothic"), Some(primary));
        let font = ch.font(primary);
        // upem 256, space advance 128, hhea ascender 220 -> 9 px cells, baseline 15.
        let m18 = CellMetrics::from_primary(&font, 18, 19, 1.0);
        assert_eq!(m18, CellMetrics { scale: 1.0, font_px: 18, half_w: 9, line_h: 19, baseline: 15, shift: 3 });
        // Odd size: a = 8.5 -> half_w rounds up to 9, so the primary is never shrunk.
        let m17 = CellMetrics::from_primary(&font, 17, 19, 1.0);
        assert_eq!((m17.font_px, m17.half_w), (17, 9));

        let mut s = Shaper::new(ch, widths(), m18);
        let cl = s.shape("Aあ─═", &style());
        assert_eq!(cells(&cl), [1, 2, 2, 1]);
        for c in cl.iter() {
            let g = c.glyphs[0];
            assert_eq!(g.font, primary, "{:?}", c.text);
            assert_ne!(g.glyph, 0, "{:?}", c.text);
            // a == w for every MS Gothic glyph at 18 px: no centring, no rescale.
            assert_eq!((g.dx, g.dy, g.size_px), (0, 15, 18.0), "{:?}", c.text);
        }
    }

    #[test]
    fn spans_split_at_font_changes() {
        let Some(ms) = msgothic("spans_split_at_font_changes") else { return };
        let mut ch = FontChain::from_files(&[ms, bundled()], Language::Japanese);
        let primary = ch.primary();
        let noto = ch.find_family("Noto Sans Mono").expect("bundled font loaded");
        assert_ne!(noto, primary);
        let font = ch.font(primary);
        let m = CellMetrics::from_primary(&font, 18, 19, 1.0);
        let mut s = Shaper::new(ch, widths(), m);
        // U+0180 is in Noto Sans Mono but not in MS Gothic: `A` and `b` shape
        // in the primary, `ƀ` in its own span with the bundled font, shrunk to
        // the 9 px cell (a = 10.8 > 9 -> size 15).
        let cl = s.shape("A\u{0180}b", &style());
        assert_eq!(cells(&cl), [1, 1, 1]);
        assert_eq!(texts(&cl), ["A", "\u{0180}", "b"]);
        assert_eq!(cl[0].glyphs[0].font, primary);
        assert_eq!(cl[1].glyphs[0].font, noto);
        assert_ne!(cl[1].glyphs[0].glyph, 0);
        assert_eq!(cl[2].glyphs[0].font, primary);
        assert_eq!((cl[1].glyphs[0].dx, cl[1].glyphs[0].size_px), (0, 15.0));
        assert_eq!((cl[0].glyphs[0].dx, cl[0].glyphs[0].size_px), (0, 18.0));
        // The baseline is the primary's for every font.
        assert!(cl.iter().all(|c| c.glyphs[0].dy == 15));
    }
```

Run: `cargo test -p erars-renderer text:: -- --nocapture 2>&1 | grep -E 'SKIP|test result'` — expected (no `ERARS_FONT_DIR`): two lines `SKIP msgothic_metrics_and_grid_are_exact: ERARS_FONT_DIR does not contain msgothic.ttc` / `SKIP spans_split_at_font_changes: …` and `test result: ok. 21 passed; 0 failed`. Then the strict gate (no `ERARS_FONT_DIR`; the two filters go after `--` because `cargo test` takes only one TESTNAME positional): `ERARS_REQUIRE_CJK_FONT=1 cargo test -p erars-renderer -- text::tests::msgothic_metrics_and_grid_are_exact text::tests::spans_split_at_font_changes 2>&1 | grep -E 'panicked|test result'` → both tests fail with `panicked … (ERARS_REQUIRE_CJK_FONT=1)`, `test result: FAILED. 0 passed; 2 failed`.

Then from the repo root: `ERARS_FONT_DIR=$PWD cargo test -p erars-renderer -- text::tests::msgothic text::tests::spans --nocapture` — expected: no `SKIP` line and

```
test text::tests::msgothic_metrics_and_grid_are_exact ... ok
test text::tests::spans_split_at_font_changes ... ok
test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 19 filtered out
```

(If `spans_split_at_font_changes` fails on `cl[1].glyphs[0].font == noto` the defect is T5's `resolve` chain order, not the shaper — report it. The full list of the 21 tests, in the order `cargo test` prints them:)

```
test text::tests::ascii_is_one_cell_each ... ok
test text::tests::bold_without_a_bold_face_sets_synth_flag ... ok
test text::tests::cache_hit_returns_the_same_arc ... ok
test text::tests::cjk_is_two_cells_even_without_a_glyph ... ok
test text::tests::colour_and_decoration_are_not_shaping_inputs ... ok
test text::tests::combining_mark_joins_its_base ... ok
test text::tests::empty_text_has_no_clusters ... ok
test text::tests::leading_zero_cell_cluster_stands_alone ... ok
test text::tests::leading_zero_cell_cluster_with_an_advance_keeps_font_size ... ok
test text::tests::metrics_from_bundled_font ... ok
test text::tests::metrics_scale_rounds_and_clamps ... ok
test text::tests::mixed_script_cells ... ok
test text::tests::msgothic_metrics_and_grid_are_exact ... ok
test text::tests::placement_a_gt_w_rescales ... ok
test text::tests::placement_a_le_w_centres ... ok
test text::tests::set_metrics_clears_the_cache ... ok
test text::tests::spans_split_at_font_changes ... ok
test text::tests::sweep_drops_entries_unused_by_the_last_layout ... ok
test text::tests::tab_expands_to_eight_cell_stops ... ok
test text::tests::zero_cell_cluster_merges_into_previous ... ok
test text::tests::zwj_joins_the_previous_cluster ... ok
```

- [ ] **Step 10: Silence dead-code noise in `main.rs`, whole-crate check, commit**

`Shaper`/`CellMetrics`/`Cluster` have no caller in the binary until T7/T10. In `crates/erars-renderer/src/main.rs` replace the line `mod text;` (line 16 after T5's Step 3; the last line of the module list) with:

```rust
#[allow(dead_code)] // Shaper/CellMetrics are consumed by layout (T7) and the app (T10)
mod text;
```

Run: `cargo test -p erars-renderer 2>&1 | grep -E 'warning: unused|dead_code|test result'` — expected: no `unused import`/`dead_code` warnings from `text.rs`; every `test result:` line `ok` (the legacy `grid::`/`atlas::`/`headless::` tests still pass or print their existing `SKIP` lines). `cargo clippy -p erars-renderer -- -D warnings` may be skipped if the crate did not pass it before this task.

Commit:

```sh
git add crates/erars-renderer/Cargo.toml Cargo.lock crates/erars-renderer/src/text.rs crates/erars-renderer/src/main.rs && git commit -m "feat(renderer): rustybuzz cell shaper with integer CellMetrics and per-layout cluster cache

Shapes each (font, flags) span once in font units with liga/clig/calt/kern off,
boxes every cluster into cells*half_w px from the WidthTable, centres or
shrinks it (a<=w / a>w) on integer pixels and shares the primary baseline.
Tabs expand to 8-cell stops; 0-cell clusters merge into their predecessor.
Results are cached per (StyleKey, text) and swept per layout generation.
MS Gothic-gated tests (ERARS_FONT_DIR) cover the 1:2 primary and span
splitting across two fonts.

Claude-Session: https://claude.ai/code/session_01XEtVTsN59k1K3cegBL8mfx"
```

---

### Task 7: Layout — `crates/erars-renderer/src/layout.rs` (rows, rects, button fragments, `layout_snapshot`, k9 goldens)

Implements spec Component 5 (`layout`), the `layout_snapshot` format of Component 7 and the layout goldens of Testing §4. Positions are in Emuera's `PointX` space (the drawer adds `shift + x0`). Layout never looks at the mouse or the active input generation; hit testing lives in `app.rs` (T10).

Verification done while planning (2026-09-02): the code of Steps 3–10 and every test of Steps 3–16 were compiled **verbatim** with `rustc --edition 2021 --test` against the workspace's built rlibs in `target/debug/deps` (real `erars_ui`, `erars_ast`, `smol_str 0.2.1`, `cosmic_text 0.12.1`, `log`), with only T6's `Shaper` and T5's `FontChain` mocked and `k9::snapshot!` replaced by a macro reproducing k9 0.11.6's `value_to_string` for plain strings — `scratchpad/probe-plan-07/real/probe.rs`: `test result: ok. 23 passed; 0 failed`, zero warnings. `LineRules::from_primary` ran against a real `cosmic_text::Font` of the bundled Noto Sans Mono (`font.rustybuzz()` → `ttf_parser::Face::{underline_metrics, strikeout_metrics}`) and produced the 17 / 9 rows asserted below; MS Gothic's `post (−17, 19)` / `OS/2 (66, 13)` at upem 256 were read from `msgothic.ttc` with ttf-parser 0.21.1 (`scratchpad/probe-plan-07/ms/ms.rs`) and give the spec's rows 16 / 10.

**Files:**
- Create: `crates/erars-renderer/src/layout.rs` (new module; `grid.rs` stays until T10 deletes it)
- Modify: `crates/erars-renderer/src/main.rs` — the `mod` declarations. In the pre-T5 file they are lines 3–13 (`mod app; mod atlas; mod draw; mod font; mod gpu; mod grid; #[allow(dead_code)] mod headless; #[cfg(test)] mod test_support; mod text;`); T5 Step 3 rewrote them (adding `mod flags;` and `#[allow(dead_code)] mod font;`, now lines 3–17). Locate `mod grid;` by content and insert after it.
- Modify: `crates/erars-renderer/Cargo.toml` — the file is 32 lines, `[dependencies]` at lines 7–32 (T5/T6 edited lines 22–32: cosmic-text default features, `smol_str`, `bitflags`, `rustybuzz`). Append a `[dev-dependencies]` table at the end of the file.
- Delete: nothing (T10 deletes `grid.rs` / `atlas.rs`).
- Test: in-module `#[cfg(test)] mod tests` inside `layout.rs` — `cargo test -p erars-renderer layout::` (23 tests at the end).
- Paste rule: all code blocks in this task are indented by two spaces because they sit inside a markdown list item — strip exactly two leading spaces from every line when pasting, so that the `row …` lines of the k9 goldens start at column 0 and cluster/rect lines keep their own two-space indent (`  0:1 "a"`). k9 0.11.6 compares the value byte-exact (wrapped in one leading and one trailing `\n`), so a golden pasted with the list indent fails every snapshot.

**Interfaces:**
- Consumes (T6, `crate::text`, verbatim from the T6 section): `#[derive(Clone, Copy, Debug, PartialEq)] pub struct CellMetrics { pub scale: f32, pub font_px: u32, pub half_w: u32, pub line_h: u32, pub baseline: u32, pub shift: u32 }`; `#[derive(Clone, Copy, Debug, PartialEq)] pub struct ShapedGlyph { pub font: fontdb::ID, pub glyph: u16, pub dx: i32, pub dy: i32, pub size_px: f32, pub flags: RasterFlags }`; `#[derive(Clone, Debug, PartialEq)] pub struct Cluster { pub cells: u8, pub text: SmolStr, pub glyphs: Vec<ShapedGlyph> }`; `Shaper::new(chain: FontChain, widths: WidthTable, m: CellMetrics) -> Self`; `Shaper::metrics(&self) -> &CellMetrics`; `Shaper::chain(&mut self) -> &mut FontChain`; `Shaper::shape(&mut self, text: &str, style: &TextStyle) -> Arc<[Cluster]>` (expands `\t` to 1-cell `" "` clusters up to the next multiple of 8 cells counted from the start of `text`; `debug_assert!`s that `text` has no `\n`; a combining-mark-only string yields one `cells: 0` cluster); `Shaper::sweep(&mut self)`.
- Consumes (T5, `crate::font`): `FontChain::from_files(files: &[PathBuf], lang: Language) -> Self`; `FontChain::primary(&self) -> fontdb::ID`; `FontChain::font(&mut self, id: fontdb::ID) -> Arc<cosmic_text::Font>`; `crate::font::bundled_font_path() -> PathBuf`.
- Consumes (T1/T2): `erars_ui::width::WidthTable::new(encoding: &'static encoding_rs::Encoding) -> WidthTable`; `erars_compiler::Language::encoding(&self) -> &'static encoding_rs::Encoding` (JP table: ASCII 1, `あ` 2, `─` 2, U+0301 0).
- Consumes (existing): `erars_ui::{ConsoleLine { align, button_start, parts }, ConsoleLinePart::{Text(String, TextStyle), Line(String, TextStyle), Button(Vec<(String, TextStyle)>, u32, Value)}, FontStyle::{NORMAL, BOLD, ITALIC, STRIKELINE, UNDERLINE}, TextStyle { color: Color, font_family: SmolStr, font_style: FontStyle }, Color(pub [u8; 3])}`; `erars_ast::{Alignment::{Left, Center, Right}, Value::{Int(i64), String(String)}}` (`Value: Clone + Debug + Eq`); `cosmic_text::Font::rustybuzz(&self) -> &rustybuzz::Face<'_>` (inherent `units_per_em() -> i32`; derefs to `ttf_parser::Face` for `underline_metrics() -> Option<LineMetrics { position: i16, thickness: i16 }>` and `strikeout_metrics()`); `log::warn!`.
- Produces (used by T8 `draw.rs`, T9 `headless.rs`, T10 `app.rs`, T11 `tests/tui.rs`), all in `crate::layout`:
  - `#[derive(Clone, Copy, Debug, PartialEq)] pub struct Geometry { pub content_w: u32, pub drawable_w: u32, pub m: CellMetrics }` + `Geometry::new(content_w: u32, m: CellMetrics) -> Geometry` (`drawable_w = content_w.saturating_sub(m.shift)`; struct literals are fine too)
  - `#[derive(Clone, Debug, Default)] pub struct Layout { pub rows: Vec<Row>, pub buttons: Vec<ButtonRegion> }`
  - `#[derive(Clone, Debug)] pub struct Row { pub line: usize, pub logical_start: bool, pub x0: i32, pub width: u32, pub clusters: Vec<PlacedCluster>, pub rects: Vec<Rect> }`
  - `#[derive(Clone, Debug)] pub struct PlacedCluster { pub x: i32, pub cells: u8, pub text: SmolStr, pub color: [u8; 3], pub style: FontStyle, pub button: Option<usize>, pub glyphs: Arc<[ShapedGlyph]> }` (`x` row-relative before `x0`; glyph `dx`/`dy` relative to `(x, row_y)`)
  - `#[derive(Clone, Copy, Debug, PartialEq, Eq)] pub enum RectKind { Underline, Strike }`
  - `#[derive(Clone, Debug, PartialEq, Eq)] pub struct Rect { pub kind: RectKind, pub x: i32, pub dy: i32, pub h: u32, pub w: u32, pub color: [u8; 3], pub button: Option<usize> }` (`x` row-relative before `x0`; `dy` relative to the row top)
  - `#[derive(Clone, Debug, PartialEq, Eq)] pub struct ButtonRegion { pub row: usize, pub x: i32, pub w: u32, pub input_gen: u32, pub value: Value }` (`x` row-relative before `x0`; draw/hit x = `shift + x0 + x`)
  - `pub fn layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper) -> Layout` (ends with `shaper.sweep()`)
  - `pub fn layout_snapshot(layout: &Layout, default_fg: [u8; 3]) -> String` (lines joined by `\n`, no trailing newline)

Decisions taken where the spec leaves a choice (also in open_questions): a residual `\n` always finishes the current row, even an empty one, so `"a\n"` yields an empty continuation row; `\n` inside a DRAWLINE string is removed before repeating; DRAWLINE trims trailing *clusters* (cell-identical to Emuera's per-character trim); the layout forces `FontStyle::NORMAL` on Line parts itself (belt and braces with T3); a button fragment is emitted only if at least one cluster landed on that row; `PlacedCluster.glyphs` is `Arc::from(&cluster.glyphs[..])` (one small copy per cluster per relayout — the spec's types leave no zero-copy option); the wrap goldens use `content_w = 93` (drawable 90 = 10 half cells) to keep the inline snapshots short, and the 760-px Emuera defaults are asserted with plain assertions (84 dashes, `abc` + 81/3); hit testing is **not** provided here (T10's `app::hit_button` owns it; `ButtonRegion` carries what it needs).

- [ ] **Step 1: Add the k9 dev-dependency in `crates/erars-renderer/Cargo.toml`**

  Check `grep -n 'smol_str\|k9' crates/erars-renderer/Cargo.toml`. `smol_str = "0.2"` must already be in `[dependencies]` (T5 Step 1); if it is missing, add the line `smol_str = "0.2"` after `etagere = "0.2"`. Then append to the end of the file (after T5 the file ends with the `[dependencies]` block — `cosmic-text = "0.12.1"` is its last line and the old `[dependencies.cosmic-text]` table is gone; add only the `k9` line if a `[dev-dependencies]` table already exists):

  ```toml

  [dev-dependencies]
  k9 = "0.11.5"
  ```

  (`k9` resolves to the already-locked 0.11.6; `Cargo.lock` gains only the new edge for `erars-renderer`. The workspace root's `k9 = "0.11.1"` is a root-package dev-dependency, not a `[workspace.dependencies]` entry, so `k9.workspace = true` is not available.) Verify: `grep -n 'k9\|smol_str' crates/erars-renderer/Cargo.toml` prints both lines.

- [ ] **Step 2: Declare the module in `crates/erars-renderer/src/main.rs`**

  In the `mod` list at the top of `main.rs`, directly after the line `mod grid;`, insert:

  ```rust
  #[allow(dead_code)] // wired into app.rs / headless.rs by T10
  mod layout;
  ```

  Run: `cargo check -p erars-renderer` — expected failure: `error[E0583]: file not found for module `layout``.

- [ ] **Step 3: Create `layout.rs` with the data model and the first (failing) unit test**

  Create `crates/erars-renderer/src/layout.rs`:

  ```rust
  //! Row layout: `ConsoleLine` parts → rows of cell-aligned clusters, using
  //! Emuera 1.824's pixel rules (design 2026-09-02, Component 5).
  //!
  //! * A cluster of `n` cells occupies `[x, x + n·half_w)` on its row; the font
  //!   that draws it never moves it.
  //! * Wrapping is character-granular at `drawable_w = content_w − shift`
  //!   (Emuera `PointX + Width > DrawableWidth`, ButtonWrap=false).
  //! * Alignment is Emuera's C# integer arithmetic on `content_w` (`WindowX`).
  //! * DRAWLINE repeats its string until it reaches `drawable_w`, trims, and is
  //!   laid out as ordinary text *after* the parts already on the line.
  //! * Underline / strike rects come from the primary font's `post` / `OS/2`
  //!   tables like GDI, with uEmuera's fixed rows as the fallback.
  //! * Buttons are split per row into `ButtonRegion` fragments. Hover and the
  //!   active input generation are not layout inputs (applied at draw time).
  //!
  //! All x positions are in Emuera's `PointX` space: the drawer adds
  //! `shift + x0` (GDI overhang padding).

  use std::sync::Arc;

  use erars_ast::{Alignment, Value};
  use erars_ui::{ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};
  use smol_str::SmolStr;

  use crate::text::{CellMetrics, Cluster, ShapedGlyph, Shaper};

  /// Horizontal geometry of the console area, in physical pixels.
  #[derive(Clone, Copy, Debug, PartialEq)]
  pub struct Geometry {
      /// Live window inner width (Emuera `WindowX`).
      pub content_w: u32,
      /// `content_w − m.shift` (Emuera `DrawableWidth`): rows wrap here.
      pub drawable_w: u32,
      pub m: CellMetrics,
  }

  impl Geometry {
      pub fn new(content_w: u32, m: CellMetrics) -> Self {
          Self {
              content_w,
              drawable_w: content_w.saturating_sub(m.shift),
              m,
          }
      }
  }

  /// Everything the drawer needs for one `ConsoleFrame` at one `Geometry`.
  #[derive(Clone, Debug, Default)]
  pub struct Layout {
      pub rows: Vec<Row>,
      pub buttons: Vec<ButtonRegion>,
  }

  /// One visual row. `line` indexes the `lines` slice given to [`layout`].
  #[derive(Clone, Debug)]
  pub struct Row {
      pub line: usize,
      /// `false` for a wrapped or residual-`\n` continuation row.
      pub logical_start: bool,
      /// Alignment offset in `PointX` space.
      pub x0: i32,
      /// Sum of the cluster boxes on this row.
      pub width: u32,
      pub clusters: Vec<PlacedCluster>,
      pub rects: Vec<Rect>,
  }

  #[derive(Clone, Debug)]
  pub struct PlacedCluster {
      /// Row-relative x, before `x0`.
      pub x: i32,
      pub cells: u8,
      /// The cluster's source characters.
      pub text: SmolStr,
      pub color: [u8; 3],
      pub style: FontStyle,
      /// Index into `Layout::buttons`.
      pub button: Option<usize>,
      /// `dx` / `dy` relative to `(x, row_y)`.
      pub glyphs: Arc<[ShapedGlyph]>,
  }

  #[derive(Clone, Copy, Debug, PartialEq, Eq)]
  pub enum RectKind {
      Underline,
      Strike,
  }

  /// One underline / strike bar spanning one styled run on one row.
  /// `x` is row-relative like `PlacedCluster::x`; `dy` is relative to the row top.
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct Rect {
      pub kind: RectKind,
      pub x: i32,
      pub dy: i32,
      pub h: u32,
      pub w: u32,
      pub color: [u8; 3],
      pub button: Option<usize>,
  }

  /// One button fragment. A button part split across rows yields several
  /// fragments with the same `input_gen` / `value`. `x` is row-relative, before
  /// `x0`; the hit rect is `[shift + x0 + x, row_y, w + 1, min(font_px + 1, line_h)]`
  /// (Emuera's inclusive test), evaluated in `app.rs`.
  #[derive(Clone, Debug, PartialEq, Eq)]
  pub struct ButtonRegion {
      pub row: usize,
      pub x: i32,
      pub w: u32,
      pub input_gen: u32,
      pub value: Value,
  }

  #[cfg(test)]
  mod tests {
      use super::*;

      /// Spec Testing §4: MS Gothic's 18 px geometry, pinned. The bundled Noto
      /// Sans Mono only supplies glyphs — it is Latin-only, so CJK and
      /// box-drawing characters resolve to `.notdef` — and every `x` / `cells` /
      /// `w` asserted below comes from the width classifier and these numbers,
      /// never from the font.
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

      fn geometry(content_w: u32) -> Geometry {
          Geometry::new(content_w, metrics())
      }

      #[test]
      fn geometry_and_alignment_use_emuera_integer_arithmetic() {
          // DrawableWidth = WindowX − max(2, FontSize/6) = 760 − 3
          assert_eq!(geometry(760).drawable_w, 757);
          assert_eq!(geometry(2).drawable_w, 0);
          // CENTER: WindowX/2 − width/2, both integer divisions (spec Component 5)
          assert_eq!(align_x0(Alignment::Center, 760, 44), 358);
          assert_eq!(align_x0(Alignment::Center, 760, 45), 358);
          assert_eq!(align_x0(Alignment::Center, 760, 46), 357);
          // RIGHT: WindowX − width
          assert_eq!(align_x0(Alignment::Right, 760, 45), 715);
          assert_eq!(align_x0(Alignment::Left, 760, 45), 0);
          // clamped at 0 when the row is wider than the window
          assert_eq!(align_x0(Alignment::Right, 30, 40), 0);
          assert_eq!(align_x0(Alignment::Center, 30, 40), 0);
      }
  }
  ```

  (`use super::*;` also brings in the parent's private imports — `Alignment`, `Value`, `ConsoleLine`, `ConsoleLinePart`, `FontStyle`, `TextStyle` — so the tests only add `Color` later.)

  Run: `cargo test -p erars-renderer layout::` — expected failure: `error[E0425]: cannot find function `align_x0` in this scope`.

- [ ] **Step 4: Implement `align_x0`**

  Insert after the `ButtonRegion` struct (before `#[cfg(test)]`):

  ```rust
  /// Emuera 1.824 `ConsoleDisplayLine.SetAlignment`: C# integer arithmetic on
  /// `WindowX` (not `DrawableWidth`; Emuera.EM differs), clamped at 0.
  fn align_x0(align: Alignment, content_w: u32, width: u32) -> i32 {
      let x0 = match align {
          Alignment::Left => 0,
          Alignment::Center => (content_w / 2) as i32 - (width / 2) as i32,
          Alignment::Right => content_w as i32 - width as i32,
      };
      x0.max(0)
  }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test layout::tests::geometry_and_alignment_use_emuera_integer_arithmetic ... ok`, `test result: ok. 1 passed` (dead-code warnings for the structs are fine until Step 10).

- [ ] **Step 5: Add the shaper helper and the failing `LineRules` test**

  Append inside `mod tests` (after `geometry`):

  ```rust
      use crate::font::{bundled_font_path, FontChain};
      use erars_compiler::Language;
      use erars_ui::width::WidthTable;

      /// Bundled Noto Sans Mono only (no system fonts, no locale) + the JP width table.
      fn shaper() -> Shaper {
          let chain = FontChain::from_files(&[bundled_font_path()], Language::Japanese);
          let widths = WidthTable::new(Language::Japanese.encoding());
          Shaper::new(chain, widths, metrics())
      }

      #[test]
      fn line_rules_follow_font_tables_with_fallback() {
          // MS Gothic: upem 256, post (−17, 19), OS/2 (66, 13) → rows 16 and 10 at 18 px (spec Component 5)
          assert_eq!(
              LineRules::compute(metrics(), 256.0, Some((-17, 19)), Some((66, 13))),
              LineRules { ul_dy: 16, ul_h: 1, st_dy: 10, st_h: 1 }
          );
          // bundled Noto Sans Mono: upem 1000, post (−100, 50), OS/2 (322, 50)
          assert_eq!(
              LineRules::compute(metrics(), 1000.0, Some((-100, 50)), Some((322, 50))),
              LineRules { ul_dy: 17, ul_h: 1, st_dy: 9, st_h: 1 }
          );
          // tables absent: uEmuera's font_px and font_px/2 − 1, 1 px thick
          assert_eq!(
              LineRules::compute(metrics(), 1000.0, None, None),
              LineRules { ul_dy: 18, ul_h: 1, st_dy: 8, st_h: 1 }
          );
          // the real primary (bundled font) reproduces the Noto numbers
          assert_eq!(
              LineRules::from_primary(&mut shaper()),
              LineRules { ul_dy: 17, ul_h: 1, st_dy: 9, st_h: 1 }
          );
      }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected failure: `error[E0433]: failed to resolve: use of undeclared type `LineRules``.

- [ ] **Step 6: Implement `LineRules`**

  Insert after `align_x0`:

  ```rust
  /// Underline / strike placement for the current metrics, relative to the row
  /// top (spec Component 5). GDI derives both from the font: underline from
  /// `post.underlinePosition/Thickness`, strike from `OS/2.yStrikeoutPosition/Size`.
  #[derive(Clone, Copy, Debug, PartialEq, Eq)]
  struct LineRules {
      ul_dy: i32,
      ul_h: u32,
      st_dy: i32,
      st_h: u32,
  }

  impl LineRules {
      /// Read the primary font's tables through the shaper's chain.
      fn from_primary(shaper: &mut Shaper) -> Self {
          let m = *shaper.metrics();
          let chain = shaper.chain();
          let primary = chain.primary();
          let font = chain.font(primary);
          // `rustybuzz::Face` derefs to `ttf_parser::Face` (underline_metrics,
          // strikeout_metrics); `units_per_em` is rustybuzz's inherent i32 accessor.
          let face = font.rustybuzz();
          let upem = face.units_per_em() as f32;
          let ul = face
              .underline_metrics()
              .map(|l| (l.position, l.thickness));
          let st = face
              .strikeout_metrics()
              .map(|l| (l.position, l.thickness));
          Self::compute(m, upem, ul, st)
      }

      /// `ul` / `st` are `(position, thickness)` in font units, or `None` when
      /// the table is absent. Fallbacks: underline at `font_px` (uEmuera),
      /// strike at `font_px / 2 − 1`, both 1 px.
      fn compute(
          m: CellMetrics,
          upem: f32,
          ul: Option<(i16, i16)>,
          st: Option<(i16, i16)>,
      ) -> Self {
          let px = m.font_px as f32;
          let scale = |v: i16| (v as f32 * px / upem).round();
          let (ul_dy, ul_h) = match ul {
              Some((pos, thick)) if upem > 0.0 => (
                  m.baseline as i32 + (-(pos as f32) * px / upem).round() as i32,
                  scale(thick).max(1.0) as u32,
              ),
              _ => (m.font_px as i32, 1),
          };
          let (st_dy, st_h) = match st {
              Some((pos, thick)) if upem > 0.0 => (
                  m.baseline as i32 - scale(pos) as i32,
                  scale(thick).max(1.0) as u32,
              ),
              _ => ((m.font_px / 2) as i32 - 1, 1),
          };
          Self {
              ul_dy,
              ul_h,
              st_dy,
              st_h,
          }
      }
  }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 2 passed`. (If `from_primary` fails to compile on `underline_metrics`, `cosmic_text::Font::rustybuzz()` is no longer returning `&rustybuzz::Face` — check T5/T6 did not wrap it; the deref chain is cosmic-text 0.12.1 `font/mod.rs:77` → rustybuzz 0.14.1 `hb/face.rs:51`.)

- [ ] **Step 7: Add the failing `rule_string` test**

  Append inside `mod tests`:

  ```rust
      use erars_ui::Color;

      /// Emuera's default ForeColor; clusters in this colour print no `c=`.
      const FG: [u8; 3] = [192, 192, 192];

      fn style() -> TextStyle {
          TextStyle {
              color: Color(FG),
              font_family: "".into(),
              font_style: FontStyle::NORMAL,
          }
      }

      #[test]
      fn rule_string_grows_then_trims() {
          let mut sh = shaper();
          let g = geometry(760); // drawable 757
          // "-" × 85 = 765 px > 757 → trim one → 84 (spec: 84 `-` at the defaults)
          assert_eq!(rule_string(&mut sh, &style(), "-", &g).unwrap().len(), 84);
          // 5-cell unit "──-": 17 reps = 765 px → trailing "-" dropped → 16 reps + "──" = 756 px
          let r = rule_string(&mut sh, &style(), "──-", &g).unwrap();
          assert_eq!(r.chars().count(), 50);
          assert!(r.ends_with("──-──"), "{r:?}");
          // zero-width / empty rules cannot fill anything (Emuera's getStBar would loop forever)
          assert_eq!(rule_string(&mut sh, &style(), "", &g), None);
          assert_eq!(rule_string(&mut sh, &style(), "\u{0301}", &g), None);
          // a `\n` inside a CUSTOMDRAWLINE string is dropped before repeating: "ab" at drawable 27 → "abab" → "aba"
          assert_eq!(
              rule_string(&mut sh, &style(), "a\nb", &geometry(30)).as_deref(),
              Some("aba")
          );
      }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected failure: `error[E0425]: cannot find function `rule_string` in this scope`.

- [ ] **Step 8: Implement `rule_string`, commit**

  Insert after `impl LineRules`:

  ```rust
  /// Emuera `getStBar` (Print.cs:543-560): repeat `s` until its width reaches
  /// `drawable_w`, then drop trailing characters while it still exceeds it.
  /// Widths are cells·half_w, so trimming whole clusters is cell-identical to
  /// Emuera's per-character trim. `None` when `s` has no width (Emuera would
  /// loop forever). `\n` is removed first (CUSTOMDRAWLINE may carry one).
  fn rule_string(shaper: &mut Shaper, style: &TextStyle, s: &str, g: &Geometry) -> Option<String> {
      let s: String = s.chars().filter(|&c| c != '\n').collect();
      if s.is_empty() {
          return None;
      }
      let unit_clusters = shaper.shape(&s, style);
      let half_w = g.m.half_w;
      let unit_cells: u32 = unit_clusters.iter().map(|c| c.cells as u32).sum();
      let unit = unit_cells * half_w;
      if unit == 0 {
          return None;
      }
      let reps = (g.drawable_w + unit - 1) / unit; // ceil; 0 when drawable_w == 0
      let mut pieces: Vec<(&str, u32)> = Vec::with_capacity(unit_clusters.len() * reps as usize);
      for _ in 0..reps {
          for c in unit_clusters.iter() {
              pieces.push((c.text.as_str(), c.cells as u32 * half_w));
          }
      }
      let mut width = reps * unit;
      while width > g.drawable_w {
          let Some((_, w)) = pieces.pop() else { break };
          width -= w;
      }
      Some(pieces.iter().map(|(t, _)| *t).collect())
  }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 3 passed`. Commit:
  `git add crates/erars-renderer/Cargo.toml Cargo.lock crates/erars-renderer/src/main.rs crates/erars-renderer/src/layout.rs && git commit -m "feat(renderer): layout data model, Emuera alignment, line rules and DRAWLINE rule string"`

- [ ] **Step 9: Add the snapshot helpers and the first golden batch (failing)**

  Append inside `mod tests`:

  ```rust
      fn styled(font_style: FontStyle) -> TextStyle {
          TextStyle {
              font_style,
              ..style()
          }
      }

      fn text(s: &str) -> ConsoleLinePart {
          ConsoleLinePart::Text(s.to_owned(), style())
      }

      fn rule(s: &str) -> ConsoleLinePart {
          ConsoleLinePart::Line(s.to_owned(), style())
      }

      fn button(s: &str, input_gen: u32, value: Value) -> ConsoleLinePart {
          ConsoleLinePart::Button(vec![(s.to_owned(), style())], input_gen, value)
      }

      fn line(align: Alignment, parts: Vec<ConsoleLinePart>) -> ConsoleLine {
          ConsoleLine {
              align,
              button_start: None,
              parts,
          }
      }

      fn snap(lines: &[ConsoleLine], content_w: u32) -> String {
          let mut sh = shaper();
          layout_snapshot(&layout(lines, &geometry(content_w), &mut sh), FG)
      }

      #[test]
      fn empty_line_is_one_row() {
          k9::snapshot!(snap(&[line(Alignment::Left, vec![])], 760), "row 0 line 0 x0=0 w=0");
      }

      #[test]
      fn plain_text_colour_and_style_tags() {
          let red_bold = TextStyle {
              color: Color([255, 0, 0]),
              ..styled(FontStyle::BOLD)
          };
          k9::snapshot!(
              snap(
                  &[line(
                      Alignment::Left,
                      vec![text("ab"), ConsoleLinePart::Text("c".into(), red_bold)]
                  )],
                  760
              ),
              r#"
  row 0 line 0 x0=0 w=27
    0:1 "a"
    9:1 "b"
    18:1 "c" c=FF0000 s=B
  "#
          );
      }

      /// `あ` is not in the bundled font (it draws `.notdef`), but the JP width
      /// table gives it 2 cells, so `b` lands at 27 regardless of the glyph.
      #[test]
      fn full_width_cells_without_glyphs() {
          k9::snapshot!(
              snap(&[line(Alignment::Left, vec![text("aあb")])], 760),
              r#"
  row 0 line 0 x0=0 w=36
    0:1 "a"
    9:2 "あ"
    27:1 "b"
  "#
          );
      }
  ```

  k9 inline-snapshot rule (k9 0.11.6 `value_to_string`): a multi-line string value is wrapped in one leading and one trailing `\n` and compared exactly, so every multi-line literal above starts and ends with a newline and its content lines start at column 0 of the raw string (the two-space indent of cluster lines is part of the snapshot format); single-line values compare bare.

  Run: `cargo test -p erars-renderer layout::` — expected failure: `error[E0425]: cannot find function `layout_snapshot` in this scope` (and the same for `layout`).

- [ ] **Step 10: Implement `LineBuilder`, `layout` and `layout_snapshot`, commit**

  Insert after `rule_string` (before `#[cfg(test)]`):

  ```rust
  /// The styled run currently being placed (for underline / strike rects).
  struct RunState {
      style: FontStyle,
      color: [u8; 3],
      /// Row-relative x of the run's first cluster on the current row.
      start: Option<i32>,
  }

  /// Walks one `ConsoleLine` with a pixel cursor and emits rows into a `Layout`.
  struct LineBuilder<'a> {
      g: &'a Geometry,
      rules: LineRules,
      line: usize,
      align: Alignment,
      logical_start: bool,
      /// Pixel cursor on the current row (sum of the boxes placed so far).
      x: i32,
      clusters: Vec<PlacedCluster>,
      rects: Vec<Rect>,
      run: Option<RunState>,
      /// The button part being walked: `(input_gen, value)`.
      button: Option<(u32, &'a Value)>,
      /// This row's fragment of that button: `(index into Layout::buttons, start x)`.
      frag: Option<(usize, i32)>,
  }

  impl<'a> LineBuilder<'a> {
      fn new(g: &'a Geometry, rules: LineRules, line: usize, align: Alignment) -> Self {
          Self {
              g,
              rules,
              line,
              align,
              logical_start: true,
              x: 0,
              clusters: Vec::new(),
              rects: Vec::new(),
              run: None,
              button: None,
              frag: None,
          }
      }

      /// Place one styled run. A `\n` (only reachable from the console paths
      /// that do not split: PRINTC/PRINTLC, PRINTPLAIN, PRINTSINGLE,
      /// CUSTOMDRAWLINE, REUSELASTLINE) finishes the row and continues on a
      /// continuation row; it occupies no cells and never reaches the shaper.
      fn push_run(&mut self, text: &str, style: &TextStyle, shaper: &mut Shaper, out: &mut Layout) {
          self.run = Some(RunState {
              style: style.font_style,
              color: style.color.0,
              start: None,
          });
          for (i, seg) in text.split('\n').enumerate() {
              if i > 0 {
                  self.break_row(out);
              }
              if seg.is_empty() {
                  continue;
              }
              let clusters = shaper.shape(seg, style);
              for c in clusters.iter() {
                  self.place(c, style, out);
              }
          }
          self.flush_run_rects();
          self.run = None;
      }

      /// Character-granular wrapping: `x + w > drawable_w` with `x > 0` finishes
      /// the row and the cluster starts the next one (a full-width cluster moves
      /// whole; the first cluster of a row is always placed).
      fn place(&mut self, c: &Cluster, style: &TextStyle, out: &mut Layout) {
          let w = c.cells as u32 * self.g.m.half_w;
          if self.x > 0 && self.x as u32 + w > self.g.drawable_w {
              self.break_row(out);
          }
          let button = match (self.button, self.frag) {
              (Some(_), Some((i, _))) => Some(i),
              (Some(_), None) => {
                  // The region is pushed when the fragment ends; nothing else can
                  // push a region in between, so its index is known now.
                  let i = out.buttons.len();
                  self.frag = Some((i, self.x));
                  Some(i)
              }
              (None, _) => None,
          };
          if let Some(run) = &mut self.run {
              if run.start.is_none() {
                  run.start = Some(self.x);
              }
          }
          self.clusters.push(PlacedCluster {
              x: self.x,
              cells: c.cells,
              text: c.text.clone(),
              color: style.color.0,
              style: style.font_style,
              button,
              glyphs: Arc::from(&c.glyphs[..]),
          });
          self.x += w as i32;
      }

      /// One rect per styled run per row, spanning its cluster boxes.
      fn flush_run_rects(&mut self) {
          let Some(run) = self.run.as_mut() else { return };
          let Some(start) = run.start.take() else { return };
          let w = (self.x - start) as u32;
          if w == 0 {
              return;
          }
          let button = self.frag.map(|(i, _)| i);
          if run.style.contains(FontStyle::UNDERLINE) {
              self.rects.push(Rect {
                  kind: RectKind::Underline,
                  x: start,
                  dy: self.rules.ul_dy,
                  h: self.rules.ul_h,
                  w,
                  color: run.color,
                  button,
              });
          }
          if run.style.contains(FontStyle::STRIKELINE) {
              self.rects.push(Rect {
                  kind: RectKind::Strike,
                  x: start,
                  dy: self.rules.st_dy,
                  h: self.rules.st_h,
                  w,
                  color: run.color,
                  button,
              });
          }
      }

      fn begin_button(&mut self, input_gen: u32, value: &'a Value) {
          self.button = Some((input_gen, value));
      }

      fn end_button(&mut self, out: &mut Layout) {
          self.end_fragment(out);
          self.button = None;
      }

      /// Emit the `ButtonRegion` for this row's fragment (if any cluster landed).
      fn end_fragment(&mut self, out: &mut Layout) {
          if let (Some((i, start)), Some((input_gen, value))) = (self.frag.take(), self.button) {
              debug_assert_eq!(i, out.buttons.len());
              out.buttons.push(ButtonRegion {
                  row: out.rows.len(),
                  x: start,
                  w: (self.x - start) as u32,
                  input_gen,
                  value: value.clone(),
              });
          }
      }

      /// Finish the current row (rects, button fragment, alignment) and start a
      /// continuation row.
      fn break_row(&mut self, out: &mut Layout) {
          self.flush_run_rects();
          self.end_fragment(out);
          let width = self.x.max(0) as u32;
          out.rows.push(Row {
              line: self.line,
              logical_start: self.logical_start,
              x0: align_x0(self.align, self.g.content_w, width),
              width,
              clusters: std::mem::take(&mut self.clusters),
              rects: std::mem::take(&mut self.rects),
          });
          self.logical_start = false;
          self.x = 0;
      }

      /// Every `ConsoleLine` yields at least one row (an empty line is a blank row).
      fn finish(mut self, out: &mut Layout) {
          self.break_row(out);
      }
  }

  /// Lay out `lines` (a `ConsoleFrame`'s lines, oldest first) at `g`.
  /// Ends with `shaper.sweep()`, so the shape cache holds exactly these strings.
  pub fn layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper) -> Layout {
      let rules = LineRules::from_primary(shaper);
      let mut out = Layout::default();
      for (li, line) in lines.iter().enumerate() {
          let mut b = LineBuilder::new(g, rules, li, line.align);
          for part in &line.parts {
              match part {
                  ConsoleLinePart::Text(s, style) => b.push_run(s, style, shaper, &mut out),
                  ConsoleLinePart::Line(s, style) => {
                      // DRAWLINE / CUSTOMDRAWLINE: Regular style, current colour
                      // (Emuera PrintBar); the console stores NORMAL too (T3).
                      let style = TextStyle {
                          font_style: FontStyle::NORMAL,
                          ..style.clone()
                      };
                      match rule_string(shaper, &style, s, g) {
                          Some(rule) => b.push_run(&rule, &style, shaper, &mut out),
                          None => log::warn!("DRAWLINE string {s:?} has no width; skipped"),
                      }
                  }
                  ConsoleLinePart::Button(parts, input_gen, value) => {
                      b.begin_button(*input_gen, value);
                      for (s, style) in parts {
                          b.push_run(s, style, shaper, &mut out);
                      }
                      b.end_button(&mut out);
                  }
              }
          }
          b.finish(&mut out);
      }
      shaper.sweep();
      out
  }

  fn style_letters(style: FontStyle) -> String {
      let mut out = String::new();
      if style.contains(FontStyle::BOLD) {
          out.push('B');
      }
      if style.contains(FontStyle::ITALIC) {
          out.push('I');
      }
      if style.contains(FontStyle::UNDERLINE) {
          out.push('U');
      }
      if style.contains(FontStyle::STRIKELINE) {
          out.push('S');
      }
      out
  }

  /// Font-independent text form of a `Layout` (spec Component 7), one line per
  /// row / cluster / rect / button, joined by `\n` without a trailing newline:
  ///
  /// * `row <r> line <line>[+] x0=<x0> w=<width>` — `+` marks a continuation row
  /// * `  <x>:<cells> "<text>" [c=RRGGBB] [s=<BIUS>] [btn=<i>]` (two-space indent)
  /// * `  rect <underline|strike> x=<x> dy=<dy> h=<h> w=<w> [btn=<i>]`
  /// * `btn <i> row=<r> x=<x> w=<w> gen=<gen> value=<Value as Debug>`
  ///
  /// `c=` only when the colour differs from `default_fg`; `s=` only when the
  /// style is not `NORMAL`. No font id, glyph id, `dx`, `dy` or `size_px`.
  pub fn layout_snapshot(layout: &Layout, default_fg: [u8; 3]) -> String {
      use std::fmt::Write;
      let mut lines: Vec<String> = Vec::new();
      for (r, row) in layout.rows.iter().enumerate() {
          lines.push(format!(
              "row {r} line {}{} x0={} w={}",
              row.line,
              if row.logical_start { "" } else { "+" },
              row.x0,
              row.width
          ));
          for c in &row.clusters {
              let mut s = format!("  {}:{} {:?}", c.x, c.cells, c.text.as_str());
              if c.color != default_fg {
                  let _ = write!(s, " c={:02X}{:02X}{:02X}", c.color[0], c.color[1], c.color[2]);
              }
              if !c.style.is_empty() {
                  let _ = write!(s, " s={}", style_letters(c.style));
              }
              if let Some(b) = c.button {
                  let _ = write!(s, " btn={b}");
              }
              lines.push(s);
          }
          for rect in &row.rects {
              let kind = match rect.kind {
                  RectKind::Underline => "underline",
                  RectKind::Strike => "strike",
              };
              let mut s = format!(
                  "  rect {kind} x={} dy={} h={} w={}",
                  rect.x, rect.dy, rect.h, rect.w
              );
              if let Some(b) = rect.button {
                  let _ = write!(s, " btn={b}");
              }
              lines.push(s);
          }
      }
      for (i, b) in layout.buttons.iter().enumerate() {
          lines.push(format!(
              "btn {i} row={} x={} w={} gen={} value={:?}",
              b.row, b.x, b.w, b.input_gen, b.value
          ));
      }
      lines.join("\n")
  }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 6 passed`. A k9 diff here is a layout, shaper (T6) or width-table (T1) bug, not a stale snapshot: every number in these goldens was derived from the rules (cells × 9, drawable 757, Emuera integer alignment) and verified in the planning probe; do not run `K9_UPDATE_SNAPSHOTS=1` to "fix" it. Commit:
  `git add crates/erars-renderer/src/layout.rs && git commit -m "feat(renderer): row layout with wrapping, button fragments, rects and layout_snapshot"`

- [ ] **Step 11: Goldens — mid-word wrap, full-width cluster moves whole, residual `\n`, tab stops**

  Append inside `mod tests` (wrap cases use `content_w = 93` → drawable 90 = 10 half cells, so the snapshots stay short; the rules are width-independent):

  ```rust
      #[test]
      fn mid_word_wrap_is_character_granular() {
          k9::snapshot!(
              snap(&[line(Alignment::Left, vec![text("abcdefghijkl")])], 93),
              r#"
  row 0 line 0 x0=0 w=90
    0:1 "a"
    9:1 "b"
    18:1 "c"
    27:1 "d"
    36:1 "e"
    45:1 "f"
    54:1 "g"
    63:1 "h"
    72:1 "i"
    81:1 "j"
  row 1 line 0+ x0=0 w=18
    0:1 "k"
    9:1 "l"
  "#
          );
      }

      /// 9 cells are used; the 2-cell `あ` would end at 99 > 90, so it moves whole.
      #[test]
      fn full_width_cluster_that_does_not_fit_moves_whole() {
          k9::snapshot!(
              snap(&[line(Alignment::Left, vec![text("abcdefghiあ")])], 93),
              r#"
  row 0 line 0 x0=0 w=81
    0:1 "a"
    9:1 "b"
    18:1 "c"
    27:1 "d"
    36:1 "e"
    45:1 "f"
    54:1 "g"
    63:1 "h"
    72:1 "i"
  row 1 line 0+ x0=0 w=18
    0:2 "あ"
  "#
          );
      }

      /// `print_plain("a\nb")` keeps the `\n` inside one Text part (T3); the row
      /// breaks there, the `\n` occupies no cells and the next row is a
      /// continuation (`+`).
      #[test]
      fn residual_newline_from_print_plain() {
          k9::snapshot!(
              snap(&[line(Alignment::Left, vec![text("a\nb")])], 760),
              r#"
  row 0 line 0 x0=0 w=9
    0:1 "a"
  row 1 line 0+ x0=0 w=9
    0:1 "b"
  "#
          );
      }

      /// The shaper (T6) expands `\t` to 1-cell `" "` clusters up to the next
      /// multiple of 8 cells, counted from the start of the part's text:
      /// `a` + 7 spaces, `b` at 72.
      #[test]
      fn tab_expands_to_eight_cell_stops() {
          k9::snapshot!(
              snap(&[line(Alignment::Left, vec![text("a\tb")])], 760),
              r#"
  row 0 line 0 x0=0 w=81
    0:1 "a"
    9:1 " "
    18:1 " "
    27:1 " "
    36:1 " "
    45:1 " "
    54:1 " "
    63:1 " "
    72:1 "b"
  "#
          );
      }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 10 passed`. (T6's own `text::tests::tab_expands_to_eight_cell_stops` pins the `["a", " ", …, "b"]` cluster representation this golden relies on.)

- [ ] **Step 12: Goldens — Center / Right offsets, alignment on every wrapped row**

  Append inside `mod tests`:

  ```rust
      /// 45 px → 380 − 22 = 358 (Center), 760 − 45 = 715 (Right); 36 px → 362; 54 px → 353.
      #[test]
      fn center_and_right_offsets_use_windowx() {
          k9::snapshot!(
              snap(
                  &[
                      line(Alignment::Center, vec![text("abcde")]),
                      line(Alignment::Right, vec![text("abcde")]),
                      line(Alignment::Center, vec![text("abcd")]),
                      line(Alignment::Center, vec![text("abcdef")]),
                  ],
                  760
              ),
              r#"
  row 0 line 0 x0=358 w=45
    0:1 "a"
    9:1 "b"
    18:1 "c"
    27:1 "d"
    36:1 "e"
  row 1 line 1 x0=715 w=45
    0:1 "a"
    9:1 "b"
    18:1 "c"
    27:1 "d"
    36:1 "e"
  row 2 line 2 x0=362 w=36
    0:1 "a"
    9:1 "b"
    18:1 "c"
    27:1 "d"
  row 3 line 3 x0=353 w=54
    0:1 "a"
    9:1 "b"
    18:1 "c"
    27:1 "d"
    36:1 "e"
    45:1 "f"
  "#
          );
      }

      /// Emuera applies the alignment to every wrapped fragment: 93 − 90 = 3, 93 − 18 = 75.
      #[test]
      fn alignment_applies_to_every_wrapped_row() {
          k9::snapshot!(
              snap(&[line(Alignment::Right, vec![text("abcdefghijkl")])], 93),
              r#"
  row 0 line 0 x0=3 w=90
    0:1 "a"
    9:1 "b"
    18:1 "c"
    27:1 "d"
    36:1 "e"
    45:1 "f"
    54:1 "g"
    63:1 "h"
    72:1 "i"
    81:1 "j"
  row 1 line 0+ x0=75 w=18
    0:1 "k"
    9:1 "l"
  "#
          );
      }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 12 passed`.

- [ ] **Step 13: Goldens — DRAWLINE fill, partial trim, `abc` + DRAWLINE spill, zero-width rule; 760-px assertions**

  Append inside `mod tests`:

  ```rust
      /// The rule keeps the colour but is forced to NORMAL (no `s=`, no rect).
      #[test]
      fn drawline_fills_drawable_width_in_normal_style() {
          let red_bold_underline = TextStyle {
              color: Color([255, 0, 0]),
              ..styled(FontStyle::BOLD | FontStyle::UNDERLINE)
          };
          k9::snapshot!(
              snap(
                  &[line(
                      Alignment::Left,
                      vec![ConsoleLinePart::Line("-".into(), red_bold_underline)]
                  )],
                  93
              ),
              r#"
  row 0 line 0 x0=0 w=90
    0:1 "-" c=FF0000
    9:1 "-" c=FF0000
    18:1 "-" c=FF0000
    27:1 "-" c=FF0000
    36:1 "-" c=FF0000
    45:1 "-" c=FF0000
    54:1 "-" c=FF0000
    63:1 "-" c=FF0000
    72:1 "-" c=FF0000
    81:1 "-" c=FF0000
  "#
          );
      }

      /// 3-character, 5-cell rule at drawable 108 (12 cells): 3 reps = 135 px,
      /// trailing "-" dropped → 108 px, so the row ends inside a repetition.
      #[test]
      fn drawline_trims_a_partial_repetition() {
          k9::snapshot!(
              snap(&[line(Alignment::Left, vec![rule("──-")])], 111),
              r#"
  row 0 line 0 x0=0 w=108
    0:2 "─"
    18:2 "─"
    36:1 "-"
    45:2 "─"
    63:2 "─"
    81:1 "-"
    90:2 "─"
  "#
          );
      }

      /// The rule string is computed once against drawable_w (10 dashes) and laid
      /// out after the pending `abc`, so 7 dashes fit and 3 spill (Emuera 1.824,
      /// ButtonWrap=false).
      #[test]
      fn text_then_drawline_spills_to_next_row() {
          k9::snapshot!(
              snap(&[line(Alignment::Left, vec![text("abc"), rule("-")])], 93),
              r#"
  row 0 line 0 x0=0 w=90
    0:1 "a"
    9:1 "b"
    18:1 "c"
    27:1 "-"
    36:1 "-"
    45:1 "-"
    54:1 "-"
    63:1 "-"
    72:1 "-"
    81:1 "-"
  row 1 line 0+ x0=0 w=27
    0:1 "-"
    9:1 "-"
    18:1 "-"
  "#
          );
      }

      /// `unit == 0` (a combining-mark-only or empty rule) is skipped with a
      /// warning; the line still takes a blank row.
      #[test]
      fn drawline_with_zero_width_rule_is_skipped() {
          k9::snapshot!(
              snap(
                  &[
                      line(Alignment::Left, vec![rule("\u{0301}")]),
                      line(Alignment::Left, vec![rule("")]),
                  ],
                  760
              ),
              r#"
  row 0 line 0 x0=0 w=0
  row 1 line 1 x0=0 w=0
  "#
          );
      }

      /// Spec Component 5 at the Emuera defaults (760 → 757): 84 `-`;
      /// `abc` + DRAWLINE → row 1 = `abc` + 81 `-`, row 2 = 3 `-`.
      #[test]
      fn drawline_at_emuera_defaults() {
          let mut sh = shaper();
          let g = geometry(760);
          let l = layout(&[line(Alignment::Left, vec![rule("-")])], &g, &mut sh);
          assert_eq!(l.rows.len(), 1);
          let row = &l.rows[0];
          assert_eq!((row.clusters.len(), row.width), (84, 756));
          assert!(row.clusters.iter().all(|c| c.cells == 1 && c.text.as_str() == "-"));
          assert_eq!(row.clusters[83].x, 747);

          let l = layout(&[line(Alignment::Left, vec![text("abc"), rule("-")])], &g, &mut sh);
          assert_eq!(l.rows.len(), 2);
          assert_eq!((l.rows[0].clusters.len(), l.rows[0].width), (84, 756));
          assert_eq!(l.rows[0].clusters[3].text.as_str(), "-");
          assert_eq!(
              (l.rows[1].clusters.len(), l.rows[1].width, l.rows[1].logical_start),
              (3, 27, false)
          );
      }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 17 passed` (the zero-width test also emits one `WARN` per skipped rule if a logger is active; that is fine).

- [ ] **Step 14: Goldens — PRINTC columns, button fragments across a wrap, two buttons on a row**

  Append inside `mod tests`:

  ```rust
      /// Three 8-cell PRINTC columns as the console pads them by cells (T3):
      /// `aa` right-aligned in cells 0–7, `あbc` (4 cells) in 8–15, `x` in 16–23.
      #[test]
      fn printc_columns_land_on_cell_boundaries() {
          k9::snapshot!(
              snap(&[line(Alignment::Left, vec![text("      aa    あbc       x")])], 760),
              r#"
  row 0 line 0 x0=0 w=216
    0:1 " "
    9:1 " "
    18:1 " "
    27:1 " "
    36:1 " "
    45:1 " "
    54:1 "a"
    63:1 "a"
    72:1 " "
    81:1 " "
    90:1 " "
    99:1 " "
    108:2 "あ"
    126:1 "b"
    135:1 "c"
    144:1 " "
    153:1 " "
    162:1 " "
    171:1 " "
    180:1 " "
    189:1 " "
    198:1 " "
    207:1 "x"
  "#
          );
      }

      /// A button part wraps mid-text: each row gets its own fragment with the
      /// same generation and value; clusters carry the fragment index.
      #[test]
      fn button_fragments_across_a_wrap() {
          k9::snapshot!(
              snap(
                  &[line(
                      Alignment::Left,
                      vec![text("ab"), button("[1] click", 3, Value::Int(1))]
                  )],
                  93
              ),
              r#"
  row 0 line 0 x0=0 w=90
    0:1 "a"
    9:1 "b"
    18:1 "[" btn=0
    27:1 "1" btn=0
    36:1 "]" btn=0
    45:1 " " btn=0
    54:1 "c" btn=0
    63:1 "l" btn=0
    72:1 "i" btn=0
    81:1 "c" btn=0
  row 1 line 0+ x0=0 w=9
    0:1 "k" btn=1
  btn 0 row=0 x=18 w=72 gen=3 value=Int(1)
  btn 1 row=1 x=0 w=9 gen=3 value=Int(1)
  "#
          );
      }

      #[test]
      fn two_buttons_on_one_row() {
          k9::snapshot!(
              snap(
                  &[line(
                      Alignment::Left,
                      vec![
                          button("[0]", 1, Value::Int(0)),
                          text(" "),
                          button("go", 1, Value::String("go".into())),
                      ]
                  )],
                  760
              ),
              r#"
  row 0 line 0 x0=0 w=54
    0:1 "[" btn=0
    9:1 "0" btn=0
    18:1 "]" btn=0
    27:1 " "
    36:1 "g" btn=1
    45:1 "o" btn=1
  btn 0 row=0 x=0 w=27 gen=1 value=Int(0)
  btn 1 row=0 x=36 w=18 gen=1 value=String("go")
  "#
          );
      }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 20 passed`.

- [ ] **Step 15: Goldens — underline / strike rects, one rect per row inside a wrapped button**

  Append inside `mod tests`:

  ```rust
      /// One rect per styled run per row. Bundled Noto Sans Mono at 18 px:
      /// underline dy = 15 + round(100·18/1000) = 17, strike dy = 15 − round(322·18/1000) = 9, 1 px each.
      #[test]
      fn underline_and_strike_rects_span_their_runs() {
          k9::snapshot!(
              snap(
                  &[line(
                      Alignment::Left,
                      vec![
                          ConsoleLinePart::Text("ab".into(), styled(FontStyle::UNDERLINE)),
                          ConsoleLinePart::Text("cd".into(), styled(FontStyle::STRIKELINE)),
                          ConsoleLinePart::Text(
                              "ef".into(),
                              styled(
                                  FontStyle::BOLD
                                      | FontStyle::ITALIC
                                      | FontStyle::UNDERLINE
                                      | FontStyle::STRIKELINE
                              )
                          ),
                      ]
                  )],
                  760
              ),
              r#"
  row 0 line 0 x0=0 w=54
    0:1 "a" s=U
    9:1 "b" s=U
    18:1 "c" s=S
    27:1 "d" s=S
    36:1 "e" s=BIUS
    45:1 "f" s=BIUS
    rect underline x=0 dy=17 h=1 w=18
    rect strike x=18 dy=9 h=1 w=18
    rect underline x=36 dy=17 h=1 w=18
    rect strike x=36 dy=9 h=1 w=18
  "#
          );
      }

      #[test]
      fn underlined_button_gets_one_rect_per_row() {
          k9::snapshot!(
              snap(
                  &[line(
                      Alignment::Left,
                      vec![ConsoleLinePart::Button(
                          vec![("abcdefghijkl".into(), styled(FontStyle::UNDERLINE))],
                          2,
                          Value::Int(5)
                      )]
                  )],
                  93
              ),
              r#"
  row 0 line 0 x0=0 w=90
    0:1 "a" s=U btn=0
    9:1 "b" s=U btn=0
    18:1 "c" s=U btn=0
    27:1 "d" s=U btn=0
    36:1 "e" s=U btn=0
    45:1 "f" s=U btn=0
    54:1 "g" s=U btn=0
    63:1 "h" s=U btn=0
    72:1 "i" s=U btn=0
    81:1 "j" s=U btn=0
    rect underline x=0 dy=17 h=1 w=90 btn=0
  row 1 line 0+ x0=0 w=18
    0:1 "k" s=U btn=1
    9:1 "l" s=U btn=1
    rect underline x=0 dy=17 h=1 w=18 btn=1
  btn 0 row=0 x=0 w=90 gen=2 value=Int(5)
  btn 1 row=1 x=0 w=18 gen=2 value=Int(5)
  "#
          );
      }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 22 passed`.

- [ ] **Step 16: Button regions carry what `app.rs` hit-tests (Right alignment)**

  Append inside `mod tests` (this is the geometry T10's `hit_test_uses_emuera_inclusive_rects_and_whole_rows` relies on: `x0 = 706`, fragment `x = 18`, `w = 36`):

  ```rust
      /// A button region's `x`/`w` are what `app.rs` hit-tests: draw x is
      /// `shift + x0 + x`, so with Right alignment the region moves with `x0`.
      #[test]
      fn button_regions_follow_alignment_offset() {
          let mut sh = shaper();
          let g = geometry(760);
          let l = layout(
              &[line(Alignment::Right, vec![text("AB"), button("[1] ", 7, Value::Int(1))])],
              &g,
              &mut sh,
          );
          assert_eq!(l.rows[0].x0, 760 - 54);
          assert_eq!(
              l.buttons,
              vec![ButtonRegion { row: 0, x: 18, w: 36, input_gen: 7, value: Value::Int(1) }]
          );
      }
  ```

  Run: `cargo test -p erars-renderer layout::` — expected: `test result: ok. 23 passed`.

- [ ] **Step 17: Full crate test run, format, commit**

  Run `cargo test -p erars-renderer` (everything, not only `layout::`) — expected: every `test result:` line `ok`, `0 failed` (legacy GPU tests may print `SKIP …: no wgpu adapter`; that is not a failure). Then `cargo fmt -p erars-renderer` and re-run `cargo test -p erars-renderer layout::` (still `23 passed`). Commit:

  `git add crates/erars-renderer/src/layout.rs && git commit -m "test(renderer): layout goldens for wrapping, alignment, DRAWLINE, PRINTC, buttons and rects"`

---

### Task 8: Raster + draw + gpu

Spec: Component 6 (`raster.rs`, `draw.rs`, atlas pages, Nearest sampler, Rect mode 0, hover recolour) and the "View state" paragraph of Component 5. Why the rules exist: critique H1 (swash 0.1.18 cannot read MS Gothic's index-format-5 EBLC subtables → path 1 goes through ttf-parser, `Source::Bitmap` is never used), H6 (hover is colour-only and applied at draw time, never in layout), H8/R13 (strikes only at integer `size_px` and only when the returned strike's `pixels_per_em` is exact — ttf-parser picks the *nearest* strike), R14 (`hint` is a `ScalerBuilder` method; `embolden`/`transform` apply only to `Source::Outline`; `Transform::skew(Angle, Angle)`; `placement` is baseline-relative with y up), R34 (`RasterFlags` lives in `flags.rs`, the bitmap switch is a `GlyphRaster` option, not a key bit).

Every library call below was compile-checked and executed against the locked crates with bare `rustc --extern … target/debug/deps/lib*.rlib` (`scratchpad/probe-plan-08/p5.rs` = this task's `raster.rs` + `draw.rs` + the new `gpu.rs` items verbatim with T5–T7 types stubbed, run on the NVIDIA/Vulkan adapter; `p6.rs` = bundled-glyph placement at 15/18 px). Measured facts the tests rely on: MS Gothic face 0 at 18 ppem — `A` and `═` are 9×18 `BitmapMonoPacked` with `x = 0, y = −3` (→ `top = 15`), `あ` 18×18 with 61 set bits, `A` 28, `═` 18, the space is an all-zero 9×18 strike; requesting 23 ppem returns the 22 ppem strike (must be rejected). Bundled Noto Sans Mono `A` at 18 px hinted = 13×13 mask with grey values, `left = −1, top = 13`; synthetic bold = 15×15; the space renders as a 2×0 image (`width == 0 || height == 0` = blank). At 15 px (the T6 rescale for the 0.6 em bundled font in a 9 px cell) `a`/`c` span x 0..9, `b` 0..10, `A` −1..10 — within ±2 px of a cell.

**Files:**
- Create: `crates/erars-renderer/src/raster.rs` (replaces `atlas.rs`, which T10 deletes)
- Modify: `crates/erars-renderer/src/draw.rs` — whole file (current lines 1–90: `build_instances` over `Grid` + one GPU test over `Grid`); the old function survives as `build_instances_legacy` until T10 deletes it with `grid.rs`/`atlas.rs`
- Modify: `crates/erars-renderer/src/gpu.rs` — line 5 (`Instance` derives), lines 176–180 (Linear sampler in `GpuContext::new`), lines 212–293 (`render`); new `nearest_sampler` + `FrameDraw` inserted after line 106 (closing brace of `create_quad_pipeline`)
- Modify: `crates/erars-renderer/src/main.rs` — module list (lines 3–13 today; T5 added `mod flags;`, T7 `mod layout;`): add `mod raster;`, put `#[allow(dead_code)]` on `mod draw;`
- Modify: `crates/erars-renderer/src/test_support.rs` — append `test_name` + `gpu_device` (file is 13 lines today; T9 replaces it wholesale and keeps both signatures)
- Modify: `crates/erars-renderer/src/app.rs` — line 15 (`use crate::draw::build_instances;`), line 159 (legacy call), line 168 (`gpu.render(...)`) — one-line patches so the crate keeps compiling until T10 rewrites `app.rs`
- Modify: `crates/erars-renderer/src/headless.rs` — line 12 and line 90 (legacy call) — same reason, T9 rewrites this file
- Modify: `crates/erars-renderer/Cargo.toml` — `+swash = "0.1.18"` after `etagere = "0.2"` (line 26 after T5's edit)
- Test: `#[cfg(test)] mod tests` inside `raster.rs` and `draw.rs`

**Interfaces:**
- Consumes (T5, `crates/erars-renderer/src/flags.rs`): `bitflags! { pub struct RasterFlags: u8 { const BOLD_SYNTH = 1; const ITALIC_SYNTH = 2; } }` deriving `Clone, Copy, Debug, Default, PartialEq, Eq, Hash` (`RasterKey` is `Hash + Eq` over it).
- Consumes (T5, `crate::font`): `FontChain::from_files(files: &[PathBuf], lang: Language) -> Self`, `FontChain::primary(&self) -> fontdb::ID`, `FontChain::font(&mut self, id: fontdb::ID) -> Arc<cosmic_text::Font>`.
- Consumes (T6, `crate::text`): `#[derive(Clone, Copy, Debug, PartialEq)] pub struct CellMetrics { pub scale: f32, pub font_px: u32, pub half_w: u32, pub line_h: u32, pub baseline: u32, pub shift: u32 }`, `CellMetrics::from_primary(font: &cosmic_text::Font, font_size: u32, line_height: u32, scale: f32) -> Self`, `#[derive(Clone, Copy, Debug, PartialEq)] pub struct ShapedGlyph { pub font: fontdb::ID, pub glyph: u16, pub dx: i32, pub dy: i32, pub size_px: f32, pub flags: RasterFlags }`, `Shaper::new(chain: FontChain, widths: WidthTable, m: CellMetrics) -> Self`, `Shaper::metrics(&self) -> &CellMetrics`, `Shaper::chain(&mut self) -> &mut FontChain`.
- Consumes (T7, `crate::layout`): `Layout { pub rows: Vec<Row>, pub buttons: Vec<ButtonRegion> }`, `Row { pub line: usize, pub logical_start: bool, pub x0: i32, pub width: u32, pub clusters: Vec<PlacedCluster>, pub rects: Vec<Rect> }`, `PlacedCluster { pub x: i32, pub cells: u8, pub text: SmolStr, pub color: [u8; 3], pub style: FontStyle, pub button: Option<usize>, pub glyphs: Arc<[ShapedGlyph]> }`, `RectKind { Underline, Strike }`, `Rect { pub kind: RectKind, pub x: i32, pub dy: i32, pub h: u32, pub w: u32, pub color: [u8; 3], pub button: Option<usize> }` (`x` row-relative before `x0`, `dy` from the row top), `ButtonRegion { pub row: usize, pub x: i32, pub w: u32, pub input_gen: u32, pub value: Value }`, `Geometry { pub content_w: u32, pub drawable_w: u32, pub m: CellMetrics }`, `layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper) -> Layout`.
- Consumes (T1/T2): `erars_ui::width::WidthTable::new(&'static encoding_rs::Encoding)`, `Language::encoding(&self) -> &'static encoding_rs::Encoding`.
- Consumes (crates, pinned): cosmic-text 0.12.1 `Font::{id, rustybuzz, as_swash}`, `cosmic_text::{fontdb, ttf_parser}` re-exports, `FontSystem::{new_with_locale_and_db, get_font}`; ttf-parser 0.21.1 `Face::glyph_raster_image(GlyphId, u16) -> Option<RasterGlyphImage { x: i16, y: i16, width: u16, height: u16, pixels_per_em: u16, format: RasterImageFormat, data: &[u8] }>`; swash 0.1.18 `ScaleContext::{new, builder}`, `ScalerBuilder::{size, hint, build}`, `Render::{new, format, embolden, transform, render}`, `Source::{ColorBitmap, ColorOutline, Outline}`, `StrikeWith::BestFit`, `scale::image::{Content, Image { content, placement, data }}`, `swash::zeno::{Angle::{from_degrees, ZERO}, Format::Alpha, Transform::skew, Placement { left, top, width, height }}`; etagere 0.2.13 `AtlasAllocator::{new, allocate}`, `size2`, `Allocation { rectangle: Box2D<i32>, id }`; wgpu 0.19.4 `RenderPass<'a>` lifetimes, `SamplerBindingType::Filtering` accepting a `Nearest` sampler.
- Produces (`crate::raster`): `pub const PAGE_SIZE: u32 = 2048`; `#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)] pub struct RasterKey { pub font: fontdb::ID, pub glyph: u16, pub size_bits: u32, pub flags: RasterFlags }` + `RasterKey::new(font: fontdb::ID, glyph: u16, size_px: f32, flags: RasterFlags) -> Self`, `RasterKey::size_px(&self) -> f32`; `#[derive(Clone, Copy, Debug, PartialEq)] pub struct AtlasRegion { pub page: usize, pub uv: [f32; 4], pub size: [u32; 2], pub left: i32, pub top: i32, pub color: bool }`; `pub struct StrikeMask { pub width: u32, pub height: u32, pub left: i32, pub top: i32, pub data: Vec<u8> }`; `pub fn decode_mono(data: &[u8], width: u32, height: u32, packed: bool) -> Option<Vec<u8>>`; `pub fn strike_mask(font: &cosmic_text::Font, glyph: u16, size_px: u32) -> Option<StrikeMask>` (T9 consumes this exact signature); `pub struct GlyphImage { pub width: u32, pub height: u32, pub left: i32, pub top: i32, pub color: bool, pub rgba: Vec<u8> }` + `GlyphImage::blank()`, `GlyphImage::is_empty(&self) -> bool`; `pub fn strike_image(font: &Font, glyph: u16, size_px: u32) -> Option<GlyphImage>`; `pub fn outline_image(ctx: &mut swash::scale::ScaleContext, font: &Font, glyph: u16, size_px: f32, flags: RasterFlags) -> Option<GlyphImage>`; `pub fn rasterize(ctx: &mut ScaleContext, font: &Font, key: RasterKey, use_bitmap_strikes: bool) -> Option<GlyphImage>`; `pub fn place(allocs: &mut Vec<etagere::AtlasAllocator>, w: u32, h: u32) -> Option<(usize, u32, u32)>`; `pub struct GlyphRaster` with `GlyphRaster::new(device: &wgpu::Device, use_bitmap_strikes: bool) -> Self`, `GlyphRaster::get(&mut self, device: &wgpu::Device, queue: &wgpu::Queue, font: &cosmic_text::Font, key: RasterKey) -> Option<AtlasRegion>`, `GlyphRaster::lookup(&self, key: &RasterKey) -> Option<Option<AtlasRegion>>`, `GlyphRaster::use_bitmap_strikes(&self) -> bool`, `GlyphRaster::page_count(&self) -> usize`, `GlyphRaster::page_view(&self, page: usize) -> &wgpu::TextureView`, `GlyphRaster::page_views(&self) -> Vec<&wgpu::TextureView>`, `GlyphRaster::pages_with<'a>(&'a self, buckets: &'a [Vec<Instance>]) -> Vec<(&'a wgpu::TextureView, &'a [Instance])>`.
- Produces (`crate::draw`): `#[derive(Clone, Copy, Debug, PartialEq, Eq)] pub struct View { pub scroll_rows: usize, pub view_h: u32, pub strip_h: u32 }` + `View::visible_rows(&self, line_h: u32) -> usize`, `View::row_y(&self, rows: usize, r: usize, line_h: u32) -> Option<i32>`, `View::strip(&self) -> View`; `pub trait RegionSource { fn page_count(&self) -> usize; fn region(&mut self, glyph: &ShapedGlyph) -> Option<AtlasRegion>; }`; `pub struct GpuRegions<'a> { pub raster: &'a mut GlyphRaster, pub device: &'a wgpu::Device, pub queue: &'a wgpu::Queue, pub shaper: &'a mut Shaper }` (implements `RegionSource`); `pub fn build_instances(layout: &Layout, view: &View, hover: Option<usize>, hl: [u8; 3], raster: &mut GlyphRaster, device: &wgpu::Device, queue: &wgpu::Queue, shaper: &mut Shaper) -> Vec<Vec<Instance>>`; `pub fn build_instances_with(layout: &Layout, view: &View, hover: Option<usize>, hl: [u8; 3], m: &CellMetrics, src: &mut dyn RegionSource) -> Vec<Vec<Instance>>`.
- Produces (`crate::gpu`): `Instance` now derives `Debug, PartialEq` (still `Clone, Copy, Pod, Zeroable`); `pub fn nearest_sampler(device: &wgpu::Device) -> wgpu::Sampler`; `pub struct FrameDraw` with `FrameDraw::new(device: &wgpu::Device, layout: &wgpu::BindGroupLayout, globals: &wgpu::Buffer, sampler: &wgpu::Sampler, pages: &[(&wgpu::TextureView, &[Instance])]) -> FrameDraw`, `FrameDraw::draw<'a>(&'a self, pass: &mut wgpu::RenderPass<'a>, pipeline: &'a wgpu::RenderPipeline)`; `GpuContext::render(&mut self, pages: &[(&wgpu::TextureView, &[Instance])], bg: [u8; 3])`. `create_quad_pipeline` and `Globals` are unchanged.
- Produces (`crate::test_support`): `pub fn test_name() -> String`, `pub fn gpu_device() -> Option<(wgpu::Device, wgpu::Queue)>` (prints `SKIP <test>: no wgpu adapter`; panics under `ERARS_REQUIRE_GPU=1`) — the exact signatures T9's rewrite of the file keeps.

Call shape for T9/T10: `let buckets = build_instances(&layout, &view, hovered, hl, &mut raster, &device, &queue, &mut shaper); gpu.render(&raster.pages_with(&buckets), bg);` — `buckets[p]` samples atlas page `p`; the input strip is a one-row `Layout` built with `View { scroll_rows: 0, view_h: window_h, strip_h: 0 }` (or `view.strip()`) and its buckets are merged page-wise into the frame's buckets before `pages_with`. `raster` and `gpu` are separate fields, so `&raster.pages_with(..)` (shared borrow) and `gpu.render(..)` (mutable) coexist.

- [ ] **Step 1: Add the swash dependency.** In `crates/erars-renderer/Cargo.toml`, directly after the line `etagere = "0.2"`:

```diff
 etagere = "0.2"
+swash = "0.1.18"
```

(cosmic-text's default `swash` feature stays on — `Font::as_swash()` needs it — so `Cargo.lock` already holds swash 0.1.18; only the `erars-renderer` entry of the lock gains `"swash"`.) Run `cd /home/riey/repos/erars && cargo metadata --offline -q >/dev/null && echo resolved` — expected: `resolved`.

- [ ] **Step 2: Declare the module.** Open `crates/erars-renderer/src/main.rs`. After T5, T6 and T7 the module list reads (`flags`, `font`, `headless`, `layout`, `text` carry `#[allow(dead_code)]`):

```rust
mod app;
mod atlas;
mod draw;
#[allow(dead_code)] // RasterFlags is consumed by the shaper/raster rewrite (T6/T8)
mod flags;
#[allow(dead_code)] // FontChain is wired into the app in T10; FontCtx stays until then
mod font;
mod gpu;
mod grid;
#[allow(dead_code)]
mod headless;
#[allow(dead_code)] // wired into app.rs / headless.rs by T10
mod layout;
#[cfg(test)]
mod test_support;
#[allow(dead_code)] // Shaper/CellMetrics are consumed by layout (T7) and the app (T10)
mod text;
```

Change `mod draw;` and add `mod raster;` so the list becomes:

```rust
mod app;
mod atlas;
#[allow(dead_code)] // the Layout-based build_instances is wired into app.rs/headless.rs in T9/T10
mod draw;
#[allow(dead_code)] // RasterFlags is consumed by the shaper/raster rewrite (T6/T8)
mod flags;
#[allow(dead_code)] // FontChain is wired into the app in T10; FontCtx stays until then
mod font;
mod gpu;
mod grid;
#[allow(dead_code)]
mod headless;
#[allow(dead_code)]
mod layout;
#[allow(dead_code)] // GlyphRaster replaces atlas.rs in T10
mod raster;
#[cfg(test)]
mod test_support;
#[allow(dead_code)] // Shaper/CellMetrics are consumed by layout (T7) and the app (T10)
mod text;
```

(If the exact comments on `flags`/`font`/`layout`/`text` differ from the above, leave those lines — including their `#[allow(dead_code)]` attributes — as they are; only the `draw` attribute and the two `raster` lines are this task's.) Run `cargo build -p erars-renderer 2>&1 | grep -m1 'E0583'` — expected: `error[E0583]: file not found for module `raster`` (the file is created in Step 4; cargo's own last line is only `error: could not compile …`).

- [ ] **Step 3: Add `test_name` and `gpu_device` to `test_support.rs`.** Append to `crates/erars-renderer/src/test_support.rs` (after `gpu_lock`, which ends at line 13):

```rust

/// The running test's name (cargo names each test thread after the test).
pub fn test_name() -> String {
    std::thread::current()
        .name()
        .unwrap_or("<unnamed test>")
        .to_string()
}

/// A headless wgpu device, or `None` after printing `SKIP <test>: no wgpu
/// adapter` on stderr. With `ERARS_REQUIRE_GPU=1` (CI with lavapipe) the
/// missing adapter is a test failure instead of a skip.
pub fn gpu_device() -> Option<(wgpu::Device, wgpu::Queue)> {
    let instance = wgpu::Instance::default();
    let adapter =
        pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions::default()));
    let device = adapter.and_then(|adapter| {
        pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                label: Some("erars-test"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::downlevel_defaults(),
            },
            None,
        ))
        .ok()
    });
    if device.is_none() {
        let name = test_name();
        if std::env::var_os("ERARS_REQUIRE_GPU").is_some_and(|v| v == "1") {
            panic!("{name}: ERARS_REQUIRE_GPU=1 but no wgpu adapter is available");
        }
        eprintln!("SKIP {name}: no wgpu adapter");
    }
    device
}
```

(`Limits::downlevel_defaults()` — `max_texture_dimension_2d = 2048` — matches `GpuContext::new` so the 2048² atlas pages are valid on the same limits the window uses.)

- [ ] **Step 4: Create `raster.rs` with the data types and the first (failing) tests.** Write `crates/erars-renderer/src/raster.rs`:

```rust
//! Glyph rasterization and the multi-page glyph atlas (spec Component 6).
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
//! Atlas pages are `PAGE_SIZE`² `Rgba8Unorm` textures: mask glyphs are stored
//! white with coverage in alpha, colour glyphs as straight RGBA. A full page
//! spawns a new one; `draw.rs` buckets instances per page. Glyph quads are
//! placed on integer pixels and sampled with `FilterMode::Nearest`.

use std::collections::HashMap;

use cosmic_text::{fontdb, ttf_parser, Font};
use etagere::{size2, AtlasAllocator};
use swash::scale::image::Content;
use swash::scale::{Render, ScaleContext, Source, StrikeWith};
use swash::zeno::{Angle, Format, Transform};

use crate::flags::RasterFlags;
use crate::gpu::Instance;

/// Side length of one atlas page in texels (= `Limits::downlevel_defaults().max_texture_dimension_2d`).
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
        let mut rgba = vec![0u8; (width * height * 4) as usize];
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
        assert_eq!(k, RasterKey::new(fontdb::ID::dummy(), 7, 15.0, RasterFlags::BOLD_SYNTH));
        assert_ne!(k, RasterKey::new(fontdb::ID::dummy(), 7, 18.0, RasterFlags::BOLD_SYNTH));
    }
}
```

Run `cargo test -p erars-renderer raster::` — expected failure: `error[E0425]: cannot find function `decode_mono` in this scope` and `error[E0425]: cannot find function `place` in this scope`.

- [ ] **Step 5: Implement `decode_mono` and `place`.** Insert into `raster.rs` directly before `#[cfg(test)]`:

```rust
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

fn new_allocator() -> AtlasAllocator {
    AtlasAllocator::new(size2(PAGE_SIZE as i32, PAGE_SIZE as i32))
}

/// Find room for a `w × h` image plus a 1 px gutter on an existing page, or on
/// a page appended to `allocs`. Returns `(page, x, y)`; `None` when the image
/// cannot fit on any page (`w + 1 > PAGE_SIZE` or `h + 1 > PAGE_SIZE`).
pub fn place(allocs: &mut Vec<AtlasAllocator>, w: u32, h: u32) -> Option<(usize, u32, u32)> {
    if w + 1 > PAGE_SIZE || h + 1 > PAGE_SIZE {
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
    Some((allocs.len() - 1, a.rectangle.min.x as u32, a.rectangle.min.y as u32))
}
```

Run `cargo test -p erars-renderer raster::` — expected: `test raster::tests::decode_mono_packed_rows ... ok`, `decode_mono_byte_padded_rows ... ok`, `decode_mono_rejects_short_data ... ok`, `place_opens_a_new_page_when_full ... ok`, `raster_key_round_trips_size ... ok` — `5 passed` (unused-import warnings for `Content`, `Render`, `HashMap`, … are expected until Steps 7 and 9).

- [ ] **Step 6: Add the raster-path tests (failing).** Append inside `mod tests` of `raster.rs` (after `raster_key_round_trips_size`):

```rust
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
    /// `SKIP` line and returns `None` otherwise.
    fn msgothic() -> Option<PathBuf> {
        let found = std::env::var_os("ERARS_FONT_DIR").and_then(|dir| {
            std::fs::read_dir(dir).ok()?.flatten().map(|e| e.path()).find(|p| {
                p.file_name()
                    .and_then(|n| n.to_str())
                    .is_some_and(|n| n.eq_ignore_ascii_case("msgothic.ttc"))
            })
        });
        if found.is_none() {
            eprintln!(
                "SKIP {}: ERARS_FONT_DIR does not contain msgothic.ttc",
                crate::test_support::test_name()
            );
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
        assert!(a.top > 0 && a.top <= 18, "top {} is above the baseline, inside the em", a.top);
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
        assert!(bold.width > plain.width, "embolden widens: {} vs {}", bold.width, plain.width);
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
        assert!(has_grey(&img), "no strike → hinted outline even with strikes enabled");
    }

    /// GPU-free companion of the spec's Testing §5 strike tests: the raw
    /// ttf-parser result for `あ` at 18 ppem, the nearest-strike behaviour at
    /// 23 ppem, and the decoded masks.
    #[test]
    fn strike_mask_decodes_ms_gothic_at_18px() {
        let Some(path) = msgothic() else {
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

        assert!(strike_mask(&font, a.0, 23).is_none(), "the 22 ppem strike must be rejected for 23 px");
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
        let Some(path) = msgothic() else {
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
```

Run `cargo test -p erars-renderer raster::` — expected failure: `error[E0425]: cannot find function `outline_image``, `strike_mask`, `strike_image`, `rasterize`.

- [ ] **Step 7: Implement the two raster paths.** Insert into `raster.rs` after `place` (before `#[cfg(test)]`):

```rust
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
    Some(GlyphImage::from_mask(m.width, m.height, m.left, m.top, &m.data))
}

/// swash sources for path 2, in priority order. `Source::Bitmap` is deliberately absent.
const SOURCES: [Source; 3] = [
    Source::ColorBitmap(StrikeWith::BestFit),
    Source::ColorOutline(0),
    Source::Outline,
];

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
    let mut scaler = ctx.builder(font.as_swash()).size(size_px).hint(true).build();
    let mut render = Render::new(&SOURCES);
    render.format(Format::Alpha);
    if flags.contains(RasterFlags::BOLD_SYNTH) {
        render.embolden(size_px / 24.0);
    }
    if flags.contains(RasterFlags::ITALIC_SYNTH) {
        render.transform(Some(Transform::skew(Angle::from_degrees(12.0), Angle::ZERO)));
    }
    let image = render.render(&mut scaler, glyph)?;
    let p = image.placement;
    if p.width == 0 || p.height == 0 {
        return Some(GlyphImage::blank());
    }
    let n = (p.width * p.height) as usize;
    let out = match image.content {
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
```

Run `cargo test -p erars-renderer raster::` — expected: the 5 earlier tests plus `outline_image_renders_bundled_a_antialiased ... ok`, `outline_image_applies_synthetic_styles ... ok`, `bundled_font_has_no_strikes_so_rasterize_uses_outlines ... ok`; `strike_mask_decodes_ms_gothic_at_18px` and `rasterize_prefers_strikes_only_when_allowed` print `SKIP raster::tests::…: ERARS_FONT_DIR does not contain msgothic.ttc` and pass — `10 passed`. Then `ERARS_FONT_DIR=/home/riey/repos/erars cargo test -p erars-renderer raster:: -- --nocapture` — expected: the two MS Gothic tests run their assertions and pass (no SKIP line for them; `msgothic.ttc` sits untracked at the repo root).

- [ ] **Step 8: Add the `GlyphRaster` GPU test (failing).** Append inside `mod tests` of `raster.rs`:

```rust
    #[test]
    fn get_uploads_a_from_the_bundled_font() {
        let _gpu = crate::test_support::gpu_lock();
        let Some((device, queue)) = crate::test_support::gpu_device() else {
            return;
        };
        let font = load_face(Path::new(BUNDLED), 0);
        let mut raster = GlyphRaster::new(&device, true);
        assert!(raster.use_bitmap_strikes());
        assert_eq!(raster.page_count(), 1);
        let key = RasterKey::new(font.id(), gid(&font, 'A'), 18.0, RasterFlags::empty());
        assert_eq!(raster.lookup(&key), None, "nothing cached yet");
        let region = raster.get(&device, &queue, &font, key).expect("'A' rasterizes");
        assert_eq!(raster.lookup(&key), Some(Some(region)));
        assert_eq!(region.page, 0);
        assert!(!region.color);
        assert!(region.size[0] > 0 && region.size[1] > 0);
        assert!(region.uv.iter().all(|v| (0.0..=1.0).contains(v)));
        assert_eq!(
            raster.get(&device, &queue, &font, key),
            Some(region),
            "second lookup is served from the map"
        );
        let space = RasterKey::new(font.id(), gid(&font, ' '), 18.0, RasterFlags::empty());
        assert_eq!(raster.get(&device, &queue, &font, space), None, "blank glyphs take no atlas space");
        assert_eq!(raster.lookup(&space), Some(None), "…but the blank result is cached");
        let bold = RasterKey::new(font.id(), gid(&font, 'A'), 18.0, RasterFlags::BOLD_SYNTH);
        let b = raster.get(&device, &queue, &font, bold).unwrap();
        assert_ne!(b.uv, region.uv, "a different key gets its own region");
        assert!(b.size[0] > region.size[0]);
        assert_eq!(raster.page_views().len(), 1);
        let buckets = vec![vec![Instance {
            rect: [0.0; 4],
            uv: region.uv,
            color: [1.0; 4],
            mode: 1,
            _pad: [0; 3],
        }]];
        let pages = raster.pages_with(&buckets);
        assert_eq!(pages.len(), 1);
        assert_eq!(pages[0].1.len(), 1);
    }
```

Run `cargo test -p erars-renderer raster::get_uploads` — expected failure: `error[E0433]: failed to resolve: use of undeclared type `GlyphRaster``.

- [ ] **Step 9: Implement `GlyphRaster` (pages, map, upload).** Insert into `raster.rs` after `rasterize` (before `#[cfg(test)]`):

```rust
struct PageTexture {
    texture: wgpu::Texture,
    view: wgpu::TextureView,
}

fn create_page(device: &wgpu::Device, index: usize) -> PageTexture {
    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some(&format!("glyph-atlas-{index}")),
        size: wgpu::Extent3d {
            width: PAGE_SIZE,
            height: PAGE_SIZE,
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
    PageTexture { texture, view }
}

/// Rasterizes glyphs on demand and keeps them in a multi-page atlas.
pub struct GlyphRaster {
    ctx: ScaleContext,
    /// One allocator per page; `allocs.len() == pages.len()` between calls.
    allocs: Vec<AtlasAllocator>,
    pages: Vec<PageTexture>,
    map: HashMap<RasterKey, Option<AtlasRegion>>,
    use_bitmap_strikes: bool,
    warned_oversize: bool,
}

impl GlyphRaster {
    /// Creates page 0. `use_bitmap_strikes = false` is the `--no-bitmap-strikes` CLI flag.
    pub fn new(device: &wgpu::Device, use_bitmap_strikes: bool) -> Self {
        Self {
            ctx: ScaleContext::new(),
            allocs: vec![new_allocator()],
            pages: vec![create_page(device, 0)],
            map: HashMap::new(),
            use_bitmap_strikes,
            warned_oversize: false,
        }
    }

    pub fn use_bitmap_strikes(&self) -> bool {
        self.use_bitmap_strikes
    }

    pub fn page_count(&self) -> usize {
        self.pages.len()
    }

    pub fn page_view(&self, page: usize) -> &wgpu::TextureView {
        &self.pages[page].view
    }

    /// Every page's texture view, in page order.
    pub fn page_views(&self) -> Vec<&wgpu::TextureView> {
        self.pages.iter().map(|p| &p.view).collect()
    }

    /// Pair every page's texture view with its instance bucket (the output of
    /// `draw::build_instances`), ready for `GpuContext::render` / `FrameDraw::new`.
    /// Buckets beyond the page count (none in practice) are dropped.
    pub fn pages_with<'a>(
        &'a self,
        buckets: &'a [Vec<Instance>],
    ) -> Vec<(&'a wgpu::TextureView, &'a [Instance])> {
        self.pages
            .iter()
            .zip(buckets)
            .map(|(p, b)| (&p.view, b.as_slice()))
            .collect()
    }

    /// Cached result for `key` without rasterizing: `None` = never seen,
    /// `Some(None)` = known blank / unrasterizable, `Some(Some(r))` = in the atlas.
    pub fn lookup(&self, key: &RasterKey) -> Option<Option<AtlasRegion>> {
        self.map.get(key).copied()
    }

    /// Region for `key`, rasterizing and uploading on first use. `None` for
    /// blank glyphs (space) and for glyphs that cannot be rasterized or fit.
    pub fn get(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        font: &Font,
        key: RasterKey,
    ) -> Option<AtlasRegion> {
        if let Some(cached) = self.map.get(&key) {
            return *cached;
        }
        let region = rasterize(&mut self.ctx, font, key, self.use_bitmap_strikes)
            .filter(|img| !img.is_empty())
            .and_then(|img| self.upload(device, queue, &img));
        self.map.insert(key, region);
        region
    }

    fn upload(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        img: &GlyphImage,
    ) -> Option<AtlasRegion> {
        let Some((page, x, y)) = place(&mut self.allocs, img.width, img.height) else {
            if !self.warned_oversize {
                self.warned_oversize = true;
                log::warn!(
                    "glyph image {}x{} exceeds an atlas page ({PAGE_SIZE}²); skipped",
                    img.width,
                    img.height
                );
            }
            return None;
        };
        while self.pages.len() <= page {
            let index = self.pages.len();
            self.pages.push(create_page(device, index));
        }
        queue.write_texture(
            wgpu::ImageCopyTexture {
                texture: &self.pages[page].texture,
                mip_level: 0,
                origin: wgpu::Origin3d { x, y, z: 0 },
                aspect: wgpu::TextureAspect::All,
            },
            &img.rgba,
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(img.width * 4),
                rows_per_image: Some(img.height),
            },
            wgpu::Extent3d {
                width: img.width,
                height: img.height,
                depth_or_array_layers: 1,
            },
        );
        let s = PAGE_SIZE as f32;
        Some(AtlasRegion {
            page,
            uv: [
                x as f32 / s,
                y as f32 / s,
                img.width as f32 / s,
                img.height as f32 / s,
            ],
            size: [img.width, img.height],
            left: img.left,
            top: img.top,
            color: img.color,
        })
    }
}
```

Run `cargo test -p erars-renderer raster::` — expected: `11 passed` (`get_uploads_a_from_the_bundled_font ... ok`, or `SKIP raster::tests::get_uploads_a_from_the_bundled_font: no wgpu adapter` + ok on a box without an adapter). Also `ERARS_REQUIRE_GPU=1 cargo test -p erars-renderer raster::get_uploads` — expected: `1 passed` on this box (NVIDIA/Vulkan).

- [ ] **Step 10: Commit the raster module.**

```
cd /home/riey/repos/erars && git add crates/erars-renderer/Cargo.toml Cargo.lock crates/erars-renderer/src/main.rs crates/erars-renderer/src/test_support.rs crates/erars-renderer/src/raster.rs && git commit -m "feat(renderer): glyph raster with embedded strikes and a multi-page atlas"
```

- [ ] **Step 11: `gpu.rs` — derive `Debug`/`PartialEq` on `Instance`, add `nearest_sampler` + `FrameDraw`.** Line 5 of `crates/erars-renderer/src/gpu.rs`:

```diff
-#[derive(Clone, Copy, Pod, Zeroable)]
+#[derive(Clone, Copy, Pod, Zeroable, Debug, PartialEq)]
 pub struct Instance {
```

Insert after the closing brace of `create_quad_pipeline` (line 106 today, before `pub struct GpuContext`):

```rust
/// The atlas sampler: glyphs are placed on integer pixels, so `Nearest`
/// reproduces bitmap strikes 1:1 and never blurs mask edges. (The bind-group
/// layout's `SamplerBindingType::Filtering` accepts any sampler in wgpu 0.19.)
pub fn nearest_sampler(device: &wgpu::Device) -> wgpu::Sampler {
    device.create_sampler(&wgpu::SamplerDescriptor {
        label: Some("atlas-sampler"),
        mag_filter: wgpu::FilterMode::Nearest,
        min_filter: wgpu::FilterMode::Nearest,
        mipmap_filter: wgpu::FilterMode::Nearest,
        ..Default::default()
    })
}

/// GPU resources for one frame's per-page instance lists: a bind group and a
/// vertex buffer per non-empty page. Built *before* the render pass because a
/// wgpu 0.19 `RenderPass<'a>` borrows every resource it uses for `'a`.
/// Shared by [`GpuContext::render`] and the headless renderer.
pub struct FrameDraw {
    pages: Vec<(wgpu::BindGroup, wgpu::Buffer, u32)>,
}

impl FrameDraw {
    pub fn new(
        device: &wgpu::Device,
        layout: &wgpu::BindGroupLayout,
        globals: &wgpu::Buffer,
        sampler: &wgpu::Sampler,
        pages: &[(&wgpu::TextureView, &[Instance])],
    ) -> Self {
        let mut out = Vec::with_capacity(pages.len());
        for (view, instances) in pages {
            if instances.is_empty() {
                continue;
            }
            let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("atlas-page"),
                layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: globals.as_entire_binding(),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::TextureView(view),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: wgpu::BindingResource::Sampler(sampler),
                    },
                ],
            });
            let buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("instances"),
                contents: bytemuck::cast_slice(instances),
                usage: wgpu::BufferUsages::VERTEX,
            });
            out.push((bind_group, buffer, instances.len() as u32));
        }
        Self { pages: out }
    }

    /// One `draw(0..6, 0..n)` per page, in page order.
    pub fn draw<'a>(&'a self, pass: &mut wgpu::RenderPass<'a>, pipeline: &'a wgpu::RenderPipeline) {
        if self.pages.is_empty() {
            return;
        }
        pass.set_pipeline(pipeline);
        for (bind_group, buffer, count) in &self.pages {
            pass.set_bind_group(0, bind_group, &[]);
            pass.set_vertex_buffer(0, buffer.slice(..));
            pass.draw(0..6, 0..*count);
        }
    }
}
```

Run `cargo build -p erars-renderer 2>&1 | tail -1` — expected: `Finished` (warnings only: `FrameDraw`/`nearest_sampler` unused until the next step).

- [ ] **Step 12: `gpu.rs` — Nearest sampler in `GpuContext::new` and per-page `render`.** Replace lines 176–180 of `gpu.rs`:

```rust
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });
```

with:

```rust
        let sampler = nearest_sampler(&device);
```

Replace the whole `render` method (lines 212–293: from the doc comment `/// Render one frame: clear to `bg`, draw `instances` against `atlas_view`.` through the method's closing brace, the last item inside `impl GpuContext`) with:

```rust
    /// Render one frame: clear to `bg`, then draw every `(atlas page view,
    /// instances)` pair with its own bind group — one draw per page.
    pub fn render(&mut self, pages: &[(&wgpu::TextureView, &[Instance])], bg: [u8; 3]) {
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

        let draw = FrameDraw::new(
            &self.device,
            &self.bind_group_layout,
            &self.globals_buf,
            &self.sampler,
            pages,
        );

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
                            r: srgb_to_linear(bg[0]),
                            g: srgb_to_linear(bg[1]),
                            b: srgb_to_linear(bg[2]),
                            a: 1.0,
                        }),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            draw.draw(&mut pass, &self.pipeline);
        }
        self.queue.submit(Some(encoder.finish()));
        frame.present();
    }
```

Then patch the single caller, `crates/erars-renderer/src/app.rs` line 168:

```diff
-        gpu.render(&atlas.view, &instances, bg);
+        gpu.render(&[(&atlas.view, instances.as_slice())], bg);
```

Run `cargo build -p erars-renderer 2>&1 | tail -1` — expected: `Finished`. Run `cargo test -p erars-renderer atlas::` — expected: `rasterizes_a_glyph ... ok` (the old atlas test still passes; T10 removes it).

- [ ] **Step 13: Commit the gpu changes.**

```
cd /home/riey/repos/erars && git add crates/erars-renderer/src/gpu.rs crates/erars-renderer/src/app.rs && git commit -m "feat(renderer): per-page atlas draws with a nearest sampler"
```

- [ ] **Step 14: `draw.rs` — keep the old path under a legacy name.** In `crates/erars-renderer/src/draw.rs`: rename `pub fn build_instances(` (line 9) to `pub fn build_instances_legacy(` and put the line `// Legacy Grid path (still used by app.rs/headless.rs); deleted in T10 with grid.rs/atlas.rs.` directly above its doc comment (line 7); delete the old `#[cfg(test)] mod tests` block (lines 47–90 — it tested the `Grid` path). Patch the two callers: `crates/erars-renderer/src/app.rs` line 15 `use crate::draw::build_instances;` → `use crate::draw::build_instances_legacy;` and line 159 `let instances = build_instances(` → `let instances = build_instances_legacy(`; `crates/erars-renderer/src/headless.rs` line 12 `use crate::draw::build_instances;` → `use crate::draw::build_instances_legacy;` and line 90 `let instances = build_instances(` → `let instances = build_instances_legacy(`. Run `cargo build -p erars-renderer 2>&1 | tail -1` — expected: `Finished`.

- [ ] **Step 15: `draw.rs` — new module header, `View`, `RegionSource`, and the failing GPU-free tests.** Replace the top of `draw.rs` (the five import lines, lines 1–5) with:

```rust
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

use cosmic_text::{FontSystem, SwashCache};

use crate::atlas::GlyphAtlas;
use crate::gpu::Instance;
use crate::grid::Grid;
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
```

Append at the end of `draw.rs`:

```rust
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
                ButtonRegion { row: 0, x: 9, w: 9, input_gen: 1, value: Value::Int(1) },
                ButtonRegion { row: 1, x: 0, w: 9, input_gen: 1, value: Value::Int(2) },
            ],
        }
    }

    fn flat(pages: &[Vec<Instance>]) -> Vec<Instance> {
        pages.iter().flatten().copied().collect()
    }

    fn rgb(c: [u8; 3]) -> [f32; 4] {
        [c[0] as f32 / 255.0, c[1] as f32 / 255.0, c[2] as f32 / 255.0, 1.0]
    }

    #[test]
    fn view_rows_are_bottom_anchored() {
        let v = View { scroll_rows: 0, view_h: 38, strip_h: 19 };
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
        let v = View { scroll_rows: 1, view_h: 38, strip_h: 19 };
        assert_eq!(v.row_y(3, 2, 19), None, "the newest row is scrolled out");
        assert_eq!(v.row_y(3, 1, 19), Some(19));
        assert_eq!(v.row_y(3, 0, 19), Some(0));
        let clamped = View { scroll_rows: 99, view_h: 38, strip_h: 19 };
        assert_eq!(clamped.row_y(3, 0, 19), Some(19), "scroll_rows is clamped to rows − 1");
    }

    #[test]
    fn view_strip_places_one_row_below_the_row_area() {
        let v = View { scroll_rows: 0, view_h: 38, strip_h: 19 };
        assert_eq!(v.strip(), View { scroll_rows: 0, view_h: 57, strip_h: 0 });
        assert_eq!(v.strip().row_y(1, 0, 19), Some(38));
    }

    #[test]
    fn quads_use_the_spec_origin_formula() {
        let view = View { scroll_rows: 0, view_h: 38, strip_h: 19 };
        let mut src = FakeRegions { pages: 1 };
        let pages = build_instances_with(&fake_layout(), &view, None, HL, &metrics(), &mut src);
        assert_eq!(pages.len(), 1);
        let inst = &pages[0];
        assert_eq!(inst.len(), 4, "underline, a, b, c");
        // Row 0 rect first (mode 0): shift + x0 + x = 3 + 9, row_y + dy = 0 + 16.
        assert_eq!(
            inst[0],
            Instance { rect: [12.0, 16.0, 9.0, 1.0], uv: [0.0; 4], color: rgb(WHITE), mode: 0, _pad: [0; 3] }
        );
        // `a`: shift + x0 + x + dx + left = 3, row_y + dy − top = 0 + 15 − 15 = 0.
        assert_eq!(inst[1].rect, [3.0, 0.0, 9.0, 18.0]);
        assert_eq!(inst[1].mode, 1);
        assert_eq!(inst[2].rect, [12.0, 0.0, 9.0, 18.0]);
        // `c` on row 1: y = 19.
        assert_eq!(inst[3].rect, [3.0, 19.0, 9.0, 18.0]);
        assert!(inst.iter().all(|i| i.color == rgb(WHITE)));
    }

    #[test]
    fn hover_recolours_exactly_the_hovered_button() {
        let view = View { scroll_rows: 0, view_h: 38, strip_h: 19 };
        let layout = fake_layout();
        let mut src = FakeRegions { pages: 1 };
        let plain = flat(&build_instances_with(&layout, &view, None, HL, &metrics(), &mut src));
        let hover0 = flat(&build_instances_with(&layout, &view, Some(0), HL, &metrics(), &mut src));
        let hover1 = flat(&build_instances_with(&layout, &view, Some(1), HL, &metrics(), &mut src));
        assert_eq!(plain.len(), hover0.len());
        for (p, h) in plain.iter().zip(&hover0) {
            assert_eq!((p.rect, p.uv, p.mode), (h.rect, h.uv, h.mode), "nothing moves");
        }
        let colors = |v: &[Instance]| v.iter().map(|i| i.color).collect::<Vec<_>>();
        assert_eq!(colors(&plain), vec![rgb(WHITE); 4]);
        assert_eq!(colors(&hover0), vec![rgb(HL), rgb(WHITE), rgb(HL), rgb(WHITE)], "underline + b");
        assert_eq!(colors(&hover1), vec![rgb(WHITE), rgb(WHITE), rgb(WHITE), rgb(HL)], "c only");
    }

    #[test]
    fn instances_are_bucketed_per_page_with_rects_on_page_zero() {
        let view = View { scroll_rows: 0, view_h: 38, strip_h: 19 };
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
        let view = View { scroll_rows: 0, view_h: 19, strip_h: 19 };
        let mut src = FakeRegions { pages: 1 };
        let only_last = flat(&build_instances_with(&fake_layout(), &view, None, HL, &metrics(), &mut src));
        assert_eq!(only_last.len(), 1, "one visible row: `c`");
        assert_eq!(only_last[0].rect, [3.0, 0.0, 9.0, 18.0]);
        let view = View { scroll_rows: 1, view_h: 19, strip_h: 19 };
        let first = flat(&build_instances_with(&fake_layout(), &view, None, HL, &metrics(), &mut src));
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
        let view = View { scroll_rows: 0, view_h: 38, strip_h: 19 };
        let pages = build_instances_with(&fake_layout(), &view, None, HL, &metrics(), &mut Growing);
        assert_eq!(pages.len(), 2);
        assert_eq!(pages[1].len(), 1);
    }
}
```

Run `cargo test -p erars-renderer draw::` — expected failure: `error[E0425]: cannot find function `build_instances_with` in this scope`.

- [ ] **Step 16: Implement `build_instances` / `build_instances_with`.** Insert into `draw.rs` between the `impl RegionSource for GpuRegions<'_>` block and the `// Legacy Grid path …` comment:

```rust
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
            let color = if hover.is_some() && rect.button == hover { hl } else { rect.color };
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
            let color = if hover.is_some() && c.button == hover { hl } else { c.color };
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
```

Run `cargo test -p erars-renderer draw::` — expected: `8 passed` (`view_rows_are_bottom_anchored`, `view_scroll_rows_hides_the_bottom`, `view_strip_places_one_row_below_the_row_area`, `quads_use_the_spec_origin_formula`, `hover_recolours_exactly_the_hovered_button`, `instances_are_bucketed_per_page_with_rects_on_page_zero`, `scrolled_out_rows_produce_no_instances`, `a_page_created_mid_build_extends_the_buckets`).

- [ ] **Step 17: Add the end-to-end GPU smoke test in `draw.rs`.** Append inside `mod tests` (after `a_page_created_mid_build_extends_the_buckets`):

```rust
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
        let g = Geometry { content_w: 760, drawable_w: 760 - m.shift, m };
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
        let view = View { scroll_rows: 0, view_h: m.line_h, strip_h: 0 };
        let pages = build_instances(&laid, &view, None, HL, &mut raster, &device, &queue, &mut shaper);
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
            assert!(i.rect[1] >= 0.0 && i.rect[1] < m.line_h as f32, "quad top inside row 0");
        }
        let pen: Vec<f32> = inst.iter().map(|i| i.rect[0]).collect();
        assert!(pen[0] < pen[1] && pen[1] < pen[2]);
        assert_eq!(raster.pages_with(&pages).len(), pages.len());
    }
```

Run `cargo test -p erars-renderer draw::` — expected: `9 passed` (`build_instances_gpu_smoke ... ok`, or a `SKIP draw::tests::build_instances_gpu_smoke: no wgpu adapter` line + ok without an adapter). Run `cargo test -p erars-renderer 2>&1 | grep 'test result'` — expected: every suite `ok`, `0 failed` (`raster::`, `draw::`, plus T5–T7's suites; the old `atlas::`/`grid::`/`headless::` tests still pass through the legacy path).

- [ ] **Step 18: Commit the draw module.**

```
cd /home/riey/repos/erars && git add crates/erars-renderer/src/draw.rs crates/erars-renderer/src/app.rs crates/erars-renderer/src/headless.rs && git commit -m "feat(renderer): build_instances over Layout with draw-time hover and per-page buckets"
```

---

### Task 9: Headless + test support — `render_frame`, `write_png`, `gpu_device`, pixel tests

Spec: Component 7 (`headless.rs`, `test_support::gpu_device`, `ERARS_REQUIRE_CJK_FONT`), the View paragraph of Component 5 (`scroll_rows = 0`, bottom-anchored rows, `line_h`-tall input strip), Testing §5 (pixel tests). Everything below was compile-checked with bare `rustc` against the locked rlibs in `target/debug/deps` (`scratchpad/probe-plan-9/png2.rs`, `bbox.rs`, `tname.rs`; MS Gothic facts from `scratchpad/probe-plan-t9/ct.rs`, `adv.rs`).

**Files:**
- Modify `crates/erars-renderer/Cargo.toml` — `[dependencies]`: add `flate2`, `crc32fast` after the `sys-locale = "0.3"` line (T5 rewrote the block above it; locate by content).
- Modify `crates/erars-renderer/src/headless.rs` — full rewrite. Current file (after T8's rename patch): lines 1–15 header/imports (line 12 reads `use crate::draw::build_instances_legacy;`), 17–51 `Rendered` + `column_ink`/`ink_right_edge`, 53–218 `render_lines` (line 90 calls `build_instances_legacy`), 220–230 `write_ppm`, 232–362 `mod tests` (three `FontCtx`/`Grid` tests). Only `Rendered`, `column_ink`, `ink_right_edge` survive.
- Modify `crates/erars-renderer/src/test_support.rs` — full rewrite (today: `gpu_lock` from the original 13 lines plus T8's appended `test_name()` / `gpu_device()`; both keep their zero-argument signatures, so T8's callers in `raster.rs` / `draw.rs` need no change).
- Modify `crates/erars-proxy-system/src/lib.rs` lines 65–89 (`ConsoleFrame` + `from_vconsole`): add `fore_color`.
- Modify `crates/erars-renderer/src/main.rs` — three spots located by content (T5/T7/T8 shifted the line numbers): the clap attribute containing `value_name = "PATH.ppm"`, the whole `fn headless_shot(…)` (the function taking `mut font: font::FontCtx`), and the `if let Some(path) = args.headless_shot.clone() {` block in `main`. Minimal shim so the binary compiles once `render_lines`/`write_ppm` are gone; the shaper construction lives in `headless::shaper_for` (this task) so T10's rewritten `main.rs` can call it from the library.
- Test: unit tests in `crates/erars-renderer/src/headless.rs` (`#[cfg(test)] mod tests`); one unit test in `crates/erars-renderer/src/test_support.rs`.

**Interfaces:**
- Consumes (verbatim from the task map / spec / earlier sections):
  - T1 `erars_ui::width::WidthTable { new(&'static Encoding), char_cells(char)->u8, str_cells(&str)->usize }`
  - T2 `Language::encoding(&self)->&'static encoding_rs::Encoding`; existing `EraConfig.{font_family: String, font_size: u32, line_height: u32, lang: Language}`
  - T3 `VirtualConsole::default_color()->Color`
  - T5 `crate::font::FontConfig<'a> { family: &'a str, game_dir: &'a Path, extra_dir: Option<PathBuf>, lang: Language }`; `FontChain::new(&FontConfig)`; `FontChain::from_files(&[PathBuf], Language)`; `FontChain::primary(&self)->fontdb::ID`; `FontChain::font(&mut self, fontdb::ID)->Arc<cosmic_text::Font>`; `FontChain::resolve(&mut self, c: char, style: &StyleKey)->(fontdb::ID, RasterFlags)`; `StyleKey::plain()->StyleKey`
  - T6 `crate::text::CellMetrics { scale: f32, font_px: u32, half_w: u32, line_h: u32, baseline: u32, shift: u32 }` (derives `Clone, Copy, Debug, PartialEq`); `CellMetrics::from_primary(&Font, font_size: u32, line_height: u32, scale: f32)`; `crate::text::Shaper::{new(FontChain, WidthTable, CellMetrics), metrics(&self)->&CellMetrics, chain(&mut self)->&mut FontChain, set_metrics(CellMetrics)}`
  - T7 `crate::layout::{Geometry { content_w: u32, drawable_w: u32, m: CellMetrics }, Geometry::new(content_w: u32, m: CellMetrics)->Geometry, Layout { rows: Vec<Row>, buttons: Vec<ButtonRegion> }, Row { line: usize, logical_start: bool, x0: i32, width: u32, clusters: Vec<PlacedCluster>, rects: Vec<Rect> }, PlacedCluster { x: i32, cells: u8, text: SmolStr, .. }, ButtonRegion { row: usize, x: i32, w: u32, input_gen: u32, value: Value }, layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper)->Layout}`
  - T8 `crate::raster::{GlyphRaster::new(device: &wgpu::Device, use_bitmap_strikes: bool), GlyphRaster::page_count(&self)->usize, GlyphRaster::pages_with<'a>(&'a self, buckets: &'a [Vec<Instance>])->Vec<(&'a wgpu::TextureView, &'a [Instance])>, strike_image(font: &Font, glyph: u16, size_px: u32)->Option<GlyphImage>, GlyphImage { width: u32, height: u32, left: i32, top: i32, color: bool, rgba: Vec<u8> }}`; T8 `crate::test_support::{gpu_lock(), test_name(), gpu_device()}` (zero-argument; this task re-implements them with the same signatures); `crate::draw::{View { scroll_rows: usize, view_h: u32, strip_h: u32 }, View::strip(&self)->View, build_instances(layout: &Layout, view: &View, hover: Option<usize>, hl: [u8; 3], raster: &mut GlyphRaster, device: &wgpu::Device, queue: &wgpu::Queue, shaper: &mut Shaper)->Vec<Vec<Instance>>}`; `crate::gpu::{create_quad_pipeline(&wgpu::Device, wgpu::TextureFormat)->(RenderPipeline, BindGroupLayout), nearest_sampler(&wgpu::Device)->wgpu::Sampler, FrameDraw::new(device, layout: &BindGroupLayout, globals: &Buffer, sampler: &Sampler, pages: &[(&TextureView, &[Instance])])->FrameDraw, FrameDraw::draw<'a>(&'a self, pass: &mut RenderPass<'a>, pipeline: &'a RenderPipeline), Globals { screen: [f32; 2], _pad: [f32; 2] }, Instance}`
  - existing `erars_proxy_system::ConsoleFrame { bg_color: Color, hl_color: Color, lines: Vec<ConsoleLine> }` (`lib.rs:65-70`), `erars_ui::{Color(pub [u8;3]), ConsoleLine { align, button_start, parts }, ConsoleLinePart::{Text(String, TextStyle), Button(Vec<(String, TextStyle)>, u32, Value)}, TextStyle { color, font_family, font_style }, FontStyle::NORMAL}`, `erars_ast::{Alignment, Value::Int(i64)}`, `cosmic_text::ttf_parser` (re-exported: `font/mod.rs:5` + `lib.rs:120`), `cosmic_text::Font::rustybuzz()` (derefs to `ttf_parser::Face`: `glyph_index`, `glyph_raster_image(GlyphId, u16)->Option<RasterGlyphImage { x: i16, y: i16, width: u16, height: u16, pixels_per_em: u16, format: RasterImageFormat, data: &[u8] }>`, `units_per_em`, `glyph_hor_advance`)
- Produces (used by T10 `main.rs`/`app.rs`, T11 `tests/tui.rs`):
  - `erars_proxy_system::ConsoleFrame.fore_color: Color` (set from `VirtualConsole::default_color()` in `from_vconsole`; `Default` = black like the other colours)
  - `crate::headless::Rendered { width: u32, height: u32, rgba: Vec<u8> }` with `pixel(&self, x: u32, y: u32)->[u8; 4]`, `band(&self, y0: u32, y1: u32)->&[u8]`, `ink_columns(&self, y0: u32, y1: u32, min: u8)->Vec<bool>`, `ink_bbox(&self, x0: u32, x1: u32, y0: u32, y1: u32, min: u8)->Option<[u32; 4]>`, `column_ink(&self, y0: u32, y1: u32)->Vec<f32>`, `Rendered::ink_right_edge(prof: &[f32], threshold: f32)->usize`
  - `crate::headless::request_device()->Option<(wgpu::Device, wgpu::Queue)>`
  - `crate::headless::shaper_for(config: &EraConfig, game_dir: &Path)->Shaper` (fonts: configured family → `<game>/font` → `ERARS_FONT_DIR` → language list → bundled; metrics from the primary at scale 1.0; used by `main.rs` here and by T10's rewritten `main.rs`)
  - `crate::headless::render_frame(shaper: &mut Shaper, frame: &ConsoleFrame, content_w: u32, height: u32, input: Option<&str>, hover: Option<usize>)->Option<Rendered>` (own device, bitmap strikes on)
  - `crate::headless::render_frame_opts(shaper: &mut Shaper, frame: &ConsoleFrame, content_w: u32, height: u32, input: Option<&str>, hover: Option<usize>, use_bitmap_strikes: bool)->Option<Rendered>` (for `--no-bitmap-strikes --headless-shot`)
  - `crate::headless::render_frame_on(device: &wgpu::Device, queue: &wgpu::Queue, shaper: &mut Shaper, frame: &ConsoleFrame, content_w: u32, height: u32, input: Option<&str>, hover: Option<usize>, use_bitmap_strikes: bool)->Rendered` (tests share one device)
  - `crate::headless::encode_png(width: u32, height: u32, rgba: &[u8])->Vec<u8>`, `crate::headless::write_png(path: &str, img: &Rendered)->std::io::Result<()>`
  - `crate::test_support::{gpu_lock()->MutexGuard<'static, ()>, test_name()->String, gpu_device()->Option<(wgpu::Device, wgpu::Queue)>, BUNDLED_FONT_PATH: &str, bundled_font()->PathBuf, require_cjk_font()->Option<PathBuf>, msgothic_font()->Option<PathBuf>, test_shaper(files: &[PathBuf], lang: Language, font_size: u32, line_height: u32)->Shaper, style(color: [u8; 3])->TextStyle, text_line(s: &str, color: [u8; 3])->ConsoleLine, frame(lines: Vec<ConsoleLine>)->ConsoleFrame}`
  - `main.rs`: `fn headless_shot(shaper: text::Shaper, receiver: ProxyReceiver, (w, h): (u32, u32), path: &str)` (private shim; T10 rewrites `main.rs` with the same function)

Facts the steps rely on (measured on this box): the bundled `NotoSansMono-Regular.ttf` is upem 1000, ascender 1069, descender −293, every glyph advance 600 (`CellMetrics::from_primary(.., 18, 19, 1.0)` = `{1.0, 18, 11, 19, 19, 3}`), covers `┏━┓┃┗┛─═║α°→` but not `あ漢한★`; its box-drawing verticals (`┏ ┓ ┃ ║`) reach y −246 and `y g p` −245, i.e. ~4.4 px **below** the 19 px baseline at 18 px — glyphs of row *r* spill into row *r+1*'s band (spec Component 4: "its descenders overlap the next row", no clamping), so pixel assertions compare rows that have identical predecessors and let ink in row *r*'s band belong to a box of row *r* **or** *r−1*. Its `_` sits at −154..−74 (fully below the baseline → clipped when the baseline is the last strip row). `msgothic.ttc` face 0: upem 256, hhea 220/−36, all 32 JIS box characters advance 256, `═║A` 128; `glyph_raster_image(gid('あ'), 18)` → `pixels_per_em 18, 18×18, x 0, y −3, BitmapMonoPacked`, 61 set bits; requesting 23 returns the 22 ppem strike (20×20); `A`/`═` at 18 → 9×18 packed. libtest runs every test on a thread named after the test (`headless::tests::<name>`), so `std::thread::current().name()` is the test name. `cargo test` captures stdout/stderr of passing tests: the `SKIP` lines are visible only with `-- --nocapture`. `NotoSansCJK-Regular.ttc` (10 faces) and `Sarasa-Regular.ttc` (48 faces) are the CJK fonts installed here; `msgothic.ttc` sits untracked at the repo root (never commit it).

Decisions taken (also in open_questions): `gpu_device()` takes no argument (the task map / T8 / T10 / T11 contract — T8 already defines and calls it that way; this task only re-implements it on top of `headless::request_device`); the msgothic gate follows T8's convention `ERARS_FONT_DIR/msgothic.ttc` (a file path in `ERARS_FONT_DIR` is accepted too); headless clears the linear `Rgba8Unorm` target with `bg/255` directly (window bytes go through an sRGB surface; headless renders are only compared with each other and with exact 0/255 masks); the input strip is laid out as its own one-line `layout()` call drawn with `View::strip()`; `render_frame` creates its own device per call (the CLI calls it once).

- [ ] **Step 1: Add the PNG dependencies.**
  In `crates/erars-renderer/Cargo.toml`, directly after the `sys-locale = "0.3"` line:
  ```diff
   sys-locale = "0.3"
  +flate2 = "1.0"
  +crc32fast = "1.3"
  ```
  Both resolve to the versions already in `Cargo.lock` (flate2 1.0.28, crc32fast 1.3.2), so no network is needed. Run `cargo check -p erars-renderer`; expect `Finished` with no errors.

- [ ] **Step 2: Write the failing PNG tests.**
  In `crates/erars-renderer/src/headless.rs`, inside the existing `mod tests` (before its final `}`, line 362), add:
  ```rust
      fn checker(w: u32, h: u32) -> Vec<u8> {
          let mut rgba = vec![0u8; (w * h * 4) as usize];
          for y in 0..h {
              for x in 0..w {
                  let i = ((y * w + x) * 4) as usize;
                  rgba[i] = (x * 37) as u8;
                  rgba[i + 1] = (y * 91) as u8;
                  rgba[i + 2] = ((x + y) % 2 * 255) as u8;
                  rgba[i + 3] = 255;
              }
          }
          rgba
      }

      /// Signature, IHDR fields, exactly IHDR/IDAT/IEND, and a valid CRC-32
      /// (over type + data) on every chunk.
      #[test]
      fn png_chunks_are_well_formed() {
          let (w, h) = (7u32, 3u32);
          let png = encode_png(w, h, &checker(w, h));
          assert_eq!(&png[..8], &[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]);
          let mut pos = 8;
          let mut kinds = Vec::new();
          while pos < png.len() {
              let len = u32::from_be_bytes(png[pos..pos + 4].try_into().unwrap()) as usize;
              let kind = &png[pos + 4..pos + 8];
              let data = &png[pos + 8..pos + 8 + len];
              let crc = u32::from_be_bytes(png[pos + 8 + len..pos + 12 + len].try_into().unwrap());
              let mut hasher = crc32fast::Hasher::new();
              hasher.update(kind);
              hasher.update(data);
              assert_eq!(hasher.finalize(), crc, "bad CRC in {}", String::from_utf8_lossy(kind));
              if kind == b"IHDR" {
                  assert_eq!(len, 13);
                  assert_eq!(&data[..8], &[0, 0, 0, 7, 0, 0, 0, 3]);
                  assert_eq!(&data[8..], &[8, 6, 0, 0, 0]); // 8-bit RGBA, deflate, filter 0, no interlace
              }
              if kind == b"IEND" {
                  assert_eq!(len, 0);
              }
              kinds.push(String::from_utf8_lossy(kind).into_owned());
              pos += 12 + len;
          }
          assert_eq!(pos, png.len());
          assert_eq!(kinds, ["IHDR", "IDAT", "IEND"]);
      }

      /// The single IDAT inflates to `height` filter-0 scanlines of the input rows.
      #[test]
      fn png_idat_inflates_to_filter0_scanlines() {
          use std::io::Read;
          let (w, h) = (5u32, 4u32);
          let rgba = checker(w, h);
          let png = encode_png(w, h, &rgba);
          // 8 signature + 25 (IHDR chunk) = 33: IDAT length at 33, type at 37, data at 41.
          let len = u32::from_be_bytes(png[33..37].try_into().unwrap()) as usize;
          assert_eq!(&png[37..41], b"IDAT");
          let mut raw = Vec::new();
          flate2::read::ZlibDecoder::new(&png[41..41 + len])
              .read_to_end(&mut raw)
              .unwrap();
          let stride = 1 + (w * 4) as usize;
          assert_eq!(raw.len(), stride * h as usize);
          for (y, line) in raw.chunks_exact(stride).enumerate() {
              assert_eq!(line[0], 0, "scanline {y} filter byte");
              let row = &rgba[y * (w * 4) as usize..(y + 1) * (w * 4) as usize];
              assert_eq!(&line[1..], row, "scanline {y}");
          }
      }
  ```
  Run `cargo test -p erars-renderer headless::tests::png_`; expect the build to fail with `error[E0425]: cannot find function \`encode_png\` in this scope`.

- [ ] **Step 3: Implement `encode_png` / `write_png` (replacing `write_ppm`) and switch the CLI to PNG.**
  Replace lines 220–230 of `headless.rs` (`write_ppm`, incl. its doc comment) with:
  ```rust
  /// Append one PNG chunk: length, type, data, CRC-32 over type + data.
  fn png_chunk(out: &mut Vec<u8>, kind: &[u8; 4], data: &[u8]) {
      out.extend_from_slice(&(data.len() as u32).to_be_bytes());
      let mut hasher = crc32fast::Hasher::new();
      hasher.update(kind);
      hasher.update(data);
      out.extend_from_slice(kind);
      out.extend_from_slice(data);
      out.extend_from_slice(&hasher.finalize().to_be_bytes());
  }

  /// Encode an RGBA8 buffer as a minimal PNG: signature, IHDR, one IDAT holding
  /// the zlib stream of filter-0 scanlines, IEND. No `png` crate needed.
  pub fn encode_png(width: u32, height: u32, rgba: &[u8]) -> Vec<u8> {
      assert_eq!(rgba.len(), (width * height * 4) as usize, "rgba size");
      let mut out = Vec::with_capacity(rgba.len() / 4 + 64);
      out.extend_from_slice(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]);
      let mut ihdr = Vec::with_capacity(13);
      ihdr.extend_from_slice(&width.to_be_bytes());
      ihdr.extend_from_slice(&height.to_be_bytes());
      ihdr.extend_from_slice(&[8, 6, 0, 0, 0]); // bit depth 8, RGBA, deflate, filter 0, no interlace
      png_chunk(&mut out, b"IHDR", &ihdr);
      let stride = (width * 4) as usize;
      let mut enc = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::default());
      for row in rgba.chunks_exact(stride) {
          enc.write_all(&[0]).expect("in-memory zlib write");
          enc.write_all(row).expect("in-memory zlib write");
      }
      let idat = enc.finish().expect("in-memory zlib finish");
      png_chunk(&mut out, b"IDAT", &idat);
      png_chunk(&mut out, b"IEND", &[]);
      out
  }

  /// Write a render as PNG — viewable anywhere, small enough to `scp` back.
  pub fn write_png(path: &str, img: &Rendered) -> std::io::Result<()> {
      std::fs::write(path, encode_png(img.width, img.height, &img.rgba))
  }
  ```
  Add `use std::io::Write;` as the first `use` line of `headless.rs` (line 7, before `use cosmic_text::SwashCache;`). In `crates/erars-renderer/src/main.rs`, inside `fn headless_shot`, change `headless::write_ppm(path, &img)` to `headless::write_png(path, &img)`.
  Run `cargo test -p erars-renderer headless::tests::png_`; expect
  ```
  test headless::tests::png_chunks_are_well_formed ... ok
  test headless::tests::png_idat_inflates_to_filter0_scanlines ... ok
  test result: ok. 2 passed
  ```
  Commit: `git add crates/erars-renderer/Cargo.toml Cargo.lock crates/erars-renderer/src/headless.rs crates/erars-renderer/src/main.rs && git commit -m "feat(renderer): minimal in-crate PNG encoder for headless shots"`

- [ ] **Step 4: `ConsoleFrame` carries the console default colour.**
  The input strip is drawn "in the default colour" (spec Component 7) and `render_frame` receives only the frame, so the frame must carry it. Replace `crates/erars-proxy-system/src/lib.rs` lines 65–89 (`#[derive(Default, Debug, Clone)] pub struct ConsoleFrame { … }` through the closing `}` of `impl ConsoleFrame`) with:
  ```rust
  #[derive(Default, Debug, Clone)]
  pub struct ConsoleFrame {
      pub bg_color: Color,
      pub hl_color: Color,
      /// The configured default text colour (`文字色`); used for chrome the
      /// frontend draws itself, such as the input strip.
      pub fore_color: Color,
      pub lines: Vec<ConsoleLine>,
  }

  impl ConsoleFrame {
      pub fn from_vconsole(vconsole: &VirtualConsole) -> Self {
          Self {
              bg_color: vconsole.bg_color,
              hl_color: vconsole.hl_color,
              fore_color: vconsole.default_color(),
              lines: vconsole
                  .lines
                  .iter()
                  .chain(if vconsole.last_line.is_empty() {
                      None
                  } else {
                      Some(&vconsole.last_line)
                  })
                  .cloned()
                  .collect(),
          }
      }
  }
  ```
  Run `cargo check -p erars-proxy-system -p erars-renderer`; expect a clean check (`ConsoleFrame::default()` in `main.rs` and `app.rs` still works; T11's `ConsoleFrame { lines: …, ..frame }` picks the field up).
  Commit: `git add crates/erars-proxy-system/src/lib.rs && git commit -m "feat(proxy): carry the console default colour in ConsoleFrame"`

- [ ] **Step 5: `request_device` in `headless.rs`, `test_support` rewrite (`gpu_device()`, font gating, fixtures), patch T8's call sites.**
  Insert directly above the doc comment of `pub fn render_lines(` (`/// Render `lines` to an RGBA buffer …`; lines 54–56 of `headless.rs` after Step 3's `use std::io::Write;` line — locate by content):
  ```rust
  /// The default adapter/device without a surface. `None` when this machine has
  /// no wgpu adapter (tests skip or fail through `test_support::gpu_device`).
  pub fn request_device() -> Option<(wgpu::Device, wgpu::Queue)> {
      let instance = wgpu::Instance::default();
      let adapter =
          pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions::default()))?;
      pollster::block_on(adapter.request_device(
          &wgpu::DeviceDescriptor {
              label: Some("erars-headless"),
              required_features: wgpu::Features::empty(),
              required_limits: wgpu::Limits::downlevel_defaults(),
          },
          None,
      ))
      .ok()
  }
  ```
  Replace `crates/erars-renderer/src/test_support.rs` entirely with:
  ```rust
  //! Shared helpers for tests: GPU gating, font gating and console fixtures.
  //!
  //! Skips are loud: every gate prints `SKIP <test>: <reason>` on stderr, and
  //! `ERARS_REQUIRE_GPU=1` / `ERARS_REQUIRE_CJK_FONT=1` turn the corresponding
  //! skip into a panic so CI cannot pass by running nothing. (`cargo test`
  //! captures the output of passing tests — use `-- --nocapture` to see SKIPs.)

  use std::path::PathBuf;
  use std::sync::{Mutex, MutexGuard, OnceLock};

  use cosmic_text::fontdb;
  use erars_ast::Alignment;
  use erars_compiler::Language;
  use erars_proxy_system::ConsoleFrame;
  use erars_ui::width::WidthTable;
  use erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};

  use crate::font::FontChain;
  use crate::text::{CellMetrics, Shaper};

  /// Serialize tests that create a wgpu device. Software adapters (lavapipe) can
  /// fail or render incompletely when several devices are built concurrently, so
  /// every GPU-touching test holds this lock for its duration.
  pub fn gpu_lock() -> MutexGuard<'static, ()> {
      static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
      LOCK.get_or_init(|| Mutex::new(()))
          .lock()
          .unwrap_or_else(|e| e.into_inner())
  }

  /// The running test's name: libtest runs each test on a thread named after it
  /// (`headless::tests::box_frame_ink_lands_in_cells`).
  pub fn test_name() -> String {
      std::thread::current()
          .name()
          .unwrap_or("<unnamed test>")
          .to_string()
  }

  fn env_is_1(var: &str) -> bool {
      std::env::var_os(var).is_some_and(|v| v == "1")
  }

  /// A headless device, or `None` after printing `SKIP <test>: no wgpu adapter`.
  /// With `ERARS_REQUIRE_GPU=1` (CI with lavapipe) the missing adapter panics.
  pub fn gpu_device() -> Option<(wgpu::Device, wgpu::Queue)> {
      match crate::headless::request_device() {
          Some(d) => Some(d),
          None => {
              let name = test_name();
              if env_is_1("ERARS_REQUIRE_GPU") {
                  panic!("{name}: ERARS_REQUIRE_GPU=1 but no wgpu adapter is available");
              }
              eprintln!("SKIP {name}: no wgpu adapter");
              None
          }
      }
  }

  /// The bundled Latin monospace — the only font the GPU-enforced tests use.
  pub const BUNDLED_FONT_PATH: &str =
      concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf");

  pub fn bundled_font() -> PathBuf {
      PathBuf::from(BUNDLED_FONT_PATH)
  }

  /// Family names that count as a usable CJK monospace for the `_cjk` tests.
  const CJK_FAMILIES: &[&str] = &[
      "Noto Sans Mono CJK JP",
      "Noto Sans Mono CJK KR",
      "Noto Sans Mono CJK SC",
      "Noto Sans Mono CJK TC",
      "Sarasa Mono J",
      "Sarasa Mono K",
      "Sarasa Mono SC",
      "Sarasa Mono TC",
  ];

  /// The file of the first face in `db` advertising one of `families`
  /// (case-insensitive, any name language), searched in `families` order.
  /// Upright regular faces are preferred (the Noto CJK family ships one TTC
  /// per weight and fontdb's load order is machine-dependent); any weight or
  /// style is accepted only when no regular face advertises the family.
  fn font_file_for(db: &fontdb::Database, families: &[&str]) -> Option<PathBuf> {
      fn search(
          db: &fontdb::Database,
          families: &[&str],
          accept: impl Fn(&fontdb::FaceInfo) -> bool,
      ) -> Option<PathBuf> {
          families.iter().find_map(|fam| {
              db.faces().filter(|face| accept(face)).find_map(|face| {
                  let hit = face
                      .families
                      .iter()
                      .any(|(name, _)| name.eq_ignore_ascii_case(fam));
                  if !hit {
                      return None;
                  }
                  match &face.source {
                      fontdb::Source::File(p) | fontdb::Source::SharedFile(p, _) => Some(p.clone()),
                      fontdb::Source::Binary(_) => None,
                  }
              })
          })
      }
      search(db, families, |face| {
          face.weight == fontdb::Weight::NORMAL && face.style == fontdb::Style::Normal
      })
      .or_else(|| search(db, families, |_| true))
  }

  /// A system CJK monospace font file for the `_cjk` tests, or `None` after
  /// `SKIP <test>: no CJK monospace font installed`. `ERARS_REQUIRE_CJK_FONT=1`
  /// turns the skip into a failure.
  pub fn require_cjk_font() -> Option<PathBuf> {
      let mut db = fontdb::Database::new();
      db.load_system_fonts();
      let found = font_file_for(&db, CJK_FAMILIES);
      if found.is_none() {
          let name = test_name();
          if env_is_1("ERARS_REQUIRE_CJK_FONT") {
              panic!("{name}: ERARS_REQUIRE_CJK_FONT=1 but no CJK monospace font is installed");
          }
          eprintln!("SKIP {name}: no CJK monospace font installed");
      }
      found
  }

  /// `msgothic.ttc` from `ERARS_FONT_DIR` (the directory containing it, or the
  /// file itself), or `None` after a SKIP line. Opt-in only: the font is
  /// proprietary and never present in CI, so there is no REQUIRE variable.
  pub fn msgothic_font() -> Option<PathBuf> {
      let found = std::env::var_os("ERARS_FONT_DIR").and_then(|d| {
          let p = PathBuf::from(d);
          let file = if p.is_file() { p } else { p.join("msgothic.ttc") };
          let is_ms = file
              .file_name()
              .and_then(|n| n.to_str())
              .is_some_and(|n| n.eq_ignore_ascii_case("msgothic.ttc"));
          (is_ms && file.is_file()).then_some(file)
      });
      if found.is_none() {
          eprintln!("SKIP {}: msgothic.ttc not found under ERARS_FONT_DIR", test_name());
      }
      found
  }

  /// A shaper over exactly `files` (no system fonts, no locale) with cell
  /// metrics taken from the primary face at scale 1.
  pub fn test_shaper(files: &[PathBuf], lang: Language, font_size: u32, line_height: u32) -> Shaper {
      let mut chain = FontChain::from_files(files, lang);
      let primary = chain.font(chain.primary());
      let m = CellMetrics::from_primary(&primary, font_size, line_height, 1.0);
      Shaper::new(chain, WidthTable::new(lang.encoding()), m)
  }

  pub fn style(color: [u8; 3]) -> TextStyle {
      TextStyle {
          color: Color(color),
          font_family: "".into(),
          font_style: FontStyle::NORMAL,
      }
  }

  pub fn text_line(s: &str, color: [u8; 3]) -> ConsoleLine {
      ConsoleLine {
          align: Alignment::Left,
          button_start: None,
          parts: vec![ConsoleLinePart::Text(s.to_string(), style(color))],
      }
  }

  /// Black background, Emuera's yellow focus colour, grey (192) default text.
  pub fn frame(lines: Vec<ConsoleLine>) -> ConsoleFrame {
      ConsoleFrame {
          bg_color: Color([0, 0, 0]),
          hl_color: Color([255, 255, 0]),
          fore_color: Color([192, 192, 192]),
          lines,
      }
  }

  #[cfg(test)]
  mod tests {
      use super::*;

      #[test]
      fn test_name_is_the_test_path() {
          assert_eq!(test_name(), "test_support::tests::test_name_is_the_test_path");
      }

      #[test]
      fn bundled_font_exists_and_makes_the_documented_metrics() {
          assert!(std::path::Path::new(BUNDLED_FONT_PATH).is_file());
          let shaper = test_shaper(&[bundled_font()], Language::Japanese, 18, 19);
          let m = *shaper.metrics();
          assert_eq!((m.font_px, m.half_w, m.line_h, m.baseline, m.shift), (18, 11, 19, 19, 3));
      }
  }
  ```
  (No `#![cfg(test)]` inner attribute — `main.rs` gates the module with `#[cfg(test)] mod test_support;` today and T10 turns it into a `pub mod` for the integration test.)
  T8's callers (`raster.rs` `get_uploads_a_from_the_bundled_font`, `draw.rs` `build_instances_gpu_smoke`) already use the zero-argument `crate::test_support::gpu_device()` / `test_name()`; confirm nothing passes an argument:
  ```
  grep -rn 'gpu_device("\|test_name("' crates/erars-renderer/src/
  ```
  The `grep` must print nothing. Run `cargo test -p erars-renderer -- test_support:: raster::get_uploads draw::build_instances_gpu --nocapture` (`cargo test` takes one TESTNAME positional; several filters go after `--`); expect `test_name_is_the_test_path ... ok`, `bundled_font_exists_and_makes_the_documented_metrics ... ok`, and the two T8 GPU tests `ok` (on the dev box; on a box without an adapter they print `SKIP raster::tests::get_uploads_a_from_the_bundled_font: no wgpu adapter` and pass).
  Commit: `git add crates/erars-renderer/src/test_support.rs crates/erars-renderer/src/headless.rs && git commit -m "test(renderer): gpu_device() with SKIP/ERARS_REQUIRE_GPU, CJK and MS Gothic font gates, console fixtures"`

- [ ] **Step 6: Write the failing `render_frame` tests.**
  Replace the whole `#[cfg(test)] mod tests { … }` of `headless.rs` (from `#[cfg(test)]` to EOF — this drops the three `FontCtx`/`Grid` tests; the two `png_*` tests and `checker` are re-included below) with:
  ```rust
  #[cfg(test)]
  mod tests {
      use super::*;
      use std::path::PathBuf;

      use erars_ast::Value;
      use erars_compiler::Language;
      use erars_ui::width::WidthTable;

      use crate::font::{FontChain, StyleKey};
      use crate::layout::{Layout, Row};
      use crate::test_support::{self as ts, bundled_font, frame, gpu_device, gpu_lock, style, text_line};
      use crate::text::CellMetrics;

      /// A column counts as inked when some pixel in the band has a channel ≥ this.
      const INK: u8 = 32;
      const WHITE: [u8; 3] = [255, 255, 255];

      fn jp_shaper(files: &[PathBuf]) -> Shaper {
          ts::test_shaper(files, Language::Japanese, 18, 19)
      }

      fn geometry(shaper: &Shaper, content_w: u32) -> Geometry {
          Geometry::new(content_w, *shaper.metrics())
      }

      fn render(
          shaper: &mut Shaper,
          dev: &(wgpu::Device, wgpu::Queue),
          fr: &ConsoleFrame,
          w: u32,
          h: u32,
          input: Option<&str>,
          hover: Option<usize>,
      ) -> Rendered {
          render_frame_on(&dev.0, &dev.1, shaper, fr, w, h, input, hover, true)
      }

      /// Screen y of row `r` in a `height`-tall render with `scroll_rows = 0`,
      /// re-derived from spec Component 5 (`view_h − (bottom_row − r + 1)·line_h`)
      /// on purpose — independent of `draw::View::row_y`.
      fn row_y(rows: usize, r: usize, height: u32, line_h: u32) -> u32 {
          let view_h = height - line_h;
          let bottom = rows - 1;
          view_h - (bottom - r + 1) as u32 * line_h
      }

      /// Screen-space cell boxes of a laid-out row: (x_start, x_end_excl, text).
      fn boxes(row: &Row, m: &CellMetrics) -> Vec<(u32, u32, String)> {
          row.clusters
              .iter()
              .map(|c| {
                  let x = (m.shift as i32 + row.x0 + c.x).max(0) as u32;
                  (x, x + c.cells as u32 * m.half_w, c.text.to_string())
              })
              .collect()
      }

      fn in_glyph_box(bx: &[(u32, u32, String)], x: u32) -> bool {
          bx.iter()
              .any(|(a, b, t)| x >= *a && x < *b && !t.trim().is_empty())
      }

      /// The "perfect fallback" invariant in pixels: every inked column of row
      /// `r`'s band lies inside a non-blank cell box of row `r` — or of row
      /// `r−1`, because a tall font's glyphs overflow the row below (spec
      /// Component 4: no clamping to `line_h`). Every row must have ink of its own.
      fn assert_ink_in_boxes(img: &Rendered, lay: &Layout, m: &CellMetrics, height: u32) {
          let rows = lay.rows.len();
          for (r, row) in lay.rows.iter().enumerate() {
              let y0 = row_y(rows, r, height, m.line_h);
              let own = boxes(row, m);
              let above = if r > 0 { boxes(&lay.rows[r - 1], m) } else { Vec::new() };
              let ink = img.ink_columns(y0, y0 + m.line_h, INK);
              let mut own_ink = 0usize;
              for (x, &inked) in ink.iter().enumerate() {
                  if !inked {
                      continue;
                  }
                  let x = x as u32;
                  if in_glyph_box(&own, x) {
                      own_ink += 1;
                  } else if !in_glyph_box(&above, x) {
                      panic!("row {r}: ink at x={x} outside every glyph box of rows {r} and {}: {own:?}", r.saturating_sub(1));
                  }
              }
              assert!(own_ink > 0, "row {r} has no ink inside its own boxes");
          }
      }

      fn checker(w: u32, h: u32) -> Vec<u8> {
          let mut rgba = vec![0u8; (w * h * 4) as usize];
          for y in 0..h {
              for x in 0..w {
                  let i = ((y * w + x) * 4) as usize;
                  rgba[i] = (x * 37) as u8;
                  rgba[i + 1] = (y * 91) as u8;
                  rgba[i + 2] = ((x + y) % 2 * 255) as u8;
                  rgba[i + 3] = 255;
              }
          }
          rgba
      }

      /// Signature, IHDR fields, exactly IHDR/IDAT/IEND, and a valid CRC-32
      /// (over type + data) on every chunk.
      #[test]
      fn png_chunks_are_well_formed() {
          let (w, h) = (7u32, 3u32);
          let png = encode_png(w, h, &checker(w, h));
          assert_eq!(&png[..8], &[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]);
          let mut pos = 8;
          let mut kinds = Vec::new();
          while pos < png.len() {
              let len = u32::from_be_bytes(png[pos..pos + 4].try_into().unwrap()) as usize;
              let kind = &png[pos + 4..pos + 8];
              let data = &png[pos + 8..pos + 8 + len];
              let crc = u32::from_be_bytes(png[pos + 8 + len..pos + 12 + len].try_into().unwrap());
              let mut hasher = crc32fast::Hasher::new();
              hasher.update(kind);
              hasher.update(data);
              assert_eq!(hasher.finalize(), crc, "bad CRC in {}", String::from_utf8_lossy(kind));
              if kind == b"IHDR" {
                  assert_eq!(len, 13);
                  assert_eq!(&data[..8], &[0, 0, 0, 7, 0, 0, 0, 3]);
                  assert_eq!(&data[8..], &[8, 6, 0, 0, 0]); // 8-bit RGBA, deflate, filter 0, no interlace
              }
              if kind == b"IEND" {
                  assert_eq!(len, 0);
              }
              kinds.push(String::from_utf8_lossy(kind).into_owned());
              pos += 12 + len;
          }
          assert_eq!(pos, png.len());
          assert_eq!(kinds, ["IHDR", "IDAT", "IEND"]);
      }

      /// The single IDAT inflates to `height` filter-0 scanlines of the input rows.
      #[test]
      fn png_idat_inflates_to_filter0_scanlines() {
          use std::io::Read;
          let (w, h) = (5u32, 4u32);
          let rgba = checker(w, h);
          let png = encode_png(w, h, &rgba);
          // 8 signature + 25 (IHDR chunk) = 33: IDAT length at 33, type at 37, data at 41.
          let len = u32::from_be_bytes(png[33..37].try_into().unwrap()) as usize;
          assert_eq!(&png[37..41], b"IDAT");
          let mut raw = Vec::new();
          flate2::read::ZlibDecoder::new(&png[41..41 + len])
              .read_to_end(&mut raw)
              .unwrap();
          let stride = 1 + (w * 4) as usize;
          assert_eq!(raw.len(), stride * h as usize);
          for (y, line) in raw.chunks_exact(stride).enumerate() {
              assert_eq!(line[0], 0, "scanline {y} filter byte");
              let row = &rgba[y * (w * 4) as usize..(y + 1) * (w * 4) as usize];
              assert_eq!(&line[1..], row, "scanline {y}");
          }
      }

      /// `bg_color` is honoured: an empty frame is a solid image of it, byte-exact.
      #[test]
      fn bg_colour_fills_the_image() {
          let _gpu = gpu_lock();
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[bundled_font()]);
          let mut fr = frame(vec![]);
          fr.bg_color = erars_ui::Color([10, 20, 30]);
          let img = render(&mut shaper, &dev, &fr, 64, 40, None, None);
          assert_eq!((img.width, img.height), (64, 40));
          assert!(img.rgba.chunks_exact(4).all(|p| p == [10, 20, 30, 255]), "not a solid bg fill");
      }

      /// One row (`hill`: no glyph reaches below the baseline, which is the
      /// strip's first pixel row) + input in a 3-row-tall image: slack row at the
      /// top (rows are bottom-anchored above the strip), the text row in the middle, the strip
      /// `> abc_` in the default colour on the bottom `line_h` rows; every strip
      /// pixel is grey (fore 192) and inside a cell box of the strip line;
      /// `input = None` leaves the strip empty.
      #[test]
      fn input_strip_is_drawn_at_the_bottom() {
          let _gpu = gpu_lock();
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[bundled_font()]);
          let m = *shaper.metrics();
          let lh = m.line_h;
          let fr = frame(vec![text_line("hill", WHITE)]);
          let (w, h) = (120, 3 * lh);
          let img = render(&mut shaper, &dev, &fr, w, h, Some("abc"), None);
          let any_ink = |img: &Rendered, y0: u32, y1: u32| img.ink_columns(y0, y1, INK).iter().any(|&b| b);
          assert!(!any_ink(&img, 0, lh), "slack row must be empty");
          assert!(any_ink(&img, lh, 2 * lh), "text row missing");
          assert!(any_ink(&img, 2 * lh, 3 * lh), "input strip missing");
          for y in 2 * lh..3 * lh {
              for x in 0..w {
                  let [r, g, b, _] = img.pixel(x, y);
                  assert!(r == g && g == b && r <= 192, "strip pixel ({x},{y}) = {:?}", (r, g, b));
              }
          }
          // Strip ink lies inside the boxes of "> abc_" and touches > a b c (the
          // bundled font's `_` sits below the baseline = the strip's last row → clipped).
          let strip_line = text_line("> abc_", [192, 192, 192]);
          let strip_lay = layout(std::slice::from_ref(&strip_line), &geometry(&shaper, w), &mut shaper);
          let bx = boxes(&strip_lay.rows[0], &m);
          let ink = img.ink_columns(2 * lh, 3 * lh, INK);
          let mut touched: Vec<&str> = Vec::new();
          for (x, &inked) in ink.iter().enumerate() {
              if !inked {
                  continue;
              }
              let (_, _, t) = bx
                  .iter()
                  .find(|(a, b, t)| (x as u32) >= *a && (x as u32) < *b && !t.trim().is_empty())
                  .unwrap_or_else(|| panic!("strip ink at x={x} outside the strip's glyph boxes {bx:?}"));
              if touched.last() != Some(&t.as_str()) {
                  touched.push(t.as_str());
              }
          }
          assert!(touched.starts_with(&[">", "a", "b", "c"]), "strip glyphs with ink: {touched:?}");
          assert!(touched.len() <= 5, "strip glyphs with ink: {touched:?}");
          let none = render(&mut shaper, &dev, &fr, w, h, None, None);
          assert!(!any_ink(&none, 2 * lh, 3 * lh), "strip drawn without input");
      }
  }
  ```
  Run `cargo test -p erars-renderer headless::tests::bg_colour`; expect the build to fail with unresolved names from the new tests — `error[E0412]: cannot find type `Shaper``/`ConsoleFrame`/`Geometry` (rustc reports the types first) and `error[E0425]: cannot find function `render_frame_on``/`layout` — all of which Step 7 provides.

- [ ] **Step 7: Implement `render_frame` / `render_frame_opts` / `render_frame_on` and the `main.rs` shim.**
  Replace everything in `headless.rs` above `#[cfg(test)]` (the header, imports, `Rendered`, `request_device`, `render_lines`, the PNG functions) with:
  ```rust
  //! Offscreen rendering for headless environments (SSH/CI, no display server)
  //! and the `--headless-shot` CLI.
  //!
  //! `render_frame` draws a `ConsoleFrame` through the same path as the window
  //! (`layout::layout` → `draw::build_instances` → `gpu::{create_quad_pipeline,
  //! nearest_sampler, FrameDraw}`) into an `Rgba8Unorm` texture and reads it
  //! back, so pixel positions can be asserted without a display. The target is
  //! linear (the window path clears an sRGB surface), so headless bytes are
  //! compared with each other and with exact 0/255 masks — never with the window.
  //!
  //! View math (spec Component 5) with `scroll_rows = 0`: `strip_h = line_h`,
  //! `view_h = height − strip_h`, row `r` at `view_h − (bottom_row − r + 1)·line_h`,
  //! so slack appears at the top. The input strip shows `> {input}_` in
  //! `frame.fore_color` on the bottom `line_h` rows when `input` is `Some`.

  use std::io::Write;
  use std::path::{Path, PathBuf};

  use erars_ast::Alignment;
  use erars_compiler::EraConfig;
  use erars_proxy_system::ConsoleFrame;
  use erars_ui::width::WidthTable;
  use erars_ui::{ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};
  use wgpu::util::DeviceExt;

  use crate::draw::{build_instances, View};
  use crate::font::{FontChain, FontConfig};
  use crate::gpu::{create_quad_pipeline, nearest_sampler, FrameDraw, Globals, Instance};
  use crate::layout::{layout, Geometry};
  use crate::raster::GlyphRaster;
  use crate::text::{CellMetrics, Shaper};

  /// A rendered RGBA8 image (row-major, 4 bytes/pixel, no row padding).
  pub struct Rendered {
      pub width: u32,
      pub height: u32,
      pub rgba: Vec<u8>,
  }

  impl Rendered {
      pub fn pixel(&self, x: u32, y: u32) -> [u8; 4] {
          let i = ((y * self.width + x) * 4) as usize;
          [self.rgba[i], self.rgba[i + 1], self.rgba[i + 2], self.rgba[i + 3]]
      }

      /// The bytes of rows `[y0, y1)`.
      pub fn band(&self, y0: u32, y1: u32) -> &[u8] {
          let stride = (self.width * 4) as usize;
          &self.rgba[y0 as usize * stride..y1.min(self.height) as usize * stride]
      }

      /// Per column: does any pixel in rows `[y0, y1)` have a channel ≥ `min`?
      pub fn ink_columns(&self, y0: u32, y1: u32, min: u8) -> Vec<bool> {
          let mut cols = vec![false; self.width as usize];
          for y in y0..y1.min(self.height) {
              for x in 0..self.width {
                  let [r, g, b, _] = self.pixel(x, y);
                  if r.max(g).max(b) >= min {
                      cols[x as usize] = true;
                  }
              }
          }
          cols
      }

      /// Bounding box `[x_min, y_min, x_max_excl, y_max_excl]` of pixels with a
      /// channel ≥ `min` inside `[x0, x1) × [y0, y1)`, or `None` if there are none.
      pub fn ink_bbox(&self, x0: u32, x1: u32, y0: u32, y1: u32, min: u8) -> Option<[u32; 4]> {
          let mut bb: Option<[u32; 4]> = None;
          for y in y0..y1.min(self.height) {
              for x in x0..x1.min(self.width) {
                  let [r, g, b, _] = self.pixel(x, y);
                  if r.max(g).max(b) < min {
                      continue;
                  }
                  bb = Some(match bb {
                      None => [x, y, x + 1, y + 1],
                      Some([a, b2, c, d]) => [a.min(x), b2.min(y), c.max(x + 1), d.max(y + 1)],
                  });
              }
          }
          bb
      }

      /// Sum of pixel luminance over the rows `[y0, y1)` for each column x.
      pub fn column_ink(&self, y0: u32, y1: u32) -> Vec<f32> {
          let y1 = y1.min(self.height);
          let mut prof = vec![0.0f32; self.width as usize];
          for y in y0..y1 {
              for x in 0..self.width {
                  let [r, g, b, _] = self.pixel(x, y);
                  prof[x as usize] += 0.299 * r as f32 + 0.587 * g as f32 + 0.114 * b as f32;
              }
          }
          prof
      }

      /// Rightmost column whose ink exceeds `threshold`, or 0 if none.
      pub fn ink_right_edge(prof: &[f32], threshold: f32) -> usize {
          prof.iter().rposition(|&v| v > threshold).unwrap_or(0)
      }
  }

  /// The default adapter/device without a surface. `None` when this machine has
  /// no wgpu adapter (tests skip or fail through `test_support::gpu_device`).
  pub fn request_device() -> Option<(wgpu::Device, wgpu::Queue)> {
      let instance = wgpu::Instance::default();
      let adapter =
          pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions::default()))?;
      pollster::block_on(adapter.request_device(
          &wgpu::DeviceDescriptor {
              label: Some("erars-headless"),
              required_features: wgpu::Features::empty(),
              required_limits: wgpu::Limits::downlevel_defaults(),
          },
          None,
      ))
      .ok()
  }

  /// The shaper for a game: fonts from the configured family → `<game>/font`
  /// → `ERARS_FONT_DIR` → the language's fixed-pitch CJK list → the bundled
  /// font (spec Component 3), cell metrics from the primary face at scale 1
  /// (headless has no window scale; the window applies its real scale factor
  /// through `Shaper::set_metrics`). Shared by `--headless-shot` and the app.
  pub fn shaper_for(config: &EraConfig, game_dir: &Path) -> Shaper {
      let mut chain = FontChain::new(&FontConfig {
          family: &config.font_family,
          game_dir,
          extra_dir: std::env::var_os("ERARS_FONT_DIR").map(PathBuf::from),
          lang: config.lang,
      });
      let primary = chain.font(chain.primary());
      let m = CellMetrics::from_primary(&primary, config.font_size, config.line_height, 1.0);
      Shaper::new(chain, WidthTable::new(config.lang.encoding()), m)
  }

  /// Render `frame` into a `content_w × height` image with `scroll_rows = 0`
  /// (bitmap strikes on). `None` if no GPU adapter is available.
  pub fn render_frame(
      shaper: &mut Shaper,
      frame: &ConsoleFrame,
      content_w: u32,
      height: u32,
      input: Option<&str>,
      hover: Option<usize>,
  ) -> Option<Rendered> {
      render_frame_opts(shaper, frame, content_w, height, input, hover, true)
  }

  /// [`render_frame`] with the `--no-bitmap-strikes` switch.
  pub fn render_frame_opts(
      shaper: &mut Shaper,
      frame: &ConsoleFrame,
      content_w: u32,
      height: u32,
      input: Option<&str>,
      hover: Option<usize>,
      use_bitmap_strikes: bool,
  ) -> Option<Rendered> {
      let (device, queue) = request_device()?;
      Some(render_frame_on(
          &device,
          &queue,
          shaper,
          frame,
          content_w,
          height,
          input,
          hover,
          use_bitmap_strikes,
      ))
  }

  /// [`render_frame`] on an existing device. `hover` indexes `Layout.buttons`
  /// of the log layout and recolours that fragment with `frame.hl_color` (draw
  /// time only, nothing moves).
  #[allow(clippy::too_many_arguments)]
  pub fn render_frame_on(
      device: &wgpu::Device,
      queue: &wgpu::Queue,
      shaper: &mut Shaper,
      frame: &ConsoleFrame,
      content_w: u32,
      height: u32,
      input: Option<&str>,
      hover: Option<usize>,
      use_bitmap_strikes: bool,
  ) -> Rendered {
      let content_w = content_w.max(1);
      let height = height.max(1);
      let m = *shaper.metrics();
      let g = Geometry::new(content_w, m);
      let mut raster = GlyphRaster::new(device, use_bitmap_strikes);
      let hl = frame.hl_color.0;

      // Log rows: bottom-anchored above the input strip.
      let strip_h = m.line_h;
      let view = View {
          scroll_rows: 0,
          view_h: height.saturating_sub(strip_h),
          strip_h,
      };
      let log = layout(&frame.lines, &g, shaper);
      let mut pages = build_instances(&log, &view, hover, hl, &mut raster, device, queue, shaper);

      // Input strip: one line laid out on its own, drawn on the bottom `line_h` rows.
      if let Some(input) = input {
          let line = ConsoleLine {
              align: Alignment::Left,
              button_start: None,
              parts: vec![ConsoleLinePart::Text(
                  format!("> {input}_"),
                  TextStyle {
                      color: frame.fore_color,
                      font_family: "".into(),
                      font_style: FontStyle::NORMAL,
                  },
              )],
          };
          let strip = layout(std::slice::from_ref(&line), &g, shaper);
          let strip_pages = build_instances(&strip, &view.strip(), None, hl, &mut raster, device, queue, shaper);
          merge_pages(&mut pages, strip_pages);
      }
      if pages.len() < raster.page_count() {
          pages.resize_with(raster.page_count(), Vec::new);
      }

      let rgba = draw_offscreen(device, queue, &raster, &pages, frame.bg_color.0, content_w, height);
      Rendered {
          width: content_w,
          height,
          rgba,
      }
  }

  /// Append `from`'s per-page instance buckets to `into`'s (growing the list).
  fn merge_pages(into: &mut Vec<Vec<Instance>>, from: Vec<Vec<Instance>>) {
      for (page, list) in from.into_iter().enumerate() {
          if into.len() <= page {
              into.resize_with(page + 1, Vec::new);
          }
          into[page].extend(list);
      }
  }

  /// Clear to `bg` (linear target, so the bytes come back exactly), draw every
  /// page's instances against that page's atlas view with the `Nearest`
  /// sampler, and read the texture back without row padding.
  fn draw_offscreen(
      device: &wgpu::Device,
      queue: &wgpu::Queue,
      raster: &GlyphRaster,
      pages: &[Vec<Instance>],
      bg: [u8; 3],
      width: u32,
      height: u32,
  ) -> Vec<u8> {
      let format = wgpu::TextureFormat::Rgba8Unorm;
      let target = device.create_texture(&wgpu::TextureDescriptor {
          label: Some("headless-target"),
          size: wgpu::Extent3d {
              width,
              height,
              depth_or_array_layers: 1,
          },
          mip_level_count: 1,
          sample_count: 1,
          dimension: wgpu::TextureDimension::D2,
          format,
          usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
          view_formats: &[],
      });
      let target_view = target.create_view(&wgpu::TextureViewDescriptor::default());

      let (pipeline, bind_group_layout) = create_quad_pipeline(device, format);
      let globals_buf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
          label: Some("globals"),
          contents: bytemuck::bytes_of(&Globals {
              screen: [width as f32, height as f32],
              _pad: [0.0; 2],
          }),
          usage: wgpu::BufferUsages::UNIFORM,
      });
      let sampler = nearest_sampler(device);
      let draw = FrameDraw::new(device, &bind_group_layout, &globals_buf, &sampler, &raster.pages_with(pages));

      // bytes_per_row must be a multiple of 256 for texture->buffer copies.
      let unpadded = width * 4;
      let padded = unpadded.div_ceil(256) * 256;
      let readback = device.create_buffer(&wgpu::BufferDescriptor {
          label: Some("readback"),
          size: (padded * height) as u64,
          usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
          mapped_at_creation: false,
      });

      let mut encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
      {
          let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
              label: Some("headless-pass"),
              color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                  view: &target_view,
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
          draw.draw(&mut pass, &pipeline);
      }
      encoder.copy_texture_to_buffer(
          wgpu::ImageCopyTexture {
              texture: &target,
              mip_level: 0,
              origin: wgpu::Origin3d::ZERO,
              aspect: wgpu::TextureAspect::All,
          },
          wgpu::ImageCopyBuffer {
              buffer: &readback,
              layout: wgpu::ImageDataLayout {
                  offset: 0,
                  bytes_per_row: Some(padded),
                  rows_per_image: Some(height),
              },
          },
          wgpu::Extent3d {
              width,
              height,
              depth_or_array_layers: 1,
          },
      );
      queue.submit(Some(encoder.finish()));

      let slice = readback.slice(..);
      let (tx, rx) = std::sync::mpsc::channel();
      slice.map_async(wgpu::MapMode::Read, move |r| {
          let _ = tx.send(r);
      });
      device.poll(wgpu::Maintain::Wait);
      rx.recv()
          .expect("map_async callback")
          .expect("readback buffer map");

      let mapped = slice.get_mapped_range();
      let mut rgba = vec![0u8; (unpadded * height) as usize];
      for y in 0..height as usize {
          let src = y * padded as usize;
          let dst = y * unpadded as usize;
          rgba[dst..dst + unpadded as usize].copy_from_slice(&mapped[src..src + unpadded as usize]);
      }
      drop(mapped);
      readback.unmap();
      rgba
  }

  /// Append one PNG chunk: length, type, data, CRC-32 over type + data.
  fn png_chunk(out: &mut Vec<u8>, kind: &[u8; 4], data: &[u8]) {
      out.extend_from_slice(&(data.len() as u32).to_be_bytes());
      let mut hasher = crc32fast::Hasher::new();
      hasher.update(kind);
      hasher.update(data);
      out.extend_from_slice(kind);
      out.extend_from_slice(data);
      out.extend_from_slice(&hasher.finalize().to_be_bytes());
  }

  /// Encode an RGBA8 buffer as a minimal PNG: signature, IHDR, one IDAT holding
  /// the zlib stream of filter-0 scanlines, IEND. No `png` crate needed.
  pub fn encode_png(width: u32, height: u32, rgba: &[u8]) -> Vec<u8> {
      assert_eq!(rgba.len(), (width * height * 4) as usize, "rgba size");
      let mut out = Vec::with_capacity(rgba.len() / 4 + 64);
      out.extend_from_slice(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]);
      let mut ihdr = Vec::with_capacity(13);
      ihdr.extend_from_slice(&width.to_be_bytes());
      ihdr.extend_from_slice(&height.to_be_bytes());
      ihdr.extend_from_slice(&[8, 6, 0, 0, 0]); // bit depth 8, RGBA, deflate, filter 0, no interlace
      png_chunk(&mut out, b"IHDR", &ihdr);
      let stride = (width * 4) as usize;
      let mut enc = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::default());
      for row in rgba.chunks_exact(stride) {
          enc.write_all(&[0]).expect("in-memory zlib write");
          enc.write_all(row).expect("in-memory zlib write");
      }
      let idat = enc.finish().expect("in-memory zlib finish");
      png_chunk(&mut out, b"IDAT", &idat);
      png_chunk(&mut out, b"IEND", &[]);
      out
  }

  /// Write a render as PNG — viewable anywhere, small enough to `scp` back.
  pub fn write_png(path: &str, img: &Rendered) -> std::io::Result<()> {
      std::fs::write(path, encode_png(img.width, img.height, &img.rgba))
  }
  ```
  Then the `main.rs` shim (locate each spot by content). (a) The clap attribute of `headless_shot` — replace the block from `#[clap(` through `headless_shot: Option<String>,` with:
  ```rust
      #[clap(
          long,
          value_name = "PATH.png",
          help = "Headless: render the first screen to a PNG image and exit (no display needed)"
      )]
      headless_shot: Option<String>,
  ```
  (b) Replace the whole `fn headless_shot(mut font: font::FontCtx, …) { … }` (with its doc comment) with:
  ```rust
  /// Headless capture: run the game until it first waits for input, then render
  /// the current screen (with an empty input strip) to a PNG file and exit.
  fn headless_shot(
      mut shaper: text::Shaper,
      receiver: erars_proxy_system::ProxyReceiver,
      (w, h): (u32, u32),
      path: &str,
  ) {
      use erars_proxy_system::SystemRequest;
      let mut frame = erars_proxy_system::ConsoleFrame::default();
      // Drain requests until the game blocks for input (screen is settled).
      loop {
          match receiver.req_rx.recv() {
              Ok(SystemRequest::Redraw(f)) => frame = f,
              Ok(SystemRequest::Input(_)) | Ok(SystemRequest::Quit) | Err(_) => break,
          }
      }
      match headless::render_frame(&mut shaper, &frame, w, h, Some(""), None) {
          Some(img) => match headless::write_png(path, &img) {
              Ok(()) => println!("Wrote {path} ({w}x{h})"),
              Err(e) => eprintln!("Failed to write {path}: {e}"),
          },
          None => eprintln!("No GPU adapter available for headless rendering"),
      }
  }
  ```
  (c) Replace the `if let Some(path) = args.headless_shot.clone() { … return; }` block in `main` with:
  ```rust
      // Headless capture mode: no window, no display server required.
      if let Some(path) = args.headless_shot.clone() {
          let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(|| {}));
          let shaper = headless::shaper_for(&config, Path::new(&args.target_path));
          spawn_vm(target_path, args.load, !args.lint_off, system, config);
          headless_shot(shaper, receiver, init_size, &path);
          return;
      }
  ```
  (`build_font()` / `App::new(build_font(), ..)` and the legacy `FontCtx` stay for the window path until T10; `Path` is already imported in `main.rs`. `headless::shaper_for` calls `FontChain::new`, which loads the system fonts — the same cost the legacy `FontCtx` paid.) If `#[allow(dead_code)]` sits on `mod layout;` / `mod raster;` from T7/T8 it may stay — nothing in this task depends on it.
  Run `cargo test -p erars-renderer headless::tests -- --nocapture`; expect
  ```
  test headless::tests::png_chunks_are_well_formed ... ok
  test headless::tests::png_idat_inflates_to_filter0_scanlines ... ok
  test headless::tests::bg_colour_fills_the_image ... ok
  test headless::tests::input_strip_is_drawn_at_the_bottom ... ok
  test result: ok. 4 passed
  ```
  (on a machine without an adapter the two GPU tests print `SKIP headless::tests::…: no wgpu adapter` and pass; with `ERARS_REQUIRE_GPU=1` they fail instead). Also `cargo build -p erars-renderer` must succeed (the binary path now compiles against `render_frame`/`write_png`).
  Commit: `git add crates/erars-renderer/src/headless.rs crates/erars-renderer/src/main.rs && git commit -m "feat(renderer): headless render_frame over the cell layout with bg/hl colours and the input strip"`

- [ ] **Step 8: Pixel tests with the bundled font (GPU-enforced in CI).**
  Insert before the final `}` of `mod tests` in `headless.rs`:
  ```rust
      /// Spec Testing §5: a box-drawing frame over an ASCII ruler. With the
      /// bundled font as primary (0.6 em → half_w 11) every JIS box character is
      /// a 2-cell, 22 px box holding a centred 10.8 px glyph; all ink must land
      /// inside `[shift + k·half_w, shift + (k+cells)·half_w)` of the cluster
      /// that owns it (or spill straight down from the row above), and the
      /// spaced row proves nothing leaks sideways into a blank cell.
      #[test]
      fn box_frame_ink_lands_in_cells() {
          let _gpu = gpu_lock();
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[bundled_font()]);
          let m = *shaper.metrics();
          assert_eq!((m.font_px, m.half_w, m.line_h, m.shift), (18, 11, 19, 3));
          let fr = frame(vec![
              text_line("01234567", WHITE),
              text_line("┏━━┓", WHITE),
              text_line("┃    ┃", WHITE),
              text_line("┗━━┛", WHITE),
              text_line("┏ ━ ┓", WHITE),
          ]);
          let (w, h) = (200, 6 * m.line_h);
          let img = render(&mut shaper, &dev, &fr, w, h, None, None);
          let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
          assert_eq!(lay.rows.len(), 5);
          // JP widths: box characters are 2 cells, so every row is 8 cells wide.
          for row in &lay.rows {
              let cells: u32 = row.clusters.iter().map(|c| c.cells as u32).sum();
              assert_eq!(cells, 8, "row {} cells", row.line);
          }
          assert_ink_in_boxes(&img, &lay, &m, h);
      }

      /// Identical lines render to byte-identical row bands: glyph origins are
      /// integer pixels and the sampler is Nearest, so nothing drifts per row.
      /// Rows 1–3 are compared (each has an identical row above it spilling the
      /// same descenders into its band; row 0 has none).
      #[test]
      fn identical_rows_are_byte_identical() {
          let _gpu = gpu_lock();
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[bundled_font()]);
          let lh = shaper.metrics().line_h;
          let fr = frame(vec![text_line("Abc123Xyz┏━┓", WHITE); 4]);
          let h = 5 * lh;
          let img = render(&mut shaper, &dev, &fr, 300, h, None, None);
          let b1 = img.band(lh, 2 * lh);
          assert!(b1.iter().any(|&v| v != 0), "no ink rendered");
          assert_eq!(b1, img.band(2 * lh, 3 * lh), "row 2 differs from row 1");
          assert_eq!(b1, img.band(3 * lh, 4 * lh), "row 3 differs from row 1");
      }

      /// A glyph whose natural advance exceeds its box (`a > w`: the bundled
      /// 10.8 px glyphs in a pinned 9 px cell) is rescaled, and one that fits
      /// (`a ≤ w`: 2-cell `α`/`°`/`→` in an 18 px box) is centred — neither may
      /// put ink outside its box.
      #[test]
      fn rescaled_and_centred_glyphs_stay_in_their_boxes() {
          let _gpu = gpu_lock();
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[bundled_font()]);
          shaper.set_metrics(CellMetrics {
              scale: 1.0,
              font_px: 18,
              half_w: 9,
              line_h: 19,
              baseline: 15,
              shift: 3,
          });
          let m = *shaper.metrics();
          let fr = frame(vec![text_line("MMMM W W", WHITE), text_line("αα°→", WHITE)]);
          let (w, h) = (200, 3 * m.line_h);
          let img = render(&mut shaper, &dev, &fr, w, h, None, None);
          let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
          assert_eq!(lay.rows[0].clusters.iter().map(|c| c.cells).collect::<Vec<_>>(), [1; 8]);
          assert_eq!(lay.rows[1].clusters.iter().map(|c| c.cells).collect::<Vec<_>>(), [2, 2, 2, 2]);
          assert_ink_in_boxes(&img, &lay, &m, h);
      }

      fn button(text: &str, gen: u32, v: i64) -> ConsoleLinePart {
          ConsoleLinePart::Button(vec![(text.to_string(), style(WHITE))], gen, Value::Int(v))
      }

      /// Hover is colour-only at draw time: `hover = Some(i)` changes pixels only
      /// inside `buttons[i]`'s columns, every changed pixel takes the focus colour
      /// (white → yellow keeps r/g, zeroes b), and `hover = None` is byte-stable.
      #[test]
      fn hover_recolours_only_the_hovered_button() {
          let _gpu = gpu_lock();
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[bundled_font()]);
          let m = *shaper.metrics();
          let line = ConsoleLine {
              align: Alignment::Left,
              button_start: None,
              parts: vec![
                  ConsoleLinePart::Text("pick: ".into(), style(WHITE)),
                  button("[1] go", 1, 1),
                  ConsoleLinePart::Text(" ".into(), style(WHITE)),
                  button("[2] stop", 1, 2),
              ],
          };
          let fr = frame(vec![line]);
          let (w, h) = (300, 2 * m.line_h);
          let base = render(&mut shaper, &dev, &fr, w, h, None, None);
          let again = render(&mut shaper, &dev, &fr, w, h, None, None);
          assert_eq!(base.rgba, again.rgba, "unhovered render is not byte-stable");
          let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
          assert_eq!(lay.buttons.len(), 2);
          let mut hovered: Vec<Rendered> = Vec::new();
          for i in 0..2 {
              let img = render(&mut shaper, &dev, &fr, w, h, None, Some(i));
              let b = &lay.buttons[i];
              let bx0 = (m.shift as i32 + lay.rows[b.row].x0 + b.x) as u32;
              let bx1 = bx0 + b.w;
              let mut changed = 0usize;
              for y in 0..h {
                  for x in 0..w {
                      let (p, q) = (base.pixel(x, y), img.pixel(x, y));
                      if p == q {
                          continue;
                      }
                      changed += 1;
                      assert!(x >= bx0 && x < bx1, "hover {i}: pixel ({x},{y}) changed outside its box [{bx0},{bx1})");
                      assert_eq!((q[0], q[1], q[2]), (p[0], p[1], 0), "hover {i}: pixel ({x},{y}) is not the focus colour");
                  }
              }
              assert!(changed > 0, "hover {i} changed nothing");
              hovered.push(img);
          }
          assert_ne!(hovered[0].rgba, hovered[1].rgba, "hovering different buttons must differ");
      }
  ```
  Run `ERARS_REQUIRE_GPU=1 cargo test -p erars-renderer headless::tests -- --nocapture`; expect `test result: ok. 8 passed` (the dev box has an NVIDIA Vulkan adapter; CI installs lavapipe in T11). If `box_frame_ink_lands_in_cells` fails with "ink at x=… outside every glyph box", print the offending row's `boxes` and compare against `shift + x0 + x + dx + left` in `draw.rs` — the layout or draw offset is wrong, not this test.
  Commit: `git add crates/erars-renderer/src/headless.rs && git commit -m "test(renderer): headless pixel tests for cell containment, row identity and hover recolour"`

- [ ] **Step 9: `_cjk` variants and the opt-in MS Gothic strike tests.**
  Insert before the final `}` of `mod tests` in `headless.rs`:
  ```rust
      /// Same frame test with a real CJK font as the *fallback* (the bundled
      /// font stays primary, half_w 11): 18 px CJK glyphs are centred in their
      /// 22 px boxes and must not leak; the CJK row really resolves to the
      /// fallback face, not to the primary's `.notdef`.
      #[test]
      fn box_frame_ink_lands_in_cells_cjk() {
          let _gpu = gpu_lock();
          let Some(cjk) = ts::require_cjk_font() else { return };
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[bundled_font(), cjk]);
          let m = *shaper.metrics();
          assert_eq!((m.font_px, m.half_w, m.line_h, m.shift), (18, 11, 19, 3), "bundled font must stay primary");
          let primary = shaper.chain().primary();
          for c in ['漢', 'あ', '한'] {
              let (id, _) = shaper.chain().resolve(c, &StyleKey::plain());
              assert_ne!(id, primary, "{c} must come from the CJK fallback");
          }
          let fr = frame(vec![
              text_line("01234567", WHITE),
              text_line("漢字한글", WHITE),
              text_line("┏━━┓", WHITE),
              text_line("あ い", WHITE),
              text_line("Aあ漢B", WHITE),
          ]);
          let (w, h) = (200, 6 * m.line_h);
          let img = render(&mut shaper, &dev, &fr, w, h, None, None);
          let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
          assert_eq!(lay.rows[1].clusters.iter().map(|c| c.cells).collect::<Vec<_>>(), [2, 2, 2, 2]);
          assert_eq!(lay.rows[4].clusters.iter().map(|c| c.cells).collect::<Vec<_>>(), [1, 2, 2, 1]);
          assert_ink_in_boxes(&img, &lay, &m, h);
      }

      #[test]
      fn identical_rows_are_byte_identical_cjk() {
          let _gpu = gpu_lock();
          let Some(cjk) = ts::require_cjk_font() else { return };
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[bundled_font(), cjk]);
          let lh = shaper.metrics().line_h;
          let fr = frame(vec![text_line("漢字한글┏━┓Ab", WHITE); 4]);
          let h = 5 * lh;
          let img = render(&mut shaper, &dev, &fr, 300, h, None, None);
          let b1 = img.band(lh, 2 * lh);
          assert!(b1.iter().any(|&v| v != 0), "no ink rendered");
          assert_eq!(b1, img.band(2 * lh, 3 * lh));
          assert_eq!(b1, img.band(3 * lh, 4 * lh));
      }

      /// Size and popcount of the exact `ppem` strike of `c` in `font`
      /// (ttf-parser route, packed 1-bit).
      fn strike_stats(font: &cosmic_text::Font, c: char, ppem: u16) -> (u32, u32, u32) {
          use cosmic_text::ttf_parser::RasterImageFormat;
          let face = font.rustybuzz();
          let gid = face.glyph_index(c).expect("cmap");
          let img = face.glyph_raster_image(gid, ppem).expect("strike");
          assert_eq!(img.pixels_per_em, ppem, "{c}: strike ppem");
          assert_eq!(img.format, RasterImageFormat::BitmapMonoPacked, "{c}: strike format");
          let n = img.width as usize * img.height as usize;
          let pop = (0..n)
              .filter(|&i| (img.data[i >> 3] >> (7 - (i & 7))) & 1 == 1)
              .count() as u32;
          (img.width as u32, img.height as u32, pop)
      }

      /// MS Gothic at 18 px draws its embedded 1-bit strikes: every pixel in a
      /// glyph's cell box is 0 or 255 in white text, the white count equals the
      /// strike's set bits, `あ`/`漢`/`─` fill an 18×18 box and `A`/`═` a 9×18 one,
      /// and the 19th (slack) row stays empty.
      #[test]
      fn msgothic_18px_uses_bitmap_strikes() {
          let _gpu = gpu_lock();
          let Some(ms) = ts::msgothic_font() else { return };
          let Some(dev) = gpu_device() else { return };
          let mut shaper = jp_shaper(&[ms]);
          let m = *shaper.metrics();
          assert_eq!((m.font_px, m.half_w, m.line_h, m.baseline, m.shift), (18, 9, 19, 15, 3));
          let fr = frame(vec![text_line("Aあ漢─═", WHITE)]);
          let (w, h) = (120, 2 * m.line_h);
          let img = render(&mut shaper, &dev, &fr, w, h, None, None);
          let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
          let primary = {
              let id = shaper.chain().primary();
              shaper.chain().font(id)
          };
          let row = &lay.rows[0];
          let expected_cells = [1u8, 2, 2, 2, 1];
          assert_eq!(row.clusters.len(), 5);
          for (c, cells) in row.clusters.iter().zip(expected_cells) {
              assert_eq!(c.cells, cells, "{:?} cells", c.text);
              let ch = c.text.chars().next().unwrap();
              let x0 = (m.shift as i32 + row.x0 + c.x) as u32;
              let x1 = x0 + c.cells as u32 * m.half_w;
              let (sw, sh, pop) = strike_stats(&primary, ch, 18);
              assert_eq!((sw, sh), (c.cells as u32 * 9, 18), "{ch}: strike size");
              assert!(pop > 0, "{ch}: empty strike");
              let mut white = 0u32;
              for y in 0..m.font_px {
                  for x in x0..x1 {
                      let [r, g, b, _] = img.pixel(x, y);
                      assert!(
                          matches!((r, g, b), (0, 0, 0) | (255, 255, 255)),
                          "{ch}: pixel ({x},{y}) = {:?} is not 0/255",
                          (r, g, b)
                      );
                      if r == 255 {
                          white += 1;
                      }
                  }
              }
              assert_eq!(white, pop, "{ch}: white pixels != strike popcount");
              assert!(img.ink_bbox(x0, x1, m.font_px, m.line_h, 1).is_none(), "{ch}: ink in the line slack row");
          }
          assert_ink_in_boxes(&img, &lay, &m, h);
      }

      /// 23 px has no exact strike (ttf-parser would hand back the 22 ppem one):
      /// the outline path is used, so anti-aliased intermediate values appear.
      #[test]
      fn msgothic_23px_uses_outlines() {
          let _gpu = gpu_lock();
          let Some(ms) = ts::msgothic_font() else { return };
          let Some(dev) = gpu_device() else { return };
          let mut shaper = ts::test_shaper(&[ms], Language::Japanese, 23, 24);
          let m = *shaper.metrics();
          assert_eq!((m.font_px, m.half_w, m.line_h, m.baseline, m.shift), (23, 12, 24, 20, 3));
          let fr = frame(vec![text_line("Aあ", WHITE)]);
          let (w, h) = (120, 2 * m.line_h);
          let img = render(&mut shaper, &dev, &fr, w, h, None, None);
          let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
          let row = &lay.rows[0];
          let mut grey = 0usize;
          for c in &row.clusters {
              let x0 = (m.shift as i32 + row.x0 + c.x) as u32;
              let x1 = x0 + c.cells as u32 * m.half_w;
              assert!(img.ink_bbox(x0, x1, 0, m.line_h, 1).is_some(), "{:?}: nothing drawn", c.text);
              for y in 0..m.line_h {
                  for x in x0..x1 {
                      let [r, _, _, _] = img.pixel(x, y);
                      if r > 0 && r < 255 {
                          grey += 1;
                      }
                  }
              }
          }
          assert!(grey > 0, "no anti-aliased pixels — a strike was used at 23 px");
          assert_ink_in_boxes(&img, &lay, &m, h);
      }

      /// GPU-free companion: the 18 ppem strike of `あ` is exact, 18×18, packed;
      /// 23 ppem yields the *nearest* (22) strike, which the raster layer
      /// (`raster::strike_image`) rejects while accepting the exact one and
      /// decoding it to a 0/255 mask placed at top = baseline (15).
      #[test]
      fn msgothic_strike_metadata_gpu_free() {
          use cosmic_text::ttf_parser::RasterImageFormat;
          let Some(ms) = ts::msgothic_font() else { return };
          let mut chain = FontChain::from_files(&[ms], Language::Japanese);
          let font = chain.font(chain.primary());
          let face = font.rustybuzz();
          let gid = face.glyph_index('あ').expect("あ in cmap");
          assert_ne!(gid.0, 0);
          let img = face.glyph_raster_image(gid, 18).expect("18 ppem strike");
          assert_eq!(img.pixels_per_em, 18);
          assert_eq!((img.width, img.height, img.x, img.y), (18, 18, 0, -3));
          assert_eq!(img.format, RasterImageFormat::BitmapMonoPacked);
          let near = face.glyph_raster_image(gid, 23).expect("nearest strike");
          assert_eq!(near.pixels_per_em, 22, "ttf-parser picks the nearest strike");
          assert!(crate::raster::strike_image(&font, gid.0, 23).is_none(), "22 ppem strike must be rejected for 23 px");
          let mask = crate::raster::strike_image(&font, gid.0, 18).expect("exact strike accepted");
          assert_eq!((mask.width, mask.height, mask.left, mask.top, mask.color), (18, 18, 0, 15, false));
          let alphas: Vec<u8> = mask.rgba.chunks_exact(4).map(|p| p[3]).collect();
          assert!(alphas.iter().all(|&a| a == 0 || a == 255), "mask must be 0/255");
          assert_eq!(alphas.iter().filter(|&&a| a == 255).count(), 61, "あ @18 has 61 set bits");
      }

      /// The classifier and MS Gothic agree on the JIS box set: the 32 JIS X 0208
      /// box characters are 2 cells / a full em, `═`/`║` 1 cell / half an em.
      #[test]
      fn msgothic_jis_box_drawing_is_full_width_and_double_lines_half() {
          let Some(ms) = ts::msgothic_font() else { return };
          let widths = WidthTable::new(Language::Japanese.encoding());
          let mut chain = FontChain::from_files(&[ms], Language::Japanese);
          let font = chain.font(chain.primary());
          let face = font.rustybuzz();
          let upem = face.units_per_em() as u32;
          assert_eq!(upem, 256);
          let adv = |c: char| face.glyph_hor_advance(face.glyph_index(c).expect("cmap")).unwrap() as u32;
          let jis = "─│┌┐┘└├┬┤┴┼━┃┏┓┛┗┣┳┫┻╋┠┯┨┷┿┝┰┥┸╂";
          assert_eq!(jis.chars().count(), 32);
          for c in jis.chars() {
              assert_eq!(widths.char_cells(c), 2, "{c} cells");
              assert_eq!(adv(c), upem, "{c} advance");
          }
          for c in ['═', '║'] {
              assert_eq!(widths.char_cells(c), 1, "{c} cells");
              assert_eq!(adv(c) * 2, upem, "{c} advance");
          }
      }
  ```
  Run, in order:
  1. `ERARS_REQUIRE_GPU=1 ERARS_REQUIRE_CJK_FONT=1 cargo test -p erars-renderer headless::tests -- --nocapture` — expect `test result: ok. 14 passed` on this box; the four `msgothic_*` tests print `SKIP headless::tests::msgothic_…: msgothic.ttc not found under ERARS_FONT_DIR` and pass.
  2. `ERARS_FONT_DIR=/home/riey/repos/erars ERARS_REQUIRE_GPU=1 cargo test -p erars-renderer msgothic -- --nocapture` — expect the four `headless::tests::msgothic_*` tests (and T8's two `raster::` MS Gothic tests) to run their assertions with no SKIP line. (`msgothic.ttc` is untracked at the repo root; never `git add` it — T11 adds it to `.gitignore`.)
  Commit: `git add crates/erars-renderer/src/headless.rs && git commit -m "test(renderer): CJK fallback and opt-in MS Gothic bitmap-strike pixel tests"`

- [ ] **Step 10: Whole-crate check and a real headless shot.**
  Run `ERARS_REQUIRE_GPU=1 cargo test -p erars-renderer -p erars-proxy-system -- --nocapture 2>&1 | grep -E "SKIP|test result"`; expect every `test result:` line `ok`; the only `SKIP` lines are the MS Gothic-gated ones (`headless::tests::msgothic_*`, `raster::tests::*ms_gothic*`/`rasterize_prefers_strikes_only_when_allowed`, `font::chain_tests::msgothic_*`, T6's two `text::tests` MS Gothic tests) plus, on a box without an upright regular+bold system family, `font::chain_tests::real_bold_face_is_preferred_over_synthesis`. Then
  ```
  cargo run -q -p erars-renderer -- --quite --headless-shot /tmp/claude-1000/-home-riey-repos-erars/50a48b53-7d56-447e-a93a-55727276ea60/scratchpad/t9-shot.png . && python3 -c "from PIL import Image; im = Image.open('/tmp/claude-1000/-home-riey-repos-erars/50a48b53-7d56-447e-a93a-55727276ea60/scratchpad/t9-shot.png'); im.load(); print(im.mode, im.size)"
  ```
  expect `Wrote … (760x480)` then `RGBA (760, 480)` (the T2 window defaults; the bottom 19 rows show `> _` in grey). Nothing to commit if clean.

---

### Task 10: App + CLI

**Files:**
- Create: `crates/erars-renderer/src/lib.rs` (new library target: the binary becomes a thin CLI over it so `tests/tui.rs` in Task 11 can reach the modules — a `tests/` integration test cannot import from a bin-only crate)
- Modify (full rewrite, all 356 lines): `crates/erars-renderer/src/app.rs`
- Modify (full rewrite; after T9 it is the original 205-line file with the `--headless-shot` shim from T9 Step 7): `crates/erars-renderer/src/main.rs`
- Modify: `crates/erars-renderer/src/font.rs` — delete the legacy `FontCtx` block (today's lines 3–139: the doc comment `/// Owns the cosmic-text FontSystem plus the bundled fallback` through the closing brace of `fn measure_cell_w`, *except* the `pub const BUNDLED_FONT` line) and the legacy `#[cfg(test)] mod tests` (today's lines 141–169: `resolve_prefers_first_installed_candidate`, `cell_metrics_are_positive`); T5 left both in place. Keep everything T5 added (`bundled_font_path`, `FontConfig`, `StyleKey`, `language_candidates`, `family_eq`, `find_family`, `font_covers`, `face_covers`, `load_dir`, `FontChain`, `mod chain_tests`).
- Modify: `crates/erars-renderer/src/text.rs` — delete the legacy block T6 kept (from the banner `// Legacy grid shaper — still called by grid.rs and atlas.rs tests; T10 deletes` through the closing brace of `impl CellShaper`, i.e. `use cosmic_text::{Attrs, Buffer, CacheKey, Family, Metrics, Shaping}; use unicode_width::UnicodeWidthStr; use crate::font::FontCtx; struct PlacedGlyph; struct ShapedRun; struct CellShaper; impl CellShaper`).
- Modify: `crates/erars-renderer/src/draw.rs` — delete `build_instances_legacy` (T8 Step 14 kept it) and its imports `use cosmic_text::{FontSystem, SwashCache};`, `use crate::atlas::GlyphAtlas;`, `use crate::grid::Grid;`.
- Modify: `crates/erars-renderer/Cargo.toml` — `[dependencies]` block (after T5–T9 it holds `winit … cosmic-text = "0.12.1"` plus `swash`, `rustybuzz`, `smol_str`, `bitflags`, `flate2`, `crc32fast`; `unicode-width` and `sys-locale` are still there) and a new `[lib]` table.
- Delete: `crates/erars-renderer/src/grid.rs`, `crates/erars-renderer/src/atlas.rs`
- Test: `#[cfg(test)] mod tests` in `crates/erars-renderer/src/app.rs` (GPU-free, bundled font only); `--headless-shot` smoke run on the repo-root sample game.

**Interfaces:**
- Consumes (T2): `EraConfig.{fore_color, bg_color, focus_color}: [u8; 3]`, `EraConfig.{font_size, line_height, window_width, window_height}: u32`, `EraConfig.font_family: String`, `EraConfig.lang: Language` (`Language: Clone + Copy`), `Language::encoding(&self) -> &'static encoding_rs::Encoding`
- Consumes (T1): `erars_ui::width::WidthTable::new(&'static Encoding)`
- Consumes (T5): `FontChain::from_files(files: &[PathBuf], lang: Language) -> FontChain`, `FontChain::primary(&self) -> fontdb::ID`, `FontChain::font(&mut self, id: fontdb::ID) -> Arc<cosmic_text::Font>`
- Consumes (T6): `#[derive(Clone, Copy, Debug, PartialEq)] pub struct CellMetrics { pub scale: f32, pub font_px: u32, pub half_w: u32, pub line_h: u32, pub baseline: u32, pub shift: u32 }`, `CellMetrics::from_primary(font: &cosmic_text::Font, font_size: u32, line_height: u32, scale: f32) -> Self`, `Shaper::new(chain: FontChain, widths: WidthTable, m: CellMetrics) -> Self`, `Shaper::metrics(&self) -> &CellMetrics`, `Shaper::chain(&mut self) -> &mut FontChain`, `Shaper::set_metrics(&mut self, m: CellMetrics)`
- Consumes (T7): `Geometry { pub content_w: u32, pub drawable_w: u32, pub m: CellMetrics }` + `Geometry::new(content_w: u32, m: CellMetrics) -> Geometry`; `#[derive(Clone, Debug, Default)] Layout { pub rows: Vec<Row>, pub buttons: Vec<ButtonRegion> }`; `Row.x0: i32`; `ButtonRegion { pub row: usize, pub x: i32, pub w: u32, pub input_gen: u32, pub value: Value }` (`x` row-relative before `x0`; draw/hit x = `shift + x0 + x`); `pub fn layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper) -> Layout` (T7 provides **no** hit-test method — the geometry lives in `app::hit_button` below, per spec Component 5/7)
- Consumes (T8): `#[derive(Clone, Copy, Debug, PartialEq, Eq)] draw::View { pub scroll_rows: usize, pub view_h: u32, pub strip_h: u32 }` + `View::visible_rows(&self, line_h: u32) -> usize`, `View::row_y(&self, rows: usize, r: usize, line_h: u32) -> Option<i32>`, `View::strip(&self) -> View`; `draw::build_instances(layout: &Layout, view: &View, hover: Option<usize>, hl: [u8; 3], raster: &mut GlyphRaster, device: &wgpu::Device, queue: &wgpu::Queue, shaper: &mut Shaper) -> Vec<Vec<Instance>>`; `raster::GlyphRaster::new(device: &wgpu::Device, use_bitmap_strikes: bool) -> Self`, `GlyphRaster::pages_with<'a>(&'a self, buckets: &'a [Vec<Instance>]) -> Vec<(&'a wgpu::TextureView, &'a [Instance])>`; `gpu::GpuContext::{new(&wgpu::Instance, wgpu::Surface<'static>, u32, u32), resize(&mut self, u32, u32), size(&self) -> (u32, u32), render(&mut self, pages: &[(&wgpu::TextureView, &[Instance])], bg: [u8; 3])}` with `pub device`, `pub queue`; `gpu::Instance: Copy`
- Consumes (T9): `erars_proxy_system::ConsoleFrame { bg_color: Color, hl_color: Color, fore_color: Color, lines: Vec<ConsoleLine> }` (`Default`); `headless::shaper_for(config: &EraConfig, game_dir: &Path) -> Shaper` (T9 Step 7; `FontChain::new` + `CellMetrics::from_primary(.., 1.0)`); `headless::render_frame(shaper: &mut Shaper, frame: &ConsoleFrame, content_w: u32, height: u32, input: Option<&str>, hover: Option<usize>) -> Option<Rendered>`; `headless::Rendered { width: u32, height: u32, rgba: Vec<u8> }`; `headless::write_png(path: &str, img: &Rendered) -> std::io::Result<()>`; `test_support` (a plain module after T9, no `#![cfg(test)]` inner attribute)
- Consumes (existing): `erars_proxy_system::{ProxyReceiver { req_rx, res_tx }, SystemRequest, SystemResponse, new_proxy}`, `erars_loader::{load_config, load_script, run_script}`, `erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, InputRequest, InputRequestType, TextStyle}`, winit 0.30.5 `ApplicationHandler`, `WindowEvent::{Resized, ScaleFactorChanged, RedrawRequested, CursorMoved, CursorLeft, MouseInput, MouseWheel, KeyboardInput, CloseRequested}`, `MouseScrollDelta::{LineDelta(f32, f32), PixelDelta(PhysicalPosition<f64>)}`
- Produces: `erars_renderer` library crate (`crates/erars-renderer/src/lib.rs`) with `pub mod {app, draw, flags, font, gpu, headless, layout, raster, test_support, text}`; `erars_renderer::app::{App, AppConfig, Wake}`; `App::new(shaper: Shaper, receiver: ProxyReceiver, cfg: AppConfig) -> App`; `#[derive(Debug, Clone, Copy)] AppConfig { pub font_size: u32, pub line_height: u32, pub default_fg: [u8; 3], pub init_size: (u32, u32), pub use_bitmap_strikes: bool }`; pure helpers `app::{max_scroll(rows: usize, visible: usize) -> usize, clamp_scroll(requested: i64, max: usize) -> usize, wheel_rows(acc: &mut f64, delta: f64, line_h: u32) -> i64, row_at(rows: usize, view: &View, line_h: u32, y: i64) -> Option<(usize, i32)>, hit_button(layout: &Layout, g: &Geometry, view: &View, active_gen: Option<u32>, cursor: (i64, i64)) -> Option<usize>, input_line(input: &str, fg: [u8; 3]) -> ConsoleLine}`; CLI flags `--headless-shot PATH.png`, `--no-bitmap-strikes`; env `ERARS_FONT_DIR`; Cargo `[lib] doctest = false`.

Decisions taken here (also in open_questions): the input strip is drawn only while an input request is pending, but its `line_h` is always reserved (`view_h = window_h − line_h`); the strip goes through `layout()` as a one-line `Layout` drawn with `View::strip()` (T8), cached and rebuilt only when the input text, metrics or width change; its colour is `frame.fore_color` (T9), with `AppConfig.default_fg` (= `emuera.config` 文字色) as the value before the first frame; `--no-bitmap-strikes` affects the window path only (`render_frame` is bitmap-strikes-on by T9's signature); the row under the cursor is derived from T8's `View::row_y` arithmetic (`row_at`) and the button from the spec's hit rect `[shift + x0 + x, row_y, w + 1, min(font_px + 1, line_h)]` — Emuera's inclusive `PointX ≤ x ≤ PointX + Width`, `0 ≤ y − row_y ≤ FontSize` — evaluated in `app::hit_button` over `Layout.buttons` (T7 deliberately provides no hit-test method; only fragments of the active input generation count); wheel `LineDelta` accumulates fractional notches, `PixelDelta` accumulates pixels and converts at `line_h` per row, positive y (wheel up) reveals older rows; `[lib] doctest = false` because the T7 doc comment of `layout_snapshot` carries a bare ``` block that rustdoc would compile as Rust once a library target exists.

All view arithmetic below was verified with a bare-rustc probe against verbatim copies of T8's `View::{visible_rows, row_y, strip}` and the inclusive hit-rect rule (`scratchpad/probe-plan-10/rowat.rs`, prints `ok`); the assembler re-derived every `hit_button` expectation of Step 2 by hand against the `hit_button` body of Step 6.

- [ ] **Step 1: Restructure — delete `grid.rs`/`atlas.rs`, add `lib.rs`, replace `main.rs` with the final CLI**

```bash
cd /home/riey/repos/erars && git rm -q crates/erars-renderer/src/grid.rs crates/erars-renderer/src/atlas.rs && ls crates/erars-renderer/src
```
Expected listing: `app.rs draw.rs flags.rs font.rs gpu.rs headless.rs layout.rs main.rs raster.rs shader.wgsl test_support.rs text.rs`.

Create `crates/erars-renderer/src/lib.rs`:

```rust
//! erars GUI renderer: Emuera-parity fixed-cell text layout on wgpu.
//!
//! The binary (`main.rs`) is a thin CLI over these modules; keeping them in a
//! library lets `tests/tui.rs` compile a game, lay it out and render it
//! through exactly the code the window uses.

pub mod app;
pub mod draw;
pub mod flags;
pub mod font;
pub mod gpu;
pub mod headless;
pub mod layout;
pub mod raster;
#[doc(hidden)]
pub mod test_support;
pub mod text;
```

`test_support` is deliberately not `#[cfg(test)]`: Task 11's `tests/tui.rs` calls `erars_renderer::test_support::{gpu_lock, gpu_device}`. If `test_support.rs` starts with a `#![cfg(test)]` line, delete that line.

Replace `crates/erars-renderer/src/main.rs` entirely with:

```rust
#![windows_subsystem = "windows"]

use std::path::Path;
use std::sync::Arc;

use erars_compiler::EraConfig;
use erars_loader::{load_config, load_script, run_script};
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest};
use erars_renderer::app::{App, AppConfig, Wake};
use erars_renderer::headless;
use erars_renderer::text::Shaper;
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
    #[clap(
        long,
        value_name = "PATH.png",
        help = "Headless: render the first screen to a PNG image and exit (no display needed)"
    )]
    headless_shot: Option<String>,
    #[clap(
        long,
        help = "Never use embedded bitmap strikes (e.g. MS Gothic 10-22 px); always rasterize outlines"
    )]
    no_bitmap_strikes: bool,
}

/// Spawn the VM runtime thread driving `system`.
fn spawn_vm(
    target_path: String,
    load: bool,
    lint: bool,
    system: erars_proxy_system::ProxySystem,
    config: EraConfig,
) {
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .name("erars-runtime".into())
        .spawn(move || {
            let system_back = system.clone();
            let system = Box::new(system);
            let ret = if load {
                unsafe { load_script(&target_path, system, config) }
            } else {
                run_script(&target_path, system, config, false, lint)
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
}

/// Headless capture: run the game until it first waits for input, then render
/// that screen (`window_width × window_height`; the input strip is shown when
/// the game is waiting for input) to a PNG file and exit. No window/display.
fn headless_shot(mut shaper: Shaper, receiver: ProxyReceiver, (w, h): (u32, u32), path: &str) {
    let mut frame = ConsoleFrame::default();
    let mut input: Option<&str> = None;
    // Drain requests until the game blocks for input (screen is settled).
    loop {
        match receiver.req_rx.recv() {
            Ok(SystemRequest::Redraw(f)) => frame = f,
            Ok(SystemRequest::Input(_)) => {
                input = Some("");
                break;
            }
            Ok(SystemRequest::Quit) | Err(_) => break,
        }
    }
    match headless::render_frame(&mut shaper, &frame, w, h, input, None) {
        Some(img) => match headless::write_png(path, &img) {
            Ok(()) => println!("Wrote {path} ({}x{})", img.width, img.height),
            Err(e) => eprintln!("Failed to write {path}: {e}"),
        },
        None => eprintln!("No GPU adapter available for headless rendering"),
    }
}

fn main() {
    use flexi_logger::*;
    let args: Args = clap::Parser::parse();

    let _handle = if args.quite {
        None
    } else {
        Some(
            Logger::try_with_str(format!(
                "warn,wgpu_hal=off,naga=warn,erars={level},erars_renderer={level}",
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
    let target_path = args.target_path.clone();
    let init_size = (config.window_width, config.window_height);
    let app_cfg = AppConfig {
        font_size: config.font_size,
        line_height: config.line_height,
        default_fg: config.fore_color,
        init_size,
        use_bitmap_strikes: !args.no_bitmap_strikes,
    };
    // Fonts: configured family → <game>/font → ERARS_FONT_DIR → per-language
    // CJK monospace → bundled Noto Sans Mono; metrics at scale 1.0 (the
    // window applies its real scale factor through Shaper::set_metrics).
    let shaper = headless::shaper_for(&config, Path::new(&target_path));

    // Headless capture mode: no window, no display server required.
    if let Some(path) = args.headless_shot.clone() {
        let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(|| {}));
        spawn_vm(target_path, args.load, !args.lint_off, system, config);
        headless_shot(shaper, receiver, init_size, &path);
        return;
    }

    let event_loop = EventLoop::<Wake>::with_user_event().build().unwrap();
    let proxy = event_loop.create_proxy();
    let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(move || {
        let _ = proxy.send_event(Wake);
    }));
    spawn_vm(target_path, args.load, !args.lint_off, system, config);

    let mut app = App::new(shaper, receiver, app_cfg);
    event_loop.run_app(&mut app).unwrap();
}
```

Check: `grep -c "^mod \|^pub mod \|font_candidates\|FontCtx" crates/erars-renderer/src/main.rs` → `0` (the CLI declares no modules and knows nothing about the old font code). The binary does not build until Step 7 (`App` is rewritten in Steps 2–7); every check until then uses `--lib`.

- [ ] **Step 2: `app.rs` — skeleton with the failing unit tests**

Replace `crates/erars-renderer/src/app.rs` entirely with the imports, `Wake`, `AppConfig` and the test module. The helpers the tests call do not exist yet.

```rust
//! winit application: owns the VM proxy, the shaper, the last layout and the
//! view state (scroll / hover / cursor / pending input).
//!
//! The layout is recomputed only when the frame, the content width or the
//! cell metrics change (spec Component 5). Hover, the active input generation
//! and scrolling change only what is drawn: Emuera recolours the pointed
//! button at draw time and never moves anything.
//!
//! View state: `scroll_rows` = whole rows hidden below the bottom of the row
//! area (0 = stuck to the bottom); `strip_h = line_h`; `view_h = window_h −
//! strip_h`; rows are bottom-anchored (slack at the top) — the arithmetic is
//! `draw::View`'s. The input strip shows `> {input}_` in the default colour.

use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use erars_ast::{Alignment, Value};
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest, SystemResponse};
use erars_ui::{
    Color, ConsoleLine, ConsoleLinePart, FontStyle, InputRequest, InputRequestType, TextStyle,
};
use winit::application::ApplicationHandler;
use winit::event::{ElementState, MouseScrollDelta, WindowEvent};
use winit::event_loop::ActiveEventLoop;
use winit::keyboard::{Key, NamedKey};
use winit::window::{Window, WindowId};

use crate::draw::{build_instances, View};
use crate::gpu::{GpuContext, Instance};
use crate::layout::{layout, Geometry, Layout};
use crate::raster::GlyphRaster;
use crate::text::{CellMetrics, Shaper};

/// User event used to wake the loop when the VM sends a request.
#[derive(Debug, Clone, Copy)]
pub struct Wake;

/// Static configuration the app needs besides the shaper.
#[derive(Debug, Clone, Copy)]
pub struct AppConfig {
    /// `emuera.config` フォントサイズ (logical px).
    pub font_size: u32,
    /// `emuera.config` 一行の高さ (logical px).
    pub line_height: u32,
    /// `emuera.config` 文字色 — input strip colour until the first frame
    /// (afterwards `ConsoleFrame::fore_color`, the console's default colour).
    pub default_fg: [u8; 3],
    /// Initial window inner size (logical px): ウィンドウ幅 × ウィンドウ高さ.
    pub init_size: (u32, u32),
    /// `false` with `--no-bitmap-strikes`.
    pub use_bitmap_strikes: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::FontChain;
    use erars_compiler::Language;
    use erars_ui::width::WidthTable;
    use std::path::PathBuf;

    const BUNDLED: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf");
    /// Pinned metrics (spec Testing §4): row geometry is font-independent.
    const M: CellMetrics = CellMetrics {
        scale: 1.0,
        font_px: 18,
        half_w: 9,
        line_h: 19,
        baseline: 15,
        shift: 3,
    };
    /// 480 px window, 19 px strip → 461 px row area → 24 rows, 5 px slack on top.
    const VIEW: View = View {
        scroll_rows: 0,
        view_h: 461,
        strip_h: 19,
    };

    fn shaper() -> Shaper {
        let lang = Language::Korean;
        Shaper::new(
            FontChain::from_files(&[PathBuf::from(BUNDLED)], lang),
            WidthTable::new(lang.encoding()),
            M,
        )
    }

    fn style() -> TextStyle {
        TextStyle {
            color: Color([255, 255, 255]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        }
    }

    fn text(s: &str) -> ConsoleLinePart {
        ConsoleLinePart::Text(s.to_owned(), style())
    }

    fn button(s: &str, gen: u32, v: i64) -> ConsoleLinePart {
        ConsoleLinePart::Button(vec![(s.to_owned(), style())], gen, Value::Int(v))
    }

    fn line(align: Alignment, parts: Vec<ConsoleLinePart>) -> ConsoleLine {
        ConsoleLine {
            align,
            button_start: None,
            parts,
        }
    }

    /// 30 one-row lines. Buttons in layout order: #0 line 5 gen 7; #1 line 6
    /// gen 6 (inactive); #2 line 6 gen 7 at x = 45 (after "BBBB" + " ");
    /// #3 line 29 gen 7 at x = 18 on a Right-aligned row ("AB" + "DDDD").
    fn thirty_lines() -> Vec<ConsoleLine> {
        (0..30)
            .map(|i| match i {
                5 => line(Alignment::Left, vec![button("AAAA", 7, 2)]),
                6 => line(
                    Alignment::Left,
                    vec![button("BBBB", 6, 3), text(" "), button("CCCC", 7, 4)],
                ),
                29 => line(Alignment::Right, vec![text("AB"), button("DDDD", 7, 1)]),
                _ => line(Alignment::Left, vec![text("row")]),
            })
            .collect()
    }

    #[test]
    fn scroll_arithmetic() {
        assert_eq!(max_scroll(30, 24), 6);
        assert_eq!(max_scroll(10, 24), 0);
        assert_eq!(clamp_scroll(-3, 10), 0);
        assert_eq!(clamp_scroll(12, 10), 10);
        assert_eq!(clamp_scroll(4, 10), 4);
        assert_eq!(clamp_scroll(4, 0), 0);
    }

    #[test]
    fn wheel_pixels_accumulate_into_rows() {
        let mut acc = 0.0;
        assert_eq!(wheel_rows(&mut acc, 10.0, 19), 0);
        assert_eq!(wheel_rows(&mut acc, 10.0, 19), 1);
        assert!((acc - 1.0).abs() < 1e-9);
        assert_eq!(wheel_rows(&mut acc, -40.0, 19), -2);
        assert!((acc + 1.0).abs() < 1e-9);
        acc = 0.0;
        assert_eq!(wheel_rows(&mut acc, 0.5, 0), 0);
    }

    #[test]
    fn row_at_matches_the_drawer_and_rejects_slack_and_strip() {
        // 30 rows, 24 visible: rows 6..=29 on screen, row 6 at top = 5.
        assert_eq!(row_at(30, &VIEW, 19, 2), None, "top slack");
        assert_eq!(row_at(30, &VIEW, 19, 5), Some((6, 0)));
        assert_eq!(row_at(30, &VIEW, 19, 23), Some((6, 18)));
        assert_eq!(row_at(30, &VIEW, 19, 24), Some((7, 0)));
        assert_eq!(row_at(30, &VIEW, 19, 460), Some((29, 18)));
        assert_eq!(row_at(30, &VIEW, 19, 461), None, "input strip");
        assert_eq!(row_at(30, &VIEW, 19, -1), None);
        assert_eq!(row_at(0, &VIEW, 19, 100), None);
        // 3 rows: bottom-anchored, row 0 at 461 − 3·19 = 404.
        assert_eq!(row_at(3, &VIEW, 19, 404), Some((0, 0)));
        assert_eq!(row_at(3, &VIEW, 19, 403), None);
        // Scrolled by one row: bottom row 28, row 5 at top = 5.
        let v1 = View {
            scroll_rows: 1,
            ..VIEW
        };
        assert_eq!(row_at(30, &v1, 19, 5), Some((5, 0)));
        assert_eq!(row_at(30, &v1, 19, 460), Some((28, 18)));
        // Every on-screen pixel maps to the row View::row_y draws there.
        for rows in [0usize, 1, 5, 24, 30] {
            for scroll in [0usize, 1, 6, 100] {
                let v = View {
                    scroll_rows: scroll,
                    ..VIEW
                };
                for y in -5i64..=480 {
                    match row_at(rows, &v, 19, y) {
                        Some((r, dy)) => {
                            let top = v.row_y(rows, r, 19).unwrap() as i64;
                            assert!(y >= top && y < top + 19, "rows {rows} scroll {scroll} y {y}");
                            assert_eq!(dy as i64, y - top);
                        }
                        None => {
                            for r in 0..rows {
                                if let Some(top) = v.row_y(rows, r, 19) {
                                    let top = top as i64;
                                    assert!(
                                        !(y >= top && y < top + 19 && y < v.view_h as i64),
                                        "missed rows {rows} scroll {scroll} y {y} r {r}"
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn input_line_is_default_coloured_plain_text() {
        let l = input_line("12", [192, 192, 192]);
        assert_eq!(l.align, Alignment::Left);
        match &l.parts[..] {
            [ConsoleLinePart::Text(s, st)] => {
                assert_eq!(s, "> 12_");
                assert_eq!(st.color, Color([192, 192, 192]));
                assert_eq!(st.font_style, FontStyle::NORMAL);
            }
            other => panic!("unexpected parts {other:?}"),
        }
    }

    #[test]
    fn hit_test_uses_emuera_inclusive_rects_and_whole_rows() {
        let mut sh = shaper();
        let g = Geometry::new(760, M);
        let l = layout(&thirty_lines(), &g, &mut sh);
        assert_eq!(l.rows.len(), 30);
        assert_eq!(l.buttons.len(), 4);
        // button 3: row 29 (bottom, top = 442), x = 18 after "AB", w = 36; Right → x0 = 760 − 54.
        let x0 = l.rows[29].x0 as i64;
        assert_eq!(x0, 706, "Right = content_w - width");
        let left = 3 + x0 + 18;
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left, 442)), Some(3));
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left + 36, 460)), Some(3), "inclusive right/bottom");
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left + 37, 450)), None);
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left - 1, 450)), None);
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left, 461)), None, "strip");
        assert_eq!(hit_button(&l, &g, &VIEW, Some(8), (left, 450)), None, "stale gen");
        assert_eq!(hit_button(&l, &g, &VIEW, None, (left, 450)), None, "no request");
        // row 5 is hidden above the row area (top = −14): never hit, even at y in [0, 5).
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (3, 2)), None);
        // row 6 sits at top = 5: button 1 is gen 6 (inactive); button 2 starts at 3 + 45 = 48.
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (10, 10)), None);
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (48, 5)), Some(2));
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (84, 23)), Some(2));
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (84, 24)), None);
        // scrolled by one row: bottom row 28, row 5 now at top = 5, row 29 gone.
        let v1 = View {
            scroll_rows: 1,
            ..VIEW
        };
        assert_eq!(hit_button(&l, &g, &v1, Some(7), (3, 5)), Some(0));
        assert_eq!(hit_button(&l, &g, &v1, Some(7), (3, 4)), None);
        assert_eq!(hit_button(&l, &g, &v1, Some(7), (left, 442)), None);
        // line_h 22 with font 18: Emuera's band is FontSize + 1 px, so the
        // bottom 3 px of each row miss (row 29 top = 458 − 22 = 436).
        let g22 = Geometry::new(760, CellMetrics { line_h: 22, ..M });
        let v22 = View {
            scroll_rows: 0,
            view_h: 458,
            strip_h: 22,
        };
        assert_eq!(hit_button(&l, &g22, &v22, Some(7), (left, 454)), Some(3));
        assert_eq!(hit_button(&l, &g22, &v22, Some(7), (left, 455)), None);
    }
}
```

- [ ] **Step 3: `font.rs` — delete the legacy `FontCtx`**

In `crates/erars-renderer/src/font.rs` delete, in this order:
1. From the doc comment line `/// Owns the cosmic-text FontSystem plus the bundled fallback, and the cell` (the line after the imports) through the closing `}` of `fn measure_cell_w` (the function whose body ends with `font_size * 0.6 // safety fallback if measurement failed` … `w` … `}`), **keeping** the two lines `/// Bundled Latin monospace fallback, always available regardless of OS.` and `pub const BUNDLED_FONT: &[u8] = include_bytes!("../assets/NotoSansMono-Regular.ttf");` — move them so they sit directly after the imports. This removes `struct FontCtx`, `impl FontCtx` (`new`, `with_candidates`, `set_scale`, `recompute`), `resolve_default_family`, `family_exists`, `measure_cell_w`.
2. The legacy test module at the end of the file — the `#[cfg(test)] mod tests { … }` that contains `resolve_prefers_first_installed_candidate` and `cell_metrics_are_positive` (T5's `mod chain_tests` stays).
3. In the `use cosmic_text::{…}` line remove `Attrs`, `Buffer`, `Family`, `Metrics`, `Shaping` so it reads `use cosmic_text::{fontdb, ttf_parser, Font, FontSystem};` (T5's `FontChain` still owns a `FontSystem`).
4. In the `//!` header T5 wrote at the top of `font.rs`, replace the two lines
```
//! selection. `FontCtx` below it is the legacy cosmic-text `Buffer` path that
//! the old grid renderer still uses; Task 10 deletes it.
```
with the single line
```
//! selection (spec Component 3).
```
(otherwise the `FontCtx` grep below — and Step 9's crate-wide one — keeps hitting the doc comment).

Check:

```bash
cd /home/riey/repos/erars && grep -n "FontCtx\|measure_cell_w\|resolve_default_family\|family_exists\|sys_locale\|Shaping" crates/erars-renderer/src/font.rs; echo "exit=$? (1 = clean)"; grep -c '^use cosmic_text::{fontdb, ttf_parser, Font, FontSystem};$' crates/erars-renderer/src/font.rs; grep -c "pub const BUNDLED_FONT\|pub fn bundled_font_path\|pub struct FontChain\|mod chain_tests" crates/erars-renderer/src/font.rs
```
Expected: no grep output, `exit=1`, then `1` (the pruned import line), then `4`. (Prose mentions of `Buffer`/`Attrs` in T5's doc comments are fine; only code may not use them — the compiler check in Step 5 enforces that.)

- [ ] **Step 4: `text.rs` — delete the legacy grid shaper**

In `crates/erars-renderer/src/text.rs` delete from the banner

```
// ---------------------------------------------------------------------------
// Legacy grid shaper — still called by grid.rs and atlas.rs tests; T10 deletes
// it together with grid.rs / atlas.rs. Do not extend.
// ---------------------------------------------------------------------------
```

through the closing `}` of `impl CellShaper` (inclusive: `use cosmic_text::{Attrs, Buffer, CacheKey, Family, Metrics, Shaping};`, `use unicode_width::UnicodeWidthStr;`, `use crate::font::FontCtx;`, `pub struct PlacedGlyph`, `pub struct ShapedRun`, `pub struct CellShaper`, `impl CellShaper { pub fn shape_run(…) }`). Nothing else in the file references these names (T6 put all new code above the banner and the tests below it).

Check:

```bash
cd /home/riey/repos/erars && grep -n "Legacy grid shaper\|CellShaper\|PlacedGlyph\|ShapedRun\|FontCtx\|unicode_width\|CacheKey" crates/erars-renderer/src/text.rs; echo "exit=$? (1 = clean)"
```
Expected: no output, `exit=1`.

- [ ] **Step 5: `draw.rs` — delete `build_instances_legacy`; first lib compile**

In `crates/erars-renderer/src/draw.rs` delete the whole `pub fn build_instances_legacy(…) { … }` (including the comment `// Legacy Grid path (still used by app.rs/headless.rs); deleted in T10 with grid.rs/atlas.rs.` and its doc comment) and the three legacy imports `use cosmic_text::{FontSystem, SwashCache};`, `use crate::atlas::GlyphAtlas;`, `use crate::grid::Grid;`. Keep `use crate::gpu::Instance;`, `use crate::layout::Layout;`, `use crate::raster::{AtlasRegion, GlyphRaster, RasterKey};`, `use crate::text::{CellMetrics, ShapedGlyph, Shaper};`.

Now the library compiles except for the helpers the app tests call:

```bash
cd /home/riey/repos/erars && cargo test -p erars-renderer --lib app:: 2>&1 | grep -E "^error" | sort -u
```
Expected exactly these (order may differ): `error[E0425]: cannot find function \`max_scroll\` in this scope`, and the same for `clamp_scroll`, `wheel_rows`, `row_at`, `hit_button`, `input_line`. Any other `error` (an unresolved `crate::grid`, `crate::atlas`, `FontCtx`, `CellShaper`, `build_instances_legacy`, `SwashCache`) is a leftover from Steps 3–5 — remove the referencing code, it is dead.

- [ ] **Step 6: `app.rs` — the pure helpers**

Insert between `pub struct AppConfig { … }` and `#[cfg(test)] mod tests`:

```rust
// ---------------------------------------------------------------------------
// Pure view arithmetic (spec Component 5 "View state"); unit-tested below.
// Row placement is `draw::View`'s; button geometry is Emuera's inclusive hit
// rect over `Layout.buttons` (spec Component 5 "Buttons").
// ---------------------------------------------------------------------------

/// Largest `scroll_rows` that still keeps the row area full.
pub fn max_scroll(rows: usize, visible: usize) -> usize {
    rows.saturating_sub(visible)
}

/// Clamp a requested scroll position into `[0, max]`.
pub fn clamp_scroll(requested: i64, max: usize) -> usize {
    requested.clamp(0, max as i64) as usize
}

/// Convert accumulated wheel pixels into whole rows (sign = direction) and
/// keep the remainder in `acc`.
pub fn wheel_rows(acc: &mut f64, delta: f64, line_h: u32) -> i64 {
    *acc += delta;
    let unit = line_h.max(1) as f64;
    let rows = (*acc / unit).trunc();
    *acc -= rows * unit;
    rows as i64
}

/// The row drawn under screen `y` and the offset of `y` inside it, or `None`
/// for the top slack, the input strip and off-screen rows. Inverse of
/// [`View::row_y`]: row `r` covers `[row_y(r), row_y(r) + line_h)`.
pub fn row_at(rows: usize, view: &View, line_h: u32, y: i64) -> Option<(usize, i32)> {
    if line_h == 0 || y < 0 || y >= view.view_h as i64 {
        return None;
    }
    let bottom = rows.checked_sub(1)? - view.scroll_rows.min(rows - 1);
    let below = ((view.view_h as i64 - 1 - y) / line_h as i64) as usize;
    if below > bottom || below >= view.visible_rows(line_h) {
        return None;
    }
    let r = bottom - below;
    let top = view.row_y(rows, r, line_h)?;
    Some((r, (y - top as i64) as i32))
}

/// Button fragment under the cursor (physical px), as an index into
/// `layout.buttons`: the row from [`row_at`], then Emuera's inclusive hit rect
/// (spec Component 5) — `shift + x0 + x ≤ px ≤ shift + x0 + x + w` and
/// `0 ≤ dy ≤ min(font_px, line_h − 1)` (i.e. the rect
/// `[shift + x0 + x, row_y, w + 1, min(font_px + 1, line_h)]`) — restricted to
/// fragments of the active input generation. Fragments are checked in layout
/// order; the first hit wins.
pub fn hit_button(
    layout: &Layout,
    g: &Geometry,
    view: &View,
    active_gen: Option<u32>,
    cursor: (i64, i64),
) -> Option<usize> {
    let active = active_gen?;
    let (row, dy) = row_at(layout.rows.len(), view, g.m.line_h, cursor.1)?;
    let band = g.m.font_px.min(g.m.line_h.saturating_sub(1)) as i32;
    if dy < 0 || dy > band {
        return None;
    }
    let px = i32::try_from(cursor.0).ok()?;
    let x0 = layout.rows.get(row)?.x0;
    layout.buttons.iter().position(|b| {
        if b.row != row || b.input_gen != active {
            return false;
        }
        let left = g.m.shift as i32 + x0 + b.x;
        px >= left && px <= left + b.w as i32
    })
}

/// The input strip line: `> {input}_` in the console's default colour.
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

/// Append per-page instance buckets (the input strip) onto the frame's buckets.
fn merge_pages(into: &mut Vec<Vec<Instance>>, from: Vec<Vec<Instance>>) {
    for (page, list) in from.into_iter().enumerate() {
        if into.len() <= page {
            into.resize_with(page + 1, Vec::new);
        }
        into[page].extend(list);
    }
}
```

Run:

```bash
cd /home/riey/repos/erars && cargo test -p erars-renderer --lib app:: 2>&1 | grep -E "^test |test result"
```
Expected:
```
test app::tests::scroll_arithmetic ... ok
test app::tests::wheel_pixels_accumulate_into_rows ... ok
test app::tests::row_at_matches_the_drawer_and_rejects_slack_and_strip ... ok
test app::tests::input_line_is_default_coloured_plain_text ... ok
test app::tests::hit_test_uses_emuera_inclusive_rects_and_whole_rows ... ok
test result: ok. 5 passed; 0 failed
```
(an `unused` warning for `merge_pages` and the unused imports is fine until Step 7). If `hit_test_…` fails at `x0 == 706`, Right alignment in T7 is wrong — report it, do not patch it here; if it fails on `(left + 36, 460)` or `(left, 455)`, re-check `hit_button`'s inclusive right edge (`px <= left + w`) and its band (`dy <= min(font_px, line_h − 1)`) against the expectations in the test (button 3: row 29, top 442, left = 3 + 706 + 18 = 727, w 36; with line_h 22 the row top is 436 and dy 19 misses).

- [ ] **Step 7: `app.rs` — the `App` state machine; the binary builds**

Insert after `merge_pages` (before `#[cfg(test)]`):

```rust
// ---------------------------------------------------------------------------
// App
// ---------------------------------------------------------------------------

pub struct App {
    cfg: AppConfig,
    shaper: Shaper,
    receiver: ProxyReceiver,
    window: Option<Arc<Window>>,
    gpu: Option<GpuContext>,
    raster: Option<GlyphRaster>,

    frame: ConsoleFrame,
    /// Layout of `frame.lines` at `layout_w` / the current metrics.
    layout: Layout,
    /// Content width (physical px) `layout` was computed for.
    layout_w: u32,
    /// Cached one-line layout of the input strip; rebuilt when `strip_dirty`.
    strip: Option<Layout>,
    strip_dirty: bool,

    current_req: Option<InputRequest>,
    input: String,
    /// Whole rows hidden below the bottom of the row area (0 = stuck to the bottom).
    scroll_rows: usize,
    /// Accumulated `PixelDelta` wheel travel not yet converted to rows.
    wheel_px: f64,
    /// Accumulated fractional `LineDelta` notches.
    wheel_lines: f32,
    /// Index into `layout.buttons` of the fragment under the cursor.
    hovered: Option<usize>,
    /// Cursor in physical px; `(-1, -1)` when outside the window.
    cursor: (i64, i64),

    /// When the current input request times out (TINPUT), and the value to
    /// send on expiry.
    timeout_deadline: Option<Instant>,
    timeout_value: Value,
}

/// Current wall-clock time as Unix nanoseconds, matching `Timeout::timeout`.
fn current_unix_nanos() -> i128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as i128)
        .unwrap_or(0)
}

impl App {
    pub fn new(shaper: Shaper, receiver: ProxyReceiver, cfg: AppConfig) -> Self {
        Self {
            cfg,
            shaper,
            receiver,
            window: None,
            gpu: None,
            raster: None,
            frame: ConsoleFrame {
                fore_color: Color(cfg.default_fg),
                ..ConsoleFrame::default()
            },
            layout: Layout::default(),
            layout_w: 0,
            strip: None,
            strip_dirty: true,
            current_req: None,
            input: String::new(),
            scroll_rows: 0,
            wheel_px: 0.0,
            wheel_lines: 0.0,
            hovered: None,
            cursor: (-1, -1),
            timeout_deadline: None,
            timeout_value: Value::Int(0),
        }
    }

    fn metrics(&self) -> CellMetrics {
        *self.shaper.metrics()
    }

    /// Surface size in physical px (the logical config size before the window exists).
    fn surface_size(&self) -> (u32, u32) {
        self.gpu.as_ref().map_or(self.cfg.init_size, |g| g.size())
    }

    fn geometry(&self) -> Geometry {
        Geometry::new(self.surface_size().0.max(1), self.metrics())
    }

    fn view(&self) -> View {
        let line_h = self.metrics().line_h;
        let (_, win_h) = self.surface_size();
        View {
            scroll_rows: self.scroll_rows,
            view_h: win_h.saturating_sub(line_h),
            strip_h: line_h,
        }
    }

    fn active_gen(&self) -> Option<u32> {
        self.current_req.as_ref().map(|r| r.generation)
    }

    fn request_redraw(&self) {
        if let Some(w) = &self.window {
            w.request_redraw();
        }
    }

    /// Recompute the layout for the current frame and surface width, then
    /// clamp the scroll position and re-derive the hovered fragment.
    fn relayout(&mut self) {
        let g = self.geometry();
        self.layout = layout(&self.frame.lines, &g, &mut self.shaper);
        self.layout_w = g.content_w;
        self.strip_dirty = true;
        self.clamp_scroll_state();
        self.update_hover();
    }

    /// Keep `scroll_rows` within `[0, rows − visible]` (never forces the bottom).
    fn clamp_scroll_state(&mut self) {
        let visible = self.view().visible_rows(self.metrics().line_h);
        let max = max_scroll(self.layout.rows.len(), visible);
        self.scroll_rows = clamp_scroll(self.scroll_rows as i64, max);
    }

    /// Scroll to `requested` rows (clamped). Returns whether the position changed.
    fn scroll_to(&mut self, requested: i64) -> bool {
        let visible = self.view().visible_rows(self.metrics().line_h);
        let max = max_scroll(self.layout.rows.len(), visible);
        let next = clamp_scroll(requested, max);
        let changed = next != self.scroll_rows;
        self.scroll_rows = next;
        changed
    }

    /// Re-derive `hovered` from the stored cursor. Returns whether it changed.
    fn update_hover(&mut self) -> bool {
        let next = hit_button(
            &self.layout,
            &self.geometry(),
            &self.view(),
            self.active_gen(),
            self.cursor,
        );
        let changed = next != self.hovered;
        self.hovered = next;
        changed
    }

    /// Apply a winit scale factor: new integer cell metrics from the primary
    /// font (clears the shape cache). The caller relayouts.
    fn apply_scale(&mut self, scale: f32) {
        let scale = if scale.is_finite() && scale > 0.0 { scale } else { 1.0 };
        if (scale - self.metrics().scale).abs() < f32::EPSILON {
            return;
        }
        let primary_id = self.shaper.chain().primary();
        let primary = self.shaper.chain().font(primary_id);
        let m = CellMetrics::from_primary(&primary, self.cfg.font_size, self.cfg.line_height, scale);
        log::info!(
            "scale {scale}: font_px {} half_w {} line_h {} baseline {} shift {}",
            m.font_px,
            m.half_w,
            m.line_h,
            m.baseline,
            m.shift
        );
        self.shaper.set_metrics(m);
    }

    fn send(&mut self, resp: SystemResponse) {
        let _ = self.receiver.res_tx.send(resp);
        self.current_req = None;
        self.input.clear();
        self.strip_dirty = true;
        self.timeout_deadline = None;
        self.update_hover();
    }

    /// Drain all pending VM requests, relayout if a frame arrived, then request a redraw.
    fn drain_requests(&mut self, event_loop: &ActiveEventLoop) {
        let mut new_frame = false;
        while let Ok(req) = self.receiver.req_rx.try_recv() {
            match req {
                SystemRequest::Quit => event_loop.exit(),
                SystemRequest::Redraw(frame) => {
                    self.frame = frame;
                    new_frame = true;
                }
                SystemRequest::Input(req) => {
                    if let Some(t) = req.timeout.as_ref() {
                        let remaining_ns = (t.timeout - current_unix_nanos()).max(0) as u128;
                        self.timeout_deadline =
                            Some(Instant::now() + Duration::from_nanos(remaining_ns as u64));
                        self.timeout_value = t.default_value.clone();
                    } else {
                        self.timeout_deadline = None;
                    }
                    self.current_req = Some(req);
                    self.strip_dirty = true;
                }
            }
        }
        if new_frame {
            // A new frame always sticks to the bottom (Emuera).
            self.scroll_rows = 0;
            self.relayout();
        } else {
            // Only the active generation may have changed.
            self.update_hover();
        }
        self.request_redraw();
    }

    fn render(&mut self) {
        let (Some(gpu), Some(raster)) = (self.gpu.as_mut(), self.raster.as_mut()) else {
            return;
        };
        let (win_w, win_h) = gpu.size();
        let m = *self.shaper.metrics();
        let view = View {
            scroll_rows: self.scroll_rows,
            view_h: win_h.saturating_sub(m.line_h),
            strip_h: m.line_h,
        };
        let hl = self.frame.hl_color.0;
        let mut pages = build_instances(
            &self.layout,
            &view,
            self.hovered,
            hl,
            raster,
            &gpu.device,
            &gpu.queue,
            &mut self.shaper,
        );
        if self.current_req.is_some() {
            if self.strip_dirty || self.strip.is_none() {
                let line = input_line(&self.input, self.frame.fore_color.0);
                let g = Geometry::new(win_w.max(1), m);
                self.strip = Some(layout(&[line], &g, &mut self.shaper));
                self.strip_dirty = false;
            }
            // `View::strip()` lands the one-row layout on the bottom line_h px.
            let strip = self.strip.as_ref().expect("strip laid out above");
            let strip_pages = build_instances(
                strip,
                &view.strip(),
                None,
                hl,
                raster,
                &gpu.device,
                &gpu.queue,
                &mut self.shaper,
            );
            merge_pages(&mut pages, strip_pages);
        }
        let pairs = raster.pages_with(&pages);
        gpu.render(&pairs, self.frame.bg_color.0);
    }

    fn on_click(&mut self) {
        let hit = hit_button(
            &self.layout,
            &self.geometry(),
            &self.view(),
            self.active_gen(),
            self.cursor,
        );
        if let Some(i) = hit {
            let value = self.layout.buttons[i].value.clone();
            self.send(SystemResponse::Input(value));
        }
    }

    fn submit(&mut self) {
        let Some(req) = self.current_req.clone() else {
            return;
        };
        match req.ty {
            InputRequestType::Int => {
                if let Ok(i) = self.input.trim().parse::<i64>() {
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
                self.send(SystemResponse::Empty);
            }
        }
    }
}

impl ApplicationHandler<Wake> for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }
        let attrs = Window::default_attributes()
            .with_title("erars")
            .with_inner_size(winit::dpi::LogicalSize::new(
                self.cfg.init_size.0,
                self.cfg.init_size.1,
            ));
        let window = Arc::new(event_loop.create_window(attrs).unwrap());
        let size = window.inner_size();
        let instance = wgpu::Instance::default();
        let surface = instance.create_surface(window.clone()).unwrap();
        let gpu = GpuContext::new(&instance, surface, size.width.max(1), size.height.max(1));
        self.raster = Some(GlyphRaster::new(&gpu.device, self.cfg.use_bitmap_strikes));
        self.gpu = Some(gpu);
        self.apply_scale(window.scale_factor() as f32);
        self.window = Some(window);
        self.relayout();
        self.drain_requests(event_loop);
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, _: Wake) {
        self.drain_requests(event_loop);
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        let Some(deadline) = self.timeout_deadline else {
            return;
        };
        if Instant::now() >= deadline {
            let v = self.timeout_value.clone();
            self.send(SystemResponse::Input(v));
            self.request_redraw();
        } else {
            // Wake again at the deadline so the timeout can fire on time.
            event_loop.set_control_flow(winit::event_loop::ControlFlow::WaitUntil(deadline));
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                if let Some(gpu) = self.gpu.as_mut() {
                    gpu.resize(size.width, size.height);
                }
                if size.width.max(1) != self.layout_w {
                    self.relayout();
                } else {
                    // Height only: rows move, the layout does not.
                    self.clamp_scroll_state();
                    self.update_hover();
                }
                self.request_redraw();
            }
            WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
                self.apply_scale(scale_factor as f32);
                self.relayout();
                self.request_redraw();
            }
            WindowEvent::RedrawRequested => self.render(),
            WindowEvent::CursorMoved { position, .. } => {
                self.cursor = (position.x.floor() as i64, position.y.floor() as i64);
                if self.update_hover() {
                    self.request_redraw();
                }
            }
            WindowEvent::CursorLeft { .. } => {
                self.cursor = (-1, -1);
                if self.update_hover() {
                    self.request_redraw();
                }
            }
            WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: winit::event::MouseButton::Left,
                ..
            } => {
                if let Some(req) = self.current_req.clone() {
                    match req.ty {
                        InputRequestType::AnyKey
                        | InputRequestType::EnterKey
                        | InputRequestType::ForceEnterKey => self.submit(),
                        _ => self.on_click(),
                    }
                    self.request_redraw();
                }
            }
            WindowEvent::MouseWheel { delta, .. } => {
                // Wheel up (positive y) reveals older rows: scroll_rows grows.
                let rows = match delta {
                    MouseScrollDelta::LineDelta(_, y) => {
                        self.wheel_lines += y;
                        let whole = self.wheel_lines.trunc();
                        self.wheel_lines -= whole;
                        whole as i64
                    }
                    MouseScrollDelta::PixelDelta(p) => {
                        let line_h = self.metrics().line_h;
                        wheel_rows(&mut self.wheel_px, p.y, line_h)
                    }
                };
                if rows != 0 && self.scroll_to(self.scroll_rows as i64 + rows) {
                    self.update_hover();
                    self.request_redraw();
                }
            }
            WindowEvent::KeyboardInput { event, .. } if event.state == ElementState::Pressed => {
                let Some(req) = self.current_req.clone() else {
                    return;
                };
                match &event.logical_key {
                    Key::Named(NamedKey::Enter) => self.submit(),
                    Key::Named(NamedKey::Backspace) => {
                        self.input.pop();
                    }
                    Key::Named(NamedKey::Space) if matches!(req.ty, InputRequestType::AnyKey) => {
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
                self.strip_dirty = true;
                self.request_redraw();
            }
            _ => {}
        }
    }
}
```

Run:

```bash
cd /home/riey/repos/erars && cargo build -p erars-renderer 2>&1 | grep -E "^(warning|error)" | sort | uniq -c; echo "exit=${PIPESTATUS[0]}"
```
Expected: `exit=0`, no `error` lines, and no warning naming `app.rs`, `main.rs` or `lib.rs` (fix any that does). Warnings in other modules belong to their tasks — leave them and list them in the final report. If the compiler reports `no method named `strip`` on `View` or `no method named `pages_with`` on `GlyphRaster`, the merged T8 spells them differently — use T8's names; `App::render` is the only caller.

- [ ] **Step 8: Cargo cleanup — drop `unicode-width`/`sys-locale`, add `[lib] doctest = false`**

```bash
cd /home/riey/repos/erars && grep -rn "unicode_width\|sys_locale" crates/erars-renderer/src; echo "exit=$? (1 = no users)"
```
Expected: no output, `exit=1` (the only users were the legacy blocks deleted in Steps 3–4). Then edit `crates/erars-renderer/Cargo.toml`:

```diff
 [package]
 name = "erars-renderer"
 version = "0.5.0"
 edition = "2021"
 license = "GPL-3.0-or-later"
 
+# No runnable doc examples; the layout docs quote snapshot text in ``` blocks
+# that rustdoc would otherwise try to compile as Rust.
+[lib]
+doctest = false
+
 [dependencies]
```
and, in `[dependencies]`, delete the two lines `unicode-width = "0.1"` and `sys-locale = "0.3"`. Leave every other line where T5–T9 put it. The block must now contain exactly these non-path entries (order as found): `flume.workspace`, `clap`, `flexi_logger`, `log-panics`, `log`, `mimalloc`, `winit`, `wgpu`, `pollster`, `bytemuck`, `etagere`, `swash`, `rustybuzz`, `flate2`, `crc32fast`, `smol_str`, `bitflags`, `cosmic-text = "0.12.1"` (no `features` line), plus `[dev-dependencies] k9 = "0.11.5"` from T7.

```bash
cd /home/riey/repos/erars && grep -n "unicode-width\|sys-locale\|monospace_fallback\|shape-run-cache" crates/erars-renderer/Cargo.toml; echo "exit=$? (1 = clean)"; grep -c '^doctest = false' crates/erars-renderer/Cargo.toml; cargo build -p erars-renderer 2>&1 | tail -n 1
```
Expected: no matches, `exit=1`, `1`, `Finished …`.

- [ ] **Step 9: Nothing of the old text path survives**

```bash
cd /home/riey/repos/erars && grep -rn "FontCtx\|CellShaper\|PlacedGlyph\|ShapedRun\|GlyphAtlas\|grid::\|atlas::\|render_lines\|write_ppm\|font_candidates\|with_candidates\|build_instances_legacy\|unicode_width\|sys_locale\|cosmic_text::SwashCache\|cosmic_text::{Attrs\|Buffer::new" crates/erars-renderer/src crates/erars-renderer/Cargo.toml; echo "grep exit=$? (1 = clean)"; grep -rn "allow(dead_code)" crates/erars-renderer/src || echo "no dead_code allowances"
```
Expected: no matches for the first grep, `grep exit=1`, then `no dead_code allowances` (the T5/T7/T8 `#[allow(dead_code)]` placeholders sat on `mod` lines of the old `main.rs`, which is gone). Any hit in the first grep is a remnant of the old code path: delete the referencing code (its modules are gone). A surviving `#[allow(dead_code)]` inside another task's file is only reported in the final summary, not edited here.

- [ ] **Step 10: Headless smoke run on the sample game and the CLI flags**

```bash
cd /home/riey/repos/erars && cargo run -q -p erars-renderer -- --quite --headless-shot /tmp/claude-1000/-home-riey-repos-erars/50a48b53-7d56-447e-a93a-55727276ea60/scratchpad/sample-shot.png . && python3 -c "import struct;d=open('/tmp/claude-1000/-home-riey-repos-erars/50a48b53-7d56-447e-a93a-55727276ea60/scratchpad/sample-shot.png','rb').read(24);print(d[:8]==b'\x89PNG\r\n\x1a\n', struct.unpack('>II', d[16:24]))" && cargo run -q -p erars-renderer -- --help | grep -c "no-bitmap-strikes\|PATH.png"
```
Expected: `Wrote /tmp/…/sample-shot.png (760x480)`, then `True (760, 480)`, then `2`. (Without any wgpu adapter the first command prints `No GPU adapter available for headless rendering` — the dev box has NVIDIA/Vulkan, so it must succeed there.) Then the same shot with `--no-bitmap-strikes` must also succeed (no MS Gothic is involved, so the image may be byte-identical):

```bash
cd /home/riey/repos/erars && cargo run -q -p erars-renderer -- --quite --no-bitmap-strikes --headless-shot /tmp/claude-1000/-home-riey-repos-erars/50a48b53-7d56-447e-a93a-55727276ea60/scratchpad/sample-shot-outline.png . | grep -c "^Wrote"
```
Expected: `1`.

- [ ] **Step 11: Whole-crate tests and commit**

```bash
cd /home/riey/repos/erars && cargo test -p erars-renderer 2>&1 | grep -E "test result|FAILED|panicked"
```
Expected: one `test result: ok. … 0 failed` line per target (lib unit tests; no doctest line because `doctest = false`), no `FAILED`/`panicked` (GPU tests may print `SKIP …` on a box without an adapter; not here).

```bash
cd /home/riey/repos/erars && git add crates/erars-renderer/Cargo.toml Cargo.lock crates/erars-renderer/src/lib.rs crates/erars-renderer/src/app.rs crates/erars-renderer/src/main.rs crates/erars-renderer/src/font.rs crates/erars-renderer/src/text.rs crates/erars-renderer/src/draw.rs && git commit -q -m "feat(renderer): row-anchored app on the cell layout, PNG headless shot, --no-bitmap-strikes

Split erars-renderer into a library + CLI, replace the Grid/atlas plumbing
in app.rs with Shaper/Layout/GlyphRaster, scroll by whole rows, recolour
hover at draw time, hit-test Emuera's inclusive button rects, and delete
grid.rs, atlas.rs and the legacy FontCtx/CellShaper/build_instances_legacy
code paths.

Claude-Session: https://claude.ai/code/session_01XEtVTsN59k1K3cegBL8mfx" && git log --oneline -1
```
Expected: one new commit line starting with `feat(renderer): row-anchored app …` (`git rm` in Step 1 already staged the two deletions).

### Task 11: Integration + CI + housekeeping

**Files:**
- Create: `crates/erars-renderer/tests/games/tui/ERB/TUI.ERB`, `crates/erars-renderer/tests/games/tui/CSV/GAMEBASE.CSV`, `crates/erars-renderer/tests/games/tui/emuera.config` (KOREAN), `crates/erars-renderer/tests/games/tui/emuera.jp.config` (JAPANESE) — all UTF-8 **with BOM** (`erars_reader::read_file` decodes BOM-less files as Shift_JIS, `crates/erars-reader/src/lib.rs:15`)
- Create: `crates/erars-renderer/tests/tui.rs`
- Modify: `crates/erars-renderer/Cargo.toml` `[dev-dependencies]` (add `anyhow`; `k9` is there from T7)
- Modify: `.github/workflows/check.yml` lines 16–32 (the `steps:` list)
- Modify: `justfile` lines 27–34 (`test-align`, `headless-shot`)
- Modify: `.gitignore` (append after line 15 `game.era`)
- Modify: `README.md` (append after line 21)
- Test: `crates/erars-renderer/tests/tui.rs` (two k9 goldens, one GPU PNG dump); final `cargo test --all` + `cargo clippy -p erars-renderer`

**Interfaces:**
- Consumes (T2): `EraConfig::from_text(&str) -> ParserResult<EraConfig>` (existing), `Language::encoding(&self) -> &'static encoding_rs::Encoding`, `EraConfig` defaults 25/3/760×480, colours (192,192,192)/(0,0,0)/(255,255,0)
- Consumes (T1): `erars_ui::width::WidthTable::new(&'static Encoding)`
- Consumes (T3): `VirtualConsole::default_color() -> Color` (through `ConsoleFrame::from_vconsole`), PRINTC 25 / PRINTLC 26 padding, `print_button` strips `\n`, `print_plain` keeps `\n`
- Consumes (T5): `FontChain::from_files(&[PathBuf], Language)`, `FontChain::new(&FontConfig)`, `FontConfig<'a> { family: &'a str, game_dir: &'a Path, extra_dir: Option<PathBuf>, lang: Language }`, `FontChain::primary`, `FontChain::font`
- Consumes (T6): `CellMetrics { scale, font_px, half_w, line_h, baseline, shift }`, `CellMetrics::from_primary(&Font, u32, u32, f32)`, `Shaper::new(FontChain, WidthTable, CellMetrics)`
- Consumes (T7): `Geometry::new(content_w: u32, m: CellMetrics)`, `layout(&[ConsoleLine], &Geometry, &mut Shaper) -> Layout`, `layout_snapshot(&Layout, default_fg: [u8; 3]) -> String` (in `erars_renderer::layout`)
- Consumes (T9): `headless::render_frame_on(device: &wgpu::Device, queue: &wgpu::Queue, shaper: &mut Shaper, frame: &ConsoleFrame, content_w: u32, height: u32, input: Option<&str>, hover: Option<usize>, use_bitmap_strikes: bool) -> Rendered`, `headless::write_png(&str, &Rendered) -> io::Result<()>`, `test_support::gpu_lock() -> MutexGuard<'static, ()>`, `test_support::gpu_device() -> Option<(wgpu::Device, wgpu::Queue)>` (prints `SKIP <test>: no wgpu adapter`; panics under `ERARS_REQUIRE_GPU=1`), `ConsoleFrame.fore_color`
- Consumes (T10): `erars_renderer` library crate, `pub mod test_support`, CLI `--headless-shot PATH.png`
- Consumes (existing): `erars_loader::run_script(&str, Box<dyn SystemFunctions>, EraConfig, error_to_stderr: bool, lint: bool) -> anyhow::Result<(TerminalVm, VmContext, VirtualConsole)>`, `erars_vm::SystemFunctions { fn input(&mut self, InputRequest) -> anyhow::Result<Option<Value>>; fn redraw(&mut self, &mut VirtualConsole) -> anyhow::Result<()> }`, `ConsoleFrame::from_vconsole(&VirtualConsole)`, `TerminalVm::start(&self, &mut VirtualConsole, &mut VmContext) -> bool` (true = normal exit), `impl Display for ConsoleLine` (concatenated part texts), `VirtualConsole::lines_from(usize)` + `last_line`
- Produces: fixture game `crates/erars-renderer/tests/games/tui/`; tests `tui_layout_korean`, `tui_layout_japanese`, `tui_png_korean`; `just test-align`, `just headless-shot <game> <out.png>`; CI with lavapipe + `ERARS_REQUIRE_GPU=1`.

- [ ] **Step 1: Fixture game — ERB**

Create `crates/erars-renderer/tests/games/tui/ERB/TUI.ERB` with exactly this content (the last PRINTBUTTON is `[2] ` followed by exactly 90 `x`; no trailing whitespace on any line — `CUSTOMDRAWLINE` keeps everything after the first space, trailing blanks included):

```
@SYSTEM_TITLE
;marker: the test drops everything the loader printed before this line
PRINTL ==TUI==
ALIGNMENT CENTER
PRINTL ★ 텍스트 UI 데모 ★
ALIGNMENT LEFT
DRAWLINE
PRINTL ┏━━━━┓
PRINTL ┃가A     ┃
PRINTL ┗━━━━┛
CUSTOMDRAWLINE ━
PRINTC 한글
PRINTC abc
PRINTC ▒▒
PRINTL
PRINTLC 항목
PRINTLC 12345
PRINTL
ALIGNMENT RIGHT
PRINTL ▒░█═║
ALIGNMENT LEFT
FONTBOLD
SETCOLOR 255, 128, 0
PRINT 굵은 주황
RESETCOLOR
FONTREGULAR
PRINTL 보통
PRINTPLAINFORM ab%UNICODE(10)%cd
PRINTL
PRINTBUTTON "[0] 시작", 0
PRINTBUTTON "[1] 이어\n하기", 1
PRINTL
PRINTBUTTON "[2] xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", 2
INPUT
QUIT
```

Then prepend the BOM and verify:

```bash
cd /home/riey/repos/erars/crates/erars-renderer/tests/games/tui && sed -i '1s/^/\xEF\xBB\xBF/' ERB/TUI.ERB && head -c 3 ERB/TUI.ERB | xxd -p && grep -o 'x' ERB/TUI.ERB | wc -l && grep -c '┃가A     ┃' ERB/TUI.ERB && grep -c ' $' ERB/TUI.ERB
```
Expected: `efbbbf`, `90`, `1` (five spaces between `A` and `┃`), `0` (no trailing spaces; note the last `grep -c` exits 1 when it prints 0 — that is the wanted outcome).

What the fixture exercises (Emuera defaults 760 px, 18/19 px, PRINTC 25, `shift` 3 → drawable 757 = 84 half cells): a centred title (20 cells → x0 290), DRAWLINE (84 `-`), a box map whose three rows are 12 cells each, CUSTOMDRAWLINE (42 `━`), a PRINTC row (3 × 25 cells; `▒` is 2 cells in EUC-KR, 1 in Shift_JIS), a PRINTLC row (2 × 26), a Right row whose width differs by language (`▒░█═║`: KR 6 cells → x0 706, JP 5 → 715), bold orange text followed by plain text on one row, a residual `\n` from PRINTPLAINFORM (continuation row), two buttons on one row (`\n` stripped from the second), and a 94-cell button wrapped into 84 + 10 cell fragments.

- [ ] **Step 2: Fixture game — CSV and configs**

```bash
cd /home/riey/repos/erars/crates/erars-renderer/tests/games/tui && mkdir -p CSV && printf '\xEF\xBB\xBF%s\n%s\n%s\n%s\n' 'コード,1' 'バージョン,1' 'タイトル,tui' '作者,erars' > CSV/GAMEBASE.CSV && printf '\xEF\xBB\xBF%s\n' '内部で使用する東アジア言語:KOREAN' > emuera.config && printf '\xEF\xBB\xBF%s\n' '内部で使用する東アジア言語:JAPANESE' > emuera.jp.config && for f in CSV/GAMEBASE.CSV emuera.config emuera.jp.config; do printf '%s ' "$f"; head -c 3 "$f" | xxd -p; done && cat emuera.config emuera.jp.config
```
Expected: three `efbbbf` lines, then the two config lines (`KOREAN`, `JAPANESE`). Every other config key keeps the T2 defaults: font 18 / line 19 / 760×480 / PRINTC 25 / colours (192,192,192)/(0,0,0)/(255,255,0).

- [ ] **Step 3: dev-dependencies (`anyhow` for the scripted `SystemFunctions`; `k9` check)**

```bash
cd /home/riey/repos/erars && grep -n "k9\|anyhow\|dev-dependencies" crates/erars-renderer/Cargo.toml
```
Expected: `[dev-dependencies]` and `k9 = "0.11.5"` present (T7), `anyhow` absent. Append to the `[dev-dependencies]` table:

```toml
anyhow.workspace = true
```
(If `[dev-dependencies]`/`k9` are missing, add `[dev-dependencies]` with both `k9 = "0.11.5"` and `anyhow.workspace = true`. Both resolve to locked versions.) Check: `cargo metadata --offline -q >/dev/null && echo resolved` → `resolved`.

- [ ] **Step 4: Integration test — write it with empty snapshots**

Create `crates/erars-renderer/tests/tui.rs`:

```rust
//! Integration: compile and run the synthetic `tests/games/tui` game headlessly
//! with a scripted `SystemFunctions`, capture the console frame at the first
//! INPUT and snapshot its layout for the KOREAN and JAPANESE configs. With a
//! GPU adapter the KOREAN frame is also rendered to a PNG under `target/tmp`.
//!
//! Expected geometry (Emuera defaults 760 px, 18/19 px, PRINTC 25, bundled
//! Noto Sans Mono through `FontChain::from_files`, pinned CellMetrics):
//! - title row Center: 20 cells = 180 px → x0 = 380 − 90 = 290
//! - DRAWLINE: 84 `-` = 756 px; CUSTOMDRAWLINE ━: 42 × 18 = 756 px
//! - PRINTC row: 3 × 25 cells; `한` at 189, `a` at 423, `▒` at 639 (KR, 2 cells)
//!   or 657 (JP, 1 cell)
//! - PRINTLC row: 2 × 26 cells; `1` at 234
//! - Right row `▒░█═║`: KR 6 cells → x0 706, JP 5 cells → x0 715
//! - bold orange `굵은 주황` (c=FF8000 s=B) then `보통` at x 81
//! - PRINTPLAINFORM `ab\ncd`: row `ab` then a continuation row `cd`
//! - buttons: `[0] 시작` (w 72) + `[1] 이어하기` (w 108, `\n` stripped) on one
//!   row; the 94-cell button wraps into fragments of 756 px and 90 px

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use erars_ast::Value;
use erars_compiler::{EraConfig, Language};
use erars_proxy_system::ConsoleFrame;
use erars_renderer::font::{FontChain, FontConfig};
use erars_renderer::headless::{render_frame_on, write_png};
use erars_renderer::layout::{layout, layout_snapshot, Geometry};
use erars_renderer::test_support::{gpu_device, gpu_lock};
use erars_renderer::text::{CellMetrics, Shaper};
use erars_ui::width::WidthTable;
use erars_ui::{InputRequest, VirtualConsole};
use erars_vm::SystemFunctions;

const GAME: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/games/tui");
const BUNDLED: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf");
/// Pinned metrics (spec Testing §4): row geometry is font-independent.
const M: CellMetrics = CellMetrics {
    scale: 1.0,
    font_px: 18,
    half_w: 9,
    line_h: 19,
    baseline: 15,
    shift: 3,
};
/// `emuera.config` 文字色 default.
const DEFAULT_FG: [u8; 3] = [192, 192, 192];

/// Records the latest redraw; freezes it when the game first asks for input;
/// answers that INPUT with 0 and refuses any further one.
struct Scripted {
    latest: ConsoleFrame,
    at_input: Arc<Mutex<Option<ConsoleFrame>>>,
    answers: Vec<Value>,
}

impl SystemFunctions for Scripted {
    fn input(&mut self, _req: InputRequest) -> anyhow::Result<Option<Value>> {
        let mut slot = self.at_input.lock().unwrap();
        if slot.is_none() {
            *slot = Some(self.latest.clone());
        }
        match self.answers.pop() {
            Some(v) => Ok(Some(v)),
            None => anyhow::bail!("script exhausted"),
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        self.latest = ConsoleFrame::from_vconsole(vconsole);
        Ok(())
    }
}

fn read_config(name: &str) -> EraConfig {
    let text = std::fs::read_to_string(Path::new(GAME).join(name)).unwrap();
    EraConfig::from_text(text.trim_start_matches('\u{feff}')).unwrap()
}

fn console_text(tx: &VirtualConsole) -> String {
    let mut out: Vec<String> = tx.lines_from(0).iter().map(|l| l.to_string()).collect();
    out.push(tx.last_line.to_string());
    out.join("\n")
}

/// Compile + run the fixture with `config`, return the frame at the first
/// INPUT with everything the loader printed before `==TUI==` removed.
/// Runs are serialised: the VM shares one global string interner.
fn run_game(config_name: &str) -> ConsoleFrame {
    static RUN: Mutex<()> = Mutex::new(());
    let _guard = RUN.lock().unwrap_or_else(|e| e.into_inner());
    let at_input = Arc::new(Mutex::new(None));
    let system = Box::new(Scripted {
        latest: ConsoleFrame::default(),
        at_input: at_input.clone(),
        answers: vec![Value::Int(0)],
    });
    let (vm, mut ctx, mut tx) =
        erars_loader::run_script(GAME, system, read_config(config_name), true, false)
            .expect("compile tests/games/tui");
    let ok = vm.start(&mut tx, &mut ctx);
    assert!(ok, "VM error:\n{}", console_text(&tx));
    let frame = at_input
        .lock()
        .unwrap()
        .take()
        .expect("the game never asked for INPUT");
    let marker = frame
        .lines
        .iter()
        .position(|l| l.to_string() == "==TUI==")
        .expect("==TUI== marker line");
    ConsoleFrame {
        lines: frame.lines[marker + 1..].to_vec(),
        ..frame
    }
}

fn snapshot_of(frame: &ConsoleFrame, lang: Language) -> String {
    let mut shaper = Shaper::new(
        FontChain::from_files(&[PathBuf::from(BUNDLED)], lang),
        WidthTable::new(lang.encoding()),
        M,
    );
    let g = Geometry::new(760, M);
    assert_eq!(g.drawable_w, 757);
    let l = layout(&frame.lines, &g, &mut shaper);
    assert_eq!(l.rows.len(), 15, "rows");
    assert_eq!(l.buttons.len(), 4, "button fragments");
    layout_snapshot(&l, DEFAULT_FG)
}

#[test]
fn tui_layout_korean() {
    let frame = run_game("emuera.config");
    assert_eq!(frame.bg_color.0, [0, 0, 0]);
    assert_eq!(frame.hl_color.0, [255, 255, 0]);
    assert_eq!(frame.fore_color.0, DEFAULT_FG);
    k9::snapshot!(snapshot_of(&frame, Language::Korean));
}

#[test]
fn tui_layout_japanese() {
    let frame = run_game("emuera.jp.config");
    k9::snapshot!(snapshot_of(&frame, Language::Japanese));
}

/// GPU: render the KOREAN frame with the real font chain (system fonts, so
/// Hangul is legible where a CJK font is installed) to
/// `target/tmp/tui-korean.png` for eyeballing.
#[test]
fn tui_png_korean() {
    let _lock = gpu_lock();
    let Some((device, queue)) = gpu_device() else {
        return; // gpu_device printed SKIP (or panicked under ERARS_REQUIRE_GPU=1)
    };
    let frame = run_game("emuera.config");
    let cfg = FontConfig {
        family: "",
        game_dir: Path::new(GAME),
        extra_dir: std::env::var_os("ERARS_FONT_DIR").map(PathBuf::from),
        lang: Language::Korean,
    };
    let mut chain = FontChain::new(&cfg);
    let primary_id = chain.primary();
    let primary = chain.font(primary_id);
    let m = CellMetrics::from_primary(&primary, 18, 19, 1.0);
    let mut shaper = Shaper::new(chain, WidthTable::new(Language::Korean.encoding()), m);
    let img = render_frame_on(&device, &queue, &mut shaper, &frame, 760, 480, Some(""), None, true);
    assert_eq!((img.width, img.height), (760, 480));
    assert!(
        img.rgba.chunks_exact(4).any(|p| p[0] > 0 || p[1] > 0 || p[2] > 0),
        "nothing was drawn"
    );
    let out = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("tui-korean.png");
    write_png(out.to_str().unwrap(), &img).unwrap();
    eprintln!("wrote {}", out.display());
}
```

Run:

```bash
cd /home/riey/repos/erars && cargo test -p erars-renderer --test tui tui_layout 2>&1 | grep -E "^test |test result|panicked|K9_UPDATE" | head
```
Expected: both `tui_layout_korean` and `tui_layout_japanese` FAIL — k9 reports a missing inline snapshot and tells you to run with `K9_UPDATE_SNAPSHOTS=1` (`test result: FAILED. 0 passed; 2 failed`). Any other failure means the fixture did not compile/run: read the `compile tests/games/tui` / `VM error:` message (the console text is printed). A `rows` assertion off by the loader's own lines means the `==TUI==` marker was not found — `PRINTL ==TUI==` must be the first output of `@SYSTEM_TITLE`.

- [ ] **Step 5: Generate the goldens and verify them against the arithmetic**

```bash
cd /home/riey/repos/erars && K9_UPDATE_SNAPSHOTS=1 cargo test -p erars-renderer --test tui tui_layout 2>&1 | grep -E "test result" && cargo test -p erars-renderer --test tui tui_layout 2>&1 | grep -E "test result"
```
Expected: first line `test result: ok. 2 passed` (k9 wrote the two snapshots into `tests/tui.rs`), second line `test result: ok. 2 passed` (stable on re-run).

Now check the generated KOREAN snapshot contains every one of these lines (exact text):

```bash
cd /home/riey/repos/erars && f=crates/erars-renderer/tests/tui.rs && for pat in 'row 0 line 0 x0=290 w=180' 'row 1 line 1 x0=0 w=756' 'row 2 line 2 x0=0 w=108' 'row 3 line 3 x0=0 w=108' 'row 4 line 4 x0=0 w=108' 'row 5 line 5 x0=0 w=756' 'row 6 line 6 x0=0 w=675' '189:2 "한"' '423:1 "a"' '639:2 "▒"' 'row 7 line 7 x0=0 w=468' '234:1 "1"' 'row 8 line 8 x0=706 w=54' 'row 9 line 9 x0=0 w=117' '0:2 "굵" c=FF8000 s=B' '81:2 "보"' 'row 10 line 10 x0=0 w=18' 'row 11 line 10+ x0=0 w=18' 'row 12 line 11 x0=0 w=180' 'row 13 line 12 x0=0 w=756' 'row 14 line 12+ x0=0 w=90' 'btn 0 row=12 x=0 w=72 gen=0 value=Int(0)' 'btn 1 row=12 x=72 w=108 gen=0 value=Int(1)' 'btn 2 row=13 x=0 w=756 gen=0 value=Int(2)' 'btn 3 row=14 x=0 w=90 gen=0 value=Int(2)'; do printf '%-45s %s\n' "$pat" "$(grep -cF -- "$pat" $f)"; done; echo "dash clusters: $(grep -c '"-"' $f) (>= 168), rule clusters: $(grep -c '"━"' $f) (>= 84)"; grep -o 'c=[0-9A-F]*\|s=[BIUS]*' $f | sort -u
```
Expected: every count ≥ 1 (patterns shared by both snapshots print `2`), `dash clusters` ≥ 168 (84 per snapshot), `rule clusters` ≥ 84 (42 per snapshot), and the last command prints exactly `c=FF8000` and `s=B`. The `gen=` value is whatever `VirtualConsole::input_gen()` was at PRINTBUTTON time — `0` if T3 starts generations at 0; if it prints `gen=1` in both snapshots, adjust the four `btn` patterns above, nothing else.

For the JAPANESE snapshot these must appear (KR never produces them): `657:1 "▒"` and `666:1 "▒"` on row 6, `row 8 line 8 x0=715 w=45`, and `0:1 "▒"` on row 8:

```bash
cd /home/riey/repos/erars && f=crates/erars-renderer/tests/tui.rs && for pat in '657:1 "▒"' '666:1 "▒"' 'row 8 line 8 x0=715 w=45' '0:1 "▒"'; do printf '%-30s %s\n' "$pat" "$(grep -cF -- "$pat" $f)"; done
```
Expected: `1` for each. If any expectation fails, the rule behind it is wrong (row geometry → T7, PRINTC padding / `\n` handling → T3, `▒` cells → T1): do not hand-edit the snapshot; report the failing pattern.

- [ ] **Step 6: GPU PNG test**

```bash
cd /home/riey/repos/erars && ERARS_REQUIRE_GPU=1 cargo test -p erars-renderer --test tui tui_png_korean -- --nocapture 2>&1 | grep -E "wrote|SKIP|test result" && ls -l target/tmp/tui-korean.png
```
Expected: `wrote /home/riey/repos/erars/target/tmp/tui-korean.png`, `test result: ok. 1 passed`, and the file listed (a few KB). Eyeball it if a viewer is at hand (over SSH: `scp` it, or `python3 -c "from PIL import Image; Image.open('target/tmp/tui-korean.png').show()"` where a display exists): the title is centred, the box frame closes, the PRINTC columns sit at a 225 px pitch, the `> _` strip is on the bottom line.

- [ ] **Step 7: Commit the fixture and the test**

```bash
cd /home/riey/repos/erars && git add crates/erars-renderer/tests crates/erars-renderer/Cargo.toml Cargo.lock && git commit -q -m "test(renderer): tui fixture game with layout goldens (KOREAN/JAPANESE) and a PNG dump

Claude-Session: https://claude.ai/code/session_01XEtVTsN59k1K3cegBL8mfx" && git log --oneline -1
```

- [ ] **Step 8: CI — lavapipe + enforced GPU tests**

Replace `.github/workflows/check.yml` lines 16–32 (from `    steps:` to the end of the file) so the whole file reads:

```yaml
name: Check

on:
  push:
    branches: [ "master" ]
  pull_request:
    branches: [ "master" ]

env:
  CARGO_TERM_COLOR: always

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v4
    - uses: dtolnay/rust-toolchain@stable
    - name: Cargo Target Cache
      uses: actions/cache@v4
      with:
        path: target
        key: ${{ runner.os }}-cargo-target-${{ hashFiles('**/Cargo.toml') }}-${{ hashFiles('**/Cargo.lock') }}
        restore-keys: |
          ${{ runner.os }}-cargo-target-${{ hashFiles('**/Cargo.toml') }}
          ${{ runner.os }}-cargo-target
    # lavapipe (Mesa's software Vulkan driver) gives wgpu an adapter on the
    # headless runner, so the renderer's pixel tests run instead of skipping.
    - name: Install lavapipe
      run: |
        sudo apt-get update
        sudo apt-get install -y --no-install-recommends mesa-vulkan-drivers libvulkan1 vulkan-tools
        vulkaninfo --summary || true
    - name: Build
      run: cargo build --verbose --all
    - name: Run tests
      run: cargo test --verbose --all
      env:
        # test_support::gpu_device panics instead of printing SKIP, so a
        # missing adapter fails the job rather than passing silently.
        ERARS_REQUIRE_GPU: "1"
```

(`actions/checkout@v3`, `actions/cache@v3` and the archived `actions-rs/toolchain@v1` run on Node versions GitHub has retired; `dtolnay/rust-toolchain@stable` is the maintained replacement. The cache step is otherwise unchanged.)

Verify the YAML parses:

```bash
cd /home/riey/repos/erars && python3 -c "import yaml;d=yaml.safe_load(open('.github/workflows/check.yml'));print([s.get('name') for s in d['jobs']['build']['steps']]);print(d['jobs']['build']['steps'][-1]['env'])"
```
Expected: `[None, None, 'Cargo Target Cache', 'Install lavapipe', 'Build', 'Run tests']` and `{'ERARS_REQUIRE_GPU': '1'}`. (If `yaml` is not installed: `python3 -m pip install --user pyyaml`, or skip this check and rely on the GitHub run.)

- [ ] **Step 9: justfile**

Replace `justfile` lines 27–34 (the `test-align` and `headless-shot` recipes with their comments) with:

```just
# Layout goldens + GPU pixel tests + the tui fixture game (no display server needed).
# ERARS_REQUIRE_GPU=1 turns "no adapter" skips into failures; K9_UPDATE_SNAPSHOTS=1 refreshes goldens.
test-align:
    cargo test -p erars-renderer --lib -- layout:: text:: headless:: draw:: raster:: --nocapture
    cargo test -p erars-renderer --test tui -- --nocapture

# Render a game's first screen to a PNG headlessly (no display), e.g. over SSH.
# Usage: just headless-shot /path/to/game /tmp/out.png
headless-shot game="." out="/tmp/erars-shot.png":
    cargo run -p erars-renderer -- --quite --headless-shot {{out}} {{game}}
```

Check:

```bash
cd /home/riey/repos/erars && just --summary && just -n headless-shot crates/erars-renderer/tests/games/tui /tmp/tui.png
```
Expected: the summary lists `test-align` and `headless-shot` (among the others); the dry run prints `cargo run -p erars-renderer -- --quite --headless-shot /tmp/tui.png crates/erars-renderer/tests/games/tui`. Then run it for real once — `just headless-shot crates/erars-renderer/tests/games/tui /tmp/claude-1000/-home-riey-repos-erars/50a48b53-7d56-447e-a93a-55727276ea60/scratchpad/tui-shot.png` → `Wrote … (760x480)`.

- [ ] **Step 10: .gitignore**

Append to `.gitignore` (after `game.era`, line 15):

```
# proprietary font kept locally for the opt-in MS Gothic tests (ERARS_FONT_DIR); never commit
msgothic.ttc
.DS_Store
```

```bash
cd /home/riey/repos/erars && git status --short | grep -E "msgothic|DS_Store"; echo "exit=$? (1 = both ignored)"
```
Expected: no output, `exit=1`.

- [ ] **Step 11: README note**

Append to `README.md` (after line 21):

```markdown

## GUI renderer (`erars-renderer`)

`cargo run -p erars-renderer -- <game dir>` opens the winit/wgpu console. Text
is laid out on a half-width cell grid exactly like Emuera 1.824 (per-language
cell widths, `WindowX − max(2, FontSize/6)` drawable width, integer pixel
metrics); the design is in
`docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md`.

- Fonts, in order: `フォント名` from `emuera.config` → `<game>/font/*.ttf|ttc|otf|otc`
  → `ERARS_FONT_DIR` (scanned recursively) → the language's fixed-pitch CJK
  families (MS Gothic / D2Coding / GulimChe / Sarasa Mono / Noto Sans Mono CJK)
  → bundled Noto Sans Mono. Any installed font may fill a glyph; cell widths
  never depend on the font.
- `--headless-shot out.png` runs the game until its first input prompt and
  writes that screen as a PNG without a display (any wgpu adapter, lavapipe
  included). `just headless-shot <game> <out.png>`.
- `--no-bitmap-strikes` disables fonts' embedded bitmap strikes (MS Gothic at
  10–22 px) and always rasterizes outlines.
- Tests: `just test-align` runs the layout goldens, the GPU pixel tests and the
  `tests/games/tui` fixture game. `ERARS_REQUIRE_GPU=1` makes a missing GPU
  adapter fail instead of skip (CI does this with lavapipe);
  `K9_UPDATE_SNAPSHOTS=1` regenerates goldens.
```

- [ ] **Step 12: Commit housekeeping**

```bash
cd /home/riey/repos/erars && git add .github/workflows/check.yml justfile .gitignore README.md && git commit -q -m "ci: enforce GPU tests with lavapipe; docs: renderer usage, justfile PNG shot, ignore msgothic.ttc

Claude-Session: https://claude.ai/code/session_01XEtVTsN59k1K3cegBL8mfx" && git log --oneline -1
```

- [ ] **Step 13: Final verification — whole workspace**

```bash
cd /home/riey/repos/erars && ERARS_REQUIRE_GPU=1 cargo test --all 2>&1 | grep -E "^test result|Running|panicked|FAILED|SKIP"
```
Expected: one `test result: ok. N passed; 0 failed` line per test binary — every workspace crate's lib/bin/integration/doc tests (erars-ui width + console tests, erars-compiler, erars-vm incl. `console_config` and the printc_count train-menu test, erars-renderer lib incl. `app::`, `tests/tui.rs`) plus the root package's `run_test` over `tests/run_tests/**` (basic KOREAN + jp JAPANESE) — no `FAILED`, no `panicked`, no `SKIP` line (the dev box has an adapter). Then:

```bash
cd /home/riey/repos/erars && cargo clippy -p erars-renderer --all-targets 2>&1 | grep -E "^(warning|error)" | sort | uniq -c; echo "exit=${PIPESTATUS[0]}"
```
Expected: `exit=0` and no `error` lines. Fix every warning that points into `src/app.rs`, `src/main.rs`, `src/lib.rs` or `tests/tui.rs`; warnings in other files belong to their tasks — list them in the final report instead of touching them. Finally:

```bash
cd /home/riey/repos/erars && git status --short && git log --oneline -6
```
Expected: a clean tree (`msgothic.ttc`/`.DS_Store` are ignored now, `target/tmp/tui-korean.png` is under `/target`) and the T10/T11 commits on top of the T1–T9 ones. If a fix was needed in this step, commit it as `fix(renderer): clippy cleanups after the parity rewrite` with the same `Claude-Session:` trailer.

---
