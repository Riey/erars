# Emuera-parity text renderer — design

**Date:** 2026-09-02
**Status:** Approved in chat (2026-09-02), pending implementation plan
**Supersedes:** the width/fallback/layout sections of
`2026-06-15-erars-renderer-design.md` (module list, GPU pipeline and event flow
there still apply unless changed below).

## Goal

Make `erars-renderer` lay out and draw the ERA console **exactly like Emuera
(1.824 / Emuera.EM, TEXTRENDERER mode, MS Gothic)** and uEmuera, so that
ASCII-art maps, box-drawing frames and PRINTC tables never mis-align, no matter
which installed font ends up supplying a glyph. Concretely:

1. Every grapheme occupies a box of **1 or 2 half-width cells** whose width is
   decided by the code point and the game language alone, never by the font
   that draws it ("perfect fallback": the layout is font-independent).
2. The VM (STRLEN, PRINTC/PRINTLC padding) and the renderer share **one width
   function**, so what the script measures is what the screen shows.
3. Line pitch is always `LineHeight`; wrapping, alignment and DRAWLINE length
   follow Emuera's pixel rules (character-granular wrapping at the drawable
   width, pixel-centred alignment, rule repeated to the drawable width).
4. Glyph pixels match GDI where possible: MS Gothic's embedded bitmap strikes
   are used at matching pixel sizes, hinted outlines otherwise.

Scope is **text only** (text, buttons, rules, bars, colours, bold / italic /
underline / strike). Images and shapes (`PRINT_IMG`, HTML `<img>` / `<shape>`)
stay out of scope; the VM keeps emitting them as text.

## Findings this design rests on

All of these were verified against sources during the investigation
(2026-09-02); file references are to this repo unless noted.

- **Emuera's grid is the font's advance.** MS Gothic (`msgothic.ttc` face 0,
  upem 256) has only two advances: 128 (half) and 256 (full) — at 18 px exactly
  9 / 18 px; ascent 220 + descent 36 = one em, so the GDI cell is 18 px tall
  inside a 19 px line. Emuera measures per styled part with
  `TextRenderer.MeasureText(NoPadding|NoPrefix)` (integer px), wraps at
  `DrawableWidth = WindowX − max(2, FontSize/6)`, centres with
  `WindowX/2 − width/2`, right-aligns with `WindowX − width`, and repeats the
  DRAWLINE string until the measured width reaches `DrawableWidth`, then trims.
  Defaults: `ＭＳ ゴシック` 18 px, LineHeight 19, WindowX 760, PrintCLength 25
  (PRINTLC uses 26), PrintCPerLine 3, ForeColor (192,192,192), FocusColor
  (255,255,0).
- **Half/full truth = encoding byte count.** Over MS Gothic's whole cmap, cells
  equal Shift-JIS byte count for all cp932-encodable code points except
  `¢ £ ¬ −` (font half, cp932 double). Unicode East-Asian-Width does *not*
  reproduce it: `width_cjk` (Ambiguous = 2, what the renderer uses today)
  disagrees on 551 code points (`═ ║ ╔` double lines, dashed/rounded box
  drawing, block elements `▀ █ ░`, `▶ ◀`, `♥ ♡`, `↔`, Latin-1 `¡ ¤ ª ®`…), the
  narrow table (Ambiguous = 1, what the VM's `pad` crate uses today) on 746
  (`★ ● ※ → ① Ⅰ ° ± × ÷`, JIS Greek/Cyrillic, the 32 JIS box-drawing
  characters). Korean Emuera setups (GulimChe, KS X 1001) make the double-line
  box characters full-width — i.e. the rule is per language encoding.
- **erars has three inconsistent width notions today:** renderer `width_cjk`
  (`crates/erars-renderer/src/text.rs:94`), PRINTC padding via `pad` →
  `UnicodeWidthStr::width` (`crates/erars-ui/src/lib.rs:363-389`), STRLEN via
  `encoding_rs` which turns unencodable characters into `&#NNNN;` (8 bytes for
  Hangul in a Japanese game, `crates/erars-vm/src/terminal_vm/executor.rs:1073`).
- **cosmic-text 0.12.1 cannot express an ordered fallback list** for a named
  family: after the exact family it goes to a hard-coded per-script Noto list
  (on Linux it looks for the non-existent family "Noto Sans CJK JA" and
  matches locales only as bare `ja`/`ko`), then a common list, then load order;
  Common-script characters (box drawing, symbols) get no script fallback at all
  and land in proportional fonts. Its `Buffer` also imposes its own line
  metrics (`line_y` depends on the fonts in the run), splits on `\n` with
  per-line byte offsets that `text.rs` mis-slices, and bins sub-pixel offsets
  from a layout x the renderer then discards. The `monospace_fallback`
  feature only affects `Family::Monospace`.
- **uEmuera** validates the cell-grid approach: it snaps every glyph quad to
  `fontSize/2` columns and `LineHeight` rows and keeps Emuera's wrapping /
  PRINTC / alignment arithmetic. Its own half/full classifier (`c < 0x127 ||
  exception set`) is demonstrably wrong versus MS Gothic and is not adopted.
- **Current renderer gaps:** no wrapping (`grid.rs` grows `col` past the grid),
  alignment and DRAWLINE width taken from the live surface in *cells*, DRAWLINE
  drawn from column 0 ignoring preceding parts, `FontStyle` never applied,
  fractional `cell_w` / `line_y` with a linear sampler, single non-growing
  atlas page, headless render ignores `bg_color` / `hl_color`.
- `Language`'s `CHINESE_HANS` / `CHINESE_HANT` labels are swapped
  (`crates/erars-compiler/src/parser.rs:355-358`), so Chinese games pick the
  wrong encoding; the width classifier depends on the encoding, so this is
  fixed here.
- No other ERA game or Emuera binary exists on the dev box; reference
  screenshots cannot be produced locally. Goldens therefore encode the rules
  above, not Emuera output. GPU is NVIDIA/Vulkan (no llvmpipe); MS Gothic,
  D2Coding and GulimChe are not installed system-wide — `msgothic.ttc` sits
  untracked at the repo root and must never be committed (proprietary).

## Invariants

- `cells(c, lang) ∈ {0, 1, 2}` is a pure function of the code point and the
  game language.
- A cluster of `n` cells is drawn inside `[x, x + n·half_w)` on row `r`, i.e.
  `[r·line_h, (r+1)·line_h)`. Nothing the font does can move it.
- `half_w`, `line_h`, `baseline` and every glyph origin are **integer physical
  pixels**.
- The VM's `STRLEN`, PRINTC padding and the renderer use the same `cells`.

## Component 1 — width classifier (`erars-ui::width`)

```rust
pub struct WidthTable { /* 2 bits per BMP code point, built once */ }
impl WidthTable {
    pub fn new(encoding: &'static encoding_rs::Encoding) -> Self;
    pub fn char_cells(&self, c: char) -> u8;      // 0, 1 or 2
    pub fn str_cells(&self, s: &str) -> usize;    // sum of char_cells
}
```

Rule, evaluated in order:

1. Control characters (`Cc`), format characters (`Cf`: ZWJ, ZWNJ, VS15/16,
   BOM…), combining marks (`Mn`, `Me`), Hangul jamo V/T (U+1160–U+11FF,
   U+D7B0–U+D7FF) → **0**. (`\n` and `\t` never reach the classifier — see the
   layout component.)
2. Encodable in the game encoding (`encoding_rs::Encoding::encode` without
   error) → **byte count** (1 or 2). Encodings: Japanese → Shift_JIS, Korean →
   EUC-KR, Chinese simplified → GBK, Chinese traditional → Big5. (This is the
   existing `VmContext::encoding` mapping; it moves to one function next to
   `Language` in `erars-compiler` and both the VM and console construction use
   it.)
3. Otherwise: Hangul syllables / L-jamo / compatibility jamo, East-Asian-Width
   `W` or `F`, Emoji_Presentation and Regional_Indicator → **2**; everything
   else → **1**.

The BMP is pre-computed into a table at construction (65 536 encodes, well
under a millisecond); astral code points go through the rule directly.
`unicode-width` is dropped from both crates; `pad` is dropped from `erars-ui`.
`encoding_rs` becomes an `erars-ui` dependency.

Expected values (tested): JP: `A` 1, `ｱ` 1, `あ` 2, `─` 2, `═` 1, `°` 2, `한`
2 (unencodable → Hangul), `😀` 2, `é` 1, U+0301 0. KR: `─` 2, `═` 2, `한` 2,
`ｱ` 1, `あ` 2.

Known, accepted deviations from Emuera: `¢ £ ¬ −` are 2 cells (Emuera draws
them half-width but counts them as 2 bytes in STRLEN); Hangul in a Japanese
game is 2 cells (Emuera's STRLEN counts the `?` replacement as 1 while GDI
FontLink draws a full-width GulimChe glyph).

## Component 2 — VM / console changes (`erars-ui`, `erars-vm`, `erars-compiler`, `erars-loader`)

- `VirtualConsole::new(printc_width, max_log, encoding)` builds a `WidthTable`
  and exposes `cells(&str)`.
- `printrc` / `print_button_rc` (PRINTC): if `cells(s) < printc_width`, prepend
  `printc_width − cells(s)` spaces. `printlc` / `print_button_lc` (PRINTLC):
  pad on the right to `printc_width + 1` cells. Strings at or beyond the field
  are printed unpadded. (Emuera: PrintCLength 25, PRINTLC 26.)
- Config defaults: `printc_width` 30 → **25**, `printc_count` 4 → **3**.
  Existing `tests/run_tests` fixtures do not use PRINTC, so no snapshots
  change; `examples`/docs that mention 30/4 are updated.
- `STRLEN`/`STRLENS`/`STRLENFORM`: `cells(s)` instead of the encoded byte
  length (identical for encodable text; fixes the `&#NNNN;` inflation).
  `STRLENU` (char count) is unchanged.
- Default text colour (192,192,192) instead of (255,255,255) in
  `VirtualConsole::new` and in the `DEFAULT_COLOR` reset
  (`executor.rs:196-200`, replacing the `TODO: respect config`).
- `Language` labels fixed (`CHINESE_HANS` ↔ `ChineseHans`).
- Print strings may contain `\n`; the console keeps them inside the part (as
  today) and the renderer splits rows on them. The stdio frontend is
  unaffected.

## Component 3 — font chain (`erars-renderer::font`)

```rust
pub struct FontConfig<'a> {
    pub family: &'a str,          // emuera.config フォント名 (may be empty)
    pub game_dir: &'a Path,       // <game>/font/*.ttf|ttc|otf is loaded (Emuera.EM)
    pub extra_dir: Option<PathBuf>, // ERARS_FONT_DIR
    pub lang: Language,
}
pub struct FontChain {
    font_system: cosmic_text::FontSystem, // fontdb + Font loading only
    chain: Vec<fontdb::ID>,               // ordered candidates, primary first
    primary: fontdb::ID,
    cache: HashMap<(char, StyleKey), fontdb::ID>,
}
impl FontChain {
    pub fn new(cfg: &FontConfig) -> Self;
    pub fn primary(&self) -> fontdb::ID;
    /// First chain font whose cmap covers `c` (bold/italic face of the same
    /// family preferred when the style asks for it), else the first font in
    /// the whole database that covers it, else the primary (renders .notdef).
    pub fn resolve(&mut self, c: char, style: FontStyle) -> fontdb::ID;
    pub fn font(&mut self, id: fontdb::ID) -> Arc<cosmic_text::Font>;
}
```

Chain order: configured family → faces from `<game>/font/` → faces from
`ERARS_FONT_DIR` → per-language fixed-pitch CJK candidates (JP: MS Gothic,
Sarasa Mono J, Noto Sans Mono CJK JP; KR: D2Coding, NanumGothicCoding,
GulimChe, Sarasa Mono K, Noto Sans Mono CJK KR; ZH: NSimSun, Sarasa Mono SC/TC,
Noto Sans Mono CJK SC/TC) → bundled Noto Sans Mono → every other face in load
order. Family matching is case-insensitive and accepts both name-ID-16 and
name-ID-1 names (so `MS Gothic` and `ＭＳ ゴシック` both work). Coverage is
checked with `Font::unicode_codepoints()` / `rustybuzz` `glyph_index`; results
are cached per (char, style). A `SETFONT` family that exists is inserted at the
head of the chain for that part; one that does not exist logs once and uses the
default chain (Emuera silently substitutes Microsoft Sans Serif).

cosmic-text's `Buffer`, `Attrs` and `SwashCache` are no longer used; the crate
stays for `FontSystem`/`fontdb` loading and `Font::{rustybuzz, as_swash}`.

## Component 4 — shaping and cell geometry (`erars-renderer::text`)

```rust
pub struct CellMetrics {
    pub scale: f32,      // winit scale factor
    pub font_px: f32,    // config.font_size * scale
    pub half_w: u32,     // round(primary space advance * font_px / upem)
    pub line_h: u32,     // round(config.line_height * scale)
    pub baseline: u32,   // round(primary ascent * font_px / upem), from the row top
}
pub struct ShapedGlyph { pub font: fontdb::ID, pub glyph: u16, pub dx: i32, pub dy: i32, pub size_px: f32 }
pub struct Cluster { pub cells: u8, pub src: Range<usize>, pub glyphs: Vec<ShapedGlyph> }
pub fn shape(chain: &mut FontChain, widths: &WidthTable, m: &CellMetrics,
             text: &str, style: &TextStyle) -> Vec<Cluster>;
```

- `text` is split into maximal spans that resolve to the same font; each span
  is shaped with rustybuzz (`liga`, `clig`, `calt` disabled so one character
  stays one glyph, as GDI does). Glyphs are grouped by rustybuzz cluster; a
  cluster's `cells` is the sum of `char_cells` over its source characters
  (base + combining marks = base width). A cluster with 0 cells is attached to
  the previous cluster's box.
- Box width `w = cells · half_w`. The glyph's natural advance `a` (in px at
  `font_px`): if `a ≤ w` the glyph is centred: `dx = floor((w − a) / 2)`. For
  the primary font `a == w` for every glyph (MS Gothic, D2Coding, Sarasa,
  GulimChe are exact 1:2 monospaces), so `dx = 0` — identical to GDI. If
  `a > w` (e.g. a 0.6 em Latin fallback in a 0.5 em grid) the whole span is
  reshaped at `size_px = font_px · w / a` so it fits, then centred.
- Vertical: `dy = baseline − y_offset`; every font shares the primary's
  baseline, so mixed-font rows stay level. The em box top sits at the row top
  like GDI; `line_h − font_px` slack is below.
- `half_w` comes from the primary font's *space advance*, not from `font_px/2`,
  so a 0.6 em primary still gets a coherent (wider) grid. With MS Gothic 18 px
  → 9; line 19; baseline 15.

## Component 5 — layout (`erars-renderer::layout`, replaces `grid.rs`)

```rust
pub struct Geometry { pub content_w: u32, pub drawable_w: u32, pub m: CellMetrics }
pub struct Layout { pub rows: Vec<Row>, pub buttons: Vec<ButtonRegion> }
pub struct Row { pub line: usize, pub logical_start: bool, pub width: u32, pub x0: i32, pub items: Vec<Item> }
pub enum Item {
    Glyph { x: i32, y: i32, font: fontdb::ID, glyph: u16, size_px: f32, color: [u8;3], flags: RasterFlags },
    Rect  { x: i32, y: i32, w: u32, h: u32, color: [u8;3] },   // underline / strike / hover bg
}
pub struct ButtonRegion { pub row: usize, pub x: i32, pub w: u32, pub input_gen: u32, pub value: Value }
pub fn layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper, hover: Option<usize>, active_gen: Option<u32>, hl: [u8;3]) -> Layout;
```

- `content_w` = the **live window inner width** in physical px (initial size
  from `window_width`/`window_height`; resizing relayouts). `drawable_w =
  content_w − max(2, round(font_px/6))` — Emuera's formula, so a 760 px window
  reproduces Emuera exactly and wider windows wrap later. Headless renders take
  `content_w` as a parameter.
- Rows are built by walking a line's parts with a pixel cursor `x`. For each
  cluster (`w = cells·half_w`): if `x + w > drawable_w` and `x > 0`, the row is
  finished and the cluster starts a new row (character granularity, mid-word,
  no kinsoku — Emuera). A `\n` inside a part finishes the row. Button parts are
  split across rows; each fragment becomes its own `ButtonRegion` with the same
  `input_gen`/`value`. Continuation rows have `logical_start = false`.
- `Line` (DRAWLINE/CUSTOMDRAWLINE): the string is repeated `ceil(drawable_w /
  cells(s)·half_w)` times, then trailing characters are dropped while the width
  exceeds `drawable_w`; the result is laid out as ordinary text **after** the
  parts already on the line (so with pending text it wraps to its own row, as
  in Emuera 1.824). With defaults this is 84 `-`.
- Alignment per row, in pixels, applied after wrapping: Left `x0 = 0`; Center
  `x0 = content_w/2 − width/2` (integer divisions, Emuera's form); Right
  `x0 = content_w − width`; clamped at 0.
- Styles: bold/italic are carried to the rasterizer as flags (real face if the
  chain has one, synthetic otherwise). Underline = 1 px rect at
  `row_y + font_px − 1`, strike = 1 px rect at `row_y + font_px/2`, both
  spanning the cluster boxes of the styled run (uEmuera's placement).
- Buttons: clickable only when `input_gen == active_gen`; the hovered fragment's
  text is drawn in `hl_color`. Hit rect = `[x0 + x, row_y, w, line_h]`.
- View: rows are **bottom-anchored** above an input strip `line_h` tall (shows
  `> input_`), like Emuera's picture box + input box. Scrolling is in whole
  rows (Emuera can show a partial top row — accepted deviation). Stick-to-bottom
  on new frames; wheel scrolls one row per notch.
- The layout is recomputed when the frame, the window width, the hover state or
  the active generation change; shaping results are cached per (font, text,
  style) so repeated screens are cheap.

## Component 6 — rasterization and atlas (`erars-renderer::raster`, replaces `atlas.rs` internals)

```rust
pub struct RasterKey { font: fontdb::ID, glyph: u16, size_bits: u32, flags: RasterFlags } // flags: BOLD_SYNTH | ITALIC_SYNTH | NO_BITMAP
pub struct GlyphRaster { ctx: swash::scale::ScaleContext, pages: Vec<AtlasPage>, map: HashMap<RasterKey, Option<AtlasRegion>> }
impl GlyphRaster { pub fn get(&mut self, device, queue, font: &Font, key: RasterKey) -> Option<AtlasRegion>; }
```

- swash is called directly (`font.as_swash()`): `Render::new(&[
  Source::Bitmap(StrikeWith::ExactSize), Source::ColorBitmap(BestFit),
  Source::ColorOutline(0), Source::Outline])`, `.format(Alpha)`,
  `.hint(true)`, integer offsets only. With MS Gothic at an integer `font_px`
  between 10 and 22 the EBDT strike is used → GDI-identical pixels. The
  `--no-bitmap-strikes` CLI flag removes `Source::Bitmap`.
- Synthetic bold: `Render::embolden(font_px / 24)` on the outline path (bitmap
  strikes cannot be emboldened, so bold falls back to outlines). Emuera's GDI
  bold widens glyphs by ~1 px and breaks its own grid; we keep the grid —
  accepted deviation. Synthetic italic: `Render::transform(skew(−0.2))`.
- Atlas: pages of 2048² `Rgba8Unorm` (mask glyphs white + alpha, colour glyphs
  RGBA); when a page is full a new page is added and the draw list is split per
  page (one draw call per page). Sampler becomes `Nearest` (glyphs are placed
  on integer pixels; this preserves the bitmap look).
- `Item::Rect` uses shader mode 0 (already in `shader.wgsl`).

## Component 7 — app / headless / CLI

- `app.rs`: holds `FontChain`, `CellMetrics`, the last `Layout`; relayouts on
  resize / scale change; hit-tests against `ButtonRegion`s in row space;
  draws the input strip; unchanged proxy wake, TINPUT and key handling.
- `headless.rs`: `render_frame(font, frame: &ConsoleFrame, content_w, height,
  active_gen)` honours `bg_color` / `hl_color`; `layout_snapshot(&Layout) ->
  String` prints one row per line (`row 3 x0=357 w=45 | 0:2 "地" 18:1 "A" …`)
  for GPU-free goldens; `write_png` (via the `png` crate, cached offline)
  replaces PPM for `--headless-shot`.
- `FontChain::from_files(&[PathBuf], lang)` builds a chain from explicit files
  only (no system fonts, fixed locale) for deterministic tests.
- CLI: `--headless-shot PATH.png`, `--no-bitmap-strikes`, existing flags. Env:
  `ERARS_FONT_DIR`.

## Error handling

- Missing configured family → warning, chain continues (never abort; Emuera
  substitutes silently).
- A glyph no font covers → primary font's `.notdef` in a box of the classifier
  width; still aligned.
- Atlas allocation failure → new page; a glyph larger than a page (impossible
  at console sizes) is skipped with a warning once.
- `line_height == 0` or `font_size == 0` in config → clamped to Emuera's
  minimum (font 8 px, line ≥ font) with a warning.
- GPU/surface errors as before (exit on adapter/device failure, reconfigure on
  Lost/Outdated).

## Testing

1. **Width classifier** (`erars-ui`, no GPU): tables per language as listed in
   Component 1; `str_cells` of mixed strings; combining/ZWJ/VS16 zero-width.
2. **Console padding** (`erars-ui`): PRINTC/PRINTLC field widths 25/26 with
   half, full and mixed strings; overlong strings unpadded; button variants.
3. **VM** (`tests/run_tests`): new fixtures for `STRLEN` with box drawing /
   Hangul / emoji, `PRINTC`+`PRINTCPERLINE`, `DRAWLINE`, `ALIGNMENT`,
   `PRINTBUTTON` (text-level `.out`), plus `ConsoleSerde` JSON snapshots where
   alignment/colour matter.
4. **Layout goldens** (`erars-renderer`, no GPU): k9 snapshots of
   `layout_snapshot` for geometry 9/19/760 using the bundled font plus a
   test-only fixed-advance stub font: mid-word wrap, full-width cluster that
   does not fit moves whole, `\n`, Center/Right pixel offsets (`357` for a
   45 px row), DRAWLINE `-`×84 and trimming of a 3-character rule, PRINTC
   columns, button fragments across a wrap, underline/strike rects.
5. **Pixel tests** (GPU, `gpu_lock`, skip without adapter): with
   `FontChain::from_files([bundled Noto Sans Mono, Sarasa/Noto CJK if present])`
   — box-drawing frame `┏━━┓ / ┃  ┃ / ┗━━┛` over an ASCII ruler: ink of every
   column lands inside `[k·half_w, (k+cells)·half_w)`; mixed `가A한B` rows
   align; a fallback glyph never exceeds its box; identical rows identical.
   With `ERARS_FONT_DIR` pointing at `msgothic.ttc` (opt-in, never in CI): the
   32 JIS box characters are 2 cells and `═║` 1 cell, and an 18 px render uses
   the bitmap strike (pixel values are only 0/255 in the glyph body).
6. **Integration** (`crates/erars-renderer/tests/games/tui/`): a small synthetic
   game (UTF-8 BOM ERB + CSV + `emuera.config` KOREAN and JAPANESE variants)
   printing a box map, a PRINTC table, centred title, DRAWLINE and buttons; the
   test runs the VM headlessly, builds the `ConsoleFrame`, and snapshots the
   layout; a second, GPU test renders it to PNG for eyeballing.

## Out of scope / accepted deviations

- Images, shapes, `<font face>` beyond family switching, GDI+ GRAPHICS-mode
  1.04 quantisation, partial top-row scrolling, GDI bold widening, backlog
  `LogColor`, tooltips, IME composition display.
- `¢ £ ¬ −` and Hangul-in-Japanese width differences listed above.
- Emoji ZWJ sequences occupy the sum of their parts' cells (one glyph centred in
  a wide box); Emuera has no defined behaviour here.

## Workspace changes

- `erars-ui`: `+encoding_rs`, `−pad`; new `width.rs`; `VirtualConsole::new`
  signature.
- `erars-compiler`: `Language::encoding()` (`+encoding_rs`), label fix,
  defaults 25/3.
- `erars-vm`: `VmContext::encoding` delegates; STRLEN uses cells; default
  colour.
- `erars-loader`, `tests/run_tests.rs`: pass the encoding to
  `VirtualConsole::new`.
- `erars-renderer`: `+rustybuzz` (already in the lock via cosmic-text),
  `+swash` (direct), `+png`, `−unicode-width`; `grid.rs` → `layout.rs`,
  `atlas.rs` → `raster.rs`; `font.rs`/`text.rs`/`draw.rs`/`headless.rs`/
  `app.rs`/`main.rs` reworked as above; `assets/` gains nothing proprietary.
- `justfile`: `headless-shot` writes PNG; `test-align` runs the layout goldens
  and pixel tests.
