# erars-renderer Design

**Date:** 2026-06-15
**Status:** Approved, pending implementation plan

## Goal

Build `erars-renderer`: a GPU-powered, OS-independent frontend for the erars ERA
runtime that renders the console as a **true fixed-width (monospace) cell grid**
with cross-script font fallback — i.e. it lays out mixed Latin / CJK / Korean /
emoji text on terminal-style columns where every cell is the same width and East
Asian Wide / Fullwidth characters occupy exactly two cells.

This replaces the existing `erars-iced` frontend, whose problems motivate the
work:

- Hardcoded `C:\Windows\Fonts\...` font paths → not OS-independent.
- Uses iced's high-level `text`/`button` widgets → **no true fixed-width grid**;
  mixed-script lines do not column-align.
- `ConsoleLine.align` (Center/Right) is commented out.
- `Line` parts repeat a char 4× instead of filling the line width.

## Context: how erars feeds the renderer

The VM produces a `VirtualConsole` of `ConsoleLine`s. Each line has an
`Alignment` and a `Vec<ConsoleLinePart>`:

- `Text(String, TextStyle)`
- `Line(String, TextStyle)` — a rule char meant to fill the row width
- `Button(Vec<(String, TextStyle)>, input_gen: u32, Value)` — clickable

`TextStyle` carries `color: Color([u8;3])`, `font_family: SmolStr`, and
`font_style` (bold/italic/strikeline/underline bitflags).

Frontends receive immutable `ConsoleFrame { bg_color, hl_color, lines }`
snapshots and `InputRequest`s through the proxy system (`erars-proxy-system`):

- `SystemRequest::Redraw(ConsoleFrame)`
- `SystemRequest::Input(InputRequest)` — types: `AnyKey`, `EnterKey`,
  `ForceEnterKey`, `Int`, `Str`; optional `timeout: Timeout` (TINPUT).
- `SystemRequest::Quit`

The frontend replies with `SystemResponse::{Empty, Input(Value)}` over `res_tx`.

`new_proxy(notify: Arc<dyn Fn() + Send + Sync>)` takes a `notify` callback,
currently a no-op in `erars-iced`. **This is the wake hook the renderer will
use** (see Event flow).

Relevant `EraConfig` fields: `font_family` (default `"D2Coding"`), `font_size`
(default 18), `line_height` (default 19), `window_width`/`window_height`,
`printc_width`, `max_log`.

## Stack decisions (settled during brainstorming)

- New standalone crate that **replaces** `erars-iced` (removed from the
  workspace).
- Window/input via **winit**; GPU via **wgpu**; shaping + font fallback +
  rasterization via **cosmic-text** (`FontSystem`, `SwashCache`); **custom glyph
  atlas** (we do not use glyphon).
- Font source: **system fonts via fontdb + a bundled monospace fallback**. The
  bundled font is a small permissively-licensed (OFL) **Latin-only** monospace
  guaranteeing a baseline; CJK/Korean/emoji come from system fonts through
  cosmic-text's `monospace_fallback`.
- **Always snap to the cell grid**: every grapheme cluster occupies an integer
  number of cells regardless of the font's natural advance.
- v1 must-haves (all in scope): line alignment (Left/Center/Right), full-width
  `Line`/rule fill, clickable buttons + hover, input timeout (TINPUT).

All of cosmic-text, swash, etagere, unicode-width, wgpu, and winit are already
present in `Cargo.lock` (transitively via iced today), so they are known-good on
this toolchain.

## Grid → glyph mapping (Approach A)

For each `(text, style)` run:

1. Shape the whole run with cosmic-text — **this is where font fallback
   happens** (per-cluster fallback across scripts).
2. **Ignore cosmic-text's x-advances.** Group shaped glyphs by their source
   `cluster` byte offset. For each cluster, compute its cell width as
   `cells = max(1, unicode_width(cluster_substr))` using `unicode-width`
   (East-Asian-Wide / Fullwidth → 2; ambiguous-width → narrow/1 by default).
3. Assign the cluster the next free column; advance the column counter by
   `cells`. Position its glyph(s) at `x = start_col * cell_w`, baseline-aligned,
   wide glyphs left-anchored within their two-cell span.

This yields cosmic-text's correct fallback/complex-script shaping **and** true
terminal columns. Rejected alternatives: per-grapheme shaping (loses
cross-cluster shaping, slower) and trusting cosmic-text advances with forced
monospace metrics (non-monospace fallback fonts break alignment — the very bug
being fixed).

## Module decomposition

Each module has one purpose and the layout/measurement logic is testable without
a GPU.

1. **`main.rs`** — CLI (clap), logging (flexi_logger + log_panics), `mimalloc`
   global allocator, spawns the VM runtime thread (same bootstrap as today's
   `erars-iced` `main`: `load_config`, `new_proxy`, runtime thread running
   `run_script`/`load_script` then `vm.start`), builds the winit event loop, and
   runs `App`.

2. **`font.rs`** — builds a cosmic-text `FontSystem` from
   `fontdb::Database::load_system_fonts()` **plus** a bundled monospace fallback
   embedded with `include_bytes!` (`db.load_font_data`). Resolves
   `config.font_family` as the default family. Computes **cell metrics**:
   `cell_w` = advance of a reference ASCII glyph (e.g. `'0'`/space) in the
   default monospace font at `font_size`; `cell_h` = `config.line_height`.
   Exposes these to `text.rs`/`grid.rs`.

3. **`text.rs` — `CellShaper`** — wraps `FontSystem`. Input: a run of
   `(String, TextStyle)`. Output:
   `Vec<PlacedGlyph { col, cell_span, font_id, glyph_id, color, x_px, y_px }>`
   plus the run's total column count. Implements Approach A above. No GPU
   dependency.

4. **`grid.rs` — `Grid`** — converts `ConsoleFrame` + window size + input state
   into draw lists. Computes `grid_cols = floor(window_w / cell_w)`. Per
   `ConsoleLine`:
   - Shape all parts via `CellShaper` to get total `line_cols`.
   - **Alignment**: shift the line's starting column — Left `0`, Center
     `(grid_cols - line_cols) / 2`, Right `grid_cols - line_cols` (clamped ≥ 0).
   - `Line` part: **repeat the rule char to fill `grid_cols`** (accounting for
     the char's own cell width).
   - `Button` part: emit a hit-rect (cell span → pixel rect) + `Value` +
     `input_gen`; coloring reflects enabled/hover (`hl_color`)/disabled.
   - Stack lines vertically by `cell_h`; apply scroll offset; stick-to-bottom on
     new frames. Reserve the bottom row for the input line + cursor.

   Output: glyph instances + solid-rect instances + the list of clickable button
   regions (for hit-testing in `app.rs`). No GPU dependency.

5. **`atlas.rs` — `GlyphAtlas`** — a growable wgpu texture (R8 alpha for glyph
   masks; an RGBA page for color glyphs/emoji) with an `etagere` rect packer.
   `get_or_insert((font_id, glyph_id, size)) -> AtlasRegion`, rasterizing on miss
   via cosmic-text `SwashCache`, uploading, and returning a UV region. On
   overflow, grow by recreating a larger texture and re-rasterizing (rare).

6. **`gpu.rs` — `GpuContext`** — wgpu instance/adapter/device/queue/surface
   config. **One instanced-quad pipeline**: each instance is
   `{ rect, uv_rect, color, mode }` with `mode ∈ {solid, alpha_mask, rgba}`.
   `render(grid)` clears to `bg_color`, builds one batched instance buffer
   (solid rects for button-hover / cursor, then glyph quads), and submits a
   single draw. Orthographic pixel→NDC via a screen-size uniform. Handles
   resize and surface-lost/outdated reconfigure.

7. **`app.rs` — `App: winit::ApplicationHandler`** — the controller. Owns the
   window, `GpuContext`, `FontSystem`/`CellShaper`, current `ConsoleFrame`,
   `Grid`, scroll offset, input state, and current `InputRequest`. On wake,
   drains the proxy `req_rx` and dispatches:
   - `Redraw(frame)` → store frame, mark stick-to-bottom, request redraw.
   - `Input(req)` → auto-skip `AnyKey`/`EnterKey` when in skip mode (parity with
     current behavior), else store as current request.
   - `Quit` → close window.

   Handles winit input: keyboard for `Int` (digits only), `Str`, `AnyKey`/
   `EnterKey`/`ForceEnterKey`; mouse move (hover) + click (button hit-test →
   `SystemResponse::Input(value)`); scroll wheel (scrollback). Drives the
   **TINPUT timeout**: track the deadline, send the default value on expiry,
   optionally render the remaining time.

## Event flow & the wake bridge

```
VM thread ──proxy──> req_rx ──(EventLoopProxy wake)──> App ──res_tx──> VM thread
```

The renderer constructs the proxy with
`new_proxy(Arc::new(move || event_loop_proxy.send_event(Wake).ok()))`. When the
VM thread sends a `SystemRequest`, the `notify` callback wakes the winit loop;
`App::user_event`/the drain step pulls all pending requests from `req_rx`. User
input/clicks/timeout send `SystemResponse` back over `res_tx`. No busy polling.

## Cell-width semantics

- Width via `unicode-width`: narrow = 1, East-Asian-Wide / Fullwidth = 2 (Hangul
  syllables are Wide → 2 cells, matching terminals).
- East-Asian-Ambiguous defaults to **narrow (1)**. (A config toggle for
  wide-ambiguous is a possible later addition, out of scope for v1.)
- Control/zero-width characters contribute 0; an empty/zero-width cluster is
  clamped to a minimum of 1 cell only if it is a visible cluster.

## Error handling

- wgpu adapter/device init failure → log a clear message and exit.
- Surface `Lost`/`Outdated` → reconfigure and retry; `OutOfMemory` → exit.
- Resize → reconfigure surface + recompute `grid_cols`.
- Bundled fallback font is compile-time guaranteed via `include_bytes!`; if
  system fonts are empty the bundled font still renders Latin.
- VM thread finishing / channel closing → app exits cleanly.

## Testing

GPU-free, deterministic (bundled font fixes `cell_w`):

- `"abc"` → 3 columns at origins 0,1,2.
- `"한글"` → 4 columns (2 each).
- mixed `"a한b"` → 4 columns with origins 0,1,3.
- Alignment: Center/Right starting-column offsets for a given `grid_cols`.
- `Line` fill: repeat count == `grid_cols / char_cells`.
- Button hit-rect maps to the correct cell span → pixel rect.

GPU/atlas paths are verified by a build + smoke render (a single frame). Logic in
`text.rs`/`grid.rs` is unit-tested directly.

## Out of scope (v1)

- Subpixel glyph positioning (integer grid positions only).
- Wide-ambiguous-width config toggle.
- Image/graphics (`gDRAW`-style) console features beyond text/line/button.
- Reusing the renderer as an embeddable library (it is a standalone binary; a
  later split is possible but not designed here).

## Workspace changes

- Add `crates/erars-renderer` to the workspace `members`.
- Remove `crates/erars-iced` from `members` and delete the crate.
- Add the bundled fallback font asset under `crates/erars-renderer/assets/`.
