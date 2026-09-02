# erars

*WARNING: This software is still experimental!*

ERA runtime made with Rust

## Run nightly

Download `erars.exe` from https://github.com/Riey/erars/actions/workflows/nightly.yml

## Why new runtime

There is `Emuera` which has emerged as an alternative to eramaker, but at this point it's very slow and dependent on the Windows platform.

erars provides fast start time and cross-platform support.

## Current start time

In my machine (r7 5800x) loading eraTHYMKR(about 1,445,295 ERB lines) with Emuera takes 7 seconds.

And with erars, it takes 0.5 second. Note that this program supports multi threading. 

## GUI renderer (`erars-renderer`)

`cargo run -p erars-renderer -- <game dir>` opens the winit/wgpu console. Text
is laid out on a half-width cell grid exactly like Emuera 1.824 (per-language
cell widths, `WindowX − max(2, FontSize/6)` drawable width, integer pixel
metrics); the design is in
`docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md`.

- Fonts, in order: `フォント名` from `emuera.config` → `<game>/font/*.ttf|ttc|otf|otc`
  → `ERARS_FONT_DIR` (scanned recursively, so point it at a directory that
  holds only fonts — not a source tree or `$HOME`) → the language's fixed-pitch CJK
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
