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

## Emuera parity

`docs/research/2026-09-03-emuera-command-gap.md` tracks command/function coverage against the Emuera
C# source. As of 2026-09-03 every `InstructionCode` has an explicit parser arm (the `match` is
exhaustive and compiler-enforced), and no executor arm is a stub. §5 of that document is the single
consolidated list of **deliberate deviations** from Emuera — mostly places where a text console has no
pixel surface, a VM has no host locale, or erars tightens an ordering Emuera leaves unspecified. Each
entry is also marked in the source with a `DELIBERATE …` comment naming the Emuera line and the reason.

## Input logs

Every front end records the input waits it answers into the game's `logs/`
directory, so a session can be measured and replayed afterwards:

- `logs/inputs_<stamp>.jsonl` — one JSON object per answered wait: the wall
  clock, the milliseconds since the session started, how long the answer took,
  the function that asked, the request type, and the value or the raw
  mouse/key event. `logs/last_inputs.jsonl` always points at the newest one.
- `logs/inputs_<stamp>.ron` — the same answers as the `Vec<Value>` that
  `erars-stdio --use-input <file>` replays, kept valid after every single
  input, so an interactive session can be re-run verbatim.

`ERARS_NO_INPUT_LOG=1` records nothing. A log that cannot be opened or written
is dropped with a warning and never stops the game.

`cargo run -p erars-loader --example analyze_inputs -- <logs dir | *.jsonl>`
turns those logs into usage statistics: the breakdown by request type, the
functions the waits come from with their latencies, the answers players
actually give, the think-time percentiles, which function follows which — and
the optimisation suggestions those numbers support.

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
