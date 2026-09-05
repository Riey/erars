# Font-Metrics-Dependent Test Fragility in `erars-vm`'s Default-Font-Fallback Test

Date: 2026-09-05
Scope: `crates/erars-vm/src/graphics.rs`, test `graphics::tests::draw_text_falls_back_to_fore_color_and_the_console_style` (as of commit `73909e7`, currently at line 2069). Found while diagnosing why this test failed on every CI `Check` run before that commit; the CI fix (installing `fonts-liberation` and pointing `ERARS_FONT_DIR` at it) makes the test pass reliably, but does not resolve the fragility described here — it is a separate, deliberately-unfixed finding.
Method: reproduced the CI failure locally with `bwrap --unshare-all ... --tmpfs /usr/share/fonts` (a genuine zero-system-font sandbox, confirmed against `/etc/fonts/fonts.conf`'s search paths), then swapped in different real font files via `ERARS_FONT_DIR` (`crates/erars-font/src/font.rs`'s `FontChain::new`, `extra_dir` seed — bypasses family-name matching entirely and becomes primary via `first_regular`) and read the instrumented `em`/`fill.top`/`fill.height` values `render()` (`crates/erars-font/src/text_image.rs`) actually produced for each.

---

## 1. The test asserts ink exists; nothing asserts *where*

The test (`crates/erars-vm/src/graphics.rs:2069-2091`) draws `"-"` at `GDRAWTEXT`'s default 100px font (`DEFAULT_FONT_SIZE_PX`, `crates/erars-font/src/text_image.rs:284`) onto an 80px-tall bitmap (`s.create(1, 120, 80)`), then asserts `!ink.is_empty()` — some pixel in the whole bitmap has nonzero alpha. No assertion touches the glyph's vertical position.

Whether that assertion holds depends entirely on `em_for_drawing` (`crates/erars-font/src/text_image.rs:195-197`, `m.line_height(size_px).ceil().max(1.0)`) for whichever font `FontChain` resolves as primary, and where the glyph's `fill.top` lands relative to the canvas height. `blend_coverage` (`crates/erars-vm/src/graphics.rs:170-207`) clips silently: rows outside `0..height` are simply never drawn, with no warning and no distinct failure mode from "the glyph was never even close."

## 2. Measured `em` and clip outcome per font (100px request, "-" glyph, 80px canvas)

| Font | `em` (px) | `fill.top` | `fill.height` | Outcome |
|---|---|---|---|---|
| Bundled `NotoSansMono-Regular.ttf` (embedded fallback, `crates/erars-font/src/font.rs:21`) | 137 | 103 | 11 | **Fails** — top already past the 80px canvas |
| Real `NotoSansMonoCJKjp-Regular.otf` ("Noto Sans Mono CJK JP", the exact string `language_candidates` looks for at `crates/erars-font/src/font.rs:74-79`) | 145 | 122 | 11 | **Fails, worse than the bundled fallback** |
| System `Sarasa Mono J` (`Sarasa-Regular.ttc`, matched via `language_candidates`) | 125 | 74 | 9 | Passes — 6-row margin before the 80px edge |
| Liberation Mono (`LiberationMono-Regular.ttf`, via `ERARS_FONT_DIR`, bypasses family matching) | 114–117 | 59–60 | 9–14 | Passes — ~20-row margin |

The ranking has nothing to do with a font being "correct" or "installed": it is purely a coincidence of each font's own vertical-metrics tables (`hhea`/`OS2` line-spacing relative to `upem`) at this specific 100px/80px combination. A CJK-capable font is not a proxy for "small metrics" — the real, officially-distributed Noto Sans Mono CJK JP file fails *harder* than the bundled font that's already known to fail.

## 3. How CI currently sidesteps this rather than resolving it

`.github/workflows/check.yml` (as of `73909e7`) installs `fonts-liberation` and sets `ERARS_FONT_DIR` to a directory containing only its Mono faces before `cargo test`. Per §2, Liberation Mono's metrics happen to keep this specific glyph on-canvas with a comfortable margin, so the test passes deterministically on any runner image regardless of what it preinstalls. That is a legitimate fix for "CI is flaky/broken depending on the runner's font set," but it does not change that the test's *contract* — "the default-font fallback draws something visible" — is unverifiable in general: swap `ERARS_FONT_DIR` for a different font, or change `DEFAULT_FONT_SIZE_PX`, or resize the test canvas even slightly, and the same test can fail again for reasons unrelated to whether font fallback actually works.

## 4. Candidate hardenings (none applied in this pass)

- **Widen the test canvas** so it comfortably exceeds `em_for_drawing(bundled, 100px)` (137px absolute worst case measured, plus glyph height) regardless of which font resolves as primary. Simplest, but doesn't defend the *real* production risk (a real deployment's `GDRAWTEXT` canvas is whatever the game script requests, not padded for this).
- **Clamp or reconsider `em_for_drawing`** (`crates/erars-font/src/text_image.rs:195-197`) so a font's line-spacing can't inflate the effective draw size this far past its nominal `size_px` (100px requested → 145px em observed for one real font). This is a production behavior change, not just a test fix, and needs its own scoped review against Emuera's actual GDI+ semantics (the function's doc comment ties it directly to `GraphicsImage.cs:127`/`Creator.Method.cs:5555`).
- **Assert on metrics directly** instead of on pixel presence — e.g. read `em_for_drawing`/`fill.top` from the resolved font and assert the glyph is expected to land within the canvas for *this* font, rather than asserting an outcome that silently depends on which font is primary.

Whichever direction is chosen belongs in its own pass with its own review, per the explicit instruction that kept this finding separate from the CI-fix commit (`73909e7`).
