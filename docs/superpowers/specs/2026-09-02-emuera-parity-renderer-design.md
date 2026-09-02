# Emuera-parity text renderer — design

**Date:** 2026-09-02 (rev 2, after adversarial review)
**Status:** Approved in chat (2026-09-02), pending implementation plan
**Supersedes:** the width/fallback/layout sections of
`2026-06-15-erars-renderer-design.md` (the GPU pipeline and the proxy/event
flow described there still apply unless changed below).

## Goal

Make `erars-renderer` lay out and draw the ERA console **exactly like Emuera
1.824 (TEXTRENDERER mode, MS Gothic)** and uEmuera, so that ASCII-art maps,
box-drawing frames and PRINTC tables never mis-align, no matter which
installed font ends up supplying a glyph. Concretely:

1. Every grapheme occupies a box of **1 or 2 half-width cells** whose width is
   decided by the code point and the game language alone, never by the font
   that draws it ("perfect fallback": the layout is font-independent).
2. The VM (`STRLEN`, `SUBSTRING`/`STRFIND`, `{}`/`%%` width padding,
   PRINTC/PRINTLC padding) and the renderer share **one width function**, so
   what the script measures is what the screen shows.
3. Line pitch is always `LineHeight`; wrapping, alignment and DRAWLINE length
   follow Emuera's pixel rules (character-granular wrapping at the drawable
   width, pixel alignment with Emuera's integer arithmetic, rule repeated to
   the drawable width, text drawn `shift` px right of its logical x as GDI
   does).
4. Glyph pixels match GDI where possible: MS Gothic's embedded 1-bit strikes
   (read via ttf-parser) are used at integer pixel sizes 10–22, hinted outlines
   otherwise.

Scope is **text only** (text, buttons, rules, bars, colours, bold / italic /
underline / strike). Images and shapes (`PRINT_IMG`, HTML `<img>` / `<shape>`)
stay out of scope; the VM keeps emitting them as text.

## Findings this design rests on

Verified against sources during the 2026-09-02 investigation and its
adversarial review; file references are to this repo unless noted. Reference
C# sources (Emuera 1.824 fork, Emuera.EM, uEmuera) were read from fetched
copies.

- **Emuera's grid is the font's advance.** MS Gothic (`msgothic.ttc` face 0,
  upem 256) has only two outline advances: 128 (half) and 256 (full) — at
  18 px exactly 9 / 18 px; hhea ascent 220 + descent 36 = one em, so the GDI
  cell is 18 px tall inside a 19 px line. Emuera measures per styled part with
  `TextRenderer.MeasureText(NoPadding|NoPrefix)` (integer px) but draws with
  `TextRenderer.DrawText(.., NoPrefix)` *without* `NoPadding`, so GDI's
  overhang padding pushes the ink `max(2, FontSize/6)` px right of `PointX`;
  Emuera compensates with `DrawingParam_ShapePositionShift` (same value) and
  `DrawableWidth = WindowX − shift` (Config.cs:169-172, integer division).
  Wrapping happens at `PointX + Width > DrawableWidth` with character
  granularity (PrintStringBuffer.cs:178-249); CENTER is
  `WindowX/2 − width/2`, RIGHT `WindowX − width` (ConsoleDisplayLine.cs:84-87,
  C# integer arithmetic; Emuera.EM aligns to `DrawableWidth` instead — we
  follow 1.824). DRAWLINE repeats the rule string until the measured width
  reaches `DrawableWidth`, then trims (Print.cs:543-560). Defaults
  (ConfigData.cs:47-64): `ＭＳ ゴシック` 18 px, LineHeight 19, WindowX 760,
  WindowY 480, PrintCLength 25 (PRINTLC uses 26), PrintCPerLine 3, ForeColor
  (192,192,192), BackColor (0,0,0), FocusColor (255,255,0), ButtonWrap false.
  Emuera's drawing width is fixed at `WindowX` (only the height is sizable).
- **Half/full truth = encoding byte count.** Over MS Gothic's whole cmap, cells
  equal Shift-JIS byte count for every cp932-encodable code point except
  `−` U+2212 (font half, cp932 double). Unicode East-Asian-Width does *not*
  reproduce it: `width_cjk` (Ambiguous = 2, what the renderer uses today)
  disagrees on 551 code points (`═ ║ ╔` double lines, dashed/rounded box
  drawing, block elements `▀ █ ░`, `▶ ◀`, `♥ ♡`, `↔`, Latin-1 `¡ ¤ ª ®`…), the
  narrow table (Ambiguous = 1, what the VM's `pad` crate uses today) on 746
  (`★ ● ※ → ① Ⅰ ° ± × ÷`, JIS Greek/Cyrillic, the 32 JIS box-drawing
  characters).
- **The rule is per language *encoding*, not per Unicode class.** GBK and Big5
  encode `═ ║ ╔ … ╬` and `█` (2 bytes → 2 cells) while Shift_JIS and EUC-KR do
  not (→ 1 cell); KS X 1001 holds only the 68 single/mixed-weight box glyphs
  `─━│┃┌…╋` plus `▒`, and `░` is in none of the four encodings (encoding_rs
  0.8.33 `euc_kr.rs:207`, `data.rs` KSX1001_BOX/KSX1001_SYMBOLS). Emuera's
  KOREAN `STRLEN` (.NET cp949) has no best-fit entry for U+2550–U+256C, `░` or
  `█`, so it also counts them as the 1-byte `?`. GulimChe's *drawn* advance for
  those glyphs is unverified (font not available); if it is full-width, Korean
  Emuera draws 2 cells while counting 1 — an Emuera inconsistency, not
  something the classifier encodes. uEmuera's classifier (`c < 0x127 ||
  exception set`) is language-independent and contradicts MS Gothic (`═` full
  but `║` and all block elements half) and is not adopted.
- **encoding_rs implements WHATWG encoders, not Windows best-fit.** In
  Shift_JIS, `¢ £ ¬` (U+00A2/A3/AC), `—` U+2014, `‖` U+2016 and `〜` U+301C are
  unmappable (cp932 best-fits them to 2 bytes; MS Gothic draws `‖ 〜 —`
  full-width), `−` U+2212 maps to 0x817C (2 bytes), `¥`/`‾` to 1 byte, the
  user-defined area U+E000–U+E757 is unmappable (cp932: 2 bytes, MS Gothic
  full-width). The classifier therefore carries a small per-language override
  table (Component 1).
- **erars has four inconsistent width notions today:** renderer `width_cjk`
  (`crates/erars-renderer/src/text.rs:94`), PRINTC padding via `pad` →
  `UnicodeWidthStr::width` (`crates/erars-ui/src/lib.rs:363-389`), form-string
  padding `{x,w}`/`%s,w%` via `pad` (`crates/erars-vm/src/terminal_vm/executor.rs:339-352`),
  and STRLEN/SUBSTRING/STRFIND via `encoding_rs` which turns unencodable
  characters into `&#NNNN;` (8 bytes for Hangul in a Japanese game,
  `executor.rs:1046-1077, 1319-1355`).
- **Emuera splits `PRINT` strings at `\n` into logical lines**
  (EmueraConsole.Print.cs:311-328: head → buffer, `NewLine()`, recurse), so
  LINECOUNT / CLEARLINE / ALIGNMENT see every embedded newline; PRINTC,
  PRINTPLAIN, PRINTSINGLE do not split, and PRINTBUTTON strips `\n`
  (Process.ScriptProc.cs:117-135). erars keeps `\n` inside the part today
  (`crates/erars-ui/src/lib.rs:335-347`); the string-literal parser and
  `UNICODE(10)` do produce it.
- **cosmic-text 0.12.1 cannot express an ordered fallback list** for a named
  family (hard-coded per-script Noto list, looks for the non-existent Linux
  family "Noto Sans CJK JA", matches locales only as bare `ja`/`ko`,
  Common-script characters get no script fallback at all). Its `Buffer` also
  imposes its own line metrics and sub-pixel binning. The `monospace_fallback`
  feature gates `Family::Monospace` fallback *and* whether `Font::new` walks
  the cmap (`Font::unicode_codepoints()` is `&[]` without it, and silently
  empty for a `monospaced` face without a space glyph); `shape-run-cache` only
  serves `Buffer` shaping. Neither is needed once `Buffer`/`Attrs` are gone.
- **swash 0.1.18 cannot read MS Gothic's bitmap strikes for CJK.** Its EBLC
  locator handles index formats 1–4 only (and the format-4 search is broken:
  `l = i + i`, `src/strike.rs` ≈606-660, unchanged on master). In
  `msgothic.ttc` face 0 every 10–22 ppem strike stores ASCII / half-width kana
  / full-width Latin in index format 2 but **all kana, kanji, box drawing and
  symbols in index format 5**, so `Source::Bitmap` would yield a mixed row
  (crisp ASCII next to anti-aliased `あ ─ ═`). ttf-parser 0.21.1 (already in
  the lock via rustybuzz, reachable as `rustybuzz::ttf_parser` from
  `cosmic_text::Font::rustybuzz()`) parses EBLC index formats 1–5 and EBDT
  image formats 1/2/5/6/7 — verified on the same file for `A あ 漢 ─ ═`. Its
  strike selection is *nearest*, not exact (23 px → the 22 ppem strike), so
  the caller must reject `pixels_per_em != font_px`. MS Gothic's odd-ppem
  strikes (11, 13, …, 21) have full-width advance `= ppem = 2·half − 1`, so at
  odd sizes Emuera's own GDI layout is not a 1:2 grid.
- **uEmuera** validates the cell-grid approach: it snaps every glyph quad to
  `fontSize/2` columns and `LineHeight` rows and keeps Emuera's wrapping /
  PRINTC / alignment arithmetic; hover is colour-only at draw time in every
  Emuera variant.
- **Current renderer gaps:** no wrapping (`grid.rs` grows `col` past the grid),
  alignment and DRAWLINE width taken from the live surface in *cells*, DRAWLINE
  drawn from column 0 ignoring preceding parts, `FontStyle` never applied,
  fractional `cell_w` / `line_y` with a linear sampler, single non-growing
  atlas page, headless render ignores `bg_color` / `hl_color`, relayout on
  every mouse move.
- `Language`'s `CHINESE_HANS` / `CHINESE_HANT` labels are swapped
  (`crates/erars-compiler/src/parser.rs:355-358`), so Chinese games pick the
  wrong encoding; the width classifier depends on the encoding, so this is
  fixed here.
- No other ERA game or Emuera binary exists on the dev box; reference
  screenshots cannot be produced locally. Goldens therefore encode the rules
  above, not Emuera output. GPU is NVIDIA/Vulkan (no llvmpipe); GitHub's
  ubuntu runner has no adapter unless lavapipe is installed. MS Gothic,
  D2Coding and GulimChe are not installed system-wide — `msgothic.ttc` sits
  untracked at the repo root and must never be committed (proprietary).

## Invariants

- `cells(c, lang) ∈ {0, 1, 2}` is a pure function of the code point and the
  game language.
- A cluster of `n` cells is drawn inside `[shift + x0 + x, shift + x0 + x +
  n·half_w)` on row `r`, i.e. `[r·line_h, (r+1)·line_h)`. Nothing the font
  does can move it.
- `font_px`, `half_w`, `line_h`, `baseline`, `shift` and every glyph origin
  are **integer physical pixels**.
- The VM's `STRLEN`/`STRLENS`/`STRLENFORM`, `SUBSTRING`/`STRFIND`, `{}`/`%%`
  width padding (`PadStr`), PRINTC/PRINTLC padding and the renderer use the
  same `cells`.

## Component 1 — width classifier (`erars-ui::width`)

```rust
pub struct WidthTable { /* 2 bits per BMP code point, built once */ }
impl WidthTable {
    pub fn new(encoding: &'static encoding_rs::Encoding) -> Self;
    pub fn char_cells(&self, c: char) -> u8;      // 0, 1 or 2
    pub fn str_cells(&self, s: &str) -> usize;    // sum of char_cells
}
```

Rule, evaluated in order (steps 1 and 3 use `unicode_width::UnicodeWidthChar::width`
from unicode-width 0.1.11, the *non-CJK* table, plus explicit overrides):

1. **Zero width.** `width(c) == None` (controls) or `Some(0)` (combining marks
   `Mn`/`Me`, format characters: ZWJ, ZWNJ, VS1–16, BOM, U+2060–2064, Hangul
   V/T jamo U+1160–U+11FF) → **0**. Overrides applied first: U+00AD → 0,
   U+D7B0–U+D7FF (V/T jamo extensions) → 0. (`\n` and `\t` are handled before
   shaping and never reach the classifier — Components 2 and 4.)
2. **Encodable in the game encoding** (`Encoder::encode_from_utf8_without_replacement`;
   `EncoderResult::Unmappable` means "not encodable") → **byte count** (1 or
   2). Encodings: Japanese → Shift_JIS, Korean → EUC-KR, Chinese simplified →
   GBK, Chinese traditional → Big5 (the existing `VmContext::encoding`
   mapping, moved to `Language::encoding()` in `erars-compiler`).
   **2b. Best-fit overrides** when step 2 fails, mirroring Windows cp932 so
   that STRLEN and MS Gothic agree: Japanese only — U+00A2 `¢`, U+00A3 `£`,
   U+00AC `¬`, U+2014 `—`, U+2016 `‖`, U+301C `〜`, and the user-defined area
   U+E000–U+E757 → **2**.
3. **Otherwise:** `width(c) == Some(2)` (East-Asian-Width `W`/`F`, Hangul
   syllables / L-jamo / compatibility jamo, emoji-presentation characters) or
   Regional_Indicator U+1F1E6–U+1F1FF (hard-coded) → **2**; everything else →
   **1**.

The BMP is pre-computed into a table at construction (about 10 ms per
language with the non-allocating encoder, built once per console); astral
code points go through the rule directly.

Expected values (tested):

| language | 1 cell | 2 cells | 0 |
|---|---|---|---|
| JP (Shift_JIS) | `A` `ｱ` `═` `║` `░` `█` `▶` `é` `♥` `¥` | `あ` `─` `°` `※` `★` `α` `А` `①` `〜` `‖` `¢` `−` U+E000 `한` `😀` | U+0301 U+200D U+00AD |
| KR (EUC-KR) | `A` `ｱ` `═` `░` `█` `¢` | `한` `あ` `─` `▒` `★` `①` `😀` | U+0301 U+1160 |
| ZH-Hans (GBK) / ZH-Hant (Big5) | `A` `ｱ` `░` | `═` `║` `█` `中` `한` | U+0301 |

Known, accepted deviations from Emuera + MS Gothic (all rare): `−` U+2212 is
2 cells (MS Gothic draws it half; STRLEN agrees with Emuera). `¢ £ ¬` are 2
cells (Emuera STRLEN 2, drawn half). About 420 symbols MS Gothic ships
full-width but cp932 lacks (enclosed alphanumerics `⑴ ⒈`, dingbats `✂ ✓`,
`☀ ☎ ☐`, `⌘`, Roman numerals `Ⅺ`–`ↂ`, pinyin `Ǎ`–`ǜ`) are 1 cell — the glyph
is scaled into its cell (Component 4); Emuera STRLEN counts them 1 while GDI
draws them full, so Emuera's own layout is inconsistent there. Combining
marks and U+00AD are 0 cells (MS Gothic carries them as half-width spacing
glyphs). Hangul in a Japanese game is 2 cells (GDI FontLink draws GulimChe
full-width; Emuera STRLEN counts the `?` replacement as 1).

## Component 2 — VM / console changes (`erars-ui`, `erars-vm`, `erars-compiler`, `erars-loader`)

- **Construction.** `erars_ui::ConsoleConfig { printc_width: usize, max_log:
  usize, encoding: &'static Encoding, fore_color: Color, bg_color: Color,
  focus_color: Color }` and `VirtualConsole::new(&ConsoleConfig)`. `erars-vm`
  provides `pub fn console_config(cfg: &EraConfig) -> ConsoleConfig` (uses
  `cfg.lang.encoding()`), used by `erars-loader` (two sites) and
  `tests/run_tests.rs`. The `WidthTable` is held as `Arc<WidthTable>`
  (`VirtualConsole` derives Clone/Debug) and exposed as `cells(&str) -> usize`
  and `char_cells(char) -> u8`.
- **`Language::encoding(&self) -> &'static encoding_rs::Encoding`** in
  `erars-compiler` (labels `CHINESE_HANS` ↔ `ChineseHans` fixed);
  `VmContext::encoding` delegates to it.
- **PRINTC / PRINTLC.** One private helper pads by cells: `printrc` /
  `print_button_rc` (PRINTC): if `cells(s) < printc_width`, prepend
  `printc_width − cells(s)` spaces. `printlc` / `print_button_lc` (PRINTLC):
  append spaces to `printc_width + 1` cells. Strings at or beyond the field
  are printed unpadded (Emuera `CreateTypeCString`, 25 / 26). `printlc` /
  `printrc` push the padded text with `push_text` directly (not through
  `print`, so no `\n` split) and clear `last_line.button_start` before and
  after (Emuera `Append(.., force_button=true)`: a PRINTC item never merges
  with neighbouring text into one button).
- **`PadStr`** (`{x, width[, align]}` / `%s, width[, align]%`, emitted at
  `compiler.rs:250-253`, executed at `executor.rs:339-352`): with `text` the
  string or the integer's decimal form, `n = width − cells(text)` (signed). If
  `n > 0` insert `n` spaces: Left → after, Right → before, Center → `n/2`
  before and `n − n/2` after; otherwise return the text unchanged (Emuera
  `FormatPercent`). **Default alignment when none is written is Right**
  (Emuera `StrForm.cs:128`); the compiler's `unwrap_or_default()` becomes
  `unwrap_or(Alignment::Right)`. `CENTER` is an erars extension and is kept.
- **String functions.** `STRLEN`/`STRLENS`/`STRLENFORM` (all
  `BuiltinMethod::StrLenS`) return `cells(s)`. `SUBSTRING(s, start, len)`
  ports `LangManager.GetSubStringLang` with `char_cells` in place of the byte
  count: skip characters until the running cell count ≥ `start`, then append
  characters until the running count ≥ `len` — whole characters only.
  `STRFIND` returns `cells(prefix before the match)` and maps its `start`
  argument with the same walk. Identical to today's byte results for
  encodable text; the `&#NNNN;` inflation disappears from all three.
  `STRLENU`/`SUBSTRINGU`/`STRFINDU` (char-based) unchanged.
- **Colours.** `EraConfig` gains `fore_color`, `bg_color`, `focus_color`
  parsed from `文字色`, `背景色`, `選択中文字色` (`r,g,b`), defaults
  (192,192,192), (0,0,0), (255,255,0). `VirtualConsole` keeps
  `default_color` and initialises `style.color`, `bg_color`, `hl_color` from
  the config; `reset_color()` restores `default_color`. Used by PRINTD
  (`executor.rs:195-200`, replacing `TODO: respect config`), `RESETCOLOR`
  (`executor.rs:2181`) and `GETDEFCOLOR` (`executor.rs:1434`).
- **Config defaults:** `printc_width` 30 → **25**, `printc_count` 4 → **3**,
  `window_width` 800 → **760**, `window_height` 600 → **480** (Emuera
  WindowX/WindowY; the height includes the input strip), `font_family`
  "D2Coding" → **""** (empty = no configured family; the per-language chain
  of Component 3 applies). No existing `tests/run_tests` fixture uses PRINTC,
  width padding, GETDEFCOLOR or the train menu, so no snapshot changes.
- **Newlines.** `VirtualConsole::print` (hence `print_line`) splits at `\n`
  exactly like Emuera's `EmueraConsole.Print`: `for (i, seg) in
  s.split('\n').enumerate() { if i > 0 { self.push_line(); } if
  !seg.is_empty() { self.last_line.push_text(self.input_gen, seg.to_owned(),
  &self.style); } }` — each `\n` starts a new *logical* `ConsoleLine`, so
  LINECOUNT, CLEARLINE and ALIGNMENT see it. Paths Emuera does not split keep
  the string whole and do not go through `print`: `printlc`/`printrc`,
  `print_plain` → `push_plain_text`, `draw_line`, `reuse_last_line`.
  `print_button`, `print_button_lc`, `print_button_rc` **remove** every `\n`
  first (`Process.ScriptProc.cs:118/135`). PRINTSINGLE: the executor ignores
  `PrintFlags::SINGLE` today, so it goes through `print` and splits (Emuera
  keeps `\n` inside the part) — accepted deviation. The stdio/JSON frontends
  therefore emit one line per `\n`.
- **DRAWLINE style.** `draw_line` stores the style with `font_style =
  FontStyle::NORMAL` (colour and family kept) — Emuera forces Regular for
  DRAWLINE/CUSTOMDRAWLINE/BAR.
- `pad` is dropped from `erars-ui` and `erars-vm`; `unicode-width` is dropped
  from `erars-vm` (unused) and `erars-renderer`, kept in `erars-ui` (rule steps
  1 and 3); `encoding_rs` becomes a workspace dependency used by `erars-ui`,
  `erars-compiler` and `erars-vm`.

## Component 3 — font chain (`erars-renderer::font`)

```rust
pub struct FontConfig<'a> {
    pub family: &'a str,            // emuera.config フォント名 (may be empty)
    pub game_dir: &'a Path,         // <game>/font/*.ttf|ttc|otf|otc is loaded (Emuera.EM)
    pub extra_dir: Option<PathBuf>, // ERARS_FONT_DIR
    pub lang: Language,
}
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct StyleKey { pub family: SmolStr, pub bold: bool, pub italic: bool } // colour/underline/strike are not shaping inputs
impl StyleKey { pub fn from(style: &TextStyle) -> Self; }
pub struct FontChain {
    font_system: cosmic_text::FontSystem,      // fontdb + Font loading only
    chain: Vec<fontdb::ID>,                    // ordered candidates, primary first
    primary: fontdb::ID,
    cache: HashMap<(char, StyleKey), (fontdb::ID, RasterFlags)>,
}
impl FontChain {
    pub fn new(cfg: &FontConfig) -> Self;                      // system fonts + game dir + extra dir + bundled
    pub fn from_files(files: &[PathBuf], lang: Language) -> Self; // tests: explicit files only, no system fonts, no locale
    pub fn primary(&self) -> fontdb::ID;
    /// First chain font whose cmap covers `c`. With `bold`/`italic` set, a real
    /// bold/italic face of that family is preferred; if none exists the regular
    /// face is returned with BOLD_SYNTH / ITALIC_SYNTH set. If no chain font
    /// covers `c`, the first face in the whole database that does; else the
    /// primary (renders .notdef).
    pub fn resolve(&mut self, c: char, style: &StyleKey) -> (fontdb::ID, RasterFlags);
    pub fn font(&mut self, id: fontdb::ID) -> Arc<cosmic_text::Font>;   // owned Arc (borrow-friendly)
}
```

- Chain order: the part's `SETFONT` family (if non-empty and present) →
  configured family → faces from `<game>/font/` → faces from `ERARS_FONT_DIR`
  → per-language fixed-pitch CJK candidates (JP: MS Gothic, Sarasa Mono J,
  Noto Sans Mono CJK JP; KR: D2Coding, NanumGothicCoding, GulimChe, Sarasa Mono
  K, Noto Sans Mono CJK KR; ZH: NSimSun, Sarasa Mono SC/TC, Noto Sans Mono CJK
  SC/TC) → bundled Noto Sans Mono → every other face in load order. The
  *primary* (metrics source) is the first present entry of the configured →
  game dir → extra dir → language list → bundled sequence.
- Family matching is a manual case-insensitive scan over fontdb's
  `FaceInfo.families` (name ID 16 in every language, or ID 1 when the font
  has no ID 16 — which is why both `MS Gothic` and `ＭＳ ゴシック` match for
  `msgothic.ttc`); `Database::query` is not used (exact, case-sensitive).
- Coverage: `font.rustybuzz().glyph_index(c).map_or(false, |g| g.0 != 0)`
  (`rustybuzz::Face` derefs to `ttf_parser::Face`; the `!= 0` guard matters
  because format-4 delta segments can return `GlyphId(0)`, e.g. U+FFFF).
  `Font::unicode_codepoints()` is not used. Database-wide fallback scans use
  `db.with_face_data(id, |data, index| ttf_parser::Face::parse(data,
  index).ok()?.glyph_index(c))` so faces are only materialised as `Font` once
  chosen. Results are cached per `(char, StyleKey)`.
- A `SETFONT` family that does not exist logs once and uses the default chain
  (Emuera silently substitutes Microsoft Sans Serif).
- cosmic-text's `Buffer`, `Attrs` and `SwashCache` are no longer used and the
  `monospace_fallback` / `shape-run-cache` features are dropped; the crate
  stays for `FontSystem`/`fontdb` loading and `Font::{rustybuzz, as_swash}`.

## Component 4 — shaping and cell geometry (`erars-renderer::text`)

```rust
pub struct CellMetrics {
    pub scale: f32,     // winit scale factor
    pub font_px: u32,   // round(config.font_size * scale).max(8) — integer, like Emuera's GraphicsUnit.Pixel font
    pub half_w: u32,    // round(primary space advance * font_px / upem); for a 0.5 em primary = ceil(font_px/2)
    pub line_h: u32,    // round(config.line_height * scale).max(font_px)
    pub baseline: u32,  // round(asc * font_px / upem), asc = ttf_parser Face::ascender() (hhea, or OS/2 typo when USE_TYPO_METRICS)
    pub shift: u32,     // max(2, font_px / 6) — integer division, Emuera's DrawingParam_ShapePositionShift
}
impl CellMetrics { pub fn from_primary(font: &cosmic_text::Font, font_size: u32, line_height: u32, scale: f32) -> Self; }

bitflags! { pub struct RasterFlags: u8 { const BOLD_SYNTH = 1; const ITALIC_SYNTH = 2; } }
pub struct ShapedGlyph { pub font: fontdb::ID, pub glyph: u16, pub dx: i32, pub dy: i32, pub size_px: f32, pub flags: RasterFlags }
pub struct Cluster { pub cells: u8, pub text: SmolStr, pub glyphs: Vec<ShapedGlyph> }

pub struct Shaper {
    chain: FontChain, widths: WidthTable, m: CellMetrics, layout_gen: u32,
    cache: HashMap<StyleKey, HashMap<String, (u32 /* last used layout_gen */, Arc<[Cluster]>)>>,
}
impl Shaper {
    pub fn new(chain: FontChain, widths: WidthTable, m: CellMetrics) -> Self;
    pub fn metrics(&self) -> &CellMetrics;
    pub fn chain(&mut self) -> &mut FontChain;                  // for GlyphRaster::get(font)
    pub fn shape(&mut self, text: &str, style: &TextStyle) -> Arc<[Cluster]>; // cached per (StyleKey, text)
    pub fn set_metrics(&mut self, m: CellMetrics);              // scale/font change: clears the cache
    pub fn sweep(&mut self);                                    // called at the end of layout(): drop entries unused by this layout, bump layout_gen
}
```

- Tabs are expanded before shaping: `\t` → spaces up to the next multiple of
  8 cells (uEmuera / GRAPHICS-mode behaviour; TEXTRENDERER has no defined
  result because MS Gothic lacks a U+0009 glyph). `str_cells` counts them the
  same way.
- `text` is split into maximal spans whose characters resolve to the same
  `(font, flags)`; each span is shaped once with rustybuzz in font units
  (`UnicodeBuffer::push_str`, default cluster level `MonotoneGraphemes`, so
  combining marks join their base) with `liga`, `clig`, `calt` and `kern`
  disabled via `Feature::new(Tag::from_bytes(b"…"), 0, ..)` — one character
  stays one glyph and advances are unkerned, as GDI does. Glyphs are grouped
  by rustybuzz cluster (byte offsets into the span); a cluster's `cells` is
  the sum of `char_cells` over its source characters; a 0-cell cluster
  (combining-only) is merged into the previous cluster's box.
- Box width `w = cells · half_w`. The cluster's natural advance `a` (linear,
  unhinted, `Σ x_advance · font_px / upem`): because `font_px` is an integer
  and `half_w = round(a_space) ≥ a_space`, a 1:2 primary (MS Gothic,
  D2Coding, Sarasa, GulimChe) always has `a ≤ w` — `a == w` at even
  `font_px`, `a = w − 0.5` at odd — and `size_px == font_px`. If `a ≤ w` the
  glyphs are centred: `dx = floor((w − a) / 2)` (0 for the primary; identical
  to GDI at even sizes). If `a > w` (a 0.6 em Latin fallback, a full-width
  symbol in a 1-cell box) the span's offsets and advances are scaled by
  `size_px = font_px · w / a` instead of `font_px` (no second shaping call —
  rustybuzz output is size-independent), then centred. Only there can
  `size_px` be non-integer.
- Vertical: `dy = baseline − round(y_offset · size_px / upem)`; every font
  shares the primary's baseline, so mixed-font rows stay level. The em box
  top sits at the row top like GDI; `line_h − font_px` slack is below. MS
  Gothic 220/256 → baseline 15 @18 px; the bundled Noto Sans Mono 1069/1000 →
  19 (its descenders overlap the next row — Emuera behaves the same with a
  tall font). No clamping to `line_h`.
- `BOLD_SYNTH`/`ITALIC_SYNTH` come from `FontChain::resolve` (no real
  bold/italic face) and ride on `ShapedGlyph.flags`.
- Cache: keyed by `(StyleKey, text)` for the current `CellMetrics`;
  `set_metrics` clears it; `layout()` ends with `sweep()`, so the cache holds
  exactly the strings of the lines last laid out (bounded by `max_log`).
  Colour is not a shaping input. The nested map lets `shape` look up by
  `&str` without allocating a key per part.

## Component 5 — layout (`erars-renderer::layout`, replaces `grid.rs`)

```rust
pub struct Geometry { pub content_w: u32, pub drawable_w: u32, pub m: CellMetrics } // drawable_w = content_w − m.shift
pub struct Layout { pub rows: Vec<Row>, pub buttons: Vec<ButtonRegion> }
pub struct Row {
    pub line: usize, pub logical_start: bool,
    pub x0: i32, pub width: u32,               // alignment offset (PointX space); sum of cluster boxes
    pub clusters: Vec<PlacedCluster>, pub rects: Vec<Rect>,
}
pub struct PlacedCluster {
    pub x: i32,                                // row-relative, before x0
    pub cells: u8,
    pub text: SmolStr,                         // the cluster's source chars
    pub color: [u8; 3], pub style: FontStyle,
    pub button: Option<usize>,                 // index into Layout.buttons
    pub glyphs: Arc<[ShapedGlyph]>,            // dx/dy relative to (x, row_y)
}
pub enum RectKind { Underline, Strike }
pub struct Rect { pub kind: RectKind, pub x: i32, pub dy: i32, pub h: u32, pub w: u32, pub color: [u8; 3], pub button: Option<usize> }
pub struct ButtonRegion { pub row: usize, pub x: i32, pub w: u32, pub input_gen: u32, pub value: Value }
pub fn layout(lines: &[ConsoleLine], g: &Geometry, shaper: &mut Shaper) -> Layout;
```

- `content_w` = the **live window inner width** in physical px (initial size
  from `window_width`/`window_height`; resizing relayouts). `drawable_w =
  content_w − shift` (Emuera: `WindowX − max(2, FontSize/6)`; 760 − 3 = 757
  at the defaults, so a 760 px window reproduces Emuera exactly).
- Rows are built by walking a line's parts with a pixel cursor `x`. For each
  cluster (`w = cells·half_w`): if `x + w > drawable_w` and `x > 0`, the row
  is finished and the cluster starts a new row (character granularity,
  mid-word, no kinsoku — Emuera with ButtonWrap=false). Button parts are
  split across rows; each fragment becomes its own `ButtonRegion` with the
  same `input_gen`/`value`, and its clusters/rects carry `button = Some(i)`.
  Continuation rows have `logical_start = false`.
- A `\n` can reach the renderer only from the non-splitting console paths
  (PRINTC/PRINTLC, PRINTPLAIN, PRINTSINGLE, CUSTOMDRAWLINE, REUSELASTLINE); it
  finishes the current row and continues on a continuation row, occupies no
  cells and is never passed to the classifier.
- `Line` (DRAWLINE/CUSTOMDRAWLINE): let `unit = cells(s)·half_w`; if `unit ==
  0` the part is skipped with a warning (Emuera's `getStBar` would loop
  forever); otherwise the string is repeated `ceil(drawable_w / unit)` times
  and trailing characters are dropped while the width exceeds `drawable_w`
  (identical to Emuera's grow-then-trim). The result is laid out as ordinary
  text **after** the parts already on the line: with pending `abc` the rule
  starts right after it on the same row and the overflow spills to the next
  row (Emuera 1.824, ButtonWrap=false; the rule string is computed once
  against `drawable_w`, not shortened to the remaining space). With defaults
  this is 84 `-`; `abc` + DRAWLINE → row 1 = `abc` + 81 `-`, row 2 = 3 `-`.
  The rule is drawn with `FontStyle::NORMAL` (Component 2).
- Alignment per row, in pixels, after wrapping, Emuera 1.824's integer form:
  Left `x0 = 0`; Center `x0 = content_w/2 − width/2` (both integer divisions:
  a 45 px row in a 760 px window → 380 − 22 = **358**; 44 → 358; 46 → 357);
  Right `x0 = content_w − width` (715 for 45 px); clamped at 0. Emuera.EM
  aligns to `drawable_w` instead — we follow 1.824.
- **Draw offset.** Every glyph and rect is drawn at `shift + x0 + x` (GDI
  overhang padding: Emuera's ink starts at `PointX + shift`; `DrawableWidth =
  WindowX − shift` is exactly "text drawn from `shift` ends at `WindowX`").
  `Row.x0`/`PlacedCluster.x` and the layout snapshot stay in Emuera's `PointX`
  space; hit rects use the same offset.
- Styles: bold/italic ride on the glyph flags. Underline and strike come from
  the primary font's tables like GDI: underline `dy = baseline + round(−post.underlinePosition
  · font_px / upem)`, `h = max(1, round(post.underlineThickness · font_px /
  upem))`; strike `dy = baseline − round(OS/2.yStrikeoutPosition · font_px /
  upem)`, `h = max(1, round(yStrikeoutSize · font_px / upem))` (MS Gothic 18
  px: rows 16 and 10). Fallback when a table is absent: `dy = font_px`
  (uEmuera) and `font_px/2 − 1`, `h = 1`. One rect per styled run per row
  spanning its cluster boxes.
- Buttons: layout never looks at the mouse or the input generation. Hit rect
  = `[shift + x0 + x, row_y, w + 1, min(font_px + 1, line_h)]` (Emuera tests
  `PointX ≤ x ≤ PointX + Width` and `0 ≤ y − row_y ≤ FontSize`, both
  inclusive). Hover is applied at draw time (Component 6/7).
- **View state** (`app.rs`): `scroll_rows: usize` = whole rows hidden below
  the bottom of the row area (0 = stuck to the bottom). `strip_h = line_h`;
  `view_h = window_h − strip_h`; `visible = view_h / line_h`; `bottom_row =
  rows.len() − 1 − scroll_rows`; row `r` is drawn at screen y `view_h −
  (bottom_row − r + 1)·line_h` for `r ∈ (bottom_row − visible, bottom_row]`,
  so slack appears at the top (Emuera's bottom-anchored picture box; Emuera
  can show a partial top row — accepted deviation). The input strip shows `>
  {input}_` in the default colour. A new frame sets `scroll_rows = 0`; a
  resize or scale change relayouts and only clamps `scroll_rows` to
  `rows.len().saturating_sub(visible)` (never forces the bottom); wheel:
  `LineDelta` ±1 row per notch, `PixelDelta` accumulated and converted at
  `line_h` per row.
- The layout is recomputed only when the frame, `content_w` or `CellMetrics`
  (scale / font size / line height) change. Hover, the active generation and
  scrolling never trigger a relayout.

## Component 6 — rasterization and atlas (`erars-renderer::raster`, replaces `atlas.rs`)

```rust
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct RasterKey { pub font: fontdb::ID, pub glyph: u16, pub size_bits: u32 /* size_px.to_bits() */, pub flags: RasterFlags }
pub struct GlyphRaster { ctx: swash::scale::ScaleContext, pages: Vec<AtlasPage>, map: HashMap<RasterKey, Option<AtlasRegion>>, use_bitmap_strikes: bool }
pub struct AtlasRegion { pub page: usize, pub uv: [f32; 4], pub size: [u32; 2], pub left: i32, pub top: i32, pub color: bool }
impl GlyphRaster {
    pub fn new(device: &wgpu::Device, use_bitmap_strikes: bool) -> Self;
    pub fn get(&mut self, device: &wgpu::Device, queue: &wgpu::Queue, font: &cosmic_text::Font, key: RasterKey) -> Option<AtlasRegion>;
}
```

- **Two raster paths.** (1) *Embedded 1-bit strikes*: when `use_bitmap_strikes`,
  `flags` is empty and `size_px` is an integer, `font.rustybuzz().glyph_raster_image(GlyphId(glyph),
  size_px as u16)` is tried; the image is used only when `pixels_per_em ==
  size_px` (ttf-parser picks the *nearest* strike) and `format` is
  `BitmapMono` (byte-padded rows) or `BitmapMonoPacked` (bit-continuous). Set
  bits become 255 in an 8-bit mask; placement `left = image.x`, `top = image.y
  + image.height` (ttf-parser's `y` is the bitmap's *bottom* edge relative to
  the baseline; at 18 px `y = −3`, `height = 18`, top = 15 = the baseline).
  With MS Gothic the strike is used exactly when `font_px ∈ [10, 22]` (default
  18 px: every scale factor up to 1.2; at 1.25 → 23 px outlines). (2)
  *Everything else* goes through swash on `font.as_swash()`: scaler
  `ctx.builder(font_ref).size(size_px).hint(true).build()`; `Render::new(&[Source::ColorBitmap(StrikeWith::BestFit),
  Source::ColorOutline(0), Source::Outline]).format(Format::Alpha)`, plus
  `.embolden(size_px / 24.0)` for `BOLD_SYNTH` and
  `.transform(Some(Transform::skew(Angle::from_degrees(12.0),
  Angle::from_degrees(0.0))))` for `ITALIC_SYNTH` (swash applies both only to
  `Source::Outline`, which is why synthetic styles bypass the strike path);
  integer offsets only. swash's `Source::Bitmap` is never used (see
  Findings). `Image.placement` is baseline-relative with y up: the glyph's
  top-left pixel is `(x + dx + placement.left, row_y + dy − placement.top)`.
- Emuera's GDI bold widens glyphs by ~1 px and breaks its own grid; we keep
  the grid — accepted deviation.
- Atlas: pages of 2048² `Rgba8Unorm` (mask glyphs white + alpha, colour
  glyphs RGBA); when a page is full a new page is added; instances are
  bucketed per page (one bind group and one draw per page). Sampler
  `Nearest` (glyphs are placed on integer pixels; preserves the bitmap look).
- `Rect` uses shader mode 0 (already in `shader.wgsl`).
- `draw.rs`: `build_instances(layout, view, hover: Option<usize>, hl: [u8;3],
  raster, device, queue, shaper) -> Vec<Vec<Instance>>` (per page) substitutes
  `hl` for the colour of every cluster/rect whose `button == hover`; nothing
  moves (Emuera `ConsoleStyledString.DrawTo(.., isSelecting, ..)` swaps the
  brush only). Quad origin `(shift + x0 + x + dx + left, row_y + dy − top)`.

## Component 7 — app / headless / CLI

- `app.rs` holds `Shaper`, the last `Layout`, `scroll_rows`, `hovered:
  Option<usize>` and the cursor; relayouts on frame / resize / scale change
  (calling `shaper.set_metrics` on scale change) and re-derives `hovered` from
  the stored cursor afterwards and on `CursorMoved` and when an `Input`
  request arrives (active generation change), requesting a redraw only when
  the hovered fragment changed; hit-tests `ButtonRegion`s (only those with
  `input_gen == active generation`); draws the input strip; unchanged proxy
  wake, TINPUT and key handling.
- `headless.rs`: `render_frame(shaper: &mut Shaper, frame: &ConsoleFrame,
  content_w: u32, height: u32, input: Option<&str>, hover: Option<usize>) ->
  Option<Rendered>` renders a `content_w × height` image with `scroll_rows =
  0`: the last `(height − line_h)/line_h` rows above a `line_h`-tall input
  strip that shows `> {input}_` when `input` is `Some`; honours `bg_color` /
  `hl_color`. `layout_snapshot(layout: &Layout, default_fg: [u8;3]) -> String`
  prints exactly:

  ```
  row <r> line <line>[+] x0=<x0> w=<width>            # '+' marks a continuation row (logical_start == false)
    <x>:<cells> "<text, Rust-escaped>" [c=RRGGBB] [s=<subset of BIUS>] [btn=<i>]   # c only when != default_fg, s only when != NORMAL
    rect <underline|strike> x=<x> dy=<dy> h=<h> w=<w> [btn=<i>]
  btn <i> row=<r> x=<x> w=<w> gen=<gen> value=<Value as Debug>
  ```

  No `fontdb::ID`, glyph id, `dx`, `dy` or `size_px` appears. `write_png`
  is a minimal in-crate encoder (IHDR/IDAT/IEND, `flate2` zlib + `crc32fast`,
  both already in the lock) replacing PPM for `--headless-shot`.
- `test_support::gpu_device() -> Option<(Device, Queue)>` prints `SKIP <test>:
  no wgpu adapter` on stderr and, when `ERARS_REQUIRE_GPU=1`, panics instead
  of skipping. CJK-font-dependent pixel tests are separate `#[test]`s suffixed
  `_cjk`, gated the same way by `ERARS_REQUIRE_CJK_FONT=1`.
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

1. **Width classifier** (`erars-ui`, no GPU): the per-language tables in
   Component 1 (JP, KR, ZH-Hans/Hant), `str_cells` of mixed strings,
   combining/ZWJ/VS16/regional-indicator cases, the JP best-fit overrides and
   EUDC.
2. **Console** (`erars-ui`): PRINTC/PRINTLC field widths 25/26 with half, full
   and mixed strings; overlong strings unpadded; button variants; the PRINTC
   button boundary; `print_line("a\nb")` yields two `ConsoleLine`s
   (`line_count()` = 2); `clear_line(1)` afterwards removes only `b`;
   `print("a\nb")`, `set_align(Right)`, `print_line("c")` leaves `a` Left and
   `bc` Right; `print_button("x\ny", …)` stores `xy`; `printrc("a\nb")` keeps
   one part containing `\n`; `draw_line` stores `NORMAL`; `reset_color`
   restores the configured colour; `ConsoleSerde` JSON snapshots.
3. **VM** (`tests/run_tests`): the harness loads `<fixture dir>/emuera.config`
   when present (fallback: repo root), so `basic/` stays KOREAN and a new `jp/`
   directory runs JAPANESE fixtures. New `.out` fixtures: `STRLEN`, `SUBSTRING`,
   `STRFIND` with box drawing, Hangul, `〜`, emoji in both languages;
   `PadStr` (`%"★●①", 6, LEFT%`, `{12, 5}` right by default, `%"あ", 1%`
   overlong, `{1, -3}` negative width); `PRINTC`/`PRINTLC`; `DRAWLINE`;
   `ALIGNMENT`; `PRINTBUTTON` with `\n`; `PRINTL "a\nb"` + `CLEARLINE 1` +
   `LINECOUNT`; `GETDEFCOLOR`. `printc_count` is covered by an erars-vm unit
   test driving the train menu with a scripted `SystemFunctions`.
4. **Layout goldens** (`erars-renderer`, no GPU): `k9::snapshot!` of
   `layout_snapshot` using only the bundled Noto Sans Mono through
   `FontChain::from_files` and a pinned `CellMetrics { font_px 18, half_w 9,
   line_h 19, baseline 15, shift 3 }` (row geometry is font-independent by
   construction; they never call `load_system_fonts()` or read the locale):
   mid-word wrap, full-width cluster that does not fit moves whole, residual
   `\n` via `print_plain`, tab expansion, Center/Right offsets for widths
   44/45/46, DRAWLINE `-`×84 and trimming of a 3-character rule, `abc` +
   DRAWLINE spill, PRINTC columns, button fragments across a wrap,
   underline/strike rects. The `a ≤ w` / `a > w` placement rule is unit-tested
   in `text.rs` with the bundled font at `half_w = 11` (a = 10.8 → dx 0, size
   18) and `half_w = 9` (a > w → size 15, dx 0).
5. **Pixel tests** (GPU, `gpu_lock`, `gpu_device`): with
   `FontChain::from_files([bundled])` — a box-drawing frame `┏━━┓ / ┃  ┃ /
   ┗━━┛` over an ASCII ruler: ink of every column lands inside `[shift +
   k·half_w, shift + (k+cells)·half_w)`; identical rows identical; a fallback
   glyph never exceeds its box; `build_instances(.., hover = Some(i), ..)`
   recolours exactly the items of `buttons[i]` and `hover = None` matches the
   unhovered render byte-for-byte. `_cjk` variants add Sarasa/Noto CJK when
   present. With `ERARS_FONT_DIR` pointing at `msgothic.ttc` (opt-in, never in
   CI): the 32 JIS box characters are 2 cells and `═║` 1 cell; an 18 px render
   of `A`, `あ`, `漢`, `─` and `═` in white uses the bitmap strike — pixel
   values are only 0/255 in each glyph body, `あ` covers an 18×18 box, `A`/`═`
   a 9×18 box; a 23 px render uses outlines; a GPU-free companion asserts
   `glyph_raster_image(gid('あ'), 18)` → `pixels_per_em 18, 18×18,
   BitmapMonoPacked` and that the raster layer rejects the 22 px strike
   returned for 23 px.
6. **Integration** (`crates/erars-renderer/tests/games/tui/`): a small synthetic
   game (UTF-8 BOM ERB + CSV + `emuera.config` KOREAN and JAPANESE variants)
   printing a box map, a PRINTC table, centred title, DRAWLINE and buttons;
   the test compiles and runs it headlessly with a scripted `SystemFunctions`,
   builds the `ConsoleFrame`, and snapshots the layout; a GPU variant renders
   it to PNG for eyeballing.
7. **CI**: `.github/workflows/check.yml` installs `mesa-vulkan-drivers`
   (lavapipe) and runs `cargo test --all` with `ERARS_REQUIRE_GPU=1`, so the
   bundled-font pixel tests are enforced; if wgpu cannot enumerate lavapipe on
   the runner the job fails on the `SKIP` panic rather than passing silently.

## Out of scope / accepted deviations

- Images, shapes, `<font face>` beyond family switching, GDI+ GRAPHICS-mode
  1.04 quantisation, backlog `LogColor`, tooltips, IME composition display.
- Emuera lays out against the fixed config `WindowX` regardless of the actual
  window size and computes the DRAWLINE string once at start-up; we relayout
  to the live inner width, so on windows wider than `WindowX` rows wrap later,
  DRAWLINE is longer and centred/right rows move (operator decision).
- Partial top-row scrolling (we scroll whole rows).
- GDI bold widening (we keep the grid).
- At odd `FontSize` (11, 13, …, 21) Emuera's GDI layout uses MS Gothic's
  bitmap advances where full-width = `2·half − 1`; we keep the 1:2 grid, so
  rows differ from Emuera by up to one px per full-width glyph at those sizes.
  Pixel identity with GDI holds at even sizes 10–22.
- PRINTSINGLE splits at `\n` (Emuera keeps it inside the part).
- The width deviations listed in Component 1 (`−`, `¢ £ ¬`, cp932-less
  full-width symbols, combining marks, Hangul in Japanese games).
- Emoji ZWJ sequences occupy the sum of their parts' cells (one glyph centred
  in a wide box); Emuera has no defined behaviour here.
- erars form strings (`PRINTFORM`) do not decode `\n` escapes today
  (`parser/expr.rs:148-160`); Emuera's lexer does. Pre-existing, out of scope.

## Workspace changes

- Workspace `Cargo.toml`: `encoding_rs = "0.8"` as a workspace dependency.
- `erars-ui`: `+encoding_rs`, `+unicode-width 0.1` (direct), `−pad`; new
  `width.rs`; `ConsoleConfig`; `VirtualConsole::new(&ConsoleConfig)`;
  `\n` splitting; PRINTC helper; `default_color`/`reset_color`; `draw_line`
  style.
- `erars-compiler`: `Language::encoding()` (`+encoding_rs`), label fix,
  colour config keys, defaults 25/3/760/480/"", `PadStr` default alignment
  Right.
- `erars-vm`: `console_config()`, `VmContext::encoding` delegates, STRLEN /
  SUBSTRING / STRFIND / PadStr via cells, PRINTD / RESETCOLOR / GETDEFCOLOR via
  `default_color`; `−pad`, `−unicode-width`.
- `erars-loader`, `tests/run_tests.rs`: use `console_config`; run_tests loads
  per-directory `emuera.config`.
- `erars-renderer`: `+swash 0.1.18` (direct), `+smol_str 0.2`, `+bitflags 2`,
  `+flate2`, `+crc32fast`, `−unicode-width`; cosmic-text features
  `monospace_fallback`/`shape-run-cache` removed; `grid.rs` → `layout.rs`,
  `atlas.rs` → `raster.rs`; `font.rs`/`text.rs`/`draw.rs`/`headless.rs`/
  `app.rs`/`main.rs` reworked as above; `test_support::gpu_device`;
  `tests/games/tui/` fixture; `assets/` gains nothing proprietary;
  `.gitignore` gains `msgothic.ttc` and `.DS_Store`.
- `.github/workflows/check.yml`: lavapipe + `ERARS_REQUIRE_GPU=1`.
- `justfile`: `headless-shot` writes PNG; `test-align` runs the layout goldens
  and pixel tests.
