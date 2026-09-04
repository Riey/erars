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
use erars_ui::width::{WidthTable, TAB_CELLS};
use erars_ui::TextStyle;
use rustybuzz::ttf_parser::{self, Tag};
use rustybuzz::{BufferClusterLevel, Direction, Feature, UnicodeBuffer};
use smol_str::SmolStr;

pub use crate::flags::RasterFlags;
use crate::font::{FontChain, StyleKey};

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

/// Upper bound on the rasterized font size (spec Component 4): far beyond any
/// real `emuera.config`, but small enough that one glyph still fits an atlas page.
pub const MAX_FONT_PX: u32 = 512;

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
        // A misconfigured `フォントサイズ` would size every glyph raster (and
        // the atlas allocation behind it); cap it before anything is rendered.
        let font_px = if font_px > MAX_FONT_PX {
            log::warn!("emuera.config: font size {font_px} px is absurd; clamped to {MAX_FONT_PX}");
            MAX_FONT_PX
        } else {
            font_px
        };
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
    /// The sum of `char_cells` over the grapheme, saturating at 255 — usually
    /// 1 or 2, but flag pairs and ZWJ emoji are wider. A 0-cell cluster only
    /// occurs at the start of a string (elsewhere it is merged into its
    /// predecessor).
    pub cells: u8,
    /// The cluster's source characters.
    pub text: SmolStr,
    /// Shared with every `PlacedCluster` laid out from this cluster, so a
    /// relayout of a cached string copies no glyphs.
    pub glyphs: Arc<[ShapedGlyph]>,
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

    /// How many `(style, text)` entries the shape cache holds.
    ///
    /// Introspection for the sweep rules, used by `erars-renderer`'s own
    /// tests — hence `pub` and not `#[cfg(test)]`, which would not exist in
    /// a dependent crate's test build.
    #[doc(hidden)]
    pub fn cache_len(&self) -> usize {
        self.cache.values().map(|inner| inner.len()).sum()
    }

    /// Is `(style, text)` in the shape cache right now? Test introspection for
    /// the sweep rules (a swept entry is re-shaped on its next use).
    #[doc(hidden)]
    pub fn is_cached(&self, text: &str, style: &TextStyle) -> bool {
        self.cache
            .get(&StyleKey::from(style))
            .is_some_and(|inner| inner.contains_key(text))
    }
}

/// OpenType features turned off so one character stays one glyph and advances
/// are unkerned, as GDI's TextRenderer draws (value 0 overrides rustybuzz's
/// default global `liga`/`clig`/`calt`/`kern`). GDI+'s `GraphicsPath.AddString`
/// does not kern or ligate either, so [`crate::text_image`] shapes with the
/// same set.
pub(crate) fn features() -> [Feature; 4] {
    [
        Feature::new(Tag::from_bytes(b"liga"), 0, ..),
        Feature::new(Tag::from_bytes(b"clig"), 0, ..),
        Feature::new(Tag::from_bytes(b"calt"), 0, ..),
        Feature::new(Tag::from_bytes(b"kern"), 0, ..),
    ]
}

/// `\t` → spaces up to the next multiple of [`TAB_CELLS`] cells, counted from
/// the start of `text` (uEmuera / GRAPHICS-mode behaviour; MS Gothic has no
/// U+0009 glyph).
fn expand_tabs<'a>(text: &'a str, widths: &WidthTable) -> Cow<'a, str> {
    if !text.contains('\t') {
        return Cow::Borrowed(text);
    }
    // Every `\t` becomes at most `TAB_CELLS` spaces (one of which replaces the
    // tab itself), so this never has to grow.
    let tabs = text.chars().filter(|&c| c == '\t').count();
    let mut out = String::with_capacity(text.len() + tabs * (TAB_CELLS - 1));
    let mut col = 0usize;
    for c in text.chars() {
        if c == '\t' {
            let n = TAB_CELLS - col % TAB_CELLS;
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

/// One shaped glyph in font units, before placement.
struct RawGlyph {
    gid: u16,
    x_adv: i32,
    x_off: i32,
    y_off: i32,
}

/// A maximal run of characters that resolved to the same `(font, flags)`.
pub(crate) struct Span {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) font: fontdb::ID,
    pub(crate) flags: RasterFlags,
}

/// Split `text` into maximal runs of characters `chain` resolves to the same
/// `(face, synthetic flags)`. Shared with [`crate::text_image`], which shapes
/// the same way but with the font's own advances instead of the cell grid.
pub(crate) fn resolve_spans(chain: &mut FontChain, text: &str, key: &StyleKey) -> Vec<Span> {
    let mut spans: Vec<Span> = Vec::new();
    for (i, c) in text.char_indices() {
        let (font, flags) = chain.resolve(c, key);
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
    spans
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
        (
            m.font_px as f32,
            ((w as f32 - a) / 2.0).floor().max(0.0) as i32,
        )
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
        let spans = resolve_spans(&mut self.chain, &expanded, key);

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

            // The script is deliberately not set: `rustybuzz::shape` runs
            // `guess_segment_properties`, which infers it from the text, while
            // the direction stays the LTR set explicitly below — every run is
            // drawn left to right on the cell grid, so shaping never reorders
            // across it (spec Component 4).
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
                    debug_assert!(
                        s.is_char_boundary(cstart) && s.is_char_boundary(cend),
                        "cluster {cstart}..{cend} is not on a char boundary of {s:?}"
                    );
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
                        // inside the previous cluster's box. `place` starts a
                        // fresh pen, so the merged glyphs' offsets are relative
                        // to that box, not to the base glyph — this is box
                        // packing, not OpenType mark placement.
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
                glyphs: Arc::from(glyphs),
            })
            .collect()
    }
}

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
        // An absurd font size is capped before anything is rasterized.
        assert_eq!(CellMetrics::from_primary(&font, 100_000, 19, 4.0).font_px, MAX_FONT_PX);
    }

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
        let Some(ms) = msgothic("msgothic_metrics_and_grid_are_exact") else {
            return;
        };
        let mut ch = FontChain::from_files(&[ms, bundled()], Language::Japanese);
        let primary = ch.primary();
        assert_eq!(ch.find_family("MS Gothic"), Some(primary));
        let font = ch.font(primary);
        // upem 256, space advance 128, hhea ascender 220 -> 9 px cells, baseline 15.
        let m18 = CellMetrics::from_primary(&font, 18, 19, 1.0);
        assert_eq!(
            m18,
            CellMetrics {
                scale: 1.0,
                font_px: 18,
                half_w: 9,
                line_h: 19,
                baseline: 15,
                shift: 3
            }
        );
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
        let Some(ms) = msgothic("spans_split_at_font_changes") else {
            return;
        };
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
}
