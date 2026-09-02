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
        let ul = face.underline_metrics().map(|l| (l.position, l.thickness));
        let st = face.strikeout_metrics().map(|l| (l.position, l.thickness));
        Self::compute(m, upem, ul, st)
    }

    /// `ul` / `st` are `(position, thickness)` in font units, or `None` when
    /// the table is absent. Fallbacks: underline at `font_px` (uEmuera),
    /// strike at `font_px / 2 − 1`, both 1 px.
    fn compute(m: CellMetrics, upem: f32, ul: Option<(i16, i16)>, st: Option<(i16, i16)>) -> Self {
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
