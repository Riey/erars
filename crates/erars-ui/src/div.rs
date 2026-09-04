//! Positioned `<div>` boxes: the one console part that is laid out at a
//! coordinate of its own instead of in the line flow.
//!
//! Emuera's `ConsoleDivPart` (`_Library/EvilMask/ConsoleDivPart.cs`) is an
//! ordinary `AConsoleDisplayPart` — it sits in the part list of the line it was
//! printed on — but three things make it unlike every other part:
//!
//! * it has **no width** (`Str = string.Empty`, `SetWidth` is empty, `:47`,
//!   `:172-174`), so it never advances the pen and the text after it prints as
//!   if the box were not there. That is why eramegaten_p_kr reserves room for
//!   an event picture with a run of blank `PRINTL`s and then lifts the picture
//!   into them with a negative `ypos`
//!   (`Data/ERB/関数/組み込み関数/メッセージ/PRINT_EVENT_PICTURE.ERB:12-70`);
//! * its content is a **list of display lines of its own**
//!   (`ConsoleDisplayLine[] children`, `:88`), wrapped at the box's inner width
//!   (`HtmlManager.cs:532-556`, `:617-620`) and drawn one `LineHeight` apart
//!   from the box's content origin (`:161-166`);
//! * it is drawn at a rect that is either relative to the print position or
//!   absolute in the console area (`:141-143`), under a clip of that rect
//!   (`:148`, `:159`), with the CSS box model painted around it (`:150`).
//!
//! [`ConsoleDiv`] is that part with every `MixedNum` already resolved to
//! pixels, exactly as the `ConsoleDivPart` constructor resolves them against
//! `Config.FontSize` at construction time (`:49-64`, `Utils.cs:19-22`).

use std::fmt::Write;

use serde::{Deserialize, Serialize};

use crate::{image::MixedNum, Color, ConsoleLine};

/// Index of one edge of a box, in Emuera's order
/// (`_Library/EvilMask/Shape.cs:14`) — the CSS shorthand order, which is also
/// what `Utils.ParseParam4MixedNum` fills (`Utils.cs:58-88`).
pub mod edge {
    pub const TOP: usize = 0;
    pub const RIGHT: usize = 1;
    pub const BOTTOM: usize = 2;
    pub const LEFT: usize = 3;
}

/// Where a positioned box measures its origin from — `display`
/// (`GameView/HtmlManager.cs:1155-1160`) as `ConsoleDivPart.IsRelative`
/// (`_Library/EvilMask/ConsoleDivPart.cs:141-143`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum DivAnchor {
    /// `display` absent or `display='relative'`, the Emuera default
    /// (`HtmlManager.cs:1077`): the box's origin is the print position — the
    /// part's own x on its line, and the top of that line
    /// (`ConsoleDivPart.cs:142`).
    #[default]
    Relative,
    /// `display='absolute-lefttop'`: the console area's top-left corner, `y`
    /// counting down.
    LeftTop,
    /// `display='absolute-leftbottom'`: the console area's bottom-left corner,
    /// so a negative `y` lifts the box into the visible area. This is the
    /// fork's plain `display='absolute'`, whose y is measured from
    /// `MainPicBox.Height` (`ConsoleDivPart.cs:143`).
    LeftBottom,
}

/// One `<div>`'s CSS box model, resolved to pixels — Emuera's
/// `StyledBoxModel` (`_Library/EvilMask/Utils.cs:51-56`) after
/// `MixedNum4ToInt4` (`ConsoleDivPart.cs:30-41`). Every array is in
/// [`edge`] order.
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct DivBox {
    pub margin: [i32; 4],
    pub border: [i32; 4],
    pub padding: [i32; 4],
    /// Per-edge border colour. `None` is Emuera's "no `bcolor`", which
    /// `BoxBorder.DrawBorder` paints in `Config.ForeColor`
    /// (`_Library/EvilMask/Shape.cs:63`).
    pub border_color: [Option<Color>; 4],
    /// `color` / `background_color`. `None` is `Color.Transparent`, the
    /// constructor's value for a missing colour (`ConsoleDivPart.cs:18`).
    pub background: Option<Color>,
}

impl DivBox {
    /// Whether anything would be painted for the box itself.
    pub fn is_painted(&self) -> bool {
        self.background.is_some() || self.border.iter().any(|&w| w > 0)
    }

    /// Left and top offset from the box rect to its content origin: the
    /// margin, border and padding of those two edges
    /// (`ConsoleDivPart.cs:145-157`, applied in that order).
    pub fn content_offset(&self) -> (i32, i32) {
        (
            self.margin[edge::LEFT] + self.border[edge::LEFT] + self.padding[edge::LEFT],
            self.margin[edge::TOP] + self.border[edge::TOP] + self.padding[edge::TOP],
        )
    }

    /// Width the box model takes away from the content area — the same sum on
    /// both horizontal edges (`HtmlManager.cs:537-555`).
    pub fn edges_w(&self) -> i32 {
        self.margin[edge::LEFT]
            + self.margin[edge::RIGHT]
            + self.border[edge::LEFT]
            + self.border[edge::RIGHT]
            + self.padding[edge::LEFT]
            + self.padding[edge::RIGHT]
    }

    /// Height the box model takes away from the content area.
    pub fn edges_h(&self) -> i32 {
        self.margin[edge::TOP]
            + self.margin[edge::BOTTOM]
            + self.border[edge::TOP]
            + self.border[edge::BOTTOM]
            + self.padding[edge::TOP]
            + self.padding[edge::BOTTOM]
    }
}

/// A positioned `<div>`: its geometry in pixels and the console lines inside
/// it. `ConsoleDivPart` (`_Library/EvilMask/ConsoleDivPart.cs:14-64`).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ConsoleDiv {
    pub anchor: DivAnchor,
    /// `xOffset` — `xpos` in pixels (`:49`).
    pub x: i32,
    /// `PointY` — `ypos` in pixels, negative upwards (`:55`).
    pub y: i32,
    /// `width` in pixels. `None` is the attribute being absent, which this
    /// fork rejects outright (`HtmlManager.cs:1166-1167`) and a newer
    /// EvilMask build treats as "as wide as the content": the box is not
    /// clipped horizontally and its decoration wraps the content. 186 of
    /// eramegaten_p_kr's 369 `<div>`s have no `width`.
    pub width: Option<u32>,
    /// `Height` in pixels, `None` as for [`Self::width`].
    pub height: Option<u32>,
    pub style: DivBox,
    /// `children`: the lines inside the box, in print order (`:88`).
    pub lines: Vec<ConsoleLine>,
    /// `altHeadTag` (`:19-48`): the opening tag rebuilt from the resolved
    /// attributes, which is what `HTML_GETPRINTEDSTR` prints for the box
    /// (`:189-200`).
    pub alt_head: String,
}

/// A `<div>`'s attributes as written, before the font size resolves them —
/// Emuera's `HtmlDivTag` (`GameView/HtmlManager.cs:1170`), the bundle the
/// parser hands to the `ConsoleDivPart` constructor. Each `None` is the
/// attribute being absent, which is not the same as a zero: an absent
/// `border` array leaves no border, while `border='0'` is one that was
/// written and rebuilt into the alt text.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct DivSpec {
    pub anchor: DivAnchor,
    pub x: Option<MixedNum>,
    pub y: Option<MixedNum>,
    pub width: Option<MixedNum>,
    pub height: Option<MixedNum>,
    /// `color` (this fork) / `background_color` (the build eramegaten_p_kr
    /// targets).
    pub background: Option<Color>,
    pub margin: Option<[MixedNum; 4]>,
    pub border: Option<[MixedNum; 4]>,
    pub padding: Option<[MixedNum; 4]>,
    /// `radius`: rounded corners. Parsed and rebuilt into the alt text
    /// exactly as Emuera does (`ConsoleDivPart.cs:37-38`) but not drawn —
    /// erars' box border is four rectangles, see §5 of
    /// `docs/research/2026-09-03-emuera-command-gap.md`. No eramegaten_p_kr
    /// `<div>` sets it.
    pub radius: Option<[MixedNum; 4]>,
    /// `bcolor` / `border_color`, per edge in [`edge`] order.
    pub border_color: Option<[Color; 4]>,
}

impl ConsoleDiv {
    /// The `ConsoleDivPart` constructor (`_Library/EvilMask/ConsoleDivPart.cs:16-64`):
    /// resolve every `MixedNum` against the font size and rebuild the opening
    /// tag from the resolved values, in Emuera's attribute order.
    pub fn new(spec: DivSpec, font_size: i32, lines: Vec<ConsoleLine>) -> Self {
        let px = |n: Option<MixedNum>| MixedNum::to_pixel(n, font_size, 0);
        let px4 = |a: Option<[MixedNum; 4]>| {
            // `MixedNum4ToInt4` leaves the array zeroed when the attribute was
            // never written (`:31-40`).
            a.map_or([0; 4], |a| a.map(|n| MixedNum::to_pixel(Some(n), font_size, 0)))
        };

        // `width.num = Math.Abs(width.num)` (`:20-21`), before the tag is
        // rebuilt, so a negative extent is a positive one everywhere.
        let size = |n: Option<MixedNum>| n.map(|n| MixedNum { num: n.num.abs(), ..n });
        let (width, height) = (size(spec.width), size(spec.height));

        let mut alt_head = String::from("<div");
        MixedNum::write_tag_arg(spec.x, "xpos", font_size, &mut alt_head);
        MixedNum::write_tag_arg(spec.y, "ypos", font_size, &mut alt_head);
        MixedNum::write_tag_arg(width, "width", font_size, &mut alt_head);
        write_color(&mut alt_head, "color", spec.background);
        MixedNum::write_tag_arg(height, "height", font_size, &mut alt_head);
        write_mixed4(&mut alt_head, "margin", spec.margin, font_size);
        write_mixed4(&mut alt_head, "padding", spec.padding, font_size);
        write_mixed4(&mut alt_head, "border", spec.border, font_size);
        write_mixed4(&mut alt_head, "radius", spec.radius, font_size);
        if let Some(colors) = spec.border_color {
            write_color4(&mut alt_head, "bcolor", colors);
        }
        alt_head.push('>');

        Self {
            anchor: spec.anchor,
            x: px(spec.x),
            y: px(spec.y),
            width: width.map(|w| px(Some(w)).max(0) as u32),
            height: height.map(|h| px(Some(h)).max(0) as u32),
            style: DivBox {
                margin: px4(spec.margin),
                border: px4(spec.border),
                padding: px4(spec.padding),
                border_color: spec.border_color.map_or([None; 4], |c| c.map(Some)),
                background: spec.background,
            },
            lines,
            alt_head,
        }
    }

    /// Width the child lines are laid out and aligned in, when the box gives
    /// one: `state.SubDivisionWidth` (`HtmlManager.cs:532-556`).
    pub fn inner_width(&self) -> Option<u32> {
        let width = self.width? as i32 - self.style.edges_w();
        Some(width.max(0) as u32)
    }

    /// Height available to the child lines, when the box gives one.
    pub fn inner_height(&self) -> Option<u32> {
        let height = self.height? as i32 - self.style.edges_h();
        Some(height.max(0) as u32)
    }

    /// `BuildString` (`ConsoleDivPart.cs:189-200`): the opening tag, every
    /// child line, and `</div>`. Emuera joins the children with `\r\n`
    /// because a display line is a screen row; erars keeps one logical line
    /// per [`ConsoleLine`], so the separator is the `<br>` that
    /// [`crate::ConsoleLine`] would have printed.
    pub fn alt_text(&self) -> String {
        let mut out = self.alt_head.clone();
        for (i, line) in self.lines.iter().enumerate() {
            if i > 0 {
                out.push_str("<br>");
            }
            out.push_str(&line.to_string());
        }
        out.push_str("</div>");
        out
    }
}

/// `Utils.AddColorParam` (`Utils.cs:177-183`): a transparent — here absent —
/// colour writes nothing. The `X6` hex is `HtmlManager.GetColorToString`
/// (`:772-779`).
fn write_color(out: &mut String, name: &str, color: Option<Color>) {
    if let Some(Color([r, g, b])) = color {
        let _ = write!(out, " {name}='#{:02X}{:02X}{:02X}'", r, g, b);
    }
}

/// `Utils.AddColorParam4` (`Utils.cs:184-205`): the CSS shorthand collapsed
/// back down, longest form last.
fn write_color4(out: &mut String, name: &str, colors: [Color; 4]) {
    let hex = |Color([r, g, b]): Color| format!("#{r:02X}{g:02X}{b:02X}");
    let [top, right, bottom, left] = colors;
    let _ = write!(out, " {name}='");
    if top == right && top == bottom && top == left {
        let _ = write!(out, "{}", hex(top));
    } else if top == bottom && right == left {
        let _ = write!(out, "{},{}", hex(top), hex(right));
    } else if right == left {
        let _ = write!(out, "{},{},{}", hex(top), hex(right), hex(bottom));
    } else {
        let _ = write!(out, "{},{},{},{}", hex(top), hex(right), hex(bottom), hex(left));
    }
    out.push('\'');
}

/// `Utils.AddTagMixedParam` (`Utils.cs:148-176`), the same collapse for the
/// box-model shorthands.
///
/// DELIBERATE: Emuera collapses by *reference* identity, which reproduces the
/// token count the source wrote because `ParseParam4MixedNum` shares one
/// object between the edges a short form covers (`Utils.cs:58-88`). erars
/// keeps values, not objects, so it collapses by value: a written-out
/// `padding='2,3,2,3'` rebuilds as `padding='2,3'`. The two forms parse to the
/// same box, and this string is only ever read back as the alt text of a box
/// (`ConsoleDivPart.cs:189-200`), never as layout input.
fn write_mixed4(out: &mut String, name: &str, nums: Option<[MixedNum; 4]>, font_size: i32) {
    let Some([top, right, bottom, left]) = nums else {
        return;
    };
    let one = |n: MixedNum, out: &mut String| n.write_value(font_size, out);
    let _ = write!(out, " {name}='");
    if top == right && top == bottom && top == left {
        one(top, out);
    } else if top == bottom && right == left {
        one(top, out);
        out.push(',');
        one(right, out);
    } else if right == left {
        one(top, out);
        out.push(',');
        one(right, out);
        out.push(',');
        one(bottom, out);
    } else {
        for (i, n) in [top, right, bottom, left].into_iter().enumerate() {
            if i > 0 {
                out.push(',');
            }
            one(n, out);
        }
    }
    out.push('\'');
}

#[cfg(test)]
mod tests {
    use super::*;

    fn box_model() -> DivBox {
        DivBox {
            margin: [1, 2, 3, 4],
            border: [5, 6, 7, 8],
            padding: [9, 10, 11, 12],
            ..DivBox::default()
        }
    }

    #[test]
    fn the_content_origin_is_the_top_and_left_edges_summed() {
        // `ConsoleDivPart.cs:145-157` insets the rect by margin, then border,
        // then padding, so the content origin is their sum on each edge.
        assert_eq!(box_model().content_offset(), (4 + 8 + 12, 1 + 5 + 9));
    }

    #[test]
    fn the_box_model_takes_both_edges_from_the_content_area() {
        // `HtmlManager.cs:537-555` subtracts left *and* right of all three.
        assert_eq!(box_model().edges_w(), 4 + 2 + 8 + 6 + 12 + 10);
        assert_eq!(box_model().edges_h(), 1 + 3 + 5 + 7 + 9 + 11);
    }

    #[test]
    fn a_box_model_wider_than_the_box_clamps_the_inner_width_at_zero() {
        let div = ConsoleDiv {
            anchor: DivAnchor::Relative,
            x: 0,
            y: 0,
            width: Some(10),
            height: None,
            style: box_model(),
            lines: Vec::new(),
            alt_head: String::new(),
        };
        assert_eq!(div.inner_width(), Some(0));
        assert_eq!(div.inner_height(), None, "no height attribute, no inner height");
    }

    #[test]
    fn an_unpainted_box_is_the_default_one() {
        assert!(!DivBox::default().is_painted());
        assert!(DivBox {
            background: Some(Color([1, 2, 3])),
            ..DivBox::default()
        }
        .is_painted());
        assert!(DivBox {
            border: [0, 0, 0, 2],
            ..DivBox::default()
        }
        .is_painted());
    }
}
