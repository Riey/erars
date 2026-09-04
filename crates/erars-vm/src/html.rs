//! `HTML_PRINT` — Emuera `GameView/HtmlManager.cs`.
//!
//! Emuera does **not** build a DOM. `html2DisplayLine` (`:452-632`) walks the
//! string once with a `StringStream`: the text between two tags is emitted with
//! whatever style the running `HtmlAnalzeState` currently holds, an open tag
//! mutates that state, and a close tag pops it. That is why the markup is
//! validated so strictly — `<b><b>` is `DuplicateTag` rather than a nested
//! element, a stray `</b>` is `UnexpectedCloseTag` rather than a discarded
//! token, and `<p>` off the line head is `TagIsNotBegin`. A tree parser cannot
//! express any of it: it normalises exactly the mistakes Emuera reports.
//!
//! Attribute syntax comes from `LexicalAnalyzer.Analyse(st, GreaterThan,
//! AllowAssignment | AllowSingleQuotationStr)` (`:931`), which yields
//! `identifier = <string literal>` triples. The value **must** be quoted:
//! `color=red` produces an `IdentifierWord` where a `LiteralStringWord` is
//! required and falls through to `HtmlTagError` (`:1385-1386`, `:1440`).
//!
//! DELIBERATE DEVIATIONS:
//!
//! * `<div>` is laid out as a positioned box, but it is *accepted without
//!   `width`/`height`, nested, inside a `<button>`, and with unknown
//!   attribute names*, where Emuera raises `NotSetAttribute`
//!   (`:1166-1169`), `NestedTag` (`:1070-1071`), `TagIsNotClosed`
//!   (`:531-532`) and `CanNotInterpretAttributeName` (`:1163-1164`).
//!   eramegaten_p_kr does all four, and reads `display='absolute-lefttop'` /
//!   `'absolute-leftbottom'` and the attribute spellings
//!   `background_color` / `border_width` / `border_color`, none of which
//!   exist in this fork — it targets a newer EvilMask build. The `<div>` arm
//!   documents each relaxation with its call site; `radius` and `depth` are
//!   parsed but not honoured.
//! * `<shape>` is validated in full and renders nothing. It is an EvilMask
//!   private-build extension built on `StyledBoxModel` margins (`:1175-1257`);
//!   there is no text approximation of an absolutely positioned box.
//! * `<font bcolor>` is validated and dropped — `TextStyle` has no button
//!   focus colour.
//! * `<nobr>` and a button's `pos` attribute are validated and dropped:
//!   `VirtualConsole` never word-wraps, so disabling word-wrap and pinning a
//!   button to a pixel column are both already true.
//! * An `<img>` inside a `<button>` is drawn but is not part of the clickable
//!   region: Emuera puts the image part inside the `ConsoleButtonString`
//!   (`PrintStringBuffer.cs:189-279`), while `ConsoleLinePart::Button` carries
//!   text runs only. The text around it stays clickable with the same value,
//!   so a script sees the same `RESULT`. No site in either corpus puts an
//!   `<img>` inside a `<button>`.
//! * Named colours resolve through `css_color`, as `SETCOLORBYNAME` does, where
//!   Emuera uses .NET `Color.FromName`. The two tables agree on the 140
//!   HTML/X11 names and differ only on .NET's `SystemColors` entries.

use anyhow::{bail, Result};
use erars_ast::{Alignment, Value};
use erars_ui::div::{ConsoleDiv, DivAnchor, DivSpec};
use erars_ui::image::{InlineImage, MixedNum};
use erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, TextStyle, VirtualConsole};
use std::sync::Arc;

use crate::graphics::ImageResolver;

/// One `<font>` frame. An inner tag inherits every field the inner tag itself
/// leaves unset (`HtmlManager.cs:1421-1431`).
#[derive(Clone, Default)]
struct FontTag {
    color: Option<Color>,
    bcolor: Option<Color>,
    face: Option<String>,
}

/// `<button>` / `<nonbutton>` — `HtmlAnalzeStateButtonTag`.
#[derive(Default)]
struct ButtonTag {
    /// `true` for `<button>`, `false` for `<nonbutton>`; only the matching
    /// close tag may end it (`:889-900`).
    is_button_tag: bool,
    /// `value` is button-only and makes the run clickable (`:1327`).
    value: Option<String>,
    title: Option<String>,
    /// `pos` is parsed and range-checked but cannot be rendered.
    pos: Option<i32>,
}

/// One entry of the run `State` accumulates. Emuera's equivalent is the
/// `List<AConsoleDisplayPart>` that `cssToButton` builds (`:743-769`), where an
/// image part sits between two string parts.
enum PendingPart {
    Text(String, TextStyle),
    Image(Arc<InlineImage>),
    /// A finished `<div>`: `cssList.Add(new ConsoleDivPart(…))` at the box's
    /// close tag (`HtmlManager.cs:567`).
    Div(Arc<ConsoleDiv>),
}

/// One open `<div>`. Emuera recurses into `html2DisplayLine` with a fresh
/// part list and the *same* `HtmlAnalzeState` (`HtmlManager.cs:557-563`), so
/// the box's content inherits every style and button flag while its parts
/// accumulate separately.
struct DivFrame {
    spec: DivSpec,
    /// Collects the box's own lines, i.e. `HtmlDivTag.Lines`.
    console: VirtualConsole,
    /// The enclosing run, parked until the box closes.
    outer: Vec<PendingPart>,
}

/// Emuera `HtmlAnalzeState`, restricted to what erars can observe.
struct State<'a> {
    /// Resolves an `<img src>` against the sprite dictionary. Borrowed rather
    /// than owned so the graphics store is never cloned per tag.
    images: ImageResolver<'a>,
    font_style: FontStyle,
    fonts: Vec<FontTag>,
    /// The console style `HTML_PRINT` started from; `<font>`/`<b>` layer over
    /// it and every one of them must be closed before the string ends.
    base_style: FontStyle,
    base_color: Color,
    base_font: String,

    flag_p: bool,
    flag_p_closed: bool,
    alignment: Option<Alignment>,

    flag_nobr: bool,
    flag_nobr_closed: bool,

    button: Option<ButtonTag>,
    /// `<clearbutton>` neuters every button inside it (`:1317-1334`).
    flag_clear_button: bool,
    flag_clear_button_tooltip: bool,

    /// The `<div>` boxes currently open, innermost last. Emuera keeps a
    /// single `CurrentDivTag` because it forbids nesting; erars allows it
    /// (see the `<div>` arm), so this is a stack.
    divs: Vec<DivFrame>,
    /// A `<div>` tag was just read and its frame has to be pushed once the
    /// tag is done — `state.StartingSubDivision` (`HtmlManager.cs:1171`).
    opening_div: Option<DivSpec>,
    /// A `</div>` was just read (`:910-914`).
    closing_div: bool,

    line_head: bool,
    flag_br: bool,
    /// Set by a button tag *and* its close tag; forces a flush (`:1338`, `:893`).
    flag_button: bool,

    /// The run accumulated since the last flush, one entry per style change.
    pending: Vec<PendingPart>,
    /// The button tag that owns `pending` — `state.LastButtonTag` (`:577`).
    pending_button: Option<(bool, Option<String>)>,
}

impl<'a> State<'a> {
    fn new(tx: &VirtualConsole, images: ImageResolver<'a>) -> Self {
        Self {
            images,
            font_style: tx.style(),
            fonts: Vec::new(),
            base_style: tx.style(),
            base_color: Color::from(tx.color()),
            base_font: tx.font().to_owned(),
            flag_p: false,
            flag_p_closed: false,
            alignment: None,
            flag_nobr: false,
            flag_nobr_closed: false,
            button: None,
            flag_clear_button: false,
            flag_clear_button_tooltip: false,
            divs: Vec::new(),
            opening_div: None,
            closing_div: false,
            line_head: true,
            flag_br: false,
            flag_button: false,
            pending: Vec::new(),
            pending_button: None,
        }
    }

    /// `HtmlAnalzeState.GetSS()`: the innermost `<font>` frame over the console's
    /// own style.
    fn text_style(&self) -> TextStyle {
        let font = self.fonts.last();
        TextStyle {
            color: font.and_then(|f| f.color).unwrap_or(self.base_color),
            font_family: font
                .and_then(|f| f.face.as_deref())
                .unwrap_or(&self.base_font)
                .into(),
            font_style: self.font_style,
        }
    }

    fn push_text(&mut self, text: String) {
        if text.is_empty() {
            return;
        }
        let style = self.text_style();
        match self.pending.last_mut() {
            Some(PendingPart::Text(buf, last)) if *last == style => buf.push_str(&text),
            _ => self.pending.push(PendingPart::Text(text, style)),
        }
    }

    fn push_image(&mut self, image: InlineImage) {
        self.pending.push(PendingPart::Image(Arc::new(image)));
    }

    /// Where printing currently goes: the innermost open `<div>`'s own lines,
    /// or the console itself. Emuera's equivalent is which recursion level of
    /// `html2DisplayLine` is running (`HtmlManager.cs:557-563`).
    fn out<'t>(&'t mut self, tx: &'t mut VirtualConsole) -> &'t mut VirtualConsole {
        match self.divs.last_mut() {
            Some(frame) => &mut frame.console,
            None => tx,
        }
    }

    /// `cssToButton` (`:743-769`): the accumulated run becomes one button when
    /// the owning tag carried a `value`, and plain styled text otherwise.
    ///
    /// An image splits a button run into one button per maximal text group,
    /// each carrying the same value — see the `<img>`-in-`<button>` note in the
    /// module header. Order is preserved, which is what a script can observe.
    fn flush(&mut self, tx: &mut VirtualConsole) {
        if self.pending.is_empty() {
            return;
        }
        let parts = std::mem::take(&mut self.pending);

        // `Int64.TryParse` decides whether the button's value is an integer
        // (`:1323-1325`).
        let value = match self.pending_button.take() {
            Some((true, Some(value))) => Some(match value.parse::<i64>() {
                Ok(i) => Value::Int(i),
                Err(_) => Value::String(value),
            }),
            _ => None,
        };

        let tx = self.out(tx);
        let mut run: Vec<(String, TextStyle)> = Vec::new();
        for part in parts {
            match part {
                PendingPart::Text(text, style) => run.push((text, style)),
                PendingPart::Image(image) => {
                    flush_run(tx, &mut run, value.as_ref());
                    tx.print_image(image);
                }
                // A box splits the run exactly as an image does, and for the
                // same reason: `ConsoleLinePart::Button` holds text runs only.
                PendingPart::Div(div) => {
                    flush_run(tx, &mut run, value.as_ref());
                    tx.print_div(div);
                }
            }
        }
        flush_run(tx, &mut run, value.as_ref());
    }
}

fn flush_run(tx: &mut VirtualConsole, run: &mut Vec<(String, TextStyle)>, value: Option<&Value>) {
    if run.is_empty() {
        return;
    }
    let run = std::mem::take(run);
    match value {
        Some(value) => tx.print_styled_button(run, value.clone()),
        None => {
            for (text, style) in run {
                tx.set_color(style.color.0[0], style.color.0[1], style.color.0[2]);
                tx.set_font(style.font_family.to_string());
                tx.set_style(style.font_style);
                tx.print(text);
            }
        }
    }
}

/// `HTML_PRINT` — `EmueraConsole.PrintHtml` (`EmueraConsole.Print.cs:421-441`).
///
/// `to_print_buffer` is the optional second argument (`SP_HTML_PRINT`,
/// `ArgumentBuilder.cs:348`). Set, the produced buttons are appended to the
/// print buffer and the line stays open; clear, Emuera flushes the print buffer
/// first and then *adds complete display lines* — so the flag-0 form always
/// terminates the line.
pub fn html_print(
    s: &str,
    to_print_buffer: bool,
    tx: &mut VirtualConsole,
    images: ImageResolver<'_>,
) -> Result<()> {
    // `if (string.IsNullOrEmpty(str)) return;` (`:423-424`).
    if s.is_empty() {
        return Ok(());
    }

    let init_style = tx.style();
    let init_color = tx.color();
    let init_font = tx.font().to_owned();
    let init_align = tx.align();

    let result = scan(s, to_print_buffer, tx, images);

    tx.set_style(init_style);
    let Color([r, g, b]) = Color::from(init_color);
    tx.set_color(r, g, b);
    tx.set_font(init_font);

    if result.is_ok() && !to_print_buffer {
        tx.new_line();
    }
    // `state.Alignment` belongs to the lines this one string produced
    // (`HtmlManager.cs:623-629` applies it per `ConsoleDisplayLine`); it is
    // never the console's own alignment, so it stops at the final line break.
    // `VirtualConsole` carries `align` across `push_line`, hence the restore.
    tx.set_align(init_align);

    result
}

fn scan(
    s: &str,
    to_print_buffer: bool,
    tx: &mut VirtualConsole,
    images: ImageResolver<'_>,
) -> Result<()> {
    // Both scanned once up front, exactly as `html2DisplayLine` does
    // (`HtmlManager.cs:463-464`): a string with no `<!--` never looks for a
    // comment and a string with no newline never treats one as `<br>`.
    let has_comment = s.contains("<!--");
    let has_return = s.contains('\n');

    let mut st = State::new(tx, images);
    let mut rest = s;

    while !rest.is_empty() {
        // The next text run ends at `<`, or at a newline when the string has
        // one (`:477-483`).
        let mut cut = rest.find('<');
        if has_return {
            if let Some(nl) = rest.find('\n') {
                if cut.is_none_or(|c| c > nl) {
                    cut = Some(nl);
                }
            }
        }

        let Some(cut) = cut else {
            st.push_text(unescape(rest)?);
            // These two are only reachable on the run that reaches the end of
            // the string (`:486-492`) — Emuera does not check the same thing
            // for a run followed by another tag.
            if st.flag_p_closed {
                bail!("</p>の後にテキストがあります");
            }
            if st.flag_nobr_closed {
                bail!("</nobr>の後にテキストがあります");
            }
            break;
        };

        if cut > 0 {
            st.push_text(unescape(&rest[..cut])?);
            st.line_head = false;
            rest = &rest[cut..];
        }

        if has_comment && rest.starts_with("<!--") {
            let Some(end) = rest[4..].find("-->") else {
                bail!("コメント終了タグ\"-->\"がみつかりません");
            };
            rest = &rest[4 + end + 3..];
            continue;
        }

        if has_return && rest.starts_with('\n') {
            // A literal newline in the text is a `<br>` (`:511-514`).
            st.flag_br = true;
            rest = &rest[1..];
        } else {
            let Some(close) = rest.find('>') else {
                bail!("タグ終端'>'が見つかりません");
            };
            tag_analyze(&mut st, rest[1..close].trim(), s)?;
            rest = &rest[close + 1..];
        }

        // `:575-588`. A `<br>` flushes the run and breaks the line; a button
        // boundary only flushes. Every line this string produces is aligned by
        // the *HTML* state, which starts LEFT (`HtmlAnalzeState.Alignment`,
        // `HtmlManager.cs:237`) and only a `<p align>` moves — the console's
        // own `ALIGNMENT` never reaches it, because `SetAlignment` keeps the
        // first alignment it is given (`ConsoleDisplayLine.cs:61-64`) and
        // `Html2DisplayLine` gets there first (`:623-629`), before the print
        // path would apply `alignment` (`EmueraConsole.Print.cs:179-182`).
        // That default is what centres eramegaten_p_kr's title picture: the
        // box's `xpos` is already the centring offset the game computed
        // (`PRINT_EVENT_PICTURE.ERB:50-69`), so the line must not centre it a
        // second time.
        //
        // The alignment reaches a `<div>`'s own lines even in the
        // print-buffer form, because the box's content comes from a full
        // `html2DisplayLine` recursion that runs the `SetAlignment` loop
        // (`:557-563`, `:610-624`).
        let in_div = !st.divs.is_empty();
        if st.flag_br {
            st.flush(tx);
            if in_div || !to_print_buffer {
                let align = st.alignment.unwrap_or(Alignment::Left);
                st.out(tx).set_align(align);
            }
            st.out(tx).new_line();
            st.line_head = true;
        } else if st.flag_button {
            st.flush(tx);
        }

        // The box's frame opens *after* the tag's own flags were handled, so
        // the run that preceded the tag still belongs to the enclosing line.
        if let Some(spec) = st.opening_div.take() {
            let console = st.out(tx).sub_console();
            let outer = std::mem::take(&mut st.pending);
            st.divs.push(DivFrame {
                spec,
                console,
                outer,
            });
        }
        if st.closing_div {
            st.closing_div = false;
            // Everything printed since the open tag is the box's content.
            st.flush(tx);
            // The box's own last line is aligned like the ones a `<br>` ended:
            // `SetAlignment(state.Alignment, SubDivisionWidth, ..)` runs over
            // every line the box produced (`HtmlManager.cs:623-627`).
            let align = st.alignment.unwrap_or(Alignment::Left);
            st.out(tx).set_align(align);
            let font_size = st.images.font_size();
            let Some(frame) = st.divs.pop() else {
                unreachable!("</div> without a frame is rejected by close_tag");
            };
            st.pending = frame.outer;
            let div = ConsoleDiv::new(frame.spec, font_size, frame.console.into_lines());
            st.pending.push(PendingPart::Div(Arc::new(div)));
        }
        st.flag_br = false;
        st.flag_button = false;
        st.pending_button = st.button.as_ref().map(|b| {
            // `<clearbutton>` strips clickability, and `notooltip='true'` the
            // tooltip too (`:1329-1334`).
            if st.flag_clear_button {
                (false, None)
            } else {
                (b.is_button_tag, b.value.clone())
            }
        });
    }

    // `</nobr>` and `</p>` may be left out; everything else may not
    // (`:590-596`). Emuera compares against `FontStyle.Regular` because its
    // state starts blank; erars layers the tags over whatever style the
    // console already had, so the base style is the zero point.
    if !st.divs.is_empty()
        || st.button.is_some()
        || !st.fonts.is_empty()
        || st.font_style != st.base_style
    {
        bail!("閉じられていないタグがあります");
    }

    st.flush(tx);
    // `Html2ButtonList` returns before the `SetAlignment` loop
    // (`HtmlManager.cs:600`), so the print-buffer form leaves the caller's
    // line alone. The printing form aligns its last line like every other one
    // it produced.
    if !to_print_buffer {
        tx.set_align(st.alignment.unwrap_or(Alignment::Left));
    }
    Ok(())
}

/// `tagAnalyze` (`HtmlManager.cs:848-1442`). `body` is the text between `<` and
/// `>`, already trimmed; `row` is the whole `HTML_PRINT` argument, which the
/// catch-all error quotes.
fn tag_analyze(st: &mut State, body: &str, row: &str) -> Result<()> {
    if let Some(name) = body.strip_prefix('/') {
        return close_tag(st, name.trim());
    }

    let (name, attrs) = match body.find(|c: char| c.is_whitespace()) {
        Some(at) => (&body[..at], body[at..].trim()),
        None => (body, ""),
    };
    if name.is_empty() {
        bail!("html文字列\"{row}\"のタグ解析中にエラーが発生しました");
    }
    let lower = name.to_ascii_lowercase();

    // `wc == null` when the tag carried no attributes at all (`:930-931`).
    let attrs = if attrs.is_empty() {
        None
    } else {
        Some(parse_attrs(attrs, row)?)
    };

    match lower.as_str() {
        "b" | "i" | "u" | "s" => {
            let bit = match lower.as_str() {
                "b" => FontStyle::BOLD,
                "i" => FontStyle::ITALIC,
                "u" => FontStyle::UNDERLINE,
                _ => FontStyle::STRIKELINE,
            };
            if attrs.is_some() {
                bail!("<{name}>タグにに属性が設定されています");
            }
            if st.font_style.contains(bit) {
                bail!("<{name}>が二重に使われています");
            }
            st.font_style |= bit;
        }
        "br" => {
            if attrs.is_some() {
                bail!("<{name}>タグにに属性が設定されています");
            }
            st.flag_br = true;
        }
        "nobr" => {
            if attrs.is_some() {
                bail!("<{name}>タグにに属性が設定されています");
            }
            if !st.line_head {
                bail!("<nobr>が行頭以外で使われています");
            }
            if st.flag_nobr {
                bail!("<nobr>が二重に使われています");
            }
            st.flag_nobr = true;
        }
        "p" => {
            let Some(attrs) = attrs else {
                bail!("<{name}>タグに属性が設定されていません");
            };
            if !st.line_head {
                bail!("<p>が行頭以外で使われています");
            }
            // Emuera really does test `FlagNobr` here, not `FlagP` (`:973-974`).
            if st.flag_nobr {
                bail!("<p>が二重に使われています");
            }
            let [(key, value)] = &attrs[..] else {
                bail!("html文字列\"{row}\"のタグ解析中にエラーが発生しました");
            };
            if !key.eq_ignore_ascii_case("align") {
                bail!("<p>タグの属性名{key}は解釈できません");
            }
            st.alignment = Some(match value.to_ascii_lowercase().as_str() {
                "left" => Alignment::Left,
                "center" => Alignment::Center,
                "right" => Alignment::Right,
                _ => bail!("属性値{value}は解釈できません"),
            });
            st.flag_p = true;
        }
        "img" => {
            let Some(attrs) = attrs else {
                bail!("<{name}>タグに属性が設定されていません");
            };
            let mut src = None;
            let mut src_b = None;
            let mut src_m = None;
            let mut width = None;
            let mut height = None;
            let mut ypos = None;
            for (key, value) in &attrs {
                // Every slot is `DuplicateAttribute` on a second occurrence —
                // the strings check `!= null` (`:1029-1046`) and the numbers go
                // through `ParseMixedNum`, which rejects a non-null target
                // (`Utils.cs:128-130`).
                match key.to_ascii_lowercase().as_str() {
                    "src" => {
                        dup_attr(&src, name, key)?;
                        src = Some(value.clone());
                    }
                    // The alternate bitmap drawn while the enclosing button is
                    // focused, and the hit mask `GETMAPPINGCOLOR` samples.
                    "srcb" => {
                        dup_attr(&src_b, name, key)?;
                        src_b = Some(value.clone());
                    }
                    "srcm" => {
                        dup_attr(&src_m, name, key)?;
                        src_m = Some(value.clone());
                    }
                    "width" => {
                        dup_attr(&width, name, key)?;
                        width = Some(parse_mixed_num(name, key, value)?);
                    }
                    "height" => {
                        dup_attr(&height, name, key)?;
                        height = Some(parse_mixed_num(name, key, value)?);
                    }
                    "ypos" => {
                        dup_attr(&ypos, name, key)?;
                        ypos = Some(parse_mixed_num(name, key, value)?);
                    }
                    // DELIBERATE: Emuera raises `CanNotInterpretAttributeName`
                    // (`HtmlManager.cs:1060-1061`). eramegaten_p_kr writes
                    // `xpos` on an `<img>` twice
                    // (`Data/ERB/関数/汎用組み込み関数/画像処理/キャラ・NPC顔アイコン.ERB:23`,
                    // `:35`) and `img_size` twice
                    // (`Data/ERB/口上/KOJO_RPG.ERB:950`, `:952`), so the strict
                    // rule takes out two live files for attributes a newer
                    // EvilMask build accepts. Ignored, as an unknown CSS
                    // property is in a browser.
                    _ => {}
                }
            }
            let Some(src) = src else {
                bail!("<{name}>タグにsrc属性が設定されていません");
            };
            match st.images.resolve(
                &src,
                src_b.as_deref(),
                src_m.as_deref(),
                width,
                height,
                ypos,
            ) {
                Ok(image) => st.push_image(image),
                // `Str = AltText` (`ConsoleImagePart.cs:69-73`): a missing
                // resource prints the reconstructed tag, it is not an error.
                Err(alt) => st.push_text(alt),
            }
        }
        // The box itself is laid out by the front-end; this builds the
        // `HtmlDivTag` Emuera's parser builds (`HtmlManager.cs:1067-1173`).
        //
        // DELIBERATE, all four the same relaxation this fork's `<div>` would
        // fail on, because eramegaten_p_kr targets a newer EvilMask build:
        // `width`/`height` are optional where `NotSetAttribute` is mandatory
        // (`:1166-1169`) — 186 of the game's 369 boxes have neither, and its
        // event-picture path
        // (`Data/ERB/関数/組み込み関数/メッセージ/PRINT_EVENT_PICTURE.ERB:70`)
        // is one of them; boxes nest where `NestedTag` forbids it
        // (`:1070-1071`) — `Data/ERB/関数/汎用組み込み関数/DIV_メッセージウィンドウ/DIV_MESSAGE_LOG.ERB:61-71`
        // nests three deep; an unknown attribute name is ignored where
        // `CanNotInterpretAttributeName` rejects it (`:1163-1164`), which is
        // what carries the game's source typos `ypps` (4 sites) and `yos`
        // (1); and a box may open inside a `<button>` where
        // `TagIsNotClosed` forbids it (`:531-532`) —
        // `Data/ERB/ＳＨＯＰ関連/120_ショップ.ERB:49` does exactly that, and
        // relies on the box's content inheriting the button.
        //
        // The newer build's attribute spellings are aliases:
        // `background_color` (48 sites) for `color`, `border_width` (58) for
        // `border`, `border_color` (19) for `bcolor`.
        "div" => {
            let mut spec = DivSpec::default();
            let mut display = false;
            for (key, value) in attrs.iter().flatten() {
                let lower = key.to_ascii_lowercase();
                match lower.as_str() {
                    "width" => {
                        dup_attr(&spec.width, name, key)?;
                        spec.width = Some(parse_mixed_num(name, key, value)?);
                    }
                    "height" => {
                        dup_attr(&spec.height, name, key)?;
                        spec.height = Some(parse_mixed_num(name, key, value)?);
                    }
                    "xpos" => {
                        dup_attr(&spec.x, name, key)?;
                        spec.x = Some(parse_mixed_num(name, key, value)?);
                    }
                    "ypos" => {
                        dup_attr(&spec.y, name, key)?;
                        spec.y = Some(parse_mixed_num(name, key, value)?);
                    }
                    // `size='w,h'` and `rect='x,y,w,h'` (`:1121-1160`) fill
                    // the same slots, so a duplicate is caught there.
                    "size" => {
                        let [w, h] = split_attr(name, key, value)?;
                        dup_attr(&spec.width, name, "width")?;
                        dup_attr(&spec.height, name, "height")?;
                        spec.width = Some(parse_mixed_num(name, "width", w)?);
                        spec.height = Some(parse_mixed_num(name, "height", h)?);
                    }
                    "rect" => {
                        let [x, y, w, h] = split_attr(name, key, value)?;
                        dup_attr(&spec.x, name, "xpos")?;
                        dup_attr(&spec.y, name, "ypos")?;
                        dup_attr(&spec.width, name, "width")?;
                        dup_attr(&spec.height, name, "height")?;
                        spec.x = Some(parse_mixed_num(name, "xpos", x)?);
                        spec.y = Some(parse_mixed_num(name, "ypos", y)?);
                        spec.width = Some(parse_mixed_num(name, "width", w)?);
                        spec.height = Some(parse_mixed_num(name, "height", h)?);
                    }
                    "color" | "background_color" => {
                        dup_attr(&spec.background, name, key)?;
                        spec.background = Some(parse_color(value)?);
                    }
                    "bcolor" | "border_color" => {
                        dup_attr(&spec.border_color, name, key)?;
                        spec.border_color = Some(parse_color4(value)?);
                    }
                    "border" | "border_width" => {
                        dup_attr(&spec.border, name, key)?;
                        spec.border = Some(parse_mixed4(name, key, value)?);
                    }
                    "margin" => {
                        dup_attr(&spec.margin, name, key)?;
                        spec.margin = Some(parse_mixed4(name, key, value)?);
                    }
                    "padding" => {
                        dup_attr(&spec.padding, name, key)?;
                        spec.padding = Some(parse_mixed4(name, key, value)?);
                    }
                    "radius" => {
                        dup_attr(&spec.radius, name, key)?;
                        spec.radius = Some(parse_mixed4(name, key, value)?);
                    }
                    "display" => {
                        if display {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        display = true;
                        spec.anchor = parse_anchor(value)?;
                    }
                    // `depth` orders a box against the `CBG*` planes
                    // (`EmueraConsole.cs:1557-1599` merges parts and CBG
                    // entries by depth). DELIBERATE: parsed and range-checked
                    // as Emuera does (`:1108-1115`) but not honoured — erars
                    // draws every box above the log text, see §5 of
                    // `docs/research/2026-09-03-emuera-command-gap.md`. No
                    // eramegaten_p_kr `<div>` sets it.
                    "depth" => {
                        if value.trim().parse::<i32>().is_err() {
                            bail!("<{name}>タグの{key}属性の属性値が数値として解釈できません");
                        }
                    }
                    _ => {}
                }
            }
            st.opening_div = Some(spec);
        }
        "shape" => {
            let Some(attrs) = attrs else {
                bail!("<{name}>タグに属性が設定されていません");
            };
            let mut color = false;
            let mut bcolor = false;
            for (key, value) in &attrs {
                match key.to_ascii_lowercase().as_str() {
                    "color" => {
                        if color {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        parse_color(value)?;
                        color = true;
                    }
                    "bcolor" => {
                        if bcolor {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        parse_color(value)?;
                        bcolor = true;
                    }
                    "type" | "param" => {}
                    _ => bail!("<{name}>タグの属性名{key}は解釈できません"),
                }
            }
        }
        "button" | "nonbutton" => {
            if st.button.is_some() {
                bail!("<button>又は<nonbutton>が入れ子にされています");
            }
            let is_button_tag = lower == "button";
            let mut tag = ButtonTag {
                is_button_tag,
                ..Default::default()
            };
            for (key, value) in attrs.iter().flatten() {
                match key.to_ascii_lowercase().as_str() {
                    "value" => {
                        // Emuera reuses `NotSetAttribute` for `value` on a
                        // `<nonbutton>` (`:1282-1283`).
                        if !is_button_tag {
                            bail!("<{name}>タグにvalue属性が設定されていません");
                        }
                        if tag.value.is_some() {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        tag.value = Some(value.clone());
                    }
                    "title" => {
                        if tag.title.is_some() {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        tag.title = Some(value.clone());
                    }
                    "pos" => {
                        if tag.pos.is_some() {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        let Ok(pos) = value.parse::<i32>() else {
                            bail!("<{name}>タグのpos属性の属性値が数値として解釈できません");
                        };
                        if !st.flag_nobr {
                            bail!("<nobr>が設定されていない行ではpos属性は使用できません");
                        }
                        if !matches!(st.alignment, None | Some(Alignment::Left)) {
                            bail!("alignがleftでない行ではpos属性は使用できません");
                        }
                        tag.pos = Some(pos);
                    }
                    _ => bail!("<{name}>タグの属性名{key}は解釈できません"),
                }
            }
            st.button = Some(tag);
            st.flag_button = true;
        }
        "clearbutton" => {
            if st.flag_clear_button {
                bail!("<clearbutton>が入れ子にされています");
            }
            for (key, value) in attrs.iter().flatten() {
                if key.eq_ignore_ascii_case("notooltip") {
                    match value.to_ascii_lowercase().as_str() {
                        "true" => st.flag_clear_button_tooltip = true,
                        "false" => {}
                        _ => bail!(
                            "<{name}>タグの{key}属性の属性値{value}は数値として解釈できません"
                        ),
                    }
                } else {
                    bail!("<{name}>タグの属性名{key}は解釈できません");
                }
            }
            st.flag_clear_button = true;
        }
        "font" => {
            let Some(attrs) = attrs else {
                bail!("<{name}>タグに属性が設定されていません");
            };
            let mut font = FontTag::default();
            for (key, value) in &attrs {
                match key.to_ascii_lowercase().as_str() {
                    "color" => {
                        if font.color.is_some() {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        font.color = Some(parse_color(value)?);
                    }
                    "bcolor" => {
                        if font.bcolor.is_some() {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        font.bcolor = Some(parse_color(value)?);
                    }
                    "face" => {
                        if font.face.is_some() {
                            bail!("<{name}>タグに{key}属性が2度以上指定されています");
                        }
                        font.face = Some(value.clone());
                    }
                    _ => bail!("<{name}>タグの属性名{key}は解釈できません"),
                }
            }
            // Inherit whatever this tag left unset from the enclosing `<font>`
            // (`:1421-1431`).
            if let Some(outer) = st.fonts.last() {
                font.color = font.color.or(outer.color);
                font.bcolor = font.bcolor.or(outer.bcolor);
                font.face = font.face.take().or_else(|| outer.face.clone());
            }
            st.fonts.push(font);
        }
        _ => bail!("html文字列\"{row}\"のタグ解析中にエラーが発生しました"),
    }

    Ok(())
}

/// The close-tag switch (`HtmlManager.cs:864-918`).
fn close_tag(st: &mut State, name: &str) -> Result<()> {
    match name.to_ascii_lowercase().as_str() {
        tag @ ("b" | "i" | "u" | "s") => {
            let bit = match tag {
                "b" => FontStyle::BOLD,
                "i" => FontStyle::ITALIC,
                "u" => FontStyle::UNDERLINE,
                _ => FontStyle::STRIKELINE,
            };
            if !st.font_style.contains(bit) {
                bail!("</{tag}>の前に<{tag}>がありません");
            }
            st.font_style ^= bit;
        }
        "p" => {
            if !st.flag_p || st.flag_p_closed {
                bail!("</p>の前に<p>がありません");
            }
            st.flag_p_closed = true;
        }
        "nobr" => {
            if !st.flag_nobr || st.flag_nobr_closed {
                bail!("</nobr>の前に<nobr>がありません");
            }
            st.flag_nobr_closed = true;
        }
        "font" => {
            if st.fonts.pop().is_none() {
                bail!("</font>の前に<font>がありません");
            }
        }
        "button" => {
            if !st.button.as_ref().is_some_and(|b| b.is_button_tag) {
                bail!("</button>の前に<button>がありません");
            }
            st.button = None;
            st.flag_button = true;
        }
        "nonbutton" => {
            if !st.button.as_ref().is_some_and(|b| !b.is_button_tag) {
                bail!("</nonbutton>の前に<nonbutton>がありません");
            }
            st.button = None;
            st.flag_button = true;
        }
        "clearbutton" => {
            if !st.flag_clear_button {
                bail!("</clearbutton>の前に<clearbutton>がありません");
            }
            st.flag_clear_button = false;
            st.flag_clear_button_tooltip = false;
        }
        // `state.CurrentDivTag = null` (`HtmlManager.cs:910-914`), which is
        // what ends the recursion that was collecting the box's content.
        "div" => {
            if st.divs.is_empty() {
                bail!("</div>の前に<div>がありません");
            }
            st.closing_div = true;
        }
        tag => bail!("終了タグ</{tag}>は解釈できません"),
    }
    Ok(())
}

/// `identifier = '<value>'` triples up to the tag's `>`. Emuera lexes these
/// with `AllowAssignment | AllowSingleQuotationStr` (`HtmlManager.cs:931`), so
/// both quote characters open a string literal and an unquoted value is a lex
/// error that lands on `HtmlTagError`.
fn parse_attrs<'a>(mut s: &'a str, row: &str) -> Result<Vec<(&'a str, String)>> {
    let err = || anyhow::anyhow!("html文字列\"{row}\"のタグ解析中にエラーが発生しました");
    let mut out = Vec::new();

    loop {
        s = s.trim_start();
        if s.is_empty() {
            return Ok(out);
        }

        let key_end = s
            .find(|c: char| c == '=' || c.is_whitespace())
            .ok_or_else(err)?;
        let (key, mut tail) = (&s[..key_end], s[key_end..].trim_start());
        if key.is_empty() || !key.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Err(err());
        }

        tail = tail.strip_prefix('=').ok_or_else(err)?.trim_start();
        let quote = match tail.chars().next() {
            Some(q @ ('\'' | '"')) => q,
            _ => return Err(err()),
        };
        let body = &tail[quote.len_utf8()..];
        let end = body.find(quote).ok_or_else(err)?;

        out.push((key, unescape(&body[..end])?));
        s = &body[end + quote.len_utf8()..];
    }
}

/// `DuplicateAttribute`, for a slot that may appear at most once. Emuera
/// spells the check out per attribute (`HtmlManager.cs:1029-1046`) and inside
/// `ParseMixedNum` (`_Library/EvilMask/Utils.cs:128-130`).
fn dup_attr<T>(slot: &Option<T>, tag: &str, key: &str) -> Result<()> {
    if slot.is_some() {
        bail!("<{tag}>タグに{key}属性が2度以上指定されています");
    }
    Ok(())
}

/// `Utils.ParseMixedNum` (`_Library/EvilMask/Utils.cs:126-139`): a
/// case-insensitive trailing `px` makes the number literal pixels, anything
/// else is a percentage of the font size. `int.TryParse` accepts a leading
/// sign and surrounding whitespace and nothing else, so `18.5px` is an error
/// rather than a truncation.
fn parse_mixed_num(tag: &str, key: &str, value: &str) -> Result<MixedNum> {
    let err = || anyhow::anyhow!("<{tag}>タグの{key}属性の属性値が数値として解釈できません");

    // `EndsWith("px", OrdinalIgnoreCase)` on the raw value. Indexed by char so
    // a multi-byte tail cannot split a code point; a two-char non-ASCII tail
    // simply fails the comparison.
    let (digits, is_px) = match value.char_indices().rev().nth(1) {
        Some((cut, _)) if value[cut..].eq_ignore_ascii_case("px") => (&value[..cut], true),
        _ => (value, false),
    };
    let num = digits.trim().parse::<i32>().map_err(|_| err())?;

    Ok(MixedNum { num, is_px })
}

/// `display` (`HtmlManager.cs:1155-1160`), plus the two spellings the newer
/// EvilMask build uses and eramegaten_p_kr writes exclusively:
/// `absolute-lefttop` (17 sites) and `absolute-leftbottom` (50).
///
/// DELIBERATE: this fork's bare `absolute` puts the box at
/// `MainPicBox.Height - ypos - height` (`ConsoleDivPart.cs:143`), i.e. a
/// *positive* `ypos` measured up from the bottom edge to the box's bottom.
/// erars reads it as `absolute-leftbottom`, where a *negative* `ypos`
/// measures up from the bottom edge to the box's top. The newer convention is
/// the one the corpus needs — `関数/汎用組み込み関数/入力関数/CONVERT_YPOS_TOP_TO_BUTTOM.ERB`
/// converts a top-based ypos into `L_YPOS + (L_BASE_YPOS + 100)` with
/// `L_BASE_YPOS = -GET_HEIGHTLENS() * 100`, and every `absolute-leftbottom`
/// site passes a negative `ypos`, which the fork's formula would push off
/// screen. No corpus site writes bare `absolute`. §5 of
/// `docs/research/2026-09-03-emuera-command-gap.md`.
fn parse_anchor(value: &str) -> Result<DivAnchor> {
    let lower = value.to_ascii_lowercase();
    Ok(match lower.as_str() {
        "relative" => DivAnchor::Relative,
        "absolute-lefttop" => DivAnchor::LeftTop,
        "absolute" | "absolute-leftbottom" => DivAnchor::LeftBottom,
        _ => bail!("属性値{value}は解釈できません"),
    })
}

/// The `size='w,h'` / `rect='x,y,w,h'` token split
/// (`HtmlManager.cs:1121-1160`): the count must be exact.
fn split_attr<'a, const N: usize>(
    tag: &str,
    key: &str,
    value: &'a str,
) -> Result<[&'a str; N]> {
    let mut tokens = value.split(',').map(str::trim);
    let out = std::array::from_fn(|_| tokens.next());
    if out.iter().any(Option::is_none) || tokens.next().is_some() {
        bail!("<{tag}>タグの{key}属性の属性値{value}が数値として解釈できません");
    }
    Ok(out.map(|t| t.expect("checked above")))
}

/// `Utils.ParseParam4MixedNum` (`_Library/EvilMask/Utils.cs:58-88`) — the CSS
/// box-model shorthand, filling [`erars_ui::div::edge`] order: one token is
/// every edge, two are `top,right` mirrored, three are `top,right,bottom`
/// with `left` mirroring `right`, four are written out.
fn parse_mixed4(tag: &str, key: &str, value: &str) -> Result<[MixedNum; 4]> {
    let tokens: Vec<&str> = value.split(',').map(str::trim).collect();
    let one = |i: usize| parse_mixed_num(tag, key, tokens[i]);
    Ok(match tokens.len() {
        1 => [one(0)?; 4],
        2 => {
            let (top, right) = (one(0)?, one(1)?);
            [top, right, top, right]
        }
        3 => {
            let (top, right) = (one(0)?, one(1)?);
            [top, right, one(2)?, right]
        }
        4 => [one(0)?, one(1)?, one(2)?, one(3)?],
        _ => bail!("属性値{value}は解釈できません"),
    })
}

/// `ParseParam4IntNum` (`GameView/HtmlManager.cs:137-169`): the same
/// shorthand over `stringToColorInt32` values, which is how `bcolor` gives
/// each edge of the border its own colour.
fn parse_color4(value: &str) -> Result<[Color; 4]> {
    let tokens: Vec<&str> = value.split(',').map(str::trim).collect();
    let one = |i: usize| parse_color(tokens[i]);
    Ok(match tokens.len() {
        1 => [one(0)?; 4],
        2 => {
            let (top, right) = (one(0)?, one(1)?);
            [top, right, top, right]
        }
        3 => {
            let (top, right) = (one(0)?, one(1)?);
            [top, right, one(2)?, right]
        }
        4 => [one(0)?, one(1)?, one(2)?, one(3)?],
        _ => bail!("属性値{value}は解釈できません"),
    })
}

/// `stringToColorInt32` (`HtmlManager.cs:1444-1484`). `#` plus hex of any
/// length goes through `Convert.ToInt32(_, 16)` and must land in
/// `0..=0xFFFFFF` — note that `#FFF` is `0x000FFF`, not `0xFFFFFF`, because
/// Emuera does not expand the CSS short form.
fn parse_color(s: &str) -> Result<Color> {
    if s.is_empty() {
        bail!("色を表す単語又は#RRGGBB値が必要です");
    }

    if let Some(hex) = s.strip_prefix('#') {
        // `Convert.ToInt32("", 16)` is 0, and a value wider than 8 nibbles
        // overflows into the `catch`.
        let value = if hex.is_empty() {
            0
        } else {
            match i64::from_str_radix(hex, 16) {
                Ok(v) if hex.len() <= 8 => v as i32 as i64,
                _ => bail!("{hex}は数値として解釈できません"),
            }
        };
        if !(0..=0xFFFFFF).contains(&value) {
            bail!("{hex}は適切な色指定の範囲外です");
        }
        return Ok(Color::from(value as u32));
    }

    // `Color.FromName("Transparent")` succeeds with `A == 0`, which Emuera
    // treats as a failure and then reports specially (`:1466-1469`).
    if s.eq_ignore_ascii_case("transparent") {
        bail!("無色透明(Transparent)は色として指定できません");
    }

    match s.parse::<css_color::Srgb>() {
        Ok(rgb) => Ok(Color([
            (rgb.red * 255.0) as u8,
            (rgb.green * 255.0) as u8,
            (rgb.blue * 255.0) as u8,
        ])),
        // A name that is nonetheless valid hex was probably a forgotten `#`
        // (`:1470-1479`).
        Err(_) if i64::from_str_radix(s, 16).is_ok() => bail!(
            "指定された色名\"{s}\"は無効な色名です(16進数で色を指定する場合には数値の前に#が必要です)"
        ),
        // Emuera passes this one through `new CodeEE(text)` without
        // `string.Format`, so the real build prints a literal `{0}`
        // (`:1476`); erars substitutes the name.
        Err(_) => bail!("指定された色名\"{s}\"は無効な色名です"),
    }
}

/// `Unescape` (`HtmlManager.cs:665-733`): the six named references plus
/// `&#N;` / `&#xN;`, capped at `U+FFFF` because Emuera appends a single
/// UTF-16 `char`.
fn unescape(s: &str) -> Result<String> {
    if !s.contains('&') {
        return Ok(s.to_owned());
    }

    let mut out = String::with_capacity(s.len());
    let mut rest = s;

    while let Some(amp) = rest.find('&') {
        out.push_str(&rest[..amp]);
        rest = &rest[amp..];

        let Some(semi) = rest.find(';') else {
            bail!("'&'に対応する';'がみつかりません");
        };
        if semi == 1 {
            bail!("'&'と';'が連続しています");
        }
        let word = &rest[1..semi];
        rest = &rest[semi + 1..];

        match word.to_ascii_lowercase().as_str() {
            "nbsp" => out.push(' '),
            "amp" => out.push('&'),
            "gt" => out.push('>'),
            "lt" => out.push('<'),
            "quot" => out.push('"'),
            "apos" => out.push('\''),
            lower => {
                let Some(digits) = lower.strip_prefix('#') else {
                    bail!("\"&{word};\"は適切な文字参照ではありません");
                };
                let code = match digits.strip_prefix('x') {
                    Some(hex) => i64::from_str_radix(hex, 16),
                    None => digits.parse::<i64>(),
                };
                let Ok(code) = code else {
                    bail!("\"&{word};\"は適切な文字参照ではありません");
                };
                // `(char)unicode` on a lone surrogate is why Emuera rejects
                // anything above the BMP (`:725-727`).
                let Some(c) = u32::try_from(code).ok().filter(|c| *c <= 0xFFFF).and_then(char::from_u32)
                else {
                    bail!("\"&{word};\"はUnicodeの範囲外です(サロゲートペアは使えません)");
                };
                out.push(c);
            }
        }
    }

    out.push_str(rest);
    Ok(out)
}

/// `HTML_ESCAPE` — Emuera `HtmlManager.Escape`
/// (`GameView/HtmlManager.cs:640-662`). The table is the five characters at
/// `:198-202`, in Emuera's own spellings: `&amp;`, `&gt;`, `&lt;`, `&quot;`,
/// `&apos;`. Nothing else is touched, so this is not `HttpUtility.HtmlEncode`
/// and non-ASCII text passes through unchanged.
pub(crate) fn escape(s: &str) -> String {
    // Emuera scans with `IndexOfAny` and only allocates when it finds one; the
    // common case in `DisplayLine2Html` is a run with no metacharacter at all.
    let Some(first) = s.find(['&', '>', '<', '"', '\'']) else {
        return s.to_owned();
    };

    let mut out = String::with_capacity(s.len() + 8);
    out.push_str(&s[..first]);
    for c in s[first..].chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '>' => out.push_str("&gt;"),
            '<' => out.push_str("&lt;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&apos;"),
            c => out.push(c),
        }
    }
    out
}

/// `HTML_TOPLAINTEXT` — Emuera `HtmlManager.Html2PlainText`
/// (`GameView/HtmlManager.cs:634-638`): `Regex.Replace(str, "\\<[^<]*\\>", "")`
/// and then [`unescape`], which is why a malformed character reference in the
/// *text* is still an error even though the tags were thrown away.
///
/// The regex is reproduced by hand rather than by pulling in a regex engine,
/// and its two quirks are load-bearing. `[^<]*` cannot cross a `<`, so a `<`
/// with no `>` before the next `<` is literal text. Within that span the `*`
/// is greedy and .NET backtracks to the **last** `>`, so `<a>b>` is removed
/// whole rather than leaving `b>`.
pub(crate) fn to_plain_text(s: &str) -> Result<String> {
    let mut stripped = String::with_capacity(s.len());
    let mut rest = s;

    while let Some(lt) = rest.find('<') {
        let after = &rest[lt + 1..];
        // `[^<]*` stops at the next `<`.
        let span = after.find('<').map_or(after, |n| &after[..n]);
        match span.rfind('>') {
            Some(gt) => {
                stripped.push_str(&rest[..lt]);
                rest = &after[gt + 1..];
            }
            // No `>` reachable without crossing a `<`: this one is text.
            None => {
                stripped.push_str(&rest[..lt + 1]);
                rest = after;
            }
        }
    }
    stripped.push_str(rest);

    unescape(&stripped)
}

/// One console line back to HTML — Emuera `HtmlManager.DisplayLine2Html`
/// (`GameView/HtmlManager.cs:290-393`), shared by `HTML_GETPRINTEDSTR`
/// (`wrap` true) and `HTML_POPPRINTINGSTR` (`wrap` false).
///
/// `fore` and `font_family` are the *configured* defaults, because that is
/// what decides whether a run needs a `<font>` tag at all:
/// `getStringStyleStartingTag` (`:780-823`) tests `Fontname != Config.FontName`
/// and `StringStyle.ColorChanged`, and `ColorChanged` is itself
/// `color != Config.ForeColor` (`EmueraConsole.Print.cs:83`).
///
/// DELIBERATE, four ways, all forced by erars keeping one logical line per
/// [`ConsoleLine`] instead of Emuera's pre-wrapped display lines:
///
/// * No `<br>` is ever emitted. Emuera joins the display lines of one logical
///   line with `<br>` (`:312-313`); erars wraps in the renderer against the
///   live window, so the VM has exactly one display line to emit and a `<br>`
///   would be a claim about a wrap that has not happened yet.
/// * No `<nonbutton>` and no `pos=`. Emuera opens a tag for a non-button run
///   only when it carries a `title` or a locked X (`:319-321`), and
///   [`ConsoleLinePart`] carries neither — `title` is dropped at parse time
///   (already recorded in §5.1) and the locked X belongs to `<div>`, which
///   renders nothing here.
/// * No `bcolor=`. `TextStyle` has no per-run focus colour, so the
///   `ButtonColor != Config.FocusColor` term of `fontChanged` is always false.
/// * An image contributes its alt text, unescaped, exactly as `:359-361`.
pub(crate) fn line_to_html(
    line: &ConsoleLine,
    wrap: bool,
    fore: Color,
    font_family: &str,
) -> String {
    let mut out = String::new();

    if wrap {
        out.push_str(match line.align {
            Alignment::Left => "<p align='left'>",
            Alignment::Center => "<p align='center'>",
            Alignment::Right => "<p align='right'>",
        });
        out.push_str("<nobr>");
    }

    for part in &line.parts {
        match part {
            ConsoleLinePart::Text(text, style) | ConsoleLinePart::Line(text, style) => {
                push_styled(&mut out, text, style, fore, font_family);
            }
            ConsoleLinePart::Button(runs, _, value) => {
                out.push_str("<button value='");
                out.push_str(&escape(&button_inputs(value)));
                out.push_str("'>");
                for (text, style) in runs {
                    push_styled(&mut out, text, style, fore, font_family);
                }
                out.push_str("</button>");
            }
            ConsoleLinePart::Image(img) => out.push_str(&img.alt),
            // A positioned box contributes nothing: `DisplayLine2Html`
            // handles `ConsoleStyledString`, `ConsoleImagePart` and
            // `ConsoleShapePart` and has no branch for a `ConsoleDivPart`
            // (`HtmlManager.cs:352-377`), so HTML_GETPRINTEDSTR drops the box
            // and its content in Emuera too. The plain-text form
            // (`Display for ConsoleLinePart`) does print it, exactly as
            // `ConsoleDisplayLine.BuildString` does.
            ConsoleLinePart::Div(_) => {}
        }
    }

    if wrap {
        out.push_str("</nobr></p>");
    }

    out
}

/// `ConsoleButtonString.Inputs` — the text the button would type into the
/// input box, which for an integer button is the number itself.
fn button_inputs(value: &Value) -> String {
    match value {
        Value::Int(i) => i.to_string(),
        Value::String(s) => s.clone(),
    }
}

/// One `ConsoleStyledString`: its opening tags, its escaped text, its closing
/// tags (`HtmlManager.cs:354-359`, `:780-846`).
fn push_styled(out: &mut String, text: &str, style: &TextStyle, fore: Color, font_family: &str) {
    // `Fontname` is `Config.FontName` when the run set none
    // (`StringStyle.cs:19-22`), so "unset" and "set to the default" are the
    // same thing here.
    let face = (!style.font_family.is_empty() && style.font_family != font_family)
        .then_some(style.font_family.as_str());
    let color_changed = style.color != fore;
    let font_changed = face.is_some() || color_changed;

    if !font_changed && style.font_style.is_empty() {
        out.push_str(&escape(text));
        return;
    }

    if font_changed {
        out.push_str("<font");
        if let Some(face) = face {
            out.push_str(" face='");
            out.push_str(&escape(face));
            out.push('\'');
        }
        if color_changed {
            let Color([r, g, b]) = style.color;
            // `colorValue.ToString("X6")` — upper case, six digits.
            out.push_str(&format!(
                " color='#{:06X}'",
                (u32::from(r) << 16) | (u32::from(g) << 8) | u32::from(b)
            ));
        }
        out.push('>');
    }

    // Opened outside-in strikeout, underline, italic, bold (`:812-819`).
    for (flag, tag) in [
        (FontStyle::STRIKELINE, "<s>"),
        (FontStyle::UNDERLINE, "<u>"),
        (FontStyle::ITALIC, "<i>"),
        (FontStyle::BOLD, "<b>"),
    ] {
        if style.font_style.contains(flag) {
            out.push_str(tag);
        }
    }

    out.push_str(&escape(text));

    // Closed inside-out (`:831-838`).
    for (flag, tag) in [
        (FontStyle::BOLD, "</b>"),
        (FontStyle::ITALIC, "</i>"),
        (FontStyle::UNDERLINE, "</u>"),
        (FontStyle::STRIKELINE, "</s>"),
    ] {
        if style.font_style.contains(flag) {
            out.push_str(tag);
        }
    }

    if font_changed {
        out.push_str("</font>");
    }
}

/// `HTML_GETPRINTEDSTR(lineNo)` — the already-printed logical line `lineNo`
/// back from the last one, or `""` when there is no such line
/// (`EmueraConsole.Print.cs:752-771`, `Creator.Method.cs:5045-5059`).
///
/// Emuera walks its display-line list backwards counting logical-line ends and
/// returns every display line of the one it lands on. erars stores one logical
/// line per entry, so the walk lands on exactly `lines[len - 1 - line_no]`,
/// and `line_no >= len` is the `null` case Emuera turns into `""`.
///
/// The line still being typed is deliberately *not* reachable here: it lives
/// in `VirtualConsole::last_line`, which is Emuera's `printBuffer` and is what
/// [`pop_printing_str`] takes.
pub(crate) fn get_printed_str(
    tx: &VirtualConsole,
    line_no: i64,
    fore: Color,
    font_family: &str,
) -> String {
    let Ok(back) = usize::try_from(line_no) else {
        return String::new();
    };
    let Some(index) = tx.lines.len().checked_sub(back + 1) else {
        return String::new();
    };

    line_to_html(&tx.lines[index], true, fore, font_family)
}

/// `HTML_POPPRINTINGSTR()` — take the line still being built out of the
/// console and return it as HTML (`EmueraConsole.Print.cs:773-780`,
/// `Creator.Method.cs:5072-5077`).
///
/// Emuera calls `printBuffer.Flush`, which *empties* the buffer, so the pending
/// text is consumed and never reaches the screen; erars empties `last_line` for
/// the same reason. An empty buffer is `null` there and `""` here, and the
/// alignment is preserved across the take exactly as `new_line` does, because
/// `SETALIGN` is line state that survives the flush
/// (`EmueraConsole.Print.cs:87-88`).
///
/// `Console.Enabled` is `false` only while Emuera is tearing down, which erars
/// has no equivalent of; `skipdisp` is the script-visible flag and it already
/// stops the text from being printed at all, so nothing reaches `last_line`.
pub(crate) fn pop_printing_str(
    tx: &mut VirtualConsole,
    fore: Color,
    font_family: &str,
) -> String {
    if tx.last_line.parts.is_empty() {
        return String::new();
    }

    let taken = std::mem::take(&mut tx.last_line);
    tx.last_line.align = taken.align;
    tx.need_rebuild = true;

    line_to_html(&taken, false, fore, font_family)
}

#[cfg(test)]
mod tests {
    use super::{html_print, parse_color, parse_mixed_num, unescape};
    use crate::graphics::{GraphicsStore, ImageResolver};
    use erars_ui::image::MixedNum;
    use erars_ui::{
        Color, ConsoleConfig, ConsoleLinePart, DivAnchor, FontStyle, VirtualConsole,
    };

    /// The console font size every geometry expectation below is derived from.
    const FS: i32 = 18;

    fn console() -> VirtualConsole {
        VirtualConsole::new(&ConsoleConfig {
            printc_width: 25,
            max_log: 500,
            drawable_cells: 84,
            encoding: encoding_rs::UTF_8,
            fore_color: Color([192, 192, 192]),
            bg_color: Color([0, 0, 0]),
            focus_color: Color([255, 255, 0]),
        })
    }

    /// A resolver over a permanently empty store, for the tests that print no
    /// image: every `<img src>` in them misses and prints its alt text.
    fn res() -> ImageResolver<'static> {
        static EMPTY: std::sync::LazyLock<GraphicsStore> =
            std::sync::LazyLock::new(GraphicsStore::default);
        ImageResolver::new(&EMPTY, FS)
    }

    /// Every completed line plus the pending one, joined by `|`.
    fn render(s: &str, to_print_buffer: bool) -> String {
        let mut tx = console();
        html_print(s, to_print_buffer, &mut tx, res()).unwrap();
        let mut out = String::new();
        for line in tx.lines_from(0).iter() {
            out.push_str(&line.to_string());
            out.push('|');
        }
        out.push_str(&tx.last_line.to_string());
        out
    }

    fn reject(s: &str) -> String {
        let mut tx = console();
        html_print(s, false, &mut tx, res())
            .expect_err(&format!("{s:?} should have been rejected"))
            .to_string()
    }

    /// A store holding one 40x20 bitmap published as sprite `PIC`.
    fn store_with_pic() -> GraphicsStore {
        let mut store = GraphicsStore::default();
        assert!(store.create(1, 40, 20));
        assert_eq!(store.sprite_create("PIC".into(), 1, None), Ok(true));
        store
    }

    fn print_html(store: &GraphicsStore, s: &str) -> VirtualConsole {
        let mut tx = console();
        html_print(s, true, &mut tx, ImageResolver::new(store, FS)).unwrap();
        tx
    }

    #[test]
    fn unescape_named_and_numeric() {
        assert_eq!(unescape("plain").unwrap(), "plain");
        assert_eq!(unescape("a&amp;b").unwrap(), "a&b");
        assert_eq!(unescape("&lt;&gt;&quot;&apos;&nbsp;").unwrap(), "<>\"' ");
        // Case-insensitive, like `escWord.ToLower()`.
        assert_eq!(unescape("&AMP;").unwrap(), "&");
        assert_eq!(unescape("&#65;&#x41;").unwrap(), "AA");
        assert_eq!(unescape("&#12354;").unwrap(), "あ");
        assert_eq!(unescape("x&#65;y&amp;z").unwrap(), "xAy&z");

        assert!(unescape("&amp").is_err(), "missing semicolon");
        assert!(unescape("&;").is_err(), "&; is rejected");
        assert!(unescape("&nope;").is_err(), "unknown named reference");
        assert!(unescape("&#zz;").is_err(), "non-numeric reference");
        // Above the BMP: Emuera appends one UTF-16 `char`.
        assert!(unescape("&#x10000;").is_err());
        assert!(unescape("&#-1;").is_err());
        assert_eq!(unescape("&#xFFFF;").unwrap(), "\u{FFFF}");
    }

    #[test]
    fn color_parsing_follows_convert_to_int32() {
        assert_eq!(parse_color("#FF0000").unwrap(), Color([0xFF, 0, 0]));
        assert_eq!(parse_color("#ff8000").unwrap(), Color([0xFF, 0x80, 0]));
        // NOT the CSS short form: `Convert.ToInt32("FFF", 16)` is 0x000FFF.
        assert_eq!(parse_color("#FFF").unwrap(), Color([0x00, 0x0F, 0xFF]));
        assert_eq!(parse_color("#0").unwrap(), Color([0, 0, 0]));
        assert_eq!(parse_color("#").unwrap(), Color([0, 0, 0]));
        // Named colours.
        assert_eq!(parse_color("red").unwrap(), Color([0xFF, 0, 0]));
        assert_eq!(parse_color("Red").unwrap(), Color([0xFF, 0, 0]));

        // `0x1000000` is past the 24-bit range.
        assert!(parse_color("#1000000").is_err());
        // `Convert.ToInt32("FFFFFFFF", 16)` is -1, which fails `i < 0`.
        assert!(parse_color("#FFFFFFFF").is_err());
        assert!(parse_color("#xyz").is_err());
        assert!(parse_color("").is_err());
        assert!(parse_color("transparent").is_err());
        assert!(parse_color("TRANSPARENT").is_err());
        assert!(parse_color("notacolour").is_err());
        // A bare hex value gets the "you forgot the #" message.
        let e = parse_color("FF0000").unwrap_err().to_string();
        assert!(e.contains("数値の前に#が必要です"), "{e}");
    }

    #[test]
    fn rendering_follows_the_linear_scan() {
        // The flag-0 form terminates the line; the flag-1 form does not.
        assert_eq!(render("plain", false), "plain|");
        assert_eq!(render("plain", true), "plain");
        // An empty string returns before anything happens.
        assert_eq!(render("", false), "");

        // Style tags leave no marks in the text.
        assert_eq!(render("<b>b</b><i>i</i><u>u</u><s>s</s>", false), "bius|");
        // `<br>` and a literal newline both break the line.
        assert_eq!(render("a<br>b", false), "a|b|");
        assert_eq!(render("a\nb", false), "a|b|");
        // A comment is dropped whole.
        assert_eq!(render("x<!-- gone -->y", false), "xy|");
        // Entities.
        assert_eq!(render("&lt;&amp;&#65;", false), "<&A|");
        // An unresolved `<img>` prints its reconstructed tag as text; a
        // `<div>` prints as its own reconstructed tag around its content,
        // which is `ConsoleDisplayLine.BuildString`
        // (`_Library/EvilMask/ConsoleDivPart.cs:189-200`) — the text form of
        // a box that the front-end draws at a coordinate. `<shape>` renders
        // nothing.
        assert_eq!(render("<img src='face'>", false), "<img src='face'>|");
        // `4%` of an 18 px font resolves to 0 px, and `MixedNum.BuilderString`
        // writes the *resolved* number (`_Library/EvilMask/Utils.cs:44-48`)
        // while `AddTagMixedNumArg` tests the written one, so the attribute
        // survives as `0`.
        assert_eq!(
            render("<div width='4' height='4'>x</div>", false),
            "<div width='0' height='0'>x</div>|"
        );
        assert_eq!(
            render("<div width='400px' height='50'>x</div>", false),
            "<div width='400px' height='9'>x</div>|"
        );
        assert_eq!(
            render("<shape type='rect' param='4'>x", false),
            "x|"
        );
        // Both quote characters open an attribute value. An unresolved name
        // prints Emuera's reconstructed tag (`ConsoleImagePart.cs:25-73`),
        // which always uses single quotes.
        assert_eq!(render("<img src=\"face\">", false), "<img src='face'>|");
    }

    /// `parse_mixed_num` mirrors `Utils.ParseMixedNum` (`Utils.cs:126-139`).
    #[test]
    fn mixed_num_parsing() {
        assert_eq!(parse_mixed_num("img", "width", "40").unwrap(), MixedNum::percent(40));
        assert_eq!(parse_mixed_num("img", "width", "-40").unwrap(), MixedNum::percent(-40));
        assert_eq!(parse_mixed_num("img", "width", "40px").unwrap(), MixedNum::px(40));
        // `EndsWith(_, OrdinalIgnoreCase)`.
        assert_eq!(parse_mixed_num("img", "width", "40PX").unwrap(), MixedNum::px(40));
        // `int.TryParse` allows surrounding whitespace and nothing else.
        assert_eq!(parse_mixed_num("img", "width", " 40 ").unwrap(), MixedNum::percent(40));
        assert!(parse_mixed_num("img", "width", "40.5").is_err());
        assert!(parse_mixed_num("img", "width", "px").is_err());
        assert!(parse_mixed_num("img", "width", "").is_err());
        // A two-char multi-byte tail must not be mistaken for `px` nor split.
        assert!(parse_mixed_num("img", "width", "４０").is_err());
    }

    /// A resolved `<img>` becomes a real image part, sized per
    /// `ConsoleImagePart.cs:76-116`.
    #[test]
    fn resolved_image_becomes_an_image_part() {
        let store = store_with_pic();

        // `src` alone — the plurality shape in eramegaten_p_kr (45 of 97,
        // `docs/research/emuera-wiki/graphics-usage.md` §5). Height is the
        // font size and the width follows the 40x20 sprite's aspect.
        let tx = print_html(&store, "<img src='pic'>");
        let [ConsoleLinePart::Image(image)] = tx.last_line.parts.as_slice() else {
            panic!("expected one image part, got {:?}", tx.last_line.parts);
        };
        assert_eq!(image.geometry.width, 36);
        assert_eq!((image.geometry.top, image.geometry.bottom), (0, 18));
        assert_eq!(image.sprite.width, 40);
        assert_eq!(image.alt, "<img src='pic'>");

        // The name is matched case-insensitively, as `GetSprite` upper-cases.
        let tx = print_html(&store, "<img src='PIC' height='200px' width='100px'>");
        let [ConsoleLinePart::Image(image)] = tx.last_line.parts.as_slice() else {
            panic!("expected one image part");
        };
        assert_eq!(image.geometry.width, 100);
        assert_eq!(image.geometry.bottom, 200);

        // `ypos` shifts the box above the line without moving `height`.
        let tx = print_html(&store, "<img src='pic' height='100px' ypos='-50px'>");
        let [ConsoleLinePart::Image(image)] = tx.last_line.parts.as_slice() else {
            panic!("expected one image part");
        };
        assert_eq!((image.geometry.top, image.geometry.bottom), (-50, 50));

        // An unresolved name is text, and text either side of an image stays
        // separate parts in source order.
        let tx = print_html(&store, "a<img src='pic'>b<img src='gone'>");
        let kinds: Vec<&str> = tx
            .last_line
            .parts
            .iter()
            .map(|p| match p {
                ConsoleLinePart::Image(_) => "img",
                _ => "text",
            })
            .collect();
        assert_eq!(kinds, ["text", "img", "text"]);
        assert_eq!(tx.last_line.to_string(), "a<img src='pic'>b<img src='gone'>");
    }

    /// An image inside a `<button>` splits the clickable run but keeps order
    /// — see the module header's `<img>`-in-`<button>` note.
    #[test]
    fn image_splits_a_button_run() {
        let store = store_with_pic();
        let tx = print_html(&store, "<button value='7'>a<img src='pic'>b</button>");

        let kinds: Vec<&str> = tx
            .last_line
            .parts
            .iter()
            .map(|p| match p {
                ConsoleLinePart::Image(_) => "img",
                ConsoleLinePart::Button(..) => "button",
                _ => "text",
            })
            .collect();
        assert_eq!(kinds, ["button", "img", "button"]);
        // `ConsoleLine`'s text form is Emuera's alt text for the image part,
        // which is what a text front-end shows.
        assert_eq!(tx.last_line.to_string(), "a<img src='pic'>b");
    }

    #[test]
    fn button_runs_are_one_part() {
        let mut tx = console();
        html_print(
            "<button value='7'>go</button>|<nonbutton>no</nonbutton>|<button>bare</button>",
            false,
            &mut tx,
            res(),
        )
        .unwrap();

        let line = tx.lines_from(0).iter().next().expect("one line").clone();
        assert_eq!(line.to_string(), "go|no|bare");

        // Only the `value` run is clickable, and `Int64.TryParse` made it an int.
        let buttons: Vec<_> = line
            .parts
            .iter()
            .filter_map(|p| match p {
                erars_ui::ConsoleLinePart::Button(parts, _, value) => {
                    Some((parts.len(), value.clone()))
                }
                _ => None,
            })
            .collect();
        assert_eq!(buttons, vec![(1, erars_ast::Value::Int(7))]);

        // A non-integer value keeps its string form.
        let mut tx = console();
        html_print("<button value='go'>x</button>", false, &mut tx, res()).unwrap();
        let line = tx.lines_from(0).iter().next().unwrap();
        assert!(matches!(
            line.parts.as_slice(),
            [erars_ui::ConsoleLinePart::Button(_, _, erars_ast::Value::String(v))] if v == "go"
        ));

        // `<clearbutton>` strips the clickability but keeps the text.
        let mut tx = console();
        html_print(
            "<clearbutton><button value='7'>dead</button></clearbutton>",
            false,
            &mut tx,
            res(),
        )
        .unwrap();
        let line = tx.lines_from(0).iter().next().unwrap();
        assert_eq!(line.to_string(), "dead");
        assert!(!line
            .parts
            .iter()
            .any(|p| matches!(p, erars_ui::ConsoleLinePart::Button(..))));
    }

    #[test]
    fn alignment_is_scoped_to_the_string() {
        let mut tx = console();
        html_print("<p align='center'>mid</p>", false, &mut tx, res()).unwrap();

        // The produced line is centred, but the console's own alignment is not
        // touched (`HtmlManager.cs:623-629`).
        assert_eq!(
            tx.lines_from(0).iter().next().unwrap().align,
            erars_ast::Alignment::Center
        );
        assert_eq!(tx.align(), erars_ast::Alignment::Left);

        // Every line of a multi-line string shares it.
        let mut tx = console();
        html_print("<p align='right'>a<br>b</p>", false, &mut tx, res()).unwrap();
        for line in tx.lines_from(0).iter() {
            assert_eq!(line.align, erars_ast::Alignment::Right);
        }
        assert_eq!(tx.align(), erars_ast::Alignment::Left);

        // The HTML state starts LEFT (`HtmlManager.cs:237`) and its
        // `SetAlignment` runs first, so `ALIGNMENT CENTER` never reaches an
        // `HTML_PRINT` line — `SetAlignment` keeps the first alignment it is
        // given (`ConsoleDisplayLine.cs:61-64`) and the print path's own call
        // (`EmueraConsole.Print.cs:179-182`) finds the line already aligned.
        // eramegaten_p_kr's title screen depends on it: the picture's box
        // carries the centring offset the game computed itself
        // (`PRINT_EVENT_PICTURE.ERB:50-69`), and a centred line would add it
        // twice.
        let mut tx = console();
        tx.set_align(erars_ast::Alignment::Center);
        html_print("a<br>b", false, &mut tx, res()).unwrap();
        for line in tx.lines_from(0).iter() {
            assert_eq!(line.align, erars_ast::Alignment::Left);
        }
        // The console keeps the alignment an ordinary `PRINT` would use.
        assert_eq!(tx.align(), erars_ast::Alignment::Center);

        // A box's own lines start from the same LEFT default.
        let d = div("<div><p align='right'>x</p></div>");
        assert_eq!(d.lines[0].align, erars_ast::Alignment::Right);
        let d = div("<div>x</div>");
        assert_eq!(d.lines[0].align, erars_ast::Alignment::Left);
    }

    #[test]
    fn a_failed_string_leaves_the_style_alone() {
        let mut tx = console();
        tx.set_style(FontStyle::ITALIC);
        tx.set_color(1, 2, 3);
        tx.set_font("Serif".into());

        html_print("<b><font color='red'>x", false, &mut tx, res()).unwrap_err();

        assert_eq!(tx.style(), FontStyle::ITALIC);
        assert_eq!(tx.color(), 0x010203);
        assert_eq!(tx.font(), "Serif");
        // The failing string never terminates the line.
        assert!(tx.lines_from(0).iter().next().is_none());
    }

    #[test]
    fn every_tag_emuera_rejects_is_rejected() {
        // Close tags. `HtmlManager.cs:866-873`, `:917`.
        assert_eq!(reject("</b>"), "</b>の前に<b>がありません");
        assert_eq!(reject("<b>x</b></b>"), "</b>の前に<b>がありません");
        assert_eq!(reject("</p>"), "</p>の前に<p>がありません");
        assert_eq!(reject("</nobr>"), "</nobr>の前に<nobr>がありません");
        assert_eq!(reject("</font>"), "</font>の前に<font>がありません");
        assert_eq!(reject("</button>"), "</button>の前に<button>がありません");
        assert_eq!(
            reject("<button value='1'></nonbutton>"),
            "</nonbutton>の前に<nonbutton>がありません"
        );
        assert_eq!(reject("</div>"), "</div>の前に<div>がありません");
        assert_eq!(
            reject("</clearbutton>"),
            "</clearbutton>の前に<clearbutton>がありません"
        );
        assert_eq!(reject("</marquee>"), "終了タグ</marquee>は解釈できません");

        // Open tags. `:943-966`.
        assert_eq!(reject("<b color='red'>x</b>"), "<b>タグにに属性が設定されています");
        assert_eq!(reject("<b><b>x</b></b>"), "<b>が二重に使われています");
        assert_eq!(reject("<br color='red'>"), "<br>タグにに属性が設定されています");
        assert_eq!(reject("x<nobr>y</nobr>"), "<nobr>が行頭以外で使われています");
        assert_eq!(
            reject("<nobr><nobr>x</nobr></nobr>"),
            "<nobr>が二重に使われています"
        );

        // `<p>`. `:967-1002`.
        assert_eq!(reject("<p>x</p>"), "<p>タグに属性が設定されていません");
        assert_eq!(
            reject("x<p align='left'>y</p>"),
            "<p>が行頭以外で使われています"
        );
        assert_eq!(
            reject("<p color='red'>x</p>"),
            "<p>タグの属性名colorは解釈できません"
        );
        assert_eq!(
            reject("<p align='diagonal'>x</p>"),
            "属性値diagonalは解釈できません"
        );

        // `<img>`. `:1005-1064`.
        assert_eq!(reject("<img>"), "<img>タグに属性が設定されていません");
        // DELIBERATE: an unknown `<img>` attribute is ignored, not
        // `CanNotInterpretAttributeName` (`HtmlManager.cs:1060-1061`) — see
        // the module header. `src` is still mandatory (`:1063-1064`).
        assert_eq!(render("<img alt='x' src='miss'>", false), "<img src='miss'>|");
        assert_eq!(
            reject("<img width='4'>"),
            "<img>タグにsrc属性が設定されていません"
        );
        assert_eq!(
            reject("<img src='a' src='b'>"),
            "<img>タグにsrc属性が2度以上指定されています"
        );
        assert_eq!(
            reject("<img src='a' height='4' height='5'>"),
            "<img>タグにheight属性が2度以上指定されています"
        );
        assert_eq!(
            reject("<img src='a' height='4.5'>"),
            "<img>タグのheight属性の属性値が数値として解釈できません"
        );

        // DELIBERATE: `<div>` does not enforce `width`/`height`
        // (`:1166-1169`), nesting (`:1070-1071`), a `<button>` around it
        // (`:531-532`) or the attribute-name whitelist (`:1163-1164`), and it
        // reads the newer build's `display` vocabulary (`:1155-1160`) — see
        // the module header. The open/close balance is still enforced.
        assert_eq!(render("<div height='4'>x</div>", false), "<div height='0'>x</div>|");
        assert_eq!(
            render("<div xpos='0' ypos='-400'>x</div>", false),
            "<div ypos='-72'>x</div>|",
            "a zero contributes nothing to the rebuilt tag (`Utils.cs:140-147`)"
        );
        assert_eq!(
            render("<div><div>x</div></div>", false),
            "<div><div>x</div></div>|"
        );
        assert_eq!(
            render("<div display='absolute-leftbottom'>x</div>", false),
            "<div>x</div>|",
            "`display` is not part of the rebuilt tag in Emuera either"
        );
        assert_eq!(
            render("<div ypps='4' img_size='9'>x</div>", false),
            "<div>x</div>|",
            "unknown names are ignored, so the source typos survive"
        );
        assert_eq!(
            render("<button value='1'><div>x</div></button>", false),
            "<div>x</div>|",
            "`Data/ERB/ＳＨＯＰ関連/120_ショップ.ERB:49` opens a box inside a button"
        );
        assert_eq!(reject("</div>"), "</div>の前に<div>がありません");
        assert_eq!(reject("<div>x"), "閉じられていないタグがあります");
        assert_eq!(reject("<div display='oblique'>x</div>"), "属性値obliqueは解釈できません");

        // `<button>`. `:1261-1305`.
        assert_eq!(
            reject("<button value='1'><button value='2'>x</button></button>"),
            "<button>又は<nonbutton>が入れ子にされています"
        );
        assert_eq!(
            reject("<nonbutton value='1'>x</nonbutton>"),
            "<nonbutton>タグにvalue属性が設定されていません"
        );
        assert_eq!(
            reject("<button value='1' value='2'>x</button>"),
            "<button>タグにvalue属性が2度以上指定されています"
        );
        assert_eq!(
            reject("<button src='x'>y</button>"),
            "<button>タグの属性名srcは解釈できません"
        );
        // `pos` needs `<nobr>` (`:603-613`).
        assert_eq!(
            reject("<button pos='4'>x</button>"),
            "<nobr>が設定されていない行ではpos属性は使用できません"
        );
        // Its companion check — `alignがleftでない行ではpos属性は使用できません` —
        // is unreachable in Emuera too: `<p>` is the only way to change the
        // alignment and it refuses to open once `FlagNobr` is set (`:973-974`),
        // while `pos` refuses to parse unless it is. The state starts at
        // `DisplayLineAlignment.LEFT` (`:236`), so the two can never meet.
        assert_eq!(
            reject("<nobr><p align='center'>x</p>"),
            "<p>が二重に使われています"
        );

        // `<clearbutton>` and `<font>`. `:1344-1418`.
        assert_eq!(
            reject("<clearbutton notooltip='maybe'>x</clearbutton>"),
            "<clearbutton>タグのnotooltip属性の属性値maybeは数値として解釈できません"
        );
        assert_eq!(reject("<font>x</font>"), "<font>タグに属性が設定されていません");
        assert_eq!(
            reject("<font size='4'>x</font>"),
            "<font>タグの属性名sizeは解釈できません"
        );
        assert_eq!(
            reject("<font color='red' color='blue'>x</font>"),
            "<font>タグにcolor属性が2度以上指定されています"
        );
        assert_eq!(
            reject("<font color='notacolour'>x</font>"),
            "指定された色名\"notacolour\"は無効な色名です"
        );

        // Unknown open tag and malformed markup. `:1435-1441`, `:520-521`.
        assert_eq!(
            reject("<marquee>x</marquee>"),
            "html文字列\"<marquee>x</marquee>\"のタグ解析中にエラーが発生しました"
        );
        // An unquoted attribute value is a lex failure in Emuera.
        assert_eq!(
            reject("<font color=red>x</font>"),
            "html文字列\"<font color=red>x</font>\"のタグ解析中にエラーが発生しました"
        );
        assert_eq!(reject("<b"), "タグ終端'>'が見つかりません");
        assert_eq!(reject("x<!-- unclosed"), "コメント終了タグ\"-->\"がみつかりません");

        // Unclosed tags at the end of the string. `:592-596`.
        assert_eq!(reject("<b>x"), "閉じられていないタグがあります");
        assert_eq!(reject("<font color='red'>x"), "閉じられていないタグがあります");
        assert_eq!(reject("<button value='1'>x"), "閉じられていないタグがあります");
        assert_eq!(
            reject("<div width='4' height='4'>x"),
            "閉じられていないタグがあります"
        );

        // Text after a close tag, but only when it runs to the end of the
        // string — Emuera only checks there (`:486-492`).
        assert_eq!(reject("<p align='left'>x</p>y"), "</p>の後にテキストがあります");
        assert_eq!(reject("<nobr>x</nobr>y"), "</nobr>の後にテキストがあります");

        // Entities. `:685-727`.
        assert_eq!(reject("&nosemi"), "'&'に対応する';'がみつかりません");
        assert_eq!(reject("&;"), "'&'と';'が連続しています");
        assert_eq!(reject("&bogus;"), "\"&bogus;\"は適切な文字参照ではありません");
        assert_eq!(
            reject("&#x10000;"),
            "\"&#x10000;\"はUnicodeの範囲外です(サロゲートペアは使えません)"
        );
    }

    /// The one `<div>` in the parts of `s`, which every box test below is
    /// about.
    fn div(s: &str) -> std::sync::Arc<erars_ui::ConsoleDiv> {
        let mut tx = console();
        html_print(s, true, &mut tx, res()).unwrap();
        let parts = &tx.last_line.parts;
        match parts.iter().find(|p| matches!(p, ConsoleLinePart::Div(_))) {
            Some(ConsoleLinePart::Div(div)) => div.clone(),
            _ => panic!("{s:?} produced no box, only {parts:?}"),
        }
    }

    /// The `ConsoleDivPart` constructor resolves every `MixedNum` against the
    /// font size (`_Library/EvilMask/ConsoleDivPart.cs:49-64`,
    /// `Utils.cs:19-22`), so a bare number is a percentage of it and `px` is
    /// literal.
    #[test]
    fn box_geometry_resolves_against_the_font_size() {
        let d = div("<div xpos='200' ypos='-2900' width='50px' height='300'></div>");
        assert_eq!((d.x, d.y), (2 * FS, -29 * FS));
        assert_eq!(d.width, Some(50));
        assert_eq!(d.height, Some(3 * FS as u32));

        // `width.num = Math.Abs(width.num)` (`:20-21`) — an extent is never
        // negative, in the box or in the rebuilt tag.
        let d = div("<div width='-40px' height='-40px'></div>");
        assert_eq!((d.width, d.height), (Some(40), Some(40)));
        assert_eq!(d.alt_head, "<div width='40px' height='40px'>");

        // A missing extent is unbounded, not zero: 186 of eramegaten_p_kr's
        // 369 boxes have neither.
        let d = div("<div xpos='100'></div>");
        assert_eq!((d.width, d.height), (None, None));
        assert_eq!(d.inner_width(), None);
    }

    /// `display` (`HtmlManager.cs:1155-1160`) and the two spellings the newer
    /// EvilMask build uses.
    #[test]
    fn box_anchor_follows_display() {
        assert_eq!(div("<div></div>").anchor, DivAnchor::Relative);
        assert_eq!(div("<div display='relative'></div>").anchor, DivAnchor::Relative);
        assert_eq!(
            div("<div display='absolute-lefttop'></div>").anchor,
            DivAnchor::LeftTop
        );
        assert_eq!(
            div("<div display='ABSOLUTE-LEFTBOTTOM'></div>").anchor,
            DivAnchor::LeftBottom,
            "the value comparison is case-insensitive (`:1157-1159`)"
        );
        assert_eq!(
            div("<div display='absolute'></div>").anchor,
            DivAnchor::LeftBottom,
            "the fork's own spelling maps onto the bottom anchor, see `parse_anchor`"
        );
    }

    /// `Utils.ParseParam4MixedNum` (`Utils.cs:58-88`) in Emuera's edge order
    /// (`Shape.cs:14`: top, right, bottom, left), through the newer build's
    /// attribute spellings.
    #[test]
    fn box_model_uses_the_css_shorthand() {
        let d = div("<div padding='1px' border='2px,3px' margin='4px,5px,6px'></div>");
        assert_eq!(d.style.padding, [1, 1, 1, 1]);
        assert_eq!(d.style.border, [2, 3, 2, 3]);
        assert_eq!(d.style.margin, [4, 5, 6, 5]);
        // Left edges: padding 1, border 3 (the `2px,3px` pair mirrors), margin 5.
        assert_eq!(d.style.content_offset(), (1 + 3 + 5, 1 + 2 + 4));

        // Percentages resolve, and `border_width` / `background_color` /
        // `border_color` are the newer build's names for `border` / `color` /
        // `bcolor`.
        let d = div(
            "<div border_width='100' background_color='#102030' border_color='red,#00FF00'></div>",
        );
        assert_eq!(d.style.border, [FS; 4]);
        assert_eq!(d.style.background, Some(Color([0x10, 0x20, 0x30])));
        assert_eq!(
            d.style.border_color,
            [
                Some(Color([255, 0, 0])),
                Some(Color([0, 255, 0])),
                Some(Color([255, 0, 0])),
                Some(Color([0, 255, 0])),
            ]
        );
        assert!(d.style.is_painted());

        // A box with no decoration paints nothing, so the front-end can skip
        // it entirely.
        assert!(!div("<div width='4'></div>").style.is_painted());
    }

    /// `size='w,h'` and `rect='x,y,w,h'` fill the same slots as the single
    /// attributes (`HtmlManager.cs:1121-1153`), so they collide with them.
    #[test]
    fn box_size_and_rect_shorthands() {
        let d = div("<div size='10px, 20px'></div>");
        assert_eq!((d.width, d.height), (Some(10), Some(20)));

        let d = div("<div rect='1px,2px,3px,4px'></div>");
        assert_eq!((d.x, d.y, d.width, d.height), (1, 2, Some(3), Some(4)));

        assert_eq!(
            reject("<div width='1px' size='2px,3px'></div>"),
            "<div>タグにwidth属性が2度以上指定されています"
        );
        assert_eq!(
            reject("<div size='2px'></div>"),
            "<div>タグのsize属性の属性値2pxが数値として解釈できません"
        );
    }

    /// The box's content is its own line list (`ConsoleDivPart.cs:88`), and
    /// the box itself takes no width on the line it was printed on
    /// (`:47`, `:172-174`) — which is why eramegaten_p_kr reserves blank lines
    /// and then lifts a picture into them with a negative `ypos`.
    #[test]
    fn box_content_is_out_of_the_line_flow() {
        let mut tx = console();
        html_print("a<div ypos='-100'>in<br>side</div>b", true, &mut tx, res()).unwrap();

        let outer: Vec<String> = tx.last_line.parts.iter().map(|p| p.to_string()).collect();
        assert_eq!(
            outer,
            vec!["a", "<div ypos='-18'>in<br>side</div>", "b"],
            "the text before and after the box stays on the one line"
        );
        assert_eq!(tx.lines_from(0).len(), 0, "no line break escaped the box");

        let d = div("a<div ypos='-100'>in<br>side</div>b");
        assert_eq!(d.lines.len(), 2, "`<br>` inside the box breaks the box's line");
        assert_eq!(d.lines[0].to_string(), "in");
        assert_eq!(d.lines[1].to_string(), "side");

        // An `<img>` inside the box lands in the box, not on the log line.
        let store = store_with_pic();
        let tx = print_html(&store, "<div><img src='PIC'></div>");
        let Some(ConsoleLinePart::Div(d)) = tx.last_line.parts.first() else {
            panic!("no box");
        };
        assert!(
            matches!(d.lines[0].parts.first(), Some(ConsoleLinePart::Image(_))),
            "the picture is the box's content: {:?}",
            d.lines[0].parts
        );
    }

    /// Boxes nest — `Data/ERB/関数/汎用組み込み関数/DIV_メッセージウィンドウ/DIV_MESSAGE_LOG.ERB:61-71`
    /// nests three deep — and each box's content is inside its own parent.
    #[test]
    fn boxes_nest() {
        let outer = div("<div xpos='100'>a<div ypos='-100'>b</div></div>");
        assert_eq!(outer.x, FS);
        assert_eq!(outer.lines.len(), 1);

        let parts = &outer.lines[0].parts;
        let Some(ConsoleLinePart::Div(inner)) = parts.get(1) else {
            panic!("the inner box is a part of the outer box's line: {parts:?}");
        };
        assert_eq!((inner.x, inner.y), (0, -FS));
        assert_eq!(inner.lines[0].to_string(), "b");
    }

    /// A box inside a `<button>` inherits the button, so its content is
    /// clickable with the button's value — which is what
    /// `Data/ERB/関数/汎用組み込み関数/メッセージ/MESSAGE_POPUP.ERB:35` relies on.
    #[test]
    fn box_content_inherits_the_enclosing_button() {
        let d = div("<button value='42'><div>hit</div></button>");
        let parts = &d.lines[0].parts;
        match parts.first() {
            Some(ConsoleLinePart::Button(runs, _, value)) => {
                assert_eq!(runs[0].0, "hit");
                assert_eq!(*value, erars_ast::Value::Int(42));
            }
            other => panic!("the box's content is not clickable: {other:?}"),
        }
    }

    /// HTML_GETPRINTEDSTR drops the box: `DisplayLine2Html` has a branch for
    /// `ConsoleStyledString`, `ConsoleImagePart` and `ConsoleShapePart` and
    /// none for `ConsoleDivPart` (`HtmlManager.cs:352-377`).
    #[test]
    fn a_box_contributes_nothing_to_the_html_round_trip() {
        let mut tx = console();
        html_print("a<div ypos='-100'>in</div>b", true, &mut tx, res()).unwrap();
        assert_eq!(
            super::line_to_html(&tx.last_line, false, Color([192, 192, 192]), ""),
            "ab"
        );
    }
}
