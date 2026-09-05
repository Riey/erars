use cow_utils::CowUtils;
use std::borrow::Cow;

use super::ParserContext;
use erars_ast::{
    var_name_alias, Alignment, BinaryOperator, BuiltinCommand, BuiltinMethod, Expr, FormText,
    InlineValue, NotNan, SelectCaseCond, Stmt, StrKey, UnaryOperator, Variable,
    VariableInfo,
};
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while1},
    character::complete::*,
    combinator::{cut, eof, map, opt, value},
    error::{context, ErrorKind, ParseError},
    error_position,
    multi::{many0, separated_list0, separated_list1},
    number::complete::float,
    sequence::{delimited, pair, preceded, terminated, tuple},
    Parser,
};

/// Expression parser error.
///
/// `nom::error::VerboseError` allocated a `Vec` for *every* failed branch of
/// every `alt`, and `context()` pushed onto it while unwinding. `single_expr`
/// is a 14-way `alt`, so an ordinary identifier paid a dozen heap allocations;
/// that dominated parse time (measured 2x on the eraTHYMKR corpus).
///
/// This keeps the same diagnostic content — the failure position plus the
/// innermost `context()` label — in a `Copy` struct with no allocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExprError<'a> {
    /// Remaining input where the failure was reported.
    input: &'a str,
    kind: ErrorKind,
    expected: Option<char>,
    context: Option<&'static str>,
}

impl<'a> ExprError<'a> {
    /// The error that got the furthest into the input wins; `alt` branches all
    /// start from the same buffer, so remaining length orders them.
    fn deeper(self, other: Self) -> Self {
        if other.input.len() < self.input.len() {
            other
        } else {
            self
        }
    }

    /// Attach a context label unless an inner one already claimed it.
    fn with_context(self, ctx: &'static str) -> Self {
        Self {
            context: self.context.or(Some(ctx)),
            ..self
        }
    }

    /// Re-point at a different buffer, for errors raised while parsing an
    /// expanded macro body that does not outlive the caller.
    fn relocate<'b>(self, input: &'b str) -> ExprError<'b> {
        ExprError {
            input,
            kind: self.kind,
            expected: self.expected,
            context: self.context,
        }
    }
}

impl<'a> ParseError<&'a str> for ExprError<'a> {
    fn from_error_kind(input: &'a str, kind: ErrorKind) -> Self {
        Self {
            input,
            kind,
            expected: None,
            context: None,
        }
    }

    /// Keep the inner error: it points at the actual failure site.
    fn append(_input: &'a str, _kind: ErrorKind, other: Self) -> Self {
        other
    }

    fn from_char(input: &'a str, ch: char) -> Self {
        Self {
            input,
            kind: ErrorKind::Char,
            expected: Some(ch),
            context: None,
        }
    }

    fn or(self, other: Self) -> Self {
        self.deeper(other)
    }
}

impl<'a> nom::error::ContextError<&'a str> for ExprError<'a> {
    fn add_context(_input: &'a str, ctx: &'static str, other: Self) -> Self {
        Self {
            // innermost context wins
            context: other.context.or(Some(ctx)),
            ..other
        }
    }
}

impl std::fmt::Display for ExprError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const MAX: usize = 40;
        let rest = match self.input.char_indices().nth(MAX) {
            Some((idx, _)) => &self.input[..idx],
            None => self.input,
        };

        match (self.context, self.expected) {
            (Some(ctx), Some(ch)) => write!(f, "in {ctx}, expected '{ch}'"),
            (Some(ctx), None) => write!(f, "in {ctx}, {:?}", self.kind),
            (None, Some(ch)) => write!(f, "expected '{ch}'"),
            (None, None) => write!(f, "{:?}", self.kind),
        }?;

        if rest.is_empty() {
            write!(f, " at end of line")
        } else {
            write!(f, " at `{rest}`")
        }
    }
}

type Error<'a> = ExprError<'a>;
type IResult<'a, T> = nom::IResult<&'a str, T, Error<'a>>;

/// Skip the whitespace Emuera ignores between tokens.
///
/// Space, tab, CR, and U+3000 IDEOGRAPHIC SPACE — the last counts as it does in
/// Emuera whenever `SystemAllowFullSpace` is on (`Sub/LexicalAnalyzer.cs:749-752`,
/// `GameProc/LogicalLineParser.cs:428-431`), which it is by default
/// (`Config/ConfigData.cs:112`).
///
/// The two ways to spell this before were `nom::take_while(is_sp)`, which walks
/// `char_indices` and decodes a `char` per byte to hand to the predicate, and
/// `trim_start_matches(SP)`, whose pattern is a *function pointer* and so
/// cannot inline the predicate at all. Between them the profile charged ~4% of
/// parse+compile self time to skipping spaces. Every character accepted here
/// is either ASCII or U+3000, whose lead byte `0xE3` it shares with all of
/// hiragana and katakana, so its two continuation bytes decide — the same test
/// [`erars_lexer::utils::cut_ident`] makes.
fn skip_sp(i: &str) -> &str {
    let bytes = i.as_bytes();
    let mut pos = 0;

    while let Some(&b) = bytes.get(pos) {
        match b {
            b' ' | b'\t' | b'\r' => pos += 1,
            // U+3000 is `E3 80 80`; no other character starts with those bytes.
            0xE3 if bytes[pos + 1..].starts_with(b"\x80\x80") => pos += 3,
            _ => break,
        }
    }

    // `pos` only ever advances over whole characters.
    unsafe { i.get_unchecked(pos..) }
}

fn sp<'a>(i: &'a str) -> IResult<'a, ()> {
    Ok((skip_sp(i), ()))
}

// fn sp_nl<'a>(i: &'a str) -> IResult<'a, ()> {
//     map(take_while(move |c| " \t\r\n".contains(c)), |_| ())(i)
// }

fn char_sp<'a>(ch: char) -> impl FnMut(&'a str) -> IResult<'a, char> {
    delimited(sp, char(ch), sp)
}

fn de_char_sp<'a, T>(
    first: char,
    p: impl Parser<&'a str, T, Error<'a>>,
    last: char,
) -> impl FnMut(&'a str) -> IResult<'a, T> {
    delimited(char_sp(first), p, char_sp(last))
}

fn de_sp<'a, T>(p: impl Parser<&'a str, T, Error<'a>>) -> impl FnMut(&'a str) -> IResult<'a, T> {
    delimited(sp, p, sp)
}

fn cut_delimited<'a, T>(
    pre: &'a str,
    content: impl Parser<&'a str, T, Error<'a>>,
    end: &'a str,
) -> impl FnMut(&'a str) -> IResult<'a, T> {
    preceded(tag(pre), cut(terminated(content, tag(end))))
}

pub fn ident<'a>(i: &'a str) -> IResult<'a, &'a str> {
    if i.as_bytes().first().is_some_and(u8::is_ascii_digit) {
        return Err(nom::Err::Error(error_position!(i, ErrorKind::AlphaNumeric)));
    }

    // Same predicate as `take_while1(is_ident_body)`, scanned a byte at a time
    // rather than a decoded `char` at a time: 2.1% of parse+compile self time
    // went to nom's `char_indices` walk over this one predicate.
    let (ident, rest) = erars_lexer::utils::cut_ident(i);
    if ident.is_empty() {
        Err(nom::Err::Error(nom::error::make_error(i, ErrorKind::TakeWhile1)))
    } else {
        Ok((rest, ident))
    }
}

pub fn ident_no_case<'a>(i: &'a str) -> IResult<'a, Cow<'a, str>> {
    map(ident, upper_no_case)(i)
}

/// Uppercase `s` the way `str::to_uppercase` would, without copying when it
/// would not change anything.
///
/// `cow_to_uppercase` asks `char::to_uppercase` about every character, so it
/// decodes each `char` and builds a `CaseMappingIter` over the Unicode case
/// tables; that was ~4.5% of parse self time in the profile
/// (`CaseMappingIter::new`, `ptr::read::<char>`, `encode_utf8_raw`).
///
/// Nearly every identifier in an ERB is already ASCII uppercase, and non-ASCII
/// identifiers in Japanese and Korean games are dominated by CJK Ideographs
/// and Hangul syllables, none of which have uppercase variants.
///
/// `CASED_BYTE_TABLE` flags ASCII lowercase bytes and all UTF-8 lead bytes
/// that can start any Unicode character where `c.to_uppercase() != c`.
/// UTF-8 lead byte `0xEA` is specially checked: Hangul syllables
/// (U+AC00..U+D7A3, starting with `0xEA 0xB0`) have second byte >= 0xB0 and
/// are uncased, so only `0xEA` followed by `< 0xB0` triggers the uppercase pass.
const CASED_BYTE_TABLE: [bool; 256] = {
    let mut table = [false; 256];
    let mut b = b'a';
    while b <= b'z' {
        table[b as usize] = true;
        b += 1;
    }
    // All UTF-8 lead bytes that can start any Unicode character where c.to_uppercase() != c,
    // except 0xEA which is handled specially to avoid triggering on Hangul syllables (>= 0xB0).
    let leads: [u8; 23] = [
        0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcd, 0xce, 0xcf,
        0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xe1, 0xe2, 0xef, 0xf0,
    ];
    let mut i = 0;
    while i < leads.len() {
        table[leads[i] as usize] = true;
        i += 1;
    }
    table
};

fn upper_no_case(s: &str) -> Cow<'_, str> {
    let bytes = s.as_bytes();
    let mut i = 0;
    let mut has_cased = false;
    while i < bytes.len() {
        let b = bytes[i];
        if CASED_BYTE_TABLE[b as usize] {
            has_cased = true;
            break;
        }
        if b == 0xEA {
            // In UTF-8, characters starting with 0xEA are cased only up to U+ABBF (second byte <= 0xAE).
            // All Hangul syllables (U+AC00..U+D7A3, starting at 0xEA 0xB0 0x80) have second byte >= 0xB0
            // and are uncased.
            if let Some(&next) = bytes.get(i + 1) {
                if next < 0xB0 {
                    has_cased = true;
                    break;
                }
            }
        }
        i += 1;
    }
    if has_cased {
        s.cow_to_uppercase()
    } else {
        Cow::Borrowed(s)
    }
}

/// Text of a `"…"` literal, with `\n`/`\t`/`\x` unescaped.
///
/// Returns a slice of `i` when there is no escape to apply, which is almost
/// always: the profile of serial parse+compile put ~10% of self time in
/// `RawVec::reserve` / `alloc::alloc` / `mi_free`, and every literal used to
/// be copied char-by-char into a fresh `String` that was interned (hash +
/// copy again) and immediately dropped.
///
/// The scan finds the next `\` or `"` with `memchr2` and takes the whole run
/// in between at once, rather than decoding a `char` per byte: see
/// [`parse_form_normal_str`], which has the same shape and is much hotter.
fn parse_str_inner<'a>(i: &'a str) -> IResult<'a, Cow<'a, str>> {
    let bytes = i.as_bytes();
    // Start of the run of ordinary text not yet moved into `owned`. While
    // `owned` is None this stays 0, so the whole text is still a slice of `i`.
    let mut run_start = 0;
    // Allocated only once an escape makes the output differ from `i`.
    let mut owned: Option<String> = None;

    let mut pos = 0;
    loop {
        let at = match memchr::memchr2(b'\\', b'"', &bytes[pos..]) {
            Some(off) => pos + off,
            // Unterminated literal: same error the char-by-char loop raised
            // when it ran off the end.
            None => return Err(nom::Err::Error(error_position!(i, ErrorKind::Escaped))),
        };

        if bytes[at] == b'"' {
            let text = match owned {
                Some(mut buf) => {
                    buf.push_str(&i[run_start..at]);
                    Cow::Owned(buf)
                }
                None => Cow::Borrowed(&i[..at]),
            };
            break Ok((&i[at + 1..], text));
        }

        let src = match i[at + 1..].chars().next() {
            Some(ch) => ch,
            None => return Err(nom::Err::Error(error_position!(i, ErrorKind::Escaped))),
        };
        let unescaped = match src {
            'n' => '\n',
            't' => '\t',
            ch => ch,
        };
        let buf = owned.get_or_insert_with(String::new);
        buf.push_str(&i[run_start..at]);
        buf.push(unescaped);
        pos = at + '\\'.len_utf8() + src.len_utf8();
        run_start = pos;
    }
}

fn alignment<'a>(i: &'a str) -> IResult<'a, Alignment> {
    alt((
        value(Alignment::Left, tag_no_case("LEFT")),
        value(Alignment::Center, tag_no_case("CENTER")),
        value(Alignment::Right, tag_no_case("RIGHT")),
    ))(i)
}

#[derive(Debug)]
pub enum FormType {
    Percent,
    Brace,
    At,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FormStrType {
    Normal,
    /// no #
    FirstCond,
    /// no \\@
    SecondCond,
    /// no comma, blank
    Arg,
    /// no comma, blank, paran
    CallArg,
    /// no "
    Str,
}

/// Offset of the first byte in `h` that ends a literal run of `ty`.
///
/// Every byte that can end a run is ASCII, so scanning raw bytes can never
/// match inside a multi-byte character — all of its bytes are >= 0x80 — and
/// the offset returned is always a UTF-8 boundary.
fn find_form_delim(h: &[u8], ty: FormStrType) -> Option<usize> {
    // `%`, `{` and `\` end a run in every context, and memchr3 vectorises
    // them; a type-specific delimiter can only win if it comes earlier, so
    // the second pass is bounded by the first hit.
    let common = memchr::memchr3(b'%', b'{', b'\\', h);
    let head = &h[..common.unwrap_or(h.len())];
    let extra = match ty {
        FormStrType::Normal | FormStrType::SecondCond => None,
        FormStrType::FirstCond => memchr::memchr(b'#', head),
        FormStrType::Arg => memchr::memchr(b',', head),
        FormStrType::CallArg => memchr::memchr2(b'(', b',', head),
        FormStrType::Str => memchr::memchr(b'"', head),
    };

    extra.or(common)
}

/// The literal text in front of the next interpolation, plus which kind of
/// interpolation terminated it.
///
/// Returns a slice of `i` unless a `\x` escape has to be applied. Form strings
/// are the bulk of an ERB, so the fresh `String` per segment was a large part
/// of the ~10% of parse self time that the profile attributed to
/// `RawVec::reserve` / `alloc::alloc` / `mi_free`.
///
/// The scan takes a whole run of ordinary text at a time. Decoding a `char`
/// per byte and asking `Option<String>::as_mut` whether to copy it was 6.5% of
/// parse self time (`next_code_point` 3.63% + `Option::as_mut` 2.91%) once the
/// allocation itself was gone; the delimiters are all ASCII, so `memchr` can
/// find the end of a run without decoding anything.
fn parse_form_normal_str<'a>(
    ty: FormStrType,
) -> impl Fn(&'a str) -> IResult<'a, (Cow<'a, str>, Option<FormType>)> {
    move |i: &'a str| {
        let bytes = i.as_bytes();
        // Start of the run of ordinary text not yet moved into `owned`. While
        // `owned` is None this stays 0, so the text is still a slice of `i`.
        let mut run_start = 0;
        // Allocated only once an escape makes the output differ from `i`.
        let mut owned: Option<String> = None;

        let mut pos = 0;
        // `text_end` is where the literal text stops, `rest` is where the
        // remaining input starts: they differ by the delimiter, which is
        // consumed for `%`/`{`/`\@` but left in place for the others.
        let (form_ty, text_end, rest) = loop {
            let at = match find_form_delim(&bytes[pos..], ty) {
                Some(off) => pos + off,
                None => break (None, bytes.len(), bytes.len()),
            };

            match bytes[at] {
                b'\\' => match bytes.get(at + 1) {
                    Some(b'@') => {
                        if ty == FormStrType::SecondCond || ty == FormStrType::FirstCond {
                            break (None, at, at);
                        } else {
                            break (Some(FormType::At), at, at + "\\@".len());
                        }
                    }
                    Some(_) => {
                        // The backslash is dropped, so the output stops being
                        // a slice of the input here.
                        let escape = i[at + 1..].chars().next().unwrap();
                        let buf = owned.get_or_insert_with(String::new);
                        buf.push_str(&i[run_start..at]);
                        buf.push(escape);
                        pos = at + '\\'.len_utf8() + escape.len_utf8();
                        run_start = pos;
                    }
                    None => {
                        return Err(nom::Err::Error(nom::error::make_error(
                            &i[at..],
                            nom::error::ErrorKind::EscapedTransform,
                        )));
                    }
                },
                b'%' => break (Some(FormType::Percent), at, at + '%'.len_utf8()),
                b'{' => break (Some(FormType::Brace), at, at + '{'.len_utf8()),
                // `#`, `(`, `,` and `"` are only in the delimiter set of the
                // `FormStrType` that stops on them, so reaching one here means
                // the segment ends and the delimiter stays for the caller.
                _ => break (None, at, at),
            }
        };

        let mut text = match owned {
            Some(mut buf) => {
                buf.push_str(&i[run_start..text_end]);
                Cow::Owned(buf)
            }
            None => Cow::Borrowed(&i[..text_end]),
        };

        if matches!(ty, FormStrType::FirstCond | FormStrType::SecondCond) {
            match &mut text {
                Cow::Borrowed(s) => *s = s.trim_end_matches(' '),
                Cow::Owned(s) => s.truncate(s.trim_end_matches(' ').len()),
            }
        }

        Ok((&i[rest..], (text, form_ty)))
    }
}

pub fn normal_form_str<'c>(
    ctx: &'c ParserContext,
) -> impl for<'a> FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| form_str(FormStrType::Normal, ctx)(i)
}

fn form_str_cond_form<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let (i, cond) = bin_expr(ctx)(i)?;
        let (i, _) = de_sp(tag("?"))(i)?;
        let (i, if_true) = form_str(FormStrType::FirstCond, ctx)(i)?;

        if let Some(i) = i.strip_prefix('#') {
            let (i, or_false) = preceded(sp, form_str(FormStrType::SecondCond, ctx))(i)?;
            let i = i.strip_prefix("\\@").unwrap();
            Ok((i, Expr::cond(cond, if_true, or_false)))
        } else if let Some(i) = i.strip_prefix("\\@") {
            Ok((i, Expr::cond(cond, if_true, Expr::str(""))))
        } else {
            unreachable!()
        }
    }
}

pub fn form_str<'c, 'a>(
    ty: FormStrType,
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i: &'a str| {
        let is_arg_init_value = ctx.is_arg.get();
        ctx.is_arg.set(false);
        let normal_str = parse_form_normal_str(ty);
        let (mut i, (normal, mut ty)) = normal_str(i)?;

        let mut form = FormText::new(erars_ast::intern_literal(&normal));

        loop {
            let (left, expr, padding, align) = match ty {
                Some(FormType::Percent) => {
                    let ban_percent = ctx.ban_percent.get();
                    ctx.ban_percent.set(true);
                    let (i, ex) = expr(ctx)(i)?;
                    let (i, padding) = opt(preceded(char_sp(','), opt(expr(ctx))))(i)?;
                    let (i, align) = if padding.is_some() {
                        opt(preceded(char_sp(','), alignment))(i)?
                    } else {
                        (i, None)
                    };
                    let padding = padding.flatten();
                    let (i, _) = opt(many0(char_sp(',')))(i)?;
                    let (i, _) = preceded(sp, char('%'))(i)?;
                    ctx.ban_percent.set(ban_percent);

                    (i, ex, padding, align)
                }
                Some(FormType::Brace) => {
                    let ban_percent = ctx.ban_percent.get();
                    ctx.ban_percent.set(false);
                    let (i, ex) = expr(ctx)(i)?;
                    let (i, padding) = opt(preceded(char_sp(','), opt(expr(ctx))))(i)?;
                    let (i, align) = if padding.is_some() {
                        opt(preceded(char_sp(','), alignment))(i)?
                    } else {
                        (i, None)
                    };
                    let padding = padding.flatten();
                    let (i, _) = opt(many0(char_sp(',')))(i)?;
                    let (i, _) = preceded(sp, char('}'))(i)?;
                    ctx.ban_percent.set(ban_percent);

                    (i, ex, padding, align)
                }
                Some(FormType::At) => {
                    let (i, cond) = form_str_cond_form(ctx)(i)?;
                    (i, cond, None, None)
                }
                None => break,
            };

            i = left;

            let (left, (normal, next_ty)) = normal_str(i)?;
            i = left;
            ty = next_ty;

            form.push(expr, padding, align, erars_ast::intern_literal(&normal));
        }

        ctx.is_arg.set(is_arg_init_value);

        Ok((i, Expr::FormText(form)))
    }
}

fn string<'a>(i: &'a str) -> IResult<'a, Cow<'a, str>> {
    context("string", preceded(char('\"'), parse_str_inner))(i)
}

fn paran_expr<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let is_arg = ctx.is_arg.get();
        let ban_percent = ctx.ban_percent.get();
        ctx.is_arg.set(false);
        ctx.ban_percent.set(false);

        let (i, expr) = de_char_sp('(', expr(ctx), ')')(i)?;

        ctx.is_arg.set(is_arg);
        ctx.ban_percent.set(ban_percent);

        Ok((i, expr))
    }
}

pub fn var_func_extern<'a>(ctx: &ParserContext, i: &'a str) -> IResult<'a, Option<StrKey>> {
    opt(map(preceded(char('@'), ident_no_case), |s| {
        ctx.intern_ident(&s)
    }))(i)
}

fn ident_or_method_expr<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let (i, ident) = ident_no_case(i)?;
        let ident = ctx.replace(&ident);
        let i = i.trim_start_matches(' ');

        if let Some(i) = i.strip_prefix('(') {
            let p = ctx.ban_percent.get();
            let a = ctx.is_arg.get();
            ctx.ban_percent.set(false);
            ctx.is_arg.set(false);
            let (i, args) = terminated(expr_list(ctx), char_sp(')'))(i)?;
            ctx.ban_percent.set(p);
            ctx.is_arg.set(a);

            match ident.parse() {
                Ok(meth) => Ok((i, Expr::BuiltinMethod(meth, args))),
                _ => Ok((i, Expr::Method(ctx.intern_ident(&ident), args))),
            }
        } else {
            let (i, func_extern) = var_func_extern(ctx, i)?;
            match ident {
                Cow::Borrowed(ident) => {
                    let var = ident;
                    if !ctx.is_arg.get() {
                        let (i, args) = variable_arg(ctx, var)(i)?;

                        if let Ok(var) = var.parse() {
                            Ok((i, Expr::BuiltinVar(var, args)))
                        } else {
                            Ok((
                                i,
                                Expr::Var(Variable {
                                    var: ctx.intern_ident(var),
                                    func_extern,
                                    args,
                                }),
                            ))
                        }
                    } else {
                        #[allow(clippy::collapsible_else_if)]
                        if let Ok(var) = var.parse() {
                            Ok((i, Expr::BuiltinVar(var, Vec::new())))
                        } else {
                            Ok((
                                i,
                                Expr::Var(Variable {
                                    var: ctx.intern_ident(var),
                                    func_extern,
                                    args: Vec::new(),
                                }),
                            ))
                        }
                    }
                }
                Cow::Owned(m) => {
                    if !ctx.is_arg.get() && erars_lexer::utils::is_ident(&m) {
                        let (i, args) = variable_arg(ctx, &m)(i)?;
                        if let Ok(var) = m.parse() {
                            Ok((i, Expr::BuiltinVar(var, args)))
                        } else {
                            Ok((
                                i,
                                Expr::Var(Variable {
                                    var: ctx.intern_ident(&m),
                                    func_extern,
                                    args,
                                }),
                            ))
                        }
                    } else {
                        match expr(ctx)(&m) {
                            Ok((left, expr)) => {
                                if !left.is_empty() {
                                    log::error!("Macro must be complete form");
                                    Err(nom::Err::Failure(error_position!(i, ErrorKind::Eof)))
                                } else {
                                    Ok((i, expr))
                                }
                            }
                            Err(err) => Err(err.map(|e| e.relocate(i))),
                        }
                    }
                }
            }
        }
    }
}

/// ASCII-case-insensitive `strip_prefix`. `nom::bytes::complete::tag_no_case`
/// decodes and case-folds char by char through `char::to_lowercase`, which the
/// profile showed costing more than the branches it guards.
fn strip_prefix_no_case<'a>(i: &'a str, prefix: &str) -> Option<&'a str> {
    let n = prefix.len();
    // `prefix` is ASCII, so `n` is a char boundary whenever the bytes match.
    (i.len() >= n && i.as_bytes()[..n].eq_ignore_ascii_case(prefix.as_bytes())).then(|| &i[n..])
}

/// The leaf of an expression, dispatched on the first byte.
///
/// This was a 14-branch `alt`, so an identifier — the common case — paid a
/// dozen failed parses first, four of them `tag_no_case`. Branch order within
/// each leading byte is the old `alt` order, so the accepted language is
/// unchanged; only the reported error kind on malformed input differs.
fn single_expr_atom<'c, 'a>(ctx: &'c ParserContext, i: &'a str) -> IResult<'a, Expr> {
    let bytes = i.as_bytes();

    // `0x`/`0o`/`0b` literals, then `<int>p<int>`, `1E<exp>`, plain integer.
    let number = |i: &'a str| -> IResult<'a, Expr> {
        if let Some(rest) = strip_prefix_no_case(i, "0x") {
            if let Ok((rest, digits)) = hex_digit1::<_, Error>(rest) {
                return Ok((rest, Expr::Int(i64::from_str_radix(digits, 16).unwrap())));
            }
        } else if let Some(rest) = strip_prefix_no_case(i, "0o") {
            if let Ok((rest, digits)) = oct_digit1::<_, Error>(rest) {
                return Ok((rest, Expr::Int(i64::from_str_radix(digits, 8).unwrap())));
            }
        } else if let Some(rest) = strip_prefix_no_case(i, "0b") {
            if let Ok((rest, digits)) = take_while1::<_, _, Error>(|c| matches!(c, '0' | '1'))(rest)
            {
                return Ok((rest, Expr::Int(i64::from_str_radix(digits, 2).unwrap())));
            }
        }

        // Emuera reads `p`/`P` as a base-2 exponent and `e`/`E` as a base-10
        // one, case-insensitively, after *any* significand, then evaluates
        // `significand * pow(base, exponent)` in `double` and range-checks the
        // result against `Int64` (`Sub/LexicalAnalyzer.cs:172-190`). The
        // exponent is unsigned — `readDigits` reads digits only — and an
        // exponent of zero leaves the significand untouched rather than
        // round-tripping it through `double`.
        //
        // The corpus needs the uppercase form: `RAND(1 + L_CHARA, 1P31 + …)`
        // at `RPG/スキル関係/60_リーダースキル/スキル効果/LEADER_SKILL_SYSTEM.ERB:93`
        // wants 2^31, and `1P6`/`2P32`/`2P16` appear elsewhere.
        if let Ok((rest, significand)) = i64::<_, Error>(i) {
            let base = match rest.as_bytes().first() {
                Some(b'p' | b'P') => Some(2.0f64),
                Some(b'e' | b'E') => Some(10.0f64),
                _ => None,
            };

            if let Some(base) = base {
                let rest = &rest[1..];
                let (rest, exponent) = u32::<_, Error>(rest).unwrap_or((rest, 0));

                if exponent == 0 {
                    return Ok((rest, Expr::Int(significand)));
                }

                let d = significand as f64 * base.powi(exponent as i32);

                if !d.is_finite() || d > i64::MAX as f64 || d < i64::MIN as f64 {
                    // Emuera throws `OoRInt64` for exactly this.
                    return Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)));
                }

                // `Int64.MaxValue` promotes to 2^63 in that comparison, so
                // `1p63` survives it and reaches C#'s *unchecked* `(Int64)d`,
                // which is `cvttsd2si`: out of range yields `long.MinValue`.
                // The corpus depends on it — `-1p63-1` is documented as
                // `Int64.MaxValue` in `tests/parse_tests/functions/juel.erb:7`
                // — while Rust's `as` would saturate to `i64::MAX` instead.
                const TWO_POW_63: f64 = 9223372036854775808.0;
                let n = if d >= -TWO_POW_63 && d < TWO_POW_63 {
                    d as i64
                } else {
                    i64::MIN
                };

                return Ok((rest, Expr::Int(n)));
            }
        }

        map(i64, Expr::Int)(i)
    };

    match bytes.first().copied() {
        // `__INT_MAX__`/`__INT_MIN__` are otherwise ordinary identifiers.
        Some(b'_') => {
            if let Some(rest) = strip_prefix_no_case(i, "__INT_MAX__") {
                Ok((rest, Expr::Int(i64::MAX)))
            } else if let Some(rest) = strip_prefix_no_case(i, "__INT_MIN__") {
                Ok((rest, Expr::Int(i64::MIN)))
            } else {
                ident_or_method_expr(ctx)(i)
            }
        }
        Some(b'@') if bytes.get(1) == Some(&b'"') => context(
            "form string",
            cut_delimited("@\"", form_str(FormStrType::Str, ctx), "\""),
        )(i),
        Some(b'\\') if bytes.get(1) == Some(&b'@') => form_str_cond_form(ctx)(&i["\\@".len()..]),
        Some(b'"') => map(string, |s| Expr::str(s))(i),
        Some(b'0'..=b'9') => number(i),
        Some(b'(') => paran_expr(ctx)(i),
        Some(_) => ident_or_method_expr(ctx)(i),
        None => Err(nom::Err::Error(error_position!(i, ErrorKind::Eof))),
    }
    .map_err(|e| e.map(|e| e.with_context("single_expr")))
}

fn single_expr<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        enum UnaryIncOp {
            Inc,
            Dec,
            Unary(UnaryOperator),
            None,
        }
        use UnaryIncOp::*;
        let i = i.trim_start_matches(' ');

        let (i, op) = if let Some(i) = i.strip_prefix("++") {
            (i, Inc)
        } else if let Some(i) = i.strip_prefix("--") {
            (i, Dec)
        } else if let Some(i) = i.strip_prefix('+') {
            (i, None)
        } else if let Some(i) = i.strip_prefix('!') {
            (i, Unary(UnaryOperator::Not))
        } else if let Some(i) = i.strip_prefix('~') {
            (i, Unary(UnaryOperator::Not))
        } else if let Some(i) = i.strip_prefix('-') {
            (i, Unary(UnaryOperator::Minus))
        } else {
            (i, None)
        };
        // `skip_sp` subsumes the ASCII-space trims that used to sit on either
        // side of it: it accepts `' '` too, so trimming spaces first changes
        // nothing, and after it the next character is by construction not one.
        let i = skip_sp(i);
        let (i, expr) = single_expr_atom(ctx, i)?;
        let i = skip_sp(i);

        let expr = match op {
            Unary(op) => Expr::unary(expr, op),
            Inc | Dec => {
                let is_inc = matches!(op, Inc);

                match expr {
                    Expr::Var(var) => Expr::IncOpExpr {
                        var,
                        is_pre: true,
                        is_inc,
                    },
                    _ => {
                        log::error!("증감연산자는 변수와 함께 써야합니다.");
                        return Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)));
                    }
                }
            }
            None => expr,
        };

        if ctx.is_arg.get() {
            return Ok((i, expr));
        }

        Ok(if let Some(i) = i.strip_prefix("++") {
            let expr = match expr {
                Expr::Var(var) => Expr::IncOpExpr {
                    var,
                    is_pre: false,
                    is_inc: true,
                },
                _ => {
                    return Err(nom::Err::Error(error_position!(i, ErrorKind::Verify)));
                }
            };
            (i, expr)
        } else if let Some(i) = i.strip_prefix("--") {
            let expr = match expr {
                Expr::Var(var) => Expr::IncOpExpr {
                    var,
                    is_pre: false,
                    is_inc: false,
                },
                _ => {
                    return Err(nom::Err::Error(error_position!(i, ErrorKind::Verify)));
                }
            };
            (i, expr)
        } else if let Some(i) = i.strip_prefix(':') {
            match expr {
                Expr::Var(mut var) => {
                    let (i, args) = separated_list1(char_sp(':'), single_expr(ctx))(i)?;
                    var.args.extend(args);
                    (i, Expr::Var(var))
                }
                _ => {
                    return Err(nom::Err::Error(error_position!(i, ErrorKind::Verify)));
                }
            }
        } else {
            (i, expr)
        })
    }
}

/// Hand-written instead of a 21-branch `alt`: every failing branch built an
/// error and merged it, and `bin_expr` calls this once per operand. Measured
/// as the single largest slice of parse time on the eraTHYMKR corpus.
fn binop(i: &str) -> IResult<'_, BinaryOperator> {
    use BinaryOperator::*;

    let b = i.as_bytes();
    let fail = || nom::Err::Error(error_position!(i, ErrorKind::Tag));

    // Longest operator first within a leading byte, matching the old `alt` order.
    let (len, op) = match (b.first().copied().ok_or_else(fail)?, b.get(1).copied()) {
        (b'+', _) => (1, Add),
        (b'-', _) => (1, Sub),
        (b'*', _) => (1, Mul),
        (b'/', _) => (1, Div),
        (b'%', _) => (1, Rem),
        (b'<', Some(b'<')) => (2, Lhs),
        (b'<', Some(b'=')) => (2, LessOrEqual),
        (b'<', _) => (1, Less),
        (b'>', Some(b'>')) => (2, Rhs),
        (b'>', Some(b'=')) => (2, GreaterOrEqual),
        (b'>', _) => (1, Greater),
        (b'^', Some(b'^')) => (2, Xor),
        (b'^', _) => (1, BitXor),
        (b'|', Some(b'|')) => (2, Or),
        (b'|', _) => (1, BitOr),
        (b'&', Some(b'&')) => (2, And),
        (b'&', _) => (1, BitAnd),
        (b'!', Some(b'|')) => (2, Nor),
        (b'!', Some(b'&')) => (2, Nand),
        (b'!', Some(b'=')) => (2, NotEqual),
        (b'=', Some(b'=')) => (2, Equal),
        _ => return Err(fail()),
    };

    Ok((&i[len..], op))
}

/// The binary operator at `i`, consumed, or `None` where a binary expression
/// has to stop. The three refusals are the ones the operand loop has always
/// made:
/// * under `is_arg`, `++`/`--` open the *next* argument instead of being
///   `+`/`-` applied to a signed operand;
/// * a `=` right after the operator makes it a compound assignment
///   (`+=`, `<<=`, …), whose left side the caller has already parsed;
/// * `%` under `ban_percent`, where it closes a form-string interpolation.
#[inline]
fn peek_binop<'a>(ctx: &ParserContext, i: &'a str) -> Option<(&'a str, BinaryOperator)> {
    if ctx.is_arg.get() && (i.starts_with("++") || i.starts_with("--")) {
        return None;
    }

    let (rest, op) = binop(i).ok()?;

    if rest.starts_with('=') || (matches!(op, BinaryOperator::Rem) && ctx.ban_percent.get()) {
        return None;
    }

    Some((rest, op))
}

/// Precedence climbing: folds every operator that binds at least as tightly as
/// `min_prec` into `lhs`.
///
/// This is what replaced a shift-reduce loop that allocated three `Vec`s — the
/// pending `(op, operand)` list plus an operator and an operand stack — for
/// every expression that had an operator in it. All operators are
/// left-associative, so the right operand climbs from `priority() + 1`; that
/// also bounds the recursion, since each call descends with a strictly greater
/// `min_prec` and [`BinaryOperator::priority`] only spans `2..=8`.
fn climb_binop<'a>(
    ctx: &ParserContext,
    mut i: &'a str,
    mut lhs: Expr,
    min_prec: usize,
) -> IResult<'a, Expr> {
    while let Some((rest, op)) = peek_binop(ctx, i) {
        if op.priority() < min_prec {
            break;
        }

        let (rest, rhs) = single_expr(ctx)(rest)?;
        let (rest, rhs) = climb_binop(ctx, rest, rhs, op.priority() + 1)?;

        i = rest;
        lhs = Expr::binary(lhs, op, rhs);
    }

    Ok((i, lhs))
}

fn bin_expr<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let (i, first) = de_sp(single_expr(ctx))(i)?;

        climb_binop(ctx, i, first, 0)
    }
}

pub fn expr<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let (i, expr) = bin_expr(ctx)(i)?;

        let (i, cond) = opt(pair(de_char_sp('?', bin_expr(ctx), '#'), bin_expr(ctx)))(i)?;

        let expr = match cond {
            Some((if_true, or_false)) => Expr::cond(expr, if_true, or_false),
            None => expr,
        };

        Ok((i, expr))
    }
}

pub fn expr_or_one<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| {
        let (i, expr) = opt(expr(ctx))(i)?;
        Ok((i, expr.unwrap_or(Expr::Int(1))))
    }
}

pub fn expr_pair<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, (Expr, Expr)> + 'c {
    move |i| pair(de_sp(expr(ctx)), preceded(char(','), de_sp(expr(ctx))))(i)
}

pub fn expr_or_blank_list<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<Expr>> + 'c {
    move |i| {
        separated_list0(
            char(','),
            map(de_sp(opt(expr(ctx))), |expr| {
                expr.unwrap_or_else(|| Expr::String(ctx.interner.get_or_intern_static("")))
            }),
        )(i)
    }
}

pub fn expr_list<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<Option<Expr>>> + 'c {
    move |i| {
        map(
            separated_list0(char(','), de_sp(opt(expr(ctx)))),
            |mut list| {
                // remove trailing empty arg
                if let Some(Some(item)) = list.pop() {
                    list.push(Some(item));
                }

                list
            },
        )(i)
    }
}

pub fn call_arg_list<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<Option<Expr>>> + 'c {
    move |i| {
        preceded(
            sp,
            alt((
                de_char_sp('(', expr_list(ctx), ')'),
                preceded(char_sp(','), expr_list(ctx)),
                value(Vec::new(), eof),
            )),
        )(i)
    }
}

pub fn call_jump_line<'c, 'a>(
    ctx: &'c ParserContext,
    is_form: bool,
) -> impl FnMut(&'a str) -> IResult<'a, (Expr, Vec<Option<Expr>>)> + 'c {
    move |i| {
        context("call_jump_line", move |i| {
            let (i, name) = if is_form {
                call_form_arg_expr(ctx)(i)?
            } else {
                let (i, function) = ident_no_case(i)?;
                let function = ctx.replace(&function);

                if !erars_lexer::utils::is_ident(function.as_ref()) {
                    panic!("CALL/JUMP문은 식별자를 받아야합니다");
                }

                (i, Expr::String(ctx.intern_ident(&function)))
            };

            let (i, args) = call_arg_list(ctx)(i)?;

            Ok((i, (name, args)))
        })(i)
    }
}

/// `REF refvar, srcvar` / `REFBYNAME refvar, <str expr>`.
///
/// Emuera's `SP_REF_ArgumentBuilder` (`ArgumentBuilder.cs:2079-2151`) takes the
/// reference variable as a bare identifier in both forms; only the *target*
/// differs, being an identifier for `REF` and a string expression for
/// `REFBYNAME` — which the builder rejects if it is an integer expression.
pub fn ref_line<'c, 'a>(
    ctx: &'c ParserContext,
    byname: bool,
) -> impl FnMut(&'a str) -> IResult<'a, (Expr, Expr)> + 'c {
    move |i| {
        context("ref_line", move |i| {
            let (i, target) = de_sp(ident_no_case)(i)?;
            let target = ctx.replace(&target);
            let (i, _) = char_sp(',')(i)?;

            let (i, src) = if byname {
                expr(ctx)(i)?
            } else {
                let (i, src) = de_sp(ident_no_case)(i)?;
                let src = ctx.replace(&src);
                (i, Expr::String(ctx.intern_ident(&src)))
            };

            Ok((i, (Expr::String(ctx.intern_ident(&target)), src)))
        })(i)
    }
}

fn case_cond<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, SelectCaseCond> + 'c {
    move |i| {
        alt((
            map(
                tuple((expr(ctx), de_sp(tag_no_case("TO")), expr(ctx))),
                |(l, _, r)| SelectCaseCond::To(l, r),
            ),
            map(
                preceded(de_sp(tag_no_case("IS")), pair(de_sp(binop), expr(ctx))),
                |(op, expr)| SelectCaseCond::Is(op, expr),
            ),
            map(expr(ctx), SelectCaseCond::Single),
        ))(i)
    }
}

pub fn case_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<SelectCaseCond>> + 'c {
    move |i| separated_list1(char_sp(','), case_cond(ctx))(i)
}

pub fn for_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, (Variable, Expr, Expr, Expr)> + 'c {
    move |i| {
        let (i, mut exprs) = expr_list(ctx)(i)?;
        match exprs.len() {
            3 => {
                let end = exprs.pop().unwrap().expect("Empty for");
                let init = exprs.pop().unwrap().expect("Empty for");
                let var = exprs.pop().unwrap().expect("Empty for").into_var().unwrap();
                Ok((i, (var, init, end, Expr::int(1))))
            }
            4 => {
                let step = exprs.pop().unwrap().expect("Empty for");
                let end = exprs.pop().unwrap().expect("Empty for");
                let init = exprs.pop().unwrap().expect("Empty for");
                let var = exprs.pop().unwrap().expect("Empty for").into_var().unwrap();
                Ok((i, (var, init, end, step)))
            }
            other => {
                log::error!("FOR문은 인자로 3개나 4개를 가져야합니다: 받은 인자수 {other}개");
                Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)))
            }
        }
    }
}

pub fn times_line<'c, 'a>(ctx: &'c ParserContext) -> impl FnMut(&'a str) -> IResult<'a, Stmt> + 'c {
    move |i| {
        let (i, var) = variable(ctx)(i)?;
        let (i, times) = preceded(char_sp(','), float)(i)?;
        Ok((i, Stmt::Times(var, NotNan::new(times).unwrap())))
    }
}

fn forward_or_back<'a>(i: &'a str) -> IResult<'a, Option<bool>> {
    opt(alt((
        value(false, de_sp(tag_no_case("BACK"))),
        value(true, de_sp(tag_no_case("FORWARD"))),
    )))(i)
}

pub fn arraysort_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Stmt> + 'c {
    move |i| {
        let (mut i, var) = variable(ctx)(i)?;

        let mut forward = true;
        let mut start = None;
        let mut end = None;

        if let Some(i_) = i.trim_start().strip_prefix(',') {
            i = i_;
            let (i_, forward_) = forward_or_back(i)?;
            i = i_;
            forward = forward_.unwrap_or(true);
            if let Some(i_) = i.trim_start().strip_prefix(',') {
                i = i_;
                let (i_, start_) = opt(expr(ctx))(i)?;
                start = start_;
                i = i_;

                if let Some(i_) = i.trim_start().strip_prefix(',') {
                    i = i_;
                    let (i_, end_) = opt(expr(ctx))(i)?;
                    i = i_;
                    end = end_;
                }
            }
        }

        Ok((
            i,
            Stmt::Command(
                BuiltinCommand::ArraySort,
                vec![
                    Some(Expr::Var(var)),
                    Some(Expr::int(forward)),
                    start.map(Expr::from),
                    end.map(Expr::from),
                ],
            ),
        ))
    }
}

pub fn sortchara_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Stmt> + 'c {
    move |i| {
        let (i, forward) = forward_or_back(i)?;

        let (i, var) = if forward.is_none() {
            opt(variable(ctx))(i)?
        } else {
            (i, None)
        };

        let (i, forward) = if forward.is_none() {
            // `SORTCHARA <key>, BACK` — the sort order is a second argument, so
            // the separator has to be eaten before the order token is read.
            let i = i.trim_start().strip_prefix(',').unwrap_or(i);
            forward_or_back(i)?
        } else {
            (i, forward)
        };

        let var = var.unwrap_or_else(|| Variable {
            var: ctx.interner.get_or_intern_static("NO"),
            args: Vec::new(),
            func_extern: None,
        });

        let forward = forward.unwrap_or(true);

        Ok((
            i,
            Stmt::Command(
                BuiltinCommand::SortChara,
                vec![Some(Expr::Var(var)), Some(Expr::int(forward))],
            ),
        ))
    }
}

/// A `#DIM`/`#DIMS` line as written, with its size expressions unevaluated.
///
/// Emuera reads a header `#DIM` line without analysing it, queueing the raw
/// token stream (`GameProc/HeaderFileLoader.cs:127-128`), and only reduces the
/// size expressions once every header file has contributed its constants
/// (`GameProc/HeaderFileLoader.cs:276-364`). Keeping the parse and the
/// constant folding apart is what lets `HeaderInfo::resolve_pending_dims`
/// retry a declaration whose size names a constant that is not defined yet,
/// and it keeps `const_eval`'s error out of the allocation-free nom error
/// type: the parser can no longer panic on a size it cannot fold.
#[derive(Debug)]
pub struct DimDecl {
    pub var: StrKey,
    /// Keyword flags, the initialiser list, and — only when `sizes` is `None` —
    /// the size implied by that initialiser.
    pub info: VariableInfo,
    /// Size expressions in declaration order, `None` when the declaration had
    /// no size list at all. [`crate::HeaderInfo::finish_dim`] folds these into
    /// `info.size`.
    pub sizes: Option<Vec<Expr>>,
}

pub fn dim_line<'c, 'a>(
    ctx: &'c ParserContext,
    is_str: bool,
) -> impl FnMut(&'a str) -> IResult<'a, DimDecl> + 'c {
    move |mut i| {
        let mut info = VariableInfo::default();
        info.is_str = is_str;

        loop {
            match de_sp(ident_no_case)(i) {
                Ok((i_, tag)) => {
                    i = i_;
                    match tag.as_ref() {
                        "CONST" => info.is_const = true,
                        "DYNAMIC" => info.is_dynamic = true,
                        "REF" => info.is_ref = true,
                        "CHARADATA" => info.is_chara = true,
                        "SAVEDATA" => info.is_savedata = true,
                        "GLOBAL" => info.is_global = true,
                        ident => {
                            let (i, (sizes, init)) = pair(
                                opt(preceded(
                                    char_sp(','),
                                    separated_list0(char_sp(','), expr(ctx)),
                                )),
                                opt(preceded(
                                    char_sp('='),
                                    separated_list0(char_sp(','), expr(ctx)),
                                )),
                            )(i)?;

                            // A sizeless declaration with an initialiser takes
                            // its length from that initialiser; nothing here
                            // needs constant folding, so it is settled now and
                            // `finish_dim` leaves it alone.
                            if sizes.is_none() {
                                info.size = init
                                    .as_ref()
                                    .map(|v| {
                                        let mut size = tinyvec::ArrayVec::new();
                                        size.push(v.len() as u32);
                                        size
                                    })
                                    .unwrap_or_default();
                            }
                            info.init = init.filter(|v| !v.is_empty()).map(Vec::into_boxed_slice);

                            break Ok((
                                i,
                                DimDecl {
                                    var: ctx.intern_ident(&ident),
                                    info,
                                    sizes,
                                },
                            ));
                        }
                    }
                }
                Err(e) => return Err(e),
            }
        }
    }
}

pub fn call_form_arg_expr<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| (de_sp(form_str(FormStrType::CallArg, ctx)))(i)
}

pub fn form_arg_expr<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Expr> + 'c {
    move |i| (de_sp(form_str(FormStrType::Arg, ctx)))(i)
}

pub fn returnform_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Stmt> + 'c {
    move |i| {
        let (i, args) = separated_list0(
            char(','),
            opt(map(form_arg_expr(ctx), |f| {
                Expr::BuiltinMethod(BuiltinMethod::ToInt, vec![Some(f)])
            })),
        )(i)?;

        Ok((i, Stmt::Command(BuiltinCommand::Return, args)))
    }
}

fn function_arg_list<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<(Variable, Option<InlineValue>)>> + 'c {
    move |i| {
        let (i, mut args) = separated_list0(
            char(','),
            de_sp(pair(
                variable(ctx),
                opt(preceded(
                    char_sp('='),
                    map(expr(ctx), |expr| {
                        ctx.header.as_ref().const_eval_log_error(&expr).into()
                    }),
                )),
            )),
        )(i)?;

        // An argument's index has to be a plain number by the time the VM sees
        // it, and the corpus writes it as a `#DIM CONST`
        // (`@CS_DICEROLL_S_STATE, …, STATE_DATA_V:CSS3_STATE, …`). Emuera
        // `Restructure`s the term for the same reason
        // (`GameProc/Function/FunctionLabelLine.cs` via
        // `GameData/Expression/ExpressionParser.cs`), so fold it here where
        // the header's constants are already resolved.
        for (var, _) in args.iter_mut() {
            for arg in var.args.iter_mut() {
                if matches!(arg, Expr::Int(_)) {
                    continue;
                }
                match ctx
                    .header
                    .as_ref()
                    .const_eval(arg)
                    .and_then(|v| v.try_into_int().map_err(Into::into))
                {
                    Ok(v) => *arg = Expr::Int(v),
                    Err(err) => {
                        log::error!("Argument index {arg:?} is not constant: {err}");
                        return Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)));
                    }
                }
            }
        }

        Ok((i, args))
    }
}

pub fn function_line<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, (Cow<'a, str>, Vec<(Variable, Option<InlineValue>)>)> + 'c {
    move |i| {
        pair(
            de_sp(ident_no_case),
            preceded(
                sp,
                alt((
                    de_char_sp('(', function_arg_list(ctx), ')'),
                    preceded(char_sp(','), function_arg_list(ctx)),
                    value(Vec::new(), eof),
                )),
            ),
        )(i)
    }
}

/// Reads the `:`-separated index list that follows a variable name.
///
/// A name from a CSV may stand in for a number, but at exactly one dimension:
/// Emuera's `GetKeywordDictionary` returns the table together with an
/// `allowIndex` and rejects the name anywhere else
/// (`GameData/ConstantData.cs:892-1090`, the guard at `:1091-1096`). That
/// index is 1 for a character variable — dimension 0 selects the character —
/// and 0 for every other one. `CDFLAG` is the single two-table variable:
/// dimension 1 reads `CDFLAG1.CSV` and dimension 2 reads `CDFLAG2.CSV`
/// (`:1019-1039`). `UP`/`DOWN`/`LOSEBASE`/`ITEMSALES` and friends borrow
/// another variable's table while keeping their own dimension, which is what
/// `var_name_alias` reproduces.
pub fn variable_arg<'c, 'a>(
    ctx: &'c ParserContext,
    var: &'c str,
) -> impl FnMut(&'a str) -> IResult<'a, Vec<Expr>> + 'c {
    move |mut i| {
        let header = ctx.header.as_ref();
        let table = |name: &str| header.var_names.get(&ctx.intern_ident(name));
        let alias = var_name_alias(var);

        let is_chara = alias.eq_ignore_ascii_case("CDFLAG")
            || header
                .global_variables
                .get(&ctx.intern_ident(alias))
                .map_or(false, |info| info.is_chara);

        // Indexed by dimension, so a name is only tried where Emuera allows one.
        let names = if alias.eq_ignore_ascii_case("CDFLAG") {
            [None, table("CDFLAG1"), table("CDFLAG2")]
        } else if is_chara {
            [None, table(alias), None]
        } else {
            [table(alias), None, None]
        };

        let is_arg = ctx.is_arg.get();
        ctx.is_arg.set(true);
        let mut args = Vec::new();
        while let Ok((i_, _)) = char_sp(':')(i) {
            if i_.chars().next().map_or(false, erars_lexer::utils::is_ident_head) {
                if let Ok((after, name)) = ident(i_) {
                    // A one-argument character variable writes its element at
                    // dimension 0, but Emuera prepends `TARGET` before it
                    // decides which dimension may carry a name
                    // (`GameData/Variable/VariableParser.cs:113-121`), so
                    // `TALENT:소질명` reaches the same table as
                    // `TALENT:CHARA:소질명`.
                    let dim = if is_chara
                        && args.is_empty()
                        && !after.trim_start_matches(' ').starts_with(':')
                    {
                        1
                    } else {
                        args.len()
                    };

                    let hit = names
                        .get(dim)
                        .copied()
                        .flatten()
                        .and_then(|t| t.get(&ctx.intern_ident(&name)));

                    if let Some(v) = hit {
                        args.push(Expr::int(*v));
                        i = after;
                        continue;
                    }
                }
            }

            let (i_, arg) = single_expr(ctx)(i_)?;

            args.push(arg);
            i = i_;
        }
        ctx.is_arg.set(is_arg);

        Ok((i, args))
    }
}

/// Reads one variable reference — the shape an assignment's left-hand side
/// and a function argument take.
///
/// Emuera macro-expands every identifier token of the text it analyses, an
/// assignment's left-hand side included: `LogicalLineParser` hands the whole
/// expression to `LexicalAnalyzer.Analyse`, which finishes by walking the
/// token list from `Pointer = 0` and substituting each identifier that names
/// a macro (`Sub/LexicalAnalyzer.cs:990-1029`, `:1018-1021` for the
/// argumentless form). A body is an arbitrary token run — this corpus writes
/// `#DEFINE FLAG_D13_出現告知制御 ダンジョンフラグ:13:5` — so it is re-read
/// here as a variable reference, and an index written after the macro name
/// concatenates onto the body's own indices exactly as the token list would.
pub fn variable<'c, 'a>(
    ctx: &'c ParserContext,
) -> impl FnMut(&'a str) -> IResult<'a, Variable> + 'c {
    move |i| {
        let (i, name) = de_sp(ident_no_case)(i)?;

        if !erars_lexer::utils::is_ident(&name) {
            log::error!("`{name}` is not a variable name");
            return Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)));
        }

        match ctx.replace(&name) {
            Cow::Borrowed(name) => {
                let (i, func_extern) = var_func_extern(ctx, i)?;
                let (i, args) = variable_arg(ctx, name)(i)?;

                Ok((
                    i,
                    Variable {
                        var: ctx.intern_ident(name),
                        func_extern,
                        args,
                    },
                ))
            }
            Cow::Owned(body) => {
                let (left, mut var) =
                    variable(ctx)(&body).map_err(|e| e.map(|e| e.relocate(i)))?;

                if !left.is_empty() {
                    log::error!("Macro body `{body}` is not a complete variable reference");
                    return Err(nom::Err::Failure(error_position!(i, ErrorKind::Verify)));
                }

                let (i, func_extern) = var_func_extern(ctx, i)?;
                let (i, args) = variable_arg(ctx, var.var.resolve())(i)?;

                var.func_extern = var.func_extern.or(func_extern);
                var.args.extend(args);

                Ok((i, var))
            }
        }
    }
}

#[cfg(test)]
mod scan_tests {
    use super::{ident, skip_sp};

    /// The predicate `take_while(is_sp)` and `trim_start_matches(SP)` used.
    fn is_sp(c: char) -> bool {
        c == ' ' || c == '\t' || c == '\r' || c == '\u{3000}'
    }

    #[test]
    fn skipping_spaces_answers_exactly_like_the_char_predicate() {
        for s in [
            "",
            " ",
            "   \t\r  ",
            "\u{3000}",
            " \u{3000}\t\u{3000} X",
            "X   ",
            // `0xE3` leads hiragana, katakana and CJK punctuation too; only
            // U+3000 may be skipped.
            "\u{3001}X",
            "\u{3042}X",
            "\u{30FB}X",
            "\u{3000}\u{3042}",
            // A lone lead byte cannot be mistaken for U+3000.
            "\u{3000}가",
            "안녕",
        ] {
            assert_eq!(skip_sp(s), s.trim_start_matches(is_sp), "{s:?}");
        }
    }

    #[test]
    fn an_identifier_stops_where_the_char_predicate_would() {
        for s in ["A", "COUNT", "A_1", "가나다", "NAME rest", "N\u{3000}X", "A+B", ""] {
            let want = s.split_at(
                s.char_indices()
                    .find(|&(_, c)| !erars_lexer::utils::is_ident_body(c))
                    .map_or(s.len(), |(at, _)| at),
            );
            match ident(s) {
                Ok((rest, got)) => assert_eq!((got, rest), want, "{s:?}"),
                Err(_) => assert!(want.0.is_empty(), "{s:?}"),
            }
        }
    }

    #[test]
    fn an_identifier_may_not_start_with_a_digit() {
        assert!(ident("1A").is_err());
        assert!(ident("0").is_err());
        assert!(ident("A1").is_ok());
    }
}
