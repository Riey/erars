mod inst;
mod inst_memo;
mod sharp;
mod square;
pub mod utils;

use std::ops::Range;

use cow_utils::CowUtils;
use hashbrown::HashMap;
use logos::Logos;

use erars_ast::*;
use std::str::FromStr;

pub use bumpalo::Bump;
pub use inst::InstructionCode;
use inst_memo::InstMemo;
pub use sharp::SharpCode;
pub use square::SquareCode;
pub use strum::IntoEnumIterator;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrintType {
    Plain,
    Form,
    FormS,
    S,
    V,
    Data,
}

/// What the innermost open `[…]` region is waiting for.
///
/// Emuera keeps this as a stack of strings (`GameProc/ErbLoader.cs:145`
/// `Stack<string> ppMatch`) holding `"SKIPEND"`, `"ELSEIF"` or `"ENDIF"`;
/// every closing directive pops one and compares it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PpExpect {
    SkipEnd,
    ElseIf,
    EndIf,
}

impl PpExpect {
    /// The name Emuera prints for a region left open at end of file.
    ///
    /// `PPState.FileEnd` (`GameProc/ErbLoader.cs:282-296`) rewrites a pending
    /// `"ELSEIF"` to `"ENDIF"` first: what an unfinished `[IF]` chain is
    /// missing is its `[ENDIF]`, not another `[ELSEIF]`.
    fn missing_name(self) -> &'static str {
        match self {
            Self::SkipEnd => "SKIPEND",
            Self::ElseIf | Self::EndIf => "ENDIF",
        }
    }
}

/// Emuera's `PPState` (`GameProc/ErbLoader.cs:139-296`): everything the `[…]`
/// preprocessor remembers while reading one file.
#[derive(Default, Clone)]
struct PpState {
    /// Inside `[SKIPSTART]`..`[SKIPEND]`. It disables unconditionally and
    /// never nests: a second `[SKIPSTART]` is a warning, not a level.
    skip: bool,
    /// A branch of the current `[IF…]` chain has already been taken, so every
    /// later `[ELSEIF]`/`[ELSE]` of that chain stays disabled.
    done: bool,
    /// Lines are read but thrown away.
    disabled: bool,
    disabled_stack: Vec<bool>,
    done_stack: Vec<bool>,
    match_stack: Vec<PpExpect>,
}

/// The `[…]` preprocessor's diagnostics.
///
/// Emuera's own wording is Japanese and lives in `_Library/EvilMask/Lang.cs`;
/// erars speaks Korean to the user (`erars-compiler/src/error.rs:12-24`), so
/// each message is translated and the original is quoted at its citation.
/// Every one of them is Emuera's warning level 1 — the line is dropped, the
/// load continues.
mod pp_msg {
    /// `ヘッダーの中に#で始まらない行があります`
    /// (`_Library/EvilMask/Lang.cs:747`).
    pub fn not_sharp_line_in_header() -> String {
        "헤더에 #으로 시작하지 않는 행이 있습니다".into()
    }

    /// `"{0}"に余分な引数があります` (`_Library/EvilMask/Lang.cs:757`).
    pub fn has_too_many_arg(token: &str) -> String {
        format!("\"{token}\"에 여분의 인수가 있습니다")
    }

    /// `[SKIPSTART]が重複して使用されています`
    /// (`_Library/EvilMask/Lang.cs:758`).
    pub fn duplicate_skipstart() -> String {
        "[SKIPSTART]가 중복되어 사용됐습니다".into()
    }

    /// `"{0}"に引数がありません` (`_Library/EvilMask/Lang.cs:759`).
    pub fn missing_arguments(token: &str) -> String {
        format!("\"{token}\"에 인수가 없습니다")
    }

    /// `不適切な{0}です` (`_Library/EvilMask/Lang.cs:760`). Emuera passes the
    /// bracketed form, `[ELSEIF]` or `[ELSE]`.
    pub fn is_invalid(what: &str) -> String {
        format!("부적절한 {what}입니다")
    }

    /// `[SKIPSTART]と対応しない[SKIPEND]です`
    /// (`_Library/EvilMask/Lang.cs:761`).
    pub fn unexpected_skipend() -> String {
        "[SKIPSTART]와 대응하지 않는 [SKIPEND]입니다".into()
    }

    /// `対応する[IF]のない[ENDIF]です` (`_Library/EvilMask/Lang.cs:762`).
    pub fn unexpected_macro_endif() -> String {
        "대응하는 [IF]가 없는 [ENDIF]입니다".into()
    }

    /// `認識できないプリプロセッサです` (`_Library/EvilMask/Lang.cs:763`).
    pub fn unrecognized_preprosessor() -> String {
        "인식할 수 없는 전처리기입니다".into()
    }

    /// `[{0}]がありません` (`_Library/EvilMask/Lang.cs:764`).
    pub fn theres_no(name: &str) -> String {
        format!("[{name}]가 없습니다")
    }

    /// `[]の使い方が不正です` (`_Library/EvilMask/Lang.cs:765`).
    pub fn invalid_sbrackets() -> String {
        "[]의 사용법이 잘못됐습니다".into()
    }

    /// `[{0}]の後ろは無視されます。` (`_Library/EvilMask/Lang.cs:766`).
    pub fn ignore_after_preprosessor(token: &str) -> String {
        format!("[{token}] 뒤는 무시됩니다")
    }
}

/// Offset of the first `[[` in `s`.
///
/// One `memchr` pass. [`Preprocessor::rename_line`] calls this on the file's
/// unsearched tail rather than on each line, so a lone `[` — which is what
/// re-enters the loop — is paid for once per file instead of once per line.
fn find_open(s: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let mut at = 0;
    loop {
        let off = at + memchr::memchr(b'[', &bytes[at..])?;
        if bytes.get(off + 1) == Some(&b'[') {
            return Some(off);
        }
        at = off + 1;
    }
}

/// `s.split_once('\n')`, vectorised.
///
/// `str`'s own single-character search goes through `CharSearcher`, which
/// calls `core`'s `memchr` — a word-at-a-time loop with an alignment prologue,
/// not the AVX2 one the `memchr` crate compiles to. The lexer cuts one line
/// per call, 890_801 times per pass over the corpus, and the profile charged
/// 5.2% of parse+compile self time to `core::slice::memchr::memchr_aligned`
/// plus 2.9% to `CharSearcher::next_match`.
fn split_nl(s: &str) -> Option<(&str, &str)> {
    let at = memchr::memchr(b'\n', s.as_bytes())?;
    // `\n` is ASCII, so `at` and `at + 1` are both char boundaries.
    Some(unsafe { (s.get_unchecked(..at), s.get_unchecked(at + 1..)) })
}

/// Splices in every `_Rename.csv` entry the line mentions.
///
/// Emuera applies the rename table as a raw *text* substitution on each
/// physical line as it is read — before the line is joined to its continuation
/// and before anything is lexed. `Sub/EraStreamReader.cs:86-89` handles a plain
/// line and `:120-123` a line inside a `{`..`}` block; both run
/// `line.Replace(key, value)` for every dictionary entry whenever the line
/// contains both `[[` and `]]`. A `[[…]]` token is therefore spliced in
/// verbatim wherever it sits — in a function label
/// (`@INDIVIDUAL_EVENT_K[[キャラ:이오]]`), as a variable name, inside an index —
/// and the result is re-read as ordinary source text. It applies to ERH too
/// (`GameProc/HeaderFileLoader.cs:86`).
///
/// Emuera loops over all ~9_500 entries for every such line. Scanning the line
/// for `[[`…`]]` and looking each token up once is equivalent, because a key is
/// exactly `[[` + the CSV's right-hand column + `]]`
/// (`GameData/ParserMediator.cs:73`) and, measured over eraMegaten's own table
/// (9503 entries, 9474 distinct keys), no column contains `[[` or `]]`. So no
/// key can occur inside another key — 445 of the columns *are* substrings of
/// other columns, but `[[A]]` can only sit inside `[[B]]` if `B` itself holds a
/// bracket pair — and no value can grow a token for a later entry to match,
/// which is what makes Emuera's iteration order unobservable. Duplicate keys
/// resolve the same way: `HashMap::insert` and `RenameDic[key] = value` both
/// keep the last row.
///
/// A token with no entry is left in place, as in Emuera, where it only becomes
/// an error once the lexer actually reaches it
/// (`Sub/LexicalAnalyzer.cs:865-880`) and so stays harmless inside a comment.
fn apply_rename<'a>(rename: &HashMap<String, String>, line: &'a str, b: &'a Bump) -> &'a str {
    let mut pieces = Vec::new();
    let mut len = 0;
    let mut rest = line;
    let mut replaced = false;

    while let Some(open) = find_open(rest) {
        let Some(close) = rest[open + 2..].find("]]") else {
            break;
        };
        let token_end = open + 2 + close + 2;

        // The dictionary key is trimmed (`ParserMediator.cs:72-73`), and the
        // padded form `[[ Key ]]` is in use, so trim the token as well.
        match rename.get(rest[open + 2..open + 2 + close].trim()) {
            Some(value) => {
                pieces.push(&rest[..open]);
                pieces.push(value.as_str());
                len += open + value.len();
                replaced = true;
            }
            None => {
                pieces.push(&rest[..token_end]);
                len += token_end;
            }
        }

        rest = &rest[token_end..];
    }

    if !replaced {
        return line;
    }

    pieces.push(rest);
    len += rest.len();

    let buf = b.alloc_slice_fill_copy(len, 0u8);
    let mut at = 0;
    for piece in pieces {
        buf[at..at + piece.len()].copy_from_slice(piece.as_bytes());
        at += piece.len();
    }

    // Every piece is a whole `str`, so the concatenation is valid UTF-8.
    unsafe { std::str::from_utf8_unchecked(buf) }
}

#[derive(Clone)]
pub struct Preprocessor<'s> {
    s: &'s str,
    rename: &'s HashMap<String, String>,
    /// `#DEFINE` names, for `[IF name]`. `None` in an ERH, which has no `[…]`
    /// preprocessor at all and whose own table is being built as it is read.
    macros: Option<&'s HashMap<String, String>>,
    /// The whole file, kept so a line's own offset can be recovered from its
    /// pointer: every line handed to [`Self::rename_line`] is a slice of it.
    orig: &'s str,
    /// Offset of the next `[[` in `orig`, or `usize::MAX` when none is left.
    /// See [`Self::rename_line`].
    next_rename: usize,
    /// Emuera's `-DEBUG`: decides `[IF_DEBUG]`/`[IF_NDEBUG]` and whether
    /// `;#;` is a marker or a comment.
    debug_mode: bool,
    /// Reading an ERH, where a line not starting with `#` is an error.
    is_header: bool,
    pp: PpState,
    warnings: Vec<(String, Range<usize>)>,
    /// `PPState.FileEnd` reports once, however many times end of file is hit.
    file_end_reported: bool,

    line_pos: usize,
    start_len: usize,
    span_begin: usize,
    span_end: usize,
    /// Memo for the instruction keyword table. `next_line` asks it about the
    /// first word of every line — 890_801 times per pass over the corpus, and
    /// 52% of those words are not instructions at all — while a single ERB
    /// file only ever starts a line with ~21 distinct words.
    inst_memo: InstMemo,
}

impl<'s> Preprocessor<'s> {
    /// An ERB: the `[…]` preprocessor and the inline markers are live.
    pub fn new_erb(
        rename: &'s HashMap<String, String>,
        macros: &'s HashMap<String, String>,
        debug_mode: bool,
        s: &'s str,
    ) -> Self {
        Self::new_impl(rename, Some(macros), debug_mode, false, s)
    }

    /// An ERH. Emuera reads headers with a loader of their own
    /// (`GameProc/HeaderFileLoader.cs:96-133`) that accepts `#` lines and
    /// nothing else, so no directive is interpreted here.
    pub fn new_erh(rename: &'s HashMap<String, String>, s: &'s str) -> Self {
        Self::new_impl(rename, None, false, true, s)
    }

    fn new_impl(
        rename: &'s HashMap<String, String>,
        macros: Option<&'s HashMap<String, String>>,
        debug_mode: bool,
        is_header: bool,
        s: &'s str,
    ) -> Self {
        let no_bom = s.trim_start_matches('\u{feff}');
        let span_begin = no_bom.as_ptr() as usize - s.as_ptr() as usize;
        let s = no_bom;

        Self {
            s,
            rename,
            macros,
            orig: s,
            next_rename: match rename.is_empty() {
                true => usize::MAX,
                false => find_open(s).unwrap_or(usize::MAX),
            },
            debug_mode,
            is_header,
            pp: PpState::default(),
            warnings: Vec::new(),
            file_end_reported: false,

            line_pos: 0,
            start_len: s.len() + span_begin,
            span_begin,
            span_end: span_begin,
            inst_memo: InstMemo::new(),
        }
    }

    /// Drains the `[…]` preprocessor's warnings.
    ///
    /// They are Emuera's level-1 warnings, so they are not failures: the
    /// caller reports them and keeps whatever compiled.
    pub fn take_warnings(&mut self) -> Vec<(String, Range<usize>)> {
        std::mem::take(&mut self.warnings)
    }

    /// Records one level-1 warning at the line being lexed. `pub` because the
    /// `#`-directive diagnostics (`GameProc/LogicalLineParser.cs:36-266`) are
    /// raised by the parser, which has the function label the preprocessor
    /// never sees, and they share this channel with the `[…]` ones.
    pub fn warn(&mut self, message: String) {
        let span = self.span();
        self.warnings.push((message, span));
    }

    /// [`apply_rename`], for the lines that can actually hold a token.
    ///
    /// A `[[` is rare — no eraTHYMKR ERB has one at all, and 6538 of
    /// eraMegaten's 8841 files have none — but the corpus is 890_801 lines of
    /// 70 bytes, and searching a slice that short is nearly all call
    /// overhead. `next_rename` is the offset of the next `[[` and only moves
    /// forward, so every byte of a file is searched exactly once, in one pass,
    /// instead of once per line. Interleaved A/B over 8 runs each, min CPU
    /// time of the preprocess+lex phase: eraTHYMKR 69.2 -> 63.6 ms, eraMegaten
    /// 92.4 -> 87.3 ms; skipping the splice entirely scores 63.1 ms, so the
    /// scan that remains is 0.5 ms.
    fn rename_line<'a>(&mut self, line: &'a str, b: &'a Bump) -> &'a str
    where
        's: 'a,
    {
        // No token left in the file — the common case, and the only one for a
        // game without a `_Rename.csv`. An empty line is also the one input
        // that need not be a slice of `orig`: `skip_ws` parks `self.s` on a
        // static `""` at end of file. So the offset below is only ever taken
        // for a real line of this file.
        if self.next_rename == usize::MAX || line.is_empty() {
            return line;
        }

        let end = line.as_ptr() as usize - self.orig.as_ptr() as usize + line.len();
        if self.next_rename >= end {
            return line;
        }

        // Every token in the line is spliced, so the scan resumes after it.
        self.next_rename = match find_open(&self.orig[end..]) {
            Some(off) => end + off,
            None => usize::MAX,
        };

        apply_rename(self.rename, line, b)
    }

    pub fn left_text(&self) -> &'s str {
        self.s
    }

    fn current_pos(&self) -> usize {
        self.start_len - self.s.len()
    }

    fn skip_ws(&mut self) {
        let mut chars = self.s.chars();

        loop {
            match chars.next() {
                // U+3000 IDEOGRAPHIC SPACE is whitespace to Emuera whenever
                // `SystemAllowFullSpace` is on, which is its default
                // (`Sub/LexicalAnalyzer.cs:749-752`,
                // `Config/ConfigData.cs:112`).
                Some(' ' | '\t' | '\r' | '\u{3000}') => {}
                Some('\n') => {
                    self.line_pos += 1;
                }
                Some(';') => {
                    // `;!;` is whitespace, always, and `;#;` is whitespace
                    // while `-debug` is on (`Sub/LexicalAnalyzer.cs:753-765`):
                    // both let the rest of the line be read as code. Any
                    // other `;` starts a comment.
                    let rest = chars.as_str();

                    if utils::marker_tail(rest.as_bytes(), self.debug_mode) {
                        chars = rest[2..].chars();
                    } else if let Some(s) = split_nl(rest) {
                        self.line_pos += 1;
                        chars = s.1.chars();
                    } else {
                        self.s = "";
                        break;
                    }
                }
                Some(ch) => {
                    let ch_len = ch.len_utf8();
                    let s = chars.as_str();

                    self.s = unsafe {
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            s.as_ptr().sub(ch_len),
                            s.len() + ch_len,
                        ))
                    };

                    break;
                }
                None => {
                    self.s = "";
                    break;
                }
            }
        }
    }

    fn next_raw_line<'a>(&mut self, b: &'a Bump) -> Result<&'a str, (String, Range<usize>)>
    where
        's: 'a,
    {
        loop {
            self.skip_ws();

            self.span_begin = self.current_pos();
            self.span_end = self.span_begin;

            if self.s.is_empty() {
                // `PPState.FileEnd` (`GameProc/ErbLoader.cs:282-296`) reports
                // exactly one region left open at end of file, however many
                // are, and names what the source is missing.
                if !std::mem::replace(&mut self.file_end_reported, true) {
                    if let Some(expect) = self.pp.match_stack.pop() {
                        self.warn(pp_msg::theres_no(expect.missing_name()));
                    }
                }

                return Ok("");
            }

            let line = if self.pp.disabled {
                // A disabled region is consumed one physical line at a time.
                // `ReadEnabledLine` skips its `{`/`}` continuation handling
                // while disabled — 「[SKIPSTART]～[SKIPEND]中にここが誤爆する
                // ので無効化」 (`Sub/EraStreamReader.cs:95-106`) — so a brace
                // inside a skipped region is text, not a block opener, and a
                // directive that follows it is still read.
                self.line_pos += 1;
                let (line, left) = split_nl(self.s).unwrap_or((self.s, ""));
                self.s = left;
                self.rename_line(line.trim_end_matches('\r'), b)
            } else if let Some(open_brace) = self.s.strip_prefix('{') {
                let mut lines = Vec::new();
                let mut all_bytes = 0;

                for line in open_brace.lines() {
                    if let Some(left) = line.trim_start().strip_prefix('}') {
                        self.s = unsafe {
                            open_brace.get_unchecked(
                                left.as_ptr() as usize - open_brace.as_ptr() as usize..,
                            )
                        };
                        break;
                    } else {
                        let line = self.rename_line(line, b);
                        all_bytes += line.len();
                        lines.push(line);
                    }
                }

                if lines.is_empty() {
                    ""
                } else {
                    unsafe {
                        let buf = b.alloc_layout(
                            std::alloc::Layout::array::<u8>(all_bytes).unwrap_unchecked(),
                        );
                        let mut start = 0;
                        for line in lines {
                            self.line_pos += 1;
                            std::ptr::copy_nonoverlapping(
                                line.as_ptr(),
                                buf.as_ptr().add(start),
                                line.len(),
                            );
                            start += line.len();
                        }
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            buf.as_ptr(),
                            start,
                        ))
                        .trim()
                    }
                }
            } else {
                self.line_pos += 1;
                let (line, left) = split_nl(self.s).unwrap_or((self.s, ""));
                self.s = left;
                self.rename_line(line.trim_end_matches('\r'), b)
            };
            self.span_end = if !self.s.is_empty() {
                // skip newline
                self.current_pos() - 1
            } else {
                self.current_pos()
            };

            let bytes = line.as_bytes();

            if self.is_header && bytes.first() == Some(&b'[') {
                // An ERH is `#` lines and nothing else
                // (`GameProc/HeaderFileLoader.cs:96-103`,
                // `ヘッダーの中に#で始まらない行があります`): the `[…]`
                // preprocessor is not read there at all, and a `[[…]]` the
                // rename table could not resolve fails the same way.
                return Err((pp_msg::not_sharp_line_in_header(), self.span()));
            }

            // A directive is recognised *before* the disabled state is
            // honoured (`GameProc/ErbLoader.cs:325-343` runs `AddKeyWord` and
            // only then `if (ppstate.Disabled) continue;`), so regions keep
            // nesting and closing inside a skipped one.
            if bytes.first() == Some(&b'[') && bytes.get(1) != Some(&b'[') {
                self.directive(line);
                continue;
            }

            if self.pp.disabled {
                continue;
            }

            if line.starts_with("[[") {
                // `apply_rename` already spliced in every `[[…]]` it had an
                // entry for, so one surviving to here has none. Emuera
                // reports exactly this, at the point its lexer reaches the
                // token (`Sub/LexicalAnalyzer.cs:865-880`).
                let Some(end_pos) = line.find("]]") else {
                    return Err(("No matched `]]`".into(), self.span()));
                };

                return Err((
                    format!("No matched rename key `{}`", line[2..end_pos].trim()),
                    self.span(),
                ));
            }

            break Ok(line);
        }
    }

    /// One `[…]` line, tokenized the way `loadErb` does
    /// (`GameProc/ErbLoader.cs:325-341`).
    fn directive(&mut self, line: &str) {
        // `st.ShiftNext()` past the `[`, `ReadSingleIdentifier`,
        // `SkipWhiteSpace`, `ReadSingleIdentifier` again.
        let (token, rest) = utils::cut_ident(&line[1..]);
        let rest = self.skip_directive_ws(rest);
        let (token2, rest) = utils::cut_ident(rest);

        // `if ((string.IsNullOrEmpty(token)) || (st.Current != ']'))`. The
        // keyword still runs afterwards, so `[IF` with no `]` opens a region
        // exactly as `[IF …]` would.
        if token.is_empty() || !rest.starts_with(']') {
            self.warn(pp_msg::invalid_sbrackets());
        }

        self.add_keyword(token, token2);

        // `st.ShiftNext(); if (!st.EOS)`: one character is stepped over — the
        // `]` when it is there — and the end-of-line test is then strict, so
        // a trailing space or comment counts as text after the directive.
        let mut tail = rest.chars();
        tail.next();
        if !tail.as_str().is_empty() {
            self.warn(pp_msg::ignore_after_preprosessor(token));
        }
    }

    /// `PPState.AddKeyWord` (`GameProc/ErbLoader.cs:150-280`).
    ///
    /// Every unhandled shape is a level-1 warning and leaves the state alone,
    /// including the `[ELSEIF]`/`[ELSE]`/`[ENDIF]`/`[SKIPEND]` mismatches
    /// whose stack pop happens *before* the check and is not undone.
    fn add_keyword(&mut self, token: &str, token2: &str) {
        match SquareCode::from_str(token) {
            Ok(SquareCode::SKIPSTART) => {
                if !token2.is_empty() {
                    self.warn(pp_msg::has_too_many_arg(token));
                } else if self.pp.skip {
                    self.warn(pp_msg::duplicate_skipstart());
                } else {
                    self.pp.match_stack.push(PpExpect::SkipEnd);
                    self.pp.disabled_stack.push(self.pp.disabled);
                    self.pp.done_stack.push(self.pp.done);
                    self.pp.skip = true;
                    self.pp.disabled = true;
                    self.pp.done = false;
                }
            }
            Ok(SquareCode::IF_DEBUG) => {
                if !token2.is_empty() {
                    self.warn(pp_msg::has_too_many_arg(token));
                } else {
                    self.push_if(!self.debug_mode);
                }
            }
            Ok(SquareCode::IF_NDEBUG) => {
                if !token2.is_empty() {
                    self.warn(pp_msg::has_too_many_arg(token));
                } else {
                    self.push_if(self.debug_mode);
                }
            }
            Ok(SquareCode::IF) => {
                if token2.is_empty() {
                    self.warn(pp_msg::missing_arguments(token));
                } else {
                    let disabled = !self.is_macro_defined(token2);
                    self.push_if(disabled);
                }
            }
            Ok(SquareCode::ELSEIF) => {
                if token2.is_empty() {
                    self.warn(pp_msg::missing_arguments(token));
                } else if self.pp.match_stack.pop() != Some(PpExpect::ElseIf) {
                    self.warn(pp_msg::is_invalid("[ELSEIF]"));
                } else {
                    self.pp.match_stack.push(PpExpect::ElseIf);
                    self.pp.disabled = self.pp.done || !self.is_macro_defined(token2);
                    self.pp.done |= !self.pp.disabled;
                }
            }
            Ok(SquareCode::ELSE) => {
                if !token2.is_empty() {
                    self.warn(pp_msg::has_too_many_arg(token));
                } else if self.pp.match_stack.pop() != Some(PpExpect::ElseIf) {
                    self.warn(pp_msg::is_invalid("[ELSE]"));
                } else {
                    self.pp.match_stack.push(PpExpect::EndIf);
                    self.pp.disabled = self.pp.done;
                    self.pp.done = true;
                }
            }
            Ok(SquareCode::SKIPEND) => {
                if !token2.is_empty() {
                    self.warn(pp_msg::has_too_many_arg(token));
                } else if self.pp.match_stack.pop() != Some(PpExpect::SkipEnd) {
                    self.warn(pp_msg::unexpected_skipend());
                } else {
                    self.pp.skip = false;
                    self.pop_region();
                }
            }
            Ok(SquareCode::ENDIF) => {
                if !token2.is_empty() {
                    self.warn(pp_msg::has_too_many_arg(token));
                } else if !matches!(
                    self.pp.match_stack.pop(),
                    Some(PpExpect::EndIf | PpExpect::ElseIf)
                ) {
                    self.warn(pp_msg::unexpected_macro_endif());
                } else {
                    self.pop_region();
                }
            }
            Err(_) => self.warn(pp_msg::unrecognized_preprosessor()),
        }

        // `if (skip) Disabled = true;` closes every case: nothing re-enables
        // lines while a `[SKIPSTART]` is open.
        if self.pp.skip {
            self.pp.disabled = true;
        }
    }

    /// Opens an `[IF]`/`[IF_DEBUG]`/`[IF_NDEBUG]` region.
    fn push_if(&mut self, disabled: bool) {
        self.pp.match_stack.push(PpExpect::ElseIf);
        self.pp.disabled_stack.push(self.pp.disabled);
        self.pp.done_stack.push(self.pp.done);
        self.pp.disabled = disabled;
        self.pp.done = !disabled;
    }

    /// Restores the state an `[ENDIF]`/`[SKIPEND]` closes.
    fn pop_region(&mut self) {
        // These two stacks can only be longer than `match_stack`, never
        // shorter: an entry is pushed onto all three together, `[ELSE]` and
        // `[ELSEIF]` pop and push `match_stack` in the same breath, and a
        // mismatched pop above discards a `match_stack` entry without
        // touching these. So the fallback is unreachable — and it is a
        // fallback rather than an `unwrap` because the input deciding it is
        // the game's own source text.
        self.pp.disabled = self.pp.disabled_stack.pop().unwrap_or(false);
        self.pp.done = self.pp.done_stack.pop().unwrap_or(false);
    }

    /// Is `name` a `#DEFINE`d macro?
    ///
    /// `[IF name]` asks `GetMacro` (`GameData/IdentifierDictionary.cs:470-477`),
    /// which uppercases the name while `変数名の大文字小文字を無視する` is on —
    /// its default — and `#DEFINE` registers the uppercased name
    /// (`GameProc/HeaderFileLoader.cs:164-165`). erars interns a macro name
    /// through `ident_no_case` (`erars-compiler/src/parser/expr.rs:177-201`),
    /// which is the same uppercasing, so the lookup has to repeat it. Only
    /// `#DEFINE` counts: `AddMacro` (`:451-455`) is the only writer of that
    /// dictionary, so a `#DIM CONST` of the same name does not enable the
    /// region. An empty macro body is still a definition
    /// (`GameProc/HeaderFileLoader.cs:184-191`, 「空マクロの許可」).
    fn is_macro_defined(&self, name: &str) -> bool {
        let Some(macros) = self.macros else {
            return false;
        };

        if name.bytes().all(|b| b.is_ascii() && !b.is_ascii_lowercase()) {
            macros.contains_key(name)
        } else {
            macros.contains_key(name.cow_to_uppercase().as_ref())
        }
    }

    /// `LexicalAnalyzer.SkipWhiteSpace` (`Sub/LexicalAnalyzer.cs:737-767`) as
    /// it applies between a directive's two identifiers.
    fn skip_directive_ws<'a>(&self, mut s: &'a str) -> &'a str {
        loop {
            s = s.trim_start_matches([' ', '\t', '\u{3000}']);

            match utils::marker_len(s.as_bytes(), self.debug_mode) {
                Some(len) => s = &s[len..],
                // A `;` here seeks to end of line, which leaves no second
                // identifier and no `]`.
                None if s.starts_with(';') => return "",
                None => return s,
            }
        }
    }

    pub fn next_line<'a>(
        &mut self,
        b: &'a Bump,
    ) -> Result<Option<EraLine<'a>>, (String, Range<usize>)>
    where
        's: 'a,
    {
        let line = self.next_raw_line(b)?;

        debug_assert!(!line.ends_with('\n'));

        if line.is_empty() {
            return Ok(None);
        }

        let (ident, args) = utils::cut_ident(line);

        // Bound before the `if let` so the `&mut self` reborrow ends here and
        // the branches below can still call `self.span()`. The memo folds case
        // itself, so the word does not have to be uppercased — which used to
        // allocate on every line whose first word had a lowercase letter.
        let inst = self.inst_memo.get(ident);

        if let Some(code) = inst {
            let args = match code {
                InstructionCode::REUSELASTLINE | InstructionCode::THROW => {
                    utils::strip_inst_separator(args)
                }
                _ => utils::cut_comment(args.trim_start_matches(' '), self.debug_mode, b),
            };
            match code {
                // The initialiser is split off here so the parser can lower it
                // to an ordinary assignment at this position; `#DIM`'s own
                // initialiser list would instead be applied when the variable
                // is created, which for a dynamic local is function entry.
                InstructionCode::VARI | InstructionCode::VARS => {
                    let (decl, init) = match lex_assign_line(args) {
                        Some((decl, init)) => (decl, Some(init)),
                        None => (args, None),
                    };
                    Ok(Some(EraLine::VarDecl {
                        is_str: code == InstructionCode::VARS,
                        decl,
                        init,
                    }))
                }
                _ => Ok(Some(EraLine::InstLine { inst: code, args })),
            }
        } else if let Some(left) = utils::strip_prefix_ignore_case(ident, "PRINT") {
            let (flags, ty) = utils::parse_print_left(left);
            let args = if !(ty == PrintType::Plain || ty == PrintType::Form) {
                utils::cut_comment(args, self.debug_mode, b)
            } else {
                utils::strip_inst_separator(args)
            };
            Ok(Some(EraLine::PrintLine { flags, ty, args }))
        } else {
            let line = utils::cut_comment(line, self.debug_mode, b).trim_start();
            if ident.is_empty() {
                if line.is_empty() {
                    Ok(None)
                } else if let Some(line) = line.strip_prefix('#') {
                    let (ident, args) = utils::cut_ident(line);
                    if let Some(sharp) = SharpCode::iter()
                        .find(|code| ident.eq_ignore_ascii_case(<&str>::from(code)))
                    {
                        Ok(Some(EraLine::SharpLine {
                            sharp,
                            args: args.trim_start(),
                        }))
                    } else {
                        Err((format!("[lexer] Unknown sharp line: {line}"), self.span()))
                    }
                } else if let Some(line) = line.strip_prefix('@') {
                    Ok(Some(EraLine::FunctionLine(line)))
                } else if let Some(line) = line.strip_prefix('$') {
                    Ok(Some(EraLine::GotoLine(line)))
                } else if let Some(left) = line.strip_prefix("++") {
                    Ok(Some(EraLine::VarInc {
                        lhs: left,
                        is_pre: true,
                        is_inc: true,
                    }))
                } else if let Some(left) = line.strip_prefix("--") {
                    Ok(Some(EraLine::VarInc {
                        lhs: left,
                        is_pre: true,
                        is_inc: false,
                    }))
                } else {
                    Err((format!("[lexer] Unknown line: {line}"), self.span()))
                }
            } else if let Some((mut left, right)) = lex_assign_line(line) {
                let complex_op = if let Some(l) = left.strip_suffix('+') {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::Add))
                } else if let Some(l) = left.strip_suffix('-') {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::Sub))
                } else if let Some(l) = left.strip_suffix('*') {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::Mul))
                } else if let Some(l) = left.strip_suffix('/') {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::Div))
                } else if let Some(l) = left.strip_suffix('%') {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::Rem))
                } else if let Some(l) = left.strip_suffix("<<") {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::Lhs))
                } else if let Some(l) = left.strip_suffix(">>") {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::Rhs))
                } else if let Some(l) = left.strip_suffix('^') {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::BitXor))
                } else if let Some(l) = left.strip_suffix('|') {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::BitOr))
                } else if let Some(l) = left.strip_suffix('&') {
                    left = l;
                    Some(ComplexAssign::Bin(BinaryOperator::BitAnd))
                } else if let Some(l) = left.strip_suffix('\'') {
                    left = l;
                    Some(ComplexAssign::Str)
                } else {
                    None
                };

                Ok(Some(EraLine::VarAssign {
                    lhs: left,
                    complex_op,
                    rhs: right,
                }))
            } else if let Some(left) = line.trim_end().strip_suffix("++") {
                Ok(Some(EraLine::VarInc {
                    lhs: left,
                    is_pre: false,
                    is_inc: true,
                }))
            } else if let Some(left) = line.trim_end().strip_suffix("--") {
                Ok(Some(EraLine::VarInc {
                    lhs: left,
                    is_pre: false,
                    is_inc: false,
                }))
            } else if let Ok(meth) = ident.cow_to_ascii_uppercase().parse::<BuiltinMethod>() {
                // Emuera's method-as-instruction fallback
                // (`FunctionIdentifier.cs:428-436`). It sits after the
                // assignment and increment forms so that it can only give a
                // meaning to a line that would otherwise be an error.
                Ok(Some(EraLine::MethodLine {
                    meth,
                    args: utils::cut_ident(line).1.trim_start_matches(' '),
                }))
            } else {
                Err((format!("[lexer] Unknown line: {line}"), self.span()))
            }
        }
    }

    pub fn span(&self) -> std::ops::Range<usize> {
        self.span_begin..self.span_end
    }

    pub fn script_pos(&self) -> ScriptPosition {
        ScriptPosition {
            line: self.line_pos as _,
        }
    }
}

fn lex_assign_line(line: &str) -> Option<(&str, &str)> {
    if line.starts_with('=') {
        return None;
    }

    unsafe {
        let mut iter = memchr::memchr_iter(b'=', line.as_bytes());

        while let Some(pos) = iter.next() {
            match line.as_bytes().get(pos + 1).copied() {
                Some(b'=') => {
                    iter.next();
                    continue;
                }
                Some(_) => match line.as_bytes().get(pos - 1).copied() {
                    Some(b'!') => {
                        continue;
                    }
                    _ => return Some((line.get_unchecked(..pos), line.get_unchecked(pos + 1..))),
                },
                None => {
                    return Some((line, ""));
                }
            }
        }

        None
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum EraLine<'s> {
    FunctionLine(&'s str),
    SharpLine {
        sharp: SharpCode,
        args: &'s str,
    },
    GotoLine(&'s str),

    PrintLine {
        flags: PrintFlags,
        ty: PrintType,
        args: &'s str,
    },

    InstLine {
        inst: InstructionCode,
        args: &'s str,
    },

    /// A built-in *method* used where an instruction is expected.
    ///
    /// Emuera registers every method as a line-head instruction as well
    /// (`GameProc/Function/FunctionIdentifier.cs:428-436`: each entry of
    /// `FunctionMethodCreator.GetMethodList()` that is not already in
    /// `funcDic` is added with `METHOD_Instruction`). Executing one evaluates
    /// the method and stores the result in `RESULT`/`RESULTS`
    /// (`GameProc/Function/Instraction.Child.cs:487-498`) — exactly what
    /// `Stmt::Method` compiles to.
    ///
    /// The `!funcDic.ContainsKey(key)` guard is why this is resolved only
    /// after [`InstructionCode`]: a name that is already an instruction keeps
    /// its instruction meaning.
    MethodLine {
        meth: BuiltinMethod,
        args: &'s str,
    },

    /// `VARI`/`VARS`: the `.NET版` fork's function-local declaration.
    ///
    /// `decl` is the `#DIM`-shaped `NAME[, SIZE]*` part and `init` the raw
    /// text after the `=`, which the fork evaluates where the line sits rather
    /// than at variable creation.
    VarDecl {
        is_str: bool,
        decl: &'s str,
        init: Option<&'s str>,
    },

    VarInc {
        lhs: &'s str,
        is_pre: bool,
        is_inc: bool,
    },

    VarAssign {
        lhs: &'s str,
        complex_op: Option<ComplexAssign>,
        rhs: &'s str,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ComplexAssign {
    Bin(BinaryOperator),
    Str,
}

#[derive(Clone, Copy, Debug, Logos)]
#[logos(skip "\u{FEFF}")]
#[logos(skip r"[ \t\r\n　]+")]
#[logos(skip r"[；;][^\n]*")]
pub enum ConfigToken<'s> {
    #[regex(r"[^:\r\n\u{FEFF}][^:\r\n]*:[^\r\n]*", |lex| lex.slice().split_once(':').unwrap())]
    Line((&'s str, &'s str)),
}
