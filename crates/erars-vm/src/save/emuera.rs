//! Reading (never writing) real Emuera's own save format.
//!
//! erars's native format (see the parent module) is gzip + `rmp_serde` behind
//! a 4-byte magic erars invented for itself. Real Emuera never wrote that
//! magic, so any file that does not start with it is either corrupt or a
//! genuine Emuera save, and [`super::RawSaveData::from_bytes`] dispatches
//! here to find out which.
//!
//! Byte-level grammar is documented in
//! `docs/research/2026-09-06-emuera-save-format.md` — originally derived
//! from decompiled IL of Emuera 1.818 (`Emuera1818_kr3.exe`) and re-verified
//! against the real Emuera C# source (uEmuera, `Assets/Scripts/Emuera/`,
//! Emuera 1824v15) in
//! `docs/research/2026-09-07-emuera-source-crosscheck.md`. This module
//! implements exactly what the spec establishes and refuses, cleanly,
//! whatever it marks unsupported rather than guessing at it. See
//! [`EmueraSaveVariant`] for the container forms Emuera itself can produce
//! and which of them this module accepts.
//!
//! ## Scope: read-only, numbered saves and `global.sav` only
//!
//! erars never writes this format back: a slot loaded from an Emuera save
//! that is later `SAVEDATA`'d becomes erars's own native
//! `save{idx:02}.rsav.gz` from that point on (`super::write_save_data`), and
//! the original `save{idx:02}.sav` is left untouched on disk, never deleted
//! or overwritten. `SAVECHARA`/`LOADCHARA` (`chara_*.dat`) and
//! `SAVEVAR`/`LOADVAR` (`var_*.dat`) compatibility is a separate, unstarted
//! piece of work — [`super::read_chara_data`]/[`super::read_var_data`]
//! recognise a foreign file there and refuse it with a message that says so,
//! rather than either silently ignoring it or failing with a generic parse
//! error a user can't act on.
//!
//! ## Container variant and text encoding
//!
//! Real Emuera picks the container per the `SystemSaveInBinary` config key
//! (spec §1) and, for text, the encoding per `SystemSaveInUTF8` (spec §1.2)
//! — a save file itself carries no reliable per-file marker of *which*
//! encoding it is (SJIS has no BOM; a UTF-8 save's BOM is only "expected,
//! not runtime-verified" per spec §6.1). This module sidesteps needing that
//! config at all: [`sniff`] tries UTF-8 first (stripping a BOM if present),
//! and only falls back to the game's own configured non-Unicode encoding
//! (`Language::encoding()` — SJIS for a Japanese game, EUC-KR/GBK/Big5 for a
//! Korean/Chinese one, matching the same mapping the rest of erars already
//! uses for script/console text) when the bytes are not valid UTF-8. This is
//! exact, not a heuristic: every byte Emuera's writer can emit outside a
//! string payload is plain ASCII, so a save that is genuinely
//! non-Unicode-encoded either decodes to something implausible as UTF-8 (an
//! invalid byte sequence — SJIS/EUC-KR/GBK/Big5 double-byte sequences reliably
//! do this for any real non-ASCII content) or consists entirely of ASCII
//! bytes, in which case decoding it as UTF-8 or as the configured encoding
//! produces an identical `String` either way.
//!
//! ## Variable mapping policy
//!
//! Every name in the foreign file is matched against *this game's currently
//! declared* [`VariableInfo`] (`HeaderInfo::global_variables`), not against
//! any shape the foreign file itself claims, and every mismatch is logged —
//! never silently dropped, never silently guessed:
//!
//! - Name not declared by this game at all: skipped. This mirrors the
//!   existing, established precedent for the same-format `LOADVAR`
//!   (`VariableStorage::restore_global_var`'s doc comment: Emuera's
//!   `LoadVariableBinary` "silently skips a name that is no longer a plain
//!   global variable") — the same reasoning applies to a name Emuera
//!   declared that this game doesn't.
//! - Declared but in the wrong scope (global file carries a non-global
//!   variable, or vice versa; a chara-scope name in a normal save): skipped.
//! - Declared but int/str type mismatch, or a different number of
//!   dimensions than the file's own record for that name: skipped. Not
//!   coercible, not safe to guess. (A string 2D/3D `SAVEDATA` variable can
//!   never actually appear in a text save at all — Emuera's own writer
//!   throws `NotImplementedException` for one, spec §2.5 — so such a name
//!   simply never has an entry to reconcile against; nothing to skip.)
//! - Declared, same shape (0 to 3 dimensions), different extent: this is the
//!   ordinary case — a `!VariableSize.csv` difference between the original
//!   game and its erars port — and is deliberately reconciled rather than
//!   discarded outright: 1D reuses
//!   [`VmVariable::overwrite_from`]'s existing "copy up to the shorter
//!   length, leave the remainder at its declared default" policy (the same
//!   policy erars's own same-format load already relies on for a CSV
//!   resize); 2D/3D apply the same policy per-axis (see [`place_2d`]),
//!   since Emuera's own row/column trimming means a flat truncate would
//!   misalign data across rows. Reported as a partial import, not silently
//!   accepted.
//!
//! `RANDDATA` is treated as an ordinary `int[4]` savedata variable: whatever
//! it holds is fed through the existing `VariableStorage::init_rand` call
//! that already runs at the end of every load, same-format or not. Emuera's
//! own PRNG algorithm differs from erars's ChaCha20 regardless, so there is
//! no real continuity to preserve either way — treating it like any other
//! array costs nothing and needs no special case.

use anyhow::{anyhow, bail, ensure, Result};
use erars_ast::{get_interner, StrKey, VariableInfo};
use erars_compiler::HeaderInfo;
use hashbrown::HashMap;

use crate::variable::{UniformVariable, VmVariable};

/// The container forms real Emuera can write (spec §1).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EmueraSaveVariant {
    /// `SystemSaveInBinary=NO` (default), `SystemSaveInUTF8=NO`: plain
    /// line-oriented text, decoded with the game's configured non-Unicode
    /// encoding (see the module doc comment — this variant is also reported
    /// for pure-ASCII text, which is byte-identical either way).
    TextSjis,
    /// `SystemSaveInBinary=NO`, `SystemSaveInUTF8=YES`, or any text save
    /// that is valid UTF-8: the same text grammar, UTF-8 (optionally
    /// BOM-prefixed).
    TextUtf8,
    /// `SystemSaveInBinary=YES`: Emuera's typed binary layout (spec §4),
    /// fully implemented and validated byte-for-byte against all three real
    /// binary captures (`save90_binary_real.sav`, `global_binary_real.sav`)
    /// — see [`parse_binary`].
    Binary,
}

/// "ERA" CR LF SUB LF — empirically confirmed against a real capture (spec
/// §4.1); an earlier IL-only reading misread this as `D4 A0 A0 0A`.
const BINARY_MAGIC: [u8; 8] = [0x89, 0x45, 0x52, 0x41, 0x0D, 0x0A, 0x1A, 0x0A];
const UTF8_BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];
const EMU_START: &str = "__EMUERA_1808_STRAT__";
const EMU_SEPARATOR: &str = "__EMU_SEPARATOR__";
const FINISHED: &str = "__FINISHED";

/// The legacy "OLD block" positional layout (spec §2.3), IL-confirmed
/// (`CharacterData::SaveToStream`/`LoadFromStream`, `.il:148923-149100`):
/// 2 string scalars, in this exact order. `VariableCode.NAME`/`CALLNAME`
/// are indices 0/1 of `dataString`; `__COUNT_SAVE_CHARACTER_STRING__ == 2`
/// bounds the loop to exactly these two — `NICKNAME`/`MASTERNAME` (indices
/// 2/3) are `SAVE_EXTENDED`-flagged and never appear here, only in the
/// extended chara section.
const CHAR_OLD_STR: [&str; 2] = ["NAME", "CALLNAME"];
/// `dataInteger[0..2)`: `VariableCode.ISASSI`/`NO`,
/// `__COUNT_SAVE_CHARACTER_INTEGER__ == 2`.
const CHAR_OLD_INT: [&str; 2] = ["ISASSI", "NO"];
/// `dataIntegerArray[0..17)`: `VariableCode.BASE` through `NOWEX`,
/// `__COUNT_SAVE_CHARACTER_INTEGER_ARRAY__ == 0x11 == 17` — `DOWNBASE`,
/// `CUP`, `CDOWN`, `TCVAR` (indices 17-20) are `SAVE_EXTENDED`-flagged and
/// never appear here. Order and membership byte-exact validated against
/// `save90_text_utf8_real.sav`: parsing this list positionally, right after
/// the 2 strings and 2 ints above, for every character, then
/// [`GLOBAL_OLD_ARR`] below, lands exactly on line 4672 —
/// `__EMUERA_1808_STRAT__` — with zero drift over 4671 preceding lines.
const CHAR_OLD_ARR: [&str; 17] = [
    "BASE", "MAXBASE", "ABL", "TALENT", "EXP", "MARK", "PALAM", "SOURCE", "EX", "CFLAG", "JUEL",
    "RELATION", "EQUIP", "TEQUIP", "STAIN", "GOTJUEL", "NOWEX",
];

/// The OLD block's global (`VariableData`) layout when embedded *inside a
/// local save* (`VariableData::SaveToStream`/`LoadFromStream`,
/// `.il:49290-49373`) — distinct from the dedicated `global.sav` layout,
/// see [`GLOBALSAVE_OLD_ARR`] below. IL-confirmed: `dataString[0..0)` and
/// `dataInteger[0..0)` are both dead loops (0 iterations — a local save's
/// OLD global block has no scalars at all), then `dataIntegerArray[0..60)`
/// (`VariableCode.DAY` through `NOTUSE_3B`, `__COUNT_SAVE_INTEGER_ARRAY__ ==
/// 0x3C == 60`), then `dataStringArray[0..1)` (just `SAVESTR`,
/// `__COUNT_SAVE_STRING_ARRAY__ == 1`). `NOTUSE_*` slots are real,
/// historically-reserved-but-unused positions Emuera itself still
/// reads/writes; they carry no declared variable in this game (or any
/// game) and are dropped by [`reconcile`]'s ordinary "not declared" path —
/// no special-casing needed.
const GLOBAL_OLD_ARR: [&str; 60] = [
    "DAY", "MONEY", "ITEM", "FLAG", "TFLAG", "UP", "PALAMLV", "EXPLV", "EJAC", "DOWN", "RESULT",
    "COUNT", "TARGET", "ASSI", "MASTER", "NOITEM", "LOSEBASE", "SELECTCOM", "ASSIPLAY", "PREVCOM",
    "NOTUSE_14", "NOTUSE_15", "TIME", "ITEMSALES", "PLAYER", "NEXTCOM", "PBAND", "BOUGHT",
    "NOTUSE_1C", "NOTUSE_1D", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "NOTUSE_38", "NOTUSE_39",
    "NOTUSE_3A", "NOTUSE_3B",
];

/// The OLD block's layout for a *standalone* `global.sav` file
/// (`VariableData::SaveGlobalToStream`/`LoadGlobalFromStream`,
/// `.il:50912-50944`) — a dedicated 2-field writer/reader, completely
/// separate from [`GLOBAL_OLD_ARR`]'s 60-array local-embedded layout:
/// exactly `dataIntegerArray[63]` (`VariableCode.GLOBAL`) then
/// `dataStringArray[5]` (`VariableCode.GLOBALS`). Byte-exact validated
/// against `global_text_utf8_real.sav` (`code`/`version` then `"100"` +
/// `__FINISHED` + `__FINISHED` lands exactly on `__EMUERA_1808_STRAT__`).
const GLOBALSAVE_OLD_ARR: &str = "GLOBAL";
const GLOBALSAVE_OLD_STR_ARR: &str = "GLOBALS";

/// One array Emuera wrote, before reconciliation against this game's
/// current declarations. `Str2D`/`Str3D` only ever come from a *binary*
/// save (type bytes 18/19, spec §4.3): Emuera's own *text* writer never
/// emits one (`NotImplementedException`, spec §2.5), so a name declared
/// that shape never appears in a parsed text file — only checked by
/// [`ParsedArray::is_str`]/[`ParsedArray::dim_count`] once the binary
/// reader has actually produced one.
pub enum ParsedArray {
    IntScalar(i64),
    StrScalar(String),
    Int1D(Vec<i64>),
    Str1D(Vec<String>),
    /// Rows in file order; each row already trimmed of trailing zero
    /// columns by Emuera's writer (spec §2.4), so a row's length is not the
    /// declared column count — only a lower bound on it.
    Int2D(Vec<Vec<i64>>),
    /// `(outer index, rows)` blocks in file order; an outer index absent
    /// from this list was entirely default and Emuera's writer omitted it.
    Int3D(Vec<(u32, Vec<Vec<i64>>)>),
    /// Binary-only (spec §4.3 type 18); rows already full-width (the
    /// binary reader materialises every declared cell, unlike the text
    /// writer's row-trimming).
    Str2D(Vec<Vec<String>>),
    /// Binary-only (spec §4.3 type 19); see [`ParsedArray::Str2D`].
    Str3D(Vec<(u32, Vec<Vec<String>>)>),
}

impl ParsedArray {
    fn is_str(&self) -> bool {
        matches!(self, Self::StrScalar(_) | Self::Str1D(_) | Self::Str2D(_) | Self::Str3D(_))
    }

    fn dim_count(&self) -> usize {
        match self {
            Self::IntScalar(_) | Self::StrScalar(_) => 0,
            Self::Int1D(_) | Self::Str1D(_) => 1,
            Self::Int2D(_) | Self::Str2D(_) => 2,
            Self::Int3D(_) | Self::Str3D(_) => 3,
        }
    }
}

/// What the seek for the extended block actually found. The OLD block is
/// always parsed first and is version-independent (spec §2.3); this records
/// which extended-block marker (if any) follows, which decides whether the
/// save's extended-only variables (`NICKNAME`, `MASTERNAME`, `CSTR`,
/// `CDFLAG`, and every user `#DIM SAVEDATA` array) were imported or are
/// being dropped — a load the player must be told about rather than one
/// that silently passes as "complete".
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExtendedMarker {
    /// A marker whose extended grammar this reader implements, carrying its
    /// Emuera version: 1700/1708/1729/1803/1808. The extended block is read
    /// with per-version group gating (2D iff >= 1708, 3D iff >= 1729, the
    /// six user-defined groups iff >= 1808; chara 2D iff >= 1803) — see
    /// `docs/research/2026-09-07-emuera-source-crosscheck.md` §6.
    Known(u32),
    /// An unknown/future `__EMUERA_..._STRAT__` marker whose grammar this
    /// reader does not implement; its exact text is kept so the report can
    /// name it. The extended block is *not* read.
    Unknown(String),
    /// No extended marker was found at all (a save written before the
    /// extended block existed, or truncated); no extended block exists.
    Absent,
}

impl Default for ExtendedMarker {
    fn default() -> Self {
        // `parse_binary` sets this explicitly; the default is the common
        // 1808-framed text case, so [`ImportReport`]/callers built without
        // one short-circuit to "nothing to warn about".
        ExtendedMarker::Known(1808)
    }
}

/// Everything read out of one Emuera save file's variable payload, in
/// foreign-name-keyed form — `String`s, not `StrKey`s: a name this game
/// never interned cannot become a `StrKey` at all, which is itself the
/// "not declared" case [`reconcile`] reports.
#[derive(Default)]
pub struct EmueraSaveData {
    /// Every non-chara variable the save carries (spec §2.5 "Variable
    /// section": built-in scalars/1D/2D/3D plus the 6 user-defined array
    /// groups, all merged into one name-keyed map — the built-in/user split
    /// only matters to Emuera's own writer layout, not to reconciliation).
    pub globals: HashMap<String, ParsedArray>,
    /// One row per `CHARADATA` entry (spec §2.5 "Character section"), in
    /// the file's own order.
    pub charas: Vec<HashMap<String, ParsedArray>>,
    /// Which extended-block marker the seek found (see [`ExtendedMarker`]);
    /// the OLD block is always read regardless. Populated by [`parse_text`];
    /// binary saves are inherently 1808-framed, so [`parse_binary`] sets
    /// [`ExtendedMarker::Known(1808)`].
    pub extended_marker: ExtendedMarker,
}

/// Sniffs which [`EmueraSaveVariant`] `bytes` (the whole file, not yet
/// gzip-decoded or otherwise transformed — real Emuera saves are never
/// wrapped the way erars's own `.rsav.gz` is) is written in, or `None` if it
/// is not recognisable as any Emuera save variant at all. `non_unicode` is
/// the game's own configured encoding, tried only as a fallback when the
/// bytes are not valid UTF-8 (see the module doc comment).
///
/// Deliberately conservative: an unrecognised file must fail cleanly
/// (`RawSaveData::from_bytes` turns `None` here into an ordinary "no such
/// slot"-style error, never a panic and never a silent empty load), so this
/// only ever claims a variant whose first line is plausibly Emuera's own
/// `ScriptUniqueCode` — a line of decimal digits.
pub fn sniff(bytes: &[u8], non_unicode: &'static encoding_rs::Encoding) -> Option<EmueraSaveVariant> {
    if bytes.starts_with(&BINARY_MAGIC) {
        return Some(EmueraSaveVariant::Binary);
    }

    if let Some(rest) = bytes.strip_prefix(&UTF8_BOM) {
        return std::str::from_utf8(rest)
            .ok()
            .filter(|text| looks_like_header(text))
            .map(|_| EmueraSaveVariant::TextUtf8);
    }

    if let Ok(text) = std::str::from_utf8(bytes) {
        if looks_like_header(text) {
            return Some(EmueraSaveVariant::TextUtf8);
        }
    }

    let (text, _, had_errors) = non_unicode.decode(bytes);
    if !had_errors && looks_like_header(&text) {
        return Some(EmueraSaveVariant::TextSjis);
    }

    None
}

/// A real Emuera text save's first line is always `ScriptUniqueCode` —
/// plain decimal digits (`EraDataWriter::Write(int64)`, spec §2.4).
fn looks_like_header(text: &str) -> bool {
    text.lines()
        .next()
        .is_some_and(|line| !line.is_empty() && line.bytes().all(|b| b.is_ascii_digit()))
}

/// Parses `bytes` (the whole file) as `variant`. `is_global` selects the
/// local-save header shape (spec §2.1: description + character count lines
/// present) versus the global-save shape (spec §2.2: neither) — for text,
/// this also picks between [`parse_variable_section`] (local: 8 built-in +
/// 6 user groups) and [`parse_global_variable_section`] (global: 6 groups,
/// no scalars at all — real Emuera's global save has no scalar groups,
/// confirmed against `global_text_utf8_real.sav`). `Binary` dispatches to
/// [`parse_binary`], which reads the same shape straight off the typed
/// byte stream instead of off lines.
pub fn parse(
    variant: EmueraSaveVariant,
    bytes: &[u8],
    non_unicode: &'static encoding_rs::Encoding,
    is_global: bool,
) -> Result<(EmueraSaveData, u32, u32, String)> {
    match variant {
        EmueraSaveVariant::Binary => parse_binary(bytes, is_global),
        EmueraSaveVariant::TextUtf8 => {
            let bytes = bytes.strip_prefix(&UTF8_BOM).unwrap_or(bytes);
            let text = std::str::from_utf8(bytes)?;
            parse_text(text, is_global)
        }
        EmueraSaveVariant::TextSjis => {
            let (text, _, had_errors) = non_unicode.decode(bytes);
            ensure!(!had_errors, "세이브 파일이 {}로 디코딩되지 않습니다", non_unicode.name());
            parse_text(&text, is_global)
        }
    }
}

/// A cursor over a text save's `\r\n`/`\n`-separated lines (`str::lines`
/// strips both transparently). Real Emuera never emits a blank *terminal*
/// line, so running past the end is always a malformed/truncated file.
struct LineCursor<'a> {
    lines: std::str::Lines<'a>,
}

impl<'a> LineCursor<'a> {
    fn new(text: &'a str) -> Self {
        Self { lines: text.lines() }
    }

    fn next(&mut self) -> Result<&'a str> {
        self.lines
            .next()
            .ok_or_else(|| anyhow!("Emuera 세이브 파일이 예상보다 짧습니다"))
    }

    fn next_i64(&mut self) -> Result<i64> {
        let line = self.next()?;
        line.trim()
            .parse()
            .map_err(|e| anyhow!("정수가 아닙니다: {line:?} ({e})"))
    }

    /// The Emuera version a recognized extended-block marker stands for, or
    /// `None` for an unknown/future marker. Real Emuera's `SeekEmuStart`
    /// recognizes exactly these five (`EraDataStream.cs:46-50, 129-165`).
    fn marker_version(line: &str) -> Option<u32> {
        match line {
            "__EMUERA_STRAT__" => Some(1700),
            "__EMUERA_1708_STRAT__" => Some(1708),
            "__EMUERA_1729_STRAT__" => Some(1729),
            "__EMUERA_1803_STRAT__" => Some(1803),
            EMU_START => Some(1808),
            _ => None,
        }
    }

    /// Consumes lines up to and including the first extended-block marker,
    /// returning which one was found. Real Emuera's own `SeekEmuStart`
    /// (`.il:142958-143054`; `EraDataStream.cs:129-165`) accepts all five
    /// markers and reports whether the extended block follows; this reader
    /// implements all five grammars via [`Self::marker_version`], so a
    /// recognized marker maps to [`ExtendedMarker::Known`] with its
    /// version. A `__EMUERA_..._STRAT__` marker this reader does not
    /// recognize (a future version) is [`ExtendedMarker::Unknown`] —
    /// reported, not read — and no marker at all (a save written before
    /// the extended block existed) is [`ExtendedMarker::Absent`]
    /// (`docs/research/2026-09-07-emuera-source-crosscheck.md` §6.3). The
    /// OLD block is never optional to *read*, only the extended block
    /// following it is.
    fn seek_emu_start(&mut self) -> ExtendedMarker {
        for line in self.lines.by_ref() {
            if let Some(version) = Self::marker_version(line) {
                return ExtendedMarker::Known(version);
            }
            // Shape-match any other `__EMUERA_..._STRAT__` (including the
            // 1700-vintage `__EMUERA_STRAT__`, which is handled above) so an
            // unknown future marker is also reported rather than swallowed.
            // Only markers live here: the seek runs between the end of the
            // OLD block and the start of the extended block, where no value
            // line can look like a marker.
            if line.starts_with("__EMUERA_") && line.ends_with("STRAT__") {
                return ExtendedMarker::Unknown(line.to_owned());
            }
        }
        ExtendedMarker::Absent
    }

    /// One value per line until `__FINISHED`, with no leading `KEY` line —
    /// the OLD block's arrays are positional (Emuera's own classic index,
    /// never a name), unlike [`Self::read_1d_arrays`]'s self-naming
    /// extended-block groups.
    fn read_old_int_array(&mut self) -> Result<Vec<i64>> {
        let mut vals = Vec::new();
        loop {
            let line = self.next()?;
            if line == FINISHED {
                return Ok(vals);
            }
            vals.push(
                line.trim()
                    .parse::<i64>()
                    .map_err(|e| anyhow!("OLD 배열 값이 정수가 아닙니다: {line:?} ({e})"))?,
            );
        }
    }

    /// As [`Self::read_old_int_array`], for a string array.
    fn read_old_str_array(&mut self) -> Result<Vec<String>> {
        let mut vals = Vec::new();
        loop {
            let line = self.next()?;
            if line == FINISHED {
                return Ok(vals);
            }
            vals.push(line.to_owned());
        }
    }


    /// One `KEY:VALUE` scalar section (spec §2.4 `WriteExtended`): reads
    /// `KEY:VALUE` lines until `EMU_SEPARATOR`, converting each value with
    /// `to_value`.
    fn read_scalars(
        &mut self,
        out: &mut HashMap<String, ParsedArray>,
        to_value: impl Fn(&'a str) -> Result<ParsedArray>,
    ) -> Result<()> {
        loop {
            let line = self.next()?;
            if line == EMU_SEPARATOR {
                return Ok(());
            }
            let (key, value) = line
                .split_once(':')
                .ok_or_else(|| anyhow!("스칼라 변수 줄이 아닙니다: {line:?}"))?;
            out.insert(key.to_owned(), to_value(value)?);
        }
    }

    /// One 1D-array group (spec §2.4 `Write(int64[])`/`Write(string[])`):
    /// repeated `KEY` line + values + `FINISHED`, until `EMU_SEPARATOR`.
    fn read_1d_arrays(
        &mut self,
        out: &mut HashMap<String, ParsedArray>,
        is_str: bool,
    ) -> Result<()> {
        loop {
            let key = self.next()?;
            if key == EMU_SEPARATOR {
                return Ok(());
            }

            let mut ints = Vec::new();
            let mut strs = Vec::new();
            loop {
                let line = self.next()?;
                if line == FINISHED {
                    break;
                }
                if is_str {
                    strs.push(line.to_owned());
                } else {
                    ints.push(
                        line.trim()
                            .parse::<i64>()
                            .map_err(|e| anyhow!("1D 배열 값이 정수가 아닙니다: {line:?} ({e})"))?,
                    );
                }
            }

            out.insert(
                key.to_owned(),
                if is_str {
                    ParsedArray::Str1D(strs)
                } else {
                    ParsedArray::Int1D(ints)
                },
            );
        }
    }

    /// One comma-row until `FINISHED` or `}` — shared by 2D and the
    /// per-index blocks of 3D (spec §2.4).
    fn read_row(line: &str) -> Result<Vec<i64>> {
        if line.is_empty() {
            return Ok(Vec::new());
        }
        line.split(',')
            .map(|s| {
                s.trim()
                    .parse::<i64>()
                    .map_err(|e| anyhow!("2D/3D 배열 값이 정수가 아닙니다: {line:?} ({e})"))
            })
            .collect()
    }

    /// One int-2D array group (spec §2.4/§2.5): repeated `KEY` line +
    /// comma-rows + `FINISHED`, until `EMU_SEPARATOR`.
    fn read_2d_arrays(&mut self, out: &mut HashMap<String, ParsedArray>) -> Result<()> {
        loop {
            let key = self.next()?;
            if key == EMU_SEPARATOR {
                return Ok(());
            }

            let mut rows = Vec::new();
            loop {
                let line = self.next()?;
                if line == FINISHED {
                    break;
                }
                rows.push(Self::read_row(line)?);
            }

            out.insert(key.to_owned(), ParsedArray::Int2D(rows));
        }
    }

    /// One int-3D array group (spec §2.4): repeated `KEY` line + `[idx]{`
    /// blocks of comma-rows terminated by `}`, then `FINISHED`, until
    /// `EMU_SEPARATOR`.
    fn read_3d_arrays(&mut self, out: &mut HashMap<String, ParsedArray>) -> Result<()> {
        loop {
            let key = self.next()?;
            if key == EMU_SEPARATOR {
                return Ok(());
            }

            let mut blocks = Vec::new();
            loop {
                let line = self.next()?;
                if line == FINISHED {
                    break;
                }
                let idx_str = line
                    .strip_prefix('[')
                    .and_then(|s| s.strip_suffix("]{"))
                    .ok_or_else(|| anyhow!("3D 배열 인덱스 줄이 아닙니다: {line:?}"))?;
                let idx: u32 = idx_str
                    .parse()
                    .map_err(|e| anyhow!("3D 배열 인덱스가 정수가 아닙니다: {idx_str:?} ({e})"))?;

                let mut rows = Vec::new();
                loop {
                    let row_line = self.next()?;
                    if row_line == "}" {
                        break;
                    }
                    rows.push(Self::read_row(row_line)?);
                }
                blocks.push((idx, rows));
            }

            out.insert(key.to_owned(), ParsedArray::Int3D(blocks));
        }
    }

    /// A group that Emuera's text writer never populates — string 2D/3D,
    /// built-in or user-defined (`NotImplementedException` in the real
    /// writer, spec §2.5) — so the very next line must already be the
    /// group's own closing separator.
    fn expect_empty_group(&mut self) -> Result<()> {
        let line = self.next()?;
        ensure!(
            line == EMU_SEPARATOR,
            "문자열 2D/3D 배열은 Emuera 텍스트 세이브에 존재할 수 없습니다: {line:?}"
        );
        Ok(())
    }
}

/// One character's OLD block (spec §2.3, corrected — see [`CHAR_OLD_ARR`]):
/// 2 string scalars, 2 int scalars, then 17 named 1D int arrays, all
/// positional. This is where a real save's actual character state lives —
/// `BASE`, `ABL`, `TALENT`, `CFLAG`, etc. — the extended chara section
/// ([`parse_chara_section`]) only ever adds `NICKNAME`/`MASTERNAME`/`CSTR`/
/// `CDFLAG` and any further custom chara-scope savedata arrays this game
/// declares.
fn parse_old_chara_block(cursor: &mut LineCursor) -> Result<HashMap<String, ParsedArray>> {
    let mut vars = HashMap::new();
    for name in CHAR_OLD_STR {
        vars.insert(name.to_owned(), ParsedArray::StrScalar(cursor.next()?.to_owned()));
    }
    for name in CHAR_OLD_INT {
        vars.insert(name.to_owned(), ParsedArray::IntScalar(cursor.next_i64()?));
    }
    for name in CHAR_OLD_ARR {
        vars.insert(name.to_owned(), ParsedArray::Int1D(cursor.read_old_int_array()?));
    }
    Ok(vars)
}

/// The OLD block's global (`VariableData`) section when embedded inside a
/// local save (spec §2.3, corrected — see [`GLOBAL_OLD_ARR`]): 60 named 1D
/// int arrays (`DAY`, `MONEY`, `FLAG`, ... ) then one named 1D string array
/// (`SAVESTR`), all positional, no scalars.
fn parse_old_variable_block(cursor: &mut LineCursor) -> Result<HashMap<String, ParsedArray>> {
    let mut vars = HashMap::new();
    for name in GLOBAL_OLD_ARR {
        vars.insert(name.to_owned(), ParsedArray::Int1D(cursor.read_old_int_array()?));
    }
    vars.insert("SAVESTR".to_owned(), ParsedArray::Str1D(cursor.read_old_str_array()?));
    Ok(vars)
}

/// The OLD block for a standalone `global.sav` file (spec §2.3, corrected —
/// see [`GLOBALSAVE_OLD_ARR`]): exactly `GLOBAL` (1D int) then `GLOBALS` (1D
/// string), positional, no scalars — a different, dedicated layout from
/// [`parse_old_variable_block`]'s 60-array local-embedded one.
fn parse_old_global_save_block(cursor: &mut LineCursor) -> Result<HashMap<String, ParsedArray>> {
    let mut vars = HashMap::new();
    vars.insert(
        GLOBALSAVE_OLD_ARR.to_owned(),
        ParsedArray::Int1D(cursor.read_old_int_array()?),
    );
    vars.insert(
        GLOBALSAVE_OLD_STR_ARR.to_owned(),
        ParsedArray::Str1D(cursor.read_old_str_array()?),
    );
    Ok(vars)
}

/// One character's extended section (spec §2.5 "Character section"): 6
/// groups, no user-defined pass, ending with the group's own trailing
/// separator (which the caller does not need to consume again — the next
/// character's or the variable section's first group follows immediately).
    fn parse_chara_section(
        cursor: &mut LineCursor,
        version: u32,
    ) -> Result<HashMap<String, ParsedArray>> {
        let mut vars = HashMap::new();
        cursor.read_scalars(&mut vars, |v| Ok(ParsedArray::StrScalar(v.to_owned())))?; // string scalars
        cursor.read_scalars(&mut vars, |v| {
            v.trim()
                .parse()
                .map(ParsedArray::IntScalar)
                .map_err(|e| anyhow!("정수 스칼라가 아닙니다: {v:?} ({e})"))
        })?; // int scalars
        cursor.read_1d_arrays(&mut vars, true)?; // string 1D
        cursor.read_1d_arrays(&mut vars, false)?; // int 1D
        // Chara 2D arrays exist only from the 1803 grammar: Emuera
        // dispatches version < 1803 to `LoadFromStreamExtended_Old1802`
        // (4 groups, `CharacterData.cs:400-428`) and 1803+ to
        // `LoadFromStreamExtended` (6 groups, `:355-399`) from
        // `VariableEvaluator.LoadFromStream`.
        if version >= 1803 {
            cursor.expect_empty_group()?; // string 2D (never present in text)
            cursor.read_2d_arrays(&mut vars)?; // int 2D
        }
        Ok(vars)
    }

/// The variable extended section (spec §2.5 "Variable section"): 8
/// built-in groups then 6 user-defined groups, ending at EOF (no trailing
/// marker after the last group).
    fn parse_variable_section(
        cursor: &mut LineCursor,
        version: u32,
    ) -> Result<HashMap<String, ParsedArray>> {
        let mut vars = HashMap::new();
        cursor.read_scalars(&mut vars, |v| Ok(ParsedArray::StrScalar(v.to_owned())))?; // string scalars
        cursor.read_scalars(&mut vars, |v| {
            v.trim()
                .parse()
                .map(ParsedArray::IntScalar)
                .map_err(|e| anyhow!("정수 스칼라가 아닙니다: {v:?} ({e})"))
        })?; // int scalars
        cursor.read_1d_arrays(&mut vars, true)?; // string 1D
        cursor.read_1d_arrays(&mut vars, false)?; // int 1D
        // The 2D/3D groups are themselves version-gated on the read side
        // (`EraDataStream.cs:297,344` 2D iff >= 1708; `:366,424` 3D iff
        // >= 1729): a 1700 file physically has no 2D slots and a 1708-1728
        // file no 3D slots, so a reader must skip them for older versions
        // or it consumes the next group's data into the wrong slot.
        if version >= 1708 {
            cursor.expect_empty_group()?; // string 2D (never present in text)
            cursor.read_2d_arrays(&mut vars)?; // int 2D
        }
        if version >= 1729 {
            cursor.expect_empty_group()?; // string 3D (never present in text)
            cursor.read_3d_arrays(&mut vars)?; // int 3D
        }
        // The six user-defined `#DIM SAVEDATA` groups exist only from 1808
        // — `if (version < 1808) return;` before the Phase-2 reads
        // (`VariableData.LoadFromStreamExtended`).
        if version >= 1808 {
            cursor.read_1d_arrays(&mut vars, true)?; // user string 1D
            cursor.read_1d_arrays(&mut vars, false)?; // user int 1D
            cursor.expect_empty_group()?; // user string 2D (never present)
            cursor.read_2d_arrays(&mut vars)?; // user int 2D
            cursor.expect_empty_group()?; // user string 3D (never present)
            cursor.read_3d_arrays(&mut vars)?; // user int 3D
        }
        Ok(vars)
    }

/// The global save's own variable section (spec §2.5, corrected):
/// **6 groups, no scalars at all** — string 1D, int 1D, string 2D (always
/// empty), int 2D, string 3D (always empty), int 3D — ending at EOF.
/// `parse_variable_section`'s 8-built-in/6-user, scalars-first shape is the
/// *local* save's grammar only; a real `global.sav` capture
/// (`global_text_utf8_real.sav`) is byte-exact 6 `__EMU_SEPARATOR__` groups
/// after the OLD block, with no `KEY:VALUE` scalar lines anywhere in it —
/// calling `parse_variable_section` here (as this parser used to,
/// unconditionally) misreads the first 1D-array group as an always-empty
/// scalar group and desynchronises everything after it.
    fn parse_global_variable_section(
        cursor: &mut LineCursor,
        version: u32,
    ) -> Result<HashMap<String, ParsedArray>> {
        let mut vars = HashMap::new();
        cursor.read_1d_arrays(&mut vars, true)?; // string 1D
        cursor.read_1d_arrays(&mut vars, false)?; // int 1D
        // 2D iff >= 1708, 3D iff >= 1729 (same gates as the local variable
        // section's built-in groups; `EraDataStream.cs:297,344,366,424`).
        if version >= 1708 {
            cursor.expect_empty_group()?; // string 2D (never present)
            cursor.read_2d_arrays(&mut vars)?; // int 2D
        }
        if version >= 1729 {
            cursor.expect_empty_group()?; // string 3D (never present)
            cursor.read_3d_arrays(&mut vars)?; // int 3D
        }
        Ok(vars)
    }

/// Real Emuera's own reader (`VariableEvaluator::LoadFromStream`,
/// `.il:108953-109155`; `VariableEvaluator::LoadGlobal`, `.il:109237-109353`)
/// reads the OLD block *first, unconditionally* — `CharacterData::
/// LoadFromStream`/`VariableData::LoadFromStream`/`LoadGlobalFromStream`
/// all run before `SeekEmuStart` is ever called — then, only if the marker
/// is found, layers the extended block on top. Spec §7's older claim that a
/// loader "may either parse the OLD block or seek to the marker" is wrong:
/// the OLD block is where nearly all of a real save's classic built-in
/// state actually lives (`DAY`, `MONEY`, `ABL`, `TALENT`, `CFLAG`, ...); the
/// extended block only ever adds `NICKNAME`/`MASTERNAME`/`CSTR`/`CDFLAG`
/// (chara) and whatever further custom savedata this game declares. This
/// function mirrors that exactly, and merges by simple `HashMap::extend`:
/// the OLD and extended name sets are disjoint by construction (IL-derived
/// per-name `SAVE_EXTENDED`/beyond-old-range flags), so there is never a
/// same-name collision to arbitrate.
fn parse_text(text: &str, is_global: bool) -> Result<(EmueraSaveData, u32, u32, String)> {
    let mut cursor = LineCursor::new(text);

    let code = cursor.next_i64()?;
    let version = cursor.next_i64()?;

    let (description, character_count) = if is_global {
        (String::new(), 0)
    } else {
        let description = cursor.next()?.to_owned();
        let character_count = cursor.next_i64()?;
        (description, character_count)
    };
    ensure!(character_count >= 0, "characterCount가 음수입니다: {character_count}");

    let mut charas: Vec<HashMap<String, ParsedArray>> = Vec::with_capacity(character_count as usize);
    let mut globals = if is_global {
        parse_old_global_save_block(&mut cursor)?
    } else {
        for _ in 0..character_count {
            charas.push(parse_old_chara_block(&mut cursor)?);
        }
        parse_old_variable_block(&mut cursor)?
    };

    // The extended block genuinely is optional — a save written before it
    // existed carries no `__EMUERA_..._STRAT__` marker at all. Only a
    // recognized marker's extended block is read, with the per-version
    // grammar; an unknown marker (or none at all) is surfaced via
    // [`ExtendedMarker`] so the caller can warn the player that the
    // extended-only variables were dropped — never a silent load.
    let extended_marker = cursor.seek_emu_start();
    if let ExtendedMarker::Known(version) = &extended_marker {
        if is_global {
            globals.extend(parse_global_variable_section(&mut cursor, *version)?);
        } else {
            for chara in charas.iter_mut() {
                chara.extend(parse_chara_section(&mut cursor, *version)?);
            }
            globals.extend(parse_variable_section(&mut cursor, *version)?);
        }
    }

    Ok((
        EmueraSaveData { globals, charas, extended_marker },
        code as u32,
        version as u32,
        description,
    ))
}

/// Binary format (spec §4): a cursor over the raw byte stream, positioned
/// right after the shared 16-byte header (magic + reader version +
/// reserved). Every accessor here is validated to the exact byte, both
/// directions, against all three real binary captures (`save90_binary_real
/// .sav`, `global_binary_real.sav`) — see `docs/research/
/// 2026-09-06-emuera-save-format.md` §4 for the citations.
struct BinCursor<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> BinCursor<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    fn take(&mut self, n: usize) -> Result<&'a [u8]> {
        let end = self
            .pos
            .checked_add(n)
            .ok_or_else(|| anyhow!("Emuera 바이너리 세이브 파일이 예상보다 짧습니다"))?;
        let slice = self
            .data
            .get(self.pos..end)
            .ok_or_else(|| anyhow!("Emuera 바이너리 세이브 파일이 예상보다 짧습니다"))?;
        self.pos = end;
        Ok(slice)
    }

    fn byte(&mut self) -> Result<u8> {
        Ok(self.take(1)?[0])
    }

    fn i16(&mut self) -> Result<i16> {
        Ok(i16::from_le_bytes(self.take(2)?.try_into().unwrap()))
    }

    fn i32(&mut self) -> Result<i32> {
        Ok(i32::from_le_bytes(self.take(4)?.try_into().unwrap()))
    }

    fn i64(&mut self) -> Result<i64> {
        Ok(i64::from_le_bytes(self.take(8)?.try_into().unwrap()))
    }

    /// Decodes an integer value whose tag byte has already been consumed
    /// (array elements read the tag first to tell it apart from a
    /// zero-run/terminator marker) — `EraBinaryDataReader::m_ReadInt`'s
    /// value encoding (`.il` `EraBinaryDataReader1808::m_ReadInt`): `<=
    /// 0xCF` is the value itself, `0xD0`/`0xD1`/`0xD2` mean a following
    /// `int16`/`int32`/`int64` LE.
    fn int_value(&mut self, tag: u8) -> Result<i64> {
        Ok(match tag {
            0x00..=0xCF => tag as i64,
            0xD0 => self.i16()? as i64,
            0xD1 => self.i32()? as i64,
            0xD2 => self.i64()?,
            other => bail!("잘못된 정수 태그입니다: {other:#x}"),
        })
    }

    /// `m_ReadInt`: reads its own tag byte then decodes it. Used for a
    /// scalar `int64` value and for a zero-run/null-run count following an
    /// `0xF0`/`0xF1`/`0xF2` array marker.
    fn m_read_int(&mut self) -> Result<i64> {
        let tag = self.byte()?;
        self.int_value(tag)
    }

    /// `BinaryReader.ReadString()` under this build's `Encoding.Unicode`
    /// (spec §4.3, empirically corrected): a 7-bit-varint *byte* length
    /// prefix, then that many UTF-16LE bytes — not UTF-8, which the .NET
    /// `BinaryReader` default would otherwise suggest.
    fn read_bstr(&mut self) -> Result<String> {
        let mut len: u32 = 0;
        let mut shift = 0u32;
        loop {
            let b = self.byte()?;
            len |= ((b & 0x7F) as u32) << shift;
            if b & 0x80 == 0 {
                break;
            }
            shift += 7;
            ensure!(shift < 35, "문자열 길이 varint가 너무 깁니다");
        }
        let bytes = self.take(len as usize)?;
        ensure!(bytes.len() % 2 == 0, "UTF-16 문자열 바이트 길이가 홀수입니다: {}", bytes.len());
        let units: Vec<u16> = bytes
            .chunks_exact(2)
            .map(|c| u16::from_le_bytes([c[0], c[1]]))
            .collect();
        String::from_utf16(&units).map_err(|e| anyhow!("문자열이 유효한 UTF-16이 아닙니다: {e}"))
    }

    /// `ReadIntArray` (int64[1D]): `int32` logical length, then elements
    /// (`0xFF` terminates; `0xF0` + `m_ReadInt()` is a zero-run of that many
    /// slots; anything else is one value via [`Self::int_value`]).
    fn read_int1d(&mut self) -> Result<Vec<i64>> {
        let len = self.i32()?.max(0) as usize;
        let mut out = vec![0i64; len];
        let mut idx = 0usize;
        loop {
            let tag = self.byte()?;
            match tag {
                0xFF => break,
                0xF0 => idx += self.m_read_int()? as usize,
                _ => {
                    let v = self.int_value(tag)?;
                    if let Some(cell) = out.get_mut(idx) {
                        *cell = v;
                    }
                    idx += 1;
                }
            }
        }
        Ok(out)
    }

    /// `ReadStrArray` (string[1D]): as [`Self::read_int1d`], but `0xF0` is a
    /// null-run and a real value is tagged `0xD8` then read via
    /// [`Self::read_bstr`] — strings have no self-describing value range,
    /// so every element needs an explicit "a value follows" tag, unlike
    /// ints.
    fn read_str1d(&mut self) -> Result<Vec<String>> {
        let len = self.i32()?.max(0) as usize;
        let mut out = vec![String::new(); len];
        let mut idx = 0usize;
        loop {
            let tag = self.byte()?;
            match tag {
                0xFF => break,
                0xF0 => idx += self.m_read_int()? as usize,
                0xD8 => {
                    let s = self.read_bstr()?;
                    if let Some(cell) = out.get_mut(idx) {
                        *cell = s;
                    }
                    idx += 1;
                }
                other => bail!("잘못된 문자열 배열 태그입니다: {other:#x}"),
            }
        }
        Ok(out)
    }

    /// `ReadIntArray2D`: `dim0:int32, dim1:int32`, then elements addressed
    /// row-major with `0xFF` terminating, `0xF1`+count zeroing that many
    /// whole rows, `0xE0` zeroing the rest of the current row and advancing
    /// to the next, `0xF0`+count zeroing that many cells within the current
    /// row, and anything else one value via [`Self::int_value`].
    fn read_int2d(&mut self) -> Result<Vec<Vec<i64>>> {
        let dim0 = self.i32()?.max(0) as usize;
        let dim1 = self.i32()?.max(0) as usize;
        let mut grid = vec![vec![0i64; dim1]; dim0];
        let (mut row, mut col) = (0usize, 0usize);
        loop {
            let tag = self.byte()?;
            match tag {
                0xFF => break,
                0xF1 => {
                    row += self.m_read_int()? as usize;
                    col = 0;
                }
                0xE0 => {
                    row += 1;
                    col = 0;
                }
                0xF0 => col += self.m_read_int()? as usize,
                _ => {
                    let v = self.int_value(tag)?;
                    if let Some(cell) = grid.get_mut(row).and_then(|r| r.get_mut(col)) {
                        *cell = v;
                    }
                    col += 1;
                }
            }
        }
        Ok(grid)
    }

    /// `ReadStrArray2D`: as [`Self::read_int2d`], with string null-runs
    /// instead of zero-runs and a real value tagged `0xD8` (see
    /// [`Self::read_str1d`]).
    fn read_str2d(&mut self) -> Result<Vec<Vec<String>>> {
        let dim0 = self.i32()?.max(0) as usize;
        let dim1 = self.i32()?.max(0) as usize;
        let mut grid = vec![vec![String::new(); dim1]; dim0];
        let (mut row, mut col) = (0usize, 0usize);
        loop {
            let tag = self.byte()?;
            match tag {
                0xFF => break,
                0xF1 => {
                    row += self.m_read_int()? as usize;
                    col = 0;
                }
                0xE0 => {
                    row += 1;
                    col = 0;
                }
                0xF0 => col += self.m_read_int()? as usize,
                0xD8 => {
                    let s = self.read_bstr()?;
                    if let Some(cell) = grid.get_mut(row).and_then(|r| r.get_mut(col)) {
                        *cell = s;
                    }
                    col += 1;
                }
                other => bail!("잘못된 문자열 2D 배열 태그입니다: {other:#x}"),
            }
        }
        Ok(grid)
    }

    /// `ReadIntArray3D`: `dim0/dim1/dim2:int32`, then elements addressed
    /// plane-major with `0xFF` terminating, `0xF2`+count zeroing that many
    /// whole planes, `0xE1` zeroing the rest of the current plane and
    /// advancing, `0xF1`+count zeroing that many whole rows within the
    /// current plane, `0xE0` zeroing the rest of the current row and
    /// advancing, `0xF0`+count zeroing that many cells within the current
    /// row, and anything else one value.
    fn read_int3d(&mut self) -> Result<Vec<(u32, Vec<Vec<i64>>)>> {
        let dim0 = self.i32()?.max(0) as usize;
        let dim1 = self.i32()?.max(0) as usize;
        let dim2 = self.i32()?.max(0) as usize;
        let mut planes = vec![vec![vec![0i64; dim2]; dim1]; dim0];
        let (mut plane, mut row, mut col) = (0usize, 0usize, 0usize);
        loop {
            let tag = self.byte()?;
            match tag {
                0xFF => break,
                0xF2 => {
                    plane += self.m_read_int()? as usize;
                    row = 0;
                    col = 0;
                }
                0xE1 => {
                    plane += 1;
                    row = 0;
                    col = 0;
                }
                0xF1 => {
                    row += self.m_read_int()? as usize;
                    col = 0;
                }
                0xE0 => {
                    row += 1;
                    col = 0;
                }
                0xF0 => col += self.m_read_int()? as usize,
                _ => {
                    let v = self.int_value(tag)?;
                    if let Some(cell) = planes
                        .get_mut(plane)
                        .and_then(|p| p.get_mut(row))
                        .and_then(|r| r.get_mut(col))
                    {
                        *cell = v;
                    }
                    col += 1;
                }
            }
        }
        Ok(planes.into_iter().enumerate().map(|(i, p)| (i as u32, p)).collect())
    }

    /// `ReadStrArray3D`: as [`Self::read_int3d`], with string null-runs
    /// instead of zero-runs and a real value tagged `0xD8`.
    fn read_str3d(&mut self) -> Result<Vec<(u32, Vec<Vec<String>>)>> {
        let dim0 = self.i32()?.max(0) as usize;
        let dim1 = self.i32()?.max(0) as usize;
        let dim2 = self.i32()?.max(0) as usize;
        let mut planes = vec![vec![vec![String::new(); dim2]; dim1]; dim0];
        let (mut plane, mut row, mut col) = (0usize, 0usize, 0usize);
        loop {
            let tag = self.byte()?;
            match tag {
                0xFF => break,
                0xF2 => {
                    plane += self.m_read_int()? as usize;
                    row = 0;
                    col = 0;
                }
                0xE1 => {
                    plane += 1;
                    row = 0;
                    col = 0;
                }
                0xF1 => {
                    row += self.m_read_int()? as usize;
                    col = 0;
                }
                0xE0 => {
                    row += 1;
                    col = 0;
                }
                0xF0 => col += self.m_read_int()? as usize,
                0xD8 => {
                    let s = self.read_bstr()?;
                    if let Some(cell) = planes
                        .get_mut(plane)
                        .and_then(|p| p.get_mut(row))
                        .and_then(|r| r.get_mut(col))
                    {
                        *cell = s;
                    }
                    col += 1;
                }
                other => bail!("잘못된 문자열 3D 배열 태그입니다: {other:#x}"),
            }
        }
        Ok(planes.into_iter().enumerate().map(|(i, p)| (i as u32, p)).collect())
    }
}

/// How a run of `WriteWithKey` records ended (spec §4.3 terminators).
enum RecordEnd {
    /// `0xFF`: end of file — the last block in either save shape.
    Eof,
    /// `0xFE`: end of one character's record block. Contrary to an earlier
    /// reading of the spec (and of `real/README.md` point 7), a *local*
    /// binary save's character block is not merely "contiguous records,
    /// no separators" — decoding `save90_binary_real.sav` byte-for-byte
    /// found exactly one `0xFE` immediately after the character's own 29th
    /// record (`CSTR`'s array terminator) and immediately before the first
    /// global record (`DAY`)'s key, and the whole file (2317 bytes) decodes
    /// clean to EOF only with this marker consumed between the character
    /// block and the global block.
    Eoc,
    /// `0xFD`: `EraSaveDataType.Separator` — used by the separate
    /// `SAVEVAR`/`SAVECHARA` binary format (out of scope here); a
    /// `SAVEDATA`/`LOADDATA`/`SAVEGLOBAL`/`LOADGLOBAL` file never contains
    /// one, so seeing it here is itself a malformed-file error, not a
    /// meaningful terminator this reader acts on.
    Separator,
}

/// Reads `WriteWithKey`-framed records (spec §4.3: `<type byte> <key
/// string> <payload>`) into `out` until a control byte ends the block.
fn read_binary_records(cur: &mut BinCursor, out: &mut HashMap<String, ParsedArray>) -> Result<RecordEnd> {
    loop {
        let tag = cur.byte()?;
        match tag {
            0xFF => return Ok(RecordEnd::Eof),
            0xFE => return Ok(RecordEnd::Eoc),
            0xFD => return Ok(RecordEnd::Separator),
            _ => {}
        }
        let key = cur.read_bstr()?;
        let value = match tag {
            0 => ParsedArray::IntScalar(cur.m_read_int()?),
            1 => ParsedArray::Int1D(cur.read_int1d()?),
            2 => ParsedArray::Int2D(cur.read_int2d()?),
            3 => ParsedArray::Int3D(cur.read_int3d()?),
            16 => ParsedArray::StrScalar(cur.read_bstr()?),
            17 => ParsedArray::Str1D(cur.read_str1d()?),
            18 => ParsedArray::Str2D(cur.read_str2d()?),
            19 => ParsedArray::Str3D(cur.read_str3d()?),
            other => bail!("알 수 없는 변수 타입 바이트입니다: {other:#x} (키 {key:?})"),
        };
        out.insert(key, value);
    }
}

/// Parses a binary Emuera save (spec §4): shared 16-byte header (magic,
/// reader version, reserved — already sniffed by [`sniff`]), then
/// `FileType` (`0` local / `1` global — cross-checked against `is_global`
/// rather than trusted blindly, the same defence-in-depth [`parse_text`]
/// gets for free from being handed the right grammar), `code`, `version`,
/// `saveText`, and — local only — an explicit `int64 characterCount`
/// followed by that many [`RecordEnd::Eoc`]-terminated character blocks,
/// then one [`RecordEnd::Eof`]-terminated global block (a global save skips
/// straight to that last block). Fully validated byte-for-byte against both
/// `save90_binary_real.sav` (2317 B, decodes to exactly its own length: a
/// 29-record character block + EOC + a 79-record global block + EOF) and
/// `global_binary_real.sav` (76 B: a single 1D int record keyed `GLOBAL`,
/// Emuera's own internal name for the legacy flat global block, then EOF).
fn parse_binary(bytes: &[u8], is_global: bool) -> Result<(EmueraSaveData, u32, u32, String)> {
    let mut cur = BinCursor::new(bytes);

    let magic = cur.take(8)?;
    ensure!(magic == BINARY_MAGIC, "바이너리 매직 바이트가 올바르지 않습니다");
    let _reader_version = cur.i32()?;
    let _reserved = cur.i32()?;

    let file_type = cur.byte()?;
    ensure!(
        (file_type == 1) == is_global,
        "파일 타입 바이트가 예상과 다릅니다 (파일: {file_type}, 예상 is_global: {is_global})"
    );

    let code = cur.i64()?;
    let version = cur.i64()?;
    let description = cur.read_bstr()?;

    let mut charas = Vec::new();
    if !is_global {
        let character_count = cur.i64()?;
        ensure!(character_count >= 0, "characterCount가 음수입니다: {character_count}");
        charas.reserve(character_count as usize);
        for _ in 0..character_count {
            let mut vars = HashMap::new();
            let end = read_binary_records(&mut cur, &mut vars)?;
            ensure!(
                matches!(end, RecordEnd::Eoc),
                "캐릭터 레코드 블록이 EOC(0xFE)로 끝나지 않았습니다"
            );
            charas.push(vars);
        }
    }

    let mut globals = HashMap::new();
    let end = read_binary_records(&mut cur, &mut globals)?;
    ensure!(matches!(end, RecordEnd::Eof), "전역 레코드 블록이 EOF(0xFF)로 끝나지 않았습니다");

    Ok((
        EmueraSaveData { globals, charas, extended_marker: ExtendedMarker::Known(1808) },
        code as u32,
        version as u32,
        description,
    ))
}

/// One variable dropped from the import, and why.
///
/// `name`/`reason` (and `PartialVariable`'s three fields below) are never
/// read back out of the stored `ImportReport` in production: the per-item
/// diagnostic a player actually sees is the `log::warn!`/`log::info!` call
/// inside [`ImportReport::skip`]/[`ImportReport::partial`], fired from the
/// same `name`/`reason`/`expected_len`/`found_len` values *before* they are
/// moved into these structs, and [`ImportReport::log_summary`] (the only
/// production reader of `self.skipped`/`self.partial`) consults just their
/// `.len()`. The struct fields exist so a test — or a future caller wanting
/// the itemised list rather than only the count — can inspect exactly what
/// happened; keep them `pub` and silence the resulting lint rather than
/// deleting fields whose only current reader is the test suite.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct SkippedVariable {
    pub name: String,
    pub reason: SkipReason,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum SkipReason {
    /// This game's `HeaderInfo` interned no variable of this name at all.
    NotDeclared,
    /// Declared, but as a chara-scope variable in a non-chara context (or
    /// vice versa), or with the wrong `is_global`.
    ScopeMismatch,
    /// Declared, but as the other of int/str.
    TypeMismatch,
    /// Declared, but with a different number of dimensions than the file's
    /// own record for this name.
    DimensionMismatch { expected: usize, found: usize },
}

/// One variable copied in only partially: same name, same scope, same
/// type, same dimensionality, different extent somewhere. See
/// [`SkippedVariable`]'s doc comment: these fields are likewise write-only
/// in production, for the same reason.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct PartialVariable {
    pub name: String,
    pub expected_len: usize,
    pub found_len: usize,
}

/// Accumulates every skip/partial-import decision a merge makes, so the
/// caller can log a trustworthy summary — a reported count of zero must mean
/// zero, never "we didn't check".
#[derive(Default, Debug, Clone)]
pub struct ImportReport {
    pub skipped: Vec<SkippedVariable>,
    pub partial: Vec<PartialVariable>,
    /// Set when the save's extended block was not imported — an older
    /// `__EMUERA_..._STRAT__` marker, or none at all — carrying a short
    /// reason (the marker text, or "마커 없음"). Kept so that
    /// [`ImportReport::log_summary`] cannot report "0 skipped, 0 partial"
    /// as if the load were complete: the version-independent OLD block is
    /// in, but every extended-only variable (`NICKNAME`, `MASTERNAME`,
    /// `CSTR`, `CDFLAG`, and the game's user `#DIM SAVEDATA` arrays) is
    /// silently absent.
    pub extended_marker_skipped: Option<String>,
}

impl ImportReport {
    fn skip(&mut self, name: &str, reason: SkipReason) {
        log::warn!("Emuera 세이브 가져오기: 변수 {name} 무시됨 ({reason:?})");
        self.skipped.push(SkippedVariable {
            name: name.to_owned(),
            reason,
        });
    }

    fn partial(&mut self, name: &str, expected_len: usize, found_len: usize) {
        log::info!(
            "Emuera 세이브 가져오기: 변수 {name} 부분적으로 가져옴 (이 게임 크기 {expected_len}, \
             세이브 파일 크기 {found_len})"
        );
        self.partial.push(PartialVariable {
            name: name.to_owned(),
            expected_len,
            found_len,
        });
    }

    /// Records that the save's extended block was skipped, logging a clear
    /// warning naming what the seek found — the same warn-routing as
    /// [`Self::skip`], so an old or absent marker is as loud as a dropped
    /// variable, and it is counted in [`Self::log_summary`] so a "0
    /// skipped" report stays trustworthy.
    fn note_marker_skipped(&mut self, marker: &ExtendedMarker) {
        match marker {
            ExtendedMarker::Known(_) => {} // block was read; nothing to report
            ExtendedMarker::Unknown(m) => {
                let detail =
                    format!("알 수 없는 확장 블록 마커 {m} 발견 — 확장 전용 변수는 가져오지 않았습니다");
                log::warn!("Emuera 세이브 가져오기: {detail}");
                self.extended_marker_skipped = Some(format!("마커 {m}"));
            }
            ExtendedMarker::Absent => {
                log::warn!(
                    "Emuera 세이브 가져오기: 확장 블록 마커가 없음 — 확장 전용 변수는 가져오지 않았습니다"
                );
                self.extended_marker_skipped = Some("마커 없음".to_owned());
            }
        }
    }

    /// One line stating exactly how many variables were skipped and how many
    /// partially imported — printed once per load, always, so "0 skipped, 0
    /// partial" is a claim a player can trust rather than the absence of a
    /// check. When the extended block was skipped, a further warning line
    /// makes that explicit so the counts alone cannot read as a complete
    /// import.
    pub fn log_summary(&self) {
        log::info!(
            "Emuera 세이브 가져오기 완료: {} 개 변수 무시됨, {} 개 변수 부분적으로 가져옴",
            self.skipped.len(),
            self.partial.len()
        );
        if let Some(detail) = &self.extended_marker_skipped {
            log::warn!(
                "Emuera 세이브 가져오기: 확장 블록 건너뜀 ({detail}) — 확장 전용 변수는 가져오지 않았습니다"
            );
        }
    }
}

/// Places `rows` (file order, each independently trimmed — see
/// [`ParsedArray::Int2D`]) into `target`, a flat row-major buffer already
/// initialised to `info`'s declared defaults, for a 2D variable of shape
/// `[rows_n, cols_n]`. Returns `(expected_len, found_len)` for
/// [`ImportReport::partial`] — `found_len` underestimates true fullness
/// exactly to the extent Emuera's own trimming does (a trimmed trailing
/// zero looks identical to "not written"). Generic over `T` so the same
/// logic serves both `Int2D`/`Int3D` (`T = i64`) and the binary-only
/// `Str2D`/`Str3D` (`T = String`).
fn place_2d<T: Clone>(target: &mut [T], rows_n: usize, cols_n: usize, rows: Vec<Vec<T>>) -> (usize, usize) {
    let mut found_len = 0;
    for (r, row) in rows.into_iter().take(rows_n).enumerate() {
        let n = row.len().min(cols_n);
        target[r * cols_n..r * cols_n + n].clone_from_slice(&row[..n]);
        found_len += n;
    }
    (rows_n * cols_n, found_len)
}

/// As [`place_2d`], generalised to 3D: `blocks` are `(outer index, rows)`
/// pairs; an outer index `>= depth_n` is out of range and dropped (`erars`'s
/// declared shape shrank since the file was written).
fn place_3d<T: Clone>(
    target: &mut [T],
    depth_n: usize,
    rows_n: usize,
    cols_n: usize,
    blocks: Vec<(u32, Vec<Vec<T>>)>,
) -> (usize, usize) {
    let mut found_len = 0;
    for (idx, rows) in blocks {
        let Ok(idx) = usize::try_from(idx) else { continue };
        if idx >= depth_n {
            continue;
        }
        let plane = &mut target[idx * rows_n * cols_n..(idx + 1) * rows_n * cols_n];
        let (_, plane_found) = place_2d(plane, rows_n, cols_n, rows);
        found_len += plane_found;
    }
    (depth_n * rows_n * cols_n, found_len)
}

/// Resolves one foreign variable against this game's current declarations,
/// checking scope, type and dimensionality, and — if it survives every
/// [`SkipReason`] gate — reconciles its extent against the declared shape.
/// Shared by [`merge_globals`] (`is_chara = false`) and
/// [`merge_chara_columns`] (`is_chara = true`); `is_global` only matters for
/// the former (a chara row is never `is_global`).
fn reconcile<'h>(
    name: &str,
    parsed: ParsedArray,
    header: &'h HeaderInfo,
    is_chara: bool,
    is_global: bool,
    report: &mut ImportReport,
) -> Option<(StrKey, &'h VariableInfo, VmVariable)> {
    let Some(key) = get_interner().get(name) else {
        report.skip(name, SkipReason::NotDeclared);
        return None;
    };

    let Some(info) = header.global_variables.get(&key) else {
        report.skip(name, SkipReason::NotDeclared);
        return None;
    };

    if info.is_chara != is_chara || info.is_global != is_global || !info.is_savedata {
        report.skip(name, SkipReason::ScopeMismatch);
        return None;
    }

    if info.is_str != parsed.is_str() {
        report.skip(name, SkipReason::TypeMismatch);
        return None;
    }

    if info.size.len() != parsed.dim_count() {
        report.skip(
            name,
            SkipReason::DimensionMismatch {
                expected: info.size.len(),
                found: parsed.dim_count(),
            },
        );
        return None;
    }

    let mut target = VmVariable::new(header, info);
    let (expected_len, found_len) = match parsed {
        ParsedArray::IntScalar(v) => {
            let _ = target.set(0, v);
            (1, 1)
        }
        ParsedArray::StrScalar(v) => {
            let _ = target.set(0, v);
            (1, 1)
        }
        ParsedArray::Int1D(v) => {
            let found_len = v.len();
            target.overwrite_from(VmVariable::Int(v));
            (info.full_size(), found_len)
        }
        ParsedArray::Str1D(v) => {
            let found_len = v.len();
            target.overwrite_from(VmVariable::Str(v));
            (info.full_size(), found_len)
        }
        ParsedArray::Int2D(rows) => {
            let (rows_n, cols_n) = (info.size[0] as usize, info.size[1] as usize);
            let ints = target.as_int().expect("is_str already checked to match");
            place_2d(ints, rows_n, cols_n, rows)
        }
        ParsedArray::Int3D(blocks) => {
            let (depth_n, rows_n, cols_n) =
                (info.size[0] as usize, info.size[1] as usize, info.size[2] as usize);
            let ints = target.as_int().expect("is_str already checked to match");
            place_3d(ints, depth_n, rows_n, cols_n, blocks)
        }
        ParsedArray::Str2D(rows) => {
            let (rows_n, cols_n) = (info.size[0] as usize, info.size[1] as usize);
            let strs = target.as_str().expect("is_str already checked to match");
            place_2d(strs, rows_n, cols_n, rows)
        }
        ParsedArray::Str3D(blocks) => {
            let (depth_n, rows_n, cols_n) =
                (info.size[0] as usize, info.size[1] as usize, info.size[2] as usize);
            let strs = target.as_str().expect("is_str already checked to match");
            place_3d(strs, depth_n, rows_n, cols_n, blocks)
        }
    };

    if found_len != expected_len {
        report.partial(name, expected_len, found_len);
    }

    Some((key, info, target))
}

/// Builds the `variables` map of a `SerializableVariableStorage` (`is_global
/// = false`) or `SerializableGlobalVariableStorage` (`is_global = true`)
/// from a parsed Emuera save's non-chara variables.
pub fn merge_globals(
    globals: HashMap<String, ParsedArray>,
    header: &HeaderInfo,
    is_global: bool,
    report: &mut ImportReport,
) -> HashMap<StrKey, (VariableInfo, UniformVariable)> {
    globals
        .into_iter()
        .filter_map(|(name, parsed)| {
            let (key, info, var) = reconcile(&name, parsed, header, false, is_global, report)?;
            Some((key, (info.clone(), UniformVariable::Normal(var))))
        })
        .collect()
}

/// Transposes the numbered save's own embedded `CHARADATA` rows (row-major:
/// one `HashMap` per character, as Emuera's file lays them out) into the
/// column-major shape `SerializableVariableStorage.variables` actually
/// wants for a chara-scope entry: `UniformVariable::Character(Vec<
/// VmVariable>)`, one element per character index.
///
/// This is the numbered save's *own* character records — the `#DIM
/// SAVEDATA` chara-scope arrays (`ABL`, `TALENT`, …) that travel inside
/// `SAVEDATA`/`LOADDATA` alongside the plain globals. It has nothing to do
/// with the separate, still out-of-scope `SAVECHARA`/`chara_*.dat`
/// mechanism, which serialises one character to its own portable file.
///
/// A name is resolved once against `header.global_variables`, not per row:
/// every row describes the same declared variable, so a type or dimension
/// mismatch is reported once for the whole column rather than once per
/// character.
pub fn merge_chara_columns(
    mut charas: Vec<HashMap<String, ParsedArray>>,
    header: &HeaderInfo,
    report: &mut ImportReport,
) -> HashMap<StrKey, (VariableInfo, UniformVariable)> {
    let chara_count = charas.len();

    let mut names = std::collections::BTreeSet::new();
    for row in &charas {
        names.extend(row.keys().cloned());
    }

    let mut result = HashMap::new();

    for name in names {
        let per_chara: Vec<Option<ParsedArray>> =
            charas.iter_mut().map(|row| row.remove(&name)).collect();

        // Resolve the declaration once, using whichever row actually has
        // data (rows that omitted the name entirely carry no shape/type
        // information at all).
        let Some(sample) = per_chara.iter().find_map(|p| p.as_ref()) else {
            continue; // every row omitted it: nothing to reconcile or report
        };
        let (sample_is_str, sample_dims) = (sample.is_str(), sample.dim_count());

        let Some(key) = get_interner().get(&name) else {
            report.skip(&name, SkipReason::NotDeclared);
            continue;
        };
        let Some(info) = header.global_variables.get(&key) else {
            report.skip(&name, SkipReason::NotDeclared);
            continue;
        };
        if !info.is_chara || info.is_global || !info.is_savedata {
            report.skip(&name, SkipReason::ScopeMismatch);
            continue;
        }
        if info.is_str != sample_is_str {
            report.skip(&name, SkipReason::TypeMismatch);
            continue;
        }
        if info.size.len() != sample_dims {
            report.skip(
                &name,
                SkipReason::DimensionMismatch {
                    expected: info.size.len(),
                    found: sample_dims,
                },
            );
            continue;
        }

        let mut columns = Vec::with_capacity(chara_count);
        let mut expected_total = 0;
        let mut found_total = 0;

        for parsed in per_chara {
            let mut target = VmVariable::new(header, info);
            if let Some(parsed) = parsed {
                let (expected_len, found_len) = match parsed {
                    ParsedArray::IntScalar(v) => {
                        let _ = target.set(0, v);
                        (1, 1)
                    }
                    ParsedArray::StrScalar(v) => {
                        let _ = target.set(0, v);
                        (1, 1)
                    }
                    ParsedArray::Int1D(v) => {
                        let found_len = v.len();
                        target.overwrite_from(VmVariable::Int(v));
                        (info.full_size(), found_len)
                    }
                    ParsedArray::Str1D(v) => {
                        let found_len = v.len();
                        target.overwrite_from(VmVariable::Str(v));
                        (info.full_size(), found_len)
                    }
                    ParsedArray::Int2D(rows) => {
                        let (rows_n, cols_n) = (info.size[0] as usize, info.size[1] as usize);
                        let ints = target.as_int().expect("is_str already checked to match");
                        place_2d(ints, rows_n, cols_n, rows)
                    }
                    ParsedArray::Int3D(blocks) => {
                        let (depth_n, rows_n, cols_n) = (
                            info.size[0] as usize,
                            info.size[1] as usize,
                            info.size[2] as usize,
                        );
                        let ints = target.as_int().expect("is_str already checked to match");
                        place_3d(ints, depth_n, rows_n, cols_n, blocks)
                    }
                    ParsedArray::Str2D(rows) => {
                        let (rows_n, cols_n) = (info.size[0] as usize, info.size[1] as usize);
                        let strs = target.as_str().expect("is_str already checked to match");
                        place_2d(strs, rows_n, cols_n, rows)
                    }
                    ParsedArray::Str3D(blocks) => {
                        let (depth_n, rows_n, cols_n) = (
                            info.size[0] as usize,
                            info.size[1] as usize,
                            info.size[2] as usize,
                        );
                        let strs = target.as_str().expect("is_str already checked to match");
                        place_3d(strs, depth_n, rows_n, cols_n, blocks)
                    }
                };
                expected_total += expected_len;
                found_total += found_len;
            } else {
                expected_total += info.full_size();
            }
            columns.push(target);
        }

        if found_total != expected_total {
            report.partial(&name, expected_total, found_total);
        }

        result.insert(key, (info.clone(), UniformVariable::Character(columns)));
    }

    result
}

/// Builds a full `SerializableVariableStorage` (the `LOADDATA` payload) from
/// one parsed numbered-slot Emuera save. `character_len` is derived from the
/// number of `CHARADATA` rows the file carried; `rand_seed` is left at its
/// default — `RANDDATA` travels as an ordinary variable (see the module doc
/// comment) and `VariableStorage::init_rand` re-derives the actual RNG state
/// from it after every load regardless of what `rand_seed` holds.
pub fn build_local_data(
    data: EmueraSaveData,
    header: &HeaderInfo,
) -> (super::SerializableVariableStorage, ImportReport) {
    let mut report = ImportReport::default();
    let character_len = data.charas.len() as u32;

    let mut variables = merge_globals(data.globals, header, false, &mut report);
    variables.extend(merge_chara_columns(data.charas, header, &mut report));

    report.note_marker_skipped(&data.extended_marker);
    report.log_summary();

    (
        super::SerializableVariableStorage {
            description: String::new(),
            code: 0,
            version: 0,
            character_len,
            rand_seed: Default::default(),
            variables,
            local_variables: HashMap::new(),
        },
        report,
    )
}

/// Builds a full `SerializableGlobalVariableStorage` (the `LOADGLOBAL`
/// payload) from a parsed `global.sav`. Real Emuera's global save has no
/// `CHARADATA` section at all (global variables have no character
/// dimension), so `data.charas` is not consulted here.
pub fn build_global_data(
    data: EmueraSaveData,
    header: &HeaderInfo,
) -> (super::SerializableGlobalVariableStorage, ImportReport) {
    let mut report = ImportReport::default();
    let variables = merge_globals(data.globals, header, true, &mut report);

    report.note_marker_skipped(&data.extended_marker);
    report.log_summary();

    (
        super::SerializableGlobalVariableStorage {
            code: 0,
            version: 0,
            variables,
            local_variables: HashMap::new(),
        },
        report,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn size(dims: &[u32]) -> tinyvec::ArrayVec<[u32; 3]> {
        let mut v = tinyvec::ArrayVec::<[u32; 3]>::new();
        for &d in dims {
            v.push(d);
        }
        v
    }

    fn info(is_chara: bool, is_global: bool, is_str: bool, dims: &[u32]) -> VariableInfo {
        VariableInfo {
            is_chara,
            is_global,
            is_str,
            is_savedata: true,
            size: size(dims),
            ..Default::default()
        }
    }

    /// Every declaration the hand-written fixture text below reconciles
    /// against — shapes taken straight from the spec's own worked example
    /// (`docs/research/2026-09-06-emuera-save-format.md`, "Byte order"
    /// section), not from any real game.
    ///
    /// Every OLD-block name ([`CHAR_OLD_STR`]/[`CHAR_OLD_INT`]/
    /// [`CHAR_OLD_ARR`]/[`GLOBAL_OLD_ARR`]) is declared here too, at a
    /// placeholder zero size/scalar shape — real Emuera's OLD block is
    /// mandatory grammar (see [`parse_old_chara_block`]/
    /// [`parse_old_variable_block`]), so a hand-built fixture save always
    /// carries all of them now, and a fixture-only game that declared none
    /// of them would make every one of the ~75 slots an (accurate, but
    /// uninteresting to this test) `NotDeclared` skip. Declared afterward,
    /// the ten names this test actually cares about override the
    /// placeholder with their real tested shape.
    fn fixture_header() -> HeaderInfo {
        erars_ast::init_interner();
        let mut global_variables = HashMap::new();
        let mut decl = |name: &'static str, v: VariableInfo| {
            global_variables.insert(get_interner().get_or_intern_static(name), v);
        };
        for name in CHAR_OLD_STR {
            decl(name, info(true, false, true, &[]));
        }
        for name in CHAR_OLD_INT {
            decl(name, info(true, false, false, &[]));
        }
        for name in CHAR_OLD_ARR {
            decl(name, info(true, false, false, &[0]));
        }
        for name in GLOBAL_OLD_ARR {
            decl(name, info(false, false, false, &[0]));
        }
        decl("DAY", info(false, false, false, &[]));
        decl("MONEY", info(false, false, false, &[]));
        decl("FLAG", info(false, false, false, &[3]));
        decl("MES", info(false, false, true, &[]));
        decl("SAVESTR", info(false, false, true, &[1]));
        decl("NICKNAME", info(true, false, true, &[]));
        decl("NO", info(true, false, false, &[]));
        decl("CSTR", info(true, false, true, &[2]));
        decl("CFLAG", info(true, false, false, &[2]));
        decl("RELATION", info(true, false, false, &[2, 3]));
        HeaderInfo {
            global_variables,
            ..Default::default()
        }
    }

    /// A real save's OLD block is mandatory grammar (spec §2.3, corrected)
    /// — these three helpers give a hand-built fixture a valid, entirely
    /// placeholder one (empty scalars, zero-length arrays), so the
    /// interesting part of each fixture stays the extended block layered
    /// on top, matching [`fixture_header`]'s placeholder zero-size decls.
    fn empty_old_chara_lines() -> Vec<String> {
        let mut lines: Vec<String> =
            vec!["-".to_owned(), "-".to_owned(), "0".to_owned(), "0".to_owned()];
        lines.extend(CHAR_OLD_ARR.iter().map(|_| FINISHED.to_owned()));
        lines
    }

    fn empty_old_global_lines() -> Vec<String> {
        let mut lines: Vec<String> = GLOBAL_OLD_ARR.iter().map(|_| FINISHED.to_owned()).collect();
        lines.push(FINISHED.to_owned()); // SAVESTR
        lines
    }

    fn empty_old_global_save_lines() -> Vec<String> {
        vec![FINISHED.to_owned(), FINISHED.to_owned()] // GLOBAL, GLOBALS
    }

    /// The extended block's body of one local save (spec §2.5: character
    /// section 6 groups, then variable section 8 built-in + 6 user-defined
    /// groups) — shared by [`fixture_local_text`] and
    /// [`fixture_local_text_with_marker`] so the only difference between
    /// the current-marker and old-marker fixtures is the marker line
    /// itself.
    fn extended_local_body() -> Vec<String> {
        [
            // --- character section (6 groups) ---
            "NICKNAME:EmuChan",
            EMU_SEPARATOR,
            "NO:7",
            EMU_SEPARATOR,
            "CSTR",
            "hello",
            "bye",
            FINISHED,
            EMU_SEPARATOR,
            "CFLAG",
            "1",
            "9",
            FINISHED,
            EMU_SEPARATOR,
            EMU_SEPARATOR, // string 2D (always empty)
            "RELATION",
            "3,0,0",
            "0,5",
            FINISHED,
            EMU_SEPARATOR,
            // --- variable section (8 built-in + 6 user-defined groups) ---
            "MES:こんにちは",
            EMU_SEPARATOR,
            "DAY:15",
            "MONEY:100",
            EMU_SEPARATOR,
            "SAVESTR",
            "store",
            FINISHED,
            EMU_SEPARATOR,
            "FLAG",
            "1",
            "1",
            "0",
            FINISHED,
            EMU_SEPARATOR,
            EMU_SEPARATOR, // string 2D (empty)
            EMU_SEPARATOR, // int 2D (empty in this fixture)
            EMU_SEPARATOR, // string 3D (empty)
            EMU_SEPARATOR, // int 3D (empty in this fixture)
            EMU_SEPARATOR, // user string 1D (empty)
            EMU_SEPARATOR, // user int 1D (empty)
            EMU_SEPARATOR, // user string 2D (empty)
            EMU_SEPARATOR, // user int 2D (empty)
            EMU_SEPARATOR, // user string 3D (empty)
            EMU_SEPARATOR, // user int 3D (empty)
        ]
        .into_iter()
        .map(str::to_owned)
        .collect()
    }

    /// One local save, hand-assembled line by line from the spec's own
    /// grammar (§2.1/§2.3/§2.4/§2.5) — not captured from any fixture file,
    /// so a parser bug that also happens to match a buggy fixture can't
    /// hide from this test.
    fn fixture_local_text() -> String {
        let mut lines: Vec<String> = vec![
            "12345".to_owned(),
            "1808".to_owned(),
            "test save".to_owned(),
            "1".to_owned(),
        ];
        lines.extend(empty_old_chara_lines());
        lines.extend(empty_old_global_lines());
        lines.push(EMU_START.to_owned());
        lines.extend(extended_local_body());
        lines.join("\n")
    }

    /// A global save's variable section per the *corrected* grammar (real
    /// captures — `global_text_utf8_real.sav` — are byte-exact 6
    /// `__EMU_SEPARATOR__` groups after the OLD block: no scalar groups at
    /// all, unlike a local save's `parse_variable_section`).
    fn fixture_global_text() -> String {
        let mut lines: Vec<String> = vec!["12345".to_owned(), "1808".to_owned()];
        lines.extend(empty_old_global_save_lines());
        lines.push(EMU_START.to_owned());
        lines.extend(
            [
                // --- global variable section: 6 groups, no scalars ---
                EMU_SEPARATOR, // string 1D (empty)
                "FLAG",
                "1",
                "1",
                "0",
                FINISHED,
                EMU_SEPARATOR, // int 1D
                EMU_SEPARATOR, // string 2D (empty)
                EMU_SEPARATOR, // int 2D (empty)
                EMU_SEPARATOR, // string 3D (empty)
                EMU_SEPARATOR, // int 3D (6th and final group)
            ]
            .into_iter()
            .map(str::to_owned),
        );
        lines.join("\n")
    }

    #[test]
    fn parse_text_reads_the_documented_local_grammar() {
        let (data, code, version, description) = parse_text(&fixture_local_text(), false).unwrap();
        assert_eq!(code, 12345);
        assert_eq!(version, 1808);
        assert_eq!(description, "test save");
        assert_eq!(data.charas.len(), 1);

        let chara = &data.charas[0];
        assert!(matches!(chara.get("NICKNAME"), Some(ParsedArray::StrScalar(s)) if s == "EmuChan"));
        assert!(matches!(chara.get("NO"), Some(ParsedArray::IntScalar(7))));
        assert!(
            matches!(chara.get("CSTR"), Some(ParsedArray::Str1D(v)) if v == &["hello".to_string(), "bye".to_string()])
        );
        assert!(matches!(chara.get("CFLAG"), Some(ParsedArray::Int1D(v)) if v == &[1, 9]));
        assert!(
            matches!(chara.get("RELATION"), Some(ParsedArray::Int2D(rows)) if rows == &vec![vec![3, 0, 0], vec![0, 5]])
        );

        assert!(matches!(data.globals.get("MES"), Some(ParsedArray::StrScalar(s)) if s == "こんにちは"));
        assert!(matches!(data.globals.get("DAY"), Some(ParsedArray::IntScalar(15))));
        assert!(matches!(data.globals.get("MONEY"), Some(ParsedArray::IntScalar(100))));
        assert!(
            matches!(data.globals.get("SAVESTR"), Some(ParsedArray::Str1D(v)) if v == &["store".to_string()])
        );
        assert!(matches!(data.globals.get("FLAG"), Some(ParsedArray::Int1D(v)) if v == &[1, 1, 0]));
    }

    #[test]
    fn parse_text_reads_the_documented_global_grammar_six_groups_no_scalars() {
        let (data, code, version, description) = parse_text(&fixture_global_text(), true).unwrap();
        assert_eq!(code, 12345);
        assert_eq!(version, 1808);
        assert_eq!(description, "");
        assert!(data.charas.is_empty());
        assert!(matches!(data.globals.get("FLAG"), Some(ParsedArray::Int1D(v)) if v == &[1, 1, 0]));
    }

    #[test]
    fn build_local_data_reconciles_every_shape_including_2d_padding() {
        let header = fixture_header();
        let (data, _, _, _) = parse_text(&fixture_local_text(), false).unwrap();
        let (storage, report) = build_local_data(data, &header);

        assert!(report.skipped.is_empty(), "unexpected skips: {:?}", report.skipped);
        // RELATION's second row ("0,5") had its trailing zero column trimmed
        // by Emuera's own writer, so the merge engine correctly reports it
        // as a partial import even though every cell is in fact recovered
        // (a trimmed trailing zero and an absent cell are indistinguishable
        // — see `place_2d`'s doc comment).
        assert_eq!(
            report.partial.iter().map(|p| p.name.as_str()).collect::<Vec<_>>(),
            vec!["RELATION"],
            "unexpected partial imports: {:?}",
            report.partial
        );
        assert_eq!(report.partial[0].expected_len, 6);
        assert_eq!(report.partial[0].found_len, 5);

        let day = get_interner().get_or_intern_static("DAY");
        let (_, var) = storage.variables.get(&day).unwrap();
        assert_eq!(var.clone().assume_normal().as_int().unwrap()[0], 15);

        let flag = get_interner().get_or_intern_static("FLAG");
        let (_, var) = storage.variables.get(&flag).unwrap();
        assert_eq!(var.clone().assume_normal().as_int().unwrap().as_slice(), &[1, 1, 0]);

        let mes = get_interner().get_or_intern_static("MES");
        let (_, var) = storage.variables.get(&mes).unwrap();
        assert_eq!(var.clone().assume_normal().as_str().unwrap()[0], "こんにちは");

        // Character-scope columns: one entry per character (here, one).
        let relation = get_interner().get_or_intern_static("RELATION");
        let (relation_info, var) = storage.variables.get(&relation).unwrap();
        assert_eq!(relation_info.size.as_slice(), &[2, 3]);
        let UniformVariable::Character(columns) = var else {
            panic!("RELATION must be chara-scope");
        };
        assert_eq!(columns.len(), 1);
        // File wrote rows "3,0,0" and "0,5" — the second row's trimmed
        // trailing zero column must come back padded, not dropped.
        assert_eq!(
            columns[0].clone().as_int().unwrap().as_slice(),
            &[3, 0, 0, 0, 5, 0]
        );

        let cflag = get_interner().get_or_intern_static("CFLAG");
        let (_, var) = storage.variables.get(&cflag).unwrap();
        let UniformVariable::Character(columns) = var else {
            panic!("CFLAG must be chara-scope");
        };
        assert_eq!(columns[0].clone().as_int().unwrap().as_slice(), &[1, 9]);
    }

    #[test]
    fn reconcile_skips_a_name_this_game_never_declared() {
        erars_ast::init_interner();
        let header = HeaderInfo::default();
        let mut report = ImportReport::default();
        let result = reconcile(
            "TOTALLY_UNDECLARED_VARIABLE_XYZ",
            ParsedArray::IntScalar(1),
            &header,
            false,
            true,
            &mut report,
        );
        assert!(result.is_none());
        assert_eq!(report.skipped.len(), 1);
        assert!(matches!(report.skipped[0].reason, SkipReason::NotDeclared));
    }

    #[test]
    fn reconcile_skips_a_chara_variable_presented_as_global() {
        erars_ast::init_interner();
        let header = fixture_header();
        let mut report = ImportReport::default();
        // NICKNAME is declared is_chara — asking for it as a non-chara,
        // is_global variable must be a scope mismatch, not silently accepted.
        let result = reconcile(
            "NICKNAME",
            ParsedArray::StrScalar("x".into()),
            &header,
            false,
            true,
            &mut report,
        );
        assert!(result.is_none());
        assert!(matches!(report.skipped[0].reason, SkipReason::ScopeMismatch));
    }

    #[test]
    fn reconcile_skips_a_non_savedata_variable() {
        erars_ast::init_interner();
        let mut global_variables = HashMap::new();
        let key = get_interner().get_or_intern_static("NOT_SAVEDATA_XYZ");
        global_variables.insert(
            key,
            VariableInfo {
                is_global: true,
                is_savedata: false,
                ..Default::default()
            },
        );
        let header = HeaderInfo {
            global_variables,
            ..Default::default()
        };
        let mut report = ImportReport::default();
        let result = reconcile(
            "NOT_SAVEDATA_XYZ",
            ParsedArray::IntScalar(1),
            &header,
            false,
            true,
            &mut report,
        );
        assert!(result.is_none());
        assert!(matches!(report.skipped[0].reason, SkipReason::ScopeMismatch));
    }

    #[test]
    fn reconcile_skips_a_type_mismatch() {
        let header = fixture_header();
        let mut report = ImportReport::default();
        // DAY is declared int; hand a string scalar for it instead.
        let result = reconcile(
            "DAY",
            ParsedArray::StrScalar("not an int".into()),
            &header,
            false,
            false,
            &mut report,
        );
        assert!(result.is_none());
        assert!(matches!(report.skipped[0].reason, SkipReason::TypeMismatch));
    }

    #[test]
    fn reconcile_skips_a_dimension_count_mismatch() {
        let header = fixture_header();
        let mut report = ImportReport::default();
        // FLAG is declared 1D; hand a 2D array for it instead.
        let result = reconcile(
            "FLAG",
            ParsedArray::Int2D(vec![vec![1, 2]]),
            &header,
            false,
            false,
            &mut report,
        );
        assert!(result.is_none());
        assert!(matches!(
            report.skipped[0].reason,
            SkipReason::DimensionMismatch { expected: 1, found: 2 }
        ));
    }

    #[test]
    fn reconcile_reports_a_partial_1d_import_both_directions() {
        let header = fixture_header();

        // File has fewer elements than this game declares: FLAG is size=[3].
        let mut report = ImportReport::default();
        let (_, _, var) = reconcile(
            "FLAG",
            ParsedArray::Int1D(vec![7]),
            &header,
            false,
            false,
            &mut report,
        )
        .unwrap();
        assert_eq!(report.partial.len(), 1);
        assert_eq!(report.partial[0].expected_len, 3);
        assert_eq!(report.partial[0].found_len, 1);
        let mut var = var;
        assert_eq!(var.as_int().unwrap().as_slice(), &[7, 0, 0]);

        // File has more elements than this game declares: excess is dropped,
        // not out-of-bounds written.
        let mut report = ImportReport::default();
        let (_, _, mut var) = reconcile(
            "FLAG",
            ParsedArray::Int1D(vec![1, 2, 3, 4, 5]),
            &header,
            false,
            false,
            &mut report,
        )
        .unwrap();
        assert_eq!(report.partial[0].expected_len, 3);
        assert_eq!(report.partial[0].found_len, 5);
        assert_eq!(var.as_int().unwrap().as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn reconcile_places_a_3d_array_leaving_an_omitted_block_at_its_default() {
        erars_ast::init_interner();
        let mut global_variables = HashMap::new();
        global_variables.insert(
            get_interner().get_or_intern_static("CUBE"),
            info(false, true, false, &[2, 2, 2]),
        );
        let header = HeaderInfo {
            global_variables,
            ..Default::default()
        };
        let mut report = ImportReport::default();
        // Only outer index 1 is present in the file; index 0 was entirely
        // default and Emuera's own writer omitted it.
        let (_, _, mut var) = reconcile(
            "CUBE",
            ParsedArray::Int3D(vec![(1, vec![vec![9], vec![8, 7]])]),
            &header,
            false,
            true,
            &mut report,
        )
        .unwrap();
        assert_eq!(
            var.as_int().unwrap().as_slice(),
            &[0, 0, 0, 0, 9, 0, 8, 7],
            "block 0 stays default; block 1's rows are placed and padded"
        );
    }

    #[test]
    fn sniff_recognises_binary_bom_utf8_and_non_unicode_text() {
        let non_unicode = encoding_rs::SHIFT_JIS;

        assert_eq!(
            sniff(&BINARY_MAGIC, non_unicode),
            Some(EmueraSaveVariant::Binary)
        );

        let plain_ascii = b"12345\n1808\n";
        assert_eq!(sniff(plain_ascii, non_unicode), Some(EmueraSaveVariant::TextUtf8));

        let mut with_bom = UTF8_BOM.to_vec();
        with_bom.extend_from_slice("12345\n1808\n".as_bytes());
        assert_eq!(sniff(&with_bom, non_unicode), Some(EmueraSaveVariant::TextUtf8));

        let mut sjis_bytes = b"12345\n1808\n".to_vec();
        let (encoded, _, had_errors) = non_unicode.encode("こんにちは");
        assert!(!had_errors);
        sjis_bytes.extend_from_slice(&encoded);
        sjis_bytes.push(b'\n');
        assert_eq!(sniff(&sjis_bytes, non_unicode), Some(EmueraSaveVariant::TextSjis));

        assert_eq!(sniff(b"not a save file at all", non_unicode), None);
    }

    // --- Real captures (primary cases, per `tests/fixtures/emuera_saves/
    // real/README.md`): all six files actually produced by running
    // `Emuera1818_kr3.exe` (eraTHYMKR v3.21, code 890016222, script version
    // 3210) under wine+Xvfb, one save each of the 3 container variants ×
    // (local slot 90, global). These caught all three corrections this
    // parser needed (binary magic, the whole binary reader, and the
    // global text grammar's missing scalar-groups branch) — the hand-built
    // fixtures above stay only for what these real captures don't
    // exercise: string 2D (Emuera's text writer can't emit it at all) and
    // an int 2D chara-scope variable with a trimmed trailing zero column
    // (eraTHYMKR's own chara-scope 2D int, `CDFLAG`, never needed trimming
    // in this particular capture).

    macro_rules! real_fixture {
        ($name:literal) => {
            include_bytes!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../../tests/fixtures/emuera_saves/real/",
                $name
            )) as &[u8]
        };
    }

    #[test]
    fn sniff_recognises_all_six_real_captures() {
        let sjis = encoding_rs::SHIFT_JIS;
        assert_eq!(
            sniff(real_fixture!("save90_text_utf8_real.sav"), sjis),
            Some(EmueraSaveVariant::TextUtf8)
        );
        // Both `*_sjis_real.sav` captures sniff as `TextUtf8`, not
        // `TextSjis` — correctly: this Korean game's Korean text is
        // unrepresentable in Shift-JIS, so Emuera's own writer degrades it
        // to literal `?` bytes (real README point 6), leaving the entire
        // file pure ASCII, which is byte-identical whether decoded as
        // UTF-8 or as the configured encoding (see the module doc comment
        // "Container variant and text encoding"). Sniffing genuinely cannot
        // recover "this was written non-Unicode" from content alone when
        // the content has no non-ASCII byte to begin with — not a defect.
        assert_eq!(
            sniff(real_fixture!("save90_text_sjis_real.sav"), sjis),
            Some(EmueraSaveVariant::TextUtf8)
        );
        assert_eq!(
            sniff(real_fixture!("save90_binary_real.sav"), sjis),
            Some(EmueraSaveVariant::Binary)
        );
        assert_eq!(
            sniff(real_fixture!("global_text_utf8_real.sav"), sjis),
            Some(EmueraSaveVariant::TextUtf8)
        );
        assert_eq!(
            sniff(real_fixture!("global_text_sjis_real.sav"), sjis),
            Some(EmueraSaveVariant::TextUtf8)
        );
        assert_eq!(
            sniff(real_fixture!("global_binary_real.sav"), sjis),
            Some(EmueraSaveVariant::Binary)
        );
    }

    #[test]
    fn parse_reads_the_real_local_utf8_capture() {
        // These values are the OLD block's — the mandatory, positional
        // block that (per `.il:108953-109155`) real Emuera reads
        // unconditionally before ever looking for an extended block, and
        // where nearly all of a real save's actual game state lives
        // (`DAY`, `MONEY`, `BASE`, ...). Byte-verified independently in
        // Python against this exact capture.
        let sjis = encoding_rs::SHIFT_JIS;
        let (data, code, version, _) = parse(
            EmueraSaveVariant::TextUtf8,
            real_fixture!("save90_text_utf8_real.sav"),
            sjis,
            false,
        )
        .unwrap();
        assert_eq!(code, 890016222);
        assert_eq!(version, 3210);
        assert_eq!(data.charas.len(), 1, "autosave slot 90 has exactly one character");
        let chara = &data.charas[0];
        assert!(matches!(chara.get("NAME"), Some(ParsedArray::StrScalar(s)) if s == "당신"));
        assert!(matches!(chara.get("CALLNAME"), Some(ParsedArray::StrScalar(s)) if s == "TEST"));
        assert!(matches!(chara.get("ISASSI"), Some(ParsedArray::IntScalar(0))));
        assert!(matches!(chara.get("NO"), Some(ParsedArray::IntScalar(0))));
        assert!(matches!(chara.get("BASE"), Some(ParsedArray::Int1D(v)) if v[..3] == [2500, 2000, 10000]));
        assert!(matches!(data.globals.get("DAY"), Some(ParsedArray::Int1D(v)) if v == &[1, 0, 0, 0, 1, 1, 1, 1]));
        assert!(
            matches!(data.globals.get("MONEY"), Some(ParsedArray::Int1D(v)) if v[..3] == [5000, 0, 0] && v.last() == Some(&1_000_000) && v.len() == 101)
        );
    }

    #[test]
    fn parse_reads_the_real_local_sjis_capture() {
        // SJIS is expected-lossy for this Korean game's Korean strings (spec
        // §6.6/real README point 6) — not a parse failure. `CALLNAME` and
        // `DAY`/`MONEY` (all-ASCII, all-numeric) survive regardless; `NAME`
        // (Korean) does not, so it is deliberately not asserted here.
        let sjis = encoding_rs::SHIFT_JIS;
        let (data, code, version, _) = parse(
            EmueraSaveVariant::TextSjis,
            real_fixture!("save90_text_sjis_real.sav"),
            sjis,
            false,
        )
        .unwrap();
        assert_eq!(code, 890016222);
        assert_eq!(version, 3210);
        assert_eq!(data.charas.len(), 1);
        assert!(matches!(data.charas[0].get("CALLNAME"), Some(ParsedArray::StrScalar(s)) if s == "TEST"));
        assert!(matches!(data.globals.get("DAY"), Some(ParsedArray::Int1D(v)) if v == &[1, 0, 0, 0, 1, 1, 1, 1]));
    }
    #[test]
    fn parse_reads_the_real_global_utf8_capture() {
        // The dedicated `global.sav` OLD block (`VariableData::
        // SaveGlobalToStream`/`LoadGlobalFromStream`, `.il:50912-50944`)
        // always carries exactly `GLOBAL` + `GLOBALS`, positionally — this
        // capture's `GLOBAL[0] == 100` (eraTHYMKR's own starting value) is
        // the one non-default value in the whole file. The extended
        // block's own 6 groups (a *different*, further set of any custom
        // `is_global` savedata this game declares) are, separately, all
        // empty in this particular capture — a fresh game's globals are
        // still at their declared default — which is itself grammar this
        // test also proves: parsing must still succeed and correctly stop
        // at 6 empty groups, not desync trying to read a 7th.
        let sjis = encoding_rs::SHIFT_JIS;
        let (data, code, version, description) = parse(
            EmueraSaveVariant::TextUtf8,
            real_fixture!("global_text_utf8_real.sav"),
            sjis,
            true,
        )
        .unwrap();
        assert_eq!(code, 890016222);
        assert_eq!(version, 3210);
        assert_eq!(description, "");
        assert!(data.charas.is_empty());
        assert_eq!(data.globals.len(), 2, "only GLOBAL + GLOBALS, from the OLD block");
        assert!(matches!(data.globals.get("GLOBAL"), Some(ParsedArray::Int1D(v)) if v.first() == Some(&100)));
        assert!(matches!(data.globals.get("GLOBALS"), Some(ParsedArray::Str1D(v)) if v.is_empty()));
    }

    #[test]
    fn parse_reads_the_real_global_sjis_capture() {
        let sjis = encoding_rs::SHIFT_JIS;
        let (data, code, version, _) = parse(
            EmueraSaveVariant::TextSjis,
            real_fixture!("global_text_sjis_real.sav"),
            sjis,
            true,
        )
        .unwrap();
        assert_eq!(code, 890016222);
        assert_eq!(version, 3210);
        assert!(data.charas.is_empty());
        assert!(matches!(data.globals.get("GLOBAL"), Some(ParsedArray::Int1D(v)) if v.first() == Some(&100)));
    }

    #[test]
    fn parse_binary_reads_the_real_local_capture_byte_exact() {
        let sjis = encoding_rs::SHIFT_JIS;
        let bytes = real_fixture!("save90_binary_real.sav");
        let (data, code, version, _) = parse(EmueraSaveVariant::Binary, bytes, sjis, false).unwrap();
        assert_eq!(code, 890016222);
        assert_eq!(version, 3210);
        assert_eq!(data.charas.len(), 1, "characterCount == 1");

        let chara = &data.charas[0];
        assert!(matches!(chara.get("ISASSI"), Some(ParsedArray::IntScalar(0))));
        assert!(matches!(chara.get("CALLNAME"), Some(ParsedArray::StrScalar(s)) if s == "TEST"));
        assert!(matches!(chara.get("BASE"), Some(ParsedArray::Int1D(v)) if v.len() == 100));
        // DOWNBASE is entirely zero in this fresh-game capture — the whole
        // 1000-element array must still come back full-length via the
        // zero-run terminator path, not truncated to nothing.
        assert!(matches!(chara.get("DOWNBASE"), Some(ParsedArray::Int1D(v)) if v.len() == 1000 && v.iter().all(|&x| x == 0)));
        // CDFLAG is this capture's only chara-scope int 2D array.
        assert!(matches!(chara.get("CDFLAG"), Some(ParsedArray::Int2D(_))));

        assert!(matches!(data.globals.get("DAY"), Some(ParsedArray::Int1D(v)) if v.len() == 1000));
        assert!(matches!(data.globals.get("MONEY"), Some(ParsedArray::Int1D(v)) if v.len() == 1000));
        // FILED_MAP is int 3D, 100x100x100 — the largest shape this capture
        // carries; must decode fully. (This capture's 3D array uses only
        // `0xF0` zero-runs and the value tags, not the structural
        // `0xF2`/`0xE1`/`0xF1`/`0xE0` markers — those are exercised by
        // `bin_cursor_read_*3d*_markers` below, against C#-writer-equivalent
        // bytes per `docs/research/2026-09-07-emuera-source-crosscheck.md`.)
        assert!(matches!(data.globals.get("FILED_MAP"), Some(ParsedArray::Int3D(blocks)) if blocks.len() == 100));
        assert_eq!(data.globals.len(), 79, "79 global records after the character's own EOC");
    }

    /// The 2D/3D *structural* markers (`EoA1{0xE0}`, `ZeroA1{0xF1}`,
    /// `EoA2{0xE1}`, `ZeroA2{0xF2}`) never appear in any of the six real
    /// captures, so these are proven against exact byte streams the 1824 C#
    /// writer would emit (verified independently in Python):
    /// `EraBinaryDataWriter.writeData(Int64[,])` (EraBinaryDataWriter.cs
    /// :167-215) / `(Int64[,,])` (:217-272) — see
    /// `docs/research/2026-09-07-emuera-source-crosscheck.md` §3.
    #[test]
    fn bin_cursor_read_int2d_uses_row_and_zero_markers() {
        // writeData(Int64[,]) for [[0,0,0],[5,0,7]]:
        //   dim0=2,dim1=3, then row 0 all-zero -> ZeroA1(0xF1) count 1,
        //   value 5, Zero(0xF0) count 1, value 7, row terminator EoA1(0xE0),
        //   EoD(0xFF).
        let bytes = [
            0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, // dims 2x3
            0xF1, 0x01, // one all-zero row
            0x05,       // row 1: value 5
            0xF0, 0x01, // one zero cell
            0x07,       // value 7
            0xE0,       // row terminator
            0xFF,       // end of array
        ];
        let mut cur = BinCursor::new(&bytes);
        let grid = cur.read_int2d().unwrap();
        assert_eq!(grid, vec![vec![0, 0, 0], vec![5, 0, 7]]);
    }

    #[test]
    fn bin_cursor_read_int3d_uses_plane_and_row_markers() {
        // writeData(Int64[,,]) for [[[0,0],[0,0]],[[1,0],[0,2]]]:
        //   dim0=2,dim1=2,dim2=2, then outer 0 all-zero -> ZeroA2(0xF2)
        //   count 1, value 1, row terminators EoA1(0xE0), Zero(0xF0) count 1
        //   + value 2 + EoA1, matrix terminator EoA2(0xE1), EoD(0xFF).
        let bytes = [
            0x02, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00,
            0x00,       // dims 2x2x2
            0xF2, 0x01, // one all-zero plane
            0x01,       // plane 1: value 1
            0xE0,       // row terminator
            0xF0, 0x01, // one zero cell
            0x02,       // value 2
            0xE0,       // row terminator
            0xE1,       // plane terminator
            0xFF,       // end of array
        ];
        let mut cur = BinCursor::new(&bytes);
        let planes = cur.read_int3d().unwrap();
        assert_eq!(
            planes,
            vec![
                (0, vec![vec![0, 0], vec![0, 0]]),
                (1, vec![vec![1, 0], vec![0, 2]]),
            ]
        );
    }

    #[test]
    fn parse_binary_reads_the_real_global_capture_byte_exact() {
        let sjis = encoding_rs::SHIFT_JIS;
        let bytes = real_fixture!("global_binary_real.sav");
        let (data, code, version, description) = parse(EmueraSaveVariant::Binary, bytes, sjis, true).unwrap();
        assert_eq!(code, 890016222);
        assert_eq!(version, 3210);
        assert_eq!(description, "");
        assert!(data.charas.is_empty());
        // `GLOBAL` is Emuera's own internal name for the legacy flat global
        // int array (`dataIntegerArray[63]`'s binary-format analogue), not
        // a name this game's own `#DIM` declares — it is expected to end up
        // `NotDeclared` at reconcile time, same as any unknown legacy name.
        assert!(
            matches!(data.globals.get("GLOBAL"), Some(ParsedArray::Int1D(v)) if v.len() == 2000 && v[0] == 100 && v[1..].iter().all(|&x| x == 0))
        );
    }

    /// [`fixture_local_text`] with `marker` standing in for the normal 1808
    /// marker. The extended body always follows (so a positive control with
    /// [`EMU_START`] reads it, and an old marker proves it is *not* read —
    /// the only variable is the marker line itself).
    fn fixture_local_text_with_marker(marker: &str) -> String {
        let mut lines: Vec<String> = vec![
            "12345".to_owned(),
            "1808".to_owned(),
            "test save".to_owned(),
            "1".to_owned(),
        ];
        lines.extend(empty_old_chara_lines());
        lines.extend(empty_old_global_lines());
        lines.push(marker.to_owned());
        lines.extend(extended_local_body());
        lines.join("\n")
    }

    /// A pre-extended-block save: the OLD block with no marker at all after
    /// it (valid pre-1.808 grammar — the extended block genuinely did not
    /// exist then).
    fn fixture_local_text_no_marker() -> String {
        let mut lines: Vec<String> = vec![
            "12345".to_owned(),
            "1808".to_owned(),
            "test save".to_owned(),
            "1".to_owned(),
        ];
        lines.extend(empty_old_chara_lines());
        lines.extend(empty_old_global_lines());
        lines.join("\n")
    }

    /// Maps each known Emuera version to the exact extended-block marker its
    /// writer emits (`EraDataStream.cs:46-50`).
    fn marker_for(version: u32) -> &'static str {
        match version {
            1700 => "__EMUERA_STRAT__",
            1708 => "__EMUERA_1708_STRAT__",
            1729 => "__EMUERA_1729_STRAT__",
            1803 => "__EMUERA_1803_STRAT__",
            1808 => "__EMUERA_1808_STRAT__",
            _ => unreachable!("no marker for version {version}"),
        }
    }

    /// A full local numbered save for a specific Emuera version: header,
    /// placeholder OLD blocks, that version's marker, then only the
    /// extended groups that version's grammar actually carries, each with
    /// **distinct sentinel values** so a version-gate miscalibration (one
    /// group too few/many, or a later group shifted into an earlier slot)
    /// shows up as a wrong value or a wrong key rather than a silent
    /// mis-import. Grammar per-version (from the C# reader dispatch,
    /// `docs/research/2026-09-07-emuera-source-crosscheck.md` §6):
    /// - chara extended: `Old1802` 4 groups (`strS,intS,str1D,int1D`) for
    ///   version < 1803, else 6 groups (+ `str2D,int2D`)
    ///   (`VariableEvaluator.LoadFromStream`; `CharacterData.cs:400-428`
    ///   vs `:355-399`)
    /// - variable extended: built-in `strS,intS,str1D,int1D` always, then
    ///   `2D` iff version >= 1708, `3D` iff version >= 1729, then the six
    ///   user-defined groups iff version >= 1808
    ///   (`VariableData.LoadFromStreamExtended` `:763-832`; gates
    ///   `EraDataStream.cs:297,344,366,424`)
    fn local_save_for(version: u32) -> String {
        let mut l: Vec<String> = vec![
            "12345".to_owned(),
            "1808".to_owned(),
            "test save".to_owned(),
            "1".to_owned(),
        ];
        l.extend(empty_old_chara_lines());
        l.extend(empty_old_global_lines());
        l.push(marker_for(version).to_owned());

        // --- chara extended ---
        l.push("CH_STR_SCALAR:chara_str_scalar".to_owned()); // str scalar
        l.push(EMU_SEPARATOR.to_owned());
        l.push("CH_INT_SCALAR:42".to_owned()); // int scalar
        l.push(EMU_SEPARATOR.to_owned());
        l.extend(["CH_STR_1D", "chara_str1d_a", "chara_str1d_b", FINISHED, EMU_SEPARATOR]
            .map(str::to_owned));
        l.extend(["CH_INT_1D", "3", "4", FINISHED, EMU_SEPARATOR]
            .map(str::to_owned));
        if version >= 1803 {
            l.push(EMU_SEPARATOR.to_owned()); // string 2D (never present in text)
            l.extend(["CH_INT_2D", "1,2", "3,4,5", FINISHED, EMU_SEPARATOR]
                .map(str::to_owned));
        }

        // --- variable section ---
        l.push("VAR_STR_SCALAR:var_str_scalar".to_owned()); // str scalar
        l.push(EMU_SEPARATOR.to_owned());
        l.push("VAR_INT_SCALAR:99".to_owned()); // int scalar
        l.push(EMU_SEPARATOR.to_owned());
        l.extend(["VAR_STR_1D", "var_str1d_a", "var_str1d_b", FINISHED, EMU_SEPARATOR]
            .map(str::to_owned));
        l.extend(["VAR_INT_1D", "7", "8", "9", FINISHED, EMU_SEPARATOR]
            .map(str::to_owned));
        if version >= 1708 {
            l.push(EMU_SEPARATOR.to_owned()); // string 2D (never present in text)
            l.extend(["VAR_INT_2D", "1,2", "3", FINISHED, EMU_SEPARATOR]
                .map(str::to_owned));
        }
        if version >= 1729 {
            l.push(EMU_SEPARATOR.to_owned()); // string 3D (never present in text)
            l.extend(
                ["VAR_INT_3D", "[0]{", "1,2", "}", "[2]{", "3", "}", FINISHED, EMU_SEPARATOR]
                    .map(str::to_owned),
            );
        }
        if version >= 1808 {
            l.extend(["USR_STR_1D", "usr_str1d", FINISHED, EMU_SEPARATOR]
                .map(str::to_owned));
            l.extend(["USR_INT_1D", "11", FINISHED, EMU_SEPARATOR]
                .map(str::to_owned));
            l.push(EMU_SEPARATOR.to_owned()); // user string 2D (ever empty)
            l.extend(["USR_INT_2D", "5", FINISHED, EMU_SEPARATOR]
                .map(str::to_owned));
            l.push(EMU_SEPARATOR.to_owned()); // user string 3D (ever empty)
            l.extend(["USR_INT_3D", "[1]{", "6,7", "}", FINISHED, EMU_SEPARATOR]
                .map(str::to_owned));
        }
        l.join("\n")
    }

    /// Asserts the *exact* extended-group inventory a version should carry
    /// (`ExtendedMarker` equals that version), with each present group's
    /// sentinel value intact and each version-gated-absent group truly
    /// absent — a shifted group would land in a wrong key/value and fail
    /// here rather than silently mis-import.
    fn assert_local_groups(data: &EmueraSaveData, version: u32) {
        assert_eq!(data.extended_marker, ExtendedMarker::Known(version));

        let chara = &data.charas[0];
        assert!(matches!(chara.get("CH_STR_SCALAR"), Some(ParsedArray::StrScalar(s)) if s == "chara_str_scalar"));
        assert!(matches!(chara.get("CH_INT_SCALAR"), Some(ParsedArray::IntScalar(v)) if *v == 42));
        assert!(matches!(chara.get("CH_STR_1D"), Some(ParsedArray::Str1D(v)) if v.iter().map(String::as_str).eq(["chara_str1d_a", "chara_str1d_b"])));
        assert!(matches!(chara.get("CH_INT_1D"), Some(ParsedArray::Int1D(v)) if v.as_slice() == [3, 4]));
        if version >= 1803 {
            assert!(
                matches!(chara.get("CH_INT_2D"), Some(ParsedArray::Int2D(r)) if r.as_slice() == [vec![1, 2], vec![3, 4, 5]]),
                "chara 2D present from 1803 (version {version})"
            );
        } else {
            assert!(chara.get("CH_INT_2D").is_none(), "chara 2D absent before 1803 (version {version})");
        }

        let g = &data.globals;
        assert!(matches!(g.get("VAR_STR_SCALAR"), Some(ParsedArray::StrScalar(s)) if s == "var_str_scalar"));
        assert!(matches!(g.get("VAR_INT_SCALAR"), Some(ParsedArray::IntScalar(v)) if *v == 99));
        assert!(matches!(g.get("VAR_STR_1D"), Some(ParsedArray::Str1D(v)) if v.iter().map(String::as_str).eq(["var_str1d_a", "var_str1d_b"])));
        assert!(matches!(g.get("VAR_INT_1D"), Some(ParsedArray::Int1D(v)) if v.as_slice() == [7, 8, 9]));
        if version >= 1708 {
            assert!(
                matches!(g.get("VAR_INT_2D"), Some(ParsedArray::Int2D(r)) if r.as_slice() == [vec![1, 2], vec![3]]),
                "var 2D present from 1708 (version {version})"
            );
        } else {
            assert!(g.get("VAR_INT_2D").is_none(), "var 2D absent before 1708 (version {version})");
        }
        if version >= 1729 {
            assert!(
                matches!(g.get("VAR_INT_3D"), Some(ParsedArray::Int3D(b)) if b.as_slice() == [(0, vec![vec![1, 2]]), (2, vec![vec![3]])]),
                "var 3D present from 1729 (version {version})"
            );
        } else {
            assert!(g.get("VAR_INT_3D").is_none(), "var 3D absent before 1729 (version {version})");
        }
        if version >= 1808 {
            assert!(matches!(g.get("USR_STR_1D"), Some(ParsedArray::Str1D(v)) if v.iter().map(String::as_str).eq(["usr_str1d"])));
            assert!(matches!(g.get("USR_INT_1D"), Some(ParsedArray::Int1D(v)) if v.as_slice() == [11]));
            assert!(matches!(g.get("USR_INT_2D"), Some(ParsedArray::Int2D(r)) if r.as_slice() == [vec![5]]));
            assert!(matches!(g.get("USR_INT_3D"), Some(ParsedArray::Int3D(b)) if b.as_slice() == [(1, vec![vec![6, 7]])]));
        } else {
            for k in ["USR_STR_1D", "USR_INT_1D", "USR_INT_2D", "USR_INT_3D"] {
                assert!(g.get(k).is_none(), "{k} must be absent before 1808 (version {version})");
            }
        }
    }

    /// 1700: the earliest marker. Chara extended = 4 groups (`Old1802`,
    /// no 2D), variable extended = 4 groups (no 2D/3D). This is the whole
    /// point of old-save support: the OLD block + scalars + 1D arrays.
    #[test]
    fn parse_text_reads_the_1700_four_group_extended_grammar() {
        let (data, ..) = parse_text(&local_save_for(1700), false).unwrap();
        assert_local_groups(&data, 1700);
    }

    /// 1708: the 2D boundary — variable extended gains the int 2D group,
    /// but chara is still 4 groups (`< 1803`) and 3D is still absent.
    #[test]
    fn parse_text_reads_the_1708_six_group_extended_grammar() {
        let (data, ..) = parse_text(&local_save_for(1708), false).unwrap();
        assert_local_groups(&data, 1708);
    }

    /// 1729: the 3D boundary — variable extended gains the int 3D group.
    /// Chara STILL 4 groups (1729 < 1803): 3D appears in the variable
    /// section two versions before the chara 2D restructure — exactly the
    /// asymmetry that desynchronises a reader that ties chara's grammar to
    /// the variable grammar.
    #[test]
    fn parse_text_reads_the_1729_eight_group_extended_grammar() {
        let (data, ..) = parse_text(&local_save_for(1729), false).unwrap();
        assert_local_groups(&data, 1729);
    }

    /// 1803: the chara restructure — chara extended jumps to 6 groups
    /// (gains CSTR-style int 2D), variable extended stays 8 (no user
    /// groups; 1803 < 1808).
    #[test]
    fn parse_text_reads_the_1803_eight_group_extended_grammar() {
        let (data, ..) = parse_text(&local_save_for(1803), false).unwrap();
        assert_local_groups(&data, 1803);
    }

    /// 1808: the full grammar — chara 6 groups, variable extended 8
    /// built-in + 6 user-defined groups (the user `#DIM SAVEDATA` pass,
    /// `if (version < 1808) return;` at `VariableData.cs:799-801`), which
    /// pre-1808 versions genuinely do not carry.
    #[test]
    fn parse_text_reads_the_1808_fourteen_group_extended_grammar() {
        let (data, ..) = parse_text(&local_save_for(1808), false).unwrap();
        assert_local_groups(&data, 1808);
    }

    /// Every known version now reads its extended block, so the end-of-load
    /// summary must NOT flag any of them as a skipped/absent extended block.
    #[test]
    fn build_report_does_not_flag_a_known_version() {
        for v in [1700, 1708, 1729, 1803, 1808] {
            let (data, ..) = parse_text(&local_save_for(v), false).unwrap();
            assert_eq!(data.extended_marker, ExtendedMarker::Known(v));
            let (_, report) = build_local_data(data, &fixture_header());
            assert!(
                report.extended_marker_skipped.is_none(),
                "known version {v} must not be reported as a skip"
            );
        }
    }

    /// An unknown/future `__EMUERA_..._STRAT__` marker is *still* reported,
    /// named, and its extended block is *not* read — we cannot claim to
    /// understand a grammar we have never seen. The marker's text is kept
    /// on [`ExtendedMarker::Unknown`] so the report can name it.
    #[test]
    fn parse_text_reports_an_unknown_future_marker_and_skips() {
        let (data, ..) =
            parse_text(&fixture_local_text_with_marker("__EMUERA_1900_STRAT__"), false).unwrap();
        assert_eq!(
            data.extended_marker,
            ExtendedMarker::Unknown("__EMUERA_1900_STRAT__".into())
        );
        let chara = &data.charas[0];
        assert!(chara.get("NICKNAME").is_none(), "unknown-marker extended block must not be read");
        assert!(data.globals.get("MES").is_none(), "unknown-marker extended block must not be read");
        let (_, report) = build_local_data(data, &fixture_header());
        assert!(
            report.extended_marker_skipped.as_deref().is_some_and(|d| d.contains("__EMUERA_1900_STRAT__")),
            "report must name the unknown marker, got {skipped:?}",
            skipped = report.extended_marker_skipped
        );
    }

    /// A save with *no* extended block at all (genuinely pre-extended, or
    /// truncated) is a real state distinct from one we merely couldn't
    /// read — it stays reportable as [`ExtendedMarker::Absent`].
    #[test]
    fn parse_text_reports_an_absent_marker() {
        let (data, ..) = parse_text(&fixture_local_text_no_marker(), false).unwrap();
        assert_eq!(data.extended_marker, ExtendedMarker::Absent);
        let (_, report) = build_local_data(data, &fixture_header());
        assert!(report.extended_marker_skipped.is_some(), "an absent marker must be reported");
    }

    /// A standalone `global.sav`'s variable section is 6 groups with no
    /// scalars; its 2D/3D groups are gated identically to the local
    /// variable section (2D iff >= 1708, 3D iff >= 1729).
    fn global_save_for(version: u32) -> String {
        let mut l: Vec<String> = vec!["12345".to_owned(), "1808".to_owned()];
        l.extend(empty_old_global_save_lines()); // GLOBAL, GLOBALS
        l.push(marker_for(version).to_owned());
        l.extend(["GV_STR_1D", "gs1", "gs2", FINISHED, EMU_SEPARATOR].map(str::to_owned));
        l.extend(["GV_INT_1D", "5", "6", FINISHED, EMU_SEPARATOR].map(str::to_owned));
        if version >= 1708 {
            l.push(EMU_SEPARATOR.to_owned()); // string 2D
            l.extend(["GV_INT_2D", "1,2", "3,4", FINISHED, EMU_SEPARATOR].map(str::to_owned));
        }
        if version >= 1729 {
            l.push(EMU_SEPARATOR.to_owned()); // string 3D
            l.extend(["GV_INT_3D", "[0]{", "9", "}", FINISHED, EMU_SEPARATOR].map(str::to_owned));
        }
        l.join("\n")
    }

    #[test]
    fn parse_global_section_gates_2d_and_3d_by_version() {
        // 1700: only string 1D + int 1D.
        let (data, ..) = parse_text(&global_save_for(1700), true).unwrap();
        assert_eq!(data.extended_marker, ExtendedMarker::Known(1700));
        assert!(matches!(data.globals.get("GV_STR_1D"), Some(ParsedArray::Str1D(v)) if v.iter().map(String::as_str).eq(["gs1", "gs2"])));
        assert!(matches!(data.globals.get("GV_INT_1D"), Some(ParsedArray::Int1D(v)) if v.as_slice() == [5, 6]));
        assert!(data.globals.get("GV_INT_2D").is_none());
        assert!(data.globals.get("GV_INT_3D").is_none());

        // 1729: + string 2D + int 2D + int 3D.
        let (data, ..) = parse_text(&global_save_for(1729), true).unwrap();
        assert_eq!(data.extended_marker, ExtendedMarker::Known(1729));
        assert!(matches!(data.globals.get("GV_INT_2D"), Some(ParsedArray::Int2D(r)) if r.as_slice() == [vec![1, 2], vec![3, 4]]));
        assert!(matches!(data.globals.get("GV_INT_3D"), Some(ParsedArray::Int3D(b)) if b.as_slice() == [(0, vec![vec![9]])]));
    }


    // =========================================================================
    // Real captures from genuinely old Emuera binaries (added 2026-09-07)
    // Byte-verified against real old-Emuera saves produced under wine+Xvfb by
    // old mainline exes (1.701/1.707/1.710/1.738/1.803) recovered from the
    // JAIST mirror of the archived SourceForge.jp/OSDN "emuera" project. Full
    // provenance in tests/fixtures/emuera_saves/real_old/README.md. These
    // validate the four old marker strings and the per-version extended
    // grammar that only the C# reader dispatch previously implied.
    macro_rules! old_real_fixture {
        ($name:literal) => {
            include_bytes!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../../tests/fixtures/emuera_saves/real_old/",
                $name
            )) as &[u8]
        };
    }

    /// The marker each old Emuera exe’s real save carries (README table).
    fn old_real_marker(version: u32) -> ExtendedMarker {
        match version {
            1700 => ExtendedMarker::Known(1700), // from Emuera 1.707
            1708 => ExtendedMarker::Known(1708), // from Emuera 1.710
            1729 => ExtendedMarker::Known(1729), // from Emuera 1.738
            1803 => ExtendedMarker::Known(1803), // from Emuera 1.803
            _ => unreachable!("no real old capture for {version}"),
        }
    }

    fn assert_old_real_capture(version: u32, bytes: &[u8], file: &str) {
        let sjis = encoding_rs::SHIFT_JIS;
        assert_eq!(sniff(bytes, sjis), Some(EmueraSaveVariant::TextUtf8), "{file}");
        let (data, code, gversion, _) =
            parse(EmueraSaveVariant::TextUtf8, bytes, sjis, false).unwrap();
        assert_eq!(code, 999000001, "{file}: game code");
        assert_eq!(gversion, 1000, "{file}: GameBase version");
        assert_eq!(data.extended_marker, old_real_marker(version), "{file} marker->grammar");
    }

    /// The four old-marker branches are now backed by real saves.
    #[test]
    fn parse_reads_real_old_marker_captures() {
        assert_old_real_capture(1700, old_real_fixture!("1707_real.sav"), "1707_real.sav");
        assert_old_real_capture(1708, old_real_fixture!("1710_real.sav"), "1710_real.sav");
        assert_old_real_capture(1729, old_real_fixture!("1738_real.sav"), "1738_real.sav");
        assert_old_real_capture(1803, old_real_fixture!("1803_real.sav"), "1803_real.sav");
    }

    /// Emuera 1.701 predates the extended block entirely and writes no marker:
    /// the reader must report `Absent`, not desync.
    #[test]
    fn parse_reports_1701_real_capture_as_absent() {
        let sjis = encoding_rs::SHIFT_JIS;
        let (data, code, gversion, _) = parse(
            EmueraSaveVariant::TextUtf8,
            old_real_fixture!("1701_real.sav"),
            sjis,
            false,
        )
        .unwrap();
        assert_eq!(code, 999000001);
        assert_eq!(gversion, 1000);
        assert_eq!(data.extended_marker, ExtendedMarker::Absent);
    }

    /// The 1803 chara boundary — the chara extended section's 4-vs-6 group
    /// restructure — is now byte-observed with a real *payload* in the new
    /// groups, not just an empty-separator count. Two games sharing the
    /// same source (full text, including the CDFLAG negative result below,
    /// in `real_old/README.md`) each `ADDVOIDCHARA` a character and set
    /// chara-scope values by explicit index with no `TARGET`
    /// (`CSTR:0:0`, `CFLAG:0:1`, `CFLAG:0:2`): one save written by
    /// Emuera1738 (marker `__EMUERA_1729_STRAT__`, version 1729 < 1803),
    /// whose writer emits the `LoadFromStreamExtended_Old1802` reader's
    /// **4** chara groups (strS,intS,str1D,int1D); the other by Emuera1803
    /// (marker `__EMUERA_1803_STRAT__`) whose writer emits all **6**
    /// (adding the 2D groups str2D,int2D) and *additionally* sets
    /// `CDFLAG:0:0:0 = 42` — real Emuera's only chara-scope int-2D
    /// savedata variable, so it is exactly what the 1803-only int2D group
    /// exists to carry.
    ///
    /// **CDFLAG could not be added to the 1738 side**: real Emuera1738
    /// (product version 1.736) rejects `CDFLAG:0:0:0 = 42` — and even the
    /// 2-arg form `CDFLAG:0:0 = 42` — at *parse* time with "라벨문·명령문·
    /// 대입문 어느 것으로도 해석할 수 없는 행입니다" ("cannot be
    /// interpreted as a label/command/assignment statement"), the generic
    /// unknown-statement error, not a range/argument-count error. Compare
    /// the same exe on `RELATION:0:0:0 = 5` (also chara+int2D, but *not*
    /// new at 1803): that fails at *runtime* instead, with "캐릭터 변수
    /// RELATION의 인수가 너무 많습니다" ("too many arguments") — a
    /// recognised-identifier error. The difference is diagnostic: `CDFLAG`
    /// is not a token Emuera1738's parser knows at all. [INFERENCE] `CDFLAG`
    /// itself was introduced into Emuera at or after 1.803, not merely
    /// reframed into new save groups at that version — the two changes
    /// (variable added, save grammar gains the groups to persist it)
    /// plausibly shipped together, though only the save-format side is
    /// directly evidenced here. Either way this is a real, reproduced
    /// negative result, not a skipped step: **the 1738 capture below has no
    /// CDFLAG line and cannot have one**, so the two fixtures are
    /// deliberately asymmetric.
    ///
    /// A marker-normalised comparison of the two saves' chara sections
    /// (bytes after the per-version marker line): identical through the
    /// int1D group's separator, then 1738 ends the char section with one
    /// more bare separator (its 4th and last group) while 1803 continues
    /// with the 1803-only str2D separator, then `CDFLAG`/`42`/`__FINISHED`
    /// (its int2D group's real content) and *that* group's separator — 5
    /// extra lines total (2 bare separators for the two new groups, 3
    /// content lines for the one value living in the second of them),
    /// exactly the 4-vs-6 restructure with a real value inside it rather
    /// than the coincidental "two empty separators" a value-free capture
    /// could not distinguish from noise.
    ///
    /// Both import with the chara values landing in the right variables:
    /// `CSTR` in the chara string-1D group, `CFLAG` in the per-character
    /// OLD block (positional, not a keyed extended entry — `CFLAG` is
    /// `CHAR_OLD_ARR[9]`), and (1803 only) `CDFLAG` in the chara int-2D
    /// group. (The old writer stores this string literal's value
    /// including its surrounding quotes — `CSTR:0:0 = "cap_name"` writes
    /// `"cap_name"`, 11 bytes, not `cap_name` — on *both* 1738 and 1803,
    /// so it is not a marker-version difference; a real modern-era capture
    /// (`save90_text_utf8_real.sav`, line 4677, game eraTHYMKR) stores its
    /// `CSTR` value unquoted, but that capture's `CSTR` comes from a
    /// character CSV's `CSTR,*,**` field rather than an ERB literal
    /// assignment, a different origin, not proven to be a version
    /// difference — [INFERENCE] flagged, not asserted as fact. What *is*
    /// verified directly against this crate's source: [`LineCursor::
    /// read_1d_arrays`] never strips quote characters at all, for any
    /// version — every string-1D value, `CSTR` included, round-trips
    /// byte-for-byte from file to [`ParsedArray::Str1D`]. So erars is
    /// internally consistent regardless of which of these two real-world
    /// shapes a save carries; there is no reader-side quote-handling bug to
    /// fix, only an upstream-Emuera authoring-path difference to be aware
    /// of when comparing values across captures.)
    #[test]
    fn parse_reads_real_old_chara_captures() {
        let sjis = encoding_rs::SHIFT_JIS;
        for (version, file, bytes) in [
            (1729, "1738_chara_real.sav", old_real_fixture!("1738_chara_real.sav")),
            (1803, "1803_chara_real.sav", old_real_fixture!("1803_chara_real.sav")),
        ] {
            assert_eq!(sniff(bytes, sjis), Some(EmueraSaveVariant::TextUtf8), "{file}");
            let (data, code, gversion, _) =
                parse(EmueraSaveVariant::TextUtf8, bytes, sjis, false).unwrap();
            assert_eq!(code, 999000001, "{file}: game code");
            assert_eq!(gversion, 1000, "{file}: GameBase version");
            assert_eq!(
                data.extended_marker,
                old_real_marker(version),
                "{file} marker -> grammar"
            );
            assert_eq!(data.charas.len(), 1, "{file}: ADDVOIDCHARA created one chara");
            let chara = &data.charas[0];
            assert!(
                matches!(chara.get("CSTR"), Some(ParsedArray::Str1D(v)) if v.as_slice() == ["\"cap_name\""]),
                "{file}: CSTR:0:0 value landed in the chara string-1D group, quotes and all"
            );
            assert!(
                matches!(chara.get("CFLAG"), Some(ParsedArray::Int1D(v)) if v.as_slice() == [0, 7, 13]),
                "{file}: CFLAG:0:1=7 and CFLAG:0:2=13 landed in the OLD-block CFLAG array"
            );
            if version >= 1803 {
                assert!(
                    matches!(chara.get("CDFLAG"), Some(ParsedArray::Int2D(rows)) if rows.as_slice() == [vec![42]]),
                    "{file}: CDFLAG:0:0:0=42 landed in the 1803-only chara int-2D group"
                );
            } else {
                assert!(
                    chara.get("CDFLAG").is_none(),
                    "{file}: the 1729 grammar has no chara int-2D group at all, so CDFLAG cannot appear \
                     (and Emuera1738 refuses to even parse a CDFLAG assignment — see the fn doc comment)"
                );
            }
        }
    }

    /// The version gate is what keeps the 4-vs-6 grammar from being
    /// interchangeable — checked in both directions, honestly reporting
    /// where each direction's evidence comes from.
    ///
    /// **1738 read as 1803 (under-read → over-consume): rejected outright.**
    /// The reader tries to consume two more chara groups (str2D, int2D)
    /// than a real 4-group file carries, walks into the variable section's
    /// own bytes expecting `__EMU_SEPARATOR__`/array-key syntax, and errors.
    /// This direction *is* a value-level discriminator: the file plainly
    /// cannot parse under the wrong grammar.
    ///
    /// **1803 read as 1729 (over-read → under-consume): also rejected, and
    /// for a reason CDFLAG makes concrete.** Without CDFLAG (the dead
    /// session's original pair) this direction did not error at all: every
    /// group past the 1729 grammar's 4th is empty in that capture, so
    /// under-consuming just hands the 1729 reader a string-scalar group
    /// that happens to start with `__EMU_SEPARATOR__` — a valid empty
    /// group — and parsing silently "succeeds" while proving nothing. With
    /// `CDFLAG` set, the 1729 grammar's char section still stops one group
    /// early, but now the very next line the 1729 reader sees is the bare
    /// word `CDFLAG` (the 1803-only int2D group's key) where it expects
    /// either `__EMU_SEPARATOR__` or a `KEY:VALUE` string-scalar line —
    /// `CDFLAG` has no `:`, so [`LineCursor::read_scalars`] rejects it
    /// outright ("스칼라 변수 줄이 아닙니다" / "not a scalar variable
    /// line"). So with real payload in the new groups, *both* directions
    /// of the wrong-grammar comparison now fail to parse; before adding
    /// CDFLAG only one direction did, and the other was silently
    /// unfalsifiable on these particular (all-empty) captures.
    #[test]
    fn parse_real_old_chara_wrong_grammar_is_rejected() {
        let sjis = encoding_rs::SHIFT_JIS;

        // 1738 save rewritten to claim the 1803 marker: parse must now use
        // the 6-group chara grammar on a file that only carries 4 chara
        // groups, and must fail rather than silently misread.
        let mut b1738 = old_real_fixture!("1738_chara_real.sav").to_vec();
        let pos = b1738
            .windows(b"__EMUERA_1729_STRAT__".len())
            .position(|w| w == b"__EMUERA_1729_STRAT__")
            .expect("1729 marker");
        b1738[pos..pos + b"__EMUERA_1729_STRAT__".len()].copy_from_slice(b"__EMUERA_1803_STRAT__");
        assert!(
            parse(EmueraSaveVariant::TextUtf8, &b1738, sjis, false).is_err(),
            "the 6-group grammar must not parse a real 4-group (1738) chara save"
        );

        // 1803 save rewritten to claim the 1729 marker: the 4-group grammar
        // under-consumes the char section by two groups, so the reader
        // trips directly over the bare `CDFLAG` key line where it expects a
        // `KEY:VALUE` string-scalar line or a separator — a real parse
        // error, not a silent misfile, precisely because CDFLAG carries an
        // actual value here (see the fn doc comment for the empty-capture
        // contrast).
        let mut b1803 = old_real_fixture!("1803_chara_real.sav").to_vec();
        let pos = b1803
            .windows(b"__EMUERA_1803_STRAT__".len())
            .position(|w| w == b"__EMUERA_1803_STRAT__")
            .expect("1803 marker");
        b1803[pos..pos + b"__EMUERA_1803_STRAT__".len()].copy_from_slice(b"__EMUERA_1729_STRAT__");
        match parse(EmueraSaveVariant::TextUtf8, &b1803, sjis, false) {
            Ok(_) => panic!("the 4-group grammar must not parse a real 6-group (1803) chara save either"),
            Err(err) => assert!(
                err.to_string().contains("CDFLAG"),
                "the parse must fail specifically on the misaligned CDFLAG line, not some unrelated cause: {err}"
            ),
        }
    }
}
