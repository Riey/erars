//! Writes real Emuera's own save-file formats — the inverse of this
//! module's reader (`super`). See `docs/research/2026-09-06-emuera-save-format.md`
//! and `docs/research/2026-09-07-emuera-source-crosscheck.md` for the
//! byte-level grammar; every algorithm here is transcribed directly from
//! the real C# writer sources (`EraBinaryDataWriter.cs`, `EraDataStream.cs`
//! `EraDataWriter`), not reverse-engineered from the reader.
//!
//! ## Why order matters
//!
//! [`super::EmueraSaveData`]'s maps are [`IndexMap`], not a plain hash map:
//! real Emuera's own writer emits built-in/user-defined variables in a
//! fixed enum/declaration order that this crate cannot fully reconstruct
//! from a foreign game's CSVs alone. Preserving *insertion* order from the
//! reader (which inserts in exactly the file's own order) instead means
//! writing back an [`super::EmueraSaveData`] that was just parsed replays
//! the original order byte-for-byte, without needing to know it — this is
//! what makes the round-trip test in `emuera.rs`'s `tests` module possible.
//! A freshly built export ([`super::super::export`]) instead controls
//! insertion order directly to match `HeaderInfo`'s own declaration order,
//! which is not required to match real Emuera's (nothing reading depends
//! on it — see the reader module's "Variable mapping policy" doc comment).
//!
//! ## Group placement (built-in-extended vs. user-defined)
//!
//! Real Emuera's chara-scope extended section (`CharacterData.
//! SaveToStreamExtended`) writes both its own extended-only built-ins
//! (`NICKNAME`, `MASTERNAME`, `CSTR`, `DOWNBASE`, `CUP`, `CDOWN`, `TCVAR`,
//! `CDFLAG`) *and* this game's user `#DIM SAVEDATA` chara arrays into the
//! same six shape-keyed groups (`VariableData.SaveToStreamExtended` /
//! `CharacterData`'s own copy — cross-checked source, `crosscheck.md` §2.5
//! wasn't specific here, but the group *count* the reader validates
//! byte-exact against `save90_text_utf8_real.sav` is exactly 6, not 12).
//! So chara-scope bucketing is simply "every non-OLD name of matching
//! shape, in file order" — no built-in/user split needed.
//!
//! The *global*-scope extended section is different: its 8 built-in groups
//! (`VariableData.SaveToStreamExtended`, `VariableData.cs:689-741`, via
//! `GetExtSaveList`) are populated by the writer, and this crate's own
//! writer previously got that wrong *twice* — first claiming no
//! `VariableCode` member could ever land there, then (having found
//! `RANDDATA` in the enum) claiming the group was empty in practice
//! anyway "empirically, every real capture examined so far". Both claims
//! are refuted by a fixture already in this crate's own corpus:
//! `tests/fixtures/emuera_saves/real_rand/randcap90_real.sav` (see its
//! README) has a non-zero `RANDDATA` sitting three `__EMU_SEPARATOR__`s
//! after the marker — the *fourth* of the local save's 14 extended
//! groups, i.e. the built-in int-1D group, not any of the six
//! user-defined ones (which start only after all 8 built-ins, group 10
//! onward; the fixture's own `#DIM SAVEDATA RANDCAP` array lands there,
//! nine separators in). `round_trip_real_captures_are_byte_exact` and
//! `export_local_places_randdata_in_builtin_int1d_group` (`emuera.rs`'s
//! `tests` module) both pin this exact layout.
//!
//! The source confirms the fixture, not just the enum flags in isolation:
//! `VariableData.SaveToStreamExtended` calls `GetExtSaveList` once per
//! shape and unconditionally writes every code it returns
//! (`VariableCode.cs:100,114,181-193`); `GetExtSaveList`
//! (`VariableIdentifier.cs:264-271`) is populated by every
//! `__SAVE_EXTENDED__`-flagged member at static-init time
//! (`VariableIdentifier.cs:249-260`), keyed by exactly the shape bits
//! (`__ARRAY_1D__`/`__ARRAY_2D__`/`__ARRAY_3D__`/`__CHARACTER_DATA__`/
//! `__STRING__`/`__INTEGER__`) `WriteExtended`'s caller masks with — the
//! same list the *reader*'s `LoadFromStreamExtended`
//! (`VariableData.cs:754-816`) filters its own dictionaries by. So
//! `GetExtSaveList` drives both sides of the built-in groups, not just
//! the reader's name recognition. Enumerating every global-scope (i.e.
//! not `__CHARACTER_DATA__`) `__SAVE_EXTENDED__` member in
//! `VariableCode.cs` gives exactly:
//!
//! | Built-in group | Members (declaration/enum order) |
//! |---|---|
//! | string scalar | *(none — no such member exists)* |
//! | int scalar | *(none)* |
//! | string 1D | `TSTR` (`0x06`, a real accessible script variable — see `VariableData.cs:200`, `VariableEvaluator.cs:1559`, not erars-invented) |
//! | int 1D | `RANDDATA` (`0x40`) |
//! | string 2D | *(none — `__COUNT_STRING_ARRAY_2D__ == 0`)* |
//! | int 2D | `DITEMTYPE`, `DA`, `DB`, `DC`, `DD`, `DE` (`0x00`-`0x05`) |
//! | string 3D | *(none — `__COUNT_STRING_ARRAY_3D__ == 0`)* |
//! | int 3D | `TA`, `TB` (`0x00`-`0x01`) |
//!
//! (Member order within a group matches `Enum.GetValues`' ascending raw
//! value, which for members sharing a shape's flag bits reduces to plain
//! declaration order — the low index byte is the only thing that
//! differs.) All 8 of these names are already implemented in this crate
//! as ordinary `is_savedata` variables (`erars-loader/src/variable.yaml`),
//! so an export can legitimately carry any of them and must route each to
//! its correct built-in group — not the user-defined pass — and must
//! exclude them from that user-defined pass so they are never written
//! twice. [`write_variable_section`] does exactly that:
//! [`BUILTIN_STR1D`]/[`BUILTIN_INT1D`]/[`BUILTIN_INT2D`]/[`BUILTIN_INT3D`]
//! give each built-in group's fixed membership and order; the four
//! groups with no member (string/int scalar, string 2D/3D) are always
//! empty for a structural reason (no such `VariableCode` exists), not an
//! observed-so-far one, so no fixture could ever falsify that half of the
//! claim short of a future Emuera version adding a new flagged member.
//!
//! **What would falsify the rest of this** — the four *non-empty* groups'
//! exact membership/order: a real capture whose built-in string-1D,
//! int-1D, int-2D, or int-3D group contains a key not in the table above,
//! or omits/reorders one of the ones listed while that variable holds
//! non-default content, or places a member in a different group entry
//! than its declared order predicts. Re-run
//! `round_trip_real_captures_are_byte_exact` against any newly captured
//! save before trusting this table for a build outside this crate's own
//! corpus.

use anyhow::{bail, ensure, Result};

use super::{
    EmueraSaveData, ExtendedMarker, IndexMap, ParsedArray, BINARY_MAGIC, CHAR_OLD_ARR,
    CHAR_OLD_INT, CHAR_OLD_STR, EMU_SEPARATOR, EMU_START, FINISHED, GLOBALSAVE_OLD_ARR,
    GLOBALSAVE_OLD_STR_ARR, GLOBAL_OLD_ARR, UTF8_BOM,
};

/// The Emuera version this crate ever *writes* the extended block as — real
/// Emuera 1.8xx itself always writes 1808 regardless of what it loaded
/// (spec §2.6); writing an older marker is out of scope (see the export
/// module's doc comment).
pub const EXPORT_VERSION: u32 = 1808;

// ---------------------------------------------------------------------
// Built-in extended-group membership (see the module doc comment's
// "Group placement" section for the source/fixture evidence)
// ---------------------------------------------------------------------

/// Built-in global-scope string-1D members, in enum order.
const BUILTIN_STR1D: &[&str] = &["TSTR"];
/// Built-in global-scope int-1D members, in enum order. `RANDDATA` is the
/// only one — this is the group the module doc comment's fixture-refuted
/// claim was about.
const BUILTIN_INT1D: &[&str] = &["RANDDATA"];
/// Built-in global-scope int-2D members, in enum order.
const BUILTIN_INT2D: &[&str] = &["DITEMTYPE", "DA", "DB", "DC", "DD", "DE"];
/// Built-in global-scope int-3D members, in enum order.
const BUILTIN_INT3D: &[&str] = &["TA", "TB"];

// ---------------------------------------------------------------------
// Shared little helpers
// ---------------------------------------------------------------------

/// One past the last non-zero element, or 0 if every element is zero —
/// `EraBinaryDataWriter.writeData(Int64[])`'s "don't store the trailing
/// zero-run, just stop" trim, reused by every int array shape.
fn trim_int(v: &[i64]) -> usize {
    v.iter().rposition(|&x| x != 0).map_or(0, |i| i + 1)
}

/// As [`trim_int`], for strings (`null || Length == 0` in the C# source).
fn trim_str(v: &[String]) -> usize {
    v.iter().rposition(|s| !s.is_empty()).map_or(0, |i| i + 1)
}

fn trim_int_rows(rows: &[Vec<i64>]) -> usize {
    rows.iter().rposition(|r| r.iter().any(|&v| v != 0)).map_or(0, |i| i + 1)
}

fn trim_str_rows(rows: &[Vec<String>]) -> usize {
    rows.iter().rposition(|r| r.iter().any(|s| !s.is_empty())).map_or(0, |i| i + 1)
}

// ---------------------------------------------------------------------
// Binary writer (spec §4) — `EraBinaryDataWriter`
// ---------------------------------------------------------------------

/// `BinaryWriter.Write(string)` under `Encoding.Unicode`: a 7-bit-varint
/// *byte*-length prefix, then that many UTF-16LE bytes.
fn write_bstr(out: &mut Vec<u8>, s: &str) {
    let units: Vec<u16> = s.encode_utf16().collect();
    let mut len = (units.len() * 2) as u32;
    loop {
        let mut b = (len & 0x7F) as u8;
        len >>= 7;
        if len != 0 {
            b |= 0x80;
            out.push(b);
        } else {
            out.push(b);
            break;
        }
    }
    for u in units {
        out.extend_from_slice(&u.to_le_bytes());
    }
}

/// `m_WriteInt`: `0x00..=0xCF` inline, else a widening tag + LE payload.
fn write_int_tag(out: &mut Vec<u8>, v: i64) {
    if (0..=0xCF).contains(&v) {
        out.push(v as u8);
    } else if (i16::MIN as i64..=i16::MAX as i64).contains(&v) {
        out.push(0xD0);
        out.extend_from_slice(&(v as i16).to_le_bytes());
    } else if (i32::MIN as i64..=i32::MAX as i64).contains(&v) {
        out.push(0xD1);
        out.extend_from_slice(&(v as i32).to_le_bytes());
    } else {
        out.push(0xD2);
        out.extend_from_slice(&v.to_le_bytes());
    }
}

/// `writeData(Int64[])`: length, then zero-run-compressed elements, EoD.
/// The trailing zero run (if any) is dropped rather than flushed — the
/// reader's zero-filled `vec![0; len]` reconstructs it from the length.
fn write_int1d(out: &mut Vec<u8>, arr: &[i64]) {
    out.extend_from_slice(&(arr.len() as i32).to_le_bytes());
    let mut count_zero: i64 = 0;
    for &v in arr {
        if v == 0 {
            count_zero += 1;
        } else {
            if count_zero > 0 {
                out.push(0xF0);
                write_int_tag(out, count_zero);
                count_zero = 0;
            }
            write_int_tag(out, v);
        }
    }
    out.push(0xFF);
}

/// `writeData(string[])`: as [`write_int1d`] with `0xD8`-tagged strings.
fn write_str1d(out: &mut Vec<u8>, arr: &[String]) {
    out.extend_from_slice(&(arr.len() as i32).to_le_bytes());
    let mut count_zero: i64 = 0;
    for v in arr {
        if v.is_empty() {
            count_zero += 1;
        } else {
            if count_zero > 0 {
                out.push(0xF0);
                write_int_tag(out, count_zero);
                count_zero = 0;
            }
            out.push(0xD8);
            write_bstr(out, v);
        }
    }
    out.push(0xFF);
}

/// `writeData(Int64[,])`: dims, then rows with `0xE0` row terminators and
/// `0xF1`-compressed runs of whole-zero rows, EoD.
fn write_int2d(out: &mut Vec<u8>, rows: &[Vec<i64>]) {
    let length0 = rows.len();
    let length1 = rows.first().map_or(0, |r| r.len());
    out.extend_from_slice(&(length0 as i32).to_le_bytes());
    out.extend_from_slice(&(length1 as i32).to_le_bytes());
    let mut count_zero: i64 = 0;
    let mut count_all_zero: i64 = 0;
    for row in rows {
        for &v in row {
            if v == 0 {
                count_zero += 1;
            } else {
                if count_all_zero > 0 {
                    out.push(0xF1);
                    write_int_tag(out, count_all_zero);
                    count_all_zero = 0;
                }
                if count_zero > 0 {
                    out.push(0xF0);
                    write_int_tag(out, count_zero);
                    count_zero = 0;
                }
                write_int_tag(out, v);
            }
        }
        if count_zero as usize == length1 {
            count_all_zero += 1;
        } else {
            out.push(0xE0);
        }
        count_zero = 0;
    }
    out.push(0xFF);
}

fn write_str2d(out: &mut Vec<u8>, rows: &[Vec<String>]) {
    let length0 = rows.len();
    let length1 = rows.first().map_or(0, |r| r.len());
    out.extend_from_slice(&(length0 as i32).to_le_bytes());
    out.extend_from_slice(&(length1 as i32).to_le_bytes());
    let mut count_zero: i64 = 0;
    let mut count_all_zero: i64 = 0;
    for row in rows {
        for v in row {
            if v.is_empty() {
                count_zero += 1;
            } else {
                if count_all_zero > 0 {
                    out.push(0xF1);
                    write_int_tag(out, count_all_zero);
                    count_all_zero = 0;
                }
                if count_zero > 0 {
                    out.push(0xF0);
                    write_int_tag(out, count_zero);
                    count_zero = 0;
                }
                out.push(0xD8);
                write_bstr(out, v);
            }
        }
        if count_zero as usize == length1 {
            count_all_zero += 1;
        } else {
            out.push(0xE0);
        }
        count_zero = 0;
    }
    out.push(0xFF);
}

/// `writeData(Int64[,,])`: dims, then planes/rows with `0xE1`/`0xE0`
/// terminators and `0xF2`/`0xF1`/`0xF0`-compressed all-zero runs, EoD.
/// `blocks` must be dense (index `i` holds plane `i`, `0..blocks.len()`) —
/// true both for a binary-parsed [`ParsedArray::Int3D`] and for a freshly
/// built export (see the module doc comment).
fn write_int3d(out: &mut Vec<u8>, blocks: &[(u32, Vec<Vec<i64>>)]) {
    let length0 = blocks.len();
    let length1 = blocks.first().map_or(0, |(_, rows)| rows.len());
    let length2 = blocks.first().and_then(|(_, rows)| rows.first()).map_or(0, |r| r.len());
    out.extend_from_slice(&(length0 as i32).to_le_bytes());
    out.extend_from_slice(&(length1 as i32).to_le_bytes());
    out.extend_from_slice(&(length2 as i32).to_le_bytes());
    let mut count_zero: i64 = 0;
    let mut count_all_zero: i64 = 0;
    let mut count_all_zero_2d: i64 = 0;
    for (_, rows) in blocks {
        for row in rows {
            for &v in row {
                if v == 0 {
                    count_zero += 1;
                } else {
                    if count_all_zero_2d > 0 {
                        out.push(0xF2);
                        write_int_tag(out, count_all_zero_2d);
                        count_all_zero_2d = 0;
                    }
                    if count_all_zero > 0 {
                        out.push(0xF1);
                        write_int_tag(out, count_all_zero);
                        count_all_zero = 0;
                    }
                    if count_zero > 0 {
                        out.push(0xF0);
                        write_int_tag(out, count_zero);
                        count_zero = 0;
                    }
                    write_int_tag(out, v);
                }
            }
            if count_zero as usize == length2 {
                count_all_zero += 1;
            } else {
                out.push(0xE0);
            }
            count_zero = 0;
        }
        if count_all_zero as usize == length1 {
            count_all_zero_2d += 1;
        } else {
            out.push(0xE1);
        }
        count_all_zero = 0;
    }
    out.push(0xFF);
}

fn write_str3d(out: &mut Vec<u8>, blocks: &[(u32, Vec<Vec<String>>)]) {
    let length0 = blocks.len();
    let length1 = blocks.first().map_or(0, |(_, rows)| rows.len());
    let length2 = blocks.first().and_then(|(_, rows)| rows.first()).map_or(0, |r| r.len());
    out.extend_from_slice(&(length0 as i32).to_le_bytes());
    out.extend_from_slice(&(length1 as i32).to_le_bytes());
    out.extend_from_slice(&(length2 as i32).to_le_bytes());
    let mut count_zero: i64 = 0;
    let mut count_all_zero: i64 = 0;
    let mut count_all_zero_2d: i64 = 0;
    for (_, rows) in blocks {
        for row in rows {
            for v in row {
                if v.is_empty() {
                    count_zero += 1;
                } else {
                    if count_all_zero_2d > 0 {
                        out.push(0xF2);
                        write_int_tag(out, count_all_zero_2d);
                        count_all_zero_2d = 0;
                    }
                    if count_all_zero > 0 {
                        out.push(0xF1);
                        write_int_tag(out, count_all_zero);
                        count_all_zero = 0;
                    }
                    if count_zero > 0 {
                        out.push(0xF0);
                        write_int_tag(out, count_zero);
                        count_zero = 0;
                    }
                    out.push(0xD8);
                    write_bstr(out, v);
                }
            }
            if count_zero as usize == length2 {
                count_all_zero += 1;
            } else {
                out.push(0xE0);
            }
            count_zero = 0;
        }
        if count_all_zero as usize == length1 {
            count_all_zero_2d += 1;
        } else {
            out.push(0xE1);
        }
        count_all_zero = 0;
    }
    out.push(0xFF);
}

/// One `WriteWithKey` record: type byte, key, payload.
fn write_binary_record(out: &mut Vec<u8>, key: &str, value: &ParsedArray) {
    let type_byte: u8 = match value {
        ParsedArray::IntScalar(_) => 0,
        ParsedArray::Int1D(_) => 1,
        ParsedArray::Int2D(_) => 2,
        ParsedArray::Int3D(_) => 3,
        ParsedArray::StrScalar(_) => 16,
        ParsedArray::Str1D(_) => 17,
        ParsedArray::Str2D(_) => 18,
        ParsedArray::Str3D(_) => 19,
    };
    out.push(type_byte);
    write_bstr(out, key);
    match value {
        ParsedArray::IntScalar(v) => write_int_tag(out, *v),
        ParsedArray::Int1D(v) => write_int1d(out, v),
        ParsedArray::Int2D(v) => write_int2d(out, v),
        ParsedArray::Int3D(v) => write_int3d(out, v),
        ParsedArray::StrScalar(v) => write_bstr(out, v),
        ParsedArray::Str1D(v) => write_str1d(out, v),
        ParsedArray::Str2D(v) => write_str2d(out, v),
        ParsedArray::Str3D(v) => write_str3d(out, v),
    }
}

fn write_binary_records(out: &mut Vec<u8>, vars: &IndexMap<String, ParsedArray>) {
    for (key, value) in vars {
        write_binary_record(out, key, value);
    }
}

/// Writes a binary Emuera save (spec §4): 16-byte header, `FileType`,
/// `code`, `version`, `saveText` (empty for global), — local only —
/// `characterCount` and that many `0xFE`-terminated character record
/// blocks, then one `0xFF`-terminated global record block.
pub fn write_binary(
    data: &EmueraSaveData,
    is_global: bool,
    code: u32,
    version: u32,
    description: &str,
) -> Vec<u8> {
    let mut out = Vec::new();
    out.extend_from_slice(&BINARY_MAGIC);
    out.extend_from_slice(&1808i32.to_le_bytes()); // Version1808
    out.extend_from_slice(&0i32.to_le_bytes()); // DataCount (0 -> no trailing reserved words)
    out.push(if is_global { 1 } else { 0 });
    out.extend_from_slice(&(code as i64).to_le_bytes());
    out.extend_from_slice(&(version as i64).to_le_bytes());
    write_bstr(&mut out, description);
    if !is_global {
        out.extend_from_slice(&(data.charas.len() as i64).to_le_bytes());
        for chara in &data.charas {
            write_binary_records(&mut out, chara);
            out.push(0xFE); // RecordEnd::Eoc
        }
    }
    write_binary_records(&mut out, &data.globals);
    out.push(0xFF); // RecordEnd::Eof
    out
}

// ---------------------------------------------------------------------
// Text writer (spec §2) — `EraDataWriter`
// ---------------------------------------------------------------------

/// Which text encoding to emit — real Emuera's own `SystemSaveInUTF8`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextEncodingChoice {
    /// `SystemSaveInUTF8:NO` (default): the game's configured non-Unicode
    /// encoding (SJIS/EUC-KR/GBK/Big5), no BOM.
    NonUnicode,
    /// `SystemSaveInUTF8:YES`: UTF-8 with a leading BOM.
    Utf8,
}

fn marker_line(version: u32) -> Result<&'static str> {
    Ok(match version {
        1700 => "__EMUERA_STRAT__",
        1708 => "__EMUERA_1708_STRAT__",
        1729 => "__EMUERA_1729_STRAT__",
        1803 => "__EMUERA_1803_STRAT__",
        1808 => EMU_START,
        other => bail!("모르는 확장 블록 버전입니다: {other}"),
    })
}

fn write_old_int_array(lines: &mut Vec<String>, arr: &[i64]) {
    let n = trim_int(arr);
    for &v in &arr[..n] {
        lines.push(v.to_string());
    }
    lines.push(FINISHED.to_owned());
}

fn write_old_str_array(lines: &mut Vec<String>, arr: &[String]) {
    let n = trim_str(arr);
    for v in &arr[..n] {
        lines.push(v.clone());
    }
    lines.push(FINISHED.to_owned());
}

fn old_int1d<'a>(vars: &'a IndexMap<String, ParsedArray>, name: &str) -> &'a [i64] {
    match vars.get(name) {
        Some(ParsedArray::Int1D(v)) => v,
        _ => &[],
    }
}

fn old_str_scalar<'a>(vars: &'a IndexMap<String, ParsedArray>, name: &str) -> &'a str {
    match vars.get(name) {
        Some(ParsedArray::StrScalar(v)) => v,
        _ => "",
    }
}

fn old_int_scalar(vars: &IndexMap<String, ParsedArray>, name: &str) -> i64 {
    match vars.get(name) {
        Some(ParsedArray::IntScalar(v)) => *v,
        _ => 0,
    }
}

fn old_str1d<'a>(vars: &'a IndexMap<String, ParsedArray>, name: &str) -> &'a [String] {
    match vars.get(name) {
        Some(ParsedArray::Str1D(v)) => v,
        _ => &[],
    }
}

/// One character's OLD block: 2 string scalars, 2 int scalars, 17 named
/// int arrays, all positional (mirrors [`super::parse_old_chara_block`]).
fn write_old_chara_block(lines: &mut Vec<String>, chara: &IndexMap<String, ParsedArray>) {
    for name in CHAR_OLD_STR {
        lines.push(old_str_scalar(chara, name).to_owned());
    }
    for name in CHAR_OLD_INT {
        lines.push(old_int_scalar(chara, name).to_string());
    }
    for name in CHAR_OLD_ARR {
        write_old_int_array(lines, old_int1d(chara, name));
    }
}

/// The OLD block's global section embedded inside a local save: 60 named
/// int arrays then `SAVESTR` (mirrors [`super::parse_old_variable_block`]).
fn write_old_variable_block(lines: &mut Vec<String>, globals: &IndexMap<String, ParsedArray>) {
    for name in GLOBAL_OLD_ARR {
        write_old_int_array(lines, old_int1d(globals, name));
    }
    write_old_str_array(lines, old_str1d(globals, "SAVESTR"));
}

/// The OLD block for a standalone `global.sav`: `GLOBAL` then `GLOBALS`,
/// positional (mirrors [`super::parse_old_global_save_block`]).
fn write_old_global_save_block(lines: &mut Vec<String>, globals: &IndexMap<String, ParsedArray>) {
    write_old_int_array(lines, old_int1d(globals, GLOBALSAVE_OLD_ARR));
    write_old_str_array(lines, old_str1d(globals, GLOBALSAVE_OLD_STR_ARR));
}

fn write_int1d_text(lines: &mut Vec<String>, key: &str, arr: &[i64]) {
    let n = trim_int(arr);
    if n == 0 {
        return;
    }
    lines.push(key.to_owned());
    for &v in &arr[..n] {
        lines.push(v.to_string());
    }
    lines.push(FINISHED.to_owned());
}

fn write_str1d_text(lines: &mut Vec<String>, key: &str, arr: &[String]) {
    let n = trim_str(arr);
    if n == 0 {
        return;
    }
    lines.push(key.to_owned());
    for v in &arr[..n] {
        lines.push(v.clone());
    }
    lines.push(FINISHED.to_owned());
}

fn write_int2d_text(lines: &mut Vec<String>, key: &str, rows: &[Vec<i64>]) {
    let count_x = trim_int_rows(rows);
    if count_x == 0 {
        return;
    }
    lines.push(key.to_owned());
    for row in &rows[..count_x] {
        let n = trim_int(row);
        let joined: Vec<String> = row[..n].iter().map(i64::to_string).collect();
        lines.push(joined.join(","));
    }
    lines.push(FINISHED.to_owned());
}

/// Real Emuera's own text writer cannot represent a string 2D array at all
/// (`WriteExtended(string, string[,])` throws `NotImplementedException`,
/// spec §2.5) — a non-empty one fails loudly instead of being dropped.
fn check_no_str2d(key: &str, rows: &[Vec<String>]) -> Result<()> {
    ensure!(
        trim_str_rows(rows) == 0,
        "변수 '{key}'는 문자열 2차원 배열이라 텍스트 형식 세이브로는 내보낼 수 없습니다 (바이너리 형식을 사용하세요)"
    );
    Ok(())
}

fn write_int3d_text(lines: &mut Vec<String>, key: &str, blocks: &[(u32, Vec<Vec<i64>>)]) {
    let count_x =
        blocks.iter().rposition(|(_, rows)| trim_int_rows(rows) != 0).map_or(0, |i| i + 1);
    if count_x == 0 {
        return;
    }
    lines.push(key.to_owned());
    for (i, (_, rows)) in blocks[..count_x].iter().enumerate() {
        lines.push(format!("{i}{{"));
        let count_y = trim_int_rows(rows);
        for row in &rows[..count_y] {
            let n = trim_int(row);
            let joined: Vec<String> = row[..n].iter().map(i64::to_string).collect();
            lines.push(joined.join(","));
        }
        lines.push("}".to_owned());
    }
    lines.push(FINISHED.to_owned());
}

/// As [`check_no_str2d`], for string 3D arrays.
fn check_no_str3d(key: &str, blocks: &[(u32, Vec<Vec<String>>)]) -> Result<()> {
    let has_any = blocks.iter().any(|(_, rows)| trim_str_rows(rows) != 0);
    ensure!(
        !has_any,
        "변수 '{key}'는 문자열 3차원 배열이라 텍스트 형식 세이브로는 내보낼 수 없습니다 (바이너리 형식을 사용하세요)"
    );
    Ok(())
}

/// Every entry of `map` not named in `exclude`, matching `(want_str,
/// want_dims)`, in the map's own (insertion) order.
fn extended_names<'a>(
    map: &'a IndexMap<String, ParsedArray>,
    exclude: &'a [&'a str],
    want_str: bool,
    want_dims: usize,
) -> impl Iterator<Item = (&'a str, &'a ParsedArray)> {
    map.iter()
        .filter(move |(name, v)| {
            v.is_str() == want_str && v.dim_count() == want_dims && !exclude.contains(&name.as_str())
        })
        .map(|(k, v)| (k.as_str(), v))
}

/// As [`extended_names`], but for a built-in group: `names` gives the
/// group's *fixed*, source-derived membership and order (see the module
/// doc comment) instead of the map's own insertion order, and there is no
/// `exclude` — a built-in name is never itself excluded from its own
/// group. Only names actually present in `map` (and of matching shape)
/// are yielded; erars implements every one of these as an ordinary
/// `is_savedata` variable, but a hand-built or foreign-parsed map need
/// not declare all of them.
fn builtin_names<'a>(
    map: &'a IndexMap<String, ParsedArray>,
    names: &'static [&'static str],
    want_str: bool,
    want_dims: usize,
) -> impl Iterator<Item = (&'a str, &'a ParsedArray)> {
    names.iter().filter_map(move |&name| {
        map.get(name)
            .filter(|v| v.is_str() == want_str && v.dim_count() == want_dims)
            .map(|v| (name, v))
    })
}

fn write_str_scalar_group<'a>(lines: &mut Vec<String>, names: impl Iterator<Item = (&'a str, &'a ParsedArray)>) {
    for (name, v) in names {
        if let ParsedArray::StrScalar(s) = v {
            if !s.is_empty() {
                lines.push(format!("{name}:{s}"));
            }
        }
    }
}

fn write_int_scalar_group<'a>(lines: &mut Vec<String>, names: impl Iterator<Item = (&'a str, &'a ParsedArray)>) {
    for (name, v) in names {
        if let ParsedArray::IntScalar(n) = v {
            if *n != 0 {
                lines.push(format!("{name}:{n}"));
            }
        }
    }
}

fn write_str1d_group<'a>(lines: &mut Vec<String>, names: impl Iterator<Item = (&'a str, &'a ParsedArray)>) {
    for (name, v) in names {
        if let ParsedArray::Str1D(arr) = v {
            write_str1d_text(lines, name, arr);
        }
    }
}

fn write_int1d_group<'a>(lines: &mut Vec<String>, names: impl Iterator<Item = (&'a str, &'a ParsedArray)>) {
    for (name, v) in names {
        if let ParsedArray::Int1D(arr) = v {
            write_int1d_text(lines, name, arr);
        }
    }
}

fn check_str2d_group<'a>(names: impl Iterator<Item = (&'a str, &'a ParsedArray)>) -> Result<()> {
    for (name, v) in names {
        if let ParsedArray::Str2D(rows) = v {
            check_no_str2d(name, rows)?;
        }
    }
    Ok(())
}

fn write_int2d_group<'a>(lines: &mut Vec<String>, names: impl Iterator<Item = (&'a str, &'a ParsedArray)>) {
    for (name, v) in names {
        if let ParsedArray::Int2D(rows) = v {
            write_int2d_text(lines, name, rows);
        }
    }
}

fn check_str3d_group<'a>(names: impl Iterator<Item = (&'a str, &'a ParsedArray)>) -> Result<()> {
    for (name, v) in names {
        if let ParsedArray::Str3D(blocks) = v {
            check_no_str3d(name, blocks)?;
        }
    }
    Ok(())
}

fn write_int3d_group<'a>(lines: &mut Vec<String>, names: impl Iterator<Item = (&'a str, &'a ParsedArray)>) {
    for (name, v) in names {
        if let ParsedArray::Int3D(blocks) = v {
            write_int3d_text(lines, name, blocks);
        }
    }
}

/// One character's extended section (spec §2.5): 6 groups if `version >=
/// 1803`, else 4 — mirrors [`super::parse_chara_section`] exactly. Every
/// non-OLD chara-scope name (built-in-extended or user `#DIM SAVEDATA`
/// alike — see the module doc comment) is bucketed by shape alone.
fn write_chara_section(lines: &mut Vec<String>, chara: &IndexMap<String, ParsedArray>, version: u32) -> Result<()> {
    let exclude: Vec<&str> =
        CHAR_OLD_STR.iter().chain(CHAR_OLD_INT.iter()).chain(CHAR_OLD_ARR.iter()).copied().collect();

    write_str_scalar_group(lines, extended_names(chara, &exclude, true, 0));
    lines.push(EMU_SEPARATOR.to_owned());
    write_int_scalar_group(lines, extended_names(chara, &exclude, false, 0));
    lines.push(EMU_SEPARATOR.to_owned());
    write_str1d_group(lines, extended_names(chara, &exclude, true, 1));
    lines.push(EMU_SEPARATOR.to_owned());
    write_int1d_group(lines, extended_names(chara, &exclude, false, 1));
    lines.push(EMU_SEPARATOR.to_owned());
    if version >= 1803 {
        check_str2d_group(extended_names(chara, &exclude, true, 2))?;
        lines.push(EMU_SEPARATOR.to_owned());
        write_int2d_group(lines, extended_names(chara, &exclude, false, 2));
        lines.push(EMU_SEPARATOR.to_owned());
    }
    Ok(())
}

/// The local save's own (non-chara) variable section (spec §2.5): 8
/// built-in groups (string/int scalar — always empty, no such member
/// exists; string 1D — `TSTR`; int 1D — `RANDDATA`; string 2D — always
/// empty; int 2D — `DITEMTYPE`/`DA`-`DE`; string 3D — always empty; int
/// 3D — `TA`/`TB`; see the module doc comment's "Group placement"
/// section), then, `version >= 1808` only, 6 user-defined groups holding
/// every other non-OLD global-scope name. Mirrors
/// [`super::parse_variable_section`] exactly.
fn write_variable_section(lines: &mut Vec<String>, globals: &IndexMap<String, ParsedArray>, version: u32) -> Result<()> {
    let mut exclude: Vec<&str> = GLOBAL_OLD_ARR.to_vec();
    exclude.push("SAVESTR");
    exclude.extend(BUILTIN_STR1D.iter().chain(BUILTIN_INT1D).chain(BUILTIN_INT2D).chain(BUILTIN_INT3D));

    // Built-in group 1 (string scalar) and 2 (int scalar): no
    // `VariableCode` member with either shape is ever flagged
    // `__SAVE_EXTENDED__` at global scope — structurally, not just
    // empirically, empty.
    lines.push(EMU_SEPARATOR.to_owned());
    lines.push(EMU_SEPARATOR.to_owned());
    // Built-in group 3 (string 1D): `TSTR`.
    write_str1d_group(lines, builtin_names(globals, BUILTIN_STR1D, true, 1));
    lines.push(EMU_SEPARATOR.to_owned());
    // Built-in group 4 (int 1D): `RANDDATA`.
    write_int1d_group(lines, builtin_names(globals, BUILTIN_INT1D, false, 1));
    lines.push(EMU_SEPARATOR.to_owned());
    if version >= 1708 {
        // Built-in group 5 (string 2D): structurally empty, as above.
        lines.push(EMU_SEPARATOR.to_owned());
        // Built-in group 6 (int 2D): `DITEMTYPE`, `DA`-`DE`.
        write_int2d_group(lines, builtin_names(globals, BUILTIN_INT2D, false, 2));
        lines.push(EMU_SEPARATOR.to_owned());
    }
    if version >= 1729 {
        // Built-in group 7 (string 3D): structurally empty, as above.
        lines.push(EMU_SEPARATOR.to_owned());
        // Built-in group 8 (int 3D): `TA`, `TB`.
        write_int3d_group(lines, builtin_names(globals, BUILTIN_INT3D, false, 3));
        lines.push(EMU_SEPARATOR.to_owned());
    }

    if version >= 1808 {
        write_str1d_group(lines, extended_names(globals, &exclude, true, 1));
        lines.push(EMU_SEPARATOR.to_owned());
        write_int1d_group(lines, extended_names(globals, &exclude, false, 1));
        lines.push(EMU_SEPARATOR.to_owned());
        check_str2d_group(extended_names(globals, &exclude, true, 2))?;
        lines.push(EMU_SEPARATOR.to_owned());
        write_int2d_group(lines, extended_names(globals, &exclude, false, 2));
        lines.push(EMU_SEPARATOR.to_owned());
        check_str3d_group(extended_names(globals, &exclude, true, 3))?;
        lines.push(EMU_SEPARATOR.to_owned());
        write_int3d_group(lines, extended_names(globals, &exclude, false, 3));
        lines.push(EMU_SEPARATOR.to_owned());
    } else {
        // No user-group pass exists before 1808 at all: any such name here
        // is real Emuera-unrepresentable at this marker version too (the
        // format itself predates `#DIM SAVEDATA` array support), not data
        // we are silently dropping — see the module doc comment.
    }
    Ok(())
}

/// A standalone `global.sav`'s own variable section (spec §2.5): 6 groups,
/// no scalars — str1D, int1D, str2D, int2D, str3D, int3D — each holding
/// every non-OLD global-scope name of that shape. Mirrors
/// [`super::parse_global_variable_section`] exactly.
fn write_global_variable_section(lines: &mut Vec<String>, globals: &IndexMap<String, ParsedArray>, version: u32) -> Result<()> {
    let exclude: Vec<&str> = vec![GLOBALSAVE_OLD_ARR, GLOBALSAVE_OLD_STR_ARR];

    write_str1d_group(lines, extended_names(globals, &exclude, true, 1));
    lines.push(EMU_SEPARATOR.to_owned());
    write_int1d_group(lines, extended_names(globals, &exclude, false, 1));
    lines.push(EMU_SEPARATOR.to_owned());
    if version >= 1708 {
        check_str2d_group(extended_names(globals, &exclude, true, 2))?;
        lines.push(EMU_SEPARATOR.to_owned());
        write_int2d_group(lines, extended_names(globals, &exclude, false, 2));
        lines.push(EMU_SEPARATOR.to_owned());
    }
    if version >= 1729 {
        check_str3d_group(extended_names(globals, &exclude, true, 3))?;
        lines.push(EMU_SEPARATOR.to_owned());
        write_int3d_group(lines, extended_names(globals, &exclude, false, 3));
        lines.push(EMU_SEPARATOR.to_owned());
    }
    Ok(())
}

/// Writes a text Emuera save (spec §2): `code`, `version`, — local only —
/// `saveText`/`characterCount`, the OLD block, then (if the marker isn't
/// [`ExtendedMarker::Absent`]) the marker line and the extended block.
/// Every line is joined with `\r\n` (`StreamWriter.WriteLine`'s default),
/// including after the very last line. `encoding_choice` picks the BOM/
/// encoding; `encoding` is only consulted for [`TextEncodingChoice::NonUnicode`].
pub fn write_text(
    data: &EmueraSaveData,
    is_global: bool,
    code: u32,
    version: u32,
    description: &str,
    encoding_choice: TextEncodingChoice,
    encoding: &'static encoding_rs::Encoding,
) -> Result<Vec<u8>> {
    let mut lines: Vec<String> = Vec::new();
    lines.push(code.to_string());
    lines.push(version.to_string());
    if !is_global {
        lines.push(description.to_owned());
        lines.push(data.charas.len().to_string());
    }

    if is_global {
        write_old_global_save_block(&mut lines, &data.globals);
    } else {
        for chara in &data.charas {
            write_old_chara_block(&mut lines, chara);
        }
        write_old_variable_block(&mut lines, &data.globals);
    }

    match &data.extended_marker {
        ExtendedMarker::Absent => {}
        ExtendedMarker::Unknown(marker) => {
            // This reader doesn't implement this marker's grammar either
            // (see `ExtendedMarker::Unknown`'s doc comment) — nothing past
            // the marker line itself can be faithfully reproduced.
            lines.push(marker.clone());
        }
        ExtendedMarker::Known(ver) => {
            let ver = *ver;
            lines.push(marker_line(ver)?.to_owned());
            if is_global {
                write_global_variable_section(&mut lines, &data.globals, ver)?;
            } else {
                for chara in &data.charas {
                    write_chara_section(&mut lines, chara, ver)?;
                }
                write_variable_section(&mut lines, &data.globals, ver)?;
            }
        }
    }

    let mut text = lines.join("\r\n");
    text.push_str("\r\n");

    Ok(match encoding_choice {
        TextEncodingChoice::Utf8 => {
            let mut out = UTF8_BOM.to_vec();
            out.extend_from_slice(text.as_bytes());
            out
        }
        TextEncodingChoice::NonUnicode => {
            let (encoded, _, had_errors) = encoding.encode(&text);
            ensure!(
                !had_errors,
                "선택한 인코딩({})으로 표현할 수 없는 문자가 있습니다",
                encoding.name()
            );
            encoded.into_owned()
        }
    })
}
