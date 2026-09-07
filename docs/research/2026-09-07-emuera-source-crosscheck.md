# Emuera Save Format — Cross-Check Against Real C# Source

**Date:** 2026-09-07
**Purpose:** replace IL-disassembly inference in
`docs/research/2026-09-06-emuera-save-format.md` with the actual Emuera C#
source, confirm (or refute) every structural claim, and resolve the three
items the previous session could not verify. No reader bugs that change
behaviour were found — the positional OLD-block layouts and the binary
reader's marker handling both match the source exactly; §4 of this doc
records the corrections that *do* need to land (all doc-level).

---

## 1. Source obtained

| Item | Value |
|---|---|
| Repo | `https://github.com/xerysherry/uEmuera` (uEmuera, a Unity/C# port of Emuera) |
| Commit | `cb66a45fd44d910b15ebd288b96b9de3853d3fdf` (2023-06-23), shallow clone |
| Emuera version | **1824v15** — NOT 1.818 |
| Original C# | Ships the full upstream `MinorShift.Emuera` source unchanged in `Assets/Scripts/Emuera/`, including the exact classes used for saves: `EraDataStream.cs` (`EraDataReader`/`EraDataWriter`), `EraBinaryDataReader.cs`, `EraBinaryDataWriter.cs`, `GameData/Variable/{VariableCode,VariableData,CharacterData,VariableEvaluator}.cs`. |

**Version caveat.** uEmuera is based on Emuera **1824**, our captures and
IL are Emuera **1.818**. Everything that matters for the on-disk format is
stable between 1.818 and 1824 — the `VariableCode` enum is contract-frozen
(its own `__SAVE_EXTENDED__` comment warns "renaming breaks loading"), and
the positional OLD-block slots are the classic eramaker-era ones unchanged
since forever. Our 1.818-derived constants (from IL + real captures) match
the 1824 source name-for-name and count-for-count, which is the strong
evidence the OLD-block layouts are version-independent. Two *writer-side*
differences between 1.818 and 1824 are called out in §6 — both are
correctly handled by our reader either way.

Citations below are `path:line` into `/tmp/uemuera` at the commit above.

---

## 2. Positional OLD-block layouts — CONFIRMED, every name and count

The reader constants (`crates/erars-vm/src/save/emuera.rs`) were derived
positionally from the IL `VariableCode` enum. The C# enum is plain text and
confirms every one of them, in order.

### 2.1 `CHAR_OLD_STR` = `[NAME, CALLNAME]`, count 2 — CONFIRMED
`VariableCode.cs:236-238`: `NAME = 0x00`, `CALLNAME = 0x01`,
`__COUNT_SAVE_CHARACTER_STRING__ = 0x02`. `NICKNAME`/`MASTERNAME`
(indices 2/3) carry `__SAVE_EXTENDED__` (`VariableCode.cs:237`) so they
never appear in the OLD block. `CharacterData.SaveToStream`/`LoadFromStream`
loop exactly `__COUNT_SAVE_CHARACTER_STRING__` strings first
(`CharacterData.cs:283,289-312`).

### 2.2 `CHAR_OLD_INT` = `[ISASSI, NO]`, count 2 — CONFIRMED
`VariableCode.cs:194-198`: `ISASSI = 0x00`, `NO = 0x01`,
`__COUNT_SAVE_CHARACTER_INTEGER__ = 0x02`. Second loop in
`CharacterData.SaveToStream` (`CharacterData.cs:284,289-312`).

### 2.3 `CHAR_OLD_ARR` = `BASE`..`NOWEX`, count 17 — CONFIRMED
`VariableCode.cs:200-232`: `BASE`(0x00) … `NOWEX`(0x10),
`__COUNT_SAVE_CHARACTER_INTEGER_ARRAY__ = 0x11` (= 17). `DOWNBASE`(0x11),
`CUP`(0x12), `CDOWN`(0x13), `TCVAR`(0x14) all carry `__SAVE_EXTENDED__ |
__EXTENDED__` (`VariableCode.cs:228-231`), so they are correctly excluded
from the OLD block and appear only in the extended chara section. Zero
string arrays (`__COUNT_SAVE_CHARACTER_STRING_ARRAY__ = 0x00`,
`VariableCode.cs:243`). Third/fourth loops in `CharacterData.SaveToStream`
(`CharacterData.cs:285-286`).

### 2.4 `GLOBAL_OLD_ARR` = `DAY`..`NOTUSE_3B`, count 60 — CONFIRMED
`VariableCode.cs:68-131`: `DAY`(0x00) … `Z`(0x37), `NOTUSE_38`(0x38),
`NOTUSE_39`(0x39), `NOTUSE_3A`(0x3A), `NOTUSE_3B`(0x3B),
`__COUNT_SAVE_INTEGER_ARRAY__ = 0x3C` (= 60). The 1824 enum's first
non-OLD array codes (`ITEMPRICE`=0x3C, `LOCAL`, `ARG`, `GLOBAL`=0x3F,
`RANDDATA`=0x40, `__COUNT_INTEGER_ARRAY__=0x41`, `VariableCode.cs:112-120`)
are all `__EXTENDED__`, confirming the OLD block stops at `NOTUSE_3B`.
`dataIntegerArray[0..60)` is the loop in `VariableData.SaveToStream`/
`LoadFromStream` (`VariableData.cs:663-688`). The local-embedded OLD global
block's string array is exactly `SAVESTR`
(`__COUNT_SAVE_STRING_ARRAY__ = 0x01`, `VariableCode.cs:133-137`).

### 2.5 `GLOBALSAVE_OLD_ARR`/`GLOBALSAVE_OLD_STR_ARR` = `GLOBAL` + `GLOBALS` — CONFIRMED
The standalone `global.sav` OLD block is a dedicated, separate
writer/reader, exactly as the doc claims: `VariableData.SaveGlobalToStream`
writes `dataIntegerArray[63]` (`GLOBAL`) then `dataStringArray[5]`
(`GLOBALS`) (`VariableData.cs:904-914`). `GLOBAL = 0x3F |
__INTEGER__ | __ARRAY_1D__ | __GLOBAL__ | __EXTENDED__`
(`VariableCode.cs:118`); `GLOBALS = 0x05 | __STRING__ | __ARRAY_1D__ |
__GLOBAL__ | __EXTENDED__` (`VariableCode.cs:143`). Index 63 = `0x3F`
(Day starts at 0), index 5 = `GLOBALS`'s string-array ordinal
(`SAVESTR`=0, `STR`=1, `RESULTS`=2, `LOCALS`=3, `ARGS`=4, `GLOBALS`=5).

### 2.6 Vector of the whole flow — CONFIRMED
`VariableEvaluator.SaveToStream` (`VariableEvaluator.cs:2273-2291`) writes
code, version, saveText, charCount, then `CharacterData` OLD × N, `VariableData`
OLD, `EmuStart()`, then extended charas + vars. `LoadFromStream`
(`VariableEvaluator.cs:2292-2325`) reads OLD blocks **first, unconditionally**,
then `SeekEmuStart()`, then the extended block only if found — matching the
doc's corrected §7 and the reader's `parse_text`. `SaveGlobal`
(`VariableEvaluator.cs:2327-2370`) writes code, version, `SaveGlobalToStream`,
`EmuStart()`, `SaveGlobalToStream1808`; `LoadGlobal`
(`VariableEvaluator.cs:2383-2430`) reads them in the same order. The local
extended variable section is 8 built-in groups + 6 user-defined groups
(`VariableData.SaveToStreamExtended`, `VariableData.cs:689-762`); the global
extended block is exactly the 6 user-defined groups, no scalars
(`VariableData.SaveGlobalToStream1808`, `VariableData.cs:916-936`). All match.

### 2.7 Extended-section flag values — CONFIRMED
Doc §2.5's flags come straight from `VariableCode.cs:18-31`:
`__INTEGER__=0x20000`, `__STRING__=0x40000`, `__ARRAY_1D__=0x80000`,
`__CHARACTER_DATA__=0x100000`, `__ARRAY_2D__=0x8000000`,
`__ARRAY_3D__=0x20000000`. `GetExtSaveList` (`VariableIdentifier.cs:264-271`)
returns exactly the `__SAVE_EXTENDED__`-flagged codes grouped by
(dim | character | int/str) — confirmed by how `extSaveListDic` is filled
(`VariableIdentifier.cs:254-261`).

---

## 3. Resolution 1 — Binary 2D/3D marker semantics

The previously-unverified markers are defined in `EraBinaryDataReader.cs`
`Ebdb` (lines 35-46):

| Marker | Value | Meaning (reader) | Used in |
|---|---|---|---|
| `Byte` | `0xCF` | single-byte non-negative int (0–207) | all int payloads |
| `Int16`/`Int32`/`Int64` | `0xD0`/`0xD1`/`0xD2` | next 2/4/8 bytes LE | all int payloads |
| `String` | `0xD8` | a string element follows (`ReadString`) | string arrays |
| `EoA1` | `0xE0` | **row terminator**: zero-fill rest of the current row, advance to next | 2D; the innermost level of 3D |
| `EoA2` | `0xE1` | **matrix terminator**: zero-fill the rest of the current 2D plane, advance to the next plane | 3D |
| `Zero` | `0xF0` | next `m_ReadInt()` = count of consecutive zero cells within the current row | 1D/2D/3D |
| `ZeroA1` | `0xF1` | next `m_ReadInt()` = count of consecutive all-zero **rows** | 2D; inner level of 3D |
| `ZeroA2` | `0xF2` | next `m_ReadInt()` = count of consecutive all-zero **matrices/planes** | 3D |
| `EoD` | `0xFF` | end of this array's data | all arrays |

The names echo nesting depth: `EoA1`/`ZeroA1` close/compress the *row*
(level 1) dimension; `EoA2`/`ZeroA2` close/compress the *matrix/plane*
(level 2) dimension. The exact read loop is in
`EraBinaryDataReader1808::ReadIntArray2D` (`EraBinaryDataReader.cs:278-370`)
and `ReadIntArray3D` (`:372-495`); the string variants mirror them
(`ReadStrArray2D` `:599-649`, `ReadStrArray3D` `:651-730`). The writer side
(`EraBinaryDataWriter.writeData(Int64[,])` `:167-215`, `(Int64[,,])`
`:217-272`) emits exactly these, so the in-memory reconstruction is
symmetric.

**Verdict: our `BinCursor` (`emuera.rs:730-1058`) already implements every
one of these identically to the C# reader** — `read_int2d`/`read_str2d`
map `0xF1`→row-advance-by-count, `0xE0`→row-advance-by-one, `0xF0`→cell
zero-run; `read_int3d`/`read_str3d` add `0xF2`→plane-advance-by-count and
`0xE1`→plane-advance-by-one, with `0xF1`/`0xE0` correctly scoped to the
plane's rows. No reader change needed. The former doc §6.2 caution ("loader
2D/3D *reading* should be treated with mild caution") is hereby retired —
verified against both directions of the source and, for `0xF0` runs and the
`0xD0/0xD1/0xD2/D8` tags, against the real capture (below).

**Capture cross-check (1.818, `save90_binary_real.sav`):** decoding the
whole file with the C# semantics reaches EOF at exactly byte 2317
(file length). It exercises `Zero`(`0xF0`) compression, the int
tag range, and `D8` string tags — but, as the doc's §6.2 accurately
predicted, the 2D/3D *structural* markers `0xE0/0xE1/0xF1/0xF2` never
appear in any of the six real captures (every 2D/3D array's rows/planes
were either concrete or reached the array end without needing an explicit
empty-run). The two tests added in this change (`emuera.rs`) feed exact
C#-writer byte streams through `read_int2d`/`read_int3d` to lock in the
structural-marker path that no capture exercised.

---

## 4. Correction 1 — §4.3 "trailing zero-run is still emitted" is WRONG

Doc §4.3 (1D int arrays) claims: *"If the tail is all zeros, the trailing
zero-run is still emitted (`0xF0 count`) — the length field carries the true
extent. (Verified: real `BASE` array = len `64` + ... values + zero-run +
`0xFF`.)"*

The C# writer does **not** do that. `EraBinaryDataWriter.writeData(Int64[])`
(`:140-165`) accumulates a trailing run of zeros and, seeing no further
non-zero element, simply writes `EoD` without flushing it — the run is
dropped and the reader's zero-fill (up to the length field) reconstructs
it. The comment at `:163` says exactly this: *"記憶途中で配列の残りが全部0
であるなら0の数も記憶せず配列の終わりを記憶"* ("if the rest of the array is all
zero, don't store the zero count, just store the end-of-array"). The 1.818
capture agrees: the real `BASE` record is `len=100`, values
`2500 2000 10000`, then `0xFF` — **no trailing zero-run**, and the length
is 100, not 64. So the doc's sentence and its "Verified" example are both
wrong.

`0xF0` runs *do* appear mid-array (26 in the capture), always followed by
more non-zero data. Our reader handles both the run and the dropped-tail
forms identically (it breaks at `0xFF` and the `vec![0; len]` init fills the
tail), so **no reader change** — this is a doc fix (see §7).

---

## 5. Resolution 2 — Old extended-block marker variants

`EraDataReader.SeekEmuStart` (`EraDataStream.cs:129-165`) accepts **all
five** historical markers and records the version:

```csharp
__EMUERA_STRAT__       -> emu_version = 1700
__EMUERA_1708_STRAT__  -> 1708
__EMUERA_1729_STRAT__  -> 1729
__EMUERA_1803_STRAT__  -> 1803
__EMUERA_1808_STRAT__  -> 1808
```

(`constants EraDataStream.cs:46-50`.) So a 1.808-era reader *does* accept
every older marker. The reader then dispatches the extended-block grammar on
`reader.DataVersion`:

- `VariableEvaluator.LoadFromStream` (`VariableEvaluator.cs:2315-2324`):
  `DataVersion < 1803` → `CharacterData.LoadFromStreamExtended_Old1802` (a
  reduced 4-group reader with no 2D, `CharacterData.cs:432-474`); else the
  full 6-group reader.
- `VariableData.LoadFromStreamExtended` (`VariableData.cs:799-801`):
  `version < 1808` → returns before the user-defined-var pass (they did not
  exist before 1808).

Emuera's *writer* only ever emits `__EMUERA_1808_STRAT__`
(`EraDataWriter.EMU_START = EraDataReader.EMU_1808_START`,
`EraDataStream.cs:480`; `EmuStart()` `:569-574`).

**Verdict (updated — old markers now implemented).** erars now reads every
marker's extended block with the per-version grammar the C# dispatch above
describes, so a `1700/1708/1729/1803` save's extended-only variables are
imported, not dropped (`emuera.rs`):
- chara extended: `parse_chara_section` keeps 4 groups (the `Old1802`
  reduced reader) for `version < 1803` and adds the 2D group for
  `>= 1803`.
- variable extended: 2D iff `>= 1708`, 3D iff `>= 1729` (the read-side
  gates `EraDataStream.cs:297,344,366,424`), then the six user-defined-var
  groups iff `>= 1808` (`VariableData.cs:799-801`). The OLD block remains
  version-independent and is parsed first, unchanged.

This supersedes the earlier "refusal" verdict. Per the user's instruction,
a marker this reader does not recognize (`Unknown`, a future version) and
an absent marker (`Absent`) are still reported and still skipped — an
unknown/future grammar may not be silently assumed to parse.

**Evidence class (updated 2026-09-07 — old markers now capture-backed).**
1808 is backed by the six 1.818 captures; the **1700 / 1708 / 1729 / 1803
markers are now capture-backed too.** Real saves were produced by actually
running the original mainline Emuera binaries `Emuera1707.exe` (writes
`__EMUERA_STRAT__`), `Emuera1710.exe` (`__EMUERA_1708_STRAT__`),
`Emuera1738.exe` (`__EMUERA_1729_STRAT__`) and `Emuera1803.exe`
(`__EMUERA_1803_STRAT__`) under wine+Xvfb, driving a tiny hand-written ERB
game — recovered from the archived SourceForge.jp/OSDN `emuera` project via
the JAIST mirror (`ftp.jaist.ac.jp/pub/sourceforge.jp/emuera/`). Captures +
full provenance in `tests/fixtures/emuera_saves/real_old/`; the reader tests
`parse_reads_real_old_marker_captures` / `parse_reports_1701_real_capture_as_absent`
prove each marker maps to its per-version grammar against the real bytes.
Notable findings: **Emuera 1.701 writes no extended block at all** (marker
`Absent`; the 1700 marker came between 1.701 and 1.707), and each old
writer's marker string is exactly the constant `EraDataStream.cs` names.
Two boundaries remain *source-only but synthetic-fixture-covered*: the
chara extended section's **4-vs-6 group** restructure at 1803 (exercising
it needs an active `TARGET` character, which requires a full chara-selection
flow the boot-driven minimal game does not reach — and closing it is not a
small extension: chara data lives in version-sensitive `CSV/Chara*.csv`
files rather than ERB, and reaching a `TARGET` needs an interactive
chara-selection routine plus real input (xdotool), an hours-scale,
multi-version effort; see `tests/fixtures/emuera_saves/real_old/README.md`), and the 2D/3D *structural*
markers `0xE0/0xE1/0xF1/0xF2` (never emitted by these small captures; covered
by the C#-writer-equivalent byte tests in §3). The variable-section group
counts per version, however, are now asserted against real old-Emuera bytes.

---

## 6. Resolution 3 — String 2D/3D arrays in text saves

Confirmed, and stronger than the doc stated:

- **Writer throws.** `EraDataWriter.WriteExtended(string, string[,])`
  (`EraDataStream.cs:658-661`) and `WriteExtended(string, string[,,])`
  (`:722-725`) both `throw new NotImplementedException("まだ実装してないよ")`
  ("not implemented yet"). The calling sites in `VariableData
  .SaveToStreamExtended` are annotated `//StringArray2Dの保存は未実装` /
  `//StringArray3Dの保存は未実装` (`VariableData.cs:718,731`).
- **Reader throws.** `EraDataReader.ReadStringArray2DExtended`
  (`EraDataStream.cs:338-349`) and `ReadStringArray3DExtended`
  (`:413-426`) throw `FileEE("StringArray2Dのロードには対応していません")` on
  any non-separator content (the message is a copy-paste; it is the 2D *and*
  3D reader). So a text save can never legitimately contain one, and
  Emuera's own reader refuses it.

So the doc's §6.4 ("3D string arrays in text are unimplemented in Emuera
itself (reader throws)") is confirmed — and it applies to **2D string too**,
and to the **writer** as well as the reader. "The upstream doesn't implement
it either" is therefore the accurate framing, not "we didn't implement it".

**Binary string 2D/3D are a different story** — fully implemented: type
bytes `0x12`/`0x13` (`EraSaveDataType.StrArray2D/3D`,
`EraBinaryDataReader.cs:20-21`), dedicated readers
(`EraBinaryDataReader.cs:599-730`) and writers
(`EraBinaryDataWriter.writeData(string[,])` `:317-360`, `(string[,,])`
`:362-412`). Our `read_str2d`/`read_str3d` (`emuera.rs:907-1033`) match them.

---

## 7. Other findings for the 2026-09-06 doc

- **`global.sav` / `save*.sav` empty-string text field:** `SaveGlobal`
  writes `bWriter.WriteString("")` (a present `0x00` length byte) before the
  variable block (`VariableEvaluator.cs:2356`), confirming doc §4.2.
- **`0xFD` separator inside a binary local char block:** from Emuera **1813**
  on, `CharacterData.SaveToStreamBinary` writes `WriteSeparator()` (`0xFD`)
  **before** the `#DIM` user-defined chara vars, then `WriteEOC()` (`0xFE`)
  (`CharacterData.cs:451-462`). So a game that declares character-scope
  `#DIM` arrays produces a binary local char block shaped
  `[built-ins] 0xFD [user-defined] 0xFE`. Our `read_binary_records` returns
  `RecordEnd::Separator` at the first `0xFD` (`emuera.rs:1070-1072`), after
  which `parse_binary` fails its `EOC` assertion (`emuera.rs:1127-1133`).
  None of the six real fixtures exercise this (eraTHYMKR's char block has no
  `#DIM` chara vars), so it is an unexercised, clean refusal — recorded as a
  known limitation, not a bug. The doc §4.1/§4.3 wording that "no `0xFD`
  appears in a local binary save" is only true of this game; tighten it.
- **Writer-version difference (1.818 vs 1824):** `EraBinaryDataWriter`
  from 1824 omits trailing zero-runs before `EoD` (§4). The 1.818 capture
  matches that behaviour, so the earlier doc claim was simply wrong, not a
  version gap. Where the versions *do* differ is invisible to a reader that
  (like ours) reconstructs from the length field.
- **Encoding:** `EraBinaryDataReader`/`EraBinaryDataWriter` construct their
  `BinaryReader`/`BinaryWriter` with `Encoding.Unicode` (UTF-16LE)
  (`EraBinaryDataReader.cs:56`, `EraBinaryDataWriter.cs:19`), so
  `BinaryReader.ReadString` = 7-bit-varint byte length + UTF-16LE — exactly
  the doc's empirically-corrected §4.3, now source-confirmed.

---

## 8. Reader bugs found

**None.** The resolutions are confirmations: (1) binary 2D/3D markers are
already implemented correctly, (3) string 2D/3D in text are unimplemented in
Emuera itself on both the writer and reader side. Item (2) is now
**delivered, not merely confirmed**: old markers are supported (this
workstream, `feat/emuera-old-marker-support`) with per-version grammars, not
treated as "OLD block only" — see the updated §6.2 verdict and its explicit
evidence class (1700/1708/1729/1803 from the C# dispatch, not from real old
captures). The doc-level corrections (§4's trailing-zero-run error, §6.2
retirement, §6.3/§7 wording) plus the marker-exercising tests stand.