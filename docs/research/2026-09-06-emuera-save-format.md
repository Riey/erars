# Emuera On-Disk Save Format (1.818 / `0x710`)

**Date:** 2026-09-06
**Authorities, in order:**
1. **Real on-disk captures** (new, definitive) — six files produced by actually running `Emuera1818_kr3.exe` (eraTHYMKR v3.21 corpus) under wine headless (Xvfb), in all container variants, at `tests/fixtures/emuera_saves/real/` (see `real/README.md`). These verify bytes where IL only implies.
2. IL disassembly of `Emuera1818_kr3.exe` (Emuera 1.818, `.NET 2.0` x86, version marker `0x710`) at `/tmp/emuera.il`, produced by mono `ikdasm`, cited as `Emuera1818_kr3.exe IL <method> (.il:<line>)`.
3. erars's own writer `crates/erars-vm/src/save.rs` (contrast only).
**WebEmuera C# source is unavailable** (verified absent system-wide); compiled IL + real captures are the byte-level truth.

> These headers/documentation in READMEs of fixtures say the same thing in shorter form.

---

## 1. Container variants

Emuera has **two container formats**, chosen per save by the config flag `SystemSaveInBinary` (`ConfigData` key `SystemSaveInBinary`; `Config::SetConfig`, `.il:163866-163882`):

| Variant | Config flag | Writer class | Reader class | Format |
|---|---|---|---|---|
| **Text** (default; `SystemSaveInBinary=false`) | NO | `EraDataWriter` | `EraDataReader` | Plain line-oriented text |
| **Binary** (`SystemSaveInBinary=true`) | YES | `EraBinaryDataWriter` | `EraBinaryDataReader` | 16-byte header + typed key/value records |

Dispatch on load is **automatic by magic**: `EraBinaryDataReader::CreateReader` sniffs the 8-byte header; a match returns a binary reader, otherwise `null` and the loader falls back to `EraDataReader` (text). See `VariableEvaluator::LoadFrom` (`/tmp/emuera.il:109696` region) which calls `CreateReader`, then `LoadFromStreamBinary` if non-null else `LoadFromStream`.

### 1.1 Binary header (`EraBinaryDataWriter::WriteHeader`, `.il:93373-93430`)
```text
uint64 (8 bytes LE)   = 0x0A1A0A0D41524589   -> bytes 89 45 52 41 0D 0A 1A 0A   (= "ERA" CR LF SUB LF; EraBDConst.Header, `.il:~94645`, liter `0xa1a0a0d41524589`)
uint32 (4 bytes LE)   = 0x710               -> version marker 1.808+ (EraBDConst.Version1808)
uint32 (4 bytes LE)   = 0x00000000          -> reserved
```
**Empirically confirmed** by every real binary capture (`save90_binary_real.sav`/`global_binary_real.sav` start `89 45 52 41 0D 0A 1A 0A`).
(The trailing loop of `WriteHeader` is a dead branch — a counter starts at 0 and loops while `< 0`, so the 128-byte padding never runs; the header is exactly the 16 bytes above.)

### 1.2 Text encoding (`EraDataWriter::.ctor` / `EraDataReader::.ctor`)
- `EraDataWriter` wraps the file with `StreamWriter(file, Config.SaveEncode)` (`.il:144085-144086`).
- `Config.SaveEncode` defaults to `SHIFT-JIS` (`Config::.cctor`, `.il:166887`) and is switched to `"UTF-8"` when `SystemSaveInUTF8` is true (`Config::SetConfig`, `.il:163866-163875`).
- `EraDataReader` likewise uses `Config.Encode` (default `SHIFT-JIS`, `.il:166881`).
- **BOM — empirically confirmed:** real text captures confirm `SystemSaveInUTF8:YES` saves **begin `EF BB BF`** (`save90_text_utf8_real.sav`), while SJIS (`SystemSaveInUTF8:NO`) and binary saves have **no BOM**. `EraDataReader::.ctor` (`IL:142680-142683`) builds `new StreamReader(Stream, Config.Encode)` with default BOM detection, so a loader must **strip `EF BB BF` before line 1** of UTF-8 text saves.
- Line separators are the `StreamWriter`/`WriteLine` default (`\r\n`).

### 1.3 File naming & locations
From `VariableEvaluator` (`/tmp/emuera.il:107755-107868`):

| Save kind | Path | Format string |
|---|---|---|
| Numbered local slot | `Config.SavDir` + `save{0:00}.sav` | `getSaveDataPath(int)` `.il:107770` |
| Global | `Config.SavDir` + `global.sav` | `getSaveDataPathG()` `.il:107763` |
| `SAVEVAR` (named/index) | `Program.DatDir` + `var_{0:00}.dat` / `var_{name}.dat` | `getSaveDataPathV` `.il:107799,107831` |
| `SAVECHARA` (named/index) | `Program.DatDir` + `chara_{0:00}.dat` / `chara_{name}.dat` | `getSaveDataPathC` `.il:107815,107847` |

- `{0:00}` = **2-digit zero-padded**, so slot 1 → `save01.sav`, slot 14 → `save14.sav`.
- `Config.SavDir`: if `UseSaveFolder` (config `UseSaveFolder`) → `ExeDir + "sav\\"`; else → `ExeDir` (game root). On startup, if `UseSaveFolder` and the `sav\` dir doesn't exist, it is created and stray saves moved in (`Config::SetConfig`, `.il:163995-164032`). So the `sav/` subfolder is **optional and config-driven**, not intrinsic.
- `Config::CreateSavDir()` mkdirs the sav directory before writing (`EraDataWriter` call sites `.il:108391,108664,109130,109607`).

---

## 2. Record structure — TEXT variant

Two layers: a **legacy OLD block** (ancient eramaker-compatible) followed by `__EMUERA_1808_STRAT__` and the **extended 1.808 block**. **The OLD block is not optional or discardable**: real Emuera's own reader (`VariableEvaluator::LoadFromStream`, `.il:108953-109155`; `LoadGlobal`, `.il:109237-109353`) reads the OLD block unconditionally, for every character then the global vars, *before* it ever calls `SeekEmuStart` to look for the extended-block marker — and for a real capture, the OLD block is where nearly all actual game state lives (`DAY`, `MONEY`, `BASE`, `ABL`, `CFLAG`, ...); the extended block only *adds* whatever `IsSavedata` variables don't fit the legacy `VariableCode` slots. A loader that skips straight to `__EMUERA_1808_STRAT__` recovers almost nothing. `SeekEmuStart` returning `false` (pre-1.808 saves, no marker at all) is the *only* case where there is no extended block to layer on top — the OLD block by itself is still a complete, valid save in that case.

### 2.1 Local save, text (`VariableEvaluator::SaveToStream`, `/tmp/emuera.il:108868`)
Program order, byte-for-byte:
```text
<int64> ScriptUniqueCode                 line 1   (EraDataWriter::Write(int64) = decimal + newline)
<int64> ScriptVersion                    line 2
<string> saveDataText                    line 3   (the load-menu description; null -> empty line)
<int64> characterCount                   line 4
CharacterData::SaveToStream (OLD)   x characterCount
VariableData::SaveToStream (OLD)
"__EMUERA_1808_STRAT__"                  (EraDataWriter::EmuStart, `.il:144301`)
CharacterData::SaveToStreamExtended x characterCount
VariableData::SaveToStreamExtended
```
No trailing marker. (See §2.3.)

### 2.2 Global save, text (`VariableEvaluator::SaveGlobal`, `/tmp/emuera.il:109115`)
```text
<int64> ScriptUniqueCode
<int64> ScriptVersion
VariableData::SaveGlobalToStream        (OLD global var block)
"__EMUERA_1808_STRAT__"
VariableData::SaveGlobalToStream1808    (extended global vars)
```
**No** save-text line, **no** char-count line, **no** character blocks — global files are strictly smaller.

**Empirically confirmed** (`global_text_utf8_real.sav` / `global_text_sjis_real.sav`):
- **OLD global block** = `dataIntegerArray[63]` (writes `100` + `__FINISHED`) then `dataStringArray[5]` (writes `__FINISHED`), via `VariableData::SaveGlobalToStream` (`IL:50907-50927`). Both arrays are trimmed to last non-zero/empty.
- **Extended global = exactly 6 user-defined group separators** (`__EMU_SEPARATOR__`), one per `str1D/int1D/str2D/int2D/str3D/int3D` group — `VariableData::SaveGlobalToStream1808` (`IL:50956-51073`) loops 6 groups, each with one `EmuSeparete()`. See §2.5 contrast with the local extended block (which has the 8 built-in + 6 user groups = 14 separators).

### 2.3 Legacy OLD blocks — the mandatory, positional data (byte-exact validated)
"Legacy" names the *format*, not the *content*: these fields are ordinary, current `IsSavedata` game state, just written positionally (one value per line, in enum-index order) instead of as `KEY:VALUE`/`KEY` pairs. The OLD scalar loops are dead code in this build (bounds `ldc.i4.0; blt`, never true — 0 scalars written/read), but the array loops run and carry real values:
- **OLD `CharacterData`** (`CharacterData::SaveToStream`/`LoadFromStream`, `.il:148923-149100`), per character: 2 strings (`NAME`, `CALLNAME`) + 2 ints (`ISASSI`, `NO`) + 17 int-arrays (`BASE` through `NOWEX`, in `VariableCode` enum order — see `crates/erars-vm/src/save/emuera.rs`'s `CHAR_OLD_STR`/`CHAR_OLD_INT`/`CHAR_OLD_ARR` consts) + 0 string-arrays.
- **OLD `VariableData`, embedded in a local save** (`VariableData::SaveToStream`/`LoadFromStream`, `.il:49290-49373`): 0 strings + 0 ints + **60 int-arrays** (`DAY` through `NOTUSE_3B`, `GLOBAL_OLD_ARR`) + 1 string-array (`SAVESTR`).
- **OLD `VariableData`, standalone `global.sav`** (`VariableData::SaveGlobalToStream`/`LoadGlobalFromStream`, `.il:50912-50944`) — a **completely separate, dedicated writer/reader**, not a special case of the local-embedded one above: exactly 2 fields, `dataIntegerArray[63]` (key `GLOBAL`) then `dataStringArray[5]` (key `GLOBALS`), nothing else. Confusing the two schemes (e.g. trying to read a standalone `global.sav`'s OLD block as 60 arrays + `SAVESTR`) desyncs immediately.

Byte-exact validated end-to-end against the real captures: parsing header → per-character OLD block (× `characterCount`) → OLD global block, positionally by the field order above, lands exactly on the `__EMUERA_1808_STRAT__` line with zero drift (`save90_text_utf8_real.sav`: line 4672 after 4671 preceding lines; `global_text_utf8_real.sav`: line 5).

### 2.4 Extended text primitives (`EraDataWriter`, `/tmp/emuera.il:144070-144423`)
Constants: `FINISHER="__FINISHED"`, `EMU_START="__EMUERA_1808_STRAT__"`, `EMU_SEPARATOR="__EMU_SEPARATOR__"`.

| Primitive | Output |
|---|---|
| `Write(int64)` | `value\n` (decimal, `WriteLine`) |
| `Write(string)` | `str\n`; null → empty line |
| `Write(int64[])` | last non-zero index trimmed; **one value per line**, then `__FINISHED` |
| `Write(string[])` | last non-null/non-empty element trimmed; one element per line (empty → blank line), then `__FINISHED` |
| `WriteExtended(key, int64)` | skip if `0`; else `{key}:{value}\n` |
| `WriteExtended(key, string)` | skip if null/empty; else `{key}:{value}\n` |
| `EmuSeparete()` | `__EMU_SEPARATOR__\n` |
| `EmuStart()` | `__EMUERA_1808_STRAT__\n` |
| 2D int array | rows as comma-joined values, one row per line, trailing zero columns trimmed, all-zero row = blank line, then `__FINISHED` |
| 3D int array | `[index]{` header line, comma-joined rows inside, `}` terminator, then `__FINISHED` |

### 2.5 Extended section layout (grammar)
Reading order of `VariableData::LoadFromStreamExtended` (`.il:~50000`) / `CharacterData::LoadFromStreamExtended` (`.il:149364`): each section is terminated by a `__EMU_SEPARATOR__` line consumed by the section reader. 2D-string and 3D sections are **empty in text saves** (`ReadStringArray2DExtended` throws if it encounters a non-separator key in text).

Flag decode (each `GetExtSaveList(code)` filter; `VariableIdentifier::GetExtSaveList`): bit `0x20000`=int scalar, `0x40000`=str scalar, `0x80000`=1D, `0x8000000`=2D, `0x20000000`=3D, `0x100000`=character.

**Variable section — LOCAL saves** (order, after `__EMUERA_1808_STRAT__`):
```text
[VAR] string scalars   (0x40000) : key:value lines
__EMU_SEPARATOR__
[VAR] int scalars      (0x20000) : key:value lines
__EMU_SEPARATOR__
[VAR] string 1D        (0xc0000) : bare key line, then values, __FINISHED
__EMU_SEPARATOR__
[VAR] int 1D           (0xa0000) : bare key line, then values, __FINISHED
__EMU_SEPARATOR__
[VAR] string 2D        (0x8040000) : EMPTY in text
__EMU_SEPARATOR__
[VAR] int 2D           (0x8020000) : bare key line, then comma-rows, __FINISHED
__EMU_SEPARATOR__
[VAR] string 3D        (0x20040000) : EMPTY in text
__EMU_SEPARATOR__
[VAR] int 3D           (0x20020000) : bare key line, [i]{ rows }, __FINISHED
__EMU_SEPARATOR__
[user-defined] str 1D, int 1D, str 2D, int 2D, str 3D, int 3D   (each preceded by __EMU_SEPARATOR__, same shape as above)
```
(The reader performs a second pass over only the 6 user-defined array sections when `version >= 0x710`, `VariableEvaluator::LoadFromStream`.)

**Character section** (per character, in `CharacterData::SaveToStreamExtended`, `.il:149094`): the six groups with **six separators** — one after each group, so a single char block ends with its last group's separator (verified: real `save90_text_utf8_real.sav` has char-seps at lines 4676,4677,4714,4715,4716,4717 then the 14 var-seps at 4718–4731).
```text
[CHAR] string scalars (0x140000)            key:value lines      (e.g. NICKNAME:foo)
__EMU_SEPARATOR__
[CHAR] int scalars    (0x120000)            key:value lines      (e.g. NO:100)
__EMU_SEPARATOR__
[CHAR] string 1D      (0x1c0000)            bare key + values + __FINISHED  (e.g. CSTR)
__EMU_SEPARATOR__
[CHAR] int 1D         (0x1a0000)            bare key + values + __FINISHED  (e.g. CFLAG)
__EMU_SEPARATOR__
[CHAR] string 2D      (0x8140000)           EMPTY in text
__EMU_SEPARATOR__
[CHAR] int 2D         (0x8120000)           bare key + comma-rows + __FINISHED  (e.g. RELATION)
__EMU_SEPARATOR__
```
Each per-char block **ends with a separator**, so the next char's first section (string scalars) begins directly after, and the final char's separator is consumed by the last char's `ReadInt64Array2DExtended`; the variable section then follows immediately (no extra separator between chars and vars).

> **Which variables appear:** only those whose `VariableData` hand is `IsSavedata` (`VariableToken::IsSavedata`), filtered by the section flag. Scalars equal to `0`/`""` and trailing all-zero/all-empty array tails are omitted (trimming in the writers). This means a loader **must apply defaults** for absent names — identical to Emuera's own reader semantics.

---

## 3. Header fields

- **ScriptUniqueCode** (`GameBase::ScriptUniqueCode`): the game's unique code, from CSV/GAMEBASE (`todo.md` in the `emuera-wiki` mirror lists `GAMEBASE_GAMECODE`); validated by `GameBase::UniqueCodeEqualTo` on load — mismatch ⇒ "not this game's save".
- **ScriptVersion** (`GameBase::ScriptVersion`): compared by `GameBase::CheckVersion` on load; mismatch ⇒ refused.
- **saveDataText** (local saves only): the description string shown in the load menu (`CHKDATA`).
- **characterCount** (local saves only): number of `CharacterData` blocks; the loader reads exactly that many char sections the extended block.

---

## 4. Record structure — BINARY variant

### 4.1 Local save, binary (`VariableEvaluator::SaveToStreamBinary`, `/tmp/emuera.il:109407`; **fully validated byte-for-byte** against `save90_binary_real.sav`)
```text
WriteHeader()           16 bytes  (magic + 0x710 + 0)
WriteFileType(Normal)   1 byte    0x00     (EraSaveFileType.Normal; `.il:94567`)
WriteInt64(code)        8 bytes   raw LE int64   (`EraBinaryDataWriter::WriteInt64` = BinaryWriter.Write(int64), `.il:93437`)
WriteInt64(version)     8 bytes
WriteString(saveText)   see §4.3 (7-bit byte-length prefix + UTF-16LE; NOT SJIS, NOT UTF-8)
WriteInt64(charCount)   8 bytes
CharacterData::SaveToStreamBinary  x charCount   (contiguous WriteWithKey records, no separators)
WriteEOF()              1 byte    0xFF
```
Real capture decoded to 37 contiguous records (`ISASSI,NO,BASE,...,TCVAR, NAME,CALLNAME,NICKNAME,MASTERNAME, CSTR, ITEM_MAP, MONSTER_MAP, PLAYER_DATA, INVENTORY_ITEM, INVENTORY_FLAG, ITEM_NAME_RPG, ITEM_DISCRIPT_RPG, ITEM_DATA_RPG, MONSTER_DATA`) then a single `0xFF` EOF at offset 2316 — **no `0xFD`/`0xFE` separators appear in a local binary save**.

### 4.2 Global save, binary (`SaveGlobal` → `SaveGlobalToStreamBinary`, `.il:109115,51432`; **validated against `global_binary_real.sav`**)
```text
WriteHeader()  WriteFileType(Global=1)  WriteInt64(code)  WriteInt64(version)
WriteString("")                          (empty string — length byte 0x00 present; binary global DOES write the text field)
VariableData::SaveGlobalToStreamBinary   (= WriteWithKey per global non-chara savedata var)
WriteEOF()
```
Real `global_binary_real.sav` (76 B): header(16) + FileType(1) + code(8) + version(8) + empty-str byte `00` + one `WriteWithKey` record (`int64[]` key `GLOBAL`) + `0xFF` EOF.

### 4.3 Binary value encoding (`EraBinaryDataWriter`)
**Strings — UTF-16LE, 7-bit byte length (empirically corrected):** `BinaryWriter.Write(string)` in this build emits a **7-bit-varint byte-length prefix followed by UTF-16LE bytes** (not UTF-8 as the .NET default might imply). Verified across real captures: key `"NO"` = `04 4E 00 4F 00` (len 4 B = 2 UTF-16 units); saveText `2026/09/06 22:48:35 1일째 낮 eratohoYM 이지 (1회차)` = len byte `5E` (94 B = 47 units). A loader MUST decode UTF-16LE strings with a 7-bit byte-length prefix.
**Integer scalar** (`m_WriteInt`, `/tmp/emuera.il:93634-93730`):
| Range | Bytes |
|---|---|
| `0 <= v <= 0xCF` | `v` (single byte) |
| `-0x8000 <= v < 0` or `0xCF < v <= 0x7FFF` | `0xD0` + `int16` LE |
| `-0x80000000 <= v < -0x8000` or `0x7FFF < v <= 0x7FFFFFFF` | `0xD1` + `int32` LE |
| else | `0xD2` + `int64` LE |

(`0xCF`=Byte, `0xD0`=Int16, `0xD1`=Int32, `0xD2`=Int64 tag — `Ebdb` markers, `.il:~94620`.)

**Integer 1D array** (`writeData(int64[])`, `.il:93732-93845`): `int32 length` (full logical length) + elements (each via `m_WriteInt`; runs of ≥1 consecutive zeros are compressed as `0xF0` `count` via `m_WriteInt`), terminated by `0xFF` (EoD). If the tail is all zeros, the trailing zero-run is still emitted (`0xF0 count`) — the length field carries the true extent. (Verified: real `BASE` array = len `64` + `0x0F`... values + zero-run + `0xFF`.)
**Integer 2D / 3D** (`writeData(int64[0...,0...])`, `.il:93847`): `dim0:int32, dim1:int32` (+ `dim2` for 3D), then elements via `m_WriteInt` with zero-run compression, terminated by `0xFF` (signature `02 10 52 45 4C 41 54 49 4F 4E 00 00 00 02 00 00 00 03 00 00 00` verified in real save for `RELATION` int2D, dims 2×3).

**Per-variable record** (`WriteWithKey(key, value)`, `/tmp/emuera.il:93491-93630`):
```
<type byte> <key string> <payload>
```
type byte: `0`=int64, `1`=int64[], `2`=int64[2D], `3`=int64[3D], `16`=string, `17`=string[], `18`=string[2D], `19`=string[3D].
`<key string>`: `BinaryWriter.Write(string)` (7-bit byte-length prefix + **UTF-16LE**).
`<payload>`: the scalar / 1D / 2D / 3D encoding above.

**Terminators:** `WriteSeparator`=0xFD (`EraSaveDataType.Separator`), `WriteEOC`=0xFE, `WriteEOF`=0xFF. **Note:** in a local/global binary save these separators do not appear between records — the type byte disambiguates each record and the file ends with a single `0xFF`. (`0xFD`/`0xFE` are used by `SaveVariable`/`LoadVariableBinary` for `SAVEVAR`/`SAVECHARA` dat files.)

> **Loader recommendation:** implement binary per the above (now fully validated against real captures), or refuse binary cleanly. Binary strings are **UTF-16LE** (7-bit byte length), and the type-byte set `{0,1,2,3,16,17,18,19}` is confirmed.

---

## 5. erars vs Emuera — comparison table

| Aspect | Emuera 1.818 | erars (`crates/erars-vm/src/save.rs`) |
|---|---|---|
| Container | Plain text (SJIS/UTF-8, UTF-8 has BOM) **or** typed binary | **gzip** + MessagePack (`rmp_serde`) (`RawSaveData`, `save.rs:16-63`) |
| Magic | 8-byte `89 45 52 41 0D 0A 1A 0A` (binary only) | 4-byte `[0x01,0x02,0xFF,0xFE]` (`save.rs:18`) |
| Header fields | uniqueCode, version, saveText, charCount (lines) | magic, unique code, version, description (`write_dat_header`, `save.rs:33`) |
| Numbered saves | `save{idx:02}.sav` | `save{idx:02}.rsav.gz` (`make_save_file_name`, `save.rs:40`) |
| Global | `global.sav` | `global.rsav` (`GLOBAL_SAVE_FILE_NAME`, `save.rs:42`) |
| SAVECHARA | `chara_{idx:02}.dat` / `chara_{name}.dat` | `chara_<name>.dat` (`write_chara_data`, `save.rs:46`) |
| SAVEVAR | `var_{idx:02}.dat` / `var_{name}.dat` | `var_<name>.dat` (`write_var_data`, `save.rs:55`) |
| SAVETEXT | `txt{idx:02}.txt` (`GetSaveDataPathText`, per `text_file_path` doc `save.rs:111`) | `txt{idx:02}.txt`, same |
| Layout | Legacy OLD block + `__EMUERA_1808_STRAT__` + extended sections | Serde struct storage (`SerializableVariableStorage`/`Global`/`Chara`/`Var`) |
| Vars | `name` + flag section, defaults applied for absent | `is_savedata` vars only, resized via `VmVariable::overwrite_from` for length mismatch |
| Reference to Emuera in `save.rs` | — | `CheckDatFilename`, `getSaveDataPathC/V`, `GetDatFiles` (`VariableEvaluator.cs:1786-1809`), `GetSaveDataPathText`, `SaveVariable`/`LoadVariableBinary`, `WriteWithKey` |

Biggest structural differences: (1) Emuera stores **plaintext** sections separated by sentinel lines; erars **gz-compresses** a binary blob. (2) Emuera's per-char `CharacterData` blocks are read in the script's listed (CSV) order; erars stores chara data column-wise and re-derives rows. (3) Binary Emuera strings are **UTF-16LE**; text Emuera strings are SJIS-or-UTF-8 (with BOM) by config; erars is always MessagePack UTF-8. (4) Emuera trims default-valued (0/empty/tail) data out of the file and re-defaults on load; erars keeps the declared defaults and applies `overwrite_from` on mismatch.

---

## 6. Unresolved / low-confidence

1. ~~**UTF-8 text BOM**~~ — **RESOLVED (empirical):** UTF-8 text saves begin `EF BB BF`; SJIS and binary do not.
2. **2D/3D exact byte framing in binary** — the writer side is fully validated (see §4.3: `RELATION` int2D dims 2×3 decodes). The `EraBinaryDataReader` 2D/3D *reader* marker semantics (EoA2 `0xE1` / ZeroA2 `0xF2` on the read path) were not exercised by the real captures (the eraTHYMKR char block's 2D/3D arrays wrote uncompressed), so loader 2D/3D *reading* should still be treated with mild caution — but the on-disk record framing is confirmed.
3. **Old-era marker variance** — older versions wrote `__EMUERA_STRAT__` (1.700), `__EMUERA_1708_STRAT__`, `_1729_`, `_1803_`. This build emits only `__EMUERA_1808_STRAT__`. Recommendation: accept `1808` as primary; treat any other marker as "unsupported version".
4. **3D string arrays in text** are unimplemented in Emuera itself (reader throws); any loader should reject a text save that contains a 3D string key.
5. **BOM / char-count edge**: a `characterCount` of 0 (pure-var or fresh global) — global saves never write a count line; local saves always do. UTF-8 text saves carry a `EF BB BF` BOM on line 1 (strip before parsing); SJIS and binary do not.
6. **SJIS is lossy for non-cp932 text.** The eraTHYMKR game is Korean; with `SystemSaveInUTF8:NO` the saveText/string values are written as SHIFT-JIS (cp932), which cannot represent Hangul — the real SJIS capture shows `?`-style byte substitution in place of Korean, i.e. the game's own Korean strings do not survive a SJIS save round-trip. Loaders should not reject on this; it is expected behavior.

---

## 7. Errata / notes for the loader

- **The OLD block must be parsed, not skipped** (see §2.3): it is where nearly all of a real save's actual game state lives. A loader reads it positionally first (per character, then the global vars), *then* scans for `__EMUERA_1808_STRAT__` (`SeekEmuStart`) and layers the extended block on top if found — mirroring `VariableEvaluator::LoadFromStream`'s exact call order (OLD reads, then `SeekEmuStart()`, then extended reads only if it returned `true`). `SeekEmuStart` returning `false` (pre-1.808 saves, no marker at all) means there is no extended block to add — not that the OLD block should have been skipped.
- Extended scalar sections are `KEY:VALUE`, arrays are `KEY` then values then `__FINISHED`, 2D is comma-joined rows + `__FINISHED`. All separators are `__EMU_SEPARATOR__` and the extended-body marker is `__EMUERA_1808_STRAT__`. End of the variable extended block is simply **EOF** (no trailing `__FINISHED` after the last user-defined section).
- Emuera's text reader reads numeric arrays up to the trimmed length and back-fills the declared default beyond it — match this with `VmVariable::overwrite_from` (already present in erars).