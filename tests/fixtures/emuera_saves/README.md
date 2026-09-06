# Emuera save fixtures

Two sets, under this directory:

- **`real/`** — authoritative captures produced by actually running `Emuera1818_kr3.exe`
  (eraTHYMKR v3.21 corpus) under wine within this repo. See [`real/README.md`](real/README.md).
- **Hand-built reference fixtures** (this directory) — minimal readable examples constructed from
  the spec, for contract/tests. **These are hand-constructed, not captured** — but every low-confidence
  claim they used to encode has since been verified against the `real/` captures.

**Source of truth:** `docs/research/2026-09-06-emuera-save-format.md` (reverse-engineered from the IL
disassembly `/tmp/emuera.il` of `Emuera1818_kr3.exe`, then confirmed byte-for-byte against the `real/`
captures).

## Files & variants

| File | Variant | Intended Emuera name |
|---|---|---|
| `save01_text_sjis.sav` | local, text, Shift-JIS | `save01.sav` |
| `save01_text_utf8.sav` | local, text, UTF-8 (with BOM) | `save01.sav` |
| `save01_binary.sav` | local, binary | `save01.sav` |
| `global_text_sjis.sav` | global, text, Shift-JIS | `global.sav` |
| `global_text_utf8.sav` | global, text, UTF-8 (with BOM) | `global.sav` |
| `global_binary.sav` | global, binary | `global.sav` |

Shared logical content:
- `ScriptUniqueCode = 12345`, `ScriptVersion = 1808` (`0x710`).
- Local save description: `test save`. Local `characterCount = 1`.
- 1 character: `NO=7`, `NICKNAME="EmuChan"`, `CSTR=["hello","bye"]`, `CFLAG=[1,9]`,
  `RELATION=[[3,0,0],[0,5]]`.
- Variables: `DAY=15`, `MONEY=100` (int scalars), `FLAG=[1,1,0]` (int 1D), `MES="こんにちは"` (str scalar),
  `SAVESTR=["store"]` (str 1D).

## Text format (both SJIS and UTF-8)

Byte order (see report §2):
```
12345                          ScriptUniqueCode
1808                           ScriptVersion
test save                      saveDataText      (local only)
1                              characterCount    (local only)
__EMUERA_1808_STRAT__          extended-body marker
[char extended: NICKNAME, sep, NO, sep, CSTR(+__FINISHED), sep, CFLAG(+__FINISHED),
 sep, (str-2D empty), sep, RELATION(+__FINISHED), sep]
[var extended: MES, sep, DAY/MONEY, sep, SAVESTR(+__FINISHED), sep, FLAG(+__FINISHED),
 sep, (str-2D empty), sep, (int-2D empty), sep, (str-3D empty), sep, (int-3D empty),
 then 6 empty user-defined groups, each + sep]
```
Line separator is `\r\n` (StreamWriter/WriteLine). Empty sections contribute only their
`__EMU_SEPARATOR__` line. Sections:
- scalars: `NAME:VALUE`
- 1D arrays: bare `NAME`, one value per line, `__FINISHED`
- 2D int: bare `NAME`, comma-joined rows (blank line = all-zero row), `__FINISHED`
- string 2D/3D are intentionally absent (Emuera writer throws `NotImplementedException` for them).

**Encoding / BOM (empirically confirmed from `real/`):** with `SystemSaveInUTF8:YES` (UTF-8 variant)
the file **begins `EF BB BF`** and a loader must strip it before line 1. The Shift-JIS variant has no BOM.

**Global text grammar (empirically confirmed):** a global save has **no** save-text line, no character
count, and no character blocks. It is: `code`, `version`, the OLD global block (`dataIntegerArray[63]`
→ `100`+`__FINISHED`, then `dataStringArray[5]` → `__FINISHED`), the marker, then **exactly 6
`__EMU_SEPARATOR__`** (user-defined global str/int 1D/2D/3D groups). See `global_text_*.sav` (6 sep
lines) versus the local saves (char 6 + var 14 = 20 sep lines).

**Simplification (documented):** the local-save legacy OLD block (old CharacterData/VariableData
sections that a real 1.808 save carries between the header/charCount and `__EMUERA_1808_STRAT__`) is
**omitted** from the local hand-built fixtures for compactness (the global fixtures DO carry the small
OLD global block). A loader that scans for `__EMUERA_1808_STRAT__` (or a multi-pass reader) handles
these files; the OLD block's shape is documented in the report (§2.3–2.4) and visible in `real/save90_*`.
For byte-exact OLD parsing, prefer the `real/` captures.

## Binary format

`save01_binary.sav` / `global_binary.sav` layout (report §4, verified against `real/save90_binary_real.sav`
/ `real/global_binary_real.sav`):
```
WriteHeader()     89 45 52 41 0D 0A 1A 0A | 10 07 00 00 | 00 00 00 00   (16 bytes; "ERA" CR LF SUB LF)
WriteFileType     0x00 local / 0x01 global
WriteInt64(12345) int64 LE
WriteInt64(1808)  int64 LE
WriteString(...)  7-bit byte-length prefix + UTF-16LE   (saveText; global writes empty string -> 0x00)
WriteInt64(1)     charCount (local only)
[records: type byte + key + payload, contiguous]        (see below)
WriteEOF()        0xFF
```
Var record (`WriteWithKey`): `type` byte (`0`=int64, `1`=int64[], `2`=int64[2D], `3`=int64[3D],
`16`=string, `17`=string[], `18`=string[2D], `19`=string[3D]), then key via 7-bit byte-length
**UTF-16LE**, then payload. Integer scalar encoding (`m_WriteInt`): `0x00..0xCF` value inline; `0xD0`+int16;
`0xD1`+int32; `0xD2`+int64. Integer 1D array: `int32` length + per-element `m_WriteInt` (runs of zeros as
`0xF0 count`) + `0xFF` EoD. Integer 2D/3D: dims (int32×2 or ×3) then elements then `0xFF`. Strings in
arrays are 7-bit byte-length UTF-16LE each, then `0xFF` EoD.

**Binary strings are UTF-16LE, not UTF-8** (empirically corrected — the .NET `BinaryWriter` default was
an assumption). The 2D/3D and string-array encodings are now exercised (RELATION int2D in `save01_binary.sav`).

## Known-good name set
The variable names above (`NO`, `NICKNAME`, `CSTR`, `CFLAG`, `RELATION`, `DAY`, `MONEY`, `FLAG`, `MES`,
`SAVESTR`) are arbitrary representative names; real games define their own via `.csv`. A loader imports by
name and applies the declared default for any name absent from the file (Emuera's own reader semantics),
and skips unknown names — so these fixtures remain valid reference cases regardless of erars's variable
table.