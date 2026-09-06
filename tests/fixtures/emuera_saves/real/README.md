# Real Emuera save captures (authoritative)

Six files produced by **actually running `Emuera1818_kr3.exe`** (Emuera 1.818, from the eraTHYMKR v3.21
corpus) under wine, headless on Xvfb `:88`, within this repo. These are the **primary authority** for
`docs/research/2026-09-06-emuera-save-format.md` — they verify bytes where the IL disassembly only
implies them.

## Files

| File | Container | Local/Global | Size |
|---|---|---|---|
| `save90_text_utf8_real.sav` | text, UTF-8 (`SystemSaveInUTF8:YES`) | local | 15506 |
| `save90_text_sjis_real.sav`  | text, Shift-JIS (`SystemSaveInUTF8:NO`)  | local | 15458 |
| `save90_binary_real.sav`     | binary (`SystemSaveInBinary:YES`)       | local | 2317 |
| `global_text_utf8_real.sav`  | text, UTF-8                              | global | 186 |
| `global_text_sjis_real.sav`  | text, Shift-JIS                          | global | 183 |
| `global_binary_real.sav`     | binary                                   | global | 76 |

The six are the 3 container variants × (local save, global save).

## Provenance

- **Executable:** `Emuera1818_kr3.exe` from the **eraTHYMKR v3.21** corpus (game code `890016222`,
  script version `3210`). Emuera 1.818, `.NET 2.0`, version marker `0x710`.
- **Runtime:** wine + wine-mono prefix under **Xvfb `:88`**; `DISPLAY=:88 WINEPREFIX=/tmp/winemono_prefix
  WINEDEBUG=-all wine Emuera1818_kr3.exe`. Input via `xdotool`; verified by screenshot between stages.
- **Game state (all three runs, fresh each):** started a new game as **TEST**, **EASY** difficulty, no
  extra features, no prologue/glaze; the per-run `emuera.config` differed only in the two save-format
  flags (`セーブデータをUTF-8で保存する` / `セーブデータをバイナリ形式で保存する`).
- **Output location:** `UseSaveFolder=YES` → saves written to `sav/save90.sav` (autosave slot 90) and
  `sav/global.sav`; copied here verbatim (renamed with the container variant suffix).
- Autosave slot **90** was used; it is an ordinary local save and serves as the "local save" capture for
  all three container variants.

## Empirical findings (confirm/fix the spec)

1. **BOM:** UTF-8 text saves start `EF BB BF`; SJIS and binary saves do not. Loaders MUST strip `EF BB BF`
   before line 1 of UTF-8 text saves (`EraDataReader` uses BOM-detecting `StreamReader`).
2. **Binary magic:** `89 45 52 41 0D 0A 1A 0A` = "ERA" CR LF SUB LF (not the hex-split `D4 A0 A0 0A` the
   earlier IL reading implied).
3. **Binary strings are UTF-16LE** with a 7-bit-varint *byte-length* prefix (e.g. key `"NO"` =
   `04 4E 00 4F 00`), not UTF-8 as the .NET `BinaryWriter` default would suggest.
4. **Global extended = exactly 6 user-group separators** (`__EMU_SEPARATOR__`), and the OLD global block =
   `dataIntegerArray[63]` (`100` + `__FINISHED`) + `dataStringArray[5]` (`__FINISHED`).
5. **Local extended = 6 char-group seps + 14 var-group seps** (marker at line 4673 of 4731 in the UTF-8
   save).
6. **SJIS is lossy for Korean:** with `SystemSaveInUTF8:NO`, the Korean strings (this is a Korean game)
   are written as cp932 and do not round-trip — visible as `?`-style bytes in the SJIS saveText. Expected;
   do not reject a save because of it.
7. **Local binary char block framing:** contiguous `WriteWithKey` records (type byte + UTF-16LE key +
   payload), no `0xFD`/`0xFE` separators, ending with a single `0xFF` EOF.

## Reproducing

1. `wineboot` a mono prefix once (`WINEPREFIX=/tmp/winemono_prefix wineboot -u`).
2. Run headless: `Xvfb :88 &` then `DISPLAY=:88 WINEPREFIX=/tmp/winemono_prefix WINEDEBUG=-all wine
   /path/Emuera1818_kr3.exe`.
3. Drive setup via `xdotool` (see the game's own prompts; each run chose TEST/EASY/no-features and let
   the autosave fire on turn 1). Saves land in `sav/save90.sav` + `sav/global.sav`.