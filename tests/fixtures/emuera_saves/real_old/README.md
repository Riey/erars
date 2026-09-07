# Real old-Emuera save captures (2026-09-07)

Five saves produced by **actually running old Emuera mainline binaries** under
wine (wine-mono prefix) headless on Xvfb `:88`, driving a tiny hand-written
ERB game. These are the first real pre-1808 saves ever seen in this project —
they byte-validate the four old extended-block **marker strings** and the
per-version extended-block grammar that previously only the C# reader's
dispatch (`docs/research/2026-09-07-emuera-source-crosscheck.md` §5) implied.

## Markers observed (the headline finding)

| File | Emuera exe | Marker in save | Extended grammar |
|---|---|---|---|
| `1701_real.sav` | `Emuera1701.exe` 1.701 | *(none)* | pre-extended (marker `Absent`) |
| `1707_real.sav` | `Emuera1707.exe` 1.707 | `__EMUERA_STRAT__` | 1700 |
| `1710_real.sav` | `Emuera1710.exe` 1.710 | `__EMUERA_1708_STRAT__` | 1708 |
| `1738_real.sav` | `Emuera1738.exe` 1.736 | `__EMUERA_1729_STRAT__` | 1729 |
| `1803_real.sav` | `Emuera1803.exe` 1.803 | `__EMUERA_1803_STRAT__` | 1803 |

Note the timeline twist: **1.701 writes no extended block at all** (the `1700`
marker `__EMUERA_STRAT__` was introduced between 1.701 and 1.707 — 1.707 writes
it). So the `1700` marker branch is capture-backed by the **1.707** save, and
the pre-extended `Absent` state is capture-backed by **1.701**.

## Files

| File | Size | Note |
|---|---|---|
| `1701_real.sav` | 865 | no extended block |
| `1707_real.sav` | 959 | `__EMUERA_STRAT__` (1700) |
| `1710_real.sav` | 998 | `__EMUERA_1708_STRAT__` |
| `1738_real.sav` | 1036 | `__EMUERA_1729_STRAT__` |
| `1803_real.sav` | 1040 | `__EMUERA_1803_STRAT__` |

All are UTF-8-text saves (`SystemSaveInUTF8` default) with authentic CRLF,
committed byte-identical to what the exe wrote (never normalised).

## Provenance

- **Executables:** `Emuera1701.exe`, `Emuera1707.exe`, `Emuera1710.exe`,
  `Emuera1738.exe`, `Emuera1803.exe` — the original mainline Emuera binaries
  from the archived SourceForge.jp/OSDN **`emuera`** project, recovered from
  the JAIST academic mirror
  `ftp.jaist.ac.jp/pub/sourceforge.jp/emuera/`.
  osdn.net itself no longer resolves (service ended); these are the preserved
  release archives. **Exact recovery URLs (so a future reader with the same
  mirror archived can re-fetch, or see these specific archives):**

  | Release | File id `ftp.jaist.ac.jp/pub/sourceforge.jp/emuera/<id>/<file>` |
  |---|---|
  | 1.701 | `40666/Emuera1701.zip` |
  | 1.707 | `40904/Emuera1707.zip` |
  | 1.710 | `41229/Emuera1710.zip` |
  | 1.736 | `47955/Emuera1738.zip` |
  | 1.803 | `53137/Emuera1803.zip` |

  Each zip holds the bare exe (no installer). The **Windows file-version
  resource strings** of the extracted exes were read at capture time and
  matched the expected version (`Emuera1738.exe` reports **1.736** despite its
  1738 build name — MinorShift's build-number vs product-version gap; the Zip
  file name is the build number, the version resource is the product
  version). If JAIST later rotates or drops these archives, this table plus
  the byte hashes of the extracted exes are the only remaining provenance
  chain for these fixtures.
- **Runtime:** wine 11.16 + wine-mono prefix under **Xvfb `:88`**;
  `DISPLAY=:88 WINEPREFIX=/tmp/winemono_prefix WINEDEBUG=-all wine ./Emuera####.exe`.
  No xdotool needed — the game saves itself on boot.
- **Game (complete source — reproduce the capture from this alone):**

  `ERB/T.ERH` (declarations live only in `.ERH`, not `.ERB`; an ERB that
  opens with `#` fails with `関数宣言の直後以外で#行が使われています`):
  ```text
  #DIM SAVEDATA X, 3
  ```
  `ERB/T.ERB` (`X:0` must be set before `SAVEDATA` is called; the 2nd arg of
  `SAVEDATA` needs a string *variable* on ≤1.703 — string expressions came in
  1.704 — so `STR:0` is used, not a literal):
  ```text
  @SYSTEM_TITLE
      X:0 = 2
      STR:0 = "cap"
      SAVEDATA 90, STR:0
      QUIT
  ```
  `CSV/GameBase.csv` (game code and version so the save header carries them):
  ```text
  コード,999000001
  バージョン,1000
  ```
  The game is byte-identical for every version, so the only variable across
  the five captures is the Emuera exe. A modern-era game (eraTHYMKR v3.21)
  was tried first but old Emuera cannot parse its modern ERB
  (`解釈できない識別子` on 2D savedata vars etc.), so the hand-written game is
  the source, as the task suggested.
- **Output:** `SAVEDATA 90` → `save90.sav` in the game dir (config
  `セーブデータをsavフォルダ内に作成する:NO` default), copied here verbatim.
  The five `.sav` here are those files byte-for-byte (authentic CRLF kept;
  `.gitattributes` marks `tests/fixtures/emuera_saves/**/*.sav binary`).

## Reproducing

1. `wineboot` a mono prefix once (`WINEPREFIX=/tmp/winemono_prefix wineboot -u`).
2. `Xvfb :88 &`.
3. Game dir with `Emuera####.exe` + `ERB/T.ERH` + `ERB/T.ERB` + `CSV/GameBase.csv`,
   then `DISPLAY=:88 WINEPREFIX=/tmp/winemono_prefix WINEDEBUG=-all wine
   ./Emuera####.exe`; wait ~30 s; `save90.sav` appears in the game dir.

## Scope / honest limits

These captures exercise the **variable** extended section (the savedata var
`X`) and byte-verify each **marker string** and the per-version **group
count**. They do **not** exercise per-character savedata: `#DIM CHARADATA
SAVEDATA` vars need an active `TARGET` character, which requires a full
new-game/character-selection flow that the boot-driven minimal game does not
reach (and old Emuera cannot drive the modern-era game far enough to reach).
So the chara extended section's **4-vs-6 group** restructure (the 1803
boundary) remains source-derived, not byte-observed — the marker and variable
grammar are now capture-backed, the chara-section boundary is not.

**Feasibility of closing that gap (for a future pass):** not a small
extension. The blocker is confirmed empirically: declaring
`#DIM CHARADATA SAVEDATA CNA, 4` in the `.ERH` compiles fine, but any
reference at title time fails with `"CNA"は解釈できない識別子です` because no
character exists — `TARGET` is only established after character creation, and
Emuera's chara model is **not** code-defined like the savedata var; it lives
in `CSV/Chara*.csv` (+ optional `CSVI/`) character files the engine parses at
boot. Closing it needs: (a) authoring correctly-shaped `CSV/Chara*.csv`
files for each of the five versions (the chara CSV schema can differ between
them), (b) an interactive chara-selection routine in ERB (`@SELECT_CHARA`,
named-chara `▽`/`▲` navigation and `TARGET`/`SETCHARA`), and (c) real input
to pick a character — the boot-driven auto-save trick does not reach a
`TARGET` at all, so xdotool or a scripted key driver is required. It is a
multi-step, version-sensitive effort (estimate: hours, not minutes), which is
why it was left source-derived.