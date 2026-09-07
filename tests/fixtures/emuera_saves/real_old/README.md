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
  the JAIST academic mirror `ftp.jaist.ac.jp/pub/sourceforge.jp/emuera/`
  (release files `Emuera1701.zip` id 40666, `Emuera1707.zip` 40904,
  `Emuera1710.zip` 41229, `Emuera1738.zip` 47955, `Emuera1803.zip` 53137).
  osdn.net itself no longer resolves (service ended); these are the preserved
  release archives.
- **Runtime:** wine 11.16 + wine-mono prefix under **Xvfb `:88`**;
  `DISPLAY=:88 WINEPREFIX=/tmp/winemono_prefix WINEDEBUG=-all wine ./Emuera####.exe`.
  No xdotool needed — the game saves itself on boot.
- **Game:** a tiny hand-written ERB game (`ERB/T.ERH` declares
  `#DIM SAVEDATA X, 3`; `ERB/T.ERB`'s `@SYSTEM_TITLE` sets `X:0 = 2` and calls
  `SAVEDATA 90`; `CSV/GameBase.csv` gives `コード,999000001` / `バージョン,1000`).
  The game is the same for every version so the only variable is the exe.
  A modern-era game (eraTHYMKR v3.21) was tried first but old Emuera cannot
  parse its modern ERB (`解釈できない識別子` on 2D savedata vars etc.), so the
  hand-written game is the source, as the task suggested.
- **Output:** `SAVEDATA 90` → `save90.sav` in the game dir (config
  `セーブデータをsavフォルダ内に作成する:NO` default), copied here verbatim.

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