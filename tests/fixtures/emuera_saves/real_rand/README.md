# Real Emuera `RAND` capture (2026-09-07)

One save produced by **actually running `Emuera1818_kr3.exe`** (the same
executable `tests/fixtures/emuera_saves/real/` uses, from the eraTHYMKR
corpus) under wine, headless on Xvfb `:88`, driving a tiny hand-written ERB
game. This is the ground truth for `crates/erars-vm/src/emuera_rand.rs`:
erars's from-scratch reimplementation of Emuera's PRNG is checked against
values this exact binary actually produced, not against a re-reading of its
own decompiled source.

## Files

| File | Size | Note |
|---|---|---|
| `randcap90_real.sav` | 8599 | `SAVEDATA 90` output, UTF-8 text save |

## Provenance

- **Executable:** `Emuera1818_kr3.exe`, SHA-256
  `a92d46467cfc36bc35055b6a747993f33b1279279c093548a3173b0431575384` — byte-
  identical to `eraTHYMKR/Emuera1818_kr3.exe` (verified by hash at capture
  time), the same exe `tests/fixtures/emuera_saves/real/README.md` used.
- **Runtime:** wine 11.16 + wine-mono prefix under **Xvfb `:88`**;
  `DISPLAY=:88 WINEPREFIX=/tmp/winemono_prefix WINEDEBUG=-all wine
  Emuera1818_kr3.exe`. No `xdotool` needed — the game saves itself on boot
  and quits, exactly like `tests/fixtures/emuera_saves/real_old/`'s
  hand-written-game pattern.
- **Game (complete source — reproduce the capture from this alone):**

  `ERB/T.ERH`:
  ```text
  #DIM SAVEDATA RANDCAP, 20
  ```
  (`RANDCAP`, not `RESULT` — `RESULT` is already an Emuera built-in and
  `#DIM`ing it is rejected with a `警告Lv2` parse warning that aborts the
  boot before anything else runs; this cost one throwaway run to discover.)

  `ERB/T.ERB`:
  ```text
  @SYSTEM_TITLE
      RANDOMIZE 23478612
      RANDCAP:0 = RAND:2
      RANDCAP:1 = RAND:16
      RANDCAP:2 = RAND:100
      RANDCAP:3 = RAND:7
      RANDCAP:4 = RAND:3
      RANDCAP:5 = RAND:100000
      RANDCAP:6 = RAND:4000000000
      RANDCAP:7 = RAND(50)
      RANDCAP:8 = RAND(10,20)
      RANDCAP:9 = RAND(-5,5)
      RANDCAP:10 = RAND(0,5000000000)
      DUMPRAND
      RANDCAP:11 = RAND:100000
      RANDCAP:12 = RAND:100000
      INITRAND
      RANDCAP:13 = RAND:100000
      RANDCAP:14 = RAND:100000
      SAVEDATA 90, "cap"
      QUIT
  ```
  Deliberately covers: the `RAND:max` pseudo-variable and the `RAND(max)` /
  `RAND(min, max)` function form; power-of-two bounds (`2`, `16`) and
  non-power-of-two bounds (`100`, `7`, `3`, `100000`) where modulo-bias
  reduction bugs surface; a bound past `u32::MAX` reached only through the
  function form's full-`i64` argument (`4000000000`, `5000000000` — `RAND:x`
  itself is limited to erars's own `u32` argument representation, a
  pre-existing, unrelated constraint, not something this capture is trying
  to probe); a negative lower bound (`RAND(-5,5)`); and a same-run
  `DUMPRAND`/`INITRAND` round trip (`RANDCAP:13,14` must equal
  `RANDCAP:11,12` if `INITRAND` truly rewinds the generator to the exact
  point `DUMPRAND` captured it at).

  `CSV/GameBase.csv` (must be **Shift-JIS**, like every other fixture in
  this tree — a UTF-8 `GameBase.csv` makes Emuera silently parse `コード` as
  `0`):
  ```text
  コード,999000001
  バージョン,1000
  ```
- **Output:** `SAVEDATA 90` → `save90.sav` in the game dir, copied here
  verbatim (authentic CRLF kept, `.gitattributes` already marks
  `tests/fixtures/emuera_saves/**/*.sav binary`).

## What the capture proves

`randcap90_real.sav`'s `RANDCAP` array (see the save's own `RANDCAP`
section, right after the last `__EMU_SEPARATOR__` group) reads:

```text
0, 1, 67, 0, 2, 95565, 3394868286, 40, 18, -5, 4119290769, 60789, 83791, 60789, 83791
```

`crates/erars-vm/src/emuera_rand.rs`'s `EmuRandom`, seeded with
`RANDOMIZE 23478612` and driven through the exact same 15 draws (see
`crates/erars-vm/tests/emuera_rand_save_fixture.rs`), reproduces every one
of these 15 values bit for bit — including the two non-trivial ones: index
6 (`3394868286`, `RAND:4000000000`, a bound past `2^32`) and indices 11-14
(the `DUMPRAND`/`INITRAND` pair matching exactly, proving the
save/restore round trip is lossless).

The save's `RANDDATA` array (the 624 SFMT-19937 state words plus the
refill index — 625 `Int64`s total, immediately after the `RANDDATA` name
in the file) is the generator's exact internal state at the moment
`DUMPRAND` ran (right after `RANDCAP:10`). It **also** matches
`EmuRandom::get_state()` bit for bit at that same point in the simulated
draw sequence — this is the actual save-compat proof:
`crates/erars-vm/tests/emuera_rand_save_fixture.rs` loads this file through
the real `LOADDATA` builtin (`erars_vm`'s save-discovery →
`save::emuera::sniff`/`parse` → `build_local_data` →
`VariableStorage::load_serializable`, which restores `RANDDATA` as an
ordinary variable and — matching real Emuera precisely — never touches the
live generator itself), then calls `INITRAND` explicitly from
`@SYSTEM_LOADEND` (`LOADDATA` transitions control flow to the shop/train
state on success, so nothing after it in the same event runs — real
Emuera's own `SYSTEM_LOADEND` hook is where a script has to put this), and
then draws two more `RAND:100000` values through erars — asserting they
come back as `60789, 83791`, the same continuation real Emuera would have
produced from this exact save. This is a different round trip than the one
the capture script above exercises (that one never calls `LOADDATA` — its
`DUMPRAND`/`INITRAND` pair is a same-session round trip): the fixture test
separately proves that a *file-based* `LOADDATA` restores `RANDDATA`
correctly too, using the same generator state either way.

## Reproducing

1. `wineboot` a mono prefix once (`WINEPREFIX=/tmp/winemono_prefix wineboot -u`).
2. `Xvfb :88 &`.
3. Game dir with `Emuera1818_kr3.exe` + `ERB/T.ERH` + `ERB/T.ERB` +
   `CSV/GameBase.csv` (all above), then `DISPLAY=:88
   WINEPREFIX=/tmp/winemono_prefix WINEDEBUG=-all wine
   ./Emuera1818_kr3.exe`; wait ~30 s; `save90.sav` appears in the game dir.
