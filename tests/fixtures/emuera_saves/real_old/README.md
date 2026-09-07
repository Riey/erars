# Real old-Emuera save captures (2026-09-07)

Seven saves produced by **actually running old Emuera mainline binaries**
under wine (wine-mono prefix) headless on Xvfb `:88`, driving tiny
hand-written ERB games. These are the first real pre-1808 saves ever seen in
this project — they byte-validate the four old extended-block **marker
strings**, the per-version extended-block grammar, and (second pass, below)
the **chara section's 4-vs-6 group restructure at 1803**, all previously only
implied by the C# reader's dispatch
(`docs/research/2026-09-07-emuera-source-crosscheck.md` §5/§6).

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
| `1738_chara_real.sav` | 1370 | `__EMUERA_1729_STRAT__`, one `ADDVOIDCHARA`'d chara, **4**-group chara section |
| `1803_chara_real.sav` | 1432 | `__EMUERA_1803_STRAT__`, one `ADDVOIDCHARA`'d chara, **6**-group chara section (`CDFLAG` set) |

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

## Chara-section boundary captures (`*_chara_real.sav`, 2026-09-07 second pass)

The 5-file pass above deliberately left the chara extended section's 4-vs-6
group restructure at 1803 source-derived: it needs at least one character to
exist, and the boot-driven minimal game above never creates one. **Closing
that gap turned out not to need the interactive `@SELECT_CHARA`/`TARGET` flow
a previous pass judged necessary** — `ADDVOIDCHARA` plus explicit-index
chara-array writes (`CSTR:0:0 = ...`, `CFLAG:0:1 = ...`) work perfectly well
non-interactively, at title time, with no `TARGET` ever set.

`ERB/T.ERH` — same as above (`#DIM SAVEDATA X, 3`).

`ERB/T.ERB` for `1738_chara_real.sav` (Emuera1738, marker `__EMUERA_1729_STRAT__`):
```text
@SYSTEM_TITLE
    X:0 = 2
    STR:0 = "cap"
    ADDVOIDCHARA
    CSTR:0:0 = "cap_name"
    CFLAG:0:1 = 7
    CFLAG:0:2 = 13
    SAVEDATA 90, STR:0
    QUIT
```

`ERB/T.ERB` for `1803_chara_real.sav` (Emuera1803, marker `__EMUERA_1803_STRAT__`)
— **identical plus one line**, `CDFLAG:0:0:0 = 42`:
```text
@SYSTEM_TITLE
    X:0 = 2
    STR:0 = "cap"
    ADDVOIDCHARA
    CSTR:0:0 = "cap_name"
    CFLAG:0:1 = 7
    CFLAG:0:2 = 13
    CDFLAG:0:0:0 = 42
    SAVEDATA 90, STR:0
    QUIT
```

`CSV/GameBase.csv` — same as above (code `999000001`, version `1000`).
**Same SJIS caveat as the main captures applies and matters just as much
here**: `CSV/GameBase.csv` must be saved as **Shift-JIS**, not UTF-8/UTF-8
BOM — with a UTF-8 `GameBase.csv`, Emuera silently mis-parses `コード` (code)
as `0` and the resulting save's game code no longer matches, making the
capture useless without any visible error. `emuera.config` is SJIS too.

### Why `1738_chara_real.sav` has no `CDFLAG` line — a real, reproduced negative result

`CDFLAG` is real Emuera's **only chara-scope int-2D savedata variable**
(`CDFLAG:chara:dim1:dim2`, default size 1×1 per the wiki — hence index
`0:0:0`, not `0:1:1`; the latter genuinely is out of range and was tried and
rejected first, see below). It is exactly the variable the 1803-only int2D
chara group exists to carry, so the task asked for it to appear in *both*
captures. It could not: **Emuera1738 (product version 1.736) does not
recognise `CDFLAG` as an identifier at all.**

Evidence, in order of what was actually tried against the real exe:

1. `CDFLAG:0:1:1 = 99` (3-index form, first guess at the default size) →
   *runtime* error on **1803**: "キャラクタ配列変数CDFLAGの第２引数(1)は配列の
   範囲外です" ("chara array variable CDFLAG's second argument (1) is out of
   the array's range") — confirms 1803 recognises `CDFLAG` and its default
   size is 1×1, exactly as the wiki says.
2. `CDFLAG:0:0:0 = 42` (index corrected to the 1×1 default) → **1803**
   succeeds, writes `CDFLAG`/`42`/`__FINISHED` into the new int2D group.
   Same line on **1738** → *parse*-time error: "警告Lv2:T.ERB:8行目:ラベル文・
   命令文・代入文のいずれとも解釈できない行です" ("cannot be interpreted as a
   label/command/assignment statement") — the generic unrecognised-statement
   error, not a range or argument-count error.
3. To confirm step 2 is about the identifier `CDFLAG` itself and not the
   3-index chara-2D syntax in general, `RELATION:0:0:0 = 5` (also a
   chara+int2D variable, but one that predates 1803) was tried on **1738**:
   it fails at *runtime* — "キャラクタ変数RELATIONの引数が多すぎます"
   ("character variable RELATION has too many arguments") — a
   recognised-identifier error, unlike `CDFLAG`'s parse-time rejection.
4. The 2-argument form `CDFLAG:0:0 = 42` was also tried on **1738**, in case
   the 3-vs-2 index count itself was the issue: same parse-time "cannot be
   interpreted" error as step 2, ruling that out.

[INFERENCE] `CDFLAG` was introduced into Emuera at or after 1.803, not merely
reframed into new save groups at that version — plausibly the two changes
(the variable itself, and the save grammar to persist it) shipped together.
Only the save-format side of that claim is directly evidenced by these
captures; the variable-introduction timing is inferred from the "unknown
identifier" vs "known identifier, wrong shape" diagnostic contrast above, not
from reading Emuera's own source for 1.736–1.803.

This is reported here as a genuine, reproduced negative result per the task's
own instruction, not smoothed over: **`1738_chara_real.sav` cannot carry a
`CDFLAG` value, ever, on real Emuera1738**, so the two chara fixtures are
deliberately asymmetric (1803's game source has one extra line that 1738's
provably cannot accept). See `crates/erars-vm/src/save/emuera.rs`'s
`parse_reads_real_old_chara_captures`/`parse_real_old_chara_wrong_grammar_is_rejected`
test doc comments for how this shapes the two tests built on these fixtures.

### The `CSTR` quoting difference

Both chara captures' `CSTR` value is `"cap_name"` — **11 bytes, quotes
included** — not `cap_name` (8 bytes), even though the ERB source is a
straightforward `CSTR:0:0 = "cap_name"` literal assignment. This holds
identically on *both* 1738 and 1803, so it is not itself a marker-version
difference between the two fixtures here.

This is not a version quirk and not an authoring-path guess — it is
deterministic Emuera semantics, and the repo already contains the proof.
Plain `=` assignment to a string variable is **FORM-syntax assignment**:
the right-hand side is substituted the same way as a `PRINTFORM` argument
and stored **verbatim**
(`docs/research/emuera-wiki/exetc.md:102-133`, "Assignment to String
Variable Using FORM Syntax"). A quoted literal like `"cap_name"` therefore
stores its quote characters as ordinary text content; nothing strips them.
The *other* string-assignment form — the `'=` operator, which evaluates the
RHS as a string expression instead of a FORM literal
(`docs/research/emuera-wiki/exetc.md:125-133`) — was only added in Emuera
**ver1813**. Both `Emuera1738.exe` (product version 1.736) and
`Emuera1803.exe` (1.803) predate 1813, so `'=` did not exist on either exe
yet: there was never an alternative form available for these captures to
have used instead. The quoting is Emuera's FORM-assignment rule applying
deterministically, not a choice that could have gone the other way.

It **does** differ from the unrelated 1808-era real capture already in this
repo (`tests/fixtures/emuera_saves/real/save90_text_utf8_real.sav`, line
4677, game eraTHYMKR v3.21): that capture's `CSTR` value (`보통 집`, "ordinary
house") has no quotes. That's consistent with the same rule, not an
exception to it: per the wiki, `CSTR` is normally populated from a
character CSV's `CSTR,*,**` field at chara-load time, not through an ERB
`=` assignment at all — a different value-origin, so the FORM-verbatim rule
above never applies to it in the first place.

What **is** directly verified against this crate's own source: `LineCursor::
read_1d_arrays` in `crates/erars-vm/src/save/emuera.rs`
never strips quote characters, for any Emuera version — a string-1D value is
copied verbatim from its own line into `ParsedArray::Str1D`. So erars's
reader has no version-dependent (or any) quote-handling logic to be
inconsistent; whatever a real Emuera save's `CSTR` line contains byte-for-byte
is exactly what erars imports.

erars's own compiler models both assignment forms and this exact
FORM-vs-expression distinction directly:
`crates/erars-compiler/src/parser.rs:3560-3586` (the plain-`=` arm on a
string-typed target) parses the RHS with `form_assign_expr`, a
comma-tolerant FORM-literal parser matching Emuera's FORM-syntax semantics,
while the sibling `ComplexAssign::Str` arm
(`crates/erars-compiler/src/parser.rs:3556-3559`, the `'=` operator) parses
the RHS as an expression list for bulk sequential-fill assignment instead.
The plain-`=` FORM-literal handling was itself a prior bug fix in this repo
(`docs/research/2026-09-06-language-feature-work.md` §2.2): an earlier
version of that branch wrongly stopped at the first unescaped comma,
corrupting exactly this kind of literal.

## Scope / honest limits

The original 5-file pass exercises the **variable** extended section (the
savedata var `X`) and byte-verifies each **marker string** and the
per-version **group count**. The 2-file chara-boundary pass above closes the
one remaining source-only claim: the chara extended section's **4-vs-6
group** restructure at 1803 is now byte-observed, with a real value
(`CDFLAG`) inside the new group, not just an empty-separator count — see
`crates/erars-vm/src/save/emuera.rs`'s `parse_reads_real_old_chara_captures`
test. The one part of the original "closing this gap" plan that turned out
to be wrong: it does **not** need `CSV/Chara*.csv` character files, an
interactive `@SELECT_CHARA` flow, or real input — `ADDVOIDCHARA` plus
explicit chara-index writes reach the exact same real-writer code paths
non-interactively, at title time, on every version tested (1738 and 1803).
