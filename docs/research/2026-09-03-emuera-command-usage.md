# Emuera Command Usage in the eraTHYMKR Corpus

Date: 2026-09-03
Scope: data collection only — corpus `/home/riey/repos/eraTHYMKR` (Emuera 1.8.1.8 game), `.ERB`/`.erb`/`.ERH` under `ERB/` (864 files: 796 `.ERB` + 61 `.erb` + 7 `.ERH`; 1,045,721 lines of `.ERB`). No source edits.
Method: ripgrep (PCRE2) sweeps. Line-head = first identifier after leading whitespace, case-insensitive (`rg -P -o -i '^\s*\K[a-z_][a-z0-9_]*'`), `;`-comment lines excluded naturally (a comment line's first non-space char is `;`, not a letter). Counts were cross-checked with per-instruction `rg -c` and an any-position whole-corpus sweep; every non-zero claim was verified by reading the file:line.

Baseline: 96 fall-through `InstructionCode` list from `docs/research/2026-09-03-emuera-command-gap.md` §2, minus the 26 codes implemented 2026-09-03 (`STRLENU, STRCOUNT, STRJOIN, BARSTR, MONEYSTR, TOUPPER, TOLOWER, TOHALF, TOFULL, ISNUMERIC, CONVERT, INRANGE, LINEISEMPTY, GROUPMATCH, NOSAMES, ALLSAMES, ISSKIP, MOUSESKIP, MESSKIP, FINDELEMENT, FINDLASTELEMENT, CURRENTALIGN, CURRENTREDRAW, RAND, FIND_CHARADATA, EXISTCSV`) = **70 instructions still compiled to the `Throw` stub**.

---

## 1. Fall-through instructions — line-head statement usage in the corpus

**All 70 have zero occurrences (0 line-head statements, 0 distinct files).** The only any-position hits in the whole repository are 5 comment/changelog mentions:

| instruction | line-head occurrences | distinct files | example |
|---|---|---|---|
| `ADDSPCHARA` | 0 | 0 | — (only comment: `ERB/SYS/TITLE.ERB:56` `;다른 장면에서는 ADDCHARA, ADDSPCHARA 등을 사용하는 것을 추천합니다`) |
| `ADDVOIDCHARA` | 0 | 0 | — |
| `ARRAYMSORT` | 0 | 0 | — |
| `ASSERT` | 0 | 0 | — |
| `AWAIT` | 0 | 0 | — |
| `BARL` | 0 | 0 | — |
| `CALLEVENT` | 0 | 0 | — |
| `CHKCHARADATA` | 0 | 0 | — |
| `CHKVARDATA` | 0 | 0 | — |
| `CLEARTEXTBOX` | 0 | 0 | — |
| `DELALLCHARA` | 0 | 0 | — |
| `FINDLASTCHARA` | 0 | 0 | — |
| `FORCEKANA` | 0 | 0 | — |
| `GCLEAR` | 0 | 0 | — |
| `GCREATE` | 0 | 0 | — |
| `GCREATED` | 0 | 0 | — |
| `GDISPOSE` | 0 | 0 | — |
| `GDRAWG` | 0 | 0 | — |
| `GDRAWGWITHMASK` | 0 | 0 | — |
| `GDRAWSPRITE` | 0 | 0 | — |
| `GETTIMES` | 0 | 0 | — |
| `GGETCOLOR` | 0 | 0 | — |
| `GHEIGHT` | 0 | 0 | — |
| `GLOAD` | 0 | 0 | — |
| `GSAVE` | 0 | 0 | — |
| `GSETBRUSH` | 0 | 0 | — |
| `GSETCOLOR` | 0 | 0 | — |
| `GSETFONT` | 0 | 0 | — |
| `GSETPEN` | 0 | 0 | — |
| `GWIDTH` | 0 | 0 | — |
| `HTML_TAGSPLIT` | 0 | 0 | — |
| `INPUTMOUSEKEY` | 0 | 0 | — |
| `LOADCHARA` | 0 | 0 | — |
| `LOADGAME` | 0 | 0 | — (only comment: `ERB/SYS/SAVELOAD.erb:7` explains the game replaces it with `CALL LOADGAME_EX`) |
| `LOADTEXT` | 0 | 0 | — |
| `LOADVAR` | 0 | 0 | — |
| `OUTPUTLOG` | 0 | 0 | — |
| `PRINTCPERLINE` | 0 | 0 | — |
| `PRINT_ABL` | 0 | 0 | — |
| `PRINT_EXP` | 0 | 0 | — |
| `PRINT_IMG` | 0 | 0 | — |
| `PRINT_ITEM` | 0 | 0 | — |
| `PRINT_MARK` | 0 | 0 | — (only comment: `ERB/SYS/INFO/INFO2.ERB:71`) |
| `PRINT_PALAM` | 0 | 0 | — (only `.txt` changelog: `etc/개발 자료/2014신통합판 갱신로그.txt:446`) |
| `PRINT_RECT` | 0 | 0 | — |
| `PRINT_SHOPITEM` | 0 | 0 | — |
| `PRINT_SPACE` | 0 | 0 | — |
| `PRINT_TALENT` | 0 | 0 | — (only comment: `ERB/SYS/INFO/INFO2.ERB:31`) |
| `REF` | 0 | 0 | — |
| `REFBYNAME` | 0 | 0 | — |
| `RESETGLOBAL` | 0 | 0 | — |
| `SAVECHARA` | 0 | 0 | — |
| `SAVENOS` | 0 | 0 | — |
| `SAVETEXT` | 0 | 0 | — |
| `SAVEVAR` | 0 | 0 | — |
| `SPRITEDISPOSE` | 0 | 0 | — |
| `SPRITEHEIGHT` | 0 | 0 | — |
| `SPRITEMOVE` | 0 | 0 | — |
| `SPRITEPOSX` | 0 | 0 | — |
| `SPRITEPOSY` | 0 | 0 | — |
| `SPRITESETPOS` | 0 | 0 | — |
| `SPRITEWIDTH` | 0 | 0 | — |
| `STOPCALLTRAIN` | 0 | 0 | — |
| `STRDATA` | 0 | 0 | — |
| `TOOLTIP_SETCOLOR` | 0 | 0 | — |
| `TOOLTIP_SETDELAY` | 0 | 0 | — |
| `TOOLTIP_SETDURATION` | 0 | 0 | — |
| `TRYCALLLIST` | 0 | 0 | — |
| `TRYGOTOLIST` | 0 | 0 | — |
| `TRYJUMPLIST` | 0 | 0 | — |

---

## 2. Reachable runtime stubs — usage counts

| instruction | scope | occurrences | distinct files | example |
|---|---|---|---|---|
| `SPRITECREATE` | line-head | 0 | 0 | — |
| `ARRAYMOVE` | line-head | 0 | 0 | — |
| `SORTCHARA` | line-head | **14** | 1 | `ERB/SYS/CHARA_SORT.ERB:289` `SORTCHARA` (also `:292` `SORTCHARA NO, BACK`, `:297` `SORTCHARA ABL:(TFLAG:2)`) |
| `SAVECHARA` | line-head | 0 | 0 | — |
| `LOADCHARA` | line-head | 0 | 0 | — |
| `FORCEKANA` | line-head | 0 | 0 | — |
| `SPRITECREATED` | anywhere | 0 | 0 | — |
| `GCREATED` | anywhere | 0 | 0 | — |
| `MESSKIP` | anywhere | 0 | 0 | — |
| `MOUSESKIP` | anywhere | 0 | 0 | — |
| `FIND_CHARADATA` | anywhere | 0 | 0 | — |
| `CHKCHARADATA` | anywhere | 0 | 0 | — |
| `CHKFONT` | **anywhere** | **7** | 2 | line-head: `ERB/TORIKO_MODE/TORIKOMODE.erb:3295` `CHKFONT "Times New Roman"`; in-expression: `ERB/SYS/PANCTION.ERB:873` `SETFONT CHKFONT(ARGS) ? ARGS # LOCALS`, `:903` `IF CHKFONT("Symbol")` |
| `CURRENTREDRAW` | anywhere | 0 | 0 | — |
| `CURRENTALIGN` | anywhere | 0 | 0 | — |

> `SORTCHARA` is counted as line-head only (it is a statement command). `CHKFONT` is counted anywhere — it appears both as a statement (`CHKFONT "Times New Roman"` in `@HEARTMARK`/`@HEARTMARK_E`/`@HEARTMARK_L` in `TORIKOMODE.erb`) and inside expressions in `ERB/SYS/PANCTION.ERB@HEARTB` (`SETFONT CHKFONT(ARGS) ? ...`, `IF CHKFONT("Symbol")`).
> Note: `FIND_CHARADATA` appears in §1 too (as a fall-through line-head statement it has 0 occurrences); here it is counted anywhere-per-line and is also 0.

---

## 3. Priority assessment — which missing instructions block this game

**Answer: none of the 70 fall-through instructions block eraTHYMKR.** Every single one has zero line-head occurrences (and zero real any-position occurrences) in the actual game code. The game simply does not use any of them.

The two stubs that DO get exercised at runtime:

1. **`SORTCHARA` (command; executor `bail!("SORTCHARA")` at `crates/erars-vm/src/terminal_vm/executor.rs:2299`) — highest priority.**
   - 14 line-head statements, all in `ERB/SYS/CHARA_SORT.ERB`, inside `@CHARA_SORT_INPUT` (variants: `SORTCHARA`, `SORTCHARA NO, BACK`, `SORTCHARA ABL:(TFLAG:2)`, `SORTCHARA EXP:(TFLAG:2)`, `SORTCHARA ABL:(TFLAG:2), BACK`, `SORTCHARA EXP:(TFLAG:2), BACK`).
   - Hot path: `@CHARA_SORT_INPUT` is called from `@CHARA_SORT` (`CHARA_SORT.ERB:96`), which is called from **`@USERSHOP`** (`ERB/SYS/SHOP/SHOP.ERB:668`) — the in-game character-sort screen reachable from the shop. A user who enters the sort screen will hit the `bail!` immediately, killing the script with `SORTCHARA` unhandled.
   - It is a pure data reorder (no rendering); implementable client-side by changing `NO`/`ABL`/`EXP`/`BACK` ordering of the chara list.

2. **`CHKFONT` (method; executor no-op stub `// TODO: CHKFONT; ctx.push(0i64)` at `executor.rs:1405-1407`) — medium priority.**
   - 7 occurrences across 2 files. Line-head uses: `@HEARTMARK`/`@HEARTMARK_E`/`@HEARTMARK_L` in `TORIKOMODE.erb` (`CHKFONT "Times New Roman"` then `SIF RESULT` — currently `RESULT` is always 0, so the "font exists" branch never runs; the fallback branch does run, so execution continues rather than bailing).
   - Expression uses: `@HEARTB` in `PANCTION.ERB` (`SETFONT CHKFONT(ARGS) ? ARGS # LOCALS`, `IF CHKFONT("Symbol")`) — `@HEARTB`/`@HEARTBW` are shared heart-mark helpers called from `PANCTION.ERB` and `ERB/CHARA/072 코가사/EVENT_K72_A.ERB:819,4454`. Because it's a `0`-returning stub the wrong branch runs (`SETFONT LOCALS` instead of `SETFONT ARGS`, `IF` false), producing wrong font selection but no crash.
   - Imperfect but non-fatal: the game continues with a different font path.

3. Everything else (the whole 70-item §1 list, plus the other reachable stubs `SPRITECREATE`, `ARRAYMOVE`, `SAVECHARA`, `LOADCHARA`, `FORCEKANA`, `SPRITECREATED`, `GCREATED`, `MESSKIP`, `MOUSESKIP`, `CHKCHARADATA`, `CURRENTREDRAW`, `CURRENTALIGN`) — **0 occurrences**. Implementing them is strictly forward-compat work; none is needed for this game to run.

Other hot-path notes (context, all with 0 usage):
- Load/save is fully custom: the game defines `@LOADGAME_EX`/`@SAVEGAME_EX` (SAVELOAD.erb) and routes around the missing `LOADGAME`/`SAVEGAME` instructions by using `CALL LOADGAME_EX` / `CHKDATA` / `LOADDATA` / `SAVEGLOBAL` / `SAVEINFO_EX` — all of which erars already implements.
- No `CALL SORTCHARA` anywhere else; `@SYSTEM_TITLE` (TITLE.ERB), `@EVENTFIRST` (EVENT_K0.ERB), train events (`TRAIN_*`/`@COMF*`) and daily/first-run scripts (`DAILY_LIFE*`, `TRAIN_BEFORE.ERB`) contain none of the 70.