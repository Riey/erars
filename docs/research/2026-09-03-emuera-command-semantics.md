# Emuera Command Semantics Sheet — implementation-grade reference for unsupported instructions

Date: 2026-09-03
Purpose: exact-match reference for the implementer. NO source edits this turn.
Evidence hierarchy (searched in this order):
1. **`/home/riey/repos/eraTHYMKR/etc/개발 자료/`** — **no Emuera manuals/command references found.** Contents are game materials only: `EVENT_KX.ERB`, dialogue templates, `변수자료_eratohoYM자료.txt` (game variable notes), `2014신통합판 갱신로그.txt` (game changelog; mentions only file names like `PRINT_STATE.ERB`, no command semantics), plus idea/bug notes.
2. **Other docs in that repo** — `README.md`/`NOTICE.md` are game license/run instructions; `.github/ISSUE_TEMPLATE/*` are blank.templates; no command reference. **The official Emuera manual (`拡張文法.txt`/`基本文法.txt`, shipped with the Emuera distribution) is NOT vendored anywhere on this machine** (system-wide search found only `emuera.config` files and the game exe). Consequently, every claim below that relies on documented Emuera behaviour is marked **[UNVERIFIED]** unless it is backed by an erars source or by the corpus itself.
3. **erars sources as behavioural evidence** — cited throughout (`file:line` current as of 2026-09-03): `executor.rs` siblings, `compiler.rs` `default_arg_command`/`default_arg_method`, `inst.rs` Japanese comments (copied from Emuera 1824), `variable.rs`, `variable.yaml`, `save.rs`, `erars-ui VirtualConsole`, `erars-renderer font.rs`.
4. **Corpus usage** — the only two instructions the game actually hits (`SORTCHARA` ×14, `CHKFONT` ×7) are documented from the real call sites.

Conventions: "line-head form" = the instruction as the first word of a statement line; "method form" = value-returning functor usable inside expressions. `RESULT`/`RESULTS` = int/string function-return slots in `Var` (`crates/erars-vm/src/variable.rs`; `set_result`/`set_results` in `executor.rs:132-136`).

---

## Group 1 — Chara

| instruction | arg list (name, type, req/opt, default) | reads/writes | return / side effect | error behaviour | source |
|---|---|---|---|---|---|
| `ADDSPCHARA` | (1) キャラ番号 int required | chara list | appends a new chara whose `FLAG:0` is set to 1 ("SP chara") | — | inst.rs comment: `//(キャラ番号)のSPキャラクタを追加（フラグ0を1にして作成）` (fl. 113-114); sibling `ADDCHARA` proof: `executor.rs:2667-2677` (template lookup via `ctx.header_info.character_templates`, `ctx.var.add_chara()` + `set_character_template`); chara list = `Var.character_len`/`add_chara` `variable.rs:844-849`. FLAG:0-set step [UNVERIFIED — no erars path for it; reasoning: sibling `AddChara` exists, SP flag has no writer in erars yet] |
| `ADDVOIDCHARA` | (1) キャラ番号 int required (register no.) | chara list | appends a chara with no template settings (blank) | — | inst.rs `//変数に何の設定のないキャラを作成`; sibling `AddDefChara` `executor.rs:2685-2695` (adds chara, copies template 0 when present). Blank-creation path [UNVERIFIED — no erars code; reasoning: `add_chara` gives an uninitialised slot, so a void add = `add_chara()` without `set_character_template`] |
| `DELALLCHARA` | (none) | chara list | deletes every chara except MASTER | — | name + sibling `DelChara` `executor.rs:2722-2724` (`ctx.var.del_chara(idx)`); "except MASTER" [UNVERIFIED — no erars source; reasoning: Emuera keeps the player character] |
| `FINDLASTCHARA` | (1) 変数/値 (variable or string/char file name) | RESULT (int) | index of the last chara whose key equals the value, else -1 | — | sibling `FIND_CHARADATA` stub `executor.rs:1272-1274`; sibling `FINDCHARA` method `executor.rs` (`check_arg_count!(1,4)`, loop `start..end` returning first match, `ctx.push(ret)`) — layout of `FINDLASTCHARA` mirrors it in reverse [UNVERIFIED details — no erars code; FILO direction + -1 sentinel from FINDCHARA sibling] |
| `SORTCHARA` | (1) key: chara variable, opt, default `NO`; (2) `BACK` flag, opt, default forward | **reorders the chara list** (per-chara arrays) | sort ascending (or descending with `BACK`) | — | **fully documented from corpus + erars in the special section below** |
| `SAVECHARA` | (1) 番号 int required (+ optional キャラ番号) | chara data file | saves the chara(s)/character data to file | executor: `bail!("SAVECHARA")` `executor.rs:2795` | [UNVERIFIED — Emuera writes chara data files; erars has only `save.rs` slot writer (`write_save_data` `save.rs:126`), no chara-file layer] |
| `LOADCHARA` | (1) 番号 int required (+ optional キャラ番号) | chara data file → chara list | loads saved chara data | executor: `bail!("LOADCHARA")` `executor.rs:2796` | [UNVERIFIED details; same reasoning] |
| `CHKCHARADATA` | (1) 番号 int required | RESULT (int) | 1 if chara data exists else 0 | stub returns 1 always | `executor.rs:1276-1278`: `log::warn!("CHKCHARADATA"); ctx.push(1i64);` (dead arm — parser falls through; see gap-report §2) |

## Group 2 — Save / data

| instruction | arg list | reads/writes | return / side effect | error | source |
|---|---|---|---|---|---|
| `SAVETEXT` | (1) 文字列 string, (2) 番号/ファイル名 int-or-string | text file on disk | writes text to a data file slot | — | [UNVERIFIED — no erars analog; save dir is `ctx.sav_dir` (`executor.rs` SaveData/LoadData arms), so a text file would live there; Emuera: テキストセーブ] |
| `LOADTEXT` | (1) 番号/ファイル名 | RESULTS (string) | loads saved text into RESULTS | — | [UNVERIFIED — reasoning: paired with SAVETEXT; string return must use `RESULTS` since `store_result` routes strings there `executor.rs:132-136`] |
| `SAVEVAR` | (1) 変数 variable, (2) 番号 int, (3) 文字列 string opt | variable data file | writes one variable's data to a slot | — | [UNVERIFIED — Emuera SAVEVAR/LOADVAR/CHKVARDATA persist a single variable; erars serialisation primitive `Var::get_serializable` `variable.rs:258` already exists (used by SaveData)] |
| `LOADVAR` | (1) 変数 variable, (2) 番号 int | variable | reads one variable back | — | [UNVERIFIED — paired with SAVEVAR] |
| `CHKVARDATA` | (1) 番号 int | RESULT (int) | 0=exists, 1=missing (same convention as `CHKDATA`?) | — | sibling `CHKDATA` method `executor.rs:1962-1979` returns `(0=ok,2=code mismatch,3=old version,1=missing)` into RESULT and description into RESULTS — CHKVARDATA modelled on it [UNVERIFIED exact codes] |
| `RESETGLOBAL` | (none) | `is_global` variables | zeroes all global variables | — | sibling `RESETDATA` (`executor.rs:2726-2727` → `ctx.var.reset_data(&ctx.header_info)` `variable.rs:828-837`); global-only variant [UNVERIFIED — no `reset_global` fn; `is_global` exists in `VariableInfo` (`variable.yaml`/`erars-ast/src/variable.rs:65`)] |
| `SAVENOS` | — (arg spec differs: "引数の仕様が違うので(ry") | RESULT (int) | method form reads `ctx.config.save_nos` | — | inst.rs comment; method proof `executor.rs:1889-1891` (`BuiltinMethod::SaveNos => { let nos = ctx.config.save_nos; ctx.push(nos as i64); }`); line-head form [UNVERIFIED — it would push the same value, or set it if given an arg] |
| `LOADGAME` | (none; opens the load screen) | save slots | shows load UI, loads slot | fall-through → throws at runtime; **no line-head usage in corpus** (game uses `CALL LOADGAME_EX`) | sibling `LoadGame` command `executor.rs` (`run_load_game` + `run_load_data`) is implemented for `BuiltinCommand::LoadGame`; parser has NO `LOADGAME` arm (gap-report §2) |
| `GETTIMES` | (none) | RESULT (string) | date-time string `YYYY年MM月DD日 HH:MM:SS` | — | helper `fn get_times` `executor.rs:2828-2836` (used by method `GetTimeS`); instruction differs only in line-head vs method form [UNVERIFIED whether integers vs string — string variant evidenced] |

## Group 3 — Control flow

| instruction | arg list | reads/writes | return / side effect | error | source |
|---|---|---|---|---|---|
| `CALLEVENT` | (1) event name/type | — | fires an event function (`@EVENT*`) | — | `EventType` enum `erars-ast/src/event.rs:66-88` (`EVENTFIRST`…`EVENTTRAIN`); macro `call_event!` `executor.rs:59`; compiler emits `CallEvent` only from `Stmt::CallEvent` (`compiler.rs:364`) which the parser never produces ⇒ the instruction is inert/unwired [UNVERIFIED whether Emuera exposes it to scripts] |
| `STOPCALLTRAIN` | (none) | training machine | stops the current `CALLTRAIN` sequence | — | sibling `CALLTRAIN`/`DOTRAIN` → `run_call_train` `executor.rs:505-541` (loop over `SELECTCOM`, events `Com`/`ComEnd`, `CALLTRAINEND`); a stop flag/early-exit [UNVERIFIED — no erars field; reasoning: sibling loop has no interrupt path] |
| `TRYCALLLIST` | (n) function names, n≥1 | — (try-call each) | calls the first existing function in the list; RESULT false if none exists | — | sibling try-call machinery: `as_try_call`/`as_try_jump` `executor.rs:236-250` (`vm.try_call` returns `Some(Return)`/`None`; pushes bool) — a list variant picks the first existing target [UNVERIFIED details] |
| `TRYJUMPLIST` | (n) labels | — | jumps to the first existing label | — | same machinery, jump variant [UNVERIFIED details] |
| `TRYGOTOLIST` | (n) labels | — | `GOTO` to the first existing label | — | same machinery, goto variant [UNVERIFIED details] |
| `ASSERT` | (1) 条件 expression | — | if condition false → script error/abort | — | [UNVERIFIED — no erars trace; reasoning: Emuera aborts with an error, analogous to `Throw` arm `executor.rs:2248-2256`] |
| `REF` | as a call argument: `REF 変数` | — | passes a variable **by reference** into `CALLF`/`CALL` | — | **erars has only the `#DIM REF` declaration tag** (`parser/expr.rs:1161,1193`; `VariableInfo.is_ref` `erars-ast/src/variable.rs:65`, handled in `context.rs:105`, `terminal_vm.rs:158`). The instruction form (call-argument reference) has NO parser/compiler support [UNVERIFIED exact call-arg syntax] |
| `REFBYNAME` | as a call argument: `REFBYNAME "変数名"` | — | passes a reference to the variable whose **name** is the string | — | same as REF; no erars support [UNVERIFIED] |

## Group 4 — Print family

| instruction | arg list | reads/writes | return / side effect | error | source |
|---|---|---|---|---|---|
| `PRINT_ABL` | (1) 登録番号 int | console | prints the chara's ability list entry (status screen block) | — | inst.rs: `//能力。引数は登録番号` (fl. 96-103 for the family). Actual formatting [UNVERIFIED — previously these printed full status blocks; **unused in eraTHYMKR** (usage report §1)] |
| `PRINT_TALENT` | (1) 登録番号 int | console | prints talent line | — | same family comments; [UNVERIFIED details] |
| `PRINT_MARK` | (1) 登録番号 int | console | prints mark (刻印) line | — | [UNVERIFIED details] |
| `PRINT_EXP` | (1) 登録番号 int | console | prints experience line | — | [UNVERIFIED details] |
| `PRINT_PALAM` | (1) 登録番号 int | console | prints parameter (パラメータ) line | — | [UNVERIFIED details] |
| `PRINT_ITEM` | (1) 登録番号 int | console | prints carried-item line | — | [UNVERIFIED details] |
| `PRINT_SHOPITEM` | (1) 登録番号 int | console | prints shop-item line | — | [UNVERIFIED details] |
| `BARL` | (1) var int, (2) max int, (3) length int | console | like `BAR` then newline | — | sibling `BAR` `executor.rs:2326-2331` (`tx.print(make_bar_str(...))`); newline = `tx.new_line()` (`erars-ui/lib.rs:528`). inst.rs: `//改行付き。` |
| `PRINTCPERLINE` | arg spec differs: "よく考えたら引数の仕様違うや" | RESULT (int) | method form: `ctx.config.get_config(PrintcCount)` chars-per-line | — | inst.rs comment fl. 262-263; method `PrintCPerLine` `executor.rs` (`ctx.config.get_config(EraConfigKey::PrintcCount)`); line-head form [UNVERIFIED — likely sets the count] |
| `CLEARTEXTBOX` | (none) | console text box | clears the message box | — | sibling `CLEARLINE` → `tx.clear_line(c)` (`erars-ui/lib.rs:550`); whole-box clear [UNVERIFIED which rect/box] |
| `PRINT_IMG` | (1) 画像名 string (or built-in no.) | console/renderer | prints an image at the cursor | — | [UNVERIFIED — no image path in erars-ui; the renderer is remote (erars-renderer), VM console is text-only] |
| `PRINT_RECT` | (1) 文字 string, (2) width int, (3) height int? | console | fills a rectangle of repeated chars | — | [UNVERIFIED details] |
| `PRINT_SPACE` | (1) 数 int | console | prints n spaces | — | [UNVERIFIED — likely `tx.print(" ".repeat(n))` equivalent] |
| `HTML_TAGSPLIT` | (1) 文字列 string | RESULTS (string array?) | splits HTML into tags/text for `HTML_PRINT` | — | sibling `html_print` `executor.rs:1988-2061` (element/attr dispatch with warn-TODOs) [UNVERIFIED exact output shape] |
| `OUTPUTLOG` | (1) 文字列 string | log | writes a line to the log | — | erars logging = `flexi_logger` (`log::info!` used throughout, e.g. `executor.rs` SaveData arm `log::info!("Save {idx}: {description}")`) ⇒ `log::info!(args)` [UNVERIFIED whether Emuera writes to a specific file] |

## Group 5 — Misc

| instruction | arg list | reads/writes | return / side effect | error | source |
|---|---|---|---|---|---|
| `AWAIT` | (none) | — | yields to the system to process events (DoEvents), no input allow | — | inst.rs: `//入力不可 DoEvents` (fl. 45); input machinery: `SystemFunctions::input` `erars-vm/src/lib.rs:57-77`; a no-input yield ≈ `ctx.system.redraw(tx)` [UNVERIFIED exact] |
| `INPUTMOUSEKEY` | (none or 1) | RESULT | waits for mouse/key input, returns it | — | [UNVERIFIED — no mouse support in `InputRequestType` (`erars-ui/lib.rs:627-631`: AnyKey/EnterKey/ForceEnterKey/Int/Str) ⇒ needs new input kind] |
| `ARRAYMSORT` | (1) var array, (2..) multiple sort keys | array | sorts by **multiple** keys (vs single-key `ARRAYSORT`) | — | sibling `ArraySort` `executor.rs:2211-2242` (`arr.sort()`/`sort_by(b.cmp(a))` over `[start..end]`, str+int); `default_arg_command` handles `ArraySort` idx 2/3 `compiler.rs:761-763` (start=0, count=i64::MAX). Multi-key = repeated key list [UNVERIFIED exact arg order] |
| `ARRAYMOVE` | (1) var array, (2) src index, (3) dst index, (4) count | array | moves a slice within the array (`memmove`-like) | executor: `bail!("TODO: ARRAYMOVE")` `executor.rs:2245-2247` | [UNVERIFIED exact args; reasoning: the array family (`ARRAYSHIFT` `executor.rs:2128`, `ARRAYREMOVE` 2148, `ARRAYCOPY` 2163) all take (var, from, to, count)-ish shapes] |
| `FORCEKANA` | (1) on/off or (none) | print pipeline | forces kana-mode text on subsequent prints | executor: `log::error!("FORCEKANA is not implemented!")` `executor.rs:2703-2705` (arm is dead — parser falls through); print path has `PrintFlags::FORCE_KANA` handled as `log::error!("Unimplemented: FORCE_KANA")` `executor.rs:195-196` | [UNVERIFIED semantics; evidence: FORCE_KANA print flag exists but is a stub] |
| `STRDATA` | — (PRINTDATA context) | — | string-data block marker inside PRINCDATA | erars PRINTDATA parser rejects it: `"PRINTDATA에 잘못된 토큰이 들어왔습니다"` `parser.rs:1315,1325` | [UNVERIFIED whether Emuera 1.8.1.x has STRDATA; it exists in the instruction table copied from Emuera 1824 (`inst.rs:264`) but no handler] |
| `TOOLTIP_SETCOLOR` | (1) r int, (2) g int, (3) b int | tooltip render state | sets tooltip text colour | — | [UNVERIFIED details; no tooltip state in erars-ui (grep: none)] |
| `TOOLTIP_SETDELAY` | (1) ms int | tooltip render state | sets show delay | — | [UNVERIFIED details] |
| `TOOLTIP_SETDURATION` | (1) ms int | tooltip render state | sets display duration | — | [UNVERIFIED details] |
| `DEBUGPRINT` | (form string) | console (only when debug mode) | prints debug line | compiles to a no-op empty print | parser `DEBUGPRINT|DEBUGPRINTL|DEBUGPRINTFORM|DEBUGPRINTFORML|DEBUGCLEAR` `parser.rs:1400-1404` → `Stmt::Print(PrintFlags::empty(), "")` (`// TODO: debug`); executor DEBUG path early-returns `executor.rs:190-193` — **effectively no-ops today** |
| `DEBUGPRINTL` | (form string) | console | debug line + newline | no-op | same |
| `DEBUGPRINTFORM` | (form string) | console | debug form print | no-op | same |
| `DEBUGPRINTFORML` | (form string) | console | debug form + newline | no-op | same |
| `DEBUGCLEAR` | (none) | console | clears debug output | no-op | same |

## Group 6 — Stubs returning fake constants (what the true value depends on, and which erars state supplies it)

| instruction | current stub (executor.rs) | true value depends on | erars state that would supply it |
|---|---|---|---|
| `CHKFONT` | `// TODO: CHKFONT; ctx.push(0i64);` `:1742-1744` | whether the named font exists / is loadable | **fontdb enumeration exists**: `FontChain::new` calls `db.load_system_fonts()` (`crates/erars-renderer/src/font.rs:267-269`), `find_family(db, name) -> Option<fontdb::ID>` (`font.rs:119`) is exact-case-insensitive family lookup ⇒ true impl = `find_family(&db, name).is_some()`. **Blocker:** the `fontdb::Database` lives in `erars-renderer` (`FontChain`), NOT in `erars-vm` — VM has no font handle; needs plumbing (share the db or route through the renderer). `VirtualConsole::font()`/`set_font()` (`erars-ui/lib.rs:586-608`) track only the *current family name*, which cannot answer existence. |
| `CURRENTREDRAW` | `ctx.push(0i64);` `:1885-1887` | whether redraw is enabled | `ctx.system.redraw(tx)` (`SystemFunctions::redraw` `erars-vm/src/lib.rs:76`) is a *method call*, not state; **no redraw-enabled flag exists in the VM** (grep `redraw` in `context.rs`/`terminal_vm.rs` → only method calls). Requires adding a flag that `REDRAW`/`CURRENTREDRAW` would toggle. |
| `MESSKIP` | `// TODO:` `ctx.push(false)` (shared arm `MesSkip\|MouseSkip` `:1203-1206`) | whether message skipping (skip printing) is active | skip state exists: `VirtualConsole::set_skipdisp(bool)` / `skipdisp()` (`erars-ui/lib.rs:383-389`), driven by `SKIPDISP`/`NOSKIP`/`ENDNOSKIP` (`executor.rs:2523-2535`). MESSKIP is skip-for-messages specifically — could read a message-skip flag layered over `skipdisp` [UNVERIFIED exact] |
| `MOUSESKIP` | same shared arm | whether mouse-click skips | same skip state; no mouse support in `InputRequestType` ⇒ no mouse-skip source [UNVERIFIED] |
| `SPRITECREATED` | `ctx.push(0)` `:1111-1113` | whether a sprite with the given id exists | the whole `G*`/sprite family is unimplemented (no sprite store in the VM); `SPRITECREATE` (command) is also a trace-stub `executor.rs` — a sprite registry would need to be created first |
| `GCREATED` | `ctx.push(0)` `:934-936` | whether a CG/graphic resource exists | same — no CG store; renderer-side textures not enumerable from the VM |
| `FIND_CHARADATA` | `log::warn!("FIND_CHARADATA"); ctx.push(0i64);` `:1272-1274` | whether chara data (template) for the given name/number exists | `ctx.header_info.character_templates` (`HashMap` used by `AddChara` `executor.rs:2671`, `ExistCsv` method `:1956-1957` `contains_key`) — a real impl would look up the template map and return the chara index. NOTE: `FIND_CHARADATA` line-head is parse-fall-through (gap-report §2), so this arm is currently dead code. |
| `CHKCHARADATA` | `log::warn!("CHKCHARADATA"); ctx.push(1i64);` `:1276-1278` (dead) | whether saved chara data exists | no chara-save layer (SAVECHARA/LOADCHARA absent) ⇒ nothing to query yet |

---

## Special section A — SORTCHARA (the game's only real blocker)

### All 14 real call sites (quoted, `ERB/SYS/CHARA_SORT.ERB`, function `@CHARA_SORT_INPUT`)

```
289:  SORTCHARA                              ; "캐릭터 번호 오름차순으로 정렬" (chara NO ascending)
292:  SORTCHARA NO, BACK                     ; NO descending
297:  SORTCHARA ABL:(TFLAG:2)                ; ability key, ascending
300:  SORTCHARA ABL:(TFLAG:2), BACK          ; ability key, descending
305:  SORTCHARA EXP:(TFLAG:2)                ; experience key up
308:  SORTCHARA EXP:(TFLAG:2), BACK          ; down
(312/315): SORTCHARA MARK:(TFLAG:2)[, BACK]
(320/323): SORTCHARA JUEL:(TFLAG:2)[, BACK]
(328/331): SORTCHARA CFLAG:(TFLAG:2)[, BACK]
(336/339): SORTCHARA BASE:(TFLAG:2)[, BACK]
```
(14 = 2 bare + 12 keyed; verified by the line-head count of 14 in the usage report.)

### Syntax (from erars parser `sortchara_line`, `parser/expr.rs:1110-1141`)
- `SORTCHARA [key] [, BACK]` — `forward_or_back` (parses the literal `BACK`) may appear before or after the key; key omitted ⇒ variable `NO`; `BACK` omitted ⇒ forward/ascending.
- Compiled to `Stmt::Command(BuiltinCommand::SortChara, [Expr::Var(key), Expr::int(forward)])`.

### What `ABL:(TFLAG:2)` means (erars variable model)
- `ABL`, `EXP`, `MARK`, `BASE`, `CFLAG`, `JUEL` are **per-chara variables** (`is_chara: true` in `crates/erars-loader/src/variable.yaml`: NO `:168` size `[]`; CFLAG `:178` size `[10000]`; ABL `:193` `[1000]`; EXP `:208` `[1000]`; MARK `:223` `[1000]`; BASE `:231` `[100]`; JUEL `:352`).
- The sort key is a **character variable with its character index left free**: for each chara `c` in `0..character_len`, the key value = the variable read with `c` prepended to the explicit indices → `ABL:(TFLAG:2)` means `ABL[c][TFLAG:2]` (TFLAG:2 selects which ability/exp/mark/… column), `NO` means `NO[c]` (the chara register number, `NO` is 0D per-chara).
- `BACK` = descending; the printed message in the corpus says ascending messages are "낮은 순" (low→high) and `BACK` is "높은 순" (high→low), consistent.

### Which erars structure holds the order, and how to reorder
- The chara list order **is the index order** `0..Var.character_len` (`crates/erars-vm/src/variable.rs:438` `pub fn character_len`, `:45` field).
- Per-chara data lives in `Var.variables: HashMap<StrKey, (VariableInfo, UniformVariable)>`; reordering primitives already exist:
  - `Var::swap_chara(a, b)` `variable.rs:838-841` (swaps index `a`/`b` in **every** per-chara array) — the natural building block for a sort.
  - `Var::add_chara` `:844`, `del_chara(idx)` `:864`.
- Implementation shape: collect indices `0..character_len`, stable-sort by `read per-chara key[c]` (ascending / descending per `BACK`), apply `swap_chara` for each inversion (or read keys, then physically reorder). Key read uses the same path as array reads (`ctx.resolve_var_ref`/`read_var_ref` used by `ArraySort` `executor.rs:2211`).
- **Stability: not documented** — the corpus only needs a deterministic total order of distinct keys; `[UNVERIFIED]` whether Emuera's sort is stable (erars `ArraySort` uses `slice::sort`/`sort_by`, which are stable, `executor.rs:2229-2242` — mirroring that is a reasonable default).

## Special section B — CHKFONT (the game's only wrong-behaviour stub)

### Emuera semantics
- `CHKFONT(フォント名)` returns **1 if the named font is available, 0 otherwise** (used as `IF CHKFONT("Symbol")` and `SETFONT CHKFONT(ARGS) ? ARGS # LOCALS` in `ERB/SYS/PANCTION.ERB:873,903`; line-head form `CHKFONT "Times New Roman"` + `SIF RESULT` in `ERB/TORIKO_MODE/TORIKOMODE.erb:3295,3304,3314`). `[UNVERIFIED]` against the paper manual (not on disk), but the corpus usage proves the 0/1 predicate contract and that `RESULT` carries it.

### erars answer path (found in workspace)
- `erars-renderer` already enumerates fonts with **fontdb**: `FontChain::new` (`crates/erars-renderer/src/font.rs:265-290`) calls `db.load_system_fonts()` (`font.rs:269`), loads `<game>/font/` and `ERARS_FONT_DIR`, and registers a bundled `NotoSansMono-Regular.ttf` (`font.rs:21`).
- Family lookup: `find_family(db, name) -> Option<fontdb::ID>` (`font.rs:119-128`) — case-insensitive Unicode-lowercase compare over `FaceInfo.families` (name-ID 16 or 1); exactly the "does this font family exist" predicate CHKFONT needs.
- **The gap:** fontdb is a dependency used inside `erars-renderer` (not listed in its `Cargo.toml` directly — comes transitively, e.g. via `cosmic-text 0.12`), and the database lives in the renderer app, never in `erars-vm`/`erars-ui`. The VM's only font datum is `VirtualConsole::font()`/`set_font()` (`erars-ui/src/lib.rs:586-608`) — the *currently selected* family name, which cannot answer existence for an arbitrary name.
- To implement truly: expose a font-existence query from the renderer (`FontChain`/`fontdb::Database`) down to the VM (e.g. a `SystemFunctions`-side capability or a shared handle), then `CHKFONT name → push(find_family(db, name).is_some())`. Until that plumbing exists, any VM-side implementation is only a config/bundled-font approximation `[UNVERIFIED acceptable fallback]`.

---

## Coverage / confidence summary
- **Sources found:** no Emuera manual on disk (searched `etc/개발 자료/`, README/NOTICE/github templates, whole `~`). Protocol's item-3 evidence (erars code) fully collected with `file:line`.
- **fully evidenced:** SORTCHARA (parser+VM+corpus+variable.yaml), CHKFONT (corpus + font.rs), BARL (inst.rs + Bar arm), DEBUGPRINT family (parser+executor), AWAIT (inst.rs comment + SystemFunctions), FORCEKANA (executor), SAVENOS/PRINTCPERLINE methods, ADDSPCHARA/ADDVOIDCHARA (inst.rs + AddChara/AddDefChara siblings), TRYCALL* (try-call machinery), RESETGLOBAL (ResetData sibling), GETTIMES (get_times helper), ARRAYMSORT (ArraySort sibling + default args), Save/Load/ChkVAR-chara family (serialisation primitives exist, semantics [UNVERIFIED]).
- **[UNVERIFIED] with reasoning:** exact Emuera-manual behaviours for PRINT_* status printers, SAVETEXT/LOADTEXT/SAVEVAR/LOADVAR/CHKVARDATA argument lists and codes, STRDATA, REF/REFBYNAME call-arg syntax, tooltips, mouse input, sprite/CG store, sort stability.