# Emuera → erars implementation spec sheet (todo)

Source of truth: this file is **self-contained** — an implementer should not need to
re-read the wiki. It was generated 2026-09-03 from the mirrored pages in this directory
(`gap.md`/`index.md`), the WebEmuera C# tree at
`/tmp/webemuera/WebEmuera-master/WebEmuera/Emuera`, and live corpus usage in
`/home/riey/repos/eraTHYMKR` (thy, 873 ERB) and `/home/riey/repos/eramegaten_p_kr/Data` (meg, 8779 ERB).

Matching is case- and underscore-insensitive (`PRINT_ABL`≡`PRINTABL`). erars enum
variants are CamelCase; Emuera tokens are UPPERCASE.
**No code under `crates/` was changed to make this sheet.**

## Scope & exclusions

This covers every *genuinely-missing* wiki item **except** the graphics/input family assigned
to another agent. Excluded (see §7): `CBG*` (8), `GFILLRECTANGLE`, `SPRITEGETCOLOR`,
`GETKEY`, `GETKEYTRIGGERED`, `MOUSEX`, `MOUSEY`, `ISACTIVE`, `SETANIMETIMER`,
`SPRITEANIME*`, `GCREATEFROMFILE`.

Sorting: **live corpus usage desc, then behavioural impact** — read top-down for priority.

---

# 1. Missing in-expression functions (13) — all `absent` in erars (runtime-confirmed)

erars status legend: `absent` = no `BuiltinMethod` variant; the parser has no static arity
(runtime-validated), so each needs a `BuiltinMethod` variant + a `normal_method!` /
`strform_method!` parser arm + an executor arm. C# authority = WebEmuera `file:line`.

**Why these are absent despite parsing OK:** erars parses any unknown `NAME(...)` as a
deferred *user* method call (`Expr::Method(name, args)`, `parser/expr.rs:540`) and only
errors **at runtime when actually executed** — `Function NAME is not exists`
(`executor.rs:3091` via `vm.dic.get_func`). Raw parse acceptance is therefore NOT presence;
every function below was confirmed absent by executing it through the full parse→compile→
FunctionDic→`TerminalVm::start(@SYSTEM_TITLE)` path and observing the runtime
`Function NAME is not exists` error. If erars implemented them, the very same probe would
return a value instead.

**Reconciliation with corpus diagnostic totals (megaten 7 errors, 9 warnings; eraTHYMKR 0):**
usage counts below are *files containing the token* (grep), which overstates executed use.
Because unknown calls only fault when invoked, and `呼び出されなかった関数を無視する`
(IgnoreUncalledFunction) is default, the HTML_*/GETLINESTR/CHARATU sync-token usages that sit
in functions not reached on the start/title path contribute **no** error; the handful of
executed unresolved calls are the observed low error total. `CHARANUM`/`LINECOUNT` (meg 734/185)
do NOT error precisely because they ARE implemented (see §2.2) — this is consistent with the
low diagnostic count, and eraTHYMKR's 0 is consistent with its 0 usage of every item here.

Ordered by corpus usage (meg = sites in `eramegaten_p_kr/Data`, thy = sites in `eraTHYMKR`);
still to implement even if zero-meg for completeness (§1.9-1.13).

### 1.1 HTML_ESCAPE — meg 17, thy 0
- **wiki sig:** `str HTML_ESCAPE(str value)`
- **does:** escape a string for HTML (convert to character references); inverse is `HTML_TOPLAINTEXT`.
- **C#:** `GameData/Function/Creator.cs:148` (dispatch `HtmlEscapeMethod`), class `GameData/Function/Creator.Method.cs:5093`.
- **erars:** absent.

### 1.2 HTML_TOPLAINTEXT — meg 15, thy 0
- **wiki sig:** `str HTML_TOPLAINTEXT(str value)`
- **does:** convert an html string to plain text (strip `<…>` tags, expand character references).
- **C#:** `Creator.cs:147`, class `Creator.Method.cs:5080`.
- **erars:** absent.

### 1.3 GETLINESTR — meg 11, thy 0
- **wiki sig:** `str GETLINESTR(str letter)`
- **does:** return the string `CUSTOMDRAWLINE`/`DRAWLINEFORM` would display for the argument (line-draw filler resolution). Wik: "引数文字列をCUSTOMDRAWLINE、DRAWLINEFORMに渡した時に表示される文字列を返す".
- **C#:** `Creator.cs:137`, class `Creator.Method.cs:4822`.
- **erars:** absent.

### 1.4 CHARATU — meg 8, thy 0
- **wiki sig:** `str CHARATU(str s, int position = 0)`
- **does:** substring/char-extraction helper (private-mod version; returns part of `s` by character). Wik cites 私家改造版更新履歴.
- **C#:** `Creator.cs:136`, class `Creator.Method.cs:4804`.
- **erars:** absent.

### 1.5 HTML_GETPRINTEDSTR — meg 6, thy 0
- **wiki sig:** `str HTML_GETPRINTEDSTR(int lineNo)`
- **does:** return printed line `lineNo` as an html string (line numbering like `LINECOUNT`/`CLEARLINE`).
- **C#:** `Creator.cs:145`, class `Creator.Method.cs:5024`.
- **erars:** absent.

### 1.6 STRFORM — meg 3, thy 0
- **wiki sig:** `str STRFORM(str value)`
- **does:** treat the argument as a `PRINTFORM`-style formatted string and return the expanded result.
- **C#:** `Creator.cs:138`, class `Creator.Method.cs:4840`.
- **erars:** absent (only a stray comment mention in `compiler.rs`).

### 1.7 HTML_POPPRINTINGSTR — meg 1, thy 0
- **wiki sig:** `str HTML_POPPRINTINGSTR()`
- **does:** fetch the in-progress (waiting-for-linebreak) print buffer as html and clear it; no `<p>` (ALIGNMENT not applied).
- **C#:** `Creator.cs:146`, class `Creator.Method.cs:5062`.
- **erars:** absent.

### 1.8 COLOR_FROMRGB — meg 1, thy 0
- **wiki sig:** `int COLOR_FROMRGB(int r, int g, int b)`
- **does:** return `0xRRGGBB` from R,G,B; args outside 0–255 → error.
- **C#:** `Creator.cs:56`, class `Creator.Method.cs:2693`.
- **erars:** absent.

### 1.9–1.13 zero-corpus (still in wiki; implement for completeness)
| name | wiki sig | does (one line) | C# authority | erars |
|---|---|---|---|---|
| COLOR_FROMNAME | `int COLOR_FROMNAME(str colorname)` | resolve a colour *name* → `0xRRGGBB`, `-1` if unknown | `Creator.cs:55`, `Creator.Method.cs:2666` | absent |
| PRINTCLENGTH | `int PRINTCLENGTH()` | return config `PRINTCの文字数` (print-c width); NOT `PRINTCPERLINE` (count) | `Creator.cs:68`, `Creator.Method.cs:2810` | absent (only `PRINTCPERLINE` exists) |
| CBRT | `int CBRT(int value)` | cube root | `Creator.cs:82`, `Creator.Method.cs:3086` | absent |
| EXPONENT | `int EXPONENT(int value)` | exponential (e^x) | `Creator.cs:85`, `Creator.Method.cs:3148` | absent |
| GETSPCHARA | `int GETSPCHARA(int no)` | find SP-chara (CFLAG:0≠0) registration number, else -1 | `Creator.cs:19`, `Creator.Method.cs:2010` | absent (no SP-chara lookup) |

---

# 2. Missing variables (26 candidates) — only **6 genuine**; 20 already present

**Methodology (2026-09-03 revision):** the variable diff is the union of `variable.yaml` +
`KnownVariableNames` (`vm/variable.rs:1591`) + `BuiltinVariable` (`erars-ast/src/variable.rs`)
+ function-scoped `ARG/ARGS/LOCAL/LOCALS` + name-CSV tables. Every row below was then
**runtime-confirmed** by executing a real `PRINTFORM {VAR}` through the full parse→compile→
FunctionDic→`TerminalVm::start(@SYSTEM_TITLE)` path. `Variable X is not exists` at runtime =
absent; a printed value = present. (Parse acceptance alone is NOT presence — an unknown
variable is only rejected on the executed path.)

### 2.1 Genuinely missing (6) — implement
| variable | type | corpus (meg/thy) | does (wiki) | C# authority | erars status |
|---|---|---|---|---|---|
| CDFLAGNAME1 | str[] (name-CSV) | 0/0 | element names for `CDFLAG` dim1 from `cdflag1.csv` | `GameData/Variable/VariableCode.cs:223` | absent — erars merges `CDFLAG1`/`CDFLAG2` name tables (`loader/lib.rs:269`) but exposes no `CDFLAGNAME1` var (runtime: `Variable CDFLAGNAME1 is not exists`) |
| CDFLAGNAME2 | str[] (name-CSV) | 0/0 | element names for `CDFLAG` dim2 from `cdflag2.csv` | `VariableCode.cs:224` | absent (runtime: not exists) |
| GAMEBASE_GAMECODE | int (read-only) | 0/1 (thy) | numeric game code from `GameBase.csv コード` | `VariableCode.cs:258` | **naming gap** — erars exposes the *same value* as `GAMEBASE_CODE` (`BuiltinVariable::GamebaseCode`, `executor.rs:382`); the wiki/Emuera name `GAMEBASE_GAMECODE` is absent (runtime). Add `GAMEBASE_GAMECODE` alias onto the existing `gamebase.code` |
| ISTIMEOUT | int (read-only) | 0/0 | set to 1 when a `TINPUT`-family command times out | `VariableCode.cs:268` | absent — new read-only var wired to TINPUT-family timeout state |
| MONEYLABEL | str (read-only) | 0/0 | money unit string from `_replace.csv "お金の単位"`, default `$` | `VariableCode.cs:252` | absent — new read-only str backed by ReplaceInfo money-unit |
| TFLAGNAME | str[] (name-CSV) | 0/0 | element names for `TFLAG` from `tflag.csv` | `VariableCode.cs:217` | absent — erars loads `TFLAG` name-CSV (`get_ctx`/`merge_name_csv`) but has no `TFLAGNAME` var |

### 2.2 Already present — do NOT re-add (previous "missing" list was a variable.yaml-only false positive)
| name | erars source | runtime probe |
|---|---|---|
| CHARANUM | `BuiltinVariable::CharaNum`, `executor.rs:396` | RUNTIME-OK |
| LINECOUNT | `BuiltinVariable::LineCount`, `executor.rs:397` | RUNTIME-OK |
| RAND | `BuiltinVariable::Rand`, `executor.rs:398` (+ `CompatiRAND` config switch) | RUNTIME-OK |
| DRAWLINESTR | `BuiltinVariable::DrawLineStr`, `executor.rs:408` | RUNTIME-OK |
| GAMEBASE_AUTHOR/_INFO/_YEAR/_TITLE/_VERSION/_ALLOWVERSION/_DEFAULTCHARA/_NOITEM | `BuiltinVariable::Gamebase*`, `executor.rs:382-390` | RUNTIME-OK |
| LASTLOAD_VERSION/_NO/_TEXT | `BuiltinVariable::LastLoad*`, `executor.rs:392-394` | RUNTIME-OK |
| ARG/ARGS/LOCAL/LOCALS | function-scoped (parser + `KnownVariableNames`) | present (CORPUS-heavy: ARG meg 7294/thy 271) |

*Dropped as extraction junk:* `GAMEBASE` and `LASTLOAD_` bare prefixes, lowercase `gamebase`.

---

# 3. Missing `#` directive: `#ONLY`

- **wiki (exfunc) verbatim:** `#ONLY` — "イベント関数専用の属性です。ONLYが指定されたイベント関数がある場合、その1つのみが実行され、他の同名イベント関数を実行されません。また#ONLYが指定された同名イベント関数が複数ある場合も実行されるのは「最初の1つ」のみとなります。"
- **does:** among multiply-defined same-name **event** functions, only the first `#ONLY`-flagged one runs.
- **C# authority:** `GameProc/LogicalLineParser.cs:25` (token set) and `:109` (`case "ONLY"`); event-flag model in `GameData/GameBase.cs`/`Process.cs`.
- **erars status:** `absent` — `SharpCode` (`erars-lexer/src/sharp.rs:15-30`) has `DEFINE, DIM, DIMS, FUNCTION, FUNCTIONS, LOCALSIZE, LOCALSSIZE, PRI, LATER, SINGLE` but **no `ONLY`**. Add an `ONLY` variant + event-flag handling in `parser.rs` (near the `PRI`/`LATER`/`SINGLE` arms, ~2689-2691).
- **corpus:** attribute only occurs inside function headers (erb), usage not separately countable here.

---

# 4. Config keys (61 missing of 75 wiki headings) + 4 extra corpus keys

C# authority for defaults/English names: `Config/ConfigData.cs:50-132` (each
`configArray.Add(new ConfigItem<…>(ConfigCode.X, "和名", "English", default))`).
**erars currently implements ~22 keys** (`EraConfigKey`, `erars-compiler/src/parser.rs:214+`).
`●` = set in the corpus `emuera.config` files (meg = eramegaten, thy = eraTHYMKR).

## 4.1 BEHAVIOURAL — the ones that will actually be implemented (36)

Changes engine semantics; a headless/CLI engine can honour them.

| # | JP key (wiki) | English (ConfigCode) | Emuera default | corpus | what it changes |
|---|---|---|---|---|---|
| 1 | 大文字小文字の違いを無視する | IgnoreCase | true | ●meg ●thy | case-insensitive commands/vars |
| 2 | _Rename.csvを利用する | UseRenameFile | false | ●meg ●thy | apply `_rename.csv` rewriting at load |
| 3 | _Replace.csvを利用する | UseReplaceFile | true | ●meg ●thy | load `_replace.csv` (defaults/messages) |
| 4 | 使用するセーブデータ数 (a.k.a. 表示するセーブデータ数) | SaveDataNos | 20 | ●meg | save-slot count (`SAVENOS`) |
| 5 | オートセーブを行なう | AutoSave | true | ●meg | autosave at `BEGIN SHOP` |
| 6 | セーブデータをsavフォルダ内に作成する | UseSaveFolder | false | ●meg | save files under `sav/` |
| 7 | 無限ループ警告までのミリ秒数 | InfiniteLoopAlertTime | 5000 | ●meg | infinite-loop watchdog timeout |
| 8 | サブディレクトリを検索する | SearchSubdirectory | false | ●meg ●thy | recurse into CSV/ERB subfolders |
| 9 | 読み込み順をファイル名順にソートする | SortWithFilename | false | ●meg ●thy | deterministic load order |
| 10 | システム関数の上書きを許可する | AllowFunctionOverloading | true | ●meg ●thy | user fns may override built-ins |
| 11 | システム関数が上書きされたとき警告を表示する | WarnFunctionOverloading | true | ●meg ●thy | warn on override |
| 12 | 同名の非イベント関数が複数定義されたとき警告する | WarnNormalFunctionOverloading | false | ●meg | warn on duplicate non-event fn |
| 13 | 全角スペースをホワイトスペースに含める | SystemAllowFullSpace | true | ●meg | full-width space = whitespace |
| 14 | FORM中の三連記号を展開しない | SystemIgnoreTripleSymbol | false | ●meg | disable `///`/`+++` FORM substitution |
| 15 | セーブデータをバイナリ形式で保存する | SystemSaveInBinary | false | ●meg | binary vs text save format |
| 16 | セーブデータをUTF-8で保存する | SystemSaveInUTF8 | false | | save text encoding |
| 17 | ONEINPUT系命令でマウスによる2文字以上の入力を許可する | AllowLongInputByMouse | false | ●meg | ONEINPUT mouse multi-char |
| 18 | 解釈不能な行があっても実行する (corpus: 解釈不可能…) | CompatiErrorLine | false | ●meg | don't abort on unparsable lines |
| 19 | CALLNAMEが空文字列の時にNAMEを代入する | CompatiCALLNAME | false | ●meg | CALLNAME fallback to NAME |
| 20 | 擬似変数RANDの仕様をeramakerに合わせる | CompatiRAND | false | ●meg | eramaker RAND generation |
| 21 | 関数・属性については大文字小文字を無視しない | CompatiFunctionNoignoreCase | false | ●meg | case-sensitive fn/attr |
| 22 | イベント関数のCALLを許可する | CompatiCallEvent | false | ●meg | CALL on event functions |
| 23 | ver1739以前の非ボタン折り返しを再現する | CompatiLinefeedAs1739 | false | ●meg | pre-1.739 wrap compat |
| 24 | ユーザー関数の全ての引数の省略を許可する | CompatiFuncArgOptional | false | ●meg | omit non-ARG/ARGS/private args |
| 25 | ユーザー関数の引数に自動的にTOSTRを補完する | CompatiFuncArgAutoConvert | false | ●meg | auto int→str on fn args |
| 26 | eramaker互換性に関する警告を表示する | WarnBackCompatibility | true | ●meg | eran-maker compat warnings |
| 27 | ロード時にレポートを表示する | DisplayReport | false | ●meg ●thy | show load report |
| 28 | ロード時に引数を解析する | ReduceArgumentOnLoad | NO | ●meg ●thy | arg-parse on load (NO/ONCE/YES) |
| 29 | 表示する最低警告レベル | DisplayWarningLevel | 1 | ●meg ●thy | min warning level shown |
| 30 | 呼び出されなかった関数を無視する | IgnoreUncalledFunction | true | ●meg ●thy | skip uncalled-fn arg analysis |
| 31 | 関数が見つからない警告の扱い | FunctionNotFoundWarning | IGNORE | ●meg ●thy | IGNORE/LATER/ONCE/DISPLAY |
| 32 | 関数が呼び出されなかった警告の扱い | FunctionNotCalledWarning | IGNORE | ●meg ●thy | same selector |
| 33 | デバッグコマンドを使用する | UseDebugCommand | false | ●meg ●thy (thy=YES) | enable debug console |
| 34 | キーボードマクロを使用する | UseKeyMacro | true | ●meg ●thy | F1–F12 macros (host input) |
| 35 | （DRAWLINEを常に新しい行で行う）‑ obsolete | CompatiDRAWLINE | false | | superseded; ignore |
| 36 | ボタンの途中で行を折りかえさない | ButtonWrap | false | ●meg ●thy | console text/wrap (render-adjacent) |

**Extra behavioural keys set by the corpus but NOT on the wiki config page — add these too:**
| JP key | English (ConfigCode) | default | corpus | erars |
|---|---|---|---|---|
| TIMESの計算をeramakerにあわせる | TimesNotRigorousCalculation | false | ●meg | absent — affects `TIMES`/decimal math (`ConfigData.cs:124`) |
| キャラクタ変数の引数を補完しない | SystemNoTarget | false | ●meg | absent — disables TARGET auto-completion of `chara:var` (`ConfigData.cs:126`) |
| 文字列変数の代入に文字列式を強制する | SystemIgnoreStringSet | false | ●meg | absent — restrict string assignment to string-expr (`ConfigData.cs:127`) |
| SPキャラを使用する | CompatiSPChara | false | ●meg | **present** — erars `EraConfigKey::UseSpChara` (`parser.rs`) |

## 4.2 PRESENTATION / WINDOW / HOST — headless engine cannot honour (25)

Window/font/UI/console/appearance; record-and-ignore (or `GETCONFIG` passthrough only), do
not build behaviour on them. C# authorities: `ConfigData.cs:56-92` (window/font/color/FPS).

`マウスを使用する`●, `メニューを使用する`●, `多重起動を許可する`●, `関連づけるテキストエディタ`●,
`コマンドライン引数`, `ウィンドウの高さを可変にする`●, `起動時にウィンドウを最大化する`●(meg)/●(thy),
`起動時のウィンドウの位置を固定する`(=設定 `起動時のウィンドウ位置を指定する`)●(meg)/●(thy),
`ウィンドウ位置X`●(meg)/●(thy), `ウィンドウ位置Y`●(meg)/●(thy), `スクロールの行数`●,
`履歴文字色`●, `フレーム毎秒`●, `最大スキップフレーム数`●, `起動時にデバッグウインドウを表示する`,
`デバッグウインドウを最前面に表示する`, `デバッグウインドウ幅`, `デバッグウインドウ高さ`,
`デバッグウインドウ位置を指定する`, `デバッグウィンドウ位置X`, `デバッグウィンドウ位置Y`,
`（イメージバッファを使用する）`‑obsolete, `（描画にGDI+を用いる）`‑obsolete,
`（ロード時にFORM文字列を解析する）`‑obsolete, `（指定したファイル中の警告を無視する）`‑removed.

*(Note: `描画インターフェース`, `フォント名/サイズ`, `一行の高さ`, `文字色`, `背景色`, `選択中文字色`,
`内部で使用する東アジア言語`(=corpus sets KOREAN), `PRINTCを並べる数`, `PRINTCの文字数` are **already
implemented** in erars `EraConfigKey` — not in the 61.*)

---

# 5. Debug commands (5) — console/UI feature

Any normal ERB instruction/expression is also accepted as a debug command; these five are
console-only. C# authority: `GameView/EmueraConsole.cs:1343+` (debug-command dispatch) +
`GameProc/ErbLoader.cs`. **erars: no interactive debug console in the crates** — host-side work,
not a VM gap.

- `@REBOOT` — reread `emuera.config`, csv, erb (`EmueraConsole.cs:1343`)
- `@OUTPUT` — dump current log to `emuera.log` (same as `OUTPUTLOG`)
- `@EXIT` — quit (same as `QUIT`)
- `@CONFIG` — open settings dialog
- `@DEBUG` — open debug dialog (debug mode only)

---

# 6. Missing CSV columns / files

## 6.1 GameBase.csv — erars reads 10 keys; wiki documents the same 10 (no column gap)
Wiki (eramacsv) & erars (`parser.rs merge_gamebase_csv`, ~1007-1059) both: `コード`,
`バージョン`, `タイトル`, `作者`, `製作年`, `追加情報`, `最初からいるキャラ`, `アイテムなし`,
`バージョン違い認める`, `ウィンドウタイトル`. C#: `GameData/GameBase.cs:116-173`.
**Gap is variable-naming, not columns:** 8 of 9 `GAMEBASE_*` vars are already exposed
(`BuiltinVariable::Gamebase*`, §2.2); only **`GAMEBASE_GAMECODE`** is wrongly named `GAMEBASE_CODE`
in erars (naming-gap, §2.1). No new GameBase.csv columns needed.

## 6.2 Chara*.csv — wiki 16 columns vs erars 16 (all present)
Wiki columns: `番号(NO)/名前(NAME)/呼び名(CALLNAME)/あだ名(NICKNAME)/主人の呼び方(MASTERNAME)/
助手(ISASSI)/CSTR/素質(TALENT)/基礎(BASE)/刻印(MARK)/能力(ABL)/経験(EXP)/相性(RELATION)/
装着物(EQUIP)/珠(JUEL)/フラグ(CFLAG)`. erars `parser.rs merge_chara_csv` handles all 16
(plus `NO`/`番号` alias). C#: `GameData/ConstantData.cs:1409,1521-1607`.
**No column gap.** (The 15 "erars-not-wiki" in `gap.md` are the Emuera-only key *aliases* erars
additionally accepts, e.g. `MASTERNAME`, `EQUIP`, `JUEL` — not missing columns.)

### 6.3 `_replace.csv` keys (16) — wiki `replace` page, erars `ReplaceInfo`
Keys: `お金の単位`, `単位の位置`, `起動時簡略表示`, `販売アイテム数`, `DRAWLINE文字`, `BAR文字1`,
`BAR文字2`, `システムメニュー0`, `システムメニュー1`, `COM_ABLE初期値`, `汚れの初期値`,
`時間切れ表示`, `EXPLVの初期値`, `PALAMLVの初期値`, `PBANDの初期値`, `RELATIONの初期値`.
Check erars `ReplaceInfo` (parser.rs) covers all 16; C# authority `Config/ConfigData.cs`
(replaceArray section) — **verify, not confirmed in this pass.**

---

# 7. Assigned elsewhere (graphics/input family) — DO NOT implement here

`CBGCLEAR`, `CBGCLEARBUTTON`, `CBGREMOVEBMAP`, `CBGREMOVERANGE`, `CBGSETBMAPG`,
`CBGSETBUTTONSPRITE`, `CBGSETG`, `CBGSETSPRITE`, `GFILLRECTANGLE`, `SPRITEGETCOLOR`,
`GETKEY`, `GETKEYTRIGGERED`, `MOUSEX`, `MOUSEY`, `ISACTIVE`, `SETANIMETIMER`,
`SPRITEANIMECREATE`, `SPRITEANIMEADDFRAME`, `GCREATEFROMFILE`.

> **Correction (2026-09-03, aligned with gap.md):** `SETANIMETIMER`, `SPRITEANIMECREATE`,
> `SPRITEANIMEADDFRAME` and `GCREATEFROMFILE` are **already implemented** in erars
> (`BuiltinMethod::SetAnimeTimer/SpriteAnimeCreate/SpriteAnimeAddFrame/GCreateFromFile`,
> each with an executor arm) — they are **not gaps for anyone**, so they should not be
> "assigned elsewhere" as work; the upstream graphics doc treats their corpus usage as
> context (graphics-usage.md §2.2/2.3/3). The genuinely-missing assigned-elsewhere set is
> therefore only: the 8 `CBG*`, `GFILLRECTANGLE`, `SPRITEGETCOLOR`, `GETKEY`,
> `GETKEYTRIGGERED`, `MOUSEX`, `MOUSEY`, `ISACTIVE`. The §1 13-function list is unaffected
> (none of those overlap this set).

---

# Priority order (top of file = do first)

Note: §2.2 (CHARANUM, LINECOUNT, RAND, DRAWLINESTR, GAMEBASE_*, LASTLOAD_*, ARG/ARGS/LOCAL/LOCALS)
are **already implemented** — do NOT re-add them (2026-09-03 revision, runtime-confirmed).

1. **HTML_ESCAPE / HTML_TOPLAINTEXT / HTML_GETPRINTEDSTR / HTML_POPPRINTINGSTR** (§1, meg 17/15/6/1) — the only functions with live corpus usage; HTML_PRINT pipeline already in erars (`HtmlPrint`, `HtmlTagSplit`), so these slot in beside it.
2. **GETLINESTR** (§1.3) — meg 11; pairs with `DRAWLINE`/`CUSTOMDRAWLINE`.
3. **Config behavioural bucket §4.1** — start with `IgnoreCase`, `_Rename.csv`, `_Replace.csv`, `SystemAllowFullSpace`, `SystemSaveInBinary/UTF8` (save format), then the `Compati*` switches; all corpus-set ones first.
4. **GAMEBASE_GAMECODE naming fix** (§2.1) — erars exposes `GAMEBASE_CODE`; add the Emuera/wiki name `GAMEBASE_GAMECODE` as an alias onto the existing `gamebase.code` (meg 0, but 1 thy site uses `GAMEBASE_KEY`-style name).
5. **TFLAGNAME / CDFLAGNAME1 / CDFLAGNAME2** (§2.1) — name-CSV reader already merges `TFLAG`/`CDFLAG1`/`CDFLAG2` tables; expose them as `str[]` vars.
6. **ISTIMEOUT** (§2.1) — read-only int; wire to TINPUT-family timeout state.
7. **MONEYLABEL** (§2.1) — read-only str backed by ReplaceInfo money-unit (`お金の単位`, default `$`).
8. **Remaining zero-corpus functions** (§1.9-1.13): COLOR_FROMNAME, PRINTCLENGTH, CBRT, EXPONENT, GETSPCHARA, CHARATU, STRFORM, COLOR_FROMRGB.
9. **#ONLY** (§3) — events already modelled; small.
10. **Debug console + `@REBOOT` etc.** (§5) — host/CLI work, separate.