# Emuera Command / Function Gap Inventory — erars

Date: 2026-09-03 (originally an inventory; **rewritten after the gaps were closed**)
Method: sweeps over `erars-ast/src/command.rs`, `erars-compiler/src/{compiler,parser}.rs`,
`erars-lexer/src/{lib,inst}.rs`, `erars-vm/src/terminal_vm/executor.rs`; every claimed `file:line`
was verified by reading the bytes at that range. Emuera line numbers refer to the WebEmuera tree
(`WebEmuera/Emuera/`). Line numbers on both sides are as of the close of this work.

---

## 0. Enum cardinality

| Enum | File | Total variants | Highest discriminant |
|---|---|---|---|
| `BuiltinMethod` | `crates/erars-ast/src/command.rs` | **159** | 305 |
| `BuiltinCommand` | `crates/erars-ast/src/command.rs` | **116** | 336 |
| `InstructionCode` | `crates/erars-lexer/src/inst.rs` | **312** | — |

Counts are variant counts, not discriminant ranges: `BuiltinCommand`/`BuiltinMethod` carry
explicit discriminants with deliberate gaps so that appending never renumbers an existing
value. `BuiltinCommand` 301 is unassigned — it held `SpriteCreate`, which is a method in
Emuera (`SpriteCreateMethod`, `GameData/Function/Creator.cs:194`) and moved to
`BuiltinMethod`; the slot is left empty rather than reused.

Recounted after the coverage sweep of §3.1, which is why these differ from the 124 / 113 / 295 the
first pass of this document recorded: those predate the in-expression-function, variable and CSV
batches. Neither enum duplicates a discriminant, and 301 is still vacant.

---

## 1. Unimplemented at runtime: none

There is no `bail!("TODO: …")`, `todo!()`, `log::warn!("… not implemented")` or fake-constant arm
left in `crates/erars-vm/src/terminal_vm/executor.rs`. A sweep for
`TODO|todo!|not implemented|unimplemented` over that file returns nothing.

The stubs the original inventory listed were closed as follows.

| Was | Now |
|---|---|
| `SortChara` → `bail!("SORTCHARA")` | `sort_chara` (`executor.rs:1080-1158`) + `chara_order` (`:1049-1073`); reproduces `VariableEvaluator.SortChara` (`GameData/Variable/VariableEvaluator.cs:1190-1239`) including the `fixMaster` early return at `Count <= 2` and the non-negated pre-sort-index tiebreak of `CharacterData.Asc/DescCharacterComparison` (`GameData/Variable/CharacterData.cs:683-696`) |
| `ArrayMove` → `bail!("TODO: ARRAYMOVE")` | implemented, and flagged `DELIBERATE EXTENSION, NOT EMUERA` (`executor.rs:2944-2965`) — see §5 |
| `ForceKana` → `log::error!` no-op | `crates/erars-ui/src/kana.rs` + `tx.set_force_kana`; `PrintFlags::FORCE_KANA` applied only to the PRINTK family (`executor.rs:208-215`), matching `Instraction.Child.cs:149-150,229-230` |
| `SaveChara` / `LoadChara` → `bail!` | implemented; both were unreachable before because the parser had no arm |
| `FindCharaData` → `log::warn!` + push `0` | `crate::save::find_dat_files` (`save.rs:264`), wired at `executor.rs:1668` |
| `ChkCharaData` → push `1` | real check against the save directory |
| `MesSkip` / `MouseSkip` → push `false` | real message-skip state; `MOUSESKIP` additionally emits Emuera's deprecation warning `MOUSESKIP()は推奨されません。代わりにMESSKIP()を使用してください` (`parser.rs:2280-2287`) |
| `CurrentRedraw` / `GCreated` / `SpriteCreated` / `ChkFont` | real state reads |
| `Sign` returned `-v` | returns `sign(v)` |
| `MoneyStr` dropped its format-string argument | full `NumberToStringFormat` port, shared with `TOSTR` (`crates/erars-vm/src/dotnet_number.rs`; `:1-15` documents what it deliberately omits) |
| `html_print` handled a subset of tags | linear scanner in `crates/erars-vm/src/html.rs` mirroring `html2DisplayLine` (`GameView/HtmlManager.cs:452-632`), 7 unit tests |
| `BeginType::Title` → `todo!("Default TITLE")` | `run_default_title` (`executor.rs:647-705`), reproducing `beginTitle`'s built-in screen and `endOpenning`'s `[0]`/`[1]` branches (`GameProc/Process.SystemProc.cs:193-252`) |
| `PrintFlags::DEBUG` dropped the text | `tx.debug_print(s, newline)` (`executor.rs:197-205`), matching the flat `StringBuilder` of `EmueraConsole.cs:1837-1854` |

Two live `bail!`s remain and are correct, not stubs:

- `executor.rs:452-455` — `bail!("Unimplemented instruction: {inst:?}")` for a bytecode opcode no `as_*`
  arm claimed. This is unreachable from any source text; it guards a corrupt or forward-version
  `game.era`.
- `Throw` (`BuiltinCommand::Throw`) still exists, but nothing in the parser emits it any more.

---

## 2. `InstructionCode` fall-through: none

The blanket parser arm is **gone**. `match inst` (`crates/erars-compiler/src/parser.rs:2036`) is now
exhaustive over `InstructionCode`, which the compiler enforces: removing any arm is a build error.
The arm that used to swallow everything —

```rust
// TODO
inst => {
    log::warn!("{inst} is not yet implemented this line will occur error when executed.");
    Stmt::Command(BuiltinCommand::Throw, vec![Some(Expr::str(self.interner, format!("[compiler] TODO: {inst}")))])
}
```

— has been deleted. The 18 variants that reached it last are accounted for as follows.

### 2.1 One was a real gap: `STRDATA`

`STRDATA` is a **top-level statement**, not a `PRINTDATA` sub-block:
`addFunction(FunctionCode.STRDATA, argb[FunctionArgType.VAR_STR], …)`
(`GameProc/Function/FunctionIdentifier.cs:302`) with `funcMatch[STRDATA] = "ENDDATA"` (`:460`). It opens
its own `DATA`/`DATAFORM`/`DATALIST` block, draws one entry at random, joins that entry's parts with
`"\n"`, and assigns the result to a changeable string variable defaulting to `RESULTS:0`
(`GameProc/Function/ArgumentBuilder.cs:1640-1648`, `GameProc/Process.ScriptProc.cs:750-774`). An empty
block assigns nothing at all (`:752-757`).

erars now has `Stmt::StrData(Variable, Vec<Vec<Expr>>)` (`crates/erars-ast/src/ast.rs:36-41`), parsed at
`parser.rs:2666-2697` over the shared `read_data_block` (`parser.rs:1821-1906`) and compiled at
`compiler.rs:498-517`.

### 2.2 Seventeen were block delimiters, and are now diagnosed

`CASE`, `CASEELSE`, `CATCH`, `DATA`, `DATAFORM`, `DATALIST`, `ELSE`, `ELSEIF`, `ENDCATCH`, `ENDDATA`,
`ENDIF`, `ENDLIST`, `ENDSELECT`, `LOOP`, `NEXT`, `REND`, `WEND` are consumed by their own block parsers
in the normal case. Reaching the top-level `match inst` means the delimiter is **stray**, which Emuera
reports as a fatal load error — `ParserMediator.Warn(…, 2, true, false)`, level 2
(`GameProc/ErbLoader.cs:1063-1420`). erars now emits Emuera's own message for each
(`parser.rs:2699-2756`), verified by loading one stray delimiter per ERB file:

| Stray | Message |
|---|---|
| `ELSE`, `ELSEIF` | `IF～ENDIFの外で"{inst}"文が使われました` |
| `ENDIF` | `対応するIFの無いENDIF文です` |
| `CASE`, `CASEELSE` | `SELECTCASE構文の分岐の外に命令"{inst}"が含まれています` |
| `ENDSELECT` | `対応するSELECTCASEの無いENDSELECT文です` |
| `REND`, `NEXT`, `WEND`, `LOOP` | `対応する"{REPEAT\|FOR\|WHILE\|DO}"の無い"{inst}"文です` (pairing from `FunctionIdentifier.cs:465-468`) |
| `CATCH` | `対応するTRYC系命令がありません` |
| `ENDCATCH` | `対応するCATCHのないENDCATCHです` |
| `DATALIST` | `対応するPRINTDATA系命令のないDATALISTです` |
| `ENDLIST` | `対応するDATALISTのないENDLISTです` |
| `DATA`, `DATAFORM` | `対応するPRINTDATA系命令のない"{inst}"です` |
| `ENDDATA` | `対応するPRINTDATA系命令もしくはSTRDATAのない"{inst}"です` |

The three nesting arrangements Emuera rejects (`_Library/EvilMask/Lang.cs:822-824`) are rejected too:
`STRDATA命令が入れ子にされています`, `PRINTDATA系命令の中にSTRDATA系命令が含まれています`,
`STRDATA系命令の中にPRINTDATA系命令が含まれています`.

### 2.3 One invented name was removed

`FINDCHARADATA` (no underscore) had a parser arm but appears **nowhere** in the Emuera source — the real
name is `FIND_CHARADATA` (`GameData/Function/Creator.cs:63`) — and nowhere in the eraTHYMKR corpus.
Accepting a spelling Emuera rejects is a silent divergence, so the `InstructionCode` variant and its
parser arm were deleted rather than documented.

### 2.4 Names rejected outright (unchanged)

A line-head word absent from `InstructionCode`, not a `PRINT*` form and not an assignment / `++` / `--` /
`#` / `@` / `$` line is rejected by the lexer: `[lexer] Unknown line: {line}`
(`crates/erars-lexer/src/lib.rs:845`, and `:912` for a line inside a function), or
`[lexer] Unknown sharp line: {line}` (`lib.rs:826`).

---

## 3. Coverage

| | Count |
|---|---:|
| `BuiltinCommand` variants with a real executor arm | **116 / 116** |
| `BuiltinMethod` variants with a real executor arm | **159 / 159** |
| `InstructionCode` variants with an explicit parser arm | **304 / 304** (exhaustive `match`, compiler-enforced) |
| `InstructionCode` variants compiled only to a `Throw` stub | **0** |
| Executor `bail!("TODO")` / `todo!()` / warn-only / fake-constant arms | **0** |

`BuiltinMethod` grew past the 136 of the first pass with the eight wiki-gap functions of §5.14 and
the renderer owner's graphics additions; every variant is named by an arm, and the only `_ =>` arms
in the executor belong to save-menu integer matches, not to builtin dispatch.

Verification at the close of this work:

- `cargo build --all` and `cargo check --all --all-targets` → 0 errors.
- `cargo test --all` → 36 suites, **423 passed, 0 failed**.
- `cargo test -p erars --test run_tests` → **132 `[o]`, 0 `[x]`**.
- eraTHYMKR corpus, shipped tree and shipped `emuera.config` → **0 diagnostics**. The game sets
  `表示する最低警告レベル:2`, which hides its 26 level-1 preprocessor warnings in Emuera as well
  (§5.15). Copying the tree with the key set to `1` reproduces exactly those 26, matched site by
  site against an independent `PPState` port (§5.12): 21 × `UnexpectedSkipend` + 5 ×
  `DuplicateSkipstart`. 16,859 functions and 334,738 interner entries, both unchanged from pristine
  `HEAD` in default, `--debug` and neutralised-marker builds alike.
- eraMegaten corpus → 125,548 functions and the 20 diagnostics §6 accounts for one by one
  (7 `E2000`, 9 `W1001`, 4 `W2000`); the game sets level `0`, so nothing is filtered and the counts
  are unchanged by the level work.
  Interner entries are **366,016**, 81 below the 366,097 of pristine `HEAD`: those 81 strings were
  the arguments of the 90 debug-print/`ASSERT` lines the release path no longer parses (§5.12).
  Measured on the same tree with the same config, `PHASES_DEBUG=1` restores them and adds 44 more
  (34 from the 100 `;#;` markers, 10 from the 27 `[IF_DEBUG]` regions), which is the whole delta.
- **One transient interner allocation failure, recorded so a second sighting is not a first one.**
  An eraTHYMKR release smoke run panicked in three parse worker threads at once with
  `lasso::LassoError { kind: FailedAllocation }` (`threaded_rodeo.rs:290`, the arena asking the
  global allocator for a bucket and being refused), which the `main` thread reported as the join
  `unwrap` at `crates/erars-stdio/src/main.rs:153`. It happened on the first run after a
  `cargo build --release` — a moment of real memory pressure — and did not reproduce: 11
  consecutive clean runs of that same binary afterwards, and 10 before it, so it is filed as an
  allocator failure and not as an interner bug. Date: 2026-09-03.

### 3.1 Runtime coverage sweep of the wiki index

The table above counts *arms*. An arm can exist and still never be reachable, and — the sharper
risk — a name with **no** implementation still parses: an unknown `NAME(...)` falls through to
`Expr::Method` (`crates/erars-compiler/src/parser/expr.rs:540`) and only faults when executed. So
parse-time acceptance is not evidence of anything. `tests/wiki_coverage.rs` measures the other side:
for every name in `docs/research/emuera-wiki/index.md` it generates a minimal use, drives it through
a real `TerminalVm`, and classifies the outcome as **absent** (nothing resolved the name), **present**
(the name resolved but the probe could not complete it — an arity or type error) or **ran**.

    cargo test -p erars --test wiki_coverage -- --nocapture --test-threads=1

writes `docs/research/emuera-wiki/coverage.md`, a row per name with the probe, the stage it reached
and the observed text. Four properties of the harness are what make the numbers mean anything:

- **A function is never probed as `RESULTS = NAME(...)`.** Assignment to a string variable takes its
  right side as a raw FORM literal, in erars and in Emuera alike, so *every* unknown name "works"
  that way. Every function rung is an evaluating FORM slot instead, and `harness_controls` pins the
  quirk so it cannot come back. The same quirk produced three false `present`/`absent` verdicts in the
  first sweep — `#DIMS`, `#LOCALSSIZE` and the five string CSV index-name rows — all four traced to
  the probe, not to erars.
- **"Accepted and silently ignored" counts as absent** for a `#` directive, a `[…]` code or a CSV
  merge, because erars only *warns* on an unknown square code: silent acceptance is the absence
  signature. A probe therefore carries a marker its output must contain.
- **A panic is `present` and also a hard failure.** `catch_unwind` plus a silent hook collects them
  into their own report section, and the test asserts the set is empty.
- **Wiki-extraction noise is `n/a`, never a gap** (prose fragments the index lists beside real
  names), and the final assertion is a subset check, so it keeps passing while the image family of
  §5.11 lands in a concurrent session.

| Index section | Rows | ran | present | absent | n/a |
|---|---:|---:|---:|---:|---:|
| (a) Instructions | 355 | 341 | 4 | **0** | 10 |
| (b) In-expression functions | 152 | 139 | 13 | **0** | 0 |
| (c) Variables and constants | 119 | 118 | 1 | **0** | 0 |
| (d) Preprocessor directives | 16 | 15 | 1 | **0** | 0 |
| (e) `emuera.config` keys | 75 | 48 | 0 | **0** | 27 |
| (f) Debug commands | 5 | 5 | 0 | **0** | 0 |
| (g) CSV files and column layouts | 82 | 82 | 0 | **0** | 0 |

**No name in the index is absent at runtime.** Section (a)'s single `present` row that is not owned
elsewhere is `THROW`, whose contract *is* to raise; the other three are
`CBGSETSPRITE`/`CBGSETBUTTONSPRITE`/`GFILLRECTANGLE`, deliberately undriven because §5.11 owns them.
(d)'s row is `#DEFINE`, which is ERH-only by design.

**Panicking probes: 0.** Seven panic classes existed when the harness first ran and every one is now
a script error, each carrying Emuera's own check: a bare `{RAND}`
(`GameData/Variable/VariableParser.cs:170-177`), `LOADDATA` on a missing slot
(`GameProc/Process.ScriptProc.cs:814-828`), `CMATCH` with a defaulted range
(`Creator.Method.cs:3308`, `:3364-3367`), `CVARSET`'s dropped fifth argument
(`Instraction.Child.cs:1472-1501`), `SWAPCHARA` out of range
(`VariableEvaluator.cs:1179-1183`), `RESET_STAIN` out of range (`:1664-1670`) and `CUPCHECK` out of
range (`:1594-1599`). Where Emuera catches these at compile time and erars at run time, the
divergence is the timing only, and it is the same one §5.7 already records.

**The `present` verdict is the one that can hide an absent arm**, so section (a)'s 54 of them were
not left as arity errors. Each was a name the blind argument ladder cannot complete — it needs a
callee, a label, a live character, a matching block terminator, an answered input request or a
specific literal — and each now gets one hand-built call that has to run and print
(`tests/wiki_coverage.rs:597-863`), with the blind ladder still appended after it so a wrong
hand-built shape degrades to the old evidence instead of inventing a gap. **50 of the 54 moved
`present` → `ran`.** `LOADDATA` is driven by a real save round-trip: `SAVEDATA` then `LOADDATA`,
with the marker printed from `@EVENTSHOP`, because a successful load ends in
`Workflow::Begin(BeginType::Shop)` (`crates/erars-vm/src/terminal_vm/executor.rs:555`) whose
`SHOW_SHOP` input loop never returns (`:851-879`).

**The exercise paid for itself: one of the 54 was a broken arm, not a probe artefact.** `SKIPDISP`
failed on *every* call, well-formed or not, with 「다른 함수의 스택을 침범했습니다」.
`run_builtin_command` pops the argument count and moves every argument off the value stack with
`take_list(c)` (`:2954-2955`); the `SkipDisp` arm then called `ctx.pop_int()` directly, so it read
the **caller's** frame and left its own argument behind — corrupting the stack on every invocation.
It also set `RESULT` to a constant `0`, where Emuera sets it to the new skip state
(`GameProc/Process.ScriptProc.cs:571-578`, one required `INT_EXPRESSION` per
`GameProc/Function/FunctionIdentifier.cs:317`). Fixed at `:3709-3724` and pinned by
`tests/run_tests/basic/skipdisp.erb` (`RESULT` = 1 after `SKIPDISP 1`, 0 after `SKIPDISP 0`, 1 after
`SKIPDISP 7`, `ISSKIP()` agreeing, and prints between them suppressed) plus
`skipdisp_arity.erb` for the new arity error. The fixture passing also means the harness'
end-of-run "stack is empty" check passed, which is the part the sweep could not assert on its own.

**Section (e) never counts a refusal as a gap.** Emuera's `GETCONFIG` reaches 26 keys and refuses
every other name outright, so a refusal on a non-reachable key is *correct* and is reported `n/a`;
what the sweep collects instead is the opposite error, a **success** on a key Emuera refuses. It
finds 36, which is the superset §5.15 records.

**Section (g) found the second real gap, and it was a missing table rather than a missing command.**
Reading every merged value back through a running VM left one row failing: `RELATION:0:<charaname>`
reported `Variable ZZIDX is not exists`. Emuera builds a reverse dictionary from every chara
template's Name, Callname and Nickname to that template's `No`, first definition winning
(`GameData/ConstantData.cs:690-700`), and hands the same dictionary to `VariableCode.RELATION` at
`allowIndex 1` and to `VariableCode.NAME` at `allowIndex -1`, both reporting `chara*.csv` as the
error position (`:1061-1070`). erars had no such table at all, so a character name was never usable
as a RELATION target index. Built in `merge_chara_csv`
(`crates/erars-compiler/src/parser.rs:1638-1661`): the names go into `var_names` under both
`RELATION` and `NAME`, `or_insert` giving Emuera's first-wins, and a `No` outside `u32` is skipped
rather than wrapped because such a character is not addressable by index either. Emuera runs the
pass once after every chara file has loaded, walking `CharacterTmplList` in load order; merging per
file in the same order picks the same winner. Pinned by
`tests/run_tests/basic/relation_charaname.erb`, which writes through the name and reads back through
the number, then the reverse, using the repo's own `CSV/CHARA3.CSV` (`番号,3` / `名前,이름`).

With that in and the `ITEMPRICE` row corrected — it is `__UNCHANGEABLE__` in Emuera too
(`GameData/Variable/VariableCode.cs:96`, an `Int1DConstantToken` at
`GameData/Variable/VariableData.cs:259`), so the probe now reads the CSV's own price back through the
name instead of writing it — section (g) is **82 of 82 ran**.

**Section (f) was the last absence, and closing it needed the probe to get sharper.** Emuera's
`doSystemCommand` **echoes** the command line before acting on it
(`GameView/EmueraConsole.cs:1336-1338`), so the command text appears in the console output of a
working console too, and the first sweep after the implementation landed still called all five
absent. The evidence is what the *script* received, not what the console shows: each probe now
prints `RESULTS=[…]` and requires the tag, so `@CONFIG`/`@DEBUG`/`@OUTPUT` must show the answer to
the **re-issued** request and `@EXIT`/`@REBOOT` must end the run before the statement after the
`INPUTS`. That fix exposed a second harness defect worth recording: the flattened console text was
truncated to 160 characters *before* being classified, so any probe whose evidence landed past that
point was reported absent — which is what `@CONFIG`, printing the whole config, did. Truncation now
belongs to the report table alone (`clip`, `tests/wiki_coverage.rs`). §5.16 states what each command
does and the four places its effect still differs from the Windows Forms original.

One over-acceptance the harness cannot see past, stated because it bounds the result: `parse_print_left`
discards the unparsed remainder after flag parsing (`crates/erars-lexer/src/utils.rs:287-318`), so
`PRINTZZNOSUCH` is silently accepted as a plain `PRINT`, where Emuera enumerates every PRINT variant
as its own `FunctionCode` and rejects the rest. **No `PRINT*` name can be proved absent by this
harness**; `harness_controls` asserts the over-acceptance so the limitation stays visible.

---

## 4. How dispatch works (adding a new command)

1. **Lexer** — `crates/erars-lexer/src/lib.rs:756-801`. For each line, `cut_ident(line)` splits the head
   word; `let inst = self.inst_memo.get(&ident_upper);` (`lib.rs:776`) looks it up in the `InstructionCode`
   phf map. A hit yields `EraLine::InstLine { inst, args }` (`lib.rs:801`). Misses fall through to PRINT /
   assignment / sharp / function / goto handling, else `[lexer] Unknown line:` (`lib.rs:845`).
   → **To add a command you must first add a `InstructionCode` variant** (`crates/erars-lexer/src/inst.rs`;
   the phf map is auto-derived via `strum::EnumString`).

2. **Parser** — `crates/erars-compiler/src/parser.rs:2036` `match inst` over `InstructionCode`. Each
   variant maps to a `Stmt`: `normal_command!(BuiltinCommand::X)` or `normal_method!(BuiltinMethod::Y)`
   (macros at `parser.rs:2012-2035`). The match is **exhaustive**, so a new `InstructionCode` variant
   without an arm is a build error rather than a silent fall-through. → **Add a `match` arm here.**

3. **AST** — `crates/erars-ast/src/command.rs`. For a statement command add a `BuiltinCommand` variant; for
   a value-returning function add a `BuiltinMethod` variant. (Both enums are `strum`-derived from their
   variant names, so the spellings in `inst.rs` and `command.rs` must agree for compile-time routing.)

4. **Compiler** — `crates/erars-compiler/src/compiler.rs:788`. `Stmt::Command(command, args)` pushes
   `Instruction::load_int(count)` then `Instruction::builtin_command(command)`; `Stmt::Method(meth, args)`
   pushes the same plus `Instruction::store_result()`. The opcodes are declared in
   `crates/erars-compiler/src/instruction.rs:203,206` (macro `builtin_command`/`builtin_method`).
   Optional/default args are resolved by `default_arg_command`/`default_arg_method`
   (`compiler.rs:896,864`).

5. **Bytecode → Executor** — `crates/erars-vm/src/terminal_vm/executor.rs:418-421`:
   `inst.as_builtin_method()` → `run_builtin_method` (`executor.rs:1160`), `inst.as_builtin_command()` →
   `run_builtin_command` (`executor.rs:2542`). The bytecode carries the `BuiltinCommand`/`BuiltinMethod`
   enum value directly, so there is no name resolution at runtime; the executor arm must be present or
   the enum-wide `match com`/`match meth` fails to compile.

6. **Fallback safety net** — any bytecode opcode that no `as_*` arm matches hits `executor.rs:452-455`
   `bail!("Unimplemented instruction: {inst:?}")`. Unreachable from source text; it guards a corrupt or
   forward-version `game.era`.

Short form of the path: **source line → `cut_ident` → `InstructionCode` (lexer `lib.rs:756-801`) →
`EraLine::InstLine` → `match inst` (parser `parser.rs:2036`) → `Stmt::Command/Method` → bytecode
`builtin_command`/`builtin_method` (compiler `compiler.rs:788`) → `as_builtin_command/method`
(executor `executor.rs:418-421`) → `run_builtin_command`/`run_builtin_method` arm.**

---

## 5. Deliberate deviations from Emuera

Every entry below is a conscious decision, not a gap. Each is also marked in the source with a
`DELIBERATE …` comment naming the Emuera line and the reason; this section is the single index.

### 5.1 No pixel surface for *shapes*

`erars_ui::TextStyle` carries colour, font family and font-style bits only, and `VirtualConsole` is a
list of styled text lines plus, since the image layer landed, `ConsoleLinePart::Image`. Emuera's
*shape* primitives still have to be approximated; its *image* primitives no longer do — those moved
to §5.11.

| Deviation | erars | Emuera |
|---|---|---|
| `PRINT_SPACE` measures in character cells, not percent-of-line-height | `executor.rs:3499-3510` (`SHAPE_CELL_LIMIT = 4096`, `:4093`) | `ConsoleShapePart.cs:56-62`, `Instraction.Child.cs:375-399` |
| `PRINT_RECT` fills with U+2588 cells; `width` is a cell count, `x` a leading blank run, `y`/`h` dropped | `executor.rs:3511-3546` | `ConsoleShapePart.cs:88-92`, `:198-204` |
| `PRINT_IMG` and `HTML_PRINT` `<img>` draw real pixels; the remaining differences are in §5.11 | `executor.rs`, `html.rs`, `erars-ui/src/image.rs` | `Instraction.Child.cs:326-336`, `HtmlManager.cs:1003-1066` |
| `HTML_PRINT` `<shape>` renders nothing; `<div>` lays out and draws as a positioned box, with `radius`/`depth` parsed but not honoured and its validation relaxed (§5.11) | `html.rs`, `erars-ui/src/div.rs`, `erars-renderer/src/{layout,draw}.rs` | `HtmlManager.cs:1068-1257`, `_Library/EvilMask/ConsoleDivPart.cs` |
| `HTML_PRINT` `<font bcolor>` validated and dropped (no button focus colour in `TextStyle`) | `html.rs:29-30` | `HtmlManager.cs:1385-1440` |
| `GDRAWG` scales nearest-neighbour | `draw_g` (`graphics.rs:377-380`), blit `graphics.rs:877`, test `graphics.rs:1079` | GDI+ `DrawImage` interpolation |
| `GSETBRUSH` / `GSETPEN` / `GSETFONT` record state only | `graphics.rs:344,355,366`, called from `executor.rs:1376,1383,1391` | GDI+ object creation |
| `CLEARTEXTBOX` is a documented no-op — erars input is a request/response round trip, so no partial input exists to discard | `executor.rs:3483-3498` | `MainWindow.cs:1071-1074` edit box |
| `DRAWLINESTR` returns the bar computed from the *configured* window metrics while the GPU renderer re-fits its own `ConsoleLinePart::Line` to the live window | `erars-ui/src/lib.rs:389-417` (`bar_string`), `erars-vm/src/lib.rs:33-54` (`drawable_cells`, `:49`), `executor.rs:408-415` | `EmueraConsole.Print.cs:632-649` (`getStBar`), `Config/Config.cs:222-225` |
| the half-width cell the bar is measured in is `font_size / 2`; Emuera measures the real GDI font | `erars-vm/src/lib.rs:33-54` | `Config.cs:222-225`, `EmueraConsole.Print.cs:632-649` |
| `GDRAWTEXT` rasterises real glyphs through `erars-font`; what differs is the anti-aliasing kernel and the cell metrics (swash + `ttf_parser` `hhea`/`OS/2`/`post` instead of GDI+ `FontFamily`), it falls back per character through the console's own font chain where `GraphicsPath.AddString` would draw one family's `.notdef`, and the drawing em is clamped at 4096 px where GDI+ would throw `OutOfMemoryException` | `crates/erars-font/src/text_image.rs` (`MAX_EM_PX`, `FaceMetrics::of`), `graphics.rs` (`draw_text`, `blend_coverage`), `executor.rs:1297-1349` | `Content/GraphicsImage.cs:120-142`, `Creator.Method.cs:5531-5566` |
| `HTML_PRINT_ISLAND` / `HTML_PRINT_ISLAND_CLEAR` keep overlay layers beside the log, with semantics derived from the corpus because the fork has no `ISLAND` source to follow (§5.11) | `executor.rs:2971-3012`, `erars-ui/src/lib.rs` (`islands`) | `.NET版` fork only, absent from `/tmp/webemuera` |
| a `GDRAWG`/`GDRAWSPRITE` colour matrix must be an array of two dimensions or more — a 2D *character* array is read as Emuera reads it, and a flat 1D array is a diagnostic where Emuera dereferences a null row; the 3D-character refusal is Emuera's own | `executor.rs` (`read_color_matrix_opt`) | `Creator.Method.cs:5180-5232` (2D chara at `:5188-5193`, `cm[x][y] = array[…]/256f`, `NotImplCodeEE` for 3D chara at `:5214-5217`) |

`DRAWLINESTR` deserves the longer explanation. Emuera bakes `stBar` once, at start-up, from the
configured window width and the measured font, and both `DRAWLINE` and `DRAWLINESTR` hand out that
one string (`GameProc/Process.cs:117`). erars keeps two metrics on purpose:
`docs/superpowers/specs/2026-09-02-emuera-parity-renderer-design.md:672-675` records the decision
that the GPU renderer re-lays-out `ConsoleLinePart::Line` against the *live* inner width, so a
`DRAWLINE` already on screen grows and trims when the window is resized. A script that asks for
`DRAWLINESTR` gets a plain string it can measure, index and concatenate, so it cannot follow a
resize; the honest answer is the string Emuera would have baked, which is what `bar_string`
computes — `getStBar`'s own algorithm, in half-width cells, against `ウィンドウ幅`. At the
defaults (760 px, font 18) that is 84 cells, the same number the design spec quotes at `:473`.
The two metrics are the only two in the tree: the `stdio` front-end used to repeat the unit 30
times and now consumes `bar_string` as well (`erars-stdio/src/stdio_frontend.rs:87-93`).

### 5.2 No host locale

| Deviation | erars | Emuera |
|---|---|---|
| the `C`/`P` standard specifiers are fixed to `CultureInfo.InvariantCulture` (`¤`, `(¤n)`, `n %`); `R`/`B` are deliberate errors | `dotnet_number.rs:46-50`, bail at `:199-204` | host `CurrentCulture` |
| every other culture-sensitive value in a custom pattern is the invariant one too: `,` group separator, `.` decimal separator, `-`/`+` signs, `%`, `‰`, and the group size fixed at 3 (the invariant `NumberGroupSizes` is `[3]`) | `dotnet_number.rs:12-15` | host `CurrentCulture` |
| `ParseFormatSpecifier`: a letter plus a digit tail counts as a standard specifier only when the whole tail is digits and is at most 999999999. .NET stops the digit scan at an embedded NUL — so `"D5\0junk"` is standard `D5` there and a custom pattern here — and throws `FormatException` at a tail of 100000000 or more, where erars falls back to the custom pattern | `dotnet_number.rs:60-77` | `Common.cs`, `ParseFormatSpecifier` |
| `TOFULL` does not fold the Mathematical Alphanumeric Symbols or the CJK Compatibility Ideographs Supplement (astral, never emitted by ERB) | `kana.rs:191-194` | `LCMAP_FULLWIDTH`, `locale.c:2618-2633` |
| `ゞ`/`ヾ` folding derived by inference — Wine's table carries no entry, and the pairing follows from the other voiced iteration marks | `kana.rs:28-32` | `LCMAP_HIRAGANA`/`LCMAP_KATAKANA` |

### 5.3 Ordering guarantees erars tightens

Emuera leaves these unspecified; erars picks a deterministic answer, which satisfies everything Emuera
promises and additionally makes runs reproducible.

| Deviation | erars | Emuera |
|---|---|---|
| `ARRAYMSORT` sorts stably | `array_msort`, `executor.rs:4171` (`sort_by`) | `List<T>.Sort` — unstable introsort |
| `FIND_CHARADATA` / `CHKCHARADATA` return names in sorted order | `save.rs:264` | `Directory.GetFiles` — order unspecified |
| the `CBG*` plane keeps equal `zdepth`s in insertion order, so the later call is on top | `cbg.rs` (`CbgLayer::push`) | `EmueraConsole.cs:203-204`, `List<T>.Sort` again |

`SORTCHARA` is *not* in this table: Emuera's comparators break ties on the pre-sort index in **both**
directions (`CharacterData.cs:683-696`), so it is already fully specified, and erars reproduces exactly
that (`chara_order`, `executor.rs:1049-1073`).

### 5.4 erars-only extension

- **`ARRAYMOVE`** (`executor.rs:2944-2965`) appears nowhere in the Emuera source, the EM+EE instruction
  index, or the era-wiki command tables. erars inherited the name in `crates/erars-lexer/src/inst.rs`
  and it is kept as a documented erars extension rather than silently accepted as Emuera parity.

### 5.5 Diagnostics erars changes on purpose

| Deviation | erars | Emuera |
|---|---|---|
| `MOUSESKIP` emits a deprecation warning at parse time and then behaves as `MESSKIP` | `parser.rs:2280-2287` | silently aliased |
| CSV integer fields are parsed **strictly**: a non-numeric value is a load error, not a warning plus `0` | `csv_parse_int!`, `parser.rs:54-61` | `ParserMediator.Warn` + `0` |
| `INPUTMOUSEKEY`'s `RESULT:4` (button-map mask under the cursor) is always `-1`: no erars front-end has a button-map bitmap. The VM itself reports whatever the front-end supplies | `MouseKeyEvent::mask`, `erars-ui/src/lib.rs:853-854` | `EmueraConsole` button map |
| a chara CSV's `助手`/`ISASSI` row sets the template's assistant flag; Emuera's arm for that key is a bare `return;`, so the row is read and thrown away | `merge_chara_csv`, `parser.rs:1100` | `ConstantData.cs:1604-1606` |
| a CSV name used at the *wrong* dimension is still resolved; Emuera compares the index it was used at against the `allowIndex` the name's table declares and raises `CannotIndexSpecifiedByString` | `variable_arg`, `parser/expr.rs:1382-1440` | `ConstantData.cs:1163-1168` |
| `MATCHALL`'s five-argument `var, index, value, start, end` form is refused rather than guessed at: erars carries the index inside the variable reference, so accepting it positionally would silently read `value` as the index. The corpus's only call is the two-argument form (`RPG/戦闘/BATTLE.ERB:1397`) | `executor.rs:2595-2607` | `.NET版` fork, `MATCHALL` |

### 5.6 Emuera behaviour deliberately declined

These were investigated and left unimplemented on purpose; each is recorded here so it is not mistaken
for an oversight.

- `REF`'s function-reference form (only the variable-reference form is supported).
- `MatchType` rules 6 and 7.
- `CALLEVENT` resolves its event name at parse time, not at run time.
- `GAMEBASE.CSV` keys `動作に必要なEmueraのバージョン`, `バージョン情報URL` and `バージョン名` are not
  read; they configure an update check erars has no equivalent for. `ウィンドウタイトル` **is** read
  now (`parser.rs:1156`, with Emuera's own absent-key fallback at `:1173-1181`) — eraMegaten's
  `Data/CSV/GameBase.csv:4` sets it, and the `stdio` front-end emits it as an OSC-2 terminal title
  (`erars-stdio/src/main.rs:136`, from `Process.cs:144`). An earlier revision of this section claimed
  use the key; that was wrong.
- The `WINDOW_TITLE` pseudo-variable is declared (`crates/erars-loader/src/variable.yaml:123`) and
  inert: Emuera's is a live window handle (`VariableToken.cs:1556`, `VariableEvaluator.cs:119`,
  `:186`), so assigning to it renames the OS window. A terminal front-end has no window to rename
  after start-up, and the renderer's title comes from the same `Gamebase::window_title`.
- `VarsizeDimConfig` (`VARSIZEの次元指定をERD機能に合わせる`, `Config/ConfigData.cs:136`) would
  subtract one from a positive `VARSIZE` dimension argument. It defaults off and no corpus config
  turns it on, so `const_eval`'s `VARSIZE` folding takes the dimension as written
  (`parser.rs:912-935`).
- A hex significand with a binary exponent (`0x10p4`) is not accepted: Emuera's lexer reads the
  exponent digits in the significand's own base, which for `0x…` would make `p` ambiguous with a hex
  digit. Decimal significands with `p`/`P` and `e`/`E` exponents are supported in full
  (`parser/expr.rs:659-706`), including the unchecked `(Int64)d` wrap that makes `1P63`
  `i64::MIN` — the corpus depends on it (`-1p63-1`, documented in
  `tests/parse_tests/functions/juel.erb:7` as `Int64.MaxValue`).

### 5.7 Load-time error recovery

Emuera's loader never throws a whole ERB away over one bad line: it records the message, replaces the
line with an `InvalidLine` that raises only if execution reaches it, and carries on
(`GameProc/ErbLoader.cs:403-407`, `:423-427`, `GameProc/LogicalLine.cs:74-85`). erars does the same
per line — the bad line becomes a `Throw` of its own diagnostic and the function still registers
(`parse_and_compile`, `parser.rs:3142-3300`, the resumable branch at `:3256-3264`;
`Compiler::push_invalid_line`, `compiler.rs:64-68`) — with two deliberate differences in
granularity and one in the consequence.

| Deviation | erars | Emuera |
|---|---|---|
| a line whose parse already consumed the lines after it — a block opener such as `IF` or `FOR`, which erars parses recursively where Emuera pairs it up in a later pass — leaves nowhere safe to resume, so **the whole enclosing function is dropped**: its remaining lines are only scanned for the next `@label` and it never registers, so calling it fails as an unknown function | `parser.rs:3246-3264` with `finish!` at `:3186-3204` | every line is `InvalidLine`d individually and the function still exists |
| a `#DIM`/`#DIMS` erars cannot read means the function's local does not exist, so the same whole-function drop applies; Emuera keeps the function and invalidates each line that mentions the name | `push_info`'s `Err` shape, `parser.rs:3238-3244` | `ErbLoader.cs` sharp-line pass |
| **erars reports every unreadable line and still starts the game.** With `解釈不可能な行があっても実行する:NO` — the default (`Config/ConfigData.cs:106`) and what eraMegaten ships (`eramegaten_p_kr/emuera.config:49`) — each `InvalidLine` clears Emuera's `noError` flag and `Process.SystemProc.cs:173-186` refuses to start at all | `compile_one`, `crates/erars-loader/src/lib.rs:450-477` (`report_error!` per line, the file's other functions still register) | refuses to start |

What is *not* a divergence, and is worth stating because it looks like one:

- A label erars cannot read poisons the whole file. That is the one failure Emuera also treats that
  way — `InvalidLabelLine` sets `noError` (`ErbLoader.cs:366`) — and nothing after it can be
  attributed to a function. A non-constant argument index in a label line reaches the same path,
  because Emuera also `Restructure`s that term while loading.
- `CONTINUE`/`BREAK` outside a loop are **warnings**. Emuera reports them through
  `ParserMediator.Warn` (`ErbLoader.cs:1041-1058`), which sets `line.IsError`
  (`GameData/ParserMediator.cs:118-131`) but never touches `noError`, so the function still
  registers and the game still starts. erars emits them on its own warning channel (W2000).

Pinned by `tests/recovery_test.rs`.

### 5.8 Host filesystem and configuration

| Deviation | erars | Emuera |
|---|---|---|
| a content path's `\` separators are rewritten to the host's. Scripts write `"タイトル画像\\タイトル001.webp"`, which .NET resolves natively and Unix would take for a single filename component | `content_path`, `executor.rs:1014-1016` | `Creator.Method.cs:5928-5937` |
| `emuera.config` is read from `<target>/emuera.config` only | `load_config`, `crates/erars-loader/src/lib.rs:40-59` | `Program.WorkingDir + "emuera.config"` (`Config/ConfigData.cs:24`, `Program.cs:44`) — beside `Emuera.exe`, i.e. the parent of `Data/` in this fork's layout |

The second row has a consequence worth writing down: eraMegaten keeps `emuera.config` next to
`Emuera.exe`, one level above the `Data/` directory that erars is pointed at, so the game currently
loads with `EraConfig::default()` — 760×480, font 18 — instead of its own 1512×882. Nothing in the
load fails because of it; the visible effect is that `DRAWLINESTR`, `CLIENTWIDTH` and
`GETCONFIG("フォントサイズ")` answer with the defaults. Widening the search to the parent directory
is a guess about layout that no Emuera source line supports, so it is recorded here rather than
implemented: the fix is to point erars at the directory holding the config, which is what
`/tmp/mgt` does.

### 5.9 `.NET版` fork instructions

eraMegaten targets the .NET Emuera fork (eraten / ShinEraTensei P), so erars implements the fork's
additions. Where the fork gates a feature behind `setting.json`, erars does not:

| Deviation | erars | Fork |
|---|---|---|
| `VARI`/`VARS` are always available | `parser.rs:2803-2812` with `parse_var_decl` at `:3075`, `crates/erars-lexer/src/inst.rs:378-381` | gated behind `UseScopedVariableInstruction` |

eraMegaten's `Data/setting.json` sets `"UseScopedVariableInstruction":true`, so the gate is satisfied
for this game; erars has no `setting.json` reader and accepting the instructions unconditionally can
only accept a script the fork would also accept. `"UseRenameInCharaCSV":true`, `"CheckUTF8withBOM":true`
and `"UseNewRandom":false` are the other keys the game sets.

The fork's other additions are implemented. The ones whose behaviour is not a plain reading of a
reference implementation are recorded in §5.1 and §5.11 (`HTML_PRINT_ISLAND`,
`HTML_PRINT_ISLAND_CLEAR`, whose semantics are derived from the corpus because the fork ships no
`ISLAND` source at all) and §5.5 (`MATCHALL`'s five-argument form).

### 5.10 Equivalences relied on

Not deviations — places where erars does something visibly different from Emuera's own code with an
argument that the result cannot differ. Recorded so the argument is reviewable.

- **`_Rename.csv` is applied in one scan, not one pass per entry.** Emuera loops over every
  dictionary entry for every physical line that contains both `[[` and `]]`
  (`Sub/EraStreamReader.cs:86-89`, `:120-123`). erars scans the line for `[[`…`]]` tokens and looks
  each one up once (`apply_rename`, `crates/erars-lexer/src/lib.rs:189-235`). The two agree because a
  key is exactly `[[` + the CSV's right-hand column + `]]` (`GameData/ParserMediator.cs:73`) and, as
  measured over eraMegaten's table (10,106 physical lines, **9,503 entries**, 9,474 distinct keys,
  zero rows with a third field), no column contains `[[` or `]]` and no *value* does either. So a
  key can never occur inside another key — 445 columns are substrings of other columns, but `[[A]]`
  can only sit inside `[[B]]` if `B` itself holds a bracket pair — and no replacement can grow a
  token for a later entry to match, which is what makes Emuera's iteration order unobservable. The
  29 duplicate keys resolve identically: `HashMap::insert` and `RenameDic[key] = value` both keep
  the last row.
- **The rename lookup trims the token's inner text.** The dictionary key is trimmed when it is built
  (`ParserMediator.cs:72-73`) and the padded form `[[ Key ]]` is in use, so the token is trimmed to
  match.
- **U+3000 is whitespace wherever U+0020 is.** Emuera has one whitespace rule, `SkipWhiteSpace`
  (`Sub/LexicalAnalyzer.cs:749-752`), which includes U+3000 while
  `全角スペースをホワイトスペースに含める` is on — its default (`Config/ConfigData.cs:112`) and what
  the corpus configs set. erars therefore has one predicate as well (`is_sp`,
  `parser/expr.rs:618-620`; `skip_ws`, `crates/erars-lexer/src/lib.rs:385-434`), U+3000 ends an
  identifier (`cut_ident`, `crates/erars-lexer/src/utils.rs:5-28`), and the single separator
  character after an instruction name may be U+3000 as well as a space or tab
  (`strip_inst_separator`, `utils.rs:30-44`, from `GameProc/LogicalLineParser.cs:428-436`). Pinned by
  `tests/run_tests/basic/u3000.erb`. The switch itself is not implemented: erars always behaves as
  the default `true`. Turning it off in Emuera makes U+3000 a plain identifier character again and
  turns it after an instruction name into its own diagnostic
  (`InvalidCharacterAfterInstruction1`, `LogicalLineParser.cs:428-433`) — no corpus config does.
- **Every method is also a line-head instruction.** Emuera registers them that way unless a
  hand-written instruction already owns the name
  (`GameProc/Function/FunctionIdentifier.cs:428-436`); erars tries the same fallback, ordered after
  the assignment and `++`/`--` forms so an assignment is never captured by it
  (`next_line`, `crates/erars-lexer/src/lib.rs:756-915`). Pinned by `tests/run_tests/basic/method_line.erb`.
- **A resource row's second token keeps its surrounding spaces, and a malformed rectangle is
  silent.** `CreateFromCsv` trims only the name (`Content/AppContents.cs:185`), so
  `face,  face.png` looks for a file whose name begins with two spaces and warns that it is missing;
  and when any of the four rectangle fields fails to parse it keeps the whole-parent default with no
  diagnostic at all (`:269`, an `if` with no `else`). Both look like erars bugs and are neither
  (`resources.rs:199-203`, `:265-271`).

### 5.11 The image surface

`erars_ui::image` gives the console real pixels: `ConsoleLinePart::Image(Arc<InlineImage>)` carries a
resolved sprite plus Emuera's own geometry, `GraphicsStore::publish` hands changed bitmaps to the
front-end once per frame, and the GPU renderer samples them. That closes most of what §5.1 used to
excuse. What remains below is deliberate, and every row is a decision about a behaviour the corpus
either never exercises or exercises in a way this fork cannot serve.

| Deviation | erars | Emuera |
|---|---|---|
| a `<div>`'s attribute vocabulary is not enforced, `width`/`height` are not required, and a box may nest or sit inside a `<button>`; `radius` and `depth` are parsed and validated but not honoured | `html.rs` (`"div"` arm), `erars-ui/src/div.rs` | `HtmlManager.cs:1068-1173`, `:531-532`, vocabulary `_Library/EvilMask/Utils.cs:99-124` |
| a box with no `width`/`height` is unbounded on that axis, so its content is neither wrapped nor clipped there | `erars-ui/src/div.rs` (`inner_width`/`inner_height`) | both are mandatory in this fork (`HtmlManager.cs:1166-1169`), so the case cannot arise |
| a bare `display='absolute'` is read as `absolute-leftbottom` | `html.rs` (`parse_anchor`) | `HtmlManager.cs:1155-1160`, whose `absolute` measures a *positive* `ypos` down from the bottom edge |
| every positioned box and every island is drawn after all log text, not during the line that printed it | `headless.rs` (`render_frame_on` draw groups) | `_Library/EvilMask/ConsoleDivPart.cs:100-166` paints inside its owner line's paint |
| a box's four borders are axis-aligned bands, left and right drawn over top and bottom, so the corner triangles belong to the horizontal edges; `radius` is never drawn | `draw.rs` (`decor_quads`) | `_Library/EvilMask/Shape.cs:60-105` mitres each corner between two polygons, `:108` rounds them |
| a box clips its content to its own rect only, never intersected with an ancestor's, and a nested box leaves its parent's clip in place for the parent's remaining lines | `layout.rs` (`Clip`, `emit_div`) | `ConsoleDivPart.cs:159-168` replaces the clip and then `ResetClip()`s, which leaves a parent's later children unclipped after a nested box — the three-deep nesting in `DIV_MESSAGE_LOG.ERB:61-71` needs the parent's clip back |
| a four-edge shorthand collapses by *value*, so `padding='1px,1px,1px,1px'` rebuilds as `padding='1px'` | `erars-ui/src/div.rs` (`write_mixed4`) | `_Library/EvilMask/Utils.cs:148-176` compares the four `MixedNum` objects with `ReferenceEquals`, so only a shorthand that was *parsed* short collapses (`AddColorParam4` at `:185-204` does compare colours by value, and erars matches it) |
| an island's content is anchored at the client origin, as if the whole island were one `display='absolute-lefttop'` box at `(0,0)`; islands accumulate within a layer and layers paint in ascending order | `erars-ui/src/lib.rs` (`islands`), `layout.rs` | fork-only command with no source to compare against |
| unknown `<img>` attributes are ignored instead of rejected | `html.rs`, `<img>` arm | `HtmlManager.cs:1060-1061` |
| an image inside a `[n]` button is drawn but is not itself clickable; the button run splits around it | `layout.rs` (`PlacedImage.button`), `html.rs` (`flush_run`) | `PrintStringBuffer.cs:189-279` |
| a printed image snapshots its sprite's geometry; a later `SPRITEMOVE`/`SPRITESETPOS` does not shift it. Pixels stay live | `graphics.rs` (`sprite_geometry`) | `ConsoleImagePart.cs:66` holds the `ASprite` itself |
| `PRINT_IMG` has no `px` keyword, so every number is a percent of font size | `executor.rs`, `PrintImg` arm | `ArgumentBuilder.cs:298` |
| images sample through a linear filter, glyphs through nearest | `gpu.rs` (`linear_sampler`) | GDI+ default bilinear; the console `Graphics` never sets `InterpolationMode` |
| no `ConsoleEscapedParts` equivalent; images are drawn inline in the single frame pass | `layout.rs` (`Row::images`) | `EmueraConsole.cs:1586-1598` |
| an animation's phase is measured from a monotonic clock started with the front-end, not latched on each sprite's first draw | `draw.rs` (`ImageCtx::now_ms`) | `CroppedImage.cs:229-235` latches `StartTime` per `SpriteAnime` |
| a `resources/` path resolves case-insensitively, component by component | `resources.rs` (`resolve_path`) | `AppContents.cs:180-315` hands the string to `Bitmap`, i.e. NTFS' own case folding |
| `GETKEY`/`GETKEYTRIGGERED` see only the keys the focused window reported; losing focus releases them all | `app.rs` (`track_key`, `Focused(false)`) | `WinInput.GetKeyState` reads the OS' global key state |
| `ISACTIVE` is the constant 1 | `executor.rs`, `IsActive` arm | `EmueraConsole.cs:276-277` is itself `return true` |
| `MOUSEX`/`MOUSEY` report `(-1, -1 - clientHeight)` while the cursor is outside the window | `app.rs` (`mouse_key_pos`, `CursorLeft`) | `EmueraConsole.cs:1985-1989` maps the global `Cursor.Position` into client space, so it keeps reporting real offsets |
| a `CBGSETBUTTONSPRITE` tooltip is evaluated and then dropped | `executor.rs`, `CbgSetButtonSprite` arm | `Creator.Method.cs:6700-6702` stores it (`EmueraConsole.cs:232`) and `:1646-1669` shows it in a WinForms `ToolTip` |
| `SPRITEGETCOLOR` returns the packed `0xAARRGGBB` the wiki documents, not what the fork's shift expression computes | `executor.rs`, `SpriteGetColor` arm | `Creator.Method.cs:5840` |
| `CHARATU` indexes Unicode scalars, so a character outside the BMP is one position and comes back whole | `executor.rs`, `CharAtU` arm | `Creator.Method.cs:4812-4819` is `str[(int)pos]`, a UTF-16 code unit, and returns half a surrogate pair |
| `HTML_GETPRINTEDSTR` never emits `<br>` | `html.rs` (`line_to_html`) | `HtmlManager.cs:311-314` joins the display lines one wrapped logical line occupies; erars keeps logical lines and lets the front-end wrap |
| `HTML_GETPRINTEDSTR` emits no `<nonbutton>`, and no `title=`/`pos=` on a `<button>` | `html.rs` (`line_to_html`) | `HtmlManager.cs:317-348`, `:379-385`; erars has neither per-run tooltips nor a locked `PointX` |
| `HTML_GETPRINTEDSTR` emits no `bcolor=` — the same missing per-run focus colour that drops `<font bcolor>` on the way in (§5.1) | `html.rs` (`push_styled`) | `HtmlManager.cs:802-808` |

**`<div>` is not a loosening, it is an impossibility.** This fork's `<div>` requires `width` and
`height` (`HtmlManager.cs:1166-1169`), forbids nesting (`:1070-1071`), allows `display` only
`absolute` or `relative` (`:1155-1160`), and rejects any name outside its box-model vocabulary
(`:1163-1164`, vocabulary at `_Library/EvilMask/Utils.cs:99-124`: `border`, `radius`, `margin`,
`padding`). eraMegaten breaks every one of those: of its **369** `<div>` tags, **186 carry neither
`width` nor `height`** and so have no box at all, some nest three deep
(`関数/汎用組み込み関数/DIV_メッセージウィンドウ/DIV_MESSAGE_LOG.ERB:61-71`), one opens inside a
`<button>` (`ＳＨＯＰ関連/120_ショップ.ERB:49`), all 67 that set `display` use a spelling this fork
does not have (50 `absolute-leftbottom`, 17 `absolute-lefttop`), and 48 use `background_color`, 58
`border_width` and 19 `border_color` — three names this fork's vocabulary does not contain. The game
therefore targets a **newer EvilMask build than the one in `/tmp/webemuera`**, and reproducing this
fork's `<div>` diagnostics is not merely inconvenient, it is wrong: it would reject markup the
game's actual engine accepts. So the parser enforces what is still knowable — that a tag is well
formed, that its values parse, and that every `<div>` is closed — and nothing else.

**The geometry, unlike the vocabulary, is knowable, and it is implemented.** `ConsoleDivPart` takes
`Math.Abs` of both extents (`_Library/EvilMask/ConsoleDivPart.cs:20-21`), resolves every `MixedNum`
against the console font size in its constructor (`:49-64`, `Utils.cs:19-22`), holds its content as
its own array of display lines (`:90`), and contributes neither text nor width to the line it was
printed on (`Str = string.Empty` at `:48`, an empty `SetWidth` at `:176-178`). erars mirrors that:
`erars_ui::ConsoleDiv` is built at print time from the parsed `DivSpec` and
`ImageResolver::font_size`, `ConsoleLinePart::Div` is a zero-width part of the line that opened it,
and the renderer places the box at `xpos`/`ypos` — relative to that part for `display='relative'`,
to the client origin for `absolute-lefttop`, and to the bottom edge (`view_h + ypos`, which is why
every corpus site passes a negative `ypos`) for `absolute-leftbottom`. `HTML_GETPRINTEDSTR` still
sees nothing, and that is faithful, not a gap: `DisplayLine2Html` (`HtmlManager.cs:290-393`) has a
branch for a styled string, an image and a shape, and none for a `ConsoleDivPart`.

What that buys is visible in the real game rather than in any fixture. eraMegaten shows every event
picture through `PRINT_EVENT_PICTURE`
(`関数/組み込み関数/メッセージ/PRINT_EVENT_PICTURE.ERB:12-70`), which reserves the vertical space by
printing `ceil(height/100) - 1` blank lines (`:64-66`, a `FOR` with an exclusive end) and then lifts
the picture back over them from the next line with
`HTML_PRINT "<div xpos='…px' ypos='-2900'><img src='…' height='3000'></div>"` (`:68`); the
bottom-anchored form of that offset is computed by
`関数/汎用組み込み関数/入力関数/CONVERT_YPOS_TOP_TO_BUTTOM.ERB`, which is where the `view_h + ypos`
arithmetic above is read from. The title screen goes through that same function
(`GCREATEFROMFILE` at `タイトル表示/PRINT_TITLE.ERB:50` → `SPRITECREATE` at `:52` →
`CALL PRINT_EVENT_PICTURE, "タイトル003", …, "CENTER"` at `:74`).

Measured on the shipped game, at font size 18 in a 1512×882 window, the box the title screen prints
is `<div xpos='216px' ypos='-522'><img src='タイトル003' height='540'></div>` — the `xpos` is the
centring offset the game computed itself, `MAX(CLIENTWIDTH() - width, 0) / 2`
(`PRINT_EVENT_PICTURE.ERB:49-56`), and both numbers are already resolved because the rebuilt tag
writes a percentage in pixels (`MixedNum.BuilderString`, `_Library/EvilMask/Utils.cs:44-48`). The
1080×540 picture is drawn at x 219–1298 (`216` plus the 3 px GDI overhang shift every log row is
drawn with) and y 72–611: `72` is `row_y(33) - 522` for row 33, the row the `HTML_PRINT` itself
landed on, and `611` is that row's own last pixel. The picture therefore covers rows 4–33 —
the 29 blank `PRINTL`s plus the box's own row, 30 rows of 18 px, exactly 540 — which is what
`pointY + PointY` (`ConsoleDivPart.cs:142`) means for a picture reserved this way. The shot taken
before this work has the same picture at y 558–881, clipped by the window's bottom edge: below the
whole gap instead of inside it.

Getting there needed one non-`<div>` fix. Every line an `HTML_PRINT` produces is aligned by the HTML
parse state, which starts `LEFT` (`HtmlAnalzeState.Alignment`, `HtmlManager.cs:237`) and moves only
for a `<p align>`: `Html2DisplayLine` calls `dl.SetAlignment(state.Alignment)` (`:623-629`) before
the print path would apply the console's own `ALIGNMENT` (`EmueraConsole.Print.cs:179-182`), and
`SetAlignment` keeps the first value it is given (`ConsoleDisplayLine.cs:61-64`). erars had left the
console's alignment in place, so on the title screen — which prints under `ALIGNMENT CENTER` — the
box's line was centred a second time. A box advances the pen by nothing (`print_div`), so that line
is zero-width and centring it moves its origin to `content_w / 2` — half the console away from
where the game put it. `html.rs` now aligns every line it produces, and a box's own lines, from that
same `LEFT` default (`alignment_is_scoped_to_the_string`).

What `<div>` still rejects, so this does not drift into "accept anything": a malformed attribute list
(unterminated quote, `=` with no value, an attribute with no name), a repeated attribute — including
the collision between `size`/`rect` and the single attributes they fill — a value that is not a
`MixedNum` or a colour, a wrong token count in a shorthand, a `display` value outside the three it
knows, a `</div>` with no open `<div>`, and an unclosed `<div>` at the end of the string, which
still raises Emuera's own `閉じられていないタグがあります`. Only the *vocabulary* and the
*required-attribute* checks are gone.

**The island commands have no reference implementation, so their semantics are read off the
corpus.** The string `ISLAND` does not occur anywhere in `/tmp/webemuera`, and the wiki has no page
for either command; what is knowable is how eramegaten_p_kr calls them, and that is unambiguous.
`HTML_PRINT_ISLAND value, {layer = 0}` takes the same markup as `HTML_PRINT` and keeps it as an
overlay that survives scrolling, input and further printing: the backlog viewer prints one island
and then reads the mouse wheel under it for as long as the user scrolls
(`関数/汎用組み込み関数/DIV_メッセージウィンドウ/DIV_MESSAGE_LOG.ERB:61-106`).

**A print stacks, it does not replace.** `RPG/ダンジョンアタック/SYSTEM_DUNGEON.ERB:2630-2641`
covers the dungeon view with one island — a single background-coloured box `CLIENTHEIGHT() - TEMP`
tall — waits 200 ms, and then prints a *second* island to the same (default) layer whose `ENCOUNT` /
`ENEMY!!` text is positioned at `(CLIENTHEIGHT() - TEMP) / 2`, the centre of the box the first
island drew. The two are only in register if the cover is still on screen, and `:2670-2688` repeats
the pattern with a full-height cover. So a layer is a paint-order group, not a slot: islands
accumulate within a layer in print order and only `HTML_PRINT_ISLAND_CLEAR` takes one down. That is
also why every corpus site clears before reprinting rather than relying on a replacement
(`DIV_MESSAGE_LOG.ERB:105`, `21_アイコンセット_プレイヤー.ERB:389`).

Layers paint in ascending numeric order: `関数/汎用組み込み関数/メッセージ/MESSAGE_POPUP.ERB:22-35`
prints its popup on `L_LAYER_NO` (99 by default) and the dimming filter that must sit *under* it on
`L_LAYER_NO - 1`. `HTML_PRINT_ISLAND_CLEAR` discards one layer with an argument (`:38-39`) and every
island without one (`SHOW_STATUS/SHOW_STATUS_WINDOW.ERB:1111`) — the per-layer form exists because
that popup is called *from* a screen that has its own islands up
(`21_アイコンセット_プレイヤー.ERB:356-368` prints one and then calls `INPUT_YN`), and a clear-all
there would wipe the caller's display.

erars stores them in a `Vec<(i64, Vec<ConsoleLine>)>` kept in paint order beside the log — never
*in* it, so an island changes neither `LINECOUNT` nor what `CLEARLINE`, `HTML_GETPRINTEDSTR` or the
backlog can reach — and `ConsoleFrame::islands` carries them to the front-end in that order. The
markup is parsed before anything is stored, so a tag error adds nothing and leaves what is on
screen alone. Two things no reference can settle: an island's own content is anchored at the client
origin, which is what `SYSTEM_DUNGEON.ERB:2630`'s
`<div display='absolute-lefttop' width='{CLIENTWIDTH()}px' …>` expects and what leaves a bare-text
island at the top left; and an island's `<button>`s are clickable exactly like a log button, since
the corpus puts its cancel button inside one (`MESSAGE_POPUP.ERB:35`). `App::hit_button`
(`app.rs`) therefore tests every placed row — island rows and `<div>` rows alike — in reverse
layout order before any flow row, so the topmost island wins and a box beats the log line it
covers (`hit_test_prefers_placed_rows_and_honours_their_clip`).

An island line may itself hold nothing but a positioned box, and that is the corpus's dominant
shape: `女神転生/ＭＡＧ/MAG_PORTRAIT.ERB:373` and `:487` print
`HTML_PRINT_ISLAND "<div display='absolute-leftbottom' xpos='0' ypos='{…}'>" + DRAWLINESTR +
"</div>", MAIN_LAYER_NO`. Such a box keeps its own anchor — it measures from the console's bottom
edge, not from the island — and takes only the island's paint slice, so two islands on the same
layer still stack in call order
(`a_box_inside_an_island_keeps_its_own_anchor_and_the_islands_slice`,
`island_entries_cover_the_log_and_each_other_in_order`).

**Unknown `<img>` attributes are ignored for the same class of reason, with the game's own typos as
the proof.** eraMegaten writes `xpos` on an `<img>` at
`Data/ERB/イベント/キャラ・NPC顔アイコン.ERB:23` and `:35` and `img_size` at
`Data/ERB/ゲーム/KOJO_RPG.ERB:950` and `:952` — neither exists in any Emuera build's `<img>` — and
its `<div>`s carry `ypps` four times and `yos` once, which are misspellings of `ypos`. A parser that
rejects an unknown attribute cannot run this corpus, and the newer EvilMask build the game targets
evidently does not. `src` stays **mandatory**, exactly as `HtmlManager.cs:1005-1010`, because that is
the one attribute without which there is nothing to draw; the two "bare `<img>`" hits in the corpus
census are regex false positives inside string literals (`DIV_MESSAGE.ERB:216`, `:733`).

**The publish/redraw ordering is enforced by the type system, not by this document.**
`SystemFunctions::redraw` and its three `input_*` siblings take a `graphics::Painted<'_>` by value;
the only thing that can construct one is `GraphicsStore::publish`, whose field is private to
`graphics.rs`, and the token borrows the store for its whole life. A call site that repaints without
publishing does not compile, and neither does one that publishes, mutates a bitmap, and then
repaints. That is why `context.rs` carries no "never call `system.redraw` directly" rule any more.

**The live input surface is a query, not a wait.** `GETKEY`, `GETKEYTRIGGERED`, `MOUSEX` and
`MOUSEY` are the only methods that read the *present* state of a device rather than a value the user
submitted, and Emuera serves them by calling the OS from the expression evaluator: `GetKeyState`
through P/Invoke (`_Library/WinInput.cs:9`) and `Cursor.Position` mapped into client space
(`EmueraConsole.GetMousePosition`, `GameView/EmueraConsole.cs:1981-1990`). erars' VM has no window,
so it asks the front-end with `SystemRequest::QueryState`, answered from the event loop's own
cached state without ever becoming an input request (`app.rs`, `drain_requests`). A front-end that
has no window at all — stdio, the headless shot — answers with `InputState::default()`, which is
exactly what Emuera reports before its window exists (`:1983-1984` returns `new Point()`).

`GETKEYTRIGGERED` is the one place the wiki and this fork disagree, and the fork wins. The wiki
describes a press edge; the C# compares `GetKeyState`'s *toggle* bit against the value the last
`GETKEY` **or** `GETKEYTRIGGERED` call stored in a process-wide `static readonly short[] keytoggle`
(`Creator.Method.cs:6709`, `:6725-6734`). Three consequences are reproduced verbatim in
`VmContext::get_key`: the first observation of an already-held key triggers, a plain `GETKEY` call
consumes the edge a later `GETKEYTRIGGERED` would have seen, and an out-of-range code returns 0
without touching the latch. What cannot be reproduced is the *source* of that bit: Windows maintains
the toggle globally, whereas winit only delivers key events to a focused window, so erars flips its
own bit on each press it is told about and releases every key when the window loses focus. A key
pressed while erars is in the background is invisible to it; in Emuera it is not.

**`CBG*` is a second image surface, not eight more sprite commands.** Emuera keeps `cbgList` on the
console itself (`GameView/EmueraConsole.cs:101-130`) and `OnPaint` walks it and the text log in one
merged loop ordered by descending `zdepth` (`:1557-1599`), so three properties come from *where* the
plane lives rather than from any command: a negative `zdepth` draws **in front of** the text, which
no other erars surface can do; the entries are placed in client pixels from the bottom-left corner
and do not scroll with the log (`:1573`, `y + ClientHeight - DestBaseSize.Height`); and the plane
outlives the game, because `CBG_Clear`'s only caller is the console constructor (`:93`) — a save
load, a new game and `SPRITEDISPOSEALL` all leave it standing. erars therefore hangs a `CbgLayer` off
`VirtualConsole`, transports it with the frame, and draws it in four groups (`app.rs`, `render`):
depth above 0, the glyphs, the inline images, depth below 0.

One consequence is worth naming because it changed existing behaviour. The plane's origin is
Emuera's `ClientHeight`, which is `MainPicBox.Height` (`:238`) — the console area *above* the input
strip, not the whole window — and the button map's hit test measures from the same edge
(`MouseDown`, `:1000-1014`). erars' `mouse_key_pos` used the whole surface, so `MOUSEY` and
`RESULT:3` were one line height off and would have disagreed with the plane they are meant to click
on; both now use `view_h`. The hit test itself is a pixel lookup, not a rectangle test: the map
bitmap is never drawn, and the colour under the cursor *is* the button value, provided its alpha is
exactly 255 (`MoveMouse`, `:2009-2025`).

That same height is also a *clip*. Emuera's plane is painted on `MainPicBox`, so an entry reaching
past the console area is clipped by the control and can never touch the input box below it; erars
draws its input strip into the same surface, where the framebuffer edge alone would let the plane
paint over the strip, so `draw.rs` (`ScreenClip::below`) trims the quad and its UV window together
at `ClientHeight`. Nothing above it needs a clip: the console area starts where the framebuffer
does. The same `ScreenClip` now carries a positioned box's four-sided clip, since a box may be cut
on any edge.

**`GFILLRECTANGLE` has no colour argument, and `SPRITEGETCOLOR`'s return value is a typo.** Both
methods have zero call sites in either corpus, so both were ported from the C# rather than from a
script, and both have a trap in exactly the place a reader would skip.

`GraphicsFillRectangleMethod` (`Creator.Method.cs:6146-6169`) carries the doc comment
`GFILLRECTANGLE(int ID, int cARGB, int x, int y, int width, int height)`, and the
`argumentTypeArray` two lines below it has **five** entries and no colour: the body reads the id at
argument 0 and the rectangle at argument 1, so the surface is `(ID, x, y, width, height)`. The fill
colour is the per-bitmap brush, `brush ?? new SolidBrush(Config.BackColor)`
(`Content/GraphicsImage.cs:190-203`), which also means it *composites*: GDI+ `FillRectangle` blends
source-over where the `Graphics.Clear` behind `GCLEAR` (`:97-112`) replaces, so a translucent brush
tints the bitmap instead of overwriting it. erars follows the code, not the comment.

**The `WINAPI` refusal is not one method's, it is the whole GDI+ surface's, and erars now has it.**
`描画インターフェース:WINAPI` selects Emuera's Win32 text backend, and 28 method classes in
`Creator.Method.cs` — 35 of the names `Creator.cs` registers, at 30 sites, because
`GCREATEFROMFILE` and `GLOAD` each guard two overload paths — open with
`if (Config.TextDrawingMode == TextDrawingMode.WINAPI) throw new
CodeEE(Lang.Error.GDIPlusOnly.Text)` before reading a single argument — `GCREATE` at `:5875`,
`GDRAWTEXT` at `:5533`, `GFILLRECTANGLE` at `:6159`, `CBGSETG` at `:6570`, `GLOAD` at `:7080`. The
first pass read that as unreachable ("erars has no GDI text-drawing mode"), which was wrong twice
over: erars parses the key into `EraConfig::text_drawing_mode`
(`crates/erars-compiler/src/parser.rs:613-614`, `:1171-1184`) and already enforced the refusal for
`GDRAWTEXT` alone, so the one method a script was most likely to notice was refused while `GCREATE`
beneath it happily allocated a bitmap. `run_builtin_method` now checks the whole set once, before
dispatch (`gdiplus_only`, `executor.rs`), which covers the 25 of the 35 erars implements. The set is
Emuera's, not "anything touching a bitmap": `SpriteStateMethod` (`:5759`, behind `SPRITECREATED`,
`SPRITEWIDTH`, `SPRITEHEIGHT`, `SPRITEPOSX`, `SPRITEPOSY`), `SpriteGetColorMethod` (`:5818`) and
`SpriteDisposeMethod` (`:6063`) never got the guard, and five more carry one the fork commented out
— `GROTATE` (`:5647`), `CBGCLEAR` (`:6489`), `CBGCLEARBUTTON` (`:6532`), `CBGREMOVEBMAP` (`:6551`)
and `CBGSETCIMG` (`:6627`) — so those keep answering. `tests/run_tests/winapi/` pins both halves
through a per-directory `emuera.config`: `gdiplus_only.erb` for the refusal, `unguarded.erb` for the
methods that still answer. Neither corpus selects `WINAPI` (`eraTHYMKR/emuera.config:11` is
`GRAPHICS`, `eramegaten_p_kr/emuera.config:11` is `TEXTRENDERER`), so this is fidelity for a
configuration no shipped game uses — which is exactly why it had gone unnoticed.

`SpriteGetColorMethod` (`:5818-5842`) is the one graphics method whose failure is `-1` rather than
`0`, for three distinct reasons: no such sprite, a sprite whose parent bitmap was `GDISPOSE`d
(`ASpriteSingle.IsCreated` is its parent's, `CroppedImage.cs:74-77`), and a point outside
`DestBaseSize`. A point that is inside the sprite but lands outside the *parent* is not a failure at
all — it is `Color.Transparent`, i.e. `0x00FFFFFF` (`:78-89`). The bounds test runs before the read
and `SpriteAnime.IsCreated` is hard-coded `true` (`:257-260`), so an animated sprite answers `-1`
outside its box and only an inside point reaches `SpriteAnime.SpriteGetColor`, whose whole body is
`throw new NotSupportedException()` (`:273-286`); erars raises a script error there.

The return value is where the fork and the wiki part company, and this is the one place the wiki
wins. `:5840` reads `return ((Int64)c.A) << 24 + c.R << 16 + c.G << 8 + c.B;`, and in C# `+` binds
tighter than `<<`, so the expression is `((A << (24 + R)) << (16 + G)) << (8 + B)` — three shifts
whose counts are pixel channels. For all but a handful of colours the bits leave the top of the
`Int64` and the method answers 0; only the wiki's documented `0xAARRGGBB` makes it useful, and
reproducing the precedence bug would leave erars with a pixel reader that reads nothing. Pinned by
`tests/run_tests/basic/graphics.erb` (the `SGC` rows) and
`tests/run_tests/basic/sprite_get_color_anime.erb`.

Porting these two also fixed the shared rectangle reader. `ReadRectangle` rejects a **zero** extent
alongside the `int` range (`:5166-5173`) — a negative one is legal and mirrors — where erars'
`read_rect` accepted zero and silently drew nothing, so `GCLEAR`, `GDRAWG`, `GDRAWSPRITE` and
`GFILLRECTANGLE` all now raise on a degenerate rect
(`tests/run_tests/basic/graphics_rect_zero.erb`). `Bitmap::clear_rect` also clamped only the far
edge of its clip, which panicked on a rect starting past the bitmap's right edge on a row that was
inside it; the clip is now shared with `fill_rect` and clamps both ends.

### 5.12 The `[…]` preprocessor, `#` directives and debug mode

`PPState` (`GameProc/ErbLoader.cs:140-291`), the `;!;`/`;#;` markers
(`Sub/LexicalAnalyzer.cs:753-765`, form-string twin `:959-970`), Emuera's debug mode
(`Program.cs:219-220`, `GameProc/Process.ScriptProc.cs:33-40`) and the six `#` directives
(`GameProc/LogicalLineParser.cs:36-253`) are now ported. What follows is what is *not* identical.

| Deviation | erars | Emuera |
|---|---|---|
| the debug flag is spelled `--debug`, to match `--load`/`--save`/`--quite` | `crates/erars-stdio/src/main.rs:49-50`, `crates/erars-renderer/src/main.rs:42-43` | `-DEBUG` (`Program.cs:219-220`) |
| `デバッグコマンドを使用する` gates only the debug console's generic fall-through, never the `--debug` preprocessor mode. It is read (`crates/erars-vm/src/debug_console.rs:199`, §5.16); neither corpus sets it | `EraConfig::use_debug_command` | `Config/ConfigData.cs:57`, `:214`, `GameView/EmueraConsole.cs:1379-1382` |
| a region left open at end of file is reported at the **end of the source**; Emuera reports it with no line at all | `next_raw_line`, `crates/erars-lexer/src/lib.rs:446-457` | `new ScriptPosition(filename, -1)` (`ErbLoader.cs:436-437`) |
| every message is Korean with the Japanese original quoted at its citation, per `crates/erars-compiler/src/error.rs:12-24`. Where Emuera has three or four sentences differing only in the directive name, one function takes the `EventFlags` and cites all of them | `mod pp_msg`, `crates/erars-lexer/src/lib.rs:83-142`; `mod sharp_msg`, `crates/erars-compiler/src/parser.rs:105-229` | `_Library/EvilMask/Lang.cs:747`, `:757-767`, `:840-872` |
| a level-1 warning raised *before* a fatal `Err` on the same file is lost, because the hard-error path returns without draining the warning channel | `parse_and_compile`, `crates/erars-compiler/src/parser.rs:3142` | every warning is emitted as it is raised |
| `#FUNCTION` on a label Emuera would call a system function is rejected only for the **nine event names**; Emuera's `label.Depth == 0` covers its whole system-label set (`SHOW_STATUS`, `USERSHOP`, `COM<n>`, `ABLUP<n>`, …, `GameData/IdentifierDictionary.cs:74-116`), which erars does not enumerate at parse time. Zero corpus sites | `push_info`, `parser.rs:2832-3055` | `LogicalLineParser.cs:167-171` |
| `#PRI`+`#LATER` **on one definition** keeps the last flag, so the body runs once; Emuera sets both booleans and `LabelDictionary` registers that body twice, so it runs twice 「eramakerの仕様」. `#SINGLE`+`#PRI` likewise keeps only `#PRI`. The `PriWithLater` warning *is* raised, so the divergence is in dispatch, not diagnosis | `EventFlags` is one enum, `crates/erars-ast/src/event.rs:41-54` | `LabelDictionary.cs:101-104`, lists at `:112-115` |

**Corpus exposure of the single-flag model, measured, not assumed.** Exactly one definition in
either corpus carries two flags: `eraTHYMKR/ERB/SYS/TRAIN_BEFORE.ERB:154-157`, `@EVENTCOMEND` with
`#SINGLE` then `#PRI`, where erars keeps `Pre` and drops `Single`. No definition carries
`#PRI`+`#LATER`: the `@EVENTTURNEND` pair at `:333-334` and `:381-382` is **two separate
definitions** of the same name, one flagged each, which Emuera also treats independently — so no
`PriWithLater` fires on either corpus. A survey that groups sharp directives by *function name*
instead of by definition reports this pair as a double-flagged function; it is not one.

**Not applicable rather than skipped.** `LocalIsProhibited` (`Lang.cs:870`) cannot fire: it is
guarded by `getLocalIsForbid`, a config erars has no equivalent of. `SortLabels`' `localMax` fold
(`LabelDictionary.cs:95-98` folded, `:116-123` written back) is a no-op for the labels that reach it — an event function's
`#LOCALSIZE` is ignored, so its `LocalLength` is 0 and the maximum collapses to the
`!VariableSize.csv` default. `#FUNCTION`'s four flag-clearing warnings
(`LogicalLineParser.cs:178-197`) are unreachable in Emuera too: every flag guard `break`s before
setting its boolean unless the label is an event function, and `#FUNCTION` rejects event functions
at `:167-171` first.

**Skipping is per physical line, and that costs 6%.** The previous implementation jumped from
`[SKIPSTART]` to `[SKIPEND]` with a compiled DFA; a directive inside a skipped region was therefore
invisible, which is exactly what the 26 warnings below are made of. The port reads each physical
line as `ReadEnabledLine` does, with its `{`/`}` continuation handling disabled while skipped
(「[SKIPSTART]～[SKIPEND]中にここが誤爆するので無効化」, `Sub/EraStreamReader.cs:95-106`), and the
markers are recognised in `skip_ws`/`cut_comment` rather than behind a whole-file `memmem` gate —
two bytes after each `;`, three at a line's first `;`, which is cheaper than an extra pass over the
file. Interleaved A/B on eraTHYMKR (857 files, 61.8 MB, five alternating runs of min-of-3, loaded
box): preprocess+lex CPU **99.7 ms → 106.1 ms, +6.4%**, against a whole-load cost of ~600 ms.

**The 26 eraTHYMKR warnings.** All are `[SKIPSTART]`/`[SKIPEND]` mismatches in character event
files, all content-benign, and all matched site by site against an independently written `PPState`
port: 21 × `UnexpectedSkipend` (`ERB/CHARA/078 누에/EVENT_K78_RR.ERB` ×18, `073 이치린/EVENT_K73.ERB`
×2, `079 하타테/EVENT_K79.erb:2008`) and 5 × `DuplicateSkipstart`
(`052 텐시/YM/EVENT_K52.ERB:3498`, `057 키스메/EVENT_K57.erb:3977`, `062 오린/EVENT_K62.ERB:3169`,
`077 뱌쿠렌/EVENT_K77.erb:3769`, `150 해바라기 요정/EVENT_K150.ERB:3284`). eraMegaten predicts and
emits **none**. A `[ENDIF]` closing a `[IF …]` written on line 1 behind a UTF-8 BOM is *matched*:
.NET's `StreamReader` consumes the BOM (`EraStreamReader.cs:45`, two-argument constructor, so
`detectEncodingFromByteOrderMarks` is on) and so does erars' decoder. A simulation that keeps the
BOM in line 1 mispredicts two extra `UnexpectedMacroEndif` in `ERB/CHARA/013 첸/EVENT_K13_C6.ERB:71`
and `EVENT_K13_T.ERB:372`.

**Debug mode changes what the default path parses, by design.** `DEBUGPRINT*`, `DEBUGCLEAR` and
`ASSERT` are `DEBUG_FUNC` (`GameProc/Function/FunctionIdentifier.cs:372`, `:450`, `:472`) and are
skipped *without their arguments being parsed* (`Process.ScriptProc.cs:33-40`,
`Function/ArgumentParser.cs:22-27`), so erars elides them to `Stmt::Nop`. They cannot become comment
lines — 「SIF文のためにコメント行扱いにはできない」 — so the elided line still fills a statement
slot, which `tests/run_tests/control_flow/assert_zero.erb` pins. The measurable consequence is the
81 fewer interner entries §3 accounts for.

Pinned by `crates/erars-compiler/tests/preprocessor.rs` (31 cases: every `pp_msg` and `sharp_msg`
text with the line it is attributed to, plus branch selection in both debug modes) and by the four
fixtures in `tests/run_tests/preprocessor/`.

### 5.13 User-function argument typing

Emuera type-checks a call's arguments in `ConvertArg`
(`GameProc/Process.CalledFunction.cs:146-223`) and offers two compatibility escapes, both **`NO` in
both corpora** (`eraTHYMKR/emuera.config:63-64`, `eramegaten_p_kr/emuera.config:62-63`). erars implements
neither key and is laxer at two of the three decisions:

| Case | erars | Emuera with the corpora's config |
|---|---|---|
| number passed to a string parameter | silently stringified — `VmVariable::set`'s "auto convert int to string" arm, `crates/erars-vm/src/variable.rs:1432-1436`. Verified: `CALL FOO, 1` into `@FOO, ARGS` prints `1` | error `CanNotConvertIntToStr` naming `ユーザー関数の引数に自動的にTOSTRを補完する`; with the key `YES` the term is wrapped in `TOSTR` (`:210-218`) |
| string passed to a number parameter | error, at run time, with the generic message `Set argument` (`terminal_vm.rs:201`) | error `CanNotConvertStrToInt`, no config escape (`:203-207`) |
| argument omitted and the parameter declares no default | filled with `0` / `""` (`set_or_default`, `variable.rs:1408-1424`) | error `CanNotOmitArgWithMessage` naming `ユーザー関数の全ての引数の省略を許可する` (`:190-200`) |

Both corpora also set `ロード時に引数を解析する:NO` (`Config/ConfigData.cs:86`), so Emuera defers
its own check to call time as well; the divergence is in the verdict, not the timing. A plain
`LOCALS = 7` is *not* one of these cases: Emuera reads the right-hand side of a string assignment as
a form string (`Function/ArgumentBuilder.cs:1036-1043`), so it stores `"7"` exactly as erars does,
and `文字列変数の代入に文字列式を強制する` — `SystemIgnoreStringSet`, `NO` in eraMegaten
(`eramegaten_p_kr/emuera.config:67`) — would forbid the `=` form outright rather than type-check it.

### 5.14 Wiki-gap batch: in-expression functions and variables

Eight functions and six variables the wiki lists and erars did not answer are now implemented
(`crates/erars-ast/src/command.rs:237-263`, `crates/erars-vm/src/terminal_vm/executor.rs:1921-2030`,
`crates/erars-ast/src/variable.rs:55-59`). Six of the fourteen match Emuera exactly and are not
listed; the divergences are:

| Item | erars | Emuera |
|---|---|---|
| `COLOR_FROMNAME` resolves CSS colour names (`css_color`) | `crates/erars-vm/src/terminal_vm/executor.rs:1961-1991` | `Color.FromName`, .NET's `KnownColor` set (`Function/Method/MethodBase.cs`, `ColorFromNameMethod`) |
| system names (`Control`, `Window`, …) are **not** colours and return `-1`; `transparent` is refused with `TransparentUnsupported` 「透過色は指定できません」 | same file | .NET resolves system names against the desktop theme, so `COLOR_FROMNAME("Control")` yields a live theme colour |
| a `#RRGGBB` literal is **not** accepted (`-1`), matching Emuera | — | `Color.FromName` only knows names |
| `CBRT` is `x.powf(1.0/3.0).round()` | `:1921-1929` | `Math.Pow(x, 1.0/3.0)` then `(long)`, i.e. truncation — so Emuera's `CBRT(64)` is 3 and `CBRT(1000)` is 9. erars rounds and gets the same two values; the divergence is only visible for a perfect cube whose `powf` lands below the integer, where erars is the *more* faithful answer to the wiki's 「立方根」 |
| `ISTIMEOUT` is **inferred**, not signalled: it is true only when the input deadline has passed *and* the value the input returned equals `Timeout::default_value` | `crates/erars-vm/src/context.rs:34`, `:151`, `:165-168` | a flag set by the input routine itself (`Process.cs`, `TimeoutManager`) |
| `GAMEBASE_GAMECODE` is the Emuera spelling; erars accepts `GAMEBASE_CODE` too, because that is the only spelling it ever offered | `crates/erars-ast/src/variable.rs:25-29` | `GAMEBASE_GAMECODE` only (`GameData/Variable/VariableData.cs:305`) |

`ISTIMEOUT`'s inference is exact for the frontends that enforce deadlines
(`crates/erars-renderer/src/app.rs`) and always `0` under `erars-stdio`, which ignores timeouts —
the same as Emuera's console build, where nothing times out either.

`GETLINESTR` takes its width from the console's line width, `PRINTCLENGTH` from
`PRINTC_COUNT`×`PRINTC_WIDTH`; both are pinned at the eraMegaten/eraTHYMKR default of 84 columns and
25 print-C columns by `tests/run_tests/methods/wiki_methods.erb`. `STRFORM` compiles its argument as
a form string in the *caller's* frame, reusing `InstructionWorkflow::EvalFormString`
(`crates/erars-vm/src/terminal_vm.rs:41`, `:79-107`) rather than a second expression parser, so
`%STR:1%` and `{2 * 3}` inside the built string see the caller's locals. `GETSPCHARA` searches only
`CFLAG:0 != 0` characters and refuses with `SPCharacterFeatureDisabled` when
`SPキャラを使用する:NO` (`crates/erars-vm/src/variable.rs:1229-1246`), which
`tests/run_tests/spchara/getspchara.erb` pins together with the "cleared `CFLAG:0:0` hides the
character from `GETSPCHARA` but not from `GETCHARA`" case.

`TFLAGNAME`, `CDFLAGNAME1` and `CDFLAGNAME2` are constant-name arrays like `PALAMNAME`, declared in
`crates/erars-loader/src/variable.yaml:123-138` and fed from the matching CSV by the `NAMES` table at
`crates/erars-vm/src/variable.rs:1294-1296`. `MONEYLABEL` reads `Config.MoneyLabel` — the
`お金の単位` replacement — not a variable, so it follows `_Replace.csv`
(`crates/erars-vm/src/terminal_vm/executor.rs:417`).

### 5.15 `emuera.config` keys: honoured, and reported-but-not-honoured

`EraConfigKey` grew from the display-only set to 51 further keys
(`crates/erars-compiler/src/parser.rs:396-564`), each carrying its Japanese label as its
`to_string` and its `Config/ConfigData.cs` line as a doc comment, plus 37 fields on `EraConfig` with
Emuera's own defaults (`:616-721`) and two new flag enums, `ReduceArgumentOnLoadFlag` and
`DisplayWarningFlag` (`:1182-1209`). Seven of them change what the loader does:

| Key | Effect | Emuera |
|---|---|---|
| `サブディレクトリを検索する` | `let subdir = if config.search_subdirectory { "/**" }` on the CSV, ERH and ERB globs, `crates/erars-loader/src/lib.rs:205-214` | `HeaderFileLoader.cs:381` |
| `_Rename.csvを利用する` / `_Replace.csvを利用する` | guarded match arms `"_RENAME" if config.use_rename_file`, `"_REPLACE" if config.use_replace_file`, with a skip arm, `:347-360` | `Process.cs:96`, `:119` |
| `CALLNAMEが空文字列の時にNAMEを代入する` | a post-chara-merge pass over the templates, `:400-410` | `ConstantData.cs:1239-1244` |
| `セーブデータをsavフォルダ内に作成する` | `fn sav_path`, `:63-70`, used by both `load_script` and `run_script` | `Config.cs:229-234` |
| `イベント関数のCALLを許可する` | `FunctionDic::compati_call_event` (`crates/erars-vm/src/function.rs:112`) makes `insert_compiled_func` register an event function under its plain name as well, first definition winning via `entry().or_insert_with()` (`:267-268`) | `LabelDictionary.cs:82-84` |
| `表示する最低警告レベル` | warnings carry their level in `ParserWarning` (`crates/erars-compiler/src/error.rs:14`) and the loader drops those below the threshold, `crates/erars-loader/src/lib.rs:504` | `ParserMediator.cs:26` |

Warning **levels are a property of origin, not a threaded parameter**: the two streams are tagged
exactly where they merge in `parse_and_compile` — preprocessor diagnostics at level 1 via
`pp_warnings` (`crates/erars-compiler/src/parser.rs:2201`, `:3834`) and line-compiler diagnostics at
level 2 (`:3809-3814`), matching `ErbLoader.cs:154-171`, `:239-252` and `:1041-1058`. `erars-lint`'s
own `W1001` is deliberately unfiltered: it has no Emuera counterpart to take a level from.

**DELIBERATE, `search_subdirectory` is a superset.** With the key on erars reads *non*-character
CSVs recursively too, where Emuera recurses for `CHARA*.CSV` alone
(`GameData/ConstantData.cs:1236`) and reads `CSV/` top level only otherwise. Measured, no corpus
file depends on the difference: every CSV in a subdirectory is a `Chara*` file — 2,365 in
eraMegaten, 226 in eraTHYMKR, none other. Emuera's defaults for `search_subdirectory`,
`use_save_folder` and `use_rename_file`/`use_replace_file` are all **off**, and so are erars'
now, so a game shipping no `emuera.config` behaves as Emuera does rather than as erars used to.

**The config is three files, not one, and reading only the middle one was a real bug.** Emuera
applies `csv/_default.config`, then `emuera.config`, then `csv/_fixed.config` onto a single
`ConfigData` (`Config/ConfigData.cs:642-664`), so a game ships overridable defaults in the first and
settings the user *cannot* override in the third. erars read only `emuera.config`
(`crates/erars-loader/src/lib.rs:40`, as it was), which is precisely why making these keys
config-gated broke loading the game as it ships: eraMegaten pins `_Rename.csvを利用する`,
`_Replace.csvを利用する` and `サブディレクトリを検索する` in `Data/CSV/_fixed.config` — none of them
in any user file — so without that file erars skipped the rename table and read only the top-level
ERB, turning 7 errors into 20 plus 26 extra `W1001`s of unrenamed `[[店舗:종류]]` names. eraTHYMKR
ships both files too.

Fixed by `load_config` (`crates/erars-loader/src/lib.rs:54-95`), which applies the same three in the
same order through the new `EraConfig::merge_text`
(`crates/erars-compiler/src/parser.rs:878-892`) — a per-file overlay that leaves keys the file does
not mention alone, where `from_text` was whole-config-from-defaults. A missing file is skipped
silently, matching `loadConfig`'s `return false` on a failed open (`:666-670`). The `fix` flag of the
third load has no runtime effect at all in Emuera — it only greys the widget out in the config
dialogs (`Forms/ConfigDialog.cs:287-311`, `Forms/DebugConfigDialog.cs:74-98`) — so load order alone
reproduces it. All three are resolved case-insensitively, like every other path this loader takes
(`:230-234`), because Emuera's filesystem is. Pinned by `config_cascade_precedence`
(`crates/erars-loader/src/lib.rs:699-751`), which gives one key to all three files and one key to
each, so a wrong order is a wrong value rather than a missing one.

An eighth is read at runtime rather than by the loader: `デバッグコマンドを使用する` gates the debug
console's generic fall-through, exactly as in Emuera (`crates/erars-vm/src/debug_console.rs:199`,
§5.16).

The remaining 43 keys parse, take their Emuera default, and are answered by `GETCONFIG` —
but do not change behaviour. Grouped by why:

| Keys | Why not honoured |
|---|---|
| `大文字小文字を区別しない`, `互換関数の大文字小文字を区別しない` | identifiers are interned case-folded, so erars is permanently in the `NO` state; honouring `YES` would mean a second, case-sensitive interner |
| `全角スペースを空白文字として扱う` | U+3000 is baked into the `logos` skip pattern (`crates/erars-lexer/src/lib.rs:394`, `:744`, `:1028`); a runtime switch would rebuild the DFA per game |
| `読み込み順をファイル名順にソートする` | erars always sorts: parallel load returns functions in completion order otherwise, so which duplicate definition wins would depend on thread timing (`crates/erars-loader/src/lib.rs:216-219`) |
| `オートセーブを行なう`, `無限ループ警告時間`, `ロード時の情報表示`, `ロード時に引数を解析する` | load-time UI and timing, no erars analogue |
| `呼ばれない関数を無視する`, `関数が見つからない警告`, `関数が呼ばれない警告`, `後方互換性の警告`, `関数の上書きを許可する`, `関数の上書き警告`, `通常関数の上書き警告` | erars' diagnostic set is the one in §5.12/§6, not Emuera's whitelist of load-time advisories |
| `解釈不可能な行があっても実行する` | erars is always lenient — §5.7 |
| `RANDの互換性`, `TIMESを厳密に計算しない`, `TARGETを設定しない` | numeric/dispatch compatibility shims for pre-1.7 scripts; erars implements the modern behaviour only |
| `セーブデータをバイナリ形式で保存する`, `セーブデータをUTF-8で保存する` | erars' save format is its own (`crates/erars-vm/src/save.rs`); it is neither Emuera's binary nor its text form |
| `改行を1739として扱う`, `ONEINPUTで2文字以上の入力を許可する`, `ボタンの折り返し`, `キーマクロを使用する` | frontend input/layout details owned by `erars-renderer` |
| `キャラクタ変数の引数を補完しない`, `ユーザー関数の引数に自動的にTOSTRを補完する`, `文字列変数の代入に文字列式を強制する` | §5.13 |
| the 13 `_Replace.csv`-backed keys (`お金の単位`, `単位の位置`, `起動時簡略表示`, …) | already carried by `ReplaceInfo`; the config arm is a no-op so that `GETCONFIG` can answer them from there |

**`GETCONFIG` is deliberately a superset.** Emuera answers only a whitelist and errors with
「コンフィグ文字列"{0}"の値の取得は許可されていません」 (`NotAllowGetConfigValue`,
`Config/ConfigData.cs:485-556`) for everything else; erars answers every key it knows, booleans as
`0`/`1`, colours as `0xRRGGBB`, replace items as strings
(`EraConfig::get_config`, `crates/erars-compiler/src/parser.rs:787-870`). This is what removes
eraMegaten's five `GETCONFIG("オートセーブを行なう")` runtime bails. The three argument errors are
Emuera's, distinctly: 「第1引数が空文字列です」, 「文字列"{key}"は適切なコンフィグ名ではありません」
and 「型が違います (GETCONFIGS関数を使用してください)」
(`crates/erars-vm/src/terminal_vm/executor.rs:2814-2842`). Presentation-only keys that erars has no
field for get **no enum variant at all**: `from_text` already drops an unparsed key silently
(`if let Ok(key) = key.parse()`), which is exactly the required "accept and ignore with no
diagnostic", and 25 dead variants would only be noise.

*Measured span of the superset, and its exposure.* Emuera's `GetConfigValueInERB` switch reaches
exactly **26** keys; every other name falls to its `default` arm. Driving all 75 keys of the wiki's
`emuera.config` page through a real VM (§3.1) shows erars answering **36** that Emuera refuses —
12 of those 75 are on Emuera's whitelist, 63 are not, and erars answers 48 of the 75 in total. The
divergence is confined to *which names are accepted*: the three argument errors above are byte-for-byte
Emuera's, and every accepted key returns the value Emuera's own default table gives it, so no
accepted key returns a *different* answer than Emuera would if Emuera answered it.
Corpus exposure is **zero**, measured rather than assumed: both corpora together use six distinct
keys — `フォントサイズ` ×47, `描画インターフェース` ×9, `オートセーブを行なう` ×5, `ウィンドウ幅` ×3,
`一行の高さ` ×1, `PRINTCの文字数` ×1 — and all six are inside Emuera's 26. So the five eraMegaten
bails this removed were bails on a **whitelisted** key that erars simply did not know yet; nothing
in either corpus depends on the part of the superset that exceeds Emuera. Recorded once here and
not re-decided.

**The eraTHYMKR warning count is a config finding, not a parser one.** eraTHYMKR ships
`表示する最低警告レベル:2` (`eraTHYMKR/emuera.config`), so Emuera shows **none** of the 26
`[SKIPSTART]`/`[SKIPEND]` warnings of §5.12, and erars now shows none either: the shipped tree
produces **0 diagnostics**. The 26 stay reproducible byte for byte by copying the tree with the
key set to `1`. eraMegaten sets `0`, so its counts in §6 are unchanged: 7 `E2000`, 9 `W1001`,
4 `W2000`.

### 5.16 The debug commands and the debug console

**Both halves are implemented; four of the console's *effects* still differ, and each difference is
named below.** The `DEBUGPRINT` family and the five `@` commands of the wiki's debug page are
separate features that happen to share a page — the coverage table shows both as `ran` (§3.1,
sections (a) and (f)).

*The `DEBUGPRINT` family.* The whole `DEBUG_FUNC` family really executes, and it obeys Emuera's mode
gate.
Emuera drops a `DEBUG_FUNC` line before its arguments are ever parsed when `-DEBUG` is off
(`GameProc/Process.ScriptProc.cs:33-40`, `GameProc/Function/ArgumentParser.cs:22-27`), but cannot
make it a comment — 「SIF文のためにコメント行扱いにはできない」 (`Process.ScriptProc.cs:35`) — so it
still occupies one statement slot for `SIF` to bind to. erars does exactly that at preprocess time
(`crates/erars-compiler/src/parser.rs:2515-2536`), which is why the release path parses 90 fewer
lines on eraMegaten and interns 81 fewer strings (§3). With the mode on, `DEBUGPRINT`/`DEBUGPRINTL`/
`DEBUGPRINTFORM`/`DEBUGPRINTFORML` append to a separate flat buffer exactly as Emuera's
`DebugPrint`/`DebugNewLine` do (`GameView/EmueraConsole.cs:1837-1854`,
`crates/erars-ui/src/lib.rs:715-726`, driven from
`crates/erars-vm/src/terminal_vm/executor.rs:198-207`), `DEBUGCLEAR` empties that buffer and nothing
else (`:3929`), and `ASSERT` builds its argument and fires on zero (`:4426-4435`).
Both modes are pinned by fixtures: `tests/run_tests/basic/debug_func_debug.erb` (mode on — the flat
buffer's continuation/termination rule, `DEBUGCLEAR`, a passing `ASSERT`, and a `SIF` whose body is a
debug line that really runs) against `tests/run_tests/basic/print_misc_commands.erb` and
`tests/run_tests/control_flow/assert_zero.erb` (mode off — the same commands produce nothing and a
zero `ASSERT` cannot fire, while the statement slot survives). A fixture whose stem ends `_debug` is
parsed with `with_debug(true)` (`tests/run_tests.rs:28-44`), and the debug buffer is rendered under a
`--- debug console ---` separator so it is comparable at all (`:134-144`).
Debug output also reaches the log unconditionally via `log::debug!`
(`crates/erars-vm/src/terminal_vm/executor.rs:204`), so it is not written into a buffer nobody reads.

*Where the console lives.* Emuera's entry point is in the front end, and only because
`EmueraConsole` owns the input loop: `PressEnterKey` sees the line the user typed, spots a leading
`@`, calls `doSystemCommand` and leaves the pending input request *unconsumed*, so the same prompt
comes back once the command has run (`GameView/EmueraConsole.cs:1103-1110`, `:1321-1390`). In erars
the **VM** owns that loop, so the interception is one level lower and the semantics stay in the
engine: `VmContext::input_redraw` is a loop that classifies the answer
(`crates/erars-vm/src/debug_console.rs:72-107`), echoes it, runs it
(`:159-209`) and re-issues the very same `InputRequest`
(`crates/erars-vm/src/context.rs:177-238`). No `SystemFunctions` method was added and no front end
reimplements anything — stdio, the TUI, the proxy, `NullSystemFunctions` and the coverage harness all
get the console from the same code. A front end only has to hand text over: `erars-stdio` forwards a
line starting with `@` verbatim before it looks at the request type
(`crates/erars-stdio/src/stdio_frontend.rs:164-176`), which matters for an `INPUT`/`ENTER` prompt that
would otherwise reject or ignore it.

*The gates are Emuera's, and they are not the gate one expects.* 「デバッグコマンドを使用する」 gates
**only** the generic fall-through (`:1377-1388`); `@DEBUG` is gated on `Program.DebugMode`
(`:1367-1373`); `@CONFIG`, `@EXIT`, `@OUTPUT` and `@REBOOT` are gated on nothing and work in an
ordinary run. Ahead of all of them sits `if (timer.Enabled)`, which refuses any command while a
`TINPUT`-family deadline counts down (`:1323-1329`) — in erars, exactly the requests carrying a
`Timeout` (`crates/erars-vm/src/context.rs:217-221`). Emuera's second guard, `if (IsInProcess)`
(`:1330`), is not reproduced: it is true only while a script is scanning or has been re-entered, and
reaching this code in erars *means* the VM is parked in an input request — the same reasoning
Emuera's own always-true `IsActive` gets (§5.10). `デバッグコマンドを使用する` is now read
(`crates/erars-vm/src/debug_console.rs:199`), so §5.12's list of parsed-but-unread keys is one
shorter, and `--debug` sets the engine flag `@DEBUG` consults
(`crates/erars-stdio/src/main.rs:127-131`) on both the source and the compiled `--load` path.

*Per command, including where erars diverges.* Each divergence below carries a `DELIBERATE` comment
at the code that causes it.

| command | erars | divergence |
|---|---|---|
| `@OUTPUT`, `@OUTPUTLOG` | Writes the console to `<game>/emuera.log`, UTF-16LE with a BOM and CRLF ends, and prints 「※※※ログファイルを…に出力しました※※※」. Emuera's `WorkingDir` is derived as the inverse of `sav_path`, because `セーブデータをsavフォルダ内に作成する` decides whether `sav_dir` is `<game>/sav` or `<game>` itself; climbing to the parent unconditionally wrote the log *outside* the game at that key's default of `NO`, which the CLI smoke below caught | none — `@OUTPUT` and the `OUTPUTLOG` instruction call one function (`debug_console::output_log`, wired at the `BuiltinCommand::OutputLog` arm of `executor.rs`), so they cannot drift. Emuera's `OutputSystemLog` differs from `OutputLog` only by omitting the `../` test, unobservably: its argument is the fixed `WorkingDir + "emuera.log"` (`GameView/EmueraConsole.Print.cs:683-736`) |
| `@EXIT`, `@QUIT` | Ends the run cleanly | none. `window.Close()` (`:1357-1361`) ends the process; erars raises `DebugConsoleQuit`, caught in one place (`crates/erars-vm/src/terminal_vm.rs:297-308`) and turned into the same clean exit `Workflow::Exit` takes — not a reported VM error. The marker exists because the command runs several frames below, inside an input request, where no return value can carry a workflow |
| `@REBOOT` | Ends the run the same way and records the request; `VmContext::reboot_requested()` reports it (`crates/erars-vm/src/context.rs:111-117`) | **the reload is not wired up.** `window.Reboot()` sets `Program.Reboot` and closes, and Emuera's `Main` loop rebuilds the engine from scratch (`Forms/MainWindow.cs:807-812`). erars stops at the flag: rebuilding needs the loader, which `erars-stdio` runs once (`crates/erars-stdio/src/main.rs:103`) and does not re-run. So `@REBOOT` today is `@EXIT` plus an answered question, and any front end that wants the restart can already ask |
| `@CONFIG` | Prints every config key as `name:value`, under Emuera's own key names (`debug_console::config_lines`) | **read-only, and a superset.** `ShowConfigDialog` opens a form that can *edit* and save, and reboots on `SaveReboot` (`Forms/MainWindow.cs:841-855`); erars renders the same content as text, because the alternative is faking a dialog. It also lists every key erars knows rather than only those with a widget (`Forms/ConfigDialog.cs:315-345`) — the same superset `GETCONFIG` already answers for (§5.15) |
| `@DEBUG` | In a `--debug` run, prints the executing line, the call stack innermost first, then the `DEBUGPRINT` buffer; otherwise refuses with 「デバッグウインドウは-Debug引数付きで…」 | **two of three tabs, and one line number.** `OpenDebugDialog` has three tabs (`GameView/EmueraConsole.cs:1814-1835`); the stack trace and console tabs are engine-owned and reproduced label for label from `GetDebugTraceLog` (`:1788-1812`), the **variable watch** is not — it evaluates expressions typed into the dialog's grid, which needs the interactive surface. And the trace's line number is erars': Emuera fills a trace entry at call time with the *callee's* declaration position and never touches it again (`GameProc/Process.State.cs:437`, `:459-461`), while erars reads the live call stack, whose outer frames each hold their own current position — the call site — because `update_position` writes only the innermost (`crates/erars-vm/src/context.rs:212-216`). Same frames, same order, different number |
| anything else after `@` | Refuses: 「デバッグコマンドを使用できない設定になっています」 when the gate is shut, and a plain statement that the feature is missing when it is open | **the evaluator is absent.** `DebugCommand` compiles the fragment and runs it against live memory (`:1377-1388`, `:1881-1960`). erars implements the gate and says so instead of pretending to run the line |

*Measured, not read off the source.* Section (f) of §3.1 answers an `INPUTS` with each command. The
command text alone proves nothing — `doSystemCommand` echoes the line before acting on it
(`:1336-1338`), so a working console prints it too; the evidence is what the *script* received.
`@CONFIG`, `@DEBUG` and `@OUTPUT` leave `RESULTS` holding the answer to the re-issued request, and
`@EXIT`/`@REBOOT` end the run before the statement after the `INPUTS`. Four fixtures pin the
behaviour with hand-derived expectations: `tests/run_tests/basic/debug_console.erb` (a normal run —
`@` alone, a near-miss name, `@DEBUG` refused, `@OUTPUT` accepted, then an ordinary answer),
`debug_console_window.erb` (`@DEBUG` in debug mode, with the trace and buffer), `debug_console_exit.erb`
and `debug_console_reboot.erb`. A fixture is run in debug mode when a `<stem>.debug` marker file
exists, its answers come from `<stem>.in`, and **every** fixture asserts `reboot_requested()` against
the presence of a `<stem>.reboot` marker (`tests/run_tests.rs`), so a command that sets that flag by
accident fails the suite.

*And exercised through the real CLI, which is what caught the `WorkingDir` bug above.* A four-line
game (`@SYSTEM_TITLE`, `PRINTL`, `INPUTS`, `PRINTFORML GOT=[%RESULTS%]`) driven by
`printf '@OUTPUT\n@DEBUG\nhello\n' | ./target/release/erars-stdio --quite <game>` prints the echo,
the log message, the `--debug` refusal for `@DEBUG` and then `GOT=[hello]`; the same game under
`--debug` with `@DEBUG`, `@nosuch`, `@CONFIG`, `@EXIT` prints the trace, the gate refusal, the whole
config listing and exits 0. The written `emuera.log` decodes as UTF-16LE with a BOM and CRLF ends,
and lands in the game directory with the save-folder key at both `NO` and `YES` — the check that
found the log being written one directory too high.

### 5.17 `PRINT*` accepts a suffix Emuera rejects

Emuera has no `PRINT` *prefix* rule. Every variant is its own entry in the
`FunctionCode` table — `PRINTFORMSLC`, `PRINTPLAINFORM`, `PRINTDATAKL` and the rest are each a
separate identifier, so a name that is not in the table is not a print statement at all and the line
is rejected. erars instead lexes the head with `parse_print_left`
(`crates/erars-lexer/src/utils.rs:287-318`): it consumes the flag letters it knows (`L`, `W`, `K`,
`D`, `C`/`LC`, `V`/`S`/`FORM`, `SINGLE`, `PLAIN`, `DATA`) and then **discards whatever is left**.
`PRINTZZNOSUCH` therefore lexes as a plain `PRINT` with the remainder thrown away, where Emuera
reports an unknown identifier.

Kept because the flag-set encoding is what makes the ~120 documented PRINT spellings one code path
instead of 120 enum discriminants, and every *spelling the wiki documents* is accepted with the right
flags — measured, section (a) of §3.1. What is lost is rejection of misspellings: a typo silently
prints instead of failing to load. Emuera would refuse the line and, with
`解釈不可能な行があっても実行する:NO`, refuse to start.

This also bounds the sweep, and §3.1 says so: **no `PRINT*` name can be proved absent by the
harness**, because every name in that family is accepted whether it exists or not. The
`harness_controls` test asserts the over-acceptance directly, so the limitation cannot quietly stop
being true.

---

## 6. eraMegaten (ShinEraTensei P 0.5.9) load report

`./target/release/erars-stdio --save --quite /home/riey/repos/eramegaten_p_kr/Data` exits 0 with
**6 errors, 9 `W1001` warnings and 4 `W2000` warnings** over 125,548 functions — the game as it
ships, with no user config and nothing assembled by hand. Every one of the 19 is a defect in the
game's own source, not an erars gap. They are enumerated here because "6 remaining errors" is
otherwise indistinguishable from "6 unimplemented features".

**The game directory is `Data`, not the repository root.** Emuera anchors everything on one
directory: `Program.cs:57-63` derives `CsvDir`, `ErbDir`, `DatDir`, `DebugDir` and `ContentDir` from
`WorkingDir` (`ExeDir` in the desktop build, the lines it replaced are still there commented out),
`Config/ConfigData.cs:24` puts `emuera.config` in the same place, and `Config/Config.cs:228-234`
puts `sav/` there too. eraMegaten's anchor is therefore `Data/`, which is where `CSV/` and `ERB/`
are — and the game's own `README.md` confirms it, telling the user to unpack `resources` "Data 안에"
and to edit `Data\emuera.config` for the font. Its `.gitignore` reserves exactly the anchor-relative
artifacts: `/Data/sav`, `/Data/debug`, `/Data/*.config`, `/Data/*.log`, `/Data/resources`. The
committed `emuera.config` beside `Emuera.exe` is a template, not the file the engine reads; the file
it reads is gitignored, which is why this checkout has no `Data/emuera.config` at all.

Pointing erars at the repository root instead fails with `No ERB script found in .../ERB`, and that
is correct rather than a gap: the root has no `ERB/`, and Emuera anchored there would throw out of
`Directory.GetDirectories`/`GetFiles` inside `Config.getFiles` (`Config/Config.cs:403-425`) for the
same reason. One anchor, and it is the directory holding the scripts.

Reproducing the numbers needs **no user config**, because a game can ship its own — see §5.15.

### 6.1 The 6 errors — all invalid ERB

Emuera rejects all six as well. It reports each one, clears `noError`
(`GameProc/ErbLoader.cs:403-407`, `:423-427`) and then **refuses to start the game**, because the
shipped `eramegaten_p_kr/emuera.config:49` sets `解釈不可能な行があっても実行する:NO`
(`GameProc/Process.SystemProc.cs:173-186`). erars reports the line, keeps the rest of the file — the
enclosing function is dropped where the failure consumed following lines, §5.7 — and starts.

| Game source | Defect | Why Emuera also refuses |
|---|---|---|
| `RPG/ダンジョンアタック/ダンジョンデータ/DUNGEON_5_大魔宮/EVENT5_大魔宮.ERB:751` | `LOCALS += @"%CALLNAME:LOCAL, "는")% …"` — the `조사처리(` opener is missing and a `)` is left over. The intended form is the macro `#DEFINE CALLNAME은 조사처리(CALLNAME,"는")` (`ERB/KR_FUNCTION/ZNAME.ERH:3-11`) | inside `%…%` the `,` starts the width expression, which is then the string `"는"` followed by an unmatched `)` |
| `口上/悪魔汎用会話口上/TALK_PUB324_男_シニカル.ERB:245` | `%CSTR:ARG:이인칭%%」` — one `%` too many; the second opens an interpolation that never closes | the trailing `%` opens a `LexEndWith.Percent` run that reaches end of line |
| `ＳＨＯＰ関連/116_アイテム合成.ERB:371` | `解説文 += @"{引数} 이상` — unterminated `@"` form literal | `ReadString` reaches end of line |
| `RPG/スキル関係/SKILL_ACTION_EXTRA.ERB:688` | `PRINTFORMW %조사처리(ARGS:0%,"을") 세트했다!` — the `%` sits before the `)` instead of after it | `LexEndWith.Percent` terminates at `%` only while `nestBracketS == 0 && nestBracketL == 0` (`Sub/LexicalAnalyzer.cs:844-852`); inside the call the `%` becomes `OperatorCode.Mod` (`:527-528`) and the following `,` breaks the expression |
| `RPG/スキル関係/52_アシストスキル/SKILL5620_コンバート.ERB:87` | same misplaced `%`, twice on the line | as above |
| `RPG/スキル関係/31_敵専用/SKILL2575_砂漠の風.ERB:111` | same misplaced `%` | as above |

### 6.2 The 9 `W1001` warnings — names in no CSV

`W1001` fires when a variable-index name resolves to nothing. All nine subjects are absent from
every CSV under `Data/CSV`, so Emuera raises `CanNotUseStringAsIndex`/`NotDefinedName` on the same
lines.

| Name | Lines | Diagnosis |
|---|---|---|
| `依存度のベクトル指定` ×6 | `調教関連/COM/COMF130_ダブル足扱き.ERB:160,162,164`; `COMF131_ダブル尻扱き.ERB:162,164,166` | a Japanese design note left in as an index; in no CSV |
| `LOCAL은` ×2 | `RPG/スキル関係/31_敵専用/SKILL2527_解除.ERB:211`, `SKILL2528_ドラゴンヘッド.ERB:107` | the Korean particle `은` is glued onto the variable name. The fork documents no 조사 auto-split; the game's own convention is the `#DEFINE CALLNAME은` macro |
| `인내력` ×1 | `RPG/依頼/REQUEST_99_ナナドラ/REQUEST_99  CHAPTER6.ERB:395` | misspelling — `Base.csv` has `인내력` nowhere; the intended name is the game's stamina entry |

### 6.3 The 4 `W2000` warnings

`CONTINUE`/`BREAK` outside a loop, which Emuera also warns about and keeps
(`ErbLoader.cs:1041-1058`, `GameData/ParserMediator.cs:118-131`; §5.7):
`RPG/依頼/REQUEST_36_白き刃の後継者/REQUEST_36_白き刃の後継者_ENEMY_AI.ERB:466,469` (`BREAK`),
`RPG/戦闘/LINKAGE.ERB:288` and
`RPG/スキル関係/50_システム・基本行動/SKILL2311_変身悪魔変更.ERB:124` (`CONTINUE`).

### 6.4 Where the game stops

Loading succeeds and the title sequence runs to `タイトル表示/PRINT_TITLE.ERB:67`,
`TITLE_NO = TITLE_LIST:(RAND:CNT_TITLE_PICTURE)`. `CNT_TITLE_PICTURE` is counted at `:20-51` by
`GCREATEFROMFILE` over `Data/resources/タイトル画像/`, which the repository does not ship — it is
`.gitignore`d and `追加画像について.MD` points at an external download
(`https://uu.getuploader.com/eraMegaten_P/download/18`). With no art the count is 0, and `RAND:0`
is a script error in Emuera as well: the `RAND` pseudo-variable refuses a non-positive argument
instead of sampling an empty range (`GameData/Variable/VariableToken.cs:1459-1465`; the `RAND()`
*method* refuses the same way at `Creator.Method.cs:2953-2961`). This is a missing asset pack, not
an engine gap — erars reports it at the correct line with the same reason, and
`tests/run_tests/basic/rand_zero.erb` pins it.

Confirmed by supplying the asset: with a single `resources/タイトル画像/タイトル003.webp` in the run
root, `GCREATEFROMFILE` succeeds, `CNT_TITLE_PICTURE` becomes 1, `PRINT_TITLE` completes and the
game reaches its interactive title menu — `[0] ＮＥＷ ＧＡＭＥ` / `[1] ＬＯＡＤ ＧＡＭＥ` — with no
VM error anywhere in the run. The game's five `GETCONFIG("オートセーブを行なう")` sites
(`ERB/SYSTEM.ERB:1071`, `:1104`, `:1125`, `:1131`, `ERB/ＳＨＯＰ関連/100_ターンエンド.ERB:345`) are
turn-end paths the title sequence does not reach, but each one used to be an unconditional runtime
bail on an unknown config name; the key now answers `1`, asserted by
`get_config_reports_bools_as_ints_and_reads_replace_items`
(`crates/erars-compiler/src/parser.rs:4089`).
