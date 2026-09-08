# Language Feature Gap Inventory → Fixes → Merge — 2026-09-06

This is a running log across several sessions the same day; each numbered section below names the
`master` commit(s) it produced, not a single frozen endpoint — a "final SHA" line here would go
stale the next time this doc is appended to, the way this line itself did once before. §4 merged
`fix/executor-crash-class` (tip `723567b`) then `feature/bulk-array-assign` (tip `5002181`), then a
direct `master` commit for `.gitignore`'s `/.worktrees` entry; §6 merged `fix/deldata-missing-file-
noop` (tip `2034f6b`) as `a06607c`, then committed doc updates as `89cb290`. All local only, never
pushed — check `git log`/`git rev-parse master` for the actual current tip rather than trusting a
number written down here.

## 1. The inventory: corpus-driven method and what it found

`docs/research/2026-09-06-language-feature-gap-inventory.md` (peer session,
`lang-gap-inventory`) inverted the usual approach to gap-finding. Instead of
diffing erars's AST/`BuiltinVariable`/`BuiltinMethod` enums against the
Emuera wiki and reporting every absent variant, it:

1. Scanned both shipping corpora (`/home/riey/repos/eraTHYMKR`, 873 files,
   ~1.05M lines; `/home/riey/repos/eramegaten_p_kr/Data`, 8,779 files, ~3.5M
   lines) for every built-in statement, function call, and operator
   expression actually *executed*.
2. Cross-referenced each real usage against erars's own implementation
   (`executor.rs`, `parser.rs`, `parser/expr.rs`).
3. Ranked findings by real corpus impact, not by how easy the defect was to
   find.

This surfaced a ranked list topped by silent-wrong-answer defects
(`TIMES`/`SQRT` `f32` truncation, bulk array-literal assignment truncated to
a single scalar, unchecked shifts/div/mod/`LIMIT`/bit-helper panics,
`FINDELEMENT` exact-match regex, and `~` aliased onto logical `!`) — all
found by asking "does this real, already-shipping line of script behave the
way the spec says it should", not "what enum variant is missing".

Three items were assigned out of that list for this arc: the `#DIM`/`#DIMS`
string-initializer twin defect, `~` bitwise-NOT, and real-corpus
verification. A fourth defect (plain `=` string-literal comma truncation)
surfaced as a byproduct of implementing bulk array assignment and was fixed
alongside it.

## 2. Each fix: corpus usage and evidence

### 2.1 Bulk array-literal assignment (`A = 1, 2, 3` / `STR '= "a", "b"`)

Inventory-reported real usage: eramegaten 169 files / 887 uses (int form),
65 files / 184 uses (string form); eraTHYMKR 0 files / 0 uses for both,
independently verified in the inventory's own §2 by a comment-stripping,
paren/bracket/brace/quote-depth-aware state machine over all 873 files.

Fix: `Stmt::ArrayAssign` + `StoreVarSeq` instruction
(`crates/erars-ast/src/ast.rs`, `crates/erars-compiler/src/instruction.rs`,
`compiler.rs`, `parser.rs`'s new `assign_stmt_from_list`,
`crates/erars-vm/src/context.rs`'s `set_var_ref_seq`). Values fill
consecutive elements starting at the assignment target's own index, bounded
by the *last* declared dimension's size per `exetc.md`'s "Batch Assignment
to Array Variables" (spilling into the next outer index is a compile/runtime
error, never silent wraparound). An interior blank element (`A = 1, , 3`)
is rejected at parse time — no documented fallback value exists for it, so
guessing one (0? unchanged? empty?) is worse than a clear compile error.
Fixtures: `tests/run_tests/assign/bulk_array_assign.{erb,out}`,
`bulk_array_assign_oob.{erb,out}` (out-of-range case).

### 2.2 Plain `=` string FORM-literal comma truncation (byproduct discovery)

The inventory's own §2 investigation, in the course of proving eraTHYMKR's
bulk-array-assign count was genuinely 0, found **87 real lines** across
eraTHYMKR with a top-level comma on the RHS of a plain `=` string
assignment (`RESULTS` 66, `LOCALS` 11, `CSTR` 4, `TSTR` 3, `STR` 2,
`COM_NAME` 1) and correctly concluded these are FORM string literals under
Emuera semantics — a comma there is a literal character, not a bulk-assign
delimiter, so they are *not* bulk-array-assign sites.

That conclusion was correct but stopped one layer short of checking erars's
own handling of that FORM syntax: the parser's plain-`=`-on-string-variable
branch called `form_arg_expr` (`FormStrType::Arg`), the same
comma-*stopping* parser used for call-argument lists, where a comma really
is meant to end one field. On a bare assignment RHS there is no argument
list to delimit, so this silently truncated **every one of those 87 real
lines** at the first unescaped comma — a plain-`=` `TSTR:99 = 체력,기력`
(from eraTHYMKR's `SHOP.ERB:1326`) stored only `체력`, silently dropping
`,기력`.

Fix: `crates/erars-compiler/src/parser/expr.rs`'s new `form_assign_expr`
(`FormStrType::Normal`, no comma stop), used for the plain-`=`-on-string
branch in `parser.rs`. Fixture:
`tests/run_tests/assign/string_comma_literal.{erb,out}`.

**In-situ real-corpus proof** (§3.2): eraTHYMKR's character-search menu
(`SHOP.ERB` `@SEARCH_CHARADATA_MENU`, reachable from the main hub via
`[400]` → `[7]`) sets `TSTR:99 = 체력,기력` then prints it via
`PRINTFORML 검색할 %TSTR:99%...`. Post-fix, driving a freshly-compiled
merged-`master` binary through this exact menu path prints
`검색할 체력,기력(을)를 선택해 주세요.` — the full string, comma intact.

### 2.3 `~` bitwise-NOT aliased onto logical `!`

**Inventory's original estimate** (ranked table, item 9): eraTHYMKR 19
uses / 5 files, eramegaten 6 uses / 3 files. This came from an initial,
less-filtered `~` grep.

**Corrected count, verified this session**: eraTHYMKR **13 real sites / 4
files** (`ERB/SYS/PANCTION_YM.ERB`, `ERB/SYS/CONFIG/CONFIG0.ERB`,
`CONFIG1.ERB`, `CONFIG2.ERB`), eramegaten **0 real sites**. Methodology:
naive `~` search is swamped by two false-positive shapes specific to these
corpora — dialogue-elongation tildes (`「そうだね〜」`-style, using the
half-width `~`) and `N~M` CSV/comment range notation (e.g. documentation
lines listing valid value ranges). The corrected pattern,
`[=&|(,]\s*~[A-Z0-9]`, requires `~` to immediately follow an operator or
open-paren context, cross-validated against two independently-derived
regex formulations; all three converge on 13/4 and 0/0. Every one of the
13 real eraTHYMKR sites is the same `FLAG:N &= ~<int-or-1pN>` bitmask-clear
idiom, confirmed by checking there are zero non-`&=` uses.

Fix: `UnaryOperator::BitNot` added to
`crates/erars-ast/src/operator.rs`; parser (`parser/expr.rs`) maps `~` to
`BitNot`, leaving `!` on `Not`; executor
(`crates/erars-vm/src/terminal_vm/executor.rs`) implements `BitNot` as
`ctx.push(!operand)` on a popped int (erroring on a string operand, same as
`Minus`). New enum variant and instruction discriminant only — no
`VERSION_MAGIC` bump needed, matching the existing precedent for appending
plain `@u32`-payload variants.

A **second, previously-masked bug** surfaced while fixing this:
`crates/erars-compiler/src/parser.rs`'s `const_eval`, in the days when `~`
and `!` shared one `UnaryOperator::Not` variant, had its `Not` arm compute
the *bitwise* complement (`!i`) — because that's what `~`, the operator
that variant used to also represent, needed. This meant any
constant-folded logical `!x` was evaluated as a bitwise complement instead
of `0`/`1`, silently wrong for as long as the sharing lasted, and invisible
because nothing exercised constant-folded `!` against an oracle. Split into
`Not` (now genuinely logical, matching the runtime executor's
`Value::as_bool`-based semantics) and `BitNot` (the bitwise complement).

Fixture `tests/run_tests/assign/bitwise_not.erb` pins both operators in one
file specifically because the failure mode was them sharing a node: it
exercises `A & ~1P2` (bit-clear) and `!0`/`!5` (logical) in the same run, so
a regression that re-merges the two variants breaks the fixture
immediately rather than only showing up in whichever operator someone
happens to test.

**In-situ real-corpus proof** (§3.2): eraTHYMKR's `CONFIGURATION_TRAIN_NONE`
(`CONFIG0.ERB:326`) executes the real line `FLAG:15 &= ~98304`. Reachable
from the main hub via `[777]` (컨피그 설정) → `[3]` (커맨드 설정) → `[98]`
(전부 유효, sets `FLAG:15 = 131071`) → `[99]` (전부 무효, calls
`CONFIGURATION_TRAIN_NONE`). Post-fix, a freshly-compiled merged-`master`
binary run through this exact path yields `131071 & ~98304 = 32767` — the
correct bitwise-clear result. Pre-fix, the logical-NOT bug would have
evaluated `~98304` as `!98304 = 0`, so `FLAG:15 &= 0` would zero the entire
flag word instead of clearing two bits.

### 2.4 `#DIM`/`#DIMS` string-initializer twin defect — no real bug, documented only

Investigated as a suspected second instance of the comma-truncation shape
(§2.2). `dim_line` (the real `#DIM`/`#DIMS` grammar,
`crates/erars-compiler/src/parser/expr.rs`) was never buggy — its own
initializer is parsed as a genuine expression list, not a comma-stopping
one. The actual comma-stopping code lives in `parse_var_decl`
(`crates/erars-compiler/src/parser.rs`), which handles a *different*
statement: `VARI`/`VARS`'s inline mid-function initializer, not `#DIM`'s
directive-level one. Its string-init branch still calls `form_arg_expr`
(comma-stopping) — the same defect shape as §2.2, left unfixed. Two
independent regex passes over both corpora for `VARI`/`VARS <name> =
...,...` found **zero real sites in either corpus**. Documented in place at
the exact code site (`parser.rs`, inside `parse_var_decl`'s `is_str`
branch) with the grep methodology and the zero-count result, so a future
reader doesn't have to re-derive "is this real" from scratch — and so
nobody "fixes it while they're here" on the mistaken belief that leaving a
known, real, unfixed defect undocumented is ever preferable to a
verified-empty one left alone with its reasoning recorded.

## 3. Two methodology results worth keeping

### 3.1 Enum-absence grepping: 6/6 false positives in Appendix A, plus a 7th

The inventory's first pass (superseded by the corpus-driven second pass
recorded here) found several apparent gaps by grepping for identifiers
absent from `BuiltinVariable`'s enum variants. Every single one — 6 for 6 —
turned out to be a false positive once checked against actual execution:
`SAVEDATA_TEXT`, `CDFLAGNAME1`, `CDFLAGNAME2`, `TFLAGNAME` (Appendix A.2,
items 1–2), `ISTIMEOUT`, and `MONEYLABEL` (items 3–4) are all fully
implemented — just not as bare enum variants. They live in
`crates/erars-loader/src/variable.yaml`'s metadata tables, or use `strum`
serialize aliases (as `GAMEBASE_GAMECODE`/`GAMEBASE_CODE` does — the
ranked table's item 13, a 7th instance of the identical false-positive
shape, found and refuted the same way, outside Appendix A because it also
needed a corpus-usage count). `EraConfigKey`-absent config keys are a
related but distinct case: `EraConfig::merge_text`
(`crates/erars-compiler/src/parser.rs:1007`) uses
`if let Ok(key) = key.parse() { ... }`, so an unrecognized key is silently
skipped at load time rather than causing a hard failure — absence from an
enum is not evidence of absence from the running system in either
shape of false positive.

**Lesson**: a construct or identifier not appearing as a named variant in
the specific enum you grepped is not evidence that erars doesn't handle it
— it may be handled one layer over (metadata table, serialize alias,
best-effort `.parse()`). Every absence claim in the corpus-driven inventory
was gated on actually running script against that boundary, not on
grepping variant names.

### 3.2 The two most valuable bugs in this arc were silent wrong answers, not missing features

Both `~`/`!` sharing a node (§2.3) and plain-`=` string comma-truncation
(§2.2) are defects in code that already "worked" in every sense a
compile-and-run smoke test would catch: the program compiled, ran, produced
output, and didn't crash. `~1p4` evaluated to *something* (`0`); `TSTR:99 =
체력,기력` assigned *something* (`체력`). Neither was findable by listing
what's missing from an enum, a match arm, or a builtin dispatch table —
there was no missing arm to find. They were found only by:

1. Comparing observed behavior against the Emuera specification for that
   exact construct (`exop.md`'s `~x = -x - 1` vs. what erars actually
   computed; `exetc.md`'s FORM-literal-with-comma example vs. what erars
   actually stored), and
2. Building a regression oracle — a fixture with a known-correct expected
   output — and confirming the *current* code disagrees with it before
   calling it a bug, and agrees with it after the fix (both fixtures in
   this arc were revert-confirmed: reverting just the fix reproduces the
   documented pre-fix wrong output byte-for-byte).

This generalizes past this arc: an inventory built purely from "what's
absent" will systematically miss the class of bug that matters most in a
reimplementation project — quiet semantic drift in code that looks
complete. Corpus-driven usage counts tell you *where* it's worth building
that oracle; they don't replace building it.

## 4. Merge and verification

`fix/executor-crash-class` (closed workstream: `LIMIT` guard, `ARRAYSORT`
optional `order`, div/mod-by-zero guards, bit-helper index bounds,
`<<`/`>>` shift-count masking, `FINDELEMENT`/`FINDLASTELEMENT`
exact-match + regex-error handling, `TIMES`/`SQRT` integer-operand
widening to `f64`, §5.1) merged into
`master` first with `--no-ff`, cleanly, no conflicts. `feature/bulk-array-assign`
(this arc: §2.1–2.4) merged second with `--no-ff`; both branches touched
`executor.rs` but at disjoint line ranges (unary-operator dispatch vs.
binary-operator/`FINDELEMENT`/`TIMES`/`SQRT` bodies), so `git`'s `ort`
merge strategy auto-merged it with no conflict markers — confirmed correct
by diffing merged `master` against each branch tip individually
(`git diff fix/executor-crash-class..master`,
`git diff feature/bulk-array-assign..master`): each diff shows exactly and
only the *other* branch's content, nothing lost or duplicated. `.gitignore`'s
`/.worktrees` entry was committed directly on `master` afterward, per
standing instruction that local worktree bookkeeping doesn't belong on a
feature branch.

On merged `master`: `cargo test --workspace --features multithread` — 456
passed, 0 failed (matches both branches' independently-reported counts).
Both real-corpus smoke checks were re-run against a binary rebuilt from
merged `master`, with each game's `game.era` bytecode cache deleted and
regenerated fresh (a stale cache from before the merge would silently
validate nothing) — eramegaten reaches the six-class role-selection screen
(데빌서머너/이능자/페르소나구사자/데빌시프터/아웃사이더/식노) with zero
errors or warnings in the run transcript; eraTHYMKR reaches the main hub
and reproduces both §2.2's and §2.3's in-situ proofs exactly as described
above, against the merged binary.

## 5. Corrections to this arc's claims and test coverage (2026-09-06, same day)

A later review found this record and the standalone inventory
(`docs/research/2026-09-06-language-feature-gap-inventory.md`) both
overstated two `fix/executor-crash-class` items. Both are corrected here
against the actual merged-`master` code, located by symbol rather than by
the (already stale) line numbers those two documents cite.

### 5.1 `TIMES` is only half-widened to `f64`; `SQRT` is fully widened

`TIMES`'s multiplication factor is an AST/instruction-payload value, not a
runtime-stack value: `Stmt::Times(Variable, NotNan<f32>)`
(`crates/erars-ast/src/ast.rs`) is compiled straight into
`Instruction::times(ratio)`, whose 4-byte payload is declared
`@NotNan<f32>` in `crates/erars-compiler/src/instruction.rs`'s
`define_instruction!` table. Growing that field to `f64` would widen every
`Instruction` (or require a constant pool for just this one opcode) —
exactly the kind of per-instruction size growth the preceding
`bytecode-opt` arc (`docs/research/2026-09-05-bytecode-dispatch-optimization.md`)
measured and rejected. This arc correctly did not do that.

What the fix actually changed, in `InstructionType::Times`'s executor arm
(`crates/erars-vm/src/terminal_vm/executor.rs`):

```rust
let t = inst.as_times().unwrap();
let arg = ctx.pop_int()?;
let ret = (arg as f64 * t.into_inner() as f64) as i64;
```

`arg` — the `i64` operand popped off the runtime stack — is now genuinely
computed in `f64`, all the way through the multiply. That's the fix that
matters: pre-fix, `arg` was cast straight to `f32` (24-bit mantissa)
*before* multiplying, silently corrupting the low bits of any integer
above 2^24 (16,777,216) — exactly the range EXP, money, and other
large-magnitude script values live in, which is why this ranked #1 by
corpus impact.

`t.into_inner()` is a different story: it is still the `NotNan<f32>` read
straight out of the instruction payload, `as f64`-widened with nothing to
recover — a `f32`'s already-rounded value converts to `f64` exactly, but
it was rounded to `f32` precision (~7 significant decimal digits) back at
parse time and stays that way. So a residual ~1e-7 relative rounding error
versus Emuera's true `double` factor remains, permanently, as a consequence
of the payload layout — not a bug left over from an incomplete fix. This is
harmless in practice: every real `TIMES` factor literal found across both
corpora during the original inventory pass is a simple decimal like `1.5`
or `0.8`, nowhere near needing `f64`'s extra 29 bits of mantissa to render
exactly.

**Correct summary: the integer-operand truncation (the real corruption
risk) is fixed; the factor remains `f32`-precision by construction, and
that residual is not corrected by, and does not need to be corrected by,
this arc.** Prior wording ("`TIMES`/`SQRT` `f32`→`f64`") implied both were
identically and completely widened; that is false for `TIMES`.

`SQRT` has no such payload constraint. `BuiltinMethod::Sqrt`'s argument is
an ordinary runtime value popped with `get_arg!(@i64: args, ctx)` — never
stored in an instruction payload — so `(x as f64).sqrt() as i64` is
complete, unconstrained `f64` end to end. `SQRT`'s widening claim was
accurate; only the sentence lumping it together with `TIMES` was not.

### 5.2 Shift-count masking is defensive hardening, not a confirmed crash

The inventory's ranked table groups `<<`/`>>` shift masking with `LIMIT`,
the bit-index helpers, div/mod-by-zero, and `ARRAYSORT`'s missing default
argument as one undifferentiated "runtime panic" crash class, evidenced
only by corpus *usage counts* of the operators — which count how often
`<<`/`>>` appear in a script, not whether any of those call sites ever
pass an out-of-range shift count. No profile in `Cargo.toml` sets
`overflow-checks` (there is no `[profile.release]` section at all), so
Cargo's default applies: `overflow-checks = false` in `--release`, `true`
in `dev`/`test`. Confirmed empirically (`target/release`, runtime-computed
shift amounts to defeat const-folding): pre-fix's raw `lhs << rhs` /
`lhs >> rhs` panicked only in `dev`/`test` builds; in `--release`, LLVM's
lowering already masked the shift count to `bits - 1` (i.e. `& 0x3F` for
`i64`) before executing the shift — `1i64 << 64 == 1`, `1i64 << -1 ==
i64::MIN`, `1i64 << 65 == 2`, matching C#'s own `& 0x3F` masking exactly.
**The shipped `--release` binary was already computing Emuera-correct
results for out-of-range shift counts before this fix; the fix hardens
`dev`/`test` builds (and removes reliance on an implementation-defined
LLVM lowering) rather than closing a release-crashing gap.** The same
applies to the bit-helpers' pre-fix bodies (`GetBit`'s raw `l >> r`;
`SetBit`/`ClearBit`/`InvertBit`'s raw `1 << idx`) — identical shift
semantics, identical release-mode masking, identical dev/test-only panic
risk. (`SetBit`/`ClearBit`/`InvertBit` additionally already rejected a
negative index pre-fix, as a script error, via `get_arg!(@usize: ...)`'s
failing conversion — only an index `>= 64` was subject to the masking
behavior above.)

Ranking the same five items by what a `--release` binary actually does
pre-fix:

- **Genuinely panics in `--release` (real crash-class fixes)**:
  `BinaryOperator::Div`/`Rem` on `rhs == 0` or `i64::MIN / -1` (integer
  division traps unconditionally in every Rust build profile, confirmed
  empirically — it is not gated by `overflow-checks`); `LIMIT`'s
  `v.clamp(low, high)` when `low > high` (`i64::clamp`'s internal bounds
  check is a real `assert!`, not `debug_assert!`, confirmed empirically to
  abort `--release` too).
- **Debug/test-only panic; `--release` already produced a defensible
  result without the fix (defensive hardening)**: `<<`/`>>` shift-count
  masking; the `GETBIT`/`SETBIT`/`CLEARBIT`/`INVERTBIT` index guards.
- **Never a Rust panic in any profile (not a crash-class item at all)**:
  `ARRAYSORT`'s missing-`order` case is an ordinary `check_arg_count!`
  failure — a `bail!`-raised `anyhow::Error` surfaced as a normal VM
  script error (`매개변수가 부족합니다`), the same path every other
  builtin's argument-count mismatch already used. It was a
  feature-completeness gap (no optional-argument default), never a
  process crash.

All six fixes in `fix/executor-crash-class` were still worth making — the
first two close a real `--release` crash, the guard-based ones make
`dev`/`test` builds match `--release`'s already-correct behavior instead
of panicking, and `ARRAYSORT`'s default argument is a genuine
feature-completeness fix — but "five crashes, all confirmed reachable in
the corpus" overstates both how many of them were process-crashing in
what ships, and what the corpus counts actually established.

## 6. `DELDATA`-on-missing-slot fix, and the `todo.md` §2–§6 residual (2026-09-06, later session)

A later corpus-driving session (`lang-residual`) merged one more real defect into `master`
(`a06607c`, `--no-ff` over `2034f6b`) and then swept `todo.md` §2–§6 for anything still genuinely
missing, since §1 (in-expression functions) was already fully implemented by an earlier arc.

### 6.1 `DELDATA` on a never-written save slot aborted the whole VM

`crates/erars-vm/src/save.rs::delete_save_data` called `std::fs::remove_file(...)?` with a bare
`?`, so deleting a slot nobody had saved to yet raised `std::io::ErrorKind::NotFound`, which
propagated as a fatal VM abort. `excom.md:1128-1131` documents DELDATA as never erroring even when
the target file is absent (matching .NET's `File.Delete`, a silent no-op for a missing path).
Every fresh `eramegaten_p_kr` save hit this: `SHOP.ERB`'s `EVENTSHOP` default branch unconditionally
runs `DELDATA SAVEDATA_NUM_FOR_CONTINUE` on an unwritten slot, so **no fresh playthrough could ever
reach the shop screen** — the VM died with `VM error occurred: No such file or directory (os error
2)` right after the `SET_MASTER` intro narration, zero call-stack frames printed. Fix: swallow only
`NotFound`; any other `remove_file` failure still propagates. `tests/run_tests/basic/save_data.erb`/
`.out` gained a DELDATA-on-missing-slot case and a DELDATA-actually-deletes-a-slot case (via
`CHKDATA` before/after), both proven to fail pre-fix by reverting the source change and reproducing
the identical abort byte-for-byte. `cargo test --workspace --features multithread` on merged
`master`: **456 passed, 0 failed.**

Two further runtime errors past `DELDATA` in the same corpus traced to genuine `SHOP.ERB` authoring
typos, not erars gaps — recorded in `2026-09-03-emuera-command-gap.md`'s corpus-typo catalog rather
than here, since that document is where this project tracks pre-existing corpus defects.

### 6.2 `todo.md` §2–§6 residual: genuinely empty except one already-known naming gap

Every item was checked by grepping the *actual* field/type name for read sites across the whole
workspace (not just its own declaration), the same discipline §3.1 above establishes for enum
variants — a declared, parseable, `GETCONFIG`-queryable field is not evidence of wired behavior,
exactly as an enum variant's absence is not evidence of missing behavior.

- **§2 (26 candidate variables, 6 claimed genuinely missing):** all 6 — `CDFLAGNAME1`,
  `CDFLAGNAME2`, `GAMEBASE_GAMECODE`, `ISTIMEOUT`, `MONEYLABEL`, `TFLAGNAME` — are already
  implemented (`crates/erars-vm/src/variable.rs`'s name-CSV table, `BuiltinVariable::IsTimeout`/
  `MoneyLabel`/`GamebaseCode` in `erars-ast/src/variable.rs` with executor arms). These are the
  same 6 (plus the `GAMEBASE_GAMECODE` alias as a 7th) §3.1 above already documented as false
  positives; nothing new here. **Residual: none.**
- **§3 (`#ONLY` directive):** `SharpCode::ONLY` exists in `crates/erars-lexer/src/sharp.rs:31`.
  **Residual: none.**
- **§4 (config keys):** the 61-heading gap `todo.md` reported no longer applies — every key in its
  §4.1 "behavioural" list (36) and its 4 "extra corpus keys" now has an `EraConfigKey` variant, an
  `EraConfig` struct field, and a `GETCONFIG`/`GETCONFIGS` accessor arm (confirmed by a runtime
  probe: `GETCONFIG("システム関数の上書きを許可する")` → `1`, `GETCONFIG("擬似変数RANDの仕様を
  eramakerに合わせる")` → `0`, matching each key's documented default). **But parsing and exposing
  a config value is not the same claim as `todo.md` §4.1 makes — "changes engine semantics" — and
  a workspace-wide grep for each field's own snake_case name (not just its `EraConfigKey` variant)
  found only 11 of the 39 real fields (36 behavioural headings minus the one, `CompatiDRAWLINE`,
  `todo.md` itself already marks obsolete/superseded and which has no corresponding `EraConfig`
  field, plus the 4 extra corpus keys) actually consulted anywhere outside the config struct's own
  parse/`GETCONFIG` code:** `ignore_case`, `save_nos`, `use_rename_file`, `use_replace_file`,
  `use_save_folder`, `use_debug_command`, `display_warning_level`, `search_subdirectory`,
  `compati_callname`, `compati_call_event`, `use_sp_chara` are genuinely wired into loader/executor
  behavior (each has a real call site cited by field name above). **The remaining 28 field names
  were re-checked individually against `docs/research/emuera-wiki/config.md`'s own wording for
  what each key is supposed to do**, which splits them into four shapes, so a future reader can
  pick keys off this list without re-deriving the analysis:

  - **Host/presentation-only — reclassify out of the residual, same as §4.2's window/mouse/font
    keys (2):** `use_key_macro` (`キーボードマクロを使用する` — F1–F12 keyboard macros bound in the
    GUI; the wiki itself calls out that it doesn't interact with `ONEINPUT`-family input "by
    design", and a headless engine has no F-key concept to bind) and `allow_long_input_by_mouse`
    (`ONEINPUT系命令でマウスによる2文字以上の入力を許可する` — mouse-click multi-character input on
    `ONEINPUT`; not even shown in Emuera's own config UI, mouse-only). Neither has any manifestation
    without a pointing device and a GUI keybinding layer.
  - **Config value is inert either way because the current code hardcodes one fixed behavior,
    unconditionally (2):** `button_wrap` (`ボタンの途中で行を折りかえさない` — whether a selectable
    `[N] - ...` line that would overflow the console width wraps as one unit or splits;
    `erars-renderer/src/layout.rs`'s wrap algorithm is written directly against the default
    `NO`/`false` behavior with no branch for `YES`) and `system_allow_full_space` (`全角スペースを
    ホワイトスペースに含める` — whether U+3000 counts as lexer whitespace; `erars-lexer/src/lib.rs`'s
    `skip_ws` treats it as whitespace unconditionally, matching only the default `true`). Wiring
    either means making an *existing* branch read the config value, not building new logic —
    smaller than the rest of this list.
  - **The underlying feature is entirely absent, not just its toggle (1):** `system_ignore_triple_
    symbol` (`FORM中の三連記号を展開しない` — eramaker's FORM syntax expands a bare `///`/`+++` run
    into literal `NAME:ASSI`/`CALLNAME:ASSI` text; default `NO` means expansion is *on*). A
    workspace-wide search for this expansion (`ASSI` as a FORM-literal target, any `///`/triple-
    symbol handling in `erars-compiler/src/parser/expr.rs` or the lexer) found nothing — erars does
    not implement eramaker's triple-symbol FORM shorthand at all, config aside. Wiring this key
    means implementing that expansion first, then gating it behind the (default-on) switch — a
    small parser feature, not a config plumbing change.
  - **Needs a headless-specific design decision, not just a wire-up, because Emuera's own response
    is a modal dialog (1):** `infinite_loop_alert_time` (`無限ループ警告までのミリ秒数` — if no
    `WAIT`-family command executes for this many milliseconds, show an interactive "this looks like
    an infinite loop, continue?" dialog; `0` disables the feature). The *trigger* — a wall-clock
    watchdog on time-since-last-`WAIT`, confirmed absent from erars: no `infinite_loop`/`watchdog`
    hits anywhere in the VM — is genuine engine timing logic a headless build could implement. The
    *response* is not: there is no user to click "continue" in `erars-stdio`. Wiring this requires
    picking a headless equivalent (log-and-continue? log-and-abort? make it configurable?) before
    there is anything to implement, which is a design call, not a mechanical port.
  - **Real, unimplemented engine semantics with a reasonably well-scoped wiring point (22),
    grouped by the subsystem each would touch:**
    - *Loader/startup behavior*: `auto_save` (`オートセーブを行なう` — autosave on `BEGIN SHOP`,
      overridable from the ERB side; touches wherever `BEGIN SHOP` is dispatched in the executor),
      `display_report` (`ロード時にレポートを表示する` — print a total-lines/functions summary at
      load, else show the `_replace.csv` loading message instead; touches the loader's end-of-load
      path), `sort_with_filename` (`読み込み順をファイル名順にソートする` — sort the CSV/ERB file
      list by name before loading instead of raw directory-enumeration order; touches the file-list
      collection step in `erars-loader/src/lib.rs`, a small, mechanical change).
    - *Load-time argument-diagnostics cluster (the wiki marks three of these "only meaningful when
      `reduce_argument_on_load` is active", so they are one feature, not four)*: `reduce_argument_
      on_load` (`ロード時に引数を解析する` — NO/ONCE/YES: whether call-argument shapes are resolved
      at load time, a load-speed/error-checking tradeoff with no analysis pass in erars today),
      `ignore_uncalled_function` (`呼び出されなかった関数を無視する` — skip that analysis for
      functions never called), `function_not_found_warning`/`function_not_called_warning`
      (`関数が見つからない警告の扱い`/`関数が呼び出されなかった警告の扱い` — IGNORE/LATER/ONCE/
      DISPLAY selectors for, respectively, a `CALL`/`JUMP` target that doesn't exist — explicitly
      *not* `CALLFORM`/`JUMPFORM` — and a defined-but-never-called function). All four require a
      new load-time call-graph/argument-shape analysis pass; none of it exists in `erars-loader` to
      hang a config check on today, so this cluster is the largest single piece of new work on the
      list.
    - *Function-registration/override cluster*: `allow_function_overloading` (`システム関数の上書き
      を許可する`, default YES — whether a user-defined function may override a name from
      `式中で使える関数`/the in-expression-function table; touches wherever `erars-loader` registers
      user functions against `FunctionDic`, which today has no reject-on-collision path at all),
      `warn_function_overloading`/`warn_normal_function_overloading` (warn on such an override /
      warn on a duplicate non-event function of the same name — both are diagnostics riding on the
      same registration path), `warn_back_compatibility` (`eramaker互換性に関する警告を表示する` —
      a general on/off banner for warnings tied to the other `Compati*` quirks below; wiring it
      means gating those warnings' emission on this flag once they exist).
    - *Compat/parsing quirks*: `compati_error_line` (`解釈不能な行があっても実行する` — **note the
      polarity**: real Emuera's *default* is `NO`, meaning it refuses to start at the title screen
      on any unparseable line; erars's loader (`erars-loader/src/lib.rs:590-610`) unconditionally
      reports the error and keeps loading regardless of this flag — i.e. erars is hardcoded to the
      *non-default* `YES` behavior, the opposite direction from the `button_wrap`/`system_allow_
      full_space` cases above. Wiring this means *adding* a startup-abort path, not just reading an
      existing one), `compati_function_no_ignore_case` (`関数・属性については大文字小文字を無視し
      ない` — case-sensitivity for function names/attributes specifically, independent of the
      already-wired blanket `ignore_case`; touches the same identifier-matching path `ignore_case`
      does, but scoped to function/attribute lookups only), `compati_linefeed_as_1739` (`ver1739
      以前の非ボタン折り返しを再現する` — reproduce a pre-1.739 `DRAWLINE`/line-wrap quirk for old
      scripts; not obsolete per the wiki, still a live key, but a legacy-version compatibility
      toggle neither corpus needs — touches `erars-renderer/src/layout.rs`'s wrap algorithm, same
      area as `button_wrap`).
    - *RNG*: `compati_rand` (`擬似変数RANDの仕様をeramakerに合わせる` — eramaker's `RAND` has
      documented quirks erars's current `rng().gen_range(0..max)` doesn't reproduce: accepts
      negative arguments, never returns ≥ 32767, and has a measurable bias once the range exceeds
      1000; wiring this means implementing eramaker's actual generator as a second mode next to the
      one `crates/erars-vm/src/terminal_vm/executor.rs`'s `Rand` arm already uses, not a parameter
      tweak on the existing one).
    - *Save format*: `system_save_in_binary`/`system_save_in_utf8` (binary vs. eramaker-compatible
      text save format, and SJIS vs. UTF-8 text encoding when saving as text — the wiki notes
      binary mode forces UTF-8 regardless of the UTF-8 flag's own setting; touches `erars-vm/src/
      save.rs`'s serialization, the same file §6.1's `DELDATA` fix lives in).
    - *Call-argument semantics*: `compati_func_arg_optional` (`ユーザー関数の全ての引数の省略を許可
      する` — let a call omit non-`ARG`/`ARGS`/private-variable parameters, leaving the callee's
      variable at whatever it held before the call rather than erroring; touches call-argument
      binding in the executor), `compati_func_arg_auto_convert` (`ユーザー関数の引数に自動的に
      TOSTRを補完する` — auto-`TOSTR` an int passed where a function expects a string parameter;
      same call-binding path).
    - *Misc executor semantics*: `times_not_rigorous_calculation` (`TIMES`/decimal math mode — see
      §5.1's `TIMES` `f32`/`f64` discussion for the existing payload-size constraint this would
      interact with), `system_no_target` (`キャラクタ変数の引数を補完しない` — disables `TARGET`
      auto-completion of a bare `chara:var` reference; touches variable-reference resolution in the
      executor), `system_ignore_string_set` (`文字列変数の代入に文字列式を強制する` — restrict
      plain `=` on a string variable to a genuine string expression, presumably rejecting what
      today silently coerces; touches the same plain-`=`-on-string path §2.2 above fixed).

      **Source note (added 2026-09-08, while wiring these four keys):**
      `docs/research/emuera-wiki/config.md` has **no heading at all** for
      `キャラクタ変数の引数を補完しない` (`SystemNoTarget`) or
      `文字列変数の代入に文字列式を強制する` (`SystemIgnoreStringSet`) — the wiki page simply does
      not document them, so there is no wording to look for. The authority for both is real
      Emuera's C# source: defaults in `Config/ConfigData.cs:114` and `:115` (both `false`);
      `SystemNoTarget`'s behaviour in `GameData/Variable/VariableParser.cs:108-137` (with the key
      on, a character variable missing its character index is refused instead of resolving through
      `TARGET`); `SystemIgnoreStringSet`'s in `GameProc/Function/ArgumentBuilder.cs:777-779` (with
      the key on, plain `=` on a string variable is rejected **at parse time** and the script must
      use `'=`). The two `Compati*` call-argument keys above *are* documented
      (`config.md:294-306`), both defaulting to `NO`, with the errors they suppress raised in
      `GameProc/Process.CalledFunction.cs:191-198` and `:199-219`; the implicit `0`/`""` default
      that makes `ARG`/`ARGS`/private parameters omittable regardless comes from
      `GameProc/ErbLoader.cs:578-590` (`canDef`).

      **Source note (added 2026-09-08, wave-1 config session — `sort_with_filename` left
      deliberately unwired):** `erars-loader/src/lib.rs` already sorts both the ERB and ERH file
      lists by lowercased filename unconditionally, before compiling — this is not currently
      gated on the `sort_with_filename` field at all. Reading the surrounding code and history
      showed this sort is load-bearing independent of the config key: without it, feeding an
      unsorted directory-enumeration iterator into rayon's `par_bridge` makes duplicate-function
      "which definition wins" and event-function registration order depend on thread-scheduling
      timing rather than file order — a real nondeterminism bug the sort fixes. `NO` per
      `config.md` (line 213) documents "raw OS enumeration order" (.NET
      `Directory.GetFiles`/`FindNextFile`, filesystem-dependent) as the unwired default; erars's
      actual file-walking goes through the `glob` crate, which — confirmed by reading
      `glob-0.3.1/src/lib.rs:877` (`children.sort_by(|p1, p2| p1.file_name().cmp(&p2.file_name()))`)
      in the vendored cargo registry — already imposes its own case-sensitive alphabetical sort at
      every directory level. So "raw enumeration order" is not reachable through erars's own
      file-listing path regardless of this key; the two ends of the switch collapse to
      case-sensitive-glob-order (`NO`, unreachable as a distinct behavior) vs.
      case-insensitive-sort-order (`YES`, what the code unconditionally does today). Gating the
      existing sort on the field would therefore be a straight regression for `NO` (reintroducing
      the documented `par_bridge` race for the — undocumented, no-`emuera.config` — default case)
      with no reachable behavioral difference to show for `YES`. Left unwired; this is a genuine
      case where the two observable end states are not the two the key's own semantics describe.

  **Net for §4: the real, engine-relevant residual is 26 keys (28 minus the 2 host-only), of which
  2 are a small "read an existing branch" change, 1 needs a small new parser feature before the
  switch means anything, 1 needs a design decision before it's a wiring problem at all, and 22 are
  genuine unimplemented semantics — the largest cluster (load-time argument diagnostics, 4 keys)
  being the single biggest piece of new work.** This is a real, verified residual, but a different
  shape of gap than §1's "absent function"/§2's "absent variable": nothing here raises `Variable X
  is not exists` or `Function X is not exists`, because there is no missing symbol — a script that
  sets `AllowFunctionOverloading:NO` or `CompatiRAND:YES` compiles, runs, and reads back the value
  it set, just with none of the described behavior change. Left unimplemented rather than fixed in
  this pass, and deliberately not started per explicit instruction pending a user decision: wiring
  up to 26 independent semantic switches is new feature work on a different scale than this arc's
  fixes, not a residual gap-sweep item, and none of it was reached by either corpus in this
  session's replay depth (`eramegaten_p_kr` and `eraTHYMKR` both ship `emuera.config` files that
  only ever set keys already in the wired-11 list, per `todo.md §4`'s own `●meg`/`●thy` usage
  columns — no corpus script in this project currently depends on any of the 26).
- **§5 (5 debug console commands):** confirmed still host/UI work, not a VM gap — `erars-stdio` has
  no interactive debug console to attach `@REBOOT`/`@OUTPUT`/`@EXIT`/`@CONFIG`/`@DEBUG` to.
  **Residual: none for the VM; out of scope for a headless engine, unchanged from `todo.md`.**
- **§6 (CSV columns):** §6.1 (`GameBase.csv`) and §6.2 (`Chara*.csv`) were already marked "no column
  gap" by `todo.md` itself. §6.3 (`_replace.csv`, 16 keys) was marked "verify, not confirmed in this
  pass" by `todo.md`; checked here: `crates/erars-compiler/src/parser.rs`'s `ReplaceInfo` struct
  covers all 16 (`money_unit`, `money_position`, `simple_message_at_start`,
  `sales_item_count`/`drawline_str`/`bar_char_1`/`bar_char_2`/`system_menu_0`/`system_menu_1`/
  `com_able_default`/`stain_default`/`time_up_string`/`explv_default`/`palamlv_default`/
  `pband_default`/`relation_default` — every one of the 16 wiki keys has a field). **Residual:
  none.**

**Net: the only pre-existing false-positive already known (§2/§3.1's 7 variables) stays closed, §3
and §6 are genuinely empty, §5 is out of VM scope by design, and §4's real residual is the 28
declared-but-unwired config switches above — not the 61-heading gap `todo.md` originally reported,
which was itself superseded once the enum/struct/GETCONFIG layer was built out.** Neither corpus's
`emuera.config` exercises any of the 28, so this residual was not reachable by the corpus-driving
methodology this arc otherwise prioritizes — it surfaces only from the systematic sweep, which is
why it is recorded here rather than fixed: implementing 28 independent semantic switches is a
separate, larger arc, not a same-session gap-sweep fix.

## 7. Worktree cleanup

All three tip commits (`723567b` fix-executor-crash-class, `5002181` feature-bulk-array-assign,
`2034f6b` fix-deldata-noop) were confirmed ancestors of `master` (`a06607c`) via
`git merge-base --is-ancestor` before any removal. `fix-deldata-noop` removed cleanly.
`fix-executor-crash-class` and `feature-bulk-array-assign` each had leftover working-tree state
blocking a plain `git worktree remove` — in both cases confirmed harmless before forcing: the only
dirty tracked file was the auto-regenerated `docs/research/emuera-wiki/coverage.md` (regenerates on
every test run, per standing project convention never staged/committed), `feature-bulk-array-assign`
additionally had an uncommitted `.gitignore` edit whose only change (`/.worktrees`) already exists
on `master` via the separate direct commit §4 describes, and an untracked draft copy of
`2026-09-06-language-feature-gap-inventory.md` that diffed entirely against the file's current,
corrected `master` content — an earlier, superseded revision (the very "first pass" §3.1 and §6.2
above cite as containing false positives), not unmerged work. `git worktree remove --force` for
both, then `git branch -d` for all three. Roster after cleanup: no worktrees, `master` only.

## 8. Wave-2 corrections: `auto_save` corpus verification and a perf finding it exposed

Two corrections to the wave-1/wave-2 `auto_save` wiring (`crates/erars-vm/src/terminal_vm/executor.rs`,
the `BeginType::Shop` handler), requested after the initial report: real-corpus replay evidence
(not just the new unit test) for the default-behaviour claim, and an explicit code comment
justifying the native-vs-Emuera save-format choice at the autosave call site.

**Default-preservation replay (`eramegaten_p_kr`, `--use-input`/`--exit-when-input-exhausted`,
deepest available RON replay from a concurrent session, 10 game-days).** Built two release
`erars-stdio` binaries — one from `master` before this arc's autosave commits, one after — and ran
the identical replay against a scratch copy of the corpus with fresh `Data/sav/`. Console output
(`sorted diff` to normalize warning-line reordering) was byte-identical between the two binaries
except for warning-count reordering; the tail (post-`[LOOK]` gameplay through the crash both
binaries still hit at the same script line, an unrelated pre-existing issue) matched exactly. The
`after` binary additionally wrote `Data/sav/save99.rsav.gz` (autosave slot) where the `before`
binary wrote nothing there — confirming the feature fires without changing any other observable
behaviour, i.e. the default (`auto_save: true`, matching real Emuera's own default per
`ConfigData.cs:60`) is additive-only against this corpus. Per-run wall time for the autosave write
itself measured at ~750ms extra (alternating-order timing to control for cache effects: `before`
~470ms, `after` ~1220-1280ms, 4 runs each) for this corpus's single `BEGIN SHOP` call.

**`eraTHYMKR` surfaced a real perf issue the eramegaten replay didn't.** `eraTHYMKR` calls
`BEGIN SHOP` from `EVENT_TURNEND.ERB:496` — unconditionally, every game turn, not from a
player-driven shop menu action like eramegaten's script does. Replaying
`bench-inputs/eraTHYMKR_ordinary_play.ron` (previously ~130ms) against the `after` binary took
**92.6 seconds** — because the corpus's own `@SYSTEM_AUTOSAVE` override
(`ERB/SYS/SAVELOAD.erb:216-233`) calls real `SAVEDATA` once per turn (rotating across 10 slots via
`GLOBAL:0 % 10`), and each `SAVEDATA` call in erars costs roughly **900ms** despite the resulting
`.rsav.gz` files being tiny (~42KB, confirmed via `ls -la Data/sav/`). The `@SYSTEM_AUTOSAVE`
function body itself (`SAVEINFO_EX`, a few string concatenations and one `GETTIME` call — no loops,
no array scans) rules out ERB-side cost; the ~900ms is native, inside
`VariableStorage::get_serializable` and/or `save::write_save_data`
(`crates/erars-vm/src/save.rs`), i.e. **pre-existing, orthogonal to this arc's wiring, and merely
newly *reachable at high frequency*** because nothing in either corpus previously called `SAVEDATA`
100+ times in a single session. Output correctness is unaffected — the replay still completes and
the tail is unchanged — so this is a latent performance defect, not a behaviour bug, and is
**out of scope for this config-wiring session** to fix (`erars-vm`'s save path is a live area:
another wave-2 session owns `erars-vm/src/save/emuera`). Flagging it here since `auto_save` is the
first config key whose *correct* wiring makes real per-turn `SAVEDATA` calls a realistic corpus
pattern rather than a rare menu action, and a future session should profile
`get_serializable`/`write_save_data` before shipping any other feature that increases `SAVEDATA`
call frequency.
