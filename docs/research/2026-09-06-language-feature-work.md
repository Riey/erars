# Language Feature Gap Inventory → Fixes → Merge — 2026-09-06

Final `master`: `281eadba7cc21ec1afc3fb110c4b9de2993022ce` (local only, not pushed).
Merged: `fix/executor-crash-class` (tip `723567b`) then `feature/bulk-array-assign`
(tip `5002181`), then a direct `master` commit for `.gitignore`'s `/.worktrees`
entry.

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
exact-match + regex-error handling, `TIMES`/`SQRT` `f32`→`f64`) merged into
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
