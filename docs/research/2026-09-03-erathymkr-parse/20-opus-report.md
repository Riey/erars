# erars x eraTHYMKR: fastest and most accurate parse+compile (researcher: opus)

Repo `/home/riey/repos/erars` @ `a8190fd` (branch new-renderer). Target `/home/riey/repos/eraTHYMKR`
(copied read-only to `<S>/opus/game`; the copy and the original produce identical results).
857 ERB files under `ERB/**/*.ERB`, 61,843,825 bytes, 1,445,295 lines, 890,752 logical lines after
preprocessing, 7 ERH, 251 CSV. Output: 16,859 compiled functions, 334,738 interned strings,
48,451,801 bytes of bytecode (50,353,108 bytes of game.era including the rmp header).

**Headline.** The single biggest cost in erars' compile path is not the parser's structure, the
lexer, the interner, or rayon granularity. It is nom's `VerboseError`, which heap-allocates a `Vec`
on **every failed alt branch**. Replacing it with `nom::error::Error` in
`crates/erars-compiler/src/parser/expr.rs` (3 lines plus one error-map fix) cuts serial
parse+compile from 1314 ms to 650 ms (2.02x), whole-program compile wall from 197 ms to 158 ms
(-20%) at 32 threads and from 363 ms to 251 ms (-31%) at 4 threads, with byte-identical output.
I measured this end to end; it is not an estimate.

## 0. Measurement conditions (read this before trusting any absolute number)

This box (32 logical CPUs, ~16 physical) ran unrelated GPU/video jobs throughout the session; load
average moved between 14 and 33 and roughly 20 cores were busy at times. Identical commands varied
by up to 40% across the hour. Therefore:

- Every parser ablation number is from a **single interleaved run** (V0, V1, V2, V3 measured in the
  same round, 4 rounds) so all variants saw the same load. Those comparisons are reliable.
- Every V0-vs-V1 end-to-end number comes from **one hyperfine invocation** containing both.
- Absolute wall times are given as ranges across the session. Do not compare them across sections.

Tools: cargo, hyperfine. No perf, no samply, so hot-spot attribution is by ablation and by
isolated-phase timing, not by sampling. The phase harness is the untracked
`crates/erars-loader/examples/phases.rs` (left in place as instructed); ablation variants were
built only in the scratch copy `<S>/opus/work` with `--target-dir <S>/opus/target`.

## 1. Baseline numbers

Pinned binary `target/release/erars-stdio` (enables `erars-loader/multithread`; rayon `par_bridge`
for CSV load and ERB parse). Run as `<bin> <game> ... < /dev/null`, warm page cache.

| Command | Mean | sigma | When |
|---|---:|---:|---|
| `--save --quite` | 203.3 ms | 6.6 | quiet window |
| `--save --quite` | 230.0 ms | 4.5 | later window (paired with V1 below) |
| `--measure-memory --quite` (compile, no save) | 168.4 ms | 5.8 | quiet window |
| `--measure-memory --quite` | 196.8 ms | 7.0 | later window |
| `--load --measure-memory --quite` (bytecode path) | 90.8 ms | 8.1 | quiet window |
| `--load --measure-memory --quite` | 128.3 ms | 9.3 | later window |
| `--version` (process floor) | 0.40 ms | 0.10 | - |

Phase log (`--log-level info`, 5 consecutive runs, quiet window):

| Phase | ms | Covers |
|---|---:|---|
| Initialize | 4 | logger, console |
| Load CSV | 2-4 | glob + parallel read of 251 CSV (`erars-loader/src/lib.rs:175`) |
| Merge CSV | 0-1 | serial `HeaderInfo` merge |
| Merge chara CSV | 0 | CHARA*.CSV |
| Merge ERH | 1 | serial `merge_header`, 7 files |
| Parse/Compile ERB | 139-145 | par_bridge parse+compile **plus** the serial `insert_compiled_func` loop (`lib.rs:333-373`) |
| Check codes | 1-2 | erars-lint (on by default) |
| Report errors | 0 | zero diagnostics |
| Save (implied) | ~33 | `save_script` = wall(save) - wall(compile-nosave) |

Isolated sub-phases (harness, idle-ish window, serial vs 32-thread rayon):

| Sub-phase | Serial | Parallel (32t) | Note |
|---|---:|---:|---|
| read_file, 857 ERB | 14.6-14.8 ms | 10.2-10.6 ms | mmap + BOM strip + `to_string()` copy of 62 MB |
| preprocess+lex only (`Preprocessor::next_line` to exhaustion) | 70.9 ms | 6.8-7.5 ms | 890,752 logical lines, ~80 ns/line |
| parse+compile (fused `parse_and_compile`) | 1313.9 ms | 122-138 ms | 4-round median for serial |
| parse to AST only (`parse()`), warm interner | 1354 ms | - | fusion is ~7% cheaper than AST-then-compile; keep it |
| `insert_compiled_func` (serial by design) | 9.2-10.2 ms | - | - |
| `erars_bytecode::write_to` into a Vec | 20.6-22.5 ms | - | 48.45 MB |

Rayon scaling of the parse+compile pass (warm interner, `RAYON_NUM_THREADS`):

| threads | V0 (current) | V1 (cheap nom error) |
|---:|---:|---:|
| 1 | 1267.9 ms | 615.4 ms |
| 2 | 694.3 | 323.8 |
| 4 | 367.2 | 167.7 |
| 8 | 219.9 | 104.3 |
| 16 | 125.7 | 64.3 |
| 32 | 127.2 | 63.3 |

Scaling is 10.1x from 1 to 16 threads and flat from 16 to 32, i.e. it tracks physical cores and
gains nothing from SMT. It does **not** flatten at 8.

## 2. Accuracy findings

erars compiles eraTHYMKR with **zero** hard diagnostics: 0 x E0000 (CSV), 0 x E1000 (ERH),
0 x E2000 (ERB), 0 x W1001 (lint, which runs by default). The whole diagnostic log for a full
`--save` run is 23 lines. Everything below is a warning or a latent/semantic hazard.

| Category | Count | Example | erars code path | Valid Emuera or game bug |
|---|---:|---|---|---|
| Nondeterministic function registration order | all 16,859 | - | `erars-loader/src/lib.rs:178` `par_bridge()` + `:333` `flat_map(..).collect::<Vec<_>>()`, consumed serially at `:365` | **erars bug.** Proven: two `par_bridge` collects in one process differ at **every one of 16,859 positions**, first divergence at index 0. `glob` itself is sorted (`glob-0.3/src/lib.rs:946` sorts children), so the sortedness Emuera relies on is destroyed by rayon, not by globbing |
| Duplicate normal function - random winner | 15 names, each defined twice | `TRY_13`, `TRY_51`, `TRY_52`, `TRY_59`, `SLAVE_ENDING_K72`, `DICTIONARY_DRUG_43`, `KOJO_B_3_72`, ... | `erars-vm/src/function.rs:235` `self.normal.insert(name, body)` | Both. The game genuinely defines these twice (Emuera warns); erars silently keeps whichever thread finished last, so the surviving body can change run to run |
| Event function order nondeterministic **and** reversed | 391 event-function instances over 6 event names (EVENTTRAIN, EVENTEND, EVENTCOM, EVENTCOMEND, EVENTSHOP, EVENTTURNEND) | - | `erars-vm/src/function.rs:258` `events.insert(0, body)` for `EventFlags::None` | **erars bug.** `insert(0)` reverses registration order for unflagged events, and the incoming order is already random per the row above. `EventFlags::Pre` uses `insert(empty_count, ..)` (`:253`), which reads like the intent for `None` too |
| `game.era` bytes differ every run | every `--save` | 3 runs -> 3 distinct MD5s (`c32888cb`, `7b2cfb90`, `d5f6d9dc`) | interner numbering under rayon + `dic.normal.iter()` over a `RandomState` HashMap (`erars-bytecode/src/lib.rs:156-170`) | erars bug. Blocks any content-hash incremental cache and makes bytecode non-reproducible |
| `[IF symbol] ... [ENDIF]` unconditionally skipped | 20 blocks (15 `ENABLE_KOJO_EQUIP_MESSAGE`, 5 `ENABLE_DESCRIPTION_TRANS_13`) | `ERB/COM/COMF5's/COMF521.ERB` | `erars-lexer/src/lib.rs:231-240`, comment `// TODO: check item is defined` at `:236` | Latent. Both symbols are **commented out** (`;#DEFINE`) in `ERB/FEATURES/MESSAGE_ENABLE.ERH:3` and `ERB/FEATURES/DESCRIPTION_ENABLE.ERH:3`, so skipping happens to match Emuera here. Any game that actually defines the symbol would silently lose the block |
| Stray `[SKIPEND]` warn-and-drop | 21 | - | `erars-lexer/src/lib.rs:264` `log::warn!("TODO: {line}")` fallthrough | Game side; Emuera also ignores an unmatched SKIPEND. Benign, but the warning text is uninformative |
| Unknown CSV ignored | 2 (`JUEL.CSV`, `NOWEX.CSV`) | - | `erars-loader/src/lib.rs:293` | Mitigated: `JUEL`/`NOWEX` alias to `PALAM`/`EX` in `erars-ast/src/lib.rs:111-117`, so the name tables are not actually lost for this game |
| BOM-less files decoded as Shift-JIS | 2 ERH | `ERB/COM/COM_VARIABLES.ERH`, `ERB/SYS/FUNCTIONS/SYSTEM_VARIABLES.ERH` | `erars-reader/src/lib.rs:15` `Bom::Null => SHIFT_JIS` | Latent. Both files are pure ASCII (59 and 64 bytes) so no mojibake today. For a kr3 game the BOM-less fallback should be CP949 / the config encoding, not Shift-JIS |
| Wrong "And N more errors" count | - | - | `erars-loader/src/lib.rs:387` prints `.take(40)` but `:397` computes `len().checked_sub(20)` | erars bug, cosmetic, inert here (0 errors) |
| `read_file` uses `from_utf8_unchecked` | - | - | `erars-reader/src/lib.rs:18-20` | Unsound on a malformed UTF-8 ERB: undefined behaviour rather than a diagnostic |

## 3. Speed findings

**3.1 The serial budget.** Serial, warm interner, idle window:

```
read 14.8 ms  ->  lex 70.9 ms  ->  parse+compile 1313.9 ms  ->  insert 9.7 ms  ->  write 21.5 ms
```

Lexing is 5.4% of the parse+compile cost. 94.6% of the serial work is in
`crates/erars-compiler/src/parser/expr.rs` and `parser.rs`. That much agrees with the other
researcher. The new result is **where inside** it goes.

**3.2 Ablation: nom's error type is half of the parser.** Four variants of `expr.rs`, built in the
scratch copy, measured interleaved (serial parse+compile of all 857 files, 4 rounds):

| Variant | Change | r1 | r2 | r3 | r4 | median | vs V0 |
|---|---|---:|---:|---:|---:|---:|---:|
| V0 | current code | 1328.9 | 1312.9 | 1314.9 | 1307.5 | **1313.9 ms** | 1.00x |
| V1 | `VerboseError` -> `nom::error::Error` | 641.0 | 651.7 | 655.9 | 647.5 | **649.6 ms** | **2.02x** |
| V2 | move `ident_or_method_expr` to 3rd in the `single_expr` alt chain | 1098.8 | 1110.3 | 1106.4 | 1110.6 | **1108.4 ms** | 1.19x |
| V3 | both | 631.4 | 624.6 | 617.3 | 620.3 | **622.5 ms** | **2.11x** |

All four variants produce the same 16,859 functions and the same 334,738-string interner, and V0
and V1 produce the same `game.era` size (50,353,108 bytes). `cargo test -p erars-compiler
-p erars-lexer -p erars-ast` passes identically on V0 and V1 (13 tests; the suite is thin, so this
is weak evidence on its own).

Why `VerboseError` costs this much: `VerboseError::from_error_kind` heap-allocates
`vec![(input, kind)]` for **every failed parser**, not every reported error, and `context()`
pushes another entry (reallocating) as the failure propagates. `single_expr`
(`expr.rs:458-490`) is an `alt` of 14 branches and every identifier in the game walks past 12 of
them. `nom::error::Error<&str>` is a two-field struct with no allocation. The allocator traffic,
not the branch count, was the cost: once errors are free (V1), reordering the branches buys only a
further 4% (V3 vs V1), whereas reordering alone against expensive errors buys 19% (V2 vs V0).

The reorder is semantically safe (`erars-lexer/src/utils.rs:25-29`: an ident body excludes `"`,
`@`, `\`, `(`, `[`, and `ident()` rejects a leading digit, so no branch above it can be shadowed,
and `__INT_MAX__`/`__INT_MIN__` stay ahead of it). But it is worth only 4% once V1 lands, so it is
optional.

**3.3 End to end.** Same hyperfine invocation, pinned V0 binary vs a V1 build of `erars-stdio`:

| Command | V0 | V1 | Delta |
|---|---:|---:|---:|
| `--save --quite` (32t) | 230.0 +/- 4.5 ms | 189.8 +/- 5.5 ms | -17% |
| `--measure-memory --quite` (32t) | 196.8 +/- 7.0 ms | 157.5 +/- 7.9 ms | -20% |
| same, `RAYON_NUM_THREADS=4` | 363.1 +/- 10.1 ms | 251.4 +/- 7.2 ms | **-31%** |
| user CPU time (`--save`) | 1772 ms | 1186 ms | **-33%** |
| `[Parse/Compile ERB]` phase | 144 ms | 111 ms | -23% |

Wall gain at 32 threads is capped because the phase is already close to its file-granularity floor
(below). The CPU-time gain is the honest measure of the win, and it is what users on ordinary
machines will feel: at 4 threads the compile is 1.44x faster.

**3.4 What is left after V1.** Per-file parse+compile, measured under 32-way contention so
absolute values are inflated and the ranking is noisy:

| | V0 | V1 |
|---|---|---|
| slowest file | `ERB/SYS/DAILY/DAILY_LIFE_SCRIPT.ERB` 96.2 ms (911 KB) | `ERB/CHARA/042 .../EVENT_K42.erb` 47.1 ms (1.89 MB) |
| 2nd | `EVENT_K42.erb` 65.1 ms | `EVENT_K18.erb` 41.0 ms |
| sum over 857 files | 2812 ms | 1296 ms |
| achieved at 32t | 127 ms | 63 ms |

With V1 the 32-thread phase (63 ms) is only 34% above the single slowest file (47 ms). File-level
parallelism is nearly exhausted; further wall gains need either a faster parser or sub-file
splitting. Note the file sizes: the slowest file is not the largest, so a size-based static
schedule is only a rough proxy.

**3.5 Things that are not the problem.** Measured, not assumed.

- Interner contention: parallel passes 2 and 3 with a fully warm interner (no inserts) are the same
  speed as pass 1 (128 / 133 / 130 ms). `lasso::ThreadedRodeo` is not a bottleneck here.
- File reading: 14.8 ms serial for 62 MB; parallel reading gains ~4 ms and is not worth the churn.
- Preprocessor DFAs (`erars-lexer/src/lib.rs:33-66`): the entire lex pass is 70.9 ms serial.
- `insert_compiled_func` serial tail: 9.7 ms.
- Bytecode write: 21.5 ms for 48 MB, of which some is the pointless
  `BTreeMap<StrKey, &str>` rebuild of all 334,738 strings at `erars-bytecode/src/lib.rs:156`.
- Fusing parse and compile: already done, and worth ~7% over a separate AST pass. Keep it.

## 4. Recommendations, ranked

| # | Change | Expected gain | Effort | Risk | Files |
|---|---|---|---|---|---|
| 1 | `type Error<'a> = nom::error::Error<&'a str>` in place of `VerboseError`; fix the one `err.map(...)` at `expr.rs:401` to `nom::error::Error::new(i, e.code)` | **measured**: serial parse+compile 1314 -> 650 ms; compile wall -20% at 32t, -31% at 4t; CPU -33% | ~5 lines | low, but E2000 messages lose the `context()` labels | `crates/erars-compiler/src/parser/expr.rs:15,23,401` |
| 2 | Keep diagnostic quality: make the error type a generic/feature-selected parameter, and on a parse failure only, re-parse that one file with `VerboseError` to build the message | restores today's message quality at zero steady-state cost (0 errors on this game) | medium (touches every signature in expr.rs, or hide it behind one type alias plus a second monomorphisation) | low | `expr.rs`, `parser.rs` `try_nom!` (`parser.rs:63-88`) |
| 3 | Deterministic function order: collect ERB paths into a sorted `Vec<PathBuf>`, then `par_iter().flat_map(...)` (indexed, order-preserving) instead of `par_bridge()` over the glob iterator | correctness (fixes the top 3 accuracy rows); no measured slowdown - the harness's indexed `par_iter` path ran 122-138 ms against 152-153 ms for the `par_bridge` path | low | low | `erars-loader/src/lib.rs:178,333-364` |
| 4 | Review `EventFlags::None => events.insert(0, body)`; compare against Emuera's documented "unflagged events run in load order" and most likely change it to `insert(empty_count, ..)` like `Pre` | correctness | trivial once decided | needs an Emuera-semantics ruling | `erars-vm/src/function.rs:249-262` |
| 5 | Report duplicate normal function definitions as a diagnostic instead of silently overwriting, and define the winner (Emuera keeps the first and warns) | correctness, 15 real cases in this game | low | low | `erars-vm/src/function.rs:234-235` |
| 6 | `[IF symbol]`: resolve the symbol against `#DEFINE` macros instead of always skipping | parity | low | low | `erars-lexer/src/lib.rs:231-240` |
| 7 | Largest-file-first scheduling: sort the path Vec by file size descending before `par_iter` (LPT). With V1 the phase is 63 ms against a 47 ms slowest file, so this recovers most of the remaining 16 ms | small but free | trivial | none | `erars-loader/src/lib.rs:333` |
| 8 | `write_to`: replace the `BTreeMap<StrKey, &str>` with a `Vec<&str>` indexed by key (`StrKey` is a dense 1..len index) and iterate `normal` in sorted key order | a few ms of the 21.5 ms write, plus it is a prerequisite for #9 | low | low | `erars-bytecode/src/lib.rs:150-182` |
| 9 | Reproducible `game.era`: #3 + #8 + a fixed-seed hasher (or sorted iteration) for `dic.normal` | reproducible builds; **prerequisite for any incremental cache** | medium | low | `erars-ast/src/lib.rs`, `erars-bytecode/src/lib.rs` |
| 10 | Per-file incremental cache keyed by (file content hash, HeaderInfo hash, compiler version) | large on the dev loop; the `--load` path already covers the no-change case at ~91-128 ms | medium-high | invalidation must cover `_RENAME`, `_REPLACE`, `#DEFINE`, CSV name tables | new module in `erars-loader` |
| 11 | Borrow the mmap instead of `to_string()` for UTF-8-BOM files, and validate UTF-8 instead of `from_utf8_unchecked` | ~15 ms serial (~1 ms of wall at 32t) and removes a soundness hole | medium (lifetimes reach into the parser) | medium | `crates/erars-reader/src/lib.rs` |
| 12 | BOM-less fallback: use the config encoding (CP949 for kr games) rather than Shift-JIS | parity for BOM-less games | trivial | low | `crates/erars-reader/src/lib.rs:15` |
| 13 | Sub-file parallelism (split at `@FUNCTION` boundaries after a cheap scan) | only meaningful after #1 and #7; the floor would drop below 47 ms | medium | low, given #3 | `erars-loader/src/lib.rs` |
| 14 | Fix `.take(40)` vs `checked_sub(20)` | cosmetic | trivial | none | `erars-loader/src/lib.rs:387,397` |

**On rewriting the nom parser by hand** (the other researcher's top structural recommendation,
scored "5-10x, high effort"): after item #1, the 32-thread parse+compile phase is 63 ms against a
47 ms single-file floor, so a hand-written parser buys very little wall time on a many-core machine
without #13 as well. It remains attractive for CPU time and for low-core machines. It should not be
attempted before #1, because #1 delivers 2x of it for five lines and would otherwise be
misattributed to the rewrite.

## 5. Verification of the other researcher's claims

| Claim (kimi) | Verdict |
|---|---|
| Event registration order nondeterministic because of `par_bridge` collect; 392 event fns | **Confirmed, and strengthened**: two collects in the same process differ at all 16,859 positions. Count is 391 event-function instances across 6 event names, not 392 |
| Emuera loads in sorted filename order | Consistent: `glob` 0.3 does sort (`lib.rs:946`), so erars starts from sorted order and loses it in `par_bridge`, not in the glob |
| Duplicate normal functions (25) get a random winner | Random winner **confirmed**; count **corrected to 15** duplicated non-event names, each defined exactly twice. Measured from the compiled function list, on uppercased names (`function_line` uses `ident_no_case`, `expr.rs:994`) |
| `game.era` bytes differ between identical runs | **Confirmed**: 3 runs, 3 distinct MD5s |
| ~92% of serial time in the nom expression parser (lex 128 ms vs parse+compile 1.7 s) | **Confirmed** as a ratio: 94.6% (lex 70.9 ms vs parse+compile 1313.9 ms). The absolute figures in that report were inflated by machine load |
| Parallel scaling flattens after 8 threads | **Refuted**: 1268 / 694 / 367 / 220 / 126 / 127 ms at 1/2/4/8/16/32 threads. 10.1x to 16 threads; flat only past the physical core count |
| Per-file floor is `EVENT_K42.erb` at 106 ms | **Partly refuted**: on V0 the slowest file is `DAILY_LIFE_SCRIPT.ERB` at 96 ms, with `EVENT_K42.erb` second at 65 ms; on V1 `EVENT_K42.erb` is slowest at 47 ms. Per-file timings are taken under 32-way contention, so the ranking is noisy |
| `[IF symbol]` blocks unconditionally skipped | **Confirmed** (`erars-lexer/src/lib.rs:236`), and the 20 blocks in this game reference two symbols that are commented out in `ERB/FEATURES/*.ERH`, so the skip coincidentally matches Emuera |

## 6. Open questions

1. Does Emuera run unflagged `#EVENT` functions in load order or reverse load order? This decides
   whether `events.insert(0, body)` (`erars-vm/src/function.rs:258`) is a bug or deliberate. It
   cannot be settled from the erars tree alone.
2. Emuera's rule for a duplicate function definition: warn and keep the first, or keep the last?
   erars needs to pick one deliberately (item #5).
3. How much of the residual 650 ms serial parse cost is `String`/`Cow` allocation in
   `parse_form_normal_str` and `ident_no_case` versus `Box`ed AST nodes? Without a sampling
   profiler I could only ablate the error type; the next ablation would be a Bump-allocated `Expr`.
   That measurement was outside this timebox.
4. Does the V1 error type degrade any real E2000 message enough to matter? This game emits zero
   parse errors, so I could not compare messages on real input. Recommendation #2 removes the
   question entirely.
5. Is the `--load` path's 91-128 ms spread page-cache noise or real variance in the rmp-serde
   header decode? Worth a look before optimising the bytecode format.
6. The bundled `benches/parse.rs` measures synthetic `PRINTL` lines and references ERB files that
   are not in the tree. It cannot catch a regression like the one in item #1 and should be
   repointed at a real corpus.

## 7. Post-review reconciliation

Kimi's review is accurate on the headline and on two of its five challenges. Below is the evidence
for each disputed item, produced after the review by extending the `order` mode of the scratch
harness (`<S>/opus/work/crates/erars-loader/examples/phases.rs`) and by grepping the game tree.
Raw output: `<S>/opus/order2.txt`.

### 7.1 Duplicate normal functions: the number is 15, and the interesting split is 9 / 6

Method: take the compiled function list produced by the real pipeline (so names are already
uppercased by `ident_no_case`, `expr.rs:994`), group by `StrKey`, drop names that parse as an
`EventType`, keep groups of size > 1. Every group has exactly 2 members. Kimi's 13 comes from a
parse-level grep; the compiled-function list is authoritative because it is what
`FunctionDic::normal.insert` actually overwrites. **Final number: 15.**

| # | Name | Definition A | Definition B | Winner is nondeterministic? |
|---:|---|---|---|---|
| 1 | `DICTIONARY_DRUG_43` | `ERB/SYS/INFO/HELP4.ERB:319` | `ERB/SYS/INFO/HELP4.ERB:329` | no (same file) |
| 2 | `DICTIONARY_NETABARE_Q147` | `ERB/SYS/INFO/HELP5.ERB:3017` | `ERB/SYS/INFO/HELP5.ERB:3057` | no |
| 3 | `DICTIONARY_NETABARE_Q148` | `ERB/SYS/INFO/HELP5.ERB:3038` | `ERB/SYS/INFO/HELP5.ERB:3071` | no |
| 4 | `K243_MESSAGE_EJAC_PLAYER_15` | `ERB/CHARA/243 사구메/EVENT_K243.ERB:1236` | `...:1275` | no |
| 5 | `K249_MESSAGE_COM_339` | `ERB/CHARA/249 시온/EVENT_K249.erb:4464` | `...:4476` | no |
| 6 | `K249_MESSAGE_COM_347` | `ERB/CHARA/249 시온/EVENT_K249.erb:4419` | `...:4431` | no |
| 7 | `K51_MESSAGE_EJAC_PLAYER_14` | `ERB/CHARA/051 이쿠/YM/EVENT_K51.ERB:2809` | `...:3797` | no |
| 8 | `KOJO_B_3_72` | `ERB/CHARA/072 코가사/EVENT_K72_B.ERB:1497` | `...:1644` | no |
| 9 | `SLAVE_ENDING_K72` | `ERB/CHARA/072 코가사/EVENT_K72_A.ERB:4891` | `...:4973` | no |
| 10 | `KOJO_243_TRAIN_MESSAGE_S_COM_243` | `ERB/CHARA/243 사구메/EVENT_K243_C0.ERB:2517` | `ERB/CHARA/243 사구메/EVENT_K243_C2.ERB:748` | **yes** |
| 11 | `KOJO_MESSAGE_MARKCNG_19` | `ERB/CHARA/074 운잔/EVENT_K74.erb:2939` | `ERB/CHARA/019 요우무/EVENT_K19.erb:7318` | **yes** |
| 12 | `TRY_13` | `ERB/CHARA/013 첸/RR판/EVENT_K13_RR.ERB:26` | `ERB/CHARA/013 첸/EVENT_K13.ERB:24` | **yes** |
| 13 | `TRY_51` | `ERB/CHARA/051 이쿠/EVENT_K51.erb:8683` | `ERB/CHARA/051 이쿠/YM/EVENT_K51.ERB:16` | **yes** |
| 14 | `TRY_52` | `ERB/CHARA/052 텐시/YM/EVENT_K52.ERB:12183` | `ERB/CHARA/052 텐시/EVENT_K52.erb:23` | **yes** |
| 15 | `TRY_59` | `ERB/CHARA/059 파르시/EVENT_K59.ERB:30` | `ERB/CHARA/059 파르시/EVENT_ASSI_K59.ERB:3436` | **yes** |

The split matters more than the total, and neither report had it: **9 of the 15 are two definitions
inside one file**, so the later definition wins deterministically and Emuera does the same; those
are pure game bugs. Only the **6 cross-file cases (#10-#15)** have a nondeterministic winner under
`par_bridge`. Rec #5 (diagnose duplicates, define the winner) should therefore be scoped to those
6 as the correctness risk, with all 15 reported as warnings.

### 7.2 "All 16,859 positions differ": the claim stands, with the method stated

It was measured, not asserted, but the report did not say how. The harness runs the exact
`run_script` pipeline shape twice **in one process** (so interner numbering is stable between the
two collects), maps each `CompiledFunction` to `(file_path key, name key)`, and compares:

```
order: len a=16859 b=16859
order: identical=false
order: first divergence at Some(0), 16859 of 16859 positions differ
order: same multiset after sort = true
order: distinct (file,name) pairs = 16712
```

`same multiset after sort = true` rules out a harness bug: the second collect contains exactly the
same functions, only in a different sequence. The count is a positionwise
`a.iter().zip(b).filter(|(x, y)| x != y).count()`. It is a single trial, so the honest statement is
"in the trial I ran, no position held the same function in both collects", not "the orders can never
coincide". Wording corrected to that.

### 7.3 Event-function count: 391 and 392 measure different things

My 391 is the number of compiled functions whose **uppercased name parses as an `EventType`**, taken
from the compiled function list. Full census:

| Event | instances |
|---|---:|
| EVENTTRAIN | 191 |
| EVENTEND | 163 |
| EVENTTURNEND | 11 |
| EVENTCOM | 10 |
| EVENTCOMEND | 8 |
| EVENTSHOP | 5 |
| EVENTBUY | 1 |
| EVENTFIRST | 1 |
| EVENTLOAD | 1 |
| **total** | **391** |

Kimi's 392 is a source-level count over the same categories; the singletons `EVENTBUY`,
`EVENTFIRST` and `EVENTLOAD` are present in both, so the residual 1 is a counting difference in the
duplicated categories (my EVENTEND is 163, kimi's 164), not a category the other side missed. Both
numbers describe the same set to within one entry; I did not spend the timebox chasing the last one.
Neither figure changes the finding: 391 or 392 event bodies are registered in a nondeterministic
order and unflagged ones are then reversed by `insert(0)`.

### 7.4 End-to-end delta: -20% (mine) and -41% (kimi) are both real

Kimi re-measured `--measure-memory` at 279.6 -> 165.8 ms (-41% wall, user 1764 -> 1151 ms, -35%).
I measured 196.8 -> 157.5 ms (-20% wall, user 1772 -> 1186 ms, -33%). The **CPU-time figures agree
within 2 points** (-33% vs -35%), which is the load-independent measure and the one to quote. The
wall-time spread is a load artefact: kimi's V0 baseline ran at 279.6 ms in a busier window against
my 196.8 ms, so the same absolute saving (~30-40 ms of critical path plus contention relief) reads
as a larger percentage there. **Quote -33% to -35% CPU, and a wall improvement of -20% to -41%
depending on machine load, converging on -31% at 4 threads where the phase is not floor-bound.**

### 7.5 On the merged ranking (kimi §3)

I accept it as written, with two notes. First, its #7 (re-parse with `VerboseError` on failure)
should move up next to #1: they ship together or #1 silently degrades every E2000 message, and
kimi's own critique of my "feature-selected" phrasing is fair, so state it concretely as a second
monomorphisation of the same generic parser, not a cargo feature. Second, drop the V2 alt-reorder
entirely rather than leaving it implicit; my own data says it is worth 4% after #1, and kimi is right
that recommending it while dismissing it was a self-contradiction. Kimi's R1 (`from_utf8_unchecked`
soundness) deserves to sit higher than #10 for a tool that loads untrusted game trees.

## Appendix: artifacts under `<S>/opus/`

- `expr.V0.rs` / `expr.V1.rs` / `expr.V2.rs` / `expr.V3b.rs` - the four ablation variants
- `bin/phases_V{0,1,2,3b}`, `bin/erars-stdio-V1` - built binaries
- `ab.sh`, `ablate.sh`, `measure2.sh`, `e2e.sh`, `final.sh` - the measurement scripts
- `m2.txt`, `e2e.txt`, `final.txt`, `v0.txt`, `V1.txt`, `V2.txt`, `V3b.txt` - raw output
- `hf_final.json`, `hf_e2e.json` - hyperfine exports
- `diag.log` - the complete 23-line diagnostic log of a full `--save` run
- `work/` - the scratch repo copy; its `expr.rs` is restored to V0 and its
  `examples/phases.rs` carries the extra `order` mode used for the determinism proof
