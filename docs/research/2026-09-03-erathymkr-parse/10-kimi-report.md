# erars × eraTHYMKR — parse/compile speed & accuracy research (researcher: kimi)

Scope recap: working repo `/home/riey/repos/erars` (branch new-renderer), target `/home/riey/repos/eraTHYMKR` (Emuera kr3 game). All measurements mine; code citations against branch a8190fd.

Note on file counts: the tree holds 873 ERB-ish files, but erars only globs `<game>/ERB/**/*.ERB` → **857** loaded (the other 16 are dev templates under `etc/`, plus 7 ERH). 1,445,252 lines, 58.98 MiB, UTF-8 w/ BOM; 251 CSV.

## 1. Baseline numbers

Machine: 32 threads. Warm page cache. hyperfine warmup 3, 10 runs for the --save pair; warmup 2, 5 runs otherwise.

| Command | Mean | σ | Notes |
|---|---:|---:|---|
| `erars-stdio <game> --save --log-level info < /dev/null` | **237.4 ms** | 6.0 | full compile + bytecode save |
| `... --save --quite < /dev/null` | 276.1 | 29.6 | no file logging |
| `... --measure-memory --quite < /dev/null` | 176.9 | 2.7 | compile, no save, exits |
| `... --load --measure-memory --quite < /dev/null` | 91.4 | 9.3 | bytecode path |
| `... --version` | 0.3 | 0.1 | process floor |

Cold-cache first run was 442.6 ms (--save --log-level info, 5 runs ±14); the numbers above are post-warm.

Phase timings (stdout `[phase]: Nms`, 7 sampled --save runs; run_script phases from `erars-loader/src/lib.rs`):

| Phase | ms (typical) | What it covers |
|---|---:|---|
| Initialize | 7–8 | logger/venv |
| Load CSV | 4–14 | glob + parallel `read_file` of 251 CSV (par_bridge over csvs, `lib.rs:174-215`) |
| Merge CSV | 0–1 | sequential `HeaderInfo` merge |
| Merge chara CSV | 0–1 | CHARA*.CSV merge |
| Merge ERH | 1–6 | sequential `merge_header` (NOT par_bridge'd) |
| Parse/Compile ERB | 176–285 | ERB par_bridge parse+compile **plus serial insert_compiled_func** (check_time fires after the loop, `lib.rs:365-373`) |
| Check codes | 1–9 | erars-lint W1001 (par_iter) |
| Report errors | 0 | diagnostics (none in this game) |
| Save (save_script) | ≈ 25–140 | `write_to` + rmp header (measured: wall − measure-memory ≈ 60) |

Multithread feature: **enabled** — `crates/erars-stdio/Cargo.toml:12` turns on `erars-loader/multithread` (rayon par_bridge for CSV load and ERB parse; also erars-vm/erars-lint multithread).

Single-thread vs multithread (measured via an isolated example harness — equivalent pipeline — built with `--features multithread`):

| Sub-phase | Serial ms | Parallel ms (32t) | Speedup |
|---|---:|---:|---:|
| read_file (read all 857 ERB) | 19.0 | 26.5 (noise; ~loss) | ~0.7× |
| preprocess+lex (drive `Preprocessor::next_line` only) | 128.0 | 13.4 | 9.6× |
| parse+compile (fused `parse_and_compile`) | 1675–1747 | 200 (warm 120–203) | ~8.3× |
| parse to AST (separate `parse()` pass) | 1888–1999 | — | fused is ≥ 8% faster than two-phase |
| insert_compiled_func (serial by design) | 21.6 | — | — |
| write_to → Vec (48.39 MB bytecode) | 36.9 | — | — |

End-to-end: ST ≈ (1675 + 22 + 37 + ~30 header) ≈ **1.76 s**; MT wall **237 ms** → **≈ 7.4×** on 32 threads (rayon scales: 4t ≈ 355 ms, 8t ≈ 225, 16t ≈ 148, 32t ≈ 126 for the parse+compile pass).

Functions compiled: **16,859**; interner 334,738 strings; bytecode 48.39 MB; per-file floor = slowest file 106 ms (EVENT_K42.erb, 1.89 MB).

## 2. Accuracy findings

Headline: **zero parse errors** — erars fully compiles eraTHYMKR. E1000 (ERH) count: 0, E2000 (ERB): 0, W1001 (lint): 0. All findings below are warnings/latent risks, not hard errors — except the determinism item, which is a real semantic hazard.

| Category | Count | Example (file:line) | erars code path | Valid-Emuera or game-bug |
|---|---:|---|---|---|
| Stray `[SKIPEND]` warnings (`TODO: [SKIPEND]`) | 21 | `ERB/CHARA/078 누에/EVENT_K78_RR.ERB:2622` | `crates/erars-lexer/src/lib.rs:264` warn-and-drop | Game side: stray SKIPEND tokens verified by an exact simulation of the square-bracket regexes (`crates/erars-compiler/build.rs:14-62`) → Emuera also skips; benign |
| `[IF symbol]...[ENDIF]` always skipped | 20 blocks (all `ENABLE_KOJO_EQUIP_MESSAGE`/`ENABLE_DESCRIPTION_TRANS_13`) | `ERB/COM/COMF5's/COMF521.ERB:424` | `crates/erars-lexer/src/lib.rs:230-247` (`// TODO: check item is defined`) | Both symbols are commented out in `ERB/FEATURES/*.ERH` → skipping **matches** Emuera for this game; latent parity bug if a game #DEFINEs the symbol |
| Unknown CSV ignored | 2 (`JUEL.CSV`, `NOWEX.CSV`) | `CSV/Juel.CSV`, `CSV/Nowex.CSV` | `crates/erars-loader/src/lib.rs:292-294` (match fallthrough warn) | Mitigated: `JUEL/NOWEX/GOTJUEL` aliases → PALAM/EX in `crates/erars-ast/src/lib.rs:111-117`; divergence only if game needed JUEL's own name table — here it doesn't break |
| Event registration order nondeterministic | 392 event fns (EVENTTRAIN×191 files, EVENTEND×164, EVENTCOM×10, EVENTSHOP×5, EVENTCOMEND×8, EVENTTURNEND×11) | e.g. `ERB/COM/COMF*` EVENTCOMs | `crates/erars-loader/src/lib.rs:333-356` par_bridge collect → `crates/erars-vm/src/function.rs:238-262` | erars-introduced: rayon `par_bridge().collect::<Vec>()` order is unspecified; Emuera config sorts load by filename. Also None-flag events reversed via `insert(0)` (`function.rs:258`) |
| Duplicate normal functions — random winner | 25 duplicate names (e.g. `DEFINE_JINKAKU_REVERSE`, `TROPHY_FEAT_GET_` ×18) | — | `crates/erars-vm/src/function.rs:234-235` `normal.insert` overwrite | Both: game dupes exist and Emuera warns; erars silently picks one nondeterministically |
| game.era nondeterministic bytes | every --save | — | hashbrown RandomState iter (`erars-bytecode/src/lib.rs:165`) + ThreadedRodeo numbering under rayon | verified: two identical identical runs produce different MD5 (3e32bb… vs f92973…) |
| diagnostics miscount message | — | — | `crates/erars-loader/src/lib.rs:387-401` shows `.take(40)` but computes `checked_sub(20)` → wrong "And N more" value | erars bug (cosmetic; inert here since 0 errors) |
| Unimplemented instruction → THROW | 0 in this game | — | `crates/erars-compiler/src/parser.rs:1756-1765` catch-all | verified: of 99 catch-all enum codes, none actually used by this game (`ENDDATA` hits were inside PRINTDATA contexts) |

## 3. Speed findings (hot spots with evidence)

1. **Serial cost lives in the nom expression parser, not the lexer.** Serial `preprocess+lex` alone = 128 ms; full parse+compile = 1747 ms → parsing/compiling beyond raw lineization = **~92 % of serial time** (≈1.15 µs/line vs lex ≈ 88 ns/line). `crates/erars-compiler/src/parser/expr.rs` builds exprs with nom combinators + `VerboseError`, allocates a fresh `String` per form-string chunk (`parse_form_normal_str`), `Cow` uppercase per identifier (`ident_no_case`), and Boxes per Binop/Unary/Cond node (`erars-ast/src/ast.rs:120-121`). The FUSED parse_and_compile beats separate AST-then-compile (1747 vs 1999 serial) — keep fusion.
2. **Parallel scaling = 8.3× → flattens after 8 threads** (4t 355, 8t 225, 16t 148, 32t 126 ms on the warm pass). Suspects: `lasso::ThreadedRodeo` atomic interner (`crates/erars-ast/src/lib.rs:18`), allocator traffic (glibc in harness; stdio binary uses mimalloc `erars-stdio/src/main.rs:8-9`), and tail imbalance — slowest single file 106 ms vs 126 ms phase floor achieved at 32t, so we're near the file-granularity floor.
3. **Stage-by-stage (serial reference):** read 19 ms → lex 128 → parse+compile 1747 → insert 22 → write 37 (+ rmp header ≈ 20–60). The header phases (~30 ms) and `read_file`'s BOM/UTF-8 unchecked-copy (`crates/erars-reader/src/lib.rs:18-20`) are negligible.
4. **Preprocessor regexes** are dense DFA over `regex-automata` (`crates/erars-lexer/src/lib.rs:33-66`), invoked per `[SKIPSTART` (5,739) and `[IF` (20) occurrence with find_earliest over remaining text — fine at 128 ms total, but a parse-rewrite should also drop regex dependency.
5. **Bytecode save** (`erars-bytecode/src/lib.rs:150-182`): per-save it rebuilds a `BTreeMap` of the whole 334k-string interner (sort on the write path), then memcpy-writes arrays. 37 ms for the 48 MB Vec-write; rmp-serde of header+local_infos adds the rest. `--load` = mmap + `read_from` + rmp decode = 91 ms for 50 MB. Format is raw `Instruction`(5 B) arrays; ~33 B of bytecode per source line.
6. **Deterministic-order gap (perf-relevant):** a per-file incremental cache can't be built until output is deterministic; today numbering/geo order is randomized per run (§2).
7. Bench target `benches/parse.rs` measures synthetic `PRINTL Hello, world!` lines — useless for real-file evaluation; the real bench files referenced (`../ERB/TITLE.ERB`) are also absent from this tree.

## 4. Recommendations, ranked

| # | Change | Expected gain | Effort | Risk | Files |
|---|---|---|---|---|---|
| 1 | Deterministic parallel collect: gather ERB paths into `Vec<PathBuf>` first (sorted), then `par_iter().flat_map` (indexed), so `insert_compiled_func` gets stable function order; make event/dupe resolution order stable | correctness (item §2.4/2.5) + paves way for #2 | low | low | `crates/erars-loader/src/lib.rs:333-356`, `crates/erars-vm/src/function.rs:234-262` |
| 2 | Per-file incremental cache: hash (content, header-hash, compiler-version) → cached `CompiledFunction`s; unchanged files skip parse+compile entirely | near-zero recompile time on no-op runs (game.era path already gives 91 ms; this benefits the *compile* path dev loop: 180 ms → ~30 ms expected) | medium | medium (invalidation must cover `_RENAME`, `#DEFINE`, CSV name tables, compiler version) | new cache in `erars-loader`, storage akin to `erars-bytecode` |
| 3 | Replace nom with a hand-written recursive-descent expression parser (memchr-driven), zero String per form/ident (borrowed slices or one reusable buffer), Bump-allocate Expr nodes per function | serial parse ~5-10× (1.7 s → 200-400 ms) | high | medium (parity-sensitive) | `crates/erars-compiler/src/parser/expr.rs`, `parser.rs` |
| 4 | Save path: iterate interner in insertion order instead of building a `BTreeMap` (`write_to`), and buffered/parallel-array writes; `--load` avoid double-buffer | −20–40 ms on save, −20-50 ms on load | low | low | `crates/erars-bytecode/src/lib.rs:150-182`, `crates/erars-loader/src/lib.rs:24-32` |
| 5 | Function-granularity rayon (split large files at `@`-boundaries after a cheap header scan) | narrows 106 ms single-file floor; only meaningful with #1 ordering fixes | medium | low | `erars-loader/src/lib.rs` |
| 6 | Fix `[IF]` semantics: evaluate defined-ness vs `#DEFINE` macros (and DEBUG) instead of unconditional skip | parity | low | low | `crates/erars-lexer/src/lib.rs:230-256` |
| 7 | Diagnostics count fix (`take(40)` vs `checked_sub(20)`) | cosmetic correctness | trivial | trivial | `erars-loader/src/lib.rs:387-401` |
| 8 | Deterministic bytecode bytes: stabilize interner numbering (seedless hasher / ordered collect) so --save is reproducible | reproducibility | medium | low | `erars-ast/src/lib.rs`, `erars-bytecode/src/lib.rs` |

## 5. Open questions

- Why does MT scaling flatten post-8t — ThreadedRodeo atomic contention vs allocator (mimalloc only in stdio binary; harness used glibc)? perf/samply unavailable here; resolve before choosing #3 vs allocator work.
- Event-order reversal via `EventFlags::None → insert(0)` (`function.rs:258`): Emuera intent? If reversed-order is wrong, fix ordering along with #1.
- Cache key scope for #2: must CSV/ERH/_RENAME/#DEFINE changes invalidate only name-dependent files, or all? HeaderInfo hashing design needed.
- Does `insert(0)` + sorted-collect equal Emuera's documented "load order = sorted filenames" for event queues? Emuera source check recommended before #1.
- `read_file`'s 19 ms floor: parallel read was *slower* (26.5 ms) in one sample — page-cache interplay; irrelevant overall but don't parallelize reads.
- game.era currently has no compatibility version beyond a fixed magic (`erars-bytecode/src/lib.rs:18`) — version-salt needed once incremental caches exist.

## Appendix: measurement artifacts (in this scratch dir)

- `hf_all.json`, `hf_save_final.json` — hyperfine exports
- `run_save_1.out/.err`, `phases.err` — raw phase/stderr captures
- `target/release/examples/phases` — isolated phase harness (pre-existing example in repo, built into scratch target; used for serial/parallel split & per-file floor)
