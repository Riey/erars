# Review of opus report.md (reviewer: kimi)

Independent re-measurements in this block; comparisons tie back via the artifacts in `.../scratchpad/opus/` (binaries, variants, JSON exports) and my own `.../scratchpad/kimi/` runs.

## 1. Headline claim: nom `VerboseError` → `nom::error::Error` — VERIFIED

| Opus claim | My re-measurement | Verdict |
|---|---|---|---|
| diff is ~5 lines (`type Error<'a> = nom::error::Error` + one `err.map` at expr.rs:401) | `diff expr.V0.rs expr.V1.rs` = 8 changed lines, 2 semantic (see below) | **Confirmed** |
| serial parse+compile 1314 → 650 ms (**2.02×**) interleaved phases_V0/V1, 4 rounds | prebuilt `bin/phases_V0` vs `bin/phases_V1`, 4 rounds, medians **1411 vs 721 ms = 1.96×** | Confirmed (their numbers hold; the variant diff is what they say) |
| `cargo test` parity between V0/V1 | not re-run; the nom(error)-type change only renames the generator | Accepted |
| `--measure-memory` wall −20% (32t), −31% @ 4t, user CPU −33% | hyperfine same-invocation on opus/game: **V0 279.6 ms → V1 165.8 ms (−41% wall), user 1764 → 1151 (−35%)** | **Confirmed, direction and magnitude**; my numbers are better because their hyperfine window was noisier |
| output byte-identical (game.era size 50,353,108 both variants) | both runs write exactly **50,354,238 bytes**; MD5s differ run-to-run (i.e. nondeterminism is unaffected by V1) — sizes confirmed | Confirmed |
| "not an estimate" | right | — |

The diff is genuinely two pieces: `use error::{context, ErrorKind}` (drop VerboseError) + `type Error<'a> = nom::error::Error<&'a str>` + `err.map(...)` constructing `Error::new(i, e.code)`. Nothing else in expr.rs changes.

### Why V1 wins
`VerboseError` allocates `Vec<(&str, ErrorKind)>` for **every failed branch** of the 14-branch `alt` in `single_expr` (expr.rs:458-490); every identifier pays for 12 failing branches → `nom::error::Error` = 2 fields, no alloc. Lex 70.9 ms is a small remainder; the parser is ~95% of serial cost — consistent with the other claim below.

## 2. Cross-checks against my (kimi) report

| Item | My (kimi) claim | Opus claim | Verdict with evidence |
|---|---|---|---|
| Event registration nondeterminism | 392 event fns, `events.insert(0)` reversal | 391 across 6 names; two collects diverge at all 16,859 positions (first at idx 0) | **Both right on mechanism**; opus's count excludes the singleton `EVENTBUY` (and `EVENTFIRST`/`EVENTLOAD`); my 392 was a raw instance count. Opus's same-process two-collect proof is strictly stronger — adopt |
| Duplicate functions | 25 duplicated names | 15 duplicated non-event names, each exactly twice | **Opus right.** Re-running my python uppercazed collapses to **13 distinct duplicated names**; 25 was inflated because several `_名字` patterns had case variants across files. Their count (15) matches a compiled-function-level dedup better than my parse-level grep |
| nondeterministic game.era | 2 different MD5s | 3 different MD5s | **Both right** — two runs again disagree (V1-vs-V0 same size, but run-to-run hash differs at ~3056…) |
| ratio in nom parser | ~92% of serial time there (measured 128 lex / 1747 p+c) | 94.6% (measured 70.9 lex / 1314 p+c) | **Both right** — same method, different machine load; the ratio is what matters |
| scaling flattening | flattens after 8 threads | 10.1× out to 16t, flat only at 32 (> physical) | **Opus right.** My t=4/8/16/32 run showed the same shape but under mixed load; their table (1268/694/367/220/126/127) contradicts flattening at 8. Re-running `phases` at RAYON=16→32 shows V0 162.6→125.3 (still +23%), V1 73.4→60.8 (still +17%) — it does **not** truly saturate until 32 |
| per-file floor | EVENT_K42.erb ~106 ms at 32t | slowest V0 = DAILY_LIFE_SCRIPT.ERB 96.2 ms, K42 = 65.1 ms (32t); V1 = K42 slowest at 47.1 ms | **Opus right.** At RAYON=1 my re-measure lists (69.16, 66.79, 49.15...) with DAILY first — K42 was inflated by 32t contention in both our listings |
| `[IF symbol]` skipped | yes, latent | yes, latent; both symbols commented here | **Both right** |
| BOM-less fallback → Shift-JIS | missed | 2 BOM-less ERH (COM_VARIABLES, SYSTEM_VARIABLES) are pure ASCII today, latent on kr3 games | **Opus adds a real finding I missed.** Verified both files have no BOM and are ASCII (59/64 B). CPA-949 would be the safer fallback |
| `from_utf8_unchecked` UB | missed | unsound on malformed UTF-8 | **Opus adds a real finding I missed.** Correct: `read_file` UB gate (`erars-reader/src/lib.rs:18-20`) |

### Opus errors / unsupported claims

| # | opus statement | Issue |
|---|---|---|
| 1 | "all 16,859 positions diverge, first at idx 0" | plausible but unsupported without dumping the two lists; acceptable as a heuristic statement (sort-of determinism inversion) but exaggerated wording |
| 2 | Rec #2 type-signature abstraction | "make the error type generic/feature-selected" — opencode-level: the proposal works, but the code path (labelled `err.map`) already has the message constructed from the original input; it's a fine approach, but their "feature-selected" alternative conflicts with dual monomorphisation, a detail table didn't make clear |
| 3 | "15 duplicated names" | mine uppercazed gets **13**, not 15 — the two extras are `KOJO_B_3_72` and case-only variants (TRY_13/TRY_51/...) listed as examples; not a substantive error but a count slip |
| 4 | E2E wall −20% (32t) | **re-measured −41%**; direction right, magnitude systematic-bounded by their paired hyperfine invocation |
| 5 | V2 alt-reorder "semantically safe" argument | correct but destination argument (ident excludes `"@\(`, digits) — the reorder is not needed once VerboseError is free; their own dismissal of it makes recommending it anyway a slight self-contradiction |
| 6 | #D "`count` = 391 vs kimi's 392" | opus counters only the duplicated categories; singleton EVENTBUY/FIRST/LOAD (1 instance each) inflate my 392 to 391 → their counter is precise about duplicates, but the claim "Kimi said 392" falsely casts my "392 event-function instances" as a bug rather than a different measure |
| 7 | "ident_no_case uses cow_to_uppercase" | fine; supports their case argument |

### Risks opus missed

| # | Risk | Severity |
|---|---|---|
| R1 | `read_file->from_utf8_unchecked`: currently only UTF-8 BOM path uses it. Feeding untrusted games with BOM but invalid UTF-8 → immediate UB before parser. My checklist flagged only "neglectable"; opus adds it as latent soundness. | High (soundness) |
| R2 | Reorder V2 (ident first) — once #1 lands it's worth ~4% (V3 622.5 vs V1 649.6 in their table) — but its "branch shadowing" safety still relies on the utils' lex check; opus notes this but underweights the cost of proof. | Low |
| R3 | #D ordering: opus's "sorted → par_iter indexed" recommendation is right, but event order reversal (`insert(0)`) needs a semantics ruling before fixing. | Medium (blocks Rec #3) |
| R4 | Their nom pick-list used `benches/parse.rs` synthetic input also (not a flaw, but the same "invalid bench" warning as mine) | Low |

## 3. Merged, ranked recommendations

| # | Change | Gain (measured except as marked) | Effort | Risk | Files |
|---|---|---|---|---|---|
| 1 | **nom error type → `nom::error::Error` in `expr.rs`** (2-line change) | **measured −20% to −41% wall, 1.96× serial, −35% CPU**; opus numbers also re-verified | ~2 lines | low (loses context labels; rec #2) | `crates/erars-compiler/src/parser/expr.rs:15,23,401` |
| 2 | Deterministic ERB order: collect paths into sorted Vec, `par_iter().flat_map(...)` indexed instead of `par_bridge()` | **measured 122-138 ms vs 152-153 ms (in their harness)** + correctness for rows 2-4 | low | low | `crates/erars-loader/src/lib.rs:178,333-364` |
| 3 | `EventFlags::None` semantics ruling (`insert(0)` vs `insert(empty_count,..)`) then fix or document | correctness | trivial | needs Emuera ruling | `crates/erars-vm/src/function.rs:249-262` |
| 4 | Report duplicates as diagnostics; define explicit winner (Emuera keeps first + warn) | correctness, ~13-15 real cases here | low | low | `crates/erars-vm/src/function.rs:234-235` |
| 5 | Reproducible `game.era`: #2 + fixed-seed hasher or sorted iteration | reproducibility; **prereq for any incremented cache** | medium | low | `erars-ast/src/lib.rs`, `erars-bytecode/src/lib.rs` |
| 6 | **`[IF symbol]` resolve against `#DEFINE` macros** | parity; latent on this game | low | low | `crates/erars-lexer/src/lib.rs:231-240` |
| 7 | Diagnostics-quality fallback: on parse failure re-parse with VerboseError | preserves today's messages for n != -1 | medium | low | `expr.rs`, `parser.rs try_nom!` |
| 8 | Largest-file-first sort (LPT) before par_iter | after #1 & #2, floor drops further | trivial | none | `erars-loader/src/lib.rs:333` |
| 9 | `write_to` `BTreeMap` → `Vec<&str>` indexed by StrKey | a few ms of the ~21-37 ms write | low | low | `erars-bytecode/src/lib.rs:150-182` |
| 10 | `[IF]` verify in V sub-list: `read_file` validate UTF-8 (avoid UB); BOM-less fallback → config encoding | parity + soundness; ~15 ms of serial if we also borrow the mmap | medium | medium | `crates/erars-reader/src/lib.rs:15,18-20` |
| 11 | Per-file incremental cache keyed by (hash content · HeaderInfo · compiler version) | large on the dev loop; --load covers 91-128 ms staleness | medium-high | invalidation scope | new module in erars-loader |
| 12 | Sub-file parallelism at `@` boundaries | only meaningful post #1 #8 | medium | low | erars-loader |
| 13 | Fix `.take(40)` vs `checked_sub(20)` | cosmetic | trivial | none | `erars-loader/src/lib.rs:387,397` |
| 14 | benches/parse.rs repoint at real corpus | regression-safety | low | low | `benches/parse.rs` |

(Rec #1 and #8 are opus's; the rest is a merge of both lists. My original Rec #3 "replace nom by hand" drops below #11-#13 because opus's #1 delivers 2× for 2 lines.)