# Bytecode Interpreter Dispatch Optimization (`bytecode-opt` branch)

Date: 2026-09-05
Scope: `crates/erars-vm/src/terminal_vm.rs`, `crates/erars-vm/src/terminal_vm/executor.rs`, `crates/erars-vm/src/function.rs`, `crates/erars-vm/src/context.rs` (`VariableStorage::index_local_var`/`index_var`), `crates/erars-vm/src/variable.rs`, `crates/erars-vm/src/inst_counter.rs`, `crates/erars-compiler/src/{compiler,instruction,parser}.rs`, `crates/erars-bytecode/src/lib.rs`, `crates/erars-lint/src/lib.rs`, `crates/erars-stdio/src/{main,stdio_frontend}.rs`. Branch `bytecode-opt`, based on `master` at `35435b9`, six commits: `42840c7` (census tool), `b006989` (A/B harness), `e4723cf` (P0: dispatch match), `99c95fb` (P0 follow-up: `index_local_var` fix), `d0f1162` (P1v2: `ReportPosition` elimination), `1d5a8df` (`index_var`'s global-path fix, §9), `88cb4cc` (real-workload replay harness + dynamic instruction histogram, §11).
Method: real corpus census (`jit_census` example) against two production ERB corpora (`eraTHYMKR`, `eramegaten_p_kr/Data`) to find where dispatch cost concentrates; a real-`TerminalVm`-execution A/B microbenchmark (`jit_ceiling_bench` example) to measure whether a candidate change actually helps, with a dynamic-instruction-histogram correctness guard on every change so a speed measurement can never be trusted without first confirming it didn't also change *which* instructions run.

**Caveat that applies to every timing number in this document except the corpus load-time, static-instruction-count, and real-workload sections**: `jit_ceiling_bench`'s interpreter-loop timings come from one synthetic ERB loop (`LOCAL`/`SUM`/`PRINTFORM`, statement mix tuned to approximate the census's corpus-observed unigram proportions — see §1 and §2) executed 3,000,000 times. This approximates, but does not equal, either real corpus's actual instruction mix or control-flow shape. An earlier draft of this document claimed running a real corpus's actual game loop end-to-end "is not automatable here — it is an interactive text game"; that claim was false and unchecked (see §13's methodology note) — `erars-stdio --use-input <ron-file> --exit-when-input-exhausted` drives a real corpus fully headlessly, with no interactive terminal required (`< /dev/null` and all), and §11 does exactly that against eraTHYMKR. §7's corpus numbers are a load-time/size/static-count cross-check; §11 is the actual corpus-driven runtime-execution cross-check this document previously claimed did not exist.

---

## Summary

**What changed and what it costs.** Three stages (P0 dispatch-match, §3; `index_local_var`'s TARGET-read fix, §4/§6; `ReportPosition` elimination, §6; plus `index_var`'s matching global-path fix, §9) together cut the synthetic `jit_ceiling_bench` loop's dispatch time by a measured **-21.25% median** (§8) — but that number has a real, non-free counterpart, and burying it in a validation section (as an earlier draft of this document did) understates it:

- **Load time: +0.92% (eraTHYMKR, weak/noisy signal, 8/10 round sign agreement) to +2.31% (eramegaten, 10/10)** (§7).
- **Net +3 bytes per compiled statement — mitigated, now landed (§12).** The old `ReportPosition` instruction was 5 bytes; its initial replacement was an 8-byte `(u32, u32)` positions-table entry plus a 4-byte per-function length prefix the old format never had, verified against measured `game.era` growth to within the corpus's own ~100-300 byte noise floor (§7: +4.774%/eraTHYMKR, +4.020%/eramegaten). A per-function narrow(`u16`)/wide(`u32`) line encoding — overflow *supported*, not merely detected: a function whose own line numbers exceed 65,535 falls back to the wide encoding for that function alone — has since landed and measurably shrinks that cost: **-2.93% (eraTHYMKR) / -2.02% (eramegaten) of the pre-fix `game.era` size**, bringing the arc's net remaining growth down to **+1.71%/+1.92%** (§12), at the cost of a small, real, one-time serialize-time increase (+0.8 to +1.3ms) and no measurable load-time regression for eraTHYMKR (eramegaten's `--load` path is independently broken by an unrelated, pre-existing bug — §12).
- **The synthetic -21.25% does not directly predict real play.** A real, played eraTHYMKR session (§11) shows a smaller, still-real, still-10/10-sign-agreeing improvement: **-9.45% median in game-loop-only time** (isolating post-load dispatch cost, 7.73-11.83% range) and **-3.15% median in full-session time** (load included). The gap exists because a real session's dynamic instruction mix is broader than the synthetic loop's `LOCAL`/`SUM`-heavy profile, and — the arc's central methodological point — because *static* instruction-occurrence counts (what §1's census measures) and *dynamic* execution-frequency (what actually happens when the code runs) are not the same measurement and do not even agree on which instructions dominate (§11).

**Every timing claim in this document follows §13's measurement protocol** (build variants first, interleave both orders, ≥10 rounds, report per-round deltas and explicit sign agreement, `uptime` before/after). See §13 also for a methodology note on two now-corrected unchecked negative claims this arc made along the way.

## 1. Where dispatch cost concentrates: the unigram/bigram census (`42840c7`)

`crates/erars-loader/examples/jit_census.rs`, adapted from the `llvm-jit` branch's AOT-feasibility spike (`d5c9d68`), loads a real corpus's compiled `FunctionDic` and counts every instruction's static frequency (unigram) and every adjacent same-function-body instruction pair (bigram; pairs never cross a function boundary). Re-run against both corpora for this document (binaries built from `99c95fb`, the commit immediately before `ReportPosition`'s removal, so the census reflects the *before* state the optimization arc targeted):

| Corpus | Total static instructions | `ReportPosition` count | `ReportPosition` share |
|---|---|---|---|
| eraTHYMKR | 5,404,279 | 812,935 | **15.04%** |
| eramegaten_p_kr/Data | 6,249,981 | 896,068 | **14.34%** |

`ReportPosition` was a dedicated instruction, emitted once per compiled statement purely to keep `VmContext`'s `ScriptPosition` current for error messages and the debug console — no data flow, no control flow, nothing but bookkeeping — and it was the single largest or second-largest instruction category in both corpora (behind only `LoadStr`/`LoadInt`, ahead of `LoadVarRef`).

**Bigram/dispatch-order finding.** In both corpora, the pairs `ReportPosition -> LoadStr` and `ReportPosition -> LoadInt` are consistently among the top 5 most frequent bigrams out of 197-232 distinct pairs (eraTHYMKR: 7.76% and 5.69%; eramegaten: 9.18% and 4.61%). Summing every bigram with `ReportPosition` as its *first* element against `ReportPosition`'s own total count accounts for effectively all of it (eraTHYMKR: 812,934 of 812,935). This confirms `ReportPosition` was placed at the *start* of essentially every compiled statement, immediately preceding that statement's first data-load instruction — a producer sitting at a fixed, predictable point relative to statement boundaries, never interleaved mid-expression. That placement is exactly why an *on-demand, consumer-driven* replacement (§6) can recover the same information without a dedicated instruction: a statement boundary's line number is recoverable from the compiled body's own structure without the compiler having to say so explicitly on every step.

The census's own "simulated post-Priority-1" pass (filtering `ReportPosition` out of the bigram stream and joining its former neighbors directly) previewed this: `LoadStr -> LoadVarRef` becomes the single most common pair post-removal in both corpora (eraTHYMKR 14.56%, eramegaten 14.69%), and new direct edges appear where `ReportPosition` used to sit (e.g. `Goto -> LoadInt` at 4.76% in eraTHYMKR, `Call -> LoadStr` at 1.79% in eramegaten) — exactly the shape §6's design produces.

Category totals (eraTHYMKR, from the `99c95fb`-era census; `ReportPosition` counted under `pure` since it has no data/control dependency): `pure` 64.92%, `var_access` 14.94%, `control_local` 11.64%, `host` 7.49%, `control_call` 1.01%.

## 2. The A/B harness (`b006989`)

`jit_ceiling_bench.rs` (an existing file inherited from `llvm-jit`, repurposed here) compiles a fixed ERB loop and calls `TerminalVm::try_call` on it directly, 9 interleaved rounds per process, reporting min/median/max wall time. The loop body:

```
@BENCH
#DIM SUM
ISSKIP 1
LOCAL = 0
SUM = 0
WHILE LOCAL < {iters}
	IF LOCAL < {iters/2}
		SUM = SUM + LOCAL
	ELSE
		PRINTFORM iter {LOCAL}
	ENDIF
	LOCAL = LOCAL + 1
WEND
```

chosen (per §1's census) to mix variable reads/writes, arithmetic, a branch, and a `PRINTFORM` per iteration rather than one instruction type, so no candidate optimization could be mis-ranked by a benchmark that barely exercises the instruction it targets. `ISSKIP 1` keeps `Print`'s dispatch cost (pop, format, flag checks) in the mix without its `VirtualConsole` buffering cost swamping millions of iterations. No `black_box` is needed or used: `TerminalVm::try_call` is a real cross-crate call into a heap-backed interpreter that cannot be proven side-effect-free and elided — unlike the native-Rust comparison loop this file originally contained on `llvm-jit`, which needed `black_box` to avoid LLVM proving it closed-form (a mistake that once produced a bogus ~400,000,000× "speedup" there). A `dynamic_instruction_histogram` function computes exact (not sampled) per-type executed counts analytically, by walking the compiled body's own real `Goto`/`GotoIfNot` targets and multiplying by the known iteration split — this is the correctness guard used before and after every change in this document: if the histogram (minus whatever instruction the change legitimately removes) is not byte-for-byte identical, the change altered *what runs*, not just *how fast*, and any timing number is void until that is understood.

This benchmark's loop body source has not changed since `b006989`; only its diagnostic/histogram-printing internals evolved alongside the production code (Step 2 added a public `Instruction::ty()` discriminant, replacing Debug-string parsing; Step 3 removed `ReportPosition` from the printed breakdown). The workload itself is invariant, which is what makes the cumulative measurement in §8 possible using the *same* script across widely separated commits.

## 3. P0: dispatch chain to `match` (`e4723cf`)

`run_instruction` dispatched on `Instruction`'s payload type via a chain of up to 36 sequential `is_x()`/`as_x()` calls, each re-deriving the discriminant check every earlier call in the chain had already failed — instruction types positioned last in the chain (`LoadDefaultArgument`, `BuiltinCommand`, `BuiltinMethod`) paid the full linear scan on every dispatch. Fixed by making `InstructionType` public and adding `Instruction::ty()` (`#[inline(always)]`, no encoding change, no `VERSION_MAGIC` bump), then converting the chain to `match inst.ty() { ... }` with one arm per variant (40 variants, no wildcard — the compiler now rejects a future instruction type added without a dispatch arm, where the old chain would have silently fallen through to dead-code `unreachable!`).

Correctness guard: histogram byte-identical at iters=10 and iters=3,000,000 (the refactor changes only *how* an instruction is selected, never which one runs). `cargo test --workspace`: 450 passed, 0 failed.

Measurement (5 interleaved pairs, baseline binary preserved before rebuilding "new," alternating within one batch, 3,000,000 iterations): all 5 pairs faster, deltas -253.2/-128.3/-116.6/-77.7/-57.0ms (min basis). Median pair delta **-116.6ms (-6.4%)**, strongest pair delta **-253.2ms (-12.9%)**; every pair agreed in sign, effect well above this box's ~8.7% same-build noise floor.

## 4. `index_local_var`'s two-part fix (`99c95fb`)

Found while investigating dispatch cost further: `VariableStorage::index_local_var`, on the hot `LoadVarRef`/`StoreVar` path for every `#DIM`-declared local access (`LOCAL`, `ARG`, any user `#DIM` — far more common than character-indexed global array access), had two stacked defects:

1. `self.read_int("TARGET", &[])?` resolved the literal `"TARGET"` through `impl StrKeyLike for &str`, calling `interner().get_or_intern(self)` — a full hash, lock-protected shard read, and arena probe — on *every single call*, to re-derive a key `index_var` (two functions up in the same file) already had sitting in a fixed `known_key` lookup table. Fixed by using `self.known_key(KnownVariableNames::Target)`.
2. That read was unconditional, but `target` is only consumed inside the `UniformVariable::Character` arm when `calculate_single_idx` returns `None` (an omitted index on a character-indexed local) — every `Normal`-variable access (the overwhelming majority: plain `#DIM` scratch variables are never character-indexed) paid the read and discarded the result. Fixed by peeking `VariableInfo::is_chara` and the index shape immutably first (new `LocalVarTable::get`, mirroring the existing `get_mut`), calling `read_int` only when a `Character` local's index is actually omitted.

Correctness guard: histogram byte-identical at iters=10 and iters=3,000,000 — this change alters no instruction *selection*, only what a couple of instructions' handlers do internally. `cargo test --workspace`: 450 passed, 0 failed.

Measurement, isolating the two fixes with a paired-interleaving protocol (5 rounds; the benchmark's `LOCAL`/`SUM` are both `Normal`, non-character, so fix 2 alone would already skip the read entirely for this workload — isolating fix 1's effect required a temporary `known_key`-but-still-eager variant, "variant A," measured once for this purpose and discarded, never committed):

| Comparison | Median | Min | Sign agreement |
|---|---|---|---|
| baseline → `known_key` only (variant A) | -231.8ms (-12.64%) | -413.2ms | 5/5 negative |
| `known_key` → +lazy read (both fixes) | -136.2ms (-8.50%) | -186.7ms | 5/5 negative |
| baseline → both fixes combined | -370.7ms (-20.06%) | -511.3ms | 5/5 negative |

Both components are separable and both matter: the interner-cost fix is the larger of the two, but the wasted eager read is a genuine second effect on top of it, not noise.

## 5. Step 3, attempt 1: lockstep per-instruction stamping (discarded, never committed)

The first design for removing `ReportPosition` kept `ctx.update_position` current by stamping a line on *every* instruction step: a `positions: Vec<(u32, u32)>` side table (pc → line, monotonic in pc) was added to `FunctionBody`, and `run_body`'s main loop walked `advance_position`/`seek_position` in lockstep with the instruction cursor, updating `ScriptPosition` unconditionally before every single dispatch.

Measured 5/5 rounds *slower*: **median +8.024ms (+0.64%), min +4.956ms (+0.40%)**, 3,000,000 iterations. Small in relative terms, but unambiguous and consistent in sign — a regression, not noise.

**Root-cause mechanism** (recorded here explicitly so it is not rediscovered expensively): `ReportPosition` cost one dispatch on a *conditional* ~13.5-15% of steps (§1). The lockstep stamp replaced that with a position-table lookup plus an `Option`-taking bookkeeping call on *literally 100%* of dispatches — itself almost as expensive per-call as the instruction it eliminated (same side table, same kind of write into `VmContext`), just paid unconditionally instead of conditionally. Trading a conditional ~13.5% cost for an unconditional 100% cost of comparable per-call weight is a net loss by construction, regardless of how cheap the individual lookup is made — the win from eliminating an instruction from the dispatch stream is only real if its replacement is paid on a **narrower** set of dispatches than the original, never a wider one. This is the trap: "the instruction is gone, so it must be faster" does not follow if what replaced it runs more often than the instruction did.

## 6. Step 3, attempt 2 (v2): on-demand, consumer-driven stamping (committed as `d0f1162`)

v2 instead stamps `ScriptPosition` only at the small set of points where a stale position could actually be *observed* — consumer-driven rather than producer-driven. An exhaustive audit of the codebase found exactly two position *consumers*: `debug_console::show_debug` and `TerminalVm::start`'s error-unwind print loop. Every point that could change what either of those would read is a stamp site; nothing else needs one.

- `erars-bytecode/src/lib.rs`: kept the `positions` side table from attempt 1 (it was never the problem — the *unconditional per-step read* was) as the lookup backing store, serialized via `write_arr!`/`read_arr!` in `FunctionBody`'s own encoding. `VERSION_MAGIC` 11 → 12.
- `erars-vm/src/function.rs`: replaced the lockstep `advance_position`/`seek_position` pair with one stateless accessor, `FunctionBody::line_at(&self, cursor: u32) -> Option<u32>` — binary search (`partition_point`) over the side table, no mutable walk state to keep synchronized with the cursor:

  ```rust
  pub fn line_at(&self, cursor: u32) -> Option<u32> {
      let idx = self.positions.partition_point(|&(pc, _)| pc <= cursor).checked_sub(1)?;
      Some(self.positions[idx].1)
  }
  ```

- `erars-vm/src/terminal_vm/executor.rs`: `run_instruction` and `run_builtin_command` both gained `cursor: u32, body: &FunctionBody` trailing parameters. A `stamp_position!(ctx, body, cursor)` macro (`if let Some(line) = body.line_at(cursor) { ctx.update_position(...) }`) is called at exactly 11 sites found by the consumer audit: before every `ctx.input_redraw` (`Print`+`WAIT`, `Twait`, `Wait`/`WaitAnykey`/`ForceWait`, the `Input` family), before `TryCall`/`TryJump`'s `vm.try_call`, before `Jump`/`Call`'s `vm.call`, before `CallEvent`'s `call_event!`, before `DoTrain`/`CallTrain`'s `run_call_train`, before `LoadData`'s `run_load_data`, before `SaveGame`'s `run_save_game`, before `LoadGame`'s combined load block. `run_save_game`/`run_load_data`/`run_call_train`/`run_train_commands`/`run_load_game` keep unchanged signatures — none of them push their own call-stack frame, so one stamp at their `run_builtin_command` dispatch site covers their entire internal excursion.
- `erars-vm/src/terminal_vm.rs`: `run_body` captures `let my_depth = ctx.call_stack().len();` right after `call_internal` pushes its frame; its `Err(err) => { ... }` arm stamps a line via `body.line_at(cursor as u32)` **only if** `ctx.call_stack().len() == my_depth`, before returning the error. Without this guard, an error thrown at call depth N+2 and propagated back through N+1 and N would get re-stamped with N+1's and then N's own cursor on the way out, silently discarding the innermost frame's true position — a correctness bug that would only manifest on a nested-call error path, easy to miss without deliberately reasoning about re-propagation.
- `erars-lint/src/lib.rs` (`check_variable_exist_inner`): migrated to `func.line_at(0).unwrap_or(1)` / per-instruction `func.line_at(i as u32)` — stateless calls are acceptable here since lint is a cold, one-time pass, not the hot interpreter loop.
- `erars-compiler/src/{compiler,instruction,parser}.rs`: `ReportPosition`/`report_position` removed outright — no instruction encodes a line number into the stream any more; the side table is now the only place line information lives.

**Correctness guard**, three layers: (1) `jit_ceiling_bench`'s histogram byte-identical for every instruction other than `ReportPosition`, at iters=10 and iters=3,000,000; (2) re-confirmed at real-corpus scale via `jit_census` — on both eraTHYMKR and eramegaten, every non-`ReportPosition` row's raw count is identical between `99c95fb` and `d0f1162`, only `ReportPosition`'s row disappears (§7); (3) both oracle fixtures (`tests/run_tests/basic/debug_console_window.out`, `tests/run_tests/control_flow/unbound_ref.out`) pass verbatim. `cargo test --workspace`: 451 passed, 0 failed.

**Measurement**: 10 interleaved rounds (5 old-first + 5 new-first, to rule out ordering bias), 3,000,000 iterations each. Deltas (NEW−OLD, ms): -81.428, -64.515, -22.149, -43.170, -70.395, -52.882, -86.371, -48.257, -82.303, -63.284. **10/10 sign agreement**, NEW always faster. Median -63.9ms (-5.1%), weakest effect -22.1ms (-1.8%), strongest -86.4ms (-6.8%). Load average rose 0.82 → 2.50 during the batch (another user on the box mid-run); noted for transparency — does not change the sign-agreement conclusion, since the effect holds under both interleave orderings and exceeds same-build noise by a wide margin.

## 7. Corpus-level validation (real ERB corpora, `99c95fb` vs `d0f1162`)

Static instruction count (`jit_census`), confirming the removal's size matches its own measured share from §1 exactly (nothing else about instruction selection changed):

| Corpus | Before | After | Drop |
|---|---|---|---|
| eraTHYMKR | 5,404,279 | 4,591,344 | **-15.04%** |
| eramegaten_p_kr/Data | 6,249,981 | 5,353,913 | **-14.34%** |

Every other instruction category's raw count is byte-for-byte identical between the two builds on both corpora — verified by diffing the full census output, not by eyeballing percentages.

`game.era` on-disk size (`erars-stdio --save --quite`, dual-order 10-round protocol, both prebuilt binaries — `game.era` is *not* byte-reproducible run-to-run under multithreaded parsing on either binary, a ~100-300 byte noise floor on 50-85MB files confirmed present on **both** `99c95fb` and `d0f1162`, four orders of magnitude below the effect below, so it does not affect the conclusion). Superseding an earlier draft's 3-run average, which did not meet this document's own measurement bar (§13):

| Corpus | Deltas (After−Before, B), old-first then new-first | Sign agreement | Median | Min | Max |
|---|---|---|---|---|---|
| eraTHYMKR | 2506309, 2506303, 2506243, 2506057, 2506115, 2506565, 2506121, 2506243, 2506245, 2506239 | 10/10 positive | **+2,506,243 B (+4.774%)** | +2,506,057 B | +2,506,565 B |
| eramegaten_p_kr/Data | 3190464, 3190090, 3190082, 3190737, 3190204, 3190528, 3189943, 3190536, 3190360, 3190260 | 10/10 positive | **+3,190,310 B (+4.020%)** | +3,189,943 B | +3,190,737 B |

This is the expected, one-time cost of persisting the new `positions` side table — not a regression to chase down.

Load time (`phases` example, `parse+compile serial` wall-min, dual-order 10-round protocol, each round one 5-round `phases` invocation reporting its own min). Superseding an earlier draft's single-order 5-round data, which did not meet this document's own measurement bar (§13):

| Corpus (function count) | Deltas (After−Before, ms), old-first then new-first | Sign agreement | Median | % of baseline |
|---|---|---|---|---|
| eraTHYMKR (16,859 functions per `phases`; 16,844 per §Summary's `FunctionBody`-level positions-table count — the two tools count slightly different units) | -2.9, +3.3, -1.5, +2.1, +3.2, +3.2, +2.9, +1.0, +1.5, +1.8 | 8/10 positive, 2/10 negative | +1.95ms | **+0.92%** |
| eramegaten_p_kr/Data (125,549 functions) | +14.1, +7.4, +3.8, +8.5, +10.0, +10.8, +9.6, +4.6, +4.3, +4.7 | 10/10 positive | +7.95ms | **+2.31%** |

A small, real, size-proportional compile-time cost from populating the positions table during compilation, scaling with function/statement count as expected — negligible in absolute terms (a few milliseconds against either corpus's ~210-345ms total load time), and paid once at load rather than on every future interpreter step. eraTHYMKR's effect is real but weak and noisy (2 of 10 rounds went the other way, unlike every other paired comparison in this document); eramegaten's is unambiguous. **No load-time regression of consequence.** Both corpora's lint-warning sets are identical in content between the two binaries (order differs run-to-run under the multithreaded lint pass regardless of code, confirmed by sorting and diffing both outputs) — confirming no functional regression alongside the timing check.

## 8. Cumulative effect: one direct `35435b9` → `d0f1162` measurement

P0 (§3), the `index_local_var` fix (§4), and P1v2 (§6) each changed the *base cost* the next stage's percentage-of-dispatches savings applies against — summing their individually-measured medians would double-count or under-count depending on which base each was measured on. Per the branch owner's explicit instruction, the cumulative effect is instead one direct paired measurement from `master`'s tip (`35435b9`, before any of this branch's changes) straight to the final commit (`d0f1162`), using the *same* `jit_ceiling_bench` loop body (§2's script is unchanged since `b006989`; `35435b9`'s worktree does not contain this file at all — the pre-`Instruction::ty()` Step-1 version of the harness was used for the "old" build, since it drives the identical script through identical `TerminalVm::try_call` setup and its histogram/diagnostic internals, which do not exist in `35435b9`'s API, run entirely before the timed region and were not the thing rebuilt or measured).

10 interleaved rounds (5 old-first + 5 new-first), 3,000,000 iterations each, both binaries prebuilt into distinct paths before any timing began:

| Order | Before (`35435b9`) | After (`d0f1162`) | Delta | % |
|---|---|---|---|---|
| old-first | 1479.854ms | 1168.353ms | -311.501ms | -21.05% |
| old-first | 1497.715ms | 1165.595ms | -332.120ms | -22.18% |
| old-first | 1502.690ms | 1158.498ms | -344.192ms | -22.91% |
| old-first | 1490.123ms | 1169.056ms | -321.067ms | -21.55% |
| old-first | 1468.125ms | 1157.293ms | -310.832ms | -21.17% |
| new-first | 1486.804ms | 1165.799ms | -321.005ms | -21.59% |
| new-first | 1472.689ms | 1193.106ms | -279.583ms | -18.98% |
| new-first | 1497.232ms | 1166.863ms | -330.369ms | -22.07% |
| new-first | 1472.022ms | 1170.222ms | -301.800ms | -20.50% |
| new-first | 1509.706ms | 1217.045ms | -292.661ms | -19.39% |

**10/10 sign agreement, NEW always faster.** Median delta **-316.25ms (-21.25%)**, weakest effect -279.6ms (-18.98%), strongest effect -344.2ms (-22.91%). This is substantially larger than any single stage's own median (P0 alone: -6.4%; `index_local_var` alone: -20.06% but measured on P0's already-reduced base; P1v2 alone: -5.1%, also measured on an already-reduced base) — consistent with the interaction the branch owner flagged: each stage's saving is a percentage of a shrinking denominator, so the compounded effect of all three together is close to, but not a naive sum of, the individual percentages.

**Instruction-selection sanity check** across the full `35435b9` → `d0f1162` span (more than one mechanism changed, so this is a sanity check rather than the single-mechanism byte-identity guarantee used in §3/§4/§6 individually): at iters=10, every non-`ReportPosition` instruction-type row's raw count in the dynamic histogram is identical between the two builds (`LoadStr` 73, `LoadVarRef` 63, `BinaryOperator` 36, `LoadInt` 35, `GotoIfNot` 21, `StoreVar` 17, `Goto` 15, `ConcatString` 5, `Print` 5, `BuiltinMethod` 1, `StoreResult` 1); only `ReportPosition` (44 at `35435b9`) disappears. Nothing else about *what* runs drifted across P0's dispatch-match conversion, the `index_local_var` internal fix, or `ReportPosition`'s removal — only how fast and by what mechanism it runs.

## 9. `index_var`'s global-path TARGET fix (`1d5a8df`)

`VariableStorage::index_var` — the *global*-variable counterpart of §4's `index_local_var` (`FLAG`, `CFLAG`, `TALENT`, and every other character/global array, which dominate real game logic far more than `#DIM` locals do) — had the identical defect class as §4's fix 2: it unconditionally read `TARGET` via `self.read_int("TARGET", &[])?` before checking whether the access was even character-indexed, discarding the read on every `Normal`-variable access (the overwhelming majority). Fixed the same way: peek `self.variables.get(&name)` (a plain `HashMap::get`, no new accessor needed) for `is_chara && calculate_single_idx(args).0.is_none()` before conditionally reading `TARGET` via `known_key`. `TARGET`'s own `VariableInfo::is_chara` is `false`, so the recursion guard the original code carried (`name != target_key`) is now provably unreachable and was removed.

**No timing number is claimed for this fix.** `jit_ceiling_bench`'s loop body (§2) only ever touches `LOCAL` and `SUM`, both `#DIM` locals — it never exercises `index_var`'s global path at all, so this fix is completely invisible to every synthetic measurement in this document. The histogram-identity correctness guard (byte-identical at iters=10 and iters=3,000,000) is therefore trivially true here, not meaningful evidence that the fix does anything — it is meaningful only in that it confirms the fix broke nothing the benchmark *does* exercise. This gap is exactly why §11's real-workload replay exists: a real game's actual `FLAG`/`TALENT` access pattern is precisely what this fix targets, and no synthetic loop in this document was ever going to see it. `cargo test --workspace`: 451 passed, 0 failed (unchanged from §6, no test count regression).

## 10. The `get_or_intern` 11.19% cost: resolved by §4, confirmed absent post-fix

An earlier pass at this arc profiled `jit_ceiling_bench` with `pprof` and found `get_or_intern` at 11.19% of samples, traced to a real caller chain: `get_or_intern <- index_local_var <- index_maybe_local_var`, pinned to the `self.read_int("TARGET", &[])?` literal string lookup at what was then `variable.rs:989` (confirmed present at that exact line in the `99c95fb^` pre-fix tree). This is not a stale or unverified claim — it is §4's fix 1, verbatim: replacing that `read_int("TARGET", &[])` with `self.known_key(KnownVariableNames::Target)` is exactly what eliminated this cost, and §4's own isolated measurement (baseline → `known_key` only: median -231.8ms, -12.64%, 5/5 negative) is the direct before/after evidence for it. The two are the same finding, reported in two different sessions of this same document.

This session re-profiled the post-`99c95fb` binary specifically to confirm the fix actually removed the cost rather than merely relocating or hiding it, since that had not been separately checked before:

- Rebuilt `jit_ceiling_bench` with debug info (`CARGO_PROFILE_RELEASE_DEBUG=true`; the root `Cargo.toml` carries no `[profile.release]` block, so release binaries otherwise ship with zero DWARF info and pprof would silently fold any inlined `get_or_intern`/`to_global` frame into its caller).
- Re-profiled at 3,000,000 iterations with proper inline-frame resolution: **no `get_or_intern`/`to_global`/`Interner` frame appears anywhere in the leaf self-time output, at any percentage**, confirming the fix is complete rather than partial. Top entries were `run_instruction` 24.09%, `run_body` 24.01%, `take_value_list` 6.74%, and small `hashbrown`/`foldhash` hash-probe noise (2.44%/1.68%/0.51%) from the `variables`/`local_variables` maps — unrelated to string interning.
- Traced every remaining `get_or_intern`/`get_or_intern_static`/`to_global`/`intern_cached` call site in the current codebase to confirm none of them are a second, undiscovered instance of the same mistake. `Expr::String`/`Expr::FormText` compile to `Instruction::load_str(key.to_global())` **once per literal, at compile time** (`erars-compiler/src/compiler.rs`) — `LoadStr`'s runtime dispatch just pushes the already-global key, no re-interning. The only remaining *runtime* call sites are `pop_strkey` (dynamic call/label targets computed at runtime), `get_arg!(@key ...)` (used only by the `VARSIZE`/`REFBYNAME` builtin family), and `LoadDefaultArgument` — all inherently dynamic (the string isn't known until runtime), not literal-lookup mistakes like the one §4 fixed, and none are exercised by `jit_ceiling_bench`'s loop.
- **Conclusion: zero residual `get_or_intern` cost, of any kind, anywhere in the current codebase's hot paths.** §4 fully resolved the 11.19% finding; this section exists only to record that the resolution was verified, not to relitigate whether the original measurement was real. It was.

## 11. Real-workload replay: dynamic histogram divergence and cumulative real-session measurement

Every measurement above this point compares a synthetic `LOCAL`/`SUM` loop's dispatch cost, or a corpus's *static* instruction occurrence count. Neither is an execution-frequency measurement of a real played session. This section is that measurement.

**The replay script.** `bench-inputs/eraTHYMKR_ordinary_play.ron` (committed alongside the harness that runs it): 8 setup answers through eraTHYMKR's title screen, mode/difficulty selection, and character creation, followed by 100× the `102` ("휴식"/rest) main-menu action — 2 rest actions per in-game day/night cycle, so 50 full in-game days. `102` was chosen over engaging every menu branch (e.g. `101`'s ability view opens a differently-numbered sub-menu) purely for reliability/validation cost — this is a real, played, representative-of-idle-play session, not an exhaustive tour of the game's content, and is reported as such rather than dressed up as more than it is. Validated end-to-end: exit code 0, reaches "50일째 1년 3월 18일 일요일(밤)" exactly as expected.

**eramegaten_p_kr/Data was tried and excluded.** It crashes reproducibly at its own title screen under `erars-stdio`, with or without an input script, interactively or with `< /dev/null`: `VM error occurred: RAND: 인수에 0 이하의 값(0)이 지정됐습니다` (RAND given a non-positive argument) at `PRINT_TITLE.ERB@67`, called from `SYSTEM_TITLE.erb@171` — some `RAND(N)` call resolves `N` to zero or less outside a real terminal (plausibly a `WINDOW_WIDTH`-style value defaulting to 0 headlessly). This is a pre-existing corpus/engine interaction bug unrelated to this arc's dispatch changes, and out of scope to fix here. eraTHYMKR alone is used for this measurement.

**Dynamic (executed) instruction histogram**, captured via the `inst-counter` feature (`cfg(feature = "inst-counter")`, `crates/erars-vm/src/inst_counter.rs`, committed alongside the replay harness) running the replay script above, 2,170,059 total dispatches over the 50-day session:

| Instruction | Share | Instruction | Share |
|---|---|---|---|
| `LoadInt` | 20.76% | `LoadCountVarRef` | 1.78% |
| `LoadStr` | 18.50% | `BuiltinMethod` | 0.74% |
| `LoadVarRef` | 17.28% | `BuiltinCommand` | 0.68% |
| `GotoIfNot` | 13.83% | `Call` | 0.62% |
| `BinaryOperator` | 12.04% | `Print` | 0.19% |
| `Goto` | 5.47% | (10 more, each <0.3%) | |
| `DuplicatePrev` | 2.52% | | |
| `StoreVar` | 2.43% | | |
| `GotoIf` | 2.33% | | |

Compared against §1's static eraTHYMKR census (`LoadStr` ~23%, `LoadInt` ~19%, `ReportPosition` ~15% [since removed], `LoadVarRef` ~12%, `BinaryOperator` ~5%, `GotoIfNot` ~4%, `Print` ~5%), the two measurements do not even agree on rank order: `GotoIfNot` is ~3.4x more prominent at runtime than its static occurrence suggested, `BinaryOperator` ~2.4x, and `LoadVarRef` — the exact instruction both §4's and §9's TARGET-read fixes target — is proportionally *more* common at runtime (17.28%) than in the static count (~12%). `Print`, conversely, is ~26x *less* common at runtime than its static presence implied: a source file can contain many `PRINT`-family statements across menus and events that a single 50-day idle-rest session never reaches, while loop/conditional logic inside the day-processing routines runs on every single action. **Static instruction-occurrence counting and dynamic execution-frequency weighting are not the same measurement, and prioritizing by the former can misrank exactly the kind of fix (§4, §9) that matters most by the latter.**

**Cumulative real-session timing**, `35435b9` (harness-backported: `--exit-when-input-exhausted`/`--bench-timing` added via a `git worktree`-isolated, behavior-inert cherry-pick onto a pristine `35435b9` checkout, so the interpreter under test is unmodified `35435b9` and only the CLI driver gained the capability needed to script and time it — same precedent as §8's Step-1 harness backport) vs. the branch tip, `--load`-ing a pre-saved, version-matched `game.era` so neither side's compile time (already measured separately in §7) contaminates the comparison. `[bench] load complete`/`[bench] session complete` markers (both binaries) isolate game-loop-only time (`session - load`) from the fixed CSV/era-load floor. 10 interleaved rounds (5 old-first + 5 new-first), both binaries prebuilt into distinct paths, `inst-counter` feature off:

| Round | Order | Old game-loop (ms) | New game-loop (ms) | Delta | % |
|---|---|---|---|---|---|
| 1 | old-first | 50.876 | 45.823 | -5.053 | -9.93% |
| 2 | old-first | 50.849 | 45.973 | -4.876 | -9.59% |
| 3 | old-first | 50.646 | 44.656 | -5.990 | -11.83% |
| 4 | old-first | 49.902 | 45.254 | -4.648 | -9.31% |
| 5 | old-first | 50.399 | 45.266 | -5.133 | -10.18% |
| 6 | new-first | 51.021 | 46.322 | -4.699 | -9.21% |
| 7 | new-first | 50.518 | 46.615 | -3.903 | -7.73% |
| 8 | new-first | 50.301 | 46.353 | -3.949 | -7.85% |
| 9 | new-first | 49.760 | 45.238 | -4.522 | -9.09% |
| 10 | new-first | 50.330 | 45.174 | -5.156 | -10.24% |

**10/10 sign agreement, NEW always faster.** Median **-9.45% game-loop-only** (min -7.73%, max -11.83%). The same 10 rounds' full-session time (load included) also agreed 10/10 but far more weakly — median **-3.15%** (range -1.03% to -7.97%) — because the ~80-90ms fixed CSV/era-load floor common to both binaries dilutes a dispatch-only effect once compile/load time is folded back in.

**This does not match the synthetic §8 figure, and the real number is the one that governs.** §8's `35435b9` → `d0f1162` synthetic `jit_ceiling_bench` measurement showed **-21.25% median**; this real, played eraTHYMKR session shows **-9.45% game-loop-only** (or **-3.15%** including load). Both are real, both are 10/10 sign-agreeing, and both point the same direction — but they disagree by more than a factor of 2 on magnitude. The synthetic loop's `LOCAL`/`SUM`-only variable traffic and tight `WHILE`/`IF` shape concentrate exactly the instructions this arc's fixes target far more densely than a real, broad-mix game session does; the dynamic histogram above shows precisely why (`LoadVarRef` is prominent in both, but the real session's `GotoIfNot`/`BinaryOperator`/`LoadStr`/`LoadInt` mix is far more varied than the synthetic loop's). **Anyone deciding whether this arc was "worth it" for a real player should use -9.45%/-3.15%, not -21.25%.**

## 12. The u16 line-encoding mitigation: landed

§7's `game.era` growth prediction (+4.774%/eraTHYMKR, +4.020%/eramegaten) came with a stated mitigation candidate: both corpora's actual maximum line numbers (35,398 and 11,107) fit comfortably inside a `u16`, so a narrower on-disk encoding could shrink each `positions` entry from 8 bytes to 6. That mitigation is now implemented, not merely proposed.

**Design: per-function encoding choice, overflow supported, not merely detected.** A generated or concatenated ERB (macro-expanded, or several files textually joined before compilation) could plausibly exceed 65,535 lines even though neither of this arc's two real corpora does — rejecting or truncating such a line number outright would be a silent correctness bug waiting to happen, not a hypothetical worth ignoring. `write_positions`/`read_positions` (`erars-bytecode/src/lib.rs`) therefore prepend each function's `positions` table with a 1-byte flag, computed once per function as `positions.iter().any(|&(_, line)| line > u16::MAX as u32)`: `0` selects a narrow encoding, 6 bytes per entry (`pc: u32`, `line: u16`); `1` selects the previous wide encoding unchanged, 8 bytes per entry (`pc: u32`, `line: u32`). A function that overflows falls back to the wide encoding for its own `positions` table only — every other function in the same file still gets the narrow encoding if its own lines fit, so one abnormally large generated function does not force every other function's table wide. This is a per-function choice, not a per-file or global one, matching `positions`'s own existing per-function granularity (each `FunctionBody` owns its table independently) and requiring no change to `FunctionBody`'s in-memory representation (`positions: Box<[(u32, u32)]>` — unchanged, in `erars-vm/src/function.rs`): only `erars-bytecode`'s serialization functions differ. This was chosen over an escape-entry-within-the-table or a varint/delta scheme specifically because it keeps `line_at`'s binary search (§6) reading a plain fixed-width array either way, with no per-entry branching at lookup time — the encoding choice only matters at (de)serialization, never during a `line_at` call itself.

`VERSION_MAGIC` bumped `[2,3,2,3,0,0,0,12]` → `[2,3,2,3,0,0,0,13]`: an older reader has no concept of the new leading flag byte and would misinterpret every subsequent function's positions table (and everything serialized after it) once flag/length framing shifts — the bump turns an old-vs-new mismatch into the existing clean-rejection path rather than a silent misparse.

**Test.** `crates/erars-bytecode/tests/positions_line_encoding.rs` compiles two real functions through the actual `erars_compiler::compile()` (not a hand-built `CompiledFunction`) — one with a label at line 12,345 (narrow), one at line 70,000 (`> u16::MAX`, forces wide) — inserts both into one `FunctionDic`, round-trips through `write_to`/`read_from`, and asserts `positions()`/`line_at(0)` match exactly for both. This defends the actual overflow path with a real line number a generated file could produce, not a synthetic struct carrying an implausible value. The pre-existing `stale_magic_is_rejected_and_positions_round_trip` test already exercised the wide path incidentally (one hand-built entry uses `line: 4_000_000_000`) and continues to pass unmodified. `cargo test -p erars-bytecode`: 2 passed, 0 failed. `cargo test --workspace`: 451 passed, 0 failed (unchanged from §9 — this fix touches no test-covered behavior other than what the new test itself defends).

**Byte savings, measured** (`erars-stdio --save --quite`, dual-order 10-round protocol, prebuilt `8d6b7f6` "before" vs. this commit's "after"):

| Corpus | Deltas (After−Before, B), old-first then new-first | Sign agreement | Median | Min | Max | % of pre-fix `game.era` |
|---|---|---|---|---|---|---|
| eraTHYMKR | -1608906, -1608830, -1608960, -1608838, -1609154, -1609148, -1608968, -1609088, -1609022, -1609088 | 10/10 negative | **-1,608,995 B** | -1,608,830 B | -1,609,154 B | **-2.93%** |
| eramegaten_p_kr/Data | -1666575, -1666701, -1666535, -1666380, -1666593, -1666343, -1666581, -1666663, -1667266, -1666561 | 10/10 negative | **-1,666,578 B** | -1,666,343 B | -1,667,266 B | **-2.02%** |

This brings the arc's net remaining `game.era` growth (relative to the pre-arc `35435b9` baseline, reconstructed by subtracting §7's measured `+2,506,243 B`/`+3,190,310 B` growth from the pre-u16 medians) down from **+4.774%/+4.020%** (§7) to **+1.71% (eraTHYMKR, +897,279 B) / +1.92% (eramegaten, +1,523,728 B)** — roughly two-thirds of the size cost eliminated, consistent with the 8→6-byte (25%) shrink per narrow entry applied against the ~15% of statements the `positions` table exists for.

**Serialize-time cost: small but real.** Isolating the `erars-bytecode` write step specifically (`phases` example's `bytecode write_to (Vec)` wall-min row, dual-order 10-round protocol — the metric this fix actually touches, as opposed to `parse+compile serial`'s in-memory positions-vector construction, which this fix does not touch at all): eraTHYMKR median **+0.8ms (+11.68%)**, 10/10 rounds slower; eramegaten median **+1.3ms (+7.72%)**, 9/10 rounds slower. This is a real, consistent regression, not noise — replacing one bulk array write with a per-entry branching write (flag check, then either 6 or 8 bytes via individual `write_u32`/`write_u16` calls) costs a small, measurable amount at *save* time. It is one-time per save, sub-2ms in absolute terms against either corpus's ~7-20ms `write_to` step and ~220-500ms total load pipeline, and — per the design goal above — does not touch the interpreter's hot dispatch path at all, so it was accepted rather than chased further with a hand-rolled buffered writer.

**Deserialize (load) time.** eraTHYMKR (`--load --bench-timing` isolated via an empty `--use-input` queue + `--exit-when-input-exhausted`, min-of-5-samples per round to damp process-launch noise, dual-order 10 rounds): median **+0.25ms (+0.37%)**, 6/10 rounds slower, 4/10 faster — indistinguishable from noise at eraTHYMKR's ~65-70ms total load time; **no measurable regression**. eramegaten's `--load` path could not be measured for this fix at all: it fails independently, on **both** the pre-u16-fix and post-u16-fix binaries, with `Failed to load <path>: depth limit exceeded` — traced to `rmp_serde::decode::from_read`'s default recursion-depth guard rejecting the `(HeaderInfo, HashMap<StrKey, Vec<(StrKey, VariableInfo)>>)` blob `erars-loader/src/lib.rs:150-151` deserializes immediately after the `erars-bytecode` payload (confirmed present before this session's change touched anything, so it predates and is unrelated to the u16 fix). This is a distinct, pre-existing `--load`-specific defect, out of scope here — not eramegaten's already-documented (§11) title-screen `RAND` crash, which is a separate failure at a separate point (in-game execution, not deserialization) — and is left for separate investigation.

## 13. Measurement protocol (for reproduction/audit)

Every A/B number in this document follows the same rules:

1. **Build every binary variant first**, into distinct paths (e.g. `/tmp/bench_x_old/bin`, `/tmp/bench_x_new/bin`), before any timing run begins — never rebuild mid-comparison.
2. **Interleave, don't block.** Run old/new alternately within one batch (never all-old-then-all-new), and for the more consequential comparisons (§6, §8) run the alternation in *both* orders (5 old-first + 5 new-first = 10 total) to rule out any first-run/thermal/scheduler ordering bias.
3. **Report per-round deltas, then min/median**, not just an aggregate. Every comparison in this document states its sign agreement explicitly (e.g. "10/10 rounds negative") — a real effect must hold under every round and both interleave orders, not just on average.
4. `uptime` before and after each batch, to flag (not silently absorb) load-average contamination from other processes on the box — see §6's noted load spike, which did not change the conclusion because the effect size and sign agreement held regardless.
5. **Correctness guard precedes every timing claim.** `BENCH_DEBUG=1 <binary> <iters>` dumps the compiled instruction stream and the analytically-computed dynamic histogram; compare old vs new with `diff` on the full histogram output (or, for corpus-scale checks, on `jit_census`'s full "Instruction census" section), never by eyeballing a handful of top rows. A change is only "faster," not "different," once every row outside the one(s) the change legitimately touches is confirmed byte-for-byte identical.
6. Reproduction commands used throughout:
   - `cargo build --release -p erars-loader --example jit_ceiling_bench` (per commit/worktree, into a distinct `target/` via `git worktree add /tmp/<name> <commit>`)
   - `<binary> 3000000` for a timing round; `BENCH_DEBUG=1 <binary> 10` for the correctness-guard dump
   - `cargo build --release --features multithread -p erars-loader --example phases --example jit_census` for the corpus tools; `phases <game-dir> <rounds>`, `jit_census <game-dir>`
   - `erars-stdio <game-dir> --save --quite` for the `game.era` on-disk size measurement (`run_script` only loads/compiles/lints and returns before any game loop starts, confirmed non-interactive and safe to script)
   - `erars-stdio <game-dir> --quite --load --bench-timing --use-input <ron-file> --exit-when-input-exhausted < /dev/null` for §11's real-workload replay (requires a version-matched `game.era` already saved via `--save` from the *same* binary); `--features inst-counter` to additionally dump the dynamic instruction histogram

**Methodology note: two unchecked negative claims, both corrected in this arc.** Twice now, this document (or a prior draft of it) asserted that something could not be done or did not carry certain data, without actually trying — both times the claim was false. First, an earlier draft claimed `game.era` did not carry `VariableInfo`; it does, and the claim was corrected before this revision. Second, an earlier draft of this document's own caveat (see the top of this document) claimed running a real corpus's game loop end-to-end "is not automatable... it is an interactive text game" — false, and §11 is the direct refutation: `erars-stdio --use-input` already drove real corpora headlessly before this arc even started, and the one genuinely missing piece (a clean way for a scripted queue to end the process, `--exit-when-input-exhausted`, added for §11) took under an hour to add. The pattern in both cases is the same: a negative claim about feasibility or data availability was treated as established fact without being tested against the actual tool. Every such claim in this document going forward should be treated as a hypothesis to check, not a conclusion to state, until it has actually been tried.
