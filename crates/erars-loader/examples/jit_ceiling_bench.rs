//! bytecode-opt Step 1: a real interpreter-loop A/B vehicle for measuring
//! dispatch-loop changes to `erars-vm` (e.g. Step 2's if-else-chain -> match
//! conversion, Step 3's `ReportPosition` side table).
//!
//! This does NOT compare against a native Rust loop (that comparison was
//! this file's original purpose on `llvm-jit`, an AOT-feasibility "ceiling"
//! probe — see that branch's history for it). A native comparison makes no
//! sense here: the loop body below prints and branches on data the compiler
//! cannot see through, so there is no meaningful "native equivalent" to
//! write, and more importantly IT ISN'T THE QUESTION. This file exists to
//! answer "did an internal `erars-vm` dispatch change make the interpreter
//! itself faster", by A/B-ing the SAME benchmark binary rebuilt from two
//! different commits — the interpreter's own timing across those two builds
//! *is* the A and the B.
//!
//! ## Why the loop body isn't pure arithmetic
//!
//! `jit_census.rs` (bytecode-opt commit 42840c7) measured instruction
//! frequency across two real ERB corpora (eraTHYMKR, eramegaten_p_kr). A
//! benchmark loop dominated by one or two instruction types (as the original
//! `LOCAL = LOCAL + 1` ceiling probe was, deliberately, for its own
//! narrower question) would mis-rank any optimization whose payoff is
//! concentrated in instructions that loop barely executes. The body below
//! mixes variable reads/writes, arithmetic, string-keyed variable-reference
//! lookups, a branch, and a `PRINTFORM` per iteration, in proportions
//! chosen to track the corpus-observed unigram census (see the statement-mix
//! comment in `main` below and `verify_instruction_mix`, which asserts the
//! compiled body's dynamic instruction proportions against it every run —
//! if a future edit to the loop source drifts the mix, the assertion
//! catches it rather than silently invalidating whatever comparison is
//! being made against this benchmark).
//!
//! ## Why there's no `black_box` trap here
//!
//! The original ceiling probe's native comparison loop
//! (`while std::hint::black_box(local) < iters { local += 1; }`) needed
//! `black_box` because LLVM can prove a data-free native loop closed-form
//! and strength-reduce it to a single assignment — that mistake, made once
//! on `llvm-jit`, produced a bogus ~400,000,000x "speedup" that measured
//! nothing (`docs/research/2026-09-05-llvm-aot-feasibility.md` on that
//! branch). `TerminalVm::try_call` cannot be proven side-effect-free and
//! elided the same way: it is a real call across a crate boundary into a
//! bytecode interpreter doing heap-allocated variable storage, dynamic
//! dispatch through `SystemFunctions`, and (below) actual `PRINTFORM`
//! evaluation and console writes every iteration. The result is still
//! asserted afterwards as a second guard against any future refactor
//! quietly making this callable from a context where it could be inlined
//! and proven pure.
//!
//! ## Why `ISSKIP 1`
//!
//! `PRINTFORM` is in the mix because Print is a top-9 corpus instruction
//! (3.7-6.3%), and the case that actually matters for this benchmark
//! (dispatch cost of the Print instruction: `ctx.pop_str()`, flag checks,
//! `tx.print()`) does NOT include the cost of buffering or rendering
//! millions of lines of console output, which would swamp the loop in
//! `VecDeque` growth/eviction rather than measuring dispatch. `ISSKIP 1`
//! (`tx.set_skipdisp(true)`) makes `tx.print()` an early-return no-op
//! (`crates/erars-ui/src/lib.rs`, `skipdisp` checks in `print`/`print_line`)
//! while the `Print` instruction itself still dispatches normally and still
//! pops/formats its string argument — the real per-instruction cost this
//! benchmark is trying to isolate.
//!
//! `cargo run --release -p erars-loader --example jit_ceiling_bench -- [iters]`
//! `BENCH_DEBUG=1` dumps the compiled instruction stream and the computed
//! per-type dynamic (executed) counts before running.
//! `BENCH_PROFILE=1` runs one extra pass under `pprof` and prints leaf self time.

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use erars_ast::StrKey;
use erars_compiler::{Bump, EraConfig, HeaderInfo, Instruction, ParserContext};
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};
use erars_ui::VirtualConsole;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn uptime() -> String {
    std::process::Command::new("uptime")
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default()
}

fn main() {
    erars_ast::init_interner();
    let iters: i64 = std::env::args()
        .nth(1)
        .map(|s| s.parse().unwrap())
        .unwrap_or(3_000_000);
    // The IF/ELSE branch below is keyed on `LOCAL < iters/2`: a clean even
    // `iters` keeps the two phases (SUM-accumulation for the first half,
    // PRINTFORM for the second half) exact halves, with no remainder to
    // reason about when validating the instruction-count math below.
    assert_eq!(iters % 2, 0, "iters must be even for exact phase-split math");

    println!("uptime: {}", uptime());

    // Statement mix per iteration, chosen against the jit_census corpus
    // unigram census (average of eraTHYMKR/eramegaten_p_kr, `/tmp/*_census3.txt`):
    // LoadStr ~23%, LoadInt ~19%, ReportPosition ~15%, LoadVarRef ~12%,
    // Print ~5%, BinaryOperator ~5%, Goto ~5%, GotoIfNot ~4%, StoreVar ~3%.
    // `verify_instruction_mix` below checks the compiled result against this
    // table every run. The branch is a single-comparison `IF LOCAL < half`
    // (not e.g. a modulo test) deliberately: it keeps BinaryOperator usage
    // down near the corpus's own ~5% share, where a `%`+`==` per-iteration
    // check would have pushed it well above corpus proportions for no
    // representativeness gain (the census does not show real scripts
    // computing a modulus on every branch).
    let half = iters / 2;
    let src = format!(
        "@BENCH\n\
         #DIM SUM\n\
         ISSKIP 1\n\
         LOCAL = 0\n\
         SUM = 0\n\
         WHILE LOCAL < {iters}\n\
         \tIF LOCAL < {half}\n\
         \t\tSUM = SUM + LOCAL\n\
         \tELSE\n\
         \t\tPRINTFORM iter {{LOCAL}}\n\
         \tENDIF\n\
         \tLOCAL = LOCAL + 1\n\
         WEND\n"
    );

    let var_infos: hashbrown::HashMap<StrKey, erars_ast::VariableInfo> =
        serde_yaml::from_str(include_str!("../src/variable.yaml")).unwrap();
    let header = Arc::new(HeaderInfo {
        global_variables: var_infos,
        ..Default::default()
    });
    let ctx_p = ParserContext::new(header.clone(), StrKey::new("BENCH.ERB"));
    let mut pp = ctx_p.preprocessor(src.as_str());
    let mut b = Bump::new();
    let erb = ctx_p.parse_and_compile(&mut pp, &mut b).unwrap_or_else(|e| {
        panic!("compile error: {}", e.0);
    });
    for (err, _) in &erb.errors {
        panic!("compile error: {err}");
    }
    assert_eq!(erb.functions.len(), 1, "expected exactly one compiled function");

    let body: Vec<Instruction> = erb.functions[0].body.iter().copied().collect();
    if std::env::var_os("BENCH_DEBUG").is_some() {
        for (i, inst) in body.iter().enumerate() {
            eprintln!("{i}: {inst:?}");
        }
        eprintln!("goto_labels: {:?}", erb.functions[0].goto_labels);
    }

    let histogram = dynamic_instruction_histogram(&body, iters);
    print_histogram(&histogram, iters);
    verify_instruction_mix(&histogram);

    let config = Arc::new(EraConfig::default());
    let mut ctx = VmContext::new(
        header.clone(),
        config.clone(),
        Box::new(NullSystemFunctions),
        PathBuf::from("/tmp/erars-jit-bench-sav"),
        PathBuf::from("/tmp/erars-jit-bench-content"),
    );

    let mut dic = FunctionDic::new();
    dic.insert_compiled_func(
        &mut ctx.var,
        &header.default_local_size,
        erb.functions.into_iter().next().unwrap(),
    );

    let vm = TerminalVm::new(dic, header.clone());
    let mut tx = VirtualConsole::new(&console_config(&config));

    // Interleaved rounds, per the project's measurement rules — noisy box,
    // take min/median of ROUNDS runs of the SAME (unchanging within this
    // process) benchmark. Cross-commit A/B is done by running this binary
    // twice, once built from each commit, and comparing THOSE numbers.
    const ROUNDS: usize = 9;
    let mut interp_times = Vec::with_capacity(ROUNDS);

    for round in 0..ROUNDS {
        let t0 = Instant::now();
        let workflow = vm.try_call("BENCH", &[], &mut tx, &mut ctx).unwrap();
        let interp_dt = t0.elapsed();
        assert!(workflow.is_some(), "BENCH function not found");
        let local = ctx.var.read_local_int("BENCH", "LOCAL", &[]).unwrap();
        assert_eq!(local, iters, "interpreter result mismatch on round {round}");
        interp_times.push(interp_dt);

        println!(
            "round {round}: interpreter {:>10.3}ms  ({:.2}ns/iter)",
            interp_dt.as_secs_f64() * 1000.0,
            interp_dt.as_secs_f64() * 1e9 / iters as f64,
        );
    }

    interp_times.sort();
    let min = interp_times[0];
    let med = interp_times[ROUNDS / 2];
    let max = interp_times[ROUNDS - 1];

    println!("\n=== summary ({iters} iterations, {} total dynamic instructions) ===", histogram.total);
    println!(
        "interpreter: min {:.3}ms  median {:.3}ms  max {:.3}ms",
        min.as_secs_f64() * 1000.0,
        med.as_secs_f64() * 1000.0,
        max.as_secs_f64() * 1000.0,
    );
    println!(
        "per-iteration: {:.2}ns   per-dynamic-instruction: {:.2}ns",
        min.as_secs_f64() * 1e9 / iters as f64,
        min.as_secs_f64() * 1e9 / histogram.total as f64,
    );

    if std::env::var_os("BENCH_PROFILE").is_some() {
        let guard = pprof::ProfilerGuardBuilder::default().frequency(1997).build().unwrap();
        let workflow = vm.try_call("BENCH", &[], &mut tx, &mut ctx).unwrap();
        assert!(workflow.is_some());
        let report = guard.report().build().unwrap();
        let mut total = 0isize;
        let mut leaves: hashbrown::HashMap<String, isize> = hashbrown::HashMap::new();
        for (frames, count) in report.data.iter() {
            total += *count;
            let leaf = frames
                .frames
                .iter()
                .flatten()
                .find(|s| !s.name().starts_with("<unknown>"))
                .map(|s| s.name())
                .unwrap_or_else(|| "<unknown>".into());
            *leaves.entry(leaf).or_default() += *count;
        }
        let mut rows: Vec<_> = leaves.into_iter().collect();
        rows.sort_by_key(|(_, c)| -*c);
        println!("\n-- self time, {total} samples --");
        for (name, count) in rows.iter().take(30) {
            println!("{:>6.2}%  {}", *count as f64 / total as f64 * 100.0, name);
        }
    }

    println!("\nuptime: {}", uptime());
}

/// Per-instruction-type dynamic (executed) counts, computed by simulating
/// the exact same cursor walk `run_body` performs — using the compiled
/// body's own real `Goto`/`GotoIfNot` targets — rather than by
/// instrumenting the interpreter (Step 1 must not touch `erars-vm`). The
/// only thing this simulation cannot read off the bytecode itself is which
/// way the one data-dependent branch (`IF LOCAL < half`) goes on a given
/// iteration (the VM only knows that by holding `LOCAL`'s real value on its
/// stack); for that one decision this mirrors the ERB source's own
/// `i < iters / 2` phase split directly. If the loop source above is ever
/// edited to change that condition, `verify_instruction_mix` is the
/// regression guard that catches a stale oracle here rather than silently
/// producing a wrong histogram.
struct Histogram {
    counts: Vec<(String, u64)>,
    total: u64,
}

fn dynamic_instruction_histogram(body: &[Instruction], iters: i64) -> Histogram {
    // The WHILE's own condition check is the first `GotoIfNot` in program
    // order: nothing before it can be a backward jump target yet, so it's
    // unambiguous. Its back-edge is the `Goto` (WEND's jump) whose target
    // is <= that index; structured `WHILE`/`WEND` never jumps forward past
    // its own condition check, so the last such `Goto` in program order is
    // the real back-edge (not, e.g., an inner `IF`'s own skip-`Goto`, which
    // always targets a point later in the body).
    let while_check = body
        .iter()
        .position(|i| i.as_goto_if_not().is_some())
        .expect("no WHILE condition (GotoIfNot) found in compiled body");
    let back_edge = body
        .iter()
        .enumerate()
        .filter(|(_, i)| i.as_goto().is_some_and(|t| t as usize <= while_check))
        .next_back()
        .expect("no WHILE back-edge (Goto) found in compiled body");
    let cond_start = back_edge.1.as_goto().unwrap() as usize;
    let half = iters / 2;

    // One iteration's cursor walk (real `Goto`/`GotoIfNot` targets from the
    // compiled body, `take_if_branch` forces the one data-dependent
    // decision — `IF LOCAL < half` — the same way `i < half` would in the
    // real interpreter) returns the *set* of indices it touched. Every
    // index visited by BOTH the if-taken and else-taken walks is visited on
    // every one of the `iters` iterations (it's outside the branch); an
    // index visited by only one of them is that arm's own body, visited
    // only on the `half` (if) or `iters - half` (else) iterations that take
    // it. This replaces an O(iters) simulation with two O(body length)
    // walks — the loop's control-flow shape doesn't depend on `iters`,
    // only which arm's body appears in the resulting set, so multiplying
    // by the known iteration split after the fact is exact, not sampled.
    let walk_once = |take_if_branch: bool| -> Vec<usize> {
        let mut visited = Vec::new();
        let mut cursor = cond_start;
        loop {
            visited.push(cursor);
            let inst = &body[cursor];
            if let Some(target) = inst.as_goto_if_not() {
                if cursor == while_check {
                    cursor += 1; // condition true this iteration
                } else {
                    cursor = if take_if_branch { cursor + 1 } else { target as usize };
                }
            } else if let Some(target) = inst.as_goto() {
                let dest = target as usize;
                if dest == cond_start {
                    break; // back edge: this iteration is done
                }
                cursor = dest;
            } else {
                cursor += 1;
            }
        }
        visited
    };
    let visited_if: hashbrown::HashSet<usize> = walk_once(true).into_iter().collect();
    let visited_else: hashbrown::HashSet<usize> = walk_once(false).into_iter().collect();

    let mut totals: hashbrown::HashMap<String, u64> = hashbrown::HashMap::new();
    let mut total = 0u64;
    let mut record = |inst: &Instruction, times: u64| {
        *totals.entry(instruction_kind_name(inst)).or_default() += times;
        total += times;
    };

    // Prologue: everything before the condition, runs once.
    for inst in &body[..cond_start] {
        record(inst, 1);
    }
    for &idx in visited_if.union(&visited_else) {
        let times = match (visited_if.contains(&idx), visited_else.contains(&idx)) {
            (true, true) => iters as u64,
            (true, false) => half as u64,
            (false, true) => (iters - half) as u64,
            (false, false) => unreachable!("index came from the union of the two sets"),
        };
        record(&body[idx], times);
    }
    // The final, failing WHILE check (`LOCAL == iters`): the condition-eval
    // instructions run once more, then exit instead of looping.
    for inst in &body[cond_start..=while_check] {
        record(inst, 1);
    }

    let mut counts: Vec<(String, u64)> = totals.into_iter().collect();
    counts.sort_by_key(|(_, c)| std::cmp::Reverse(*c));
    Histogram { counts, total }
}

/// Mirrors `jit_census.rs`'s `variant_name`: `InstructionType`/`ty` are
/// private to `erars-compiler`, so recover the variant name from `Debug`,
/// which strum-derives to the bare enum name for empty variants and
/// `Name(data)` otherwise. Both files will switch to a public discriminant
/// once Step 2 lands one.
fn instruction_kind_name(inst: &Instruction) -> String {
    let s = format!("{inst:?}");
    match s.find('(') {
        Some(idx) => s[..idx].to_string(),
        None => s,
    }
}

fn print_histogram(h: &Histogram, iters: i64) {
    println!("\n=== dynamic instruction histogram ({iters} iterations, computed analytically) ===");
    for (name, count) in &h.counts {
        println!("{name:>20} {count:>14}  ({:5.2}%)", 100.0 * *count as f64 / h.total as f64);
    }
}

/// Checks the compiled loop's dynamic instruction proportions against the
/// jit_census corpus averages (see the doc comment at the top of this
/// file). Loose bounds, with headroom above the corpus's own per-type
/// shares — this loop is a tight, variable-access-heavy microbenchmark, so
/// LoadStr/LoadVarRef naturally run higher than a full-corpus average that
/// includes non-loop scaffolding (menus, dialogue, calls); the check is a
/// sanity/drift guard against a future edit to the ERB source silently
/// producing a degenerate mix (e.g. all-arithmetic, no branches), not a
/// precision claim about matching the corpus exactly.
fn verify_instruction_mix(h: &Histogram) {
    let pct = |name: &str| -> f64 {
        h.counts
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, c)| 100.0 * *c as f64 / h.total as f64)
            .unwrap_or(0.0)
    };
    let checks: &[(&str, f64, f64)] = &[
        ("LoadStr", 15.0, 35.0),
        ("LoadInt", 5.0, 30.0),
        ("ReportPosition", 8.0, 22.0),
        ("LoadVarRef", 6.0, 25.0),
        ("Print", 0.5, 12.0),
        ("BinaryOperator", 2.0, 15.0),
        ("Goto", 1.0, 10.0),
        ("GotoIfNot", 1.0, 10.0),
    ];
    let mut failures = Vec::new();
    for (name, lo, hi) in checks {
        let p = pct(name);
        if p < *lo || p > *hi {
            failures.push(format!("{name}: {p:.2}% not in [{lo}, {hi}]%"));
        }
    }
    assert!(
        failures.is_empty(),
        "loop body's dynamic instruction mix drifted from the jit_census-derived target range: {failures:?}"
    );
}
