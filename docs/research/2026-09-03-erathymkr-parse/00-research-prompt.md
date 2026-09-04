# Research task: fastest and most accurate parse+compile of eraTHYMKR with erars

You are one of three independent researchers (opus, kimi, glm). Your name: <NAME>.
Working repo: /home/riey/repos/erars (Rust workspace, branch new-renderer). Target game: /home/riey/repos/eraTHYMKR — an Emuera 1.8.1.8 (kr3) game: 873 ERB files, ~1.46M lines, 62 MB, UTF-8 with BOM, plus 7 ERH and 251 CSV.

Goal: find how erars can parse and compile this game (a) as fast as possible and (b) as accurately as possible (Emuera-parity: no false parse errors, same semantics). Evidence over opinion: measure and cite code.

## Hard rules
- READ-ONLY on the repos: do not edit, create, or delete files under /home/riey/repos/erars or /home/riey/repos/eraTHYMKR. No git commands that change state. No `cargo clean`.
- Put every scratch file and your report under /tmp/claude-1000/-home-riey-repos-erars/6648b565-fad0-4a94-b80a-ec88c824873c/scratchpad/<NAME>/ (create it if missing).
- Do not install tools. Available: cargo, hyperfine. perf/samply are NOT installed.
- Build exactly `cargo build --release -p erars-stdio` in /home/riey/repos/erars and reuse target/release/erars-stdio. Other processes share that target dir; cargo waits on its lock, that is expected. If you need different features, add `--target-dir /tmp/claude-1000/-home-riey-repos-erars/6648b565-fad0-4a94-b80a-ec88c824873c/scratchpad/<NAME>/target`.
- Do not use bash heredocs; write files with your write tool.

## Files that matter (open all of them)
- crates/erars-loader/src/lib.rs — run_script pipeline: glob CSV/ERH/ERB, merge CSV+ERH into HeaderInfo, then ERB parse_and_compile (rayon par_bridge under feature `multithread`), then single-threaded insert_compiled_func; save_script/load_script = bytecode cache (erars-bytecode + rmp-serde + memmap2). Phase timings are logged by the check_time! macro.
- crates/erars-loader/Cargo.toml (multithread feature), crates/erars-stdio/Cargo.toml (check whether it enables it), crates/erars-stdio/src/main.rs (CLI flags --save/--load, --log-level, --use-input)
- crates/erars-compiler/src/parser.rs (ParserContext, Preprocessor + PP_REGEX, parse_stmt, parse_and_compile; nom 7), crates/erars-compiler/src/parser/expr.rs, crates/erars-compiler/src/compiler.rs, crates/erars-compiler/src/instruction.rs
- crates/erars-lexer/src/lib.rs, inst.rs, sharp.rs, square.rs, utils.rs (logos 0.14)
- crates/erars-ast/src/*.rs (AST, StrKey interner)
- crates/erars-bytecode/src/lib.rs
- benches/parse.rs (criterion bench; synthetic input only)
- Cargo.toml (workspace deps and profiles)

## Questions to answer
1. Baseline. Time `target/release/erars-stdio /home/riey/repos/eraTHYMKR --save --log-level info < /dev/null` (wrap in `timeout 600`). With `--save` the process compiles, writes the bytecode file into the game dir, and exits, so hyperfine works. Do NOT pass `--use-input /dev/null`: it panics (RON parse of an empty file). Phase timings are printed to stdout as `[phase]: Nms` and also logged under /home/riey/repos/eraTHYMKR/logs/. Report per-phase times (Initialize / CSV / ERH / Parse-Compile ERB / Save) and wall time, single-thread vs multithread if the feature differs. Also time `--load` (bytecode path). Use hyperfine when the process exits by itself.
2. Accuracy. Collect every diagnostic erars emits on eraTHYMKR (E1000 ERH, E2000 ERB parse errors, and lint if run). Group by root cause: which Emuera syntax or feature erars mishandles. For each group: count, one example ERB file:line, and the erars code path responsible (file:line). Distinguish "erars rejects valid Emuera code" from "game bug Emuera also rejects".
3. Speed. Where does the time go? Use whatever you can: per-phase logs, `--log-level debug` per-file timings, running the parser on the largest ERB files, reasoning from the code. Evaluate at least: file read + BOM/UTF-8 decode path, the Preprocessor regex pass, logos lexer, nom parser structure (backtracking, String allocation), StrKey interning (global interner contention under rayon), Bump arena use, rayon granularity (par_bridge over a glob iterator, per-file work imbalance: list the 10 largest ERB files), single-threaded insert_compiled_func, bytecode serialization format (rmp-serde) vs zero-copy alternatives (rkyv etc), and a per-file incremental cache keyed by content hash so unchanged files skip parsing.
4. Parallel-correctness risks: anything order-dependent in parsing (rename/#DEFINE, ERH merge order, duplicate function definitions, function registration order) that a faster design must preserve.

## Deliverable
Write /tmp/claude-1000/-home-riey-repos-erars/6648b565-fad0-4a94-b80a-ec88c824873c/scratchpad/<NAME>/report.md with sections:
1. Baseline numbers (table)
2. Accuracy findings (table: category, count, example file:line, erars code path, valid-Emuera-or-game-bug)
3. Speed findings (hot spots with evidence)
4. Recommendations, ranked (change, expected gain, effort, risk, which files)
5. Open questions
Then print the full report as your final answer.
