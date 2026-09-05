//! Phase-1 feasibility census for the LLVM JIT spike.
//!
//! `cargo run --release -p erars-loader --features multithread --example jit_census -- <game dir>`
//!
//! For every compiled function in the corpus: tallies `Instruction` variant
//! frequency, categorizes each variant, and reports what fraction of
//! functions consist solely of "JIT-able" categories (pure computation,
//! control flow, variable access) with no host/system call.
//!
//! Corpus loading mirrors `phases.rs` (same header-build + parse_and_compile
//! path) rather than re-deriving it.

use std::path::PathBuf;

use erars_ast::StrKey;
use erars_compiler::{Bump, HeaderInfo, Instruction, ParserContext};
use erars_reader::read_file;
use erars_vm::{FunctionDic, VariableStorage};
use hashbrown::HashMap;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn erb_paths(target: &str) -> Vec<PathBuf> {
    let mut v = glob::glob_with(
        &format!("{target}/ERB/**/*.ERB"),
        glob::MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: true,
            require_literal_separator: true,
        },
    )
    .unwrap()
    .map(|p| p.unwrap())
    .collect::<Vec<_>>();
    v.sort();
    v
}

fn build_header(target: &str) -> HeaderInfo {
    let var_infos: HashMap<StrKey, erars_ast::VariableInfo> =
        serde_yaml::from_str(include_str!("../src/variable.yaml")).unwrap();
    let mut info = HeaderInfo {
        global_variables: var_infos,
        ..Default::default()
    };

    let csvs = glob::glob_with(
        &format!("{target}/CSV/**/*.CSV"),
        glob::MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: true,
            require_literal_separator: true,
        },
    )
    .unwrap();

    let mut csv_dic: HashMap<String, String> = HashMap::new();
    let mut chara = Vec::new();
    for csv in csvs {
        let csv = csv.unwrap();
        let s = match read_file(&csv) {
            Ok(s) => s,
            Err(_) => continue,
        };
        let k = csv.file_stem().unwrap().to_str().unwrap().to_ascii_uppercase();
        if k.starts_with("CHARA") {
            chara.push(s);
        } else {
            csv_dic.insert(k, s);
        }
    }

    for (k, v) in csv_dic.iter() {
        let _ = match k.as_str() {
            "ABL" | "MARK" | "BASE" | "CFLAG" | "EQUIP" | "TEQUIP" | "PALAM" | "EXP" | "EX"
            | "FLAG" | "TFLAG" | "TALENT" | "STAIN" | "SOURCE" | "TSTR" | "CSTR" | "SAVESTR"
            | "GLOBAL" | "GLOBALS" | "TRAIN" | "TCVAR" => info.merge_name_csv(k, v),
            "STRNAME" => info.merge_name_csv("STR", v),
            "STR" => info.merge_str_csv(v),
            "GAMEBASE" => info.merge_gamebase_csv(v),
            "VARIABLESIZE" => info.merge_variable_size_csv(v),
            "_RENAME" => info.merge_rename_csv(v),
            "_REPLACE" => info.merge_replace_csv(v),
            "ITEM" => info.merge_item_csv(v),
            _ => Ok(()),
        };
    }
    for c in chara {
        let _ = info.merge_chara_csv(&c);
    }

    let mut erhs = glob::glob_with(
        &format!("{target}/ERB/**/*.ERH"),
        glob::MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: true,
            require_literal_separator: true,
        },
    )
    .unwrap()
    .map(Result::unwrap)
    .collect::<Vec<_>>();
    erhs.sort_by_cached_key(|p| p.to_string_lossy().to_lowercase());

    let sources: Vec<String> = erhs.iter().map(|erh| read_file(erh).unwrap()).collect();
    let mut pending = Vec::new();
    for (idx, source) in sources.iter().enumerate() {
        info.merge_header_defines(idx, source, &mut pending).unwrap();
    }
    for (idx, (err, _)) in info.resolve_pending_dims(pending) {
        panic!("Unresolved #DIM in {}: {err}", erhs[idx].display());
    }

    info
}

/// `InstructionType`/`ty` are private to `erars-compiler`; recover the
/// variant name from `Debug`, which strum-derives to the bare enum name for
/// empty variants and `Name(data)` otherwise.
fn variant_name(inst: Instruction) -> String {
    let s = format!("{inst:?}");
    match s.find('(') {
        Some(idx) => s[..idx].to_string(),
        None => s,
    }
}

#[derive(Default, Clone, Copy)]
struct FuncStats {
    total_insts: usize,
    host_insts: usize,
    control_call_insts: usize,
}

/// (a) pure computation, (b) control flow, (c) variable access, (d) host call.
fn category(name: &str) -> &'static str {
    match name {
        // (a) pure computation: arithmetic, comparison, stack manip, constants
        "Nop" | "Pop" | "Duplicate" | "DuplicatePrev" | "LoadInt" | "LoadIntSuffix"
        | "LoadStr" | "ConcatString" | "Times" | "BinaryOperator" | "UnaryOperator"
        | "ReportPosition" => "pure",

        // (b) control flow, in-function: static jumps resolve to a
        // compile-time offset (`Goto`/`GotoIf`/`GotoIfNot`); `GotoLabel`/
        // `TryGotoLabel` are the rare dynamic-label form (label name is a
        // runtime string, resolved via a linear scan of this function's own
        // `goto_labels` table) but still never leave the function body.
        "Goto" | "GotoIf" | "GotoIfNot" | "GotoLabel" | "TryGotoLabel" => "control_local",

        // Function/event calls and `#BEGIN`: these leave the function body
        // entirely and re-enter the VM's dynamic dispatch (`FunctionDic`
        // lookup by name, a fresh call frame/local table, arbitrary callee
        // bytecode, `Workflow` handling). Native code can only reach this
        // via a call back into the interpreter/runtime, not a branch —
        // counted separately from in-function control flow.
        "Call" | "TryCall" | "Jump" | "TryJump" | "CallEvent" | "Begin" => "control_call",

        // (c) variable access: read/write to VariableStorage. `StoreResult`
        // writes the RESULT/RESULTS known variables through
        // `VariableStorage::set_result`, so it belongs here, not with pure
        // stack ops.
        "LoadVarRef" | "LoadExternVarRef" | "LoadCountVarRef" | "ReadVar" | "StoreVar"
        | "StoreResult" => "var_access",

        // (d) host calls: console I/O, commands, anything touching
        // SystemFunctions/VM state outside the pure stack machine.
        // `EvalFormString` additionally requires the *parser/compiler* at
        // runtime (STRFORM et al. recompile a string into instructions on
        // the fly) — categorically un-jittable, not just "a host call".
        "Print" | "PrintButton" | "ReuseLastLine" | "BuiltinVar" | "BuiltinCommand"
        | "BuiltinMethod" | "SetAlignment" | "PadStr" | "EvalFormString" | "Debug"
        | "LoadDefaultArgument" => "host",

        other => panic!("uncategorized instruction variant: {other}"),
    }
}

fn main() {
    erars_ast::init_interner();
    let target = std::env::args().nth(1).expect("game path");

    println!("uptime: {}", std::process::Command::new("uptime")
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default());

    let header = std::sync::Arc::new(build_header(&target));
    let paths = erb_paths(&target);
    println!("{} ERB files", paths.len());

    let sources: Vec<String> = paths.iter().map(|p| read_file(p).unwrap()).collect();

    erars_ast::reset_literal_store();
    let mut funcs = Vec::new();
    let mut b = Bump::new();
    for (p, s) in paths.iter().zip(sources.iter()) {
        let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
        let mut pp = ctx.preprocessor(s.as_str());
        b.reset();
        match ctx.parse_and_compile(&mut pp, &mut b) {
            Ok(erb) => {
                funcs.extend(erb.functions);
                for (err, _) in erb.errors {
                    eprintln!("compile error {}: {err}", p.display());
                }
            }
            Err(e) => eprintln!("compile error {}: {}", p.display(), e.0),
        }
    }

    println!("{} raw parsed functions (before FunctionDic insertion)", funcs.len());

    // `FunctionDic::insert_func` is a plain `HashMap::insert` keyed by name:
    // two functions sharing a name silently collapse to whichever was
    // inserted last (sorted-filename order). Detect that here, before
    // insertion erases the evidence, so a raw-vs-registered count gap has an
    // explicit cause instead of being a silent rounding difference.
    let mut seen_names: HashMap<String, Vec<StrKey>> = HashMap::new();
    for f in &funcs {
        seen_names
            .entry(f.header.name.resolve().to_string())
            .or_default()
            .push(f.header.file_path);
    }
    let mut dupes: Vec<(&String, &Vec<StrKey>)> =
        seen_names.iter().filter(|(_, files)| files.len() > 1).collect();
    dupes.sort_by_key(|(name, _)| name.as_str());
    if !dupes.is_empty() {
        println!(
            "\n=== {} duplicate function name(s) across files (later file wins for normal functions) ===",
            dupes.len()
        );
        for (name, files) in &dupes {
            let file_list = files.iter().map(|f| f.resolve()).collect::<Vec<_>>().join(", ");
            println!("  {name}  x{}: {file_list}", files.len());
        }
    }

    let mut var = VariableStorage::new(header.clone(), &header.global_variables);
    let mut dic = FunctionDic::new();
    var.reserve_local_functions(funcs.len());
    for f in funcs {
        dic.insert_compiled_func(&mut var, &header.default_local_size, f);
    }

    // Walk every registered function body (normal + event) exactly once.
    let mut bodies: Vec<&erars_vm::FunctionBody> = Vec::new();
    for body in dic.normal.values() {
        bodies.push(body);
    }
    for coll in dic.event.values() {
        for body in coll.events.iter() {
            bodies.push(body);
        }
    }

    println!("{} function bodies (normal + event)", bodies.len());

    let mut inst_counts: HashMap<String, u64> = HashMap::new();
    let mut cat_counts: HashMap<&'static str, u64> = HashMap::new();
    let mut bigram_counts: HashMap<(String, String), u64> = HashMap::new();
    let mut post_p1_bigram_counts: HashMap<(String, String), u64> = HashMap::new();
    let mut total_insts: u64 = 0;
    let mut total_bigrams: u64 = 0;
    let mut total_post_p1_bigrams: u64 = 0;

    let mut jitable_funcs: u64 = 0;
    let mut jitable_insts: u64 = 0; // sum of body len over jit-able functions
    let mut leaf_jitable_funcs: u64 = 0;
    let mut leaf_jitable_insts: u64 = 0;
    let mut empty_funcs: u64 = 0;

    for body in &bodies {
        let insts = body.body();
        if insts.is_empty() {
            empty_funcs += 1;
            continue;
        }
        let mut stats = FuncStats::default();
        let mut prev_name: Option<String> = None;
        let mut prev_non_rp_name: Option<String> = None;
        for &inst in insts.iter() {
            let name = variant_name(inst);
            let cat = category(&name);
            total_insts += 1;
            stats.total_insts += 1;
            match cat {
                "host" => stats.host_insts += 1,
                "control_call" => stats.control_call_insts += 1,
                _ => {}
            }
            *inst_counts.entry(name.clone()).or_default() += 1;
            *cat_counts.entry(cat).or_default() += 1;
            if let Some(prev) = prev_name.take() {
                *bigram_counts.entry((prev, name.clone())).or_default() += 1;
                total_bigrams += 1;
            }
            prev_name = Some(name.clone());
            if name != "ReportPosition" {
                if let Some(prev) = prev_non_rp_name.take() {
                    *post_p1_bigram_counts.entry((prev, name.clone())).or_default() += 1;
                    total_post_p1_bigrams += 1;
                }
                prev_non_rp_name = Some(name);
            }
        }
        if stats.host_insts == 0 {
            jitable_funcs += 1;
            jitable_insts += stats.total_insts as u64;
            if stats.control_call_insts == 0 {
                leaf_jitable_funcs += 1;
                leaf_jitable_insts += stats.total_insts as u64;
            }
        }
    }

    println!("\n=== Instruction census (sorted by frequency) ===");
    let mut counts: Vec<(String, u64)> = inst_counts.into_iter().collect();
    counts.sort_by(|a, b| b.1.cmp(&a.1));
    for (name, count) in &counts {
        let pct = 100.0 * *count as f64 / total_insts as f64;
        println!("{name:>20}  {count:>10}  ({pct:.2}%)  [{}]", category(name));
    }

    let mut bigrams: Vec<((String, String), u64)> = bigram_counts.into_iter().collect();
    bigrams.sort_by(|a, b| b.1.cmp(&a.1));

    println!(
        "\n=== Instruction bigram census (all {} distinct pairs by frequency, {total_bigrams} total \
         adjacent pairs within a function body; pairs never cross a function boundary) ==="
        , bigrams.len()
    );
    for ((a, b), count) in bigrams.iter() {
        let pct = 100.0 * *count as f64 / total_bigrams as f64;
        println!("{a:>20} -> {b:<20}  {count:>10}  ({pct:.4}%)");
    }

    let mut post_p1_bigrams: Vec<((String, String), u64)> =
        post_p1_bigram_counts.into_iter().collect();
    post_p1_bigrams.sort_by(|a, b| b.1.cmp(&a.1));

    println!(
        "\n=== Simulated post-Priority-1 bigram census (ReportPosition filtered out of the \
         stream, direct neighbors joined; all {} distinct pairs, {total_post_p1_bigrams} total \
         adjacent pairs) ==="
        , post_p1_bigrams.len()
    );
    for ((a, b), count) in post_p1_bigrams.iter().take(30) {
        let pct = 100.0 * *count as f64 / total_post_p1_bigrams as f64;
        println!("{a:>20} -> {b:<20}  {count:>10}  ({pct:.4}%)");
    }

    println!("\n=== Category totals ((a) pure / (b) control_local+control_call / (c) var_access / (d) host) ===");
    let mut cats: Vec<(&str, u64)> = cat_counts.into_iter().collect();
    cats.sort_by(|a, b| b.1.cmp(&a.1));
    for (cat, count) in &cats {
        let pct = 100.0 * *count as f64 / total_insts as f64;
        println!("{cat:>14}  {count:>10}  ({pct:.2}%)");
    }

    println!("\n=== Function-level JIT-ability ===");
    println!("total function bodies:  {}", bodies.len());
    println!("empty bodies:           {empty_funcs}");
    let non_empty = bodies.len() as u64 - empty_funcs;
    println!(
        "\n[permissive] no host-call instruction (Call/Jump/CallEvent/Begin allowed as\n\
         a trampoline back into the interpreter/VM dispatch):"
    );
    println!(
        "  jit-able functions:    {jitable_funcs}  ({:.2}% of non-empty)",
        100.0 * jitable_funcs as f64 / non_empty as f64
    );
    println!(
        "  jit-able instructions: {jitable_insts}  ({:.2}% of all instructions)",
        100.0 * jitable_insts as f64 / total_insts as f64
    );
    println!(
        "\n[strict] leaf functions only — no host call AND no call/jump/begin\n\
         crossing into another function body (fully self-contained native code,\n\
         no runtime callback of any kind):"
    );
    println!(
        "  jit-able functions:    {leaf_jitable_funcs}  ({:.2}% of non-empty)",
        100.0 * leaf_jitable_funcs as f64 / non_empty as f64
    );
    println!(
        "  jit-able instructions: {leaf_jitable_insts}  ({:.2}% of all instructions)",
        100.0 * leaf_jitable_insts as f64 / total_insts as f64
    );

    println!("\nuptime: {}", std::process::Command::new("uptime")
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default());
}
