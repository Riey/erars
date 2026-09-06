//! `count`-operand distribution for `LoadVarRef`, and `StrKey`/interner
//! headroom, for the P2 `LoadStr`+`LoadVarRef` fusion feasibility check
//! (see `docs/research/2026-09-05-bytecode-dispatch-optimization.md`).
//!
//! `cargo run --release --features multithread -p erars-loader --example loadvarref_census -- <game dir>`
//!
//! Corpus loading mirrors `jit_census.rs`/`phases.rs` (same header-build +
//! parse_and_compile path) rather than re-deriving it.

use std::path::PathBuf;

use erars_ast::StrKey;
use erars_compiler::{Bump, HeaderInfo, ParserContext};
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
            require_literal_separator: false,
            require_literal_leading_dot: false,
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

fn main() {
    erars_ast::init_interner();
    let target = std::env::args().nth(1).expect("game path");

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

    let mut var = VariableStorage::new(header.clone(), &header.global_variables);
    let mut dic = FunctionDic::new();
    var.reserve_local_functions(funcs.len());
    for f in funcs {
        dic.insert_compiled_func(&mut var, &header.default_local_size, f);
    }

    let mut bodies: Vec<&erars_vm::FunctionBody> = Vec::new();
    for body in dic.normal.values() {
        bodies.push(body);
    }
    for coll in dic.event.values() {
        for b in coll.events.iter() {
            bodies.push(b);
        }
    }
    println!("{} function bodies (normal + event)", bodies.len());

    let mut count_hist: HashMap<u32, u64> = HashMap::new();
    let mut total_load_var_ref: u64 = 0;
    let mut total_insts: u64 = 0;
    let mut load_str_then_load_var_ref: u64 = 0;
    let mut load_var_ref_not_preceded_by_load_str: u64 = 0;

    for body in &bodies {
        let insts = body.body();
        total_insts += insts.len() as u64;
        for (i, &inst) in insts.iter().enumerate() {
            if let Some(count) = inst.as_load_var_ref() {
                total_load_var_ref += 1;
                *count_hist.entry(count).or_default() += 1;
                let preceded_by_load_str =
                    i > 0 && insts[i - 1].as_load_str().is_some();
                if preceded_by_load_str {
                    load_str_then_load_var_ref += 1;
                } else {
                    load_var_ref_not_preceded_by_load_str += 1;
                }
            }
        }
    }

    println!(
        "\n=== LoadVarRef: {total_load_var_ref} occurrences out of {total_insts} total static instructions ({:.2}%) ===",
        100.0 * total_load_var_ref as f64 / total_insts as f64
    );
    println!(
        "LoadVarRef immediately preceded by LoadStr: {load_str_then_load_var_ref} ({:.4}% of LoadVarRef)",
        100.0 * load_str_then_load_var_ref as f64 / total_load_var_ref as f64
    );
    println!(
        "LoadVarRef NOT immediately preceded by LoadStr: {load_var_ref_not_preceded_by_load_str}"
    );

    println!("\n=== `count` operand distribution ===");
    let mut hist: Vec<(u32, u64)> = count_hist.into_iter().collect();
    hist.sort_by_key(|(count, _)| *count);
    for (count, n) in &hist {
        let pct = 100.0 * *n as f64 / total_load_var_ref as f64;
        println!("count={count:>3}  {n:>10}  ({pct:.2}%)");
    }
    let max_count = hist.iter().map(|(c, _)| *c).max().unwrap_or(0);
    println!("max count observed: {max_count}");

    println!(
        "\n=== StrKey/interner headroom ===\ninterner len: {}\nliteral store len: {}",
        erars_ast::get_interner().len(),
        erars_ast::literal_store_len()
    );
}
