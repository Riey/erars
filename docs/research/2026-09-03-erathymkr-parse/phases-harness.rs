//! TEMPORARY measurement harness (opus research task). Delete after use.
//!
//! Isolates the ERB load phases: file read, preprocess/lex only, parse to AST,
//! parse+compile, and the sequential `insert_compiled_func` pass.

use std::{path::PathBuf, sync::Arc, time::Instant};

use erars_ast::{StrKey, VariableInfo};
use erars_compiler::{Bump, HeaderInfo, ParserContext, Preprocessor, PP_REGEX};
use erars_reader::read_file;
use erars_vm::{FunctionDic, VariableStorage};
use hashbrown::HashMap;
use rayon::prelude::*;

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
    let var_infos: HashMap<StrKey, VariableInfo> =
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

    let erhs = glob::glob_with(
        &format!("{target}/ERB/**/*.ERH"),
        glob::MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: true,
            require_literal_separator: true,
        },
    )
    .unwrap();
    for erh in erhs {
        let erh = erh.unwrap();
        let source = read_file(&erh).unwrap();
        info.merge_header(&source).unwrap();
    }

    info
}

macro_rules! phase {
    ($name:expr, $body:block) => {{
        let t = Instant::now();
        let r = $body;
        println!("{:<34} {:>8.1} ms", $name, t.elapsed().as_secs_f64() * 1000.0);
        r
    }};
}

fn main() {
    erars_ast::init_interner();
    let target = std::env::args().nth(1).expect("game path");
    let mode = std::env::args().nth(2).unwrap_or_else(|| "all".into());

    let header = Arc::new(build_header(&target));
    let paths = erb_paths(&target);
    println!("{} ERB files", paths.len());

    // Warm the page cache and measure serial read.
    let sources: Vec<String> = phase!("read_file serial", {
        paths.iter().map(|p| read_file(p).unwrap()).collect()
    });
    let total: usize = sources.iter().map(|s| s.len()).sum();
    let lines: usize = sources.iter().map(|s| s.lines().count()).sum();
    println!("{} bytes, {} lines", total, lines);

    phase!("read_file parallel", {
        let v: Vec<String> = paths.par_iter().map(|p| read_file(p).unwrap()).collect();
        std::hint::black_box(v.len())
    });

    // Lex only: drive the preprocessor to exhaustion, discard the lines.
    let lex = |sources: &Vec<String>| {
        let mut n = 0usize;
        for (p, s) in paths.iter().zip(sources.iter()) {
            let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
            let mut pp = Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, s.as_str());
            let mut b = Bump::new();
            loop {
                match pp.next_line(&b) {
                    Ok(Some(_)) => n += 1,
                    Ok(None) if pp.left_text().is_empty() => break,
                    Ok(None) => {}
                    Err(e) => {
                        eprintln!("lex error {}: {:?}", p.display(), e.0);
                        break;
                    }
                }
                b.reset();
            }
        }
        n
    };

    let nlines = phase!("preprocess+lex serial", { lex(&sources) });
    println!("{} logical lines", nlines);

    phase!("preprocess+lex parallel", {
        let n: usize = paths
            .par_iter()
            .zip(sources.par_iter())
            .map(|(p, s)| {
                let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
                let mut pp = Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, s.as_str());
                let mut b = Bump::new();
                let mut n = 0;
                loop {
                    match pp.next_line(&b) {
                        Ok(Some(_)) => n += 1,
                        Ok(None) if pp.left_text().is_empty() => break,
                        Ok(None) => {}
                        Err(_) => break,
                    }
                    b.reset();
                }
                n
            })
            .sum();
        n
    });

    if mode == "all" {
        phase!("parse to AST serial", {
            let mut n = 0usize;
            for (p, s) in paths.iter().zip(sources.iter()) {
                let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
                let mut pp = Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, s.as_str());
                let mut b = Bump::new();
                match ctx.parse(&mut pp, &mut b) {
                    Ok(f) => n += f.len(),
                    Err(e) => eprintln!("parse error {}: {}", p.display(), e.0),
                }
            }
            n
        });
    }

    let funcs = phase!("parse+compile serial", {
        let mut out = Vec::new();
        for (p, s) in paths.iter().zip(sources.iter()) {
            let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
            let mut pp = Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, s.as_str());
            let mut b = Bump::new();
            match ctx.parse_and_compile(&mut pp, &mut b) {
                Ok(f) => out.extend(f),
                Err(e) => eprintln!("compile error {}: {}", p.display(), e.0),
            }
        }
        out
    });
    println!("{} functions", funcs.len());

    let par_pc = || {
        paths
            .par_iter()
            .zip(sources.par_iter())
            .flat_map_iter(|(p, s)| {
                let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
                let mut pp = Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, s.as_str());
                let mut b = Bump::new();
                ctx.parse_and_compile(&mut pp, &mut b).unwrap_or_default()
            })
            .collect::<Vec<_>>()
    };
    let funcs2 = phase!("parse+compile parallel #1", { par_pc() });
    drop(funcs2);
    let funcs2 = phase!("parse+compile parallel #2 (warm interner)", { par_pc() });
    drop(funcs2);
    let funcs2 = phase!("parse+compile parallel #3 (warm interner)", { par_pc() });

    phase!("parse+compile serial #2 (warm interner)", {
        let mut n = 0;
        for (p, s) in paths.iter().zip(sources.iter()) {
            let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
            let mut pp = Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, s.as_str());
            let mut b = Bump::new();
            n += ctx.parse_and_compile(&mut pp, &mut b).unwrap_or_default().len();
        }
        n
    });

    phase!("parse to AST serial (warm interner)", {
        let mut n = 0usize;
        for (p, s) in paths.iter().zip(sources.iter()) {
            let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
            let mut pp = Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, s.as_str());
            let mut b = Bump::new();
            n += ctx.parse(&mut pp, &mut b).map(|f| f.len()).unwrap_or(0);
        }
        n
    });

    // read + parse + compile fused, exactly like run_script's par_bridge shape.
    phase!("read+parse+compile par_bridge", {
        let it = glob::glob_with(
            &format!("{target}/ERB/**/*.ERB"),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )
        .unwrap();
        let v: Vec<_> = it
            .par_bridge()
            .flat_map(|erb| {
                let erb = erb.unwrap();
                let source = read_file(&erb).unwrap();
                let ctx = ParserContext::new(header.clone(), StrKey::new(erb.to_str().unwrap()));
                ctx.parse_and_compile(
                    &mut Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, source.as_str()),
                    &mut Bump::new(),
                )
                .unwrap_or_default()
            })
            .collect();
        std::hint::black_box(v.len())
    });

    drop(funcs2);

    let mut var = VariableStorage::new(header.clone(), &header.global_variables);
    let mut dic = FunctionDic::new();
    phase!("insert_compiled_func serial", {
        for f in funcs {
            dic.insert_compiled_func(&mut var, &header.default_local_size, f);
        }
    });

    phase!("bytecode write_to (Vec)", {
        let mut out = Vec::with_capacity(64 * 1024 * 1024);
        erars_bytecode::write_to(&mut out, &dic).unwrap();
        println!("  bytecode {} bytes", out.len());
        std::hint::black_box(out.len())
    });

    println!("interner len {}", erars_ast::get_interner().len());

    // Order determinism of the run_script-shaped par_bridge pipeline.
    if mode == "order" {
        let collect_once = || -> Vec<(u32, u32)> {
            let it = glob::glob_with(
                &format!("{target}/ERB/**/*.ERB"),
                glob::MatchOptions {
                    case_sensitive: false,
                    require_literal_leading_dot: true,
                    require_literal_separator: true,
                },
            )
            .unwrap();
            it.par_bridge()
                .flat_map(|erb| {
                    let erb = erb.unwrap();
                    let source = read_file(&erb).unwrap();
                    let ctx =
                        ParserContext::new(header.clone(), StrKey::new(erb.to_str().unwrap()));
                    ctx.parse_and_compile(
                        &mut Preprocessor::new(
                            &PP_REGEX,
                            &ctx.header.as_ref().rename,
                            source.as_str(),
                        ),
                        &mut Bump::new(),
                    )
                    .unwrap_or_default()
                })
                .map(|f| (f.header.file_path.to_u32(), f.header.name.to_u32()))
                .collect::<Vec<_>>()
        };

        let a = collect_once();
        let b2 = collect_once();
        println!("order: len a={} b={}", a.len(), b2.len());
        let same = a == b2;
        println!("order: identical={}", same);
        if !same {
            let first = a.iter().zip(b2.iter()).position(|(x, y)| x != y);
            let diff = a.iter().zip(b2.iter()).filter(|(x, y)| x != y).count();
            println!("order: first divergence at {:?}, {} of {} positions differ", first, diff, a.len());
        }

        // Duplicate function names across the whole game.
        let mut seen: HashMap<u32, usize> = HashMap::new();
        for (_f, n) in a.iter() {
            *seen.entry(*n).or_insert(0) += 1;
        }
        let dups: Vec<_> = seen.iter().filter(|(_, c)| **c > 1).collect();
        let mut ev = 0usize;
        let mut evdup = 0usize;
        for (k, c) in seen.iter() {
            let name = StrKey::from_u32(*k).resolve();
            if name.parse::<erars_ast::EventType>().is_ok() {
                ev += *c;
                if *c > 1 {
                    evdup += 1;
                }
            }
        }
        println!("order: {} distinct names, {} duplicated names, {} event-fn instances ({} event names duplicated)", seen.len(), dups.len(), ev, evdup);

        // permutation check: same multiset, different sequence?
        let mut sa = a.clone();
        let mut sb = b2.clone();
        sa.sort();
        sb.sort();
        println!("order: same multiset after sort = {}", sa == sb);
        let uniq_a: std::collections::BTreeSet<_> = a.iter().collect();
        println!("order: distinct (file,name) pairs = {}", uniq_a.len());

        // full event-instance census, including singletons
        let mut evcount: Vec<(String, usize)> = Vec::new();
        for (k, c) in seen.iter() {
            let name = StrKey::from_u32(*k).resolve();
            if name.parse::<erars_ast::EventType>().is_ok() {
                evcount.push((name.to_string(), *c));
            }
        }
        evcount.sort();
        let evtotal: usize = evcount.iter().map(|(_, c)| *c).sum();
        println!("order: event census ({} names, {} instances):", evcount.len(), evtotal);
        for (n, c) in evcount.iter() {
            println!("    EVENT {n} x{c}");
        }

        // duplicated non-event names with the files that define them
        println!("order: duplicated non-event definitions (name -> files):");
        let mut byname: HashMap<u32, Vec<u32>> = HashMap::new();
        for (f, n) in a.iter() {
            byname.entry(*n).or_default().push(*f);
        }
        let mut rows: Vec<(String, Vec<String>)> = byname
            .iter()
            .filter(|(n, fs)| {
                fs.len() > 1 && StrKey::from_u32(**n).resolve().parse::<erars_ast::EventType>().is_err()
            })
            .map(|(n, fs)| {
                (
                    StrKey::from_u32(*n).resolve().to_string(),
                    fs.iter().map(|f| StrKey::from_u32(*f).resolve().to_string()).collect(),
                )
            })
            .collect();
        rows.sort();
        for (n, fs) in rows.iter() {
            println!("    {} x{}", n, fs.len());
            for f in fs {
                println!("        {}", f);
            }
        }
        println!("order: FINAL duplicated non-event name count = {}", rows.len());
        let mut names: Vec<String> = dups
            .iter()
            .filter(|(k, _)| !StrKey::from_u32(**k).resolve().parse::<erars_ast::EventType>().is_ok())
            .map(|(k, c)| format!("{} x{}", StrKey::from_u32(**k).resolve(), c))
            .collect();
        names.sort();
        println!("order: duplicated NON-event functions ({}):", names.len());
        for n in names.iter().take(40) {
            println!("    {n}");
        }
    }

    // Per-file parse+compile cost, largest first.
    if mode == "perfile" {
        let mut rows: Vec<(f64, usize, String)> = paths
            .par_iter()
            .zip(sources.par_iter())
            .map(|(p, s)| {
                let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()));
                let t = Instant::now();
                let mut pp = Preprocessor::new(&PP_REGEX, &ctx.header.as_ref().rename, s.as_str());
                let mut b = Bump::new();
                let _ = ctx.parse_and_compile(&mut pp, &mut b);
                (
                    t.elapsed().as_secs_f64() * 1000.0,
                    s.len(),
                    p.display().to_string(),
                )
            })
            .collect();
        rows.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap());
        println!("\n-- slowest 15 files (parse+compile, ms / bytes) --");
        for (ms, bytes, name) in rows.iter().take(15) {
            println!("{ms:>8.2} {bytes:>9} {name}");
        }
        let sum: f64 = rows.iter().map(|r| r.0).sum();
        let top: f64 = rows.iter().take(1).map(|r| r.0).sum();
        println!("sum {sum:.1} ms, slowest single file {top:.1} ms (critical path floor)");
    }
}
