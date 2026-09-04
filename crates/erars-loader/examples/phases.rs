//! Phase timing harness for the ERB load pipeline.
//!
//! `cargo run --release -p erars-loader --features multithread --example phases -- <game dir> [rounds]`
//!
//! Reports serial read / lex / parse+compile / insert / bytecode-write and the
//! rayon parallel parse+compile (honours `RAYON_NUM_THREADS`). Serial numbers are
//! the low-core-count (phone) proxy; each phase runs `rounds` times and the
//! minimum is reported alongside the median.

use std::{path::PathBuf, sync::Arc, time::Instant};

use erars_ast::{StrKey, VariableInfo};
use erars_compiler::{Bump, HeaderInfo, ParserContext};
use erars_reader::read_file;
use erars_vm::{FunctionDic, VariableStorage};
use hashbrown::HashMap;
use rayon::prelude::*;

// Match the shipped binaries (`erars-stdio`, `erars-renderer`), which both
// install mimalloc; allocator choice moves these numbers a lot.
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
    // Same order the loader uses, so the same declarations resolve in the same
    // pass and the timing is comparable.
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

/// Process CPU time (user+sys). Load-insensitive, unlike wall time; this box
/// runs other work, so CPU time is the metric to compare across builds and the
/// best proxy for a phone's single-core budget.
fn cpu_ms() -> f64 {
    let mut usage = std::mem::MaybeUninit::<libc::rusage>::uninit();
    // SAFETY: getrusage fills the whole struct for RUSAGE_SELF.
    let usage = unsafe {
        assert_eq!(libc::getrusage(libc::RUSAGE_SELF, usage.as_mut_ptr()), 0);
        usage.assume_init()
    };
    let secs = |t: libc::timeval| t.tv_sec as f64 * 1000.0 + t.tv_usec as f64 / 1000.0;
    secs(usage.ru_utime) + secs(usage.ru_stime)
}

fn bench<T>(name: &str, rounds: usize, mut body: impl FnMut() -> T) -> T {
    let mut times = Vec::with_capacity(rounds);
    let mut cpus = Vec::with_capacity(rounds);
    let mut last = None;
    for _ in 0..rounds {
        let c = cpu_ms();
        let t = Instant::now();
        let r = body();
        times.push(t.elapsed().as_secs_f64() * 1000.0);
        cpus.push(cpu_ms() - c);
        last = Some(r);
    }
    times.sort_by(|a, b| a.partial_cmp(b).unwrap());
    cpus.sort_by(|a, b| a.partial_cmp(b).unwrap());
    println!(
        "{name:<32} wall min {:>8.1} med {:>8.1} | cpu min {:>8.1} med {:>8.1} ms",
        times[0],
        times[times.len() / 2],
        cpus[0],
        cpus[cpus.len() / 2],
    );
    last.unwrap()
}

/// Self-time profile of `body`, printed as a leaf-frame histogram.
///
/// In-process SIGPROF sampling; `perf`/`samply` need `perf_event_paranoid <= 1`
/// which this box does not grant.
fn profile<T>(body: impl FnOnce() -> T) -> T {
    let guard = pprof::ProfilerGuardBuilder::default()
        .frequency(1997)
        .build()
        .unwrap();
    let r = body();
    let report = guard.report().build().unwrap();

    let mut total = 0isize;
    let mut leaves: HashMap<String, isize> = HashMap::new();
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
    println!("-- self time, {total} samples --");
    for (name, count) in rows.iter().take(30) {
        println!("{:>6.2}%  {}", *count as f64 / total as f64 * 100.0, name);
    }
    r
}

fn main() {
    erars_ast::init_interner();
    let target = std::env::args().nth(1).expect("game path");
    let rounds: usize = std::env::args().nth(2).map(|s| s.parse().unwrap()).unwrap_or(3);
    // `PHASES_DEBUG=1` parses as `--debug` does, so the interner and function
    // counts of the two modes can be compared: the debug-print family is
    // elided to `Stmt::Nop` without its arguments being parsed when it is off
    // (`GameProc/Process.ScriptProc.cs:33-40`).
    let debug_mode = std::env::var_os("PHASES_DEBUG").is_some();

    let header = Arc::new(build_header(&target));
    let paths = erb_paths(&target);

    let sources: Vec<String> = bench("read_file serial", rounds, || {
        paths.iter().map(|p| read_file(p).unwrap()).collect()
    });
    let bytes: usize = sources.iter().map(|s| s.len()).sum();
    println!("{} ERB files, {} bytes", paths.len(), bytes);

    bench("preprocess+lex serial", rounds, || {
        let mut n = 0usize;
        for (p, s) in paths.iter().zip(sources.iter()) {
            let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()))
                .with_debug(debug_mode);
            let mut pp = ctx.preprocessor(s.as_str());
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
    });

    let run_serial = |rounds| {
        bench("parse+compile serial", rounds, || {
            let mut out = Vec::new();
            let mut b = Bump::new();
            for (p, s) in paths.iter().zip(sources.iter()) {
                let ctx = ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()))
                    .with_debug(debug_mode);
                let mut pp = ctx.preprocessor(s.as_str());
                b.reset();
                match ctx.parse_and_compile(&mut pp, &mut b) {
                    Ok(erb) => {
                        out.extend(erb.functions);
                        for (err, _) in erb.errors {
                            eprintln!("compile error {}: {err}", p.display());
                        }
                    }
                    Err(e) => eprintln!("compile error {}: {}", p.display(), e.0),
                }
            }
            out
        })
    };

    let funcs = if std::env::var_os("PHASES_PROFILE").is_some() {
        profile(|| run_serial(rounds))
    } else {
        run_serial(rounds)
    };
    println!("{} functions", funcs.len());

    if std::env::var_os("PHASES_SERIAL_ONLY").is_some() {
        return;
    }

    let threads = rayon::current_num_threads();
    bench(
        &format!("parse+compile par ({threads}t)"),
        rounds,
        || -> usize {
            let v = paths
                .par_iter()
                .zip(sources.par_iter())
                .flat_map_iter(|(p, s)| {
                    let ctx =
                        ParserContext::new(header.clone(), StrKey::new(p.to_str().unwrap()))
                            .with_debug(debug_mode);
                    let mut pp = ctx.preprocessor(s.as_str());
                    let mut b = Bump::new();
                    ctx.parse_and_compile(&mut pp, &mut b)
                        .map_or_else(|_| Vec::new(), |erb| erb.functions)
                })
                .collect::<Vec<_>>();
            std::hint::black_box(v.len())
        },
    );

    let mut var = VariableStorage::new(header.clone(), &header.global_variables);
    let mut dic = FunctionDic::new();
    bench("insert_compiled_func serial", 1, || {
        for f in funcs.clone() {
            dic.insert_compiled_func(&mut var, &header.default_local_size, f);
        }
    });

    bench("bytecode write_to (Vec)", rounds, || {
        let mut out = Vec::with_capacity(64 * 1024 * 1024);
        erars_bytecode::write_to(&mut out, &dic).unwrap();
        std::hint::black_box(out.len())
    });

    println!("interner len {}", erars_ast::get_interner().len());
}
