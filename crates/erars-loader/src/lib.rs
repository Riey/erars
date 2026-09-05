use anyhow::Context;
use erars_reader::read_file;
use parking_lot::Mutex;
#[cfg(feature = "multithread")]
use rayon::prelude::*;
use std::{
    fs::File,
    io::BufWriter,
    path::{Path, PathBuf},
    sync::Arc,
    time::Instant,
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        termcolor::{ColorChoice, StandardStream, WriteColor},
        Config,
    },
};
use erars_ast::{StrKey, VariableInfo};
use erars_compiler::{Bump, CompiledFunction, EraConfig, HeaderInfo, ParserContext};
use erars_lint::{check_function, ErarsFiles};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, SystemFunctions, TerminalVm, VmContext};
use hashbrown::HashMap;

pub fn save_script(vm: TerminalVm, ctx: VmContext, target_path: &str) -> anyhow::Result<()> {
    let target = resolve_game_path(target_path);
    let target_path = target.to_str().unwrap_or(target_path);
    let mut out = BufWriter::new(File::create(Path::new(target_path).join("game.era"))?);
    erars_bytecode::write_to(&mut out, &vm.dic)?;
    let local_infos: HashMap<StrKey, Vec<(StrKey, &VariableInfo)>> =
        ctx.var.local_infos().collect();
    rmp_serde::encode::write(&mut out, &(&*ctx.header_info, local_infos)).unwrap();

    Ok(())
}

/// Emuera's config is not one file but three, applied onto a single
/// `ConfigData` in a fixed order (`Config/ConfigData.cs:642-664`):
///
/// 1. `csv/_default.config` — defaults the *game* ships, meant to be overridden
/// 2. `emuera.config` — the user's own, next to the executable
/// 3. `csv/_fixed.config` — settings the game pins; loaded last, so it wins
///
/// Reading only the middle file is why erars pointed at a game that ships the
/// other two loaded its own defaults instead: eraMegaten pins
/// `_Rename.csvを利用する`, `_Replace.csvを利用する` and `サブディレクトリを検索する`
/// in `CSV/_fixed.config`, none of which is in the user file, and without them
/// the rename table is skipped and most of the tree is never read.
///
/// A file that does not exist is skipped silently, exactly as Emuera's
/// `loadConfig` returns `false` on a failed open (`:666-670`) — a game shipping
/// none of the three is not an error.
/// Resolve the root directory of an ERA game.
///
/// Games like eraMegaten keep their scripts and CSVs under a `Data/` subdirectory
/// rather than the repository root. If `target_path/ERB` does not exist but
/// `target_path/Data/ERB` does, this resolves to `target_path/Data`.
pub fn resolve_game_path<P: AsRef<Path>>(path: P) -> PathBuf {
    let p = path.as_ref();
    if !p.join("ERB").exists() && p.join("Data").join("ERB").exists() {
        p.join("Data")
    } else {
        p.to_path_buf()
    }
}

pub fn load_config(target_path: &str) -> EraConfig {
    let target = resolve_game_path(target_path);
    let target_path = target.to_str().unwrap_or(target_path);
    log::info!("Load config");

    let mut config = EraConfig::default();
    for name in ["CSV/_default.config", "emuera.config", "CSV/_fixed.config"] {
        // Emuera runs on a case-insensitive filesystem, and the rest of this
        // loader already matches `CSV/`, `ERB/` and `*.CSV` case-insensitively
        // (`:230-234`); resolving these three the same way keeps a game with a
        // lowercase `csv/` loading the same on Linux as on Windows.
        let Some(path) = first_match(target_path, name) else {
            continue;
        };
        match read_file(&path) {
            Ok(s) => {
                if let Err((err, _)) = config.merge_text(&s) {
                    log::error!("{}: {err}", path.display());
                }
            }
            Err(err) => log::error!("{}: config file load error: {err}", path.display()),
        }
    }

    log::info!("Config: {config:?}");

    config
}

/// The first case-insensitive match for `relative` under `target_path`, or
/// `None` when the file is absent.
fn first_match(target_path: &str, relative: &str) -> Option<PathBuf> {
    glob::glob_with(
        &format!("{target_path}/{relative}"),
        glob::MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: true,
            require_literal_separator: true,
        },
    )
    .ok()?
    .flatten()
    .find(|path| path.is_file())
}

/// Where save files live: `sav/` under the game root only when
/// `セーブデータをsavフォルダ内に作成する` is on, the game root itself otherwise —
/// Emuera picks between exactly those two (`Config/Config.cs:229-234`), and
/// its own default is off.
fn sav_path(target_path: &str, config: &EraConfig) -> PathBuf {
    let root = Path::new(target_path);
    if config.use_save_folder {
        root.join("sav")
    } else {
        root.to_owned()
    }
}

/// SAFETY: Any reference to interner is not exist
pub unsafe fn load_script(
    target_path: &str,
    system: Box<dyn SystemFunctions>,
    config: EraConfig,
) -> anyhow::Result<(TerminalVm, VmContext, VirtualConsole)> {
    let target = resolve_game_path(target_path);
    let target_path = target.to_str().unwrap_or(target_path);
    let start = Instant::now();

    log::info!("Load game script");

    let game_path = Path::new(target_path).join("game.era");
    let file = File::open(game_path).context("Open bytecode file")?;
    let file = memmap2::MmapOptions::new()
        .populate()
        .map(&file)
        .context("mmap bytecode file")?;
    let mut file_bytes = &*file;
    let dic = erars_bytecode::read_from(&mut file_bytes)?;

    log::info!("Load game data");
    let (mut header, local_infos): (HeaderInfo, HashMap<StrKey, Vec<(StrKey, VariableInfo)>>) =
        rmp_serde::decode::from_read(&mut file_bytes)?;
    header.init_macro_filter();
    let vconsole = VirtualConsole::new(&console_config(&config));

    let elapsed = start.elapsed();
    log::info!("Load done! {}ms elapsed", elapsed.as_millis());

    let sav = sav_path(target_path, &config);
    let mut ctx = VmContext::new(
        Arc::new(header),
        Arc::new(config),
        system,
        sav,
        Path::new(target_path).join("resources"),
    );

    ctx.var.reserve_local_functions(local_infos.len());
    for (key, vars) in local_infos {
        ctx.var.insert_local_table(key, vars);
    }

    Ok((
        TerminalVm {
            dic,
            header: ctx.header_info.clone(),
        },
        ctx,
        vconsole,
    ))
}

/// Loads a game and hands back a VM ready to run it.
///
/// `debug_mode` is Emuera's `-DEBUG` (`Program.cs:82-88`): fixed before any
/// script is read, it decides `[IF_DEBUG]`/`[IF_NDEBUG]`, the `;#;` marker and
/// whether the `DEBUGPRINT` family is compiled at all. That makes it part of
/// loading rather than of the running VM — and `--save` bakes the decision
/// into `game.era`.
#[allow(unused_assignments)]
pub fn run_script(
    target_path: &str,
    mut system: Box<dyn SystemFunctions>,
    config: EraConfig,
    error_to_stderr: bool,
    lint: bool,
    debug_mode: bool,
) -> anyhow::Result<(TerminalVm, VmContext, VirtualConsole)> {
    let target = resolve_game_path(target_path);
    let target_path = target.to_str().unwrap_or(target_path);
    erars_ast::init_interner();

    let interner = erars_ast::get_interner();

    let mut time = Instant::now();

    let config = Arc::new(config);
    let mut tx = VirtualConsole::new(&console_config(&config));

    // Load-progress paints happen before any ERB has executed, so no bitmap
    // can exist yet and `tx.images` is provably empty. This store is what
    // mints the `Painted` token those paints need; publishing it is a no-op,
    // and `VmContext` takes over with its own store the moment it exists
    // (`check_time!($work, @ctx)` below).
    let mut load_graphics = erars_vm::GraphicsStore::default();

    macro_rules! check_time {
        ($work:expr) => {
            check_time!($work, system);
        };

        ($work:expr, @ctx $ctx:expr) => {
            let m = time.elapsed().as_millis();
            time = Instant::now();

            log::info!("[{}]: {}ms", $work, m);
            tx.print_line(format!("[{}]: {}ms", $work, m));
            $ctx.redraw(&mut tx)?;
        };

        ($work:expr, $system:expr) => {
            let m = time.elapsed().as_millis();
            time = Instant::now();

            log::info!("[{}]: {}ms", $work, m);
            tx.print_line(format!("[{}]: {}ms", $work, m));
            let painted = load_graphics.publish(&tx.images);
            $system.redraw(&mut tx, painted)?;
        };
    }

    let mut function_dic = FunctionDic::new();
    // `イベント関数のCALLを許可する` decides registration, so it has to be set
    // before the first function lands (`GameProc/LabelDictionary.cs:83-84`).
    function_dic.compati_call_event = config.compati_call_event;
    let header_info;
    let mut ctx: VmContext;

    {
        check_time!("Initialize");

        let var_infos: HashMap<_, VariableInfo> =
            serde_yaml::from_str(include_str!("./variable.yaml"))?;

        // `サブディレクトリを検索する` — Emuera walks `ERB/` and `ERH/`
        // recursively only when this is on (`Config/Config.cs:403-405` feeding
        // `GameProc/ErbLoader.cs:45` and `GameProc/HeaderFileLoader.cs:39`),
        // and its own default is off.
        //
        // DELIBERATE: with the key on, erars also reads *non*-character CSVs
        // from subdirectories, where Emuera always reads `CSV/` top level only
        // (`GameProc/HeaderFileLoader.cs:381`) and recurses for `CHARA*.CSV`
        // alone (`GameData/ConstantData.cs:1236`). That is a superset: a game
        // keeping `ABL.CSV` in a subfolder would not run under Emuera at all.
        // With the key off both engines read `CSV/` top level only.
        let subdir = if config.search_subdirectory { "/**" } else { "" };
        let match_options = glob::MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: true,
            require_literal_separator: true,
        };

        let csvs = glob::glob_with(&format!("{target_path}/CSV{subdir}/*.CSV"), match_options)?;

        let erhs = glob::glob_with(&format!("{target_path}/ERB{subdir}/*.ERH"), match_options)?;

        // `glob` walks directories in a deterministic order, but feeding that iterator to
        // `par_bridge` returned the compiled functions in *completion* order, so which
        // duplicate definition won (and in which order event functions were registered)
        // depended on thread timing. Collect the paths and sort them instead, the way
        // Emuera loads ERB files in sorted filename order.
        //
        // The sort key is the lowercased path: the glob matches case-insensitively and
        // real scripts mix `.ERB` with `.erb`, so casing must not decide the load order.
        // `sort_by_cached_key` is stable and evaluates the key once per path, so paths
        // differing only in case keep glob's own (deterministic) relative order.
        let mut erbs =
            glob::glob_with(&format!("{target_path}/ERB{subdir}/*.ERB"), match_options)?
                .collect::<Result<Vec<PathBuf>, _>>()?;
        erbs.sort_by_cached_key(|erb| erb.to_string_lossy().to_lowercase());

        // A game root with no script at all is not a game root. Emuera refuses to start
        // in that case rather than opening an empty session (`Program.cs:132-137`,
        // message `erbフォルダが見つかりません` at `_Library/EvilMask/Lang.cs:113`);
        // erars used to compile zero functions, write an empty `game.era` and exit 0,
        // which reads as success. The glob is case-insensitive, so this covers a missing
        // `ERB` directory and an `erb` directory holding no scripts alike.
        if erbs.is_empty() {
            anyhow::bail!("No ERB script found in {target_path}/ERB");
        }

        #[cfg(feature = "multithread")]
        let csvs = csvs.par_bridge();

        let files = Mutex::new(ErarsFiles::new());
        let diagnostics = Mutex::new(Vec::new());

        macro_rules! report_error {
            ($code:expr, $msg:expr, $path:expr, $source:expr, $err:expr, $span:expr) => {
                diagnostics.lock().push(
                    Diagnostic::error()
                        .with_code($code)
                        .with_message($msg)
                        .with_labels(vec![Label::primary(files.lock().add(interner.get_or_intern($path.display().to_string()), $source), $span).with_message($err)]),
                );
            };
        }

        macro_rules! report_warning {
            ($code:expr, $msg:expr, $path:expr, $source:expr, $err:expr, $span:expr) => {
                diagnostics.lock().push(
                    Diagnostic::warning()
                        .with_code($code)
                        .with_message($msg)
                        .with_labels(vec![Label::primary(files.lock().add(interner.get_or_intern($path.display().to_string()), $source), $span).with_message($err)]),
                );
            };
        }

        let mut info = HeaderInfo {
            global_variables: var_infos,
            ..Default::default()
        };

        let mut csv_dic = csvs
            .filter_map(|csv| match csv {
                Ok(csv) => {
                    log::trace!("Load {}", csv.display());
                    let s = read_file(&csv).ok()?;

                    Some((
                        csv.file_stem().unwrap().to_str().unwrap().to_ascii_uppercase(),
                        (csv, s),
                    ))
                }
                Err(err) => {
                    log::error!("Failed to load csv file: {err}");
                    None
                }
            })
            .collect::<HashMap<_, _>>();

        let chara_csv_dic = csv_dic
            .extract_if(|k, _v| k.starts_with("CHARA"))
            .collect::<HashMap<_, _>>();

        check_time!("Load CSV");

        for (k, (path, v)) in csv_dic.into_iter() {
            log::debug!("Merge {k}.CSV");
            match k.as_str() {
                // Emuera's complete name-CSV set, `ConstantData.LoadData`
                // (`GameData/ConstantData.cs:634-666`). `CDFLAG1`/`CDFLAG2`
                // are `CDFLAG`'s two sub-index tables and keep their own
                // keys; `variable_arg` picks the right one per dimension.
                "ABL" | "MARK" | "BASE" | "CFLAG" | "EQUIP" | "TEQUIP" | "PALAM" | "EXP" | "EX"
                | "FLAG" | "TFLAG" | "TALENT" | "STAIN" | "SOURCE" | "TSTR" | "CSTR"
                | "SAVESTR" | "GLOBAL" | "GLOBALS" | "TRAIN" | "TCVAR" | "CDFLAG1" | "CDFLAG2"
                | "DAY" | "TIME" | "MONEY" => {
                    match info.merge_name_csv(&k, &v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse name csv", path, v, err, span);
                        }
                    }
                }
                "STRNAME" => match info.merge_name_csv("STR", &v) {
                    Ok(()) => {}
                    Err((err, span)) => {
                        report_error!("E0000", "Parse name csv", path, v, err, span);
                    }
                },
                "STR" => match info.merge_str_csv(&v) {
                    Ok(()) => {}
                    Err((err, span)) => {
                        report_error!("E0000", "Parse str csv", path, v, err, span);
                    }
                },
                "GAMEBASE" => {
                    match info.merge_gamebase_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse gamebase csv", path, v, err, span);
                        }
                    }

                    log::info!("GAMEBASE: {:?}", info.gamebase);
                }
                "VARIABLESIZE" => match info.merge_variable_size_csv(&v) {
                    Ok(()) => {}
                    Err((err, span)) => {
                        report_error!("E0000", "Parse variablesize csv", path, v, err, span);
                    }
                },
                // `_Rename.csvを利用する` / `_Replace.csvを利用する` — Emuera
                // skips the file outright when the key is off
                // (`GameProc/Process.cs:96`, `:119`).
                "_RENAME" if config.use_rename_file => {
                    log::debug!("Merge _RENAME.CSV");
                    match info.merge_rename_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse _rename csv", path, v, err, span);
                        }
                    }
                }
                "_REPLACE" if config.use_replace_file => {
                    log::debug!("Merge _REPLACE.CSV");
                    match info.merge_replace_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse _replace csv", path, v, err, span);
                        }
                    }
                    log::info!("Replace: {:?}", info.replace);
                }
                "_RENAME" | "_REPLACE" => {
                    log::debug!("Skip {k}.CSV: disabled by config");
                }
                "ITEM" => {
                    log::debug!("Merge ITEM.CSV");
                    match info.merge_item_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            report_error!("E0000", "Parse item csv", path, v, err, span);
                        }
                    }
                }
                other => {
                    log::warn!("Unknown csv name {other}");
                }
            }
        }

        check_time!("Merge CSV");

        for (k, (path, v)) in chara_csv_dic.into_iter() {
            log::debug!("Merge {k}.CSV");
            match info.merge_chara_csv(&v) {
                Ok(()) => {}
                Err((err, span)) => {
                    report_error!("E0000", "Parse character csv", path, v, err, span);
                }
            }
        }

        // `CALLNAMEが空文字列の時にNAMEを代入する` — Emuera fills an empty
        // `Callname` from `Name` once every character CSV has been read, on the
        // templates rather than at chara-add time
        // (`GameData/ConstantData.cs:1239-1244`).
        if config.compati_callname {
            for tmpl in info.character_templates.values_mut() {
                if tmpl.call_name.is_empty() {
                    tmpl.call_name = tmpl.name.clone();
                }
            }
        }

        check_time!("Merge chara CSV");

        tx.print_line(info.replace.start_message.clone());

        // Sort the header files for the same reason the ERB list is sorted
        // below: Emuera loads them in filename order (`Config.GetFiles` feeding
        // `GameProc/HeaderFileLoader.cs:39-59`, with
        // `読み込み順をファイル名順にソートする` on). Header loading is no longer
        // order-sensitive after the two passes below, but which `#DEFINE` wins a
        // redefinition, and the order of the diagnostics reported for
        // unresolvable `#DIM` lines, both still follow file order — so the input
        // has to be deterministic.
        let mut erhs = erhs.map(|erh| erh.unwrap()).collect::<Vec<PathBuf>>();
        erhs.sort_by_cached_key(|erh| erh.to_string_lossy().to_lowercase());

        // Pass 1: `#DEFINE` in file order, `#DIM`/`#DIMS` queued. Pass 2 below
        // resolves the queue as a whole, so a declaration may size itself with
        // a constant declared anywhere in any header file. Emuera splits header
        // loading exactly here (`GameProc/HeaderFileLoader.cs:123-131` queueing,
        // `:61-65` draining after every file has been read).
        let sources = erhs
            .iter()
            .map(|erh| read_file(erh).with_context(|| format!("Read {}", erh.display())))
            .collect::<anyhow::Result<Vec<String>>>()?;
        let mut pending = Vec::new();

        for (idx, (erh, source)) in erhs.iter().zip(sources.iter()).enumerate() {
            log::debug!("Parse {}", erh.display());

            match info.merge_header_defines(idx, source, &mut pending) {
                Ok(()) => (),
                Err((err, span)) => {
                    report_error!("E1000", "Parse erh", erh, source.clone(), err, span);
                }
            }
        }

        // Pass 2. Every line that never resolves comes back as its own
        // diagnostic naming the file and line it was written on, matching the
        // level-2 (fatal) warning Emuera reports for the remainder
        // (`GameProc/HeaderFileLoader.cs:349-353`).
        for (idx, (err, span)) in info.resolve_pending_dims(pending) {
            let erh = &erhs[idx];
            report_error!("E1001", "Declare erh variable", erh, sources[idx].clone(), err, span);
        }

        check_time!("Merge ERH");

        // log::trace!("Header: {info:#?}");

        header_info = Arc::new(info);

        // Dispatch the largest file first (longest-processing-time-first): compile time
        // tracks file length, so handing out the ~1.9MB outlier before the long tail of
        // small files keeps it from being the last straggler, and its peak working set
        // lands while the heap is still small. The second effect is the one that measures:
        // eraTHYMKR peak RSS, medians of 7 runs, is 253.2MB on 4 threads and 287.6MB on 8
        // against 267.4MB / 299.6MB when the same queue is fed in sorted order, while the
        // ERB phase itself is a wash (128.7ms vs 122.9ms on 4 threads, well inside the
        // run-to-run spread of this box). Each item also carries its index in the sorted
        // list, which is what restores sorted order for registration below; stat-ing a few
        // hundred files costs nothing next to reading and compiling them.
        let mut work = erbs
            .iter()
            .enumerate()
            .map(|(idx, erb)| {
                (
                    idx,
                    erb.as_path(),
                    std::fs::metadata(erb).map_or(0, |meta| meta.len()),
                )
            })
            .collect::<Vec<_>>();
        work.sort_by_key(|&(idx, _, len)| (std::cmp::Reverse(len), idx));

        let compile_one = |erb: &Path| -> Vec<CompiledFunction> {
            let source = read_file(erb).unwrap();
            let ctx = ParserContext::new(header_info.clone(), StrKey::new(erb.to_str().unwrap()))
                .with_debug(debug_mode);

            log::debug!("Parse And Compile {}", erb.display());

            let program = ctx.parse_and_compile(&mut ctx.preprocessor(&source), &mut Bump::new());

            // One diagnostic per line that failed, and the file's other
            // functions still register — Emuera never discards a whole ERB
            // over one bad line (`GameProc/ErbLoader.cs:403-407`).
            match program {
                Ok(erb_out) => {
                    for (err, span) in erb_out.errors {
                        report_error!("E2000", "Parse erb", erb, source.clone(), err, span);
                    }
                    for (err, span, level) in erb_out.warnings {
                        // `表示する最低警告レベル` — Emuera drops a warning
                        // below the configured level before it reaches the
                        // console (`GameData/ParserMediator.cs:26`).
                        if level < config.display_warning_level {
                            continue;
                        }
                        report_warning!("W2000", "Parse erb", erb, source.clone(), err, span);
                    }
                    erb_out.functions
                }
                Err((err, span)) => {
                    report_error!("E2000", "Parse erb", erb, source, err, span);
                    Vec::new()
                }
            }
        };

        // `par_bridge` hands the items out one at a time from a shared queue, so the
        // largest-first order above becomes longest-processing-time-first scheduling.
        // `par_iter` would split the sorted vec into contiguous index ranges instead,
        // undoing that: same ERB phase wall time, but 265.8MB peak RSS on 4 threads and
        // 305.4MB on 8 against 257.7MB / 287.7MB for the shape below.
        //
        // Tag every function with its file's index and collect one flat vec.
        // `parse_and_compile` returns a `Vec<CompiledFunction>` that starts at
        // `with_capacity(1024)` while the average ERB defines ~20 functions (16859 over
        // 857 files), so collecting those per-file vecs and registering out of them kept
        // 857 buffers alive to the end of the phase: 93.4MB of capacity for 1.8MB of
        // functions, +102MB peak RSS at 1 and at 4 threads. Draining each vec inside the
        // worker hands the buffer straight back to the thread that allocated it.
        //
        // A stable sort by the tag is enough to restore sorted order: one file is compiled
        // by exactly one worker, `flat_map_iter` finishes a file before it starts the next,
        // and rayon's collect concatenates the per-worker vecs rather than interleaving
        // them, so one file's functions are always contiguous and in source order.
        #[cfg(feature = "multithread")]
        let mut funcs = work
            .into_iter()
            .par_bridge()
            .flat_map_iter(|(idx, erb, _)| compile_one(erb).into_iter().map(move |f| (idx, f)))
            .collect::<Vec<(usize, CompiledFunction)>>();
        #[cfg(not(feature = "multithread"))]
        let mut funcs = work
            .into_iter()
            .flat_map(|(idx, erb, _)| compile_one(erb).into_iter().map(move |f| (idx, f)))
            .collect::<Vec<(usize, CompiledFunction)>>();

        funcs.sort_by_key(|&(idx, _)| idx);

        let sav = sav_path(target_path, &config);
        ctx = VmContext::new(
            header_info.clone(),
            config,
            system,
            sav,
            Path::new(target_path).join("resources"),
        );

        // `Program.Main` loads `resources/` before anything runs
        // (`Program.cs:63`, `AppContents.LoadContents`). Every failure in
        // there is a `ParserMediator.Warn`, never an error: a game with a
        // broken sprite CSV still starts, with those sprites missing.
        for warning in erars_vm::resources::load(
            &mut ctx.graphics,
            &ctx.content_dir,
            ctx.config.lang.encoding(),
        ) {
            if error_to_stderr {
                eprintln!("{warning}");
            }
            log::warn!("{warning}");
        }

        ctx.var.reserve_local_functions(funcs.len());
        for (_, func) in funcs {
            function_dic.insert_compiled_func(
                &mut ctx.var,
                &ctx.header_info.default_local_size,
                func,
            );
        }

        check_time!("Parse/Compile ERB", @ctx ctx);

        let mut diagnostics = diagnostics.into_inner();
        let mut files = files.into_inner();

        if lint {
            diagnostics.extend(check_function(&function_dic, &ctx.var, &mut files));
            check_time!("Check codes", @ctx ctx);
        }

        if !diagnostics.is_empty() {
            /// How many diagnostics are printed before the rest are summarised.
            const LIMIT: usize = 40;

            let config = Config::default();
            let writer = StandardStream::stderr(ColorChoice::Always);
            let mut writer = writer.lock();
            for diagnostic in diagnostics.iter().take(LIMIT) {
                if error_to_stderr {
                    codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic)
                        .unwrap();
                }
                let mut writer = LogWriter::default();
                codespan_reporting::term::emit(&mut writer, &config, &files, &diagnostic).unwrap();
                std::io::Write::flush(&mut writer).unwrap();
            }

            if let Some(left) = diagnostics.len().checked_sub(LIMIT).filter(|n| *n > 0) {
                if error_to_stderr {
                    eprintln!("And {left} more errors...");
                    log::error!("And {left} more errors...");
                }
            }
        }

        check_time!("Report errors", @ctx ctx);
    }

    let vm = TerminalVm::new(function_dic, ctx.header_info.clone());

    Ok((vm, ctx, tx))
}

#[derive(Default)]
struct LogWriter(String);

impl std::io::Write for LogWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let s = std::str::from_utf8(buf).unwrap();
        self.0.push_str(s);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        for line in self.0.lines() {
            log::error!("{line}");
        }
        self.0.clear();
        Ok(())
    }
}

impl WriteColor for LogWriter {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(
        &mut self,
        _spec: &codespan_reporting::term::termcolor::ColorSpec,
    ) -> std::io::Result<()> {
        Ok(())
    }

    fn reset(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::load_config;

    /// Emuera applies `csv/_default.config`, then the user's `emuera.config`,
    /// then `csv/_fixed.config` onto one `ConfigData`
    /// (`Config/ConfigData.cs:642-664`), so the game's *fixed* file wins over
    /// the user's and the user's wins over the game's defaults. Each of the
    /// three keys here is set by exactly one file, plus one key all three set,
    /// so a wrong order shows up as a wrong value rather than as a missing one.
    #[test]
    fn config_cascade_precedence() {
        let root = std::env::temp_dir().join("erars-loader-config-cascade");
        let csv = root.join("CSV");
        std::fs::create_dir_all(&csv).unwrap();

        // `PRINTCの文字数` is set by all three, `履歴ログの行数` only by the
        // default file, `ウィンドウ幅` only by the user file and
        // `サブディレクトリを検索する` only by the fixed file.
        std::fs::write(
            csv.join("_default.config"),
            "\u{feff}PRINTCの文字数:11\r\n履歴ログの行数:1234\r\n",
        )
        .unwrap();
        std::fs::write(
            root.join("emuera.config"),
            "\u{feff}PRINTCの文字数:22\r\nウィンドウ幅:1512\r\n",
        )
        .unwrap();
        std::fs::write(
            csv.join("_fixed.config"),
            "\u{feff}PRINTCの文字数:33\r\nサブディレクトリを検索する:YES\r\n",
        )
        .unwrap();

        let config = load_config(root.to_str().unwrap());

        assert_eq!(config.printc_width, 33, "_fixed.config must win");
        assert_eq!(config.max_log, 1234, "_default.config must still apply");
        assert_eq!(config.window_width, 1512, "emuera.config must still apply");
        assert!(config.search_subdirectory, "_fixed.config-only key must apply");

        // A game shipping none of the three is not an error: every key keeps its
        // built-in default, the way Emuera's `loadConfig` just returns `false`
        // on a failed open (`Config/ConfigData.cs:666-670`).
        let empty = root.join("empty");
        std::fs::create_dir_all(&empty).unwrap();
        let config = load_config(empty.to_str().unwrap());
        assert_eq!(config.printc_width, erars_compiler::EraConfig::default().printc_width);

        std::fs::remove_dir_all(&root).ok();
    }
}
