use parking_lot::Mutex;
#[cfg(feature = "multithread")]
use rayon::prelude::*;
use std::{
    fs::File,
    io::{BufRead, BufReader, BufWriter, Read, Seek, SeekFrom},
    path::Path,
    sync::Arc,
    time::Instant,
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use erars_ast::{StrKey, Value, VariableInfo};
use erars_compiler::{CompiledFunction, EraConfig, HeaderInfo, Lexer, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{FunctionDic, TerminalVm, VmContext};
use hashbrown::HashMap;

/// Read string from path respect BOM
fn read_file(path: &Path) -> std::io::Result<String> {
    let mut file = std::fs::File::open(path)?;

    use unicode_bom::Bom;

    match Bom::from(&mut file) {
        Bom::Utf8 => {
            file.seek(SeekFrom::Start(Bom::Utf8.len() as _))?;
            let mut out = String::with_capacity(file.metadata()?.len() as usize);
            file.read_to_string(&mut out)?;
            Ok(out)
        }
        other => {
            file.seek(SeekFrom::Start(other.len() as _))?;
            let encoding = match other {
                Bom::Null => encoding_rs::SHIFT_JIS,
                Bom::Utf16Be => encoding_rs::UTF_16BE,
                Bom::Utf16Le => encoding_rs::UTF_16LE,
                Bom::Utf8 => unreachable!(),
                other => panic!("Unsupported BOM {other}"),
            };

            let mut decoder = encoding.new_decoder();

            let len = decoder
                .max_utf8_buffer_length(file.metadata()?.len() as usize)
                .expect("File is too large");

            let mut reader = BufReader::new(file);
            let mut out = String::with_capacity(len);

            loop {
                let src = reader.fill_buf()?;

                if src.is_empty() {
                    break;
                }

                let (_ret, size, had_errors) = decoder.decode_to_string(src, &mut out, false);

                if had_errors {
                    log::error!("Invalid text detected from {}", path.display());
                }

                reader.consume(size);
            }

            Ok(out)
        }
    }
}

pub fn save_script(vm: TerminalVm, ctx: VmContext) -> anyhow::Result<()> {
    let mut out = BufWriter::new(File::create("game.era")?);
    erars_bytecode::write_to(&mut out, &vm.dic)?;
    let local_infos: HashMap<StrKey, Vec<(StrKey, &VariableInfo)>> =
        ctx.var.local_infos().collect();
    rmp_serde::encode::write(&mut out, &(&*ctx.header_info, local_infos)).unwrap();

    Ok(())
}

fn make_fs_manager(target_path: &str) -> Box<dyn erars_vm::SaveLoadManager> {
    Box::new(erars_saveload_fs::FsSaveManager::new(
        Path::new(&target_path).join("sav"),
    ))
}

/// SAFETY: Any reference to interner is not exist
pub unsafe fn load_script(
    target_path: String,
    inputs: Vec<Value>,
) -> anyhow::Result<(TerminalVm, VmContext, VirtualConsole)> {
    let start = Instant::now();

    log::info!("Load config");
    let config_path = Path::new(target_path.as_str()).join("emuera.config");

    let config = if Path::new(&config_path).exists() {
        match read_file(config_path.as_ref()) {
            Ok(s) => EraConfig::from_text(&s).unwrap(),
            Err(err) => {
                log::error!("config file load error: {err}");
                EraConfig::default()
            }
        }
    } else {
        EraConfig::default()
    };

    log::info!("Config: {config:?}");

    log::info!("Load game script");
    let mut read = BufReader::new(File::open("game.era").unwrap());
    let dic = erars_bytecode::read_from(&mut read)?;

    log::info!("Load game data");
    let (header, local_infos): (HeaderInfo, HashMap<StrKey, Vec<(StrKey, VariableInfo)>>) =
        rmp_serde::decode::from_read(&mut read)?;
    let mut vconsole = VirtualConsole::new(config.printc_width);

    let elapsed = start.elapsed();
    log::info!("Load done! {}ms elapsed", elapsed.as_millis());

    let mut ctx = VmContext::new(
        Arc::new(header),
        Arc::new(config),
        make_fs_manager(&target_path),
    );

    for (key, vars) in local_infos {
        for var in vars {
            ctx.var.add_local_info(key, var.0, var.1);
        }
    }

    for input in inputs {
        vconsole.push_input(input);
    }

    Ok((TerminalVm { dic }, ctx, vconsole))
}

#[allow(unused_assignments)]
pub fn run_script(
    target_path: String,
    inputs: Vec<Value>,
) -> anyhow::Result<(TerminalVm, VmContext, VirtualConsole)> {
    erars_ast::init_interner();

    let mut time = Instant::now();

    let config_path = Path::new(target_path.as_str()).join("emuera.config");

    let config = if Path::new(&config_path).exists() {
        match read_file(config_path.as_ref()) {
            Ok(s) => EraConfig::from_text(&s).unwrap(),
            Err(err) => {
                log::error!("config file load error: {err}");
                EraConfig::default()
            }
        }
    } else {
        EraConfig::default()
    };

    log::trace!("Config: {config:?}");

    let config = Arc::new(config);
    let mut tx = VirtualConsole::new(config.printc_width);

    macro_rules! check_time {
        ($work:expr) => {
            let m = time.elapsed().as_millis();
            time = Instant::now();

            tx.print_line(format!("[{}]: {}ms", $work, m));
            log::info!("[{}]: {}ms", $work, m);
        };
    }

    let mut function_dic = FunctionDic::new();
    let header_info;
    let mut ctx: VmContext;

    {
        check_time!("Initialize");

        let var_infos: HashMap<_, VariableInfo> =
            serde_yaml::from_str(include_str!("./variable.yaml"))?;

        let csvs = glob::glob_with(
            &format!("{}/CSV/**/*.CSV", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )?;

        let erhs = glob::glob_with(
            &format!("{}/ERB/**/*.ERH", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )?;

        let erbs = glob::glob_with(
            &format!("{}/ERB/**/*.ERB", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )?;

        #[cfg(feature = "multithread")]
        let csvs = csvs.par_bridge();

        #[cfg(feature = "multithread")]
        let erbs = erbs.par_bridge();

        let mut files = Mutex::new(SimpleFiles::new());
        let mut diagnostic =
            Mutex::new(Diagnostic::error().with_code("E0001").with_message("Compile ERROR"));

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
                Err(_) => None,
            })
            .collect::<HashMap<_, _>>();

        let chara_csv_dic = csv_dic
            .drain_filter(|k, _v| k.starts_with("CHARA"))
            .collect::<HashMap<_, _>>();

        check_time!("Load CSV");

        for (k, (path, v)) in csv_dic.into_iter() {
            match k.as_str() {
                "ABL" | "MARK" | "BASE" | "CFLAG" | "EQUIP" | "TEQUIP" | "PALAM" | "EXP" | "EX"
                | "FLAG" | "TFLAG" | "TALENT" | "STAIN" | "SOURCE" | "TSTR" | "CSTR" | "STR"
                | "SAVESTR" | "GLOBAL" | "GLOBALS" | "TRAIN" | "TCVAR" => {
                    log::debug!("Merge {k}.CSV");
                    match info.merge_name_csv(&k, &v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            let file_id = files.lock().add(path.display().to_string(), v);
                            diagnostic.lock().labels.push(
                                Label::primary(file_id, span).with_message(format!("{}", err)),
                            );
                        }
                    }
                }
                "GAMEBASE" => {
                    log::debug!("Merge GAMEBASE.CSV");

                    match info.merge_gamebase_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            let file_id = files.lock().add(path.display().to_string(), v);
                            diagnostic.lock().labels.push(
                                Label::primary(file_id, span).with_message(format!("{}", err)),
                            );
                        }
                    }

                    log::info!("GAMEBASE: {:?}", info.gamebase);
                }
                "VARIABLESIZE" => {
                    log::debug!("Merge VARIABLESIZE.CSV");

                    match info.merge_variable_size_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            let file_id = files.lock().add(path.display().to_string(), v);
                            diagnostic.lock().labels.push(
                                Label::primary(file_id, span).with_message(format!("{}", err)),
                            );
                        }
                    }
                }
                "_REPLACE" => {
                    log::debug!("Merge _REPLACE.CSV");
                    match info.merge_replace_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            let file_id = files.lock().add(path.display().to_string(), v);
                            diagnostic.lock().labels.push(
                                Label::primary(file_id, span).with_message(format!("{}", err)),
                            );
                        }
                    }
                    log::info!("Replace: {:?}", info.replace);
                }
                "ITEM" => {
                    log::debug!("Merge ITEM.CSV");
                    match info.merge_item_csv(&v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            let file_id = files.lock().add(path.display().to_string(), v);
                            diagnostic.lock().labels.push(
                                Label::primary(file_id, span).with_message(format!("{}", err)),
                            );
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
                    let file_id = files.lock().add(path.display().to_string(), v);
                    diagnostic
                        .lock()
                        .labels
                        .push(Label::primary(file_id, span).with_message(format!("{}", err)));
                }
            }
        }

        check_time!("Merge chara CSV");

        tx.print_line(info.replace.start_message.clone());

        for erh in erhs {
            let erh = erh.unwrap();
            let source = read_file(&erh).unwrap();
            log::debug!("Parse {}", erh.display());

            match info.merge_header(&source) {
                Ok(()) => (),
                Err((err, span)) => {
                    let file_id = files.get_mut().add(erh.to_str().unwrap().to_string(), source);
                    diagnostic
                        .get_mut()
                        .labels
                        .push(Label::primary(file_id, span).with_message(format!("{}", err)));
                }
            }
        }

        check_time!("Merge ERH");

        // log::trace!("Header: {info:#?}");

        header_info = Arc::new(info);

        let funcs = erbs
            // .into_iter()
            .flat_map(|erb| {
                let erb = erb.unwrap();
                let source = read_file(&erb).unwrap();
                let ctx =
                    ParserContext::new(header_info.clone(), StrKey::new(erb.to_str().unwrap()));

                log::debug!("Parse And Compile {}", erb.display());

                let program = ctx.parse_and_compile(&mut Lexer::new(source.as_str()));

                match program {
                    Ok(p) => p,
                    Err((err, span)) => {
                        let file_id = files.lock().add(erb.to_str().unwrap().to_string(), source);
                        diagnostic
                            .lock()
                            .labels
                            .push(Label::primary(file_id, span).with_message(format!("{}", err)));
                        Vec::new()
                    }
                }
            })
            .collect::<Vec<CompiledFunction>>();

        ctx = VmContext::new(header_info.clone(), config, make_fs_manager(&target_path));

        for input in inputs {
            tx.push_input(input);
        }

        for func in funcs {
            function_dic.insert_compiled_func(
                &mut ctx.var,
                &ctx.header_info.default_local_size,
                func,
            );
        }

        check_time!("Parse/Compile ERB");

        let diagnostic = diagnostic.into_inner();
        let files = files.into_inner();

        if !diagnostic.labels.is_empty() {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = Config::default();
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                .unwrap();
            log::error!("총 {}개의 에러가 발생했습니다.", diagnostic.labels.len());
        }
    }

    let vm = TerminalVm::new(function_dic);

    Ok((vm, ctx, tx))
}
