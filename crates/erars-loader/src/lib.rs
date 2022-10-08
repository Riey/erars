use parking_lot::Mutex;
use rayon::prelude::*;
use std::{path::Path, sync::Arc, time::Instant};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use erars_ast::{Value, VariableInfo};
use erars_compiler::{CompiledFunction, EraConfig, HeaderInfo, Lexer, ParserContext};
use erars_ui::{ConsoleChannel, ConsoleSender};
use erars_vm::{FunctionDic, TerminalVm, VmContext};
use hashbrown::HashMap;

#[allow(unused_assignments)]
pub fn run_script(chan: Arc<ConsoleChannel>, target_path: String, inputs: Vec<Value>) {
    let mut time = Instant::now();

    let config_path = format!("{target_path}/emuera.config");

    let config = if Path::new(&config_path).exists() {
        match std::fs::read_to_string(&config_path) {
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
    let mut tx = ConsoleSender::new(chan, config.printc_width);

    macro_rules! check_time {
        ($work:expr) => {
            let m = time.elapsed().as_millis();
            time = Instant::now();

            tx.print_line(format!("[{}]: {}ms", $work, m));
        };
    }

    let mut function_dic = FunctionDic::new();
    let header_info;
    let mut ctx: VmContext;

    {
        check_time!("Initialize");

        let var_infos: HashMap<_, VariableInfo> =
            serde_yaml::from_str(include_str!("./variable.yaml")).unwrap();

        let csvs = glob::glob_with(
            &format!("{}/CSV/**/*.CSV", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )
        .unwrap();

        let erhs = glob::glob_with(
            &format!("{}/ERB/**/*.ERH", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )
        .unwrap();

        let erbs = glob::glob_with(
            &format!("{}/ERB/**/*.ERB", target_path),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )
        .unwrap();

        let mut files = Mutex::new(SimpleFiles::new());
        let mut diagnostic =
            Mutex::new(Diagnostic::error().with_code("E0001").with_message("Compile ERROR"));

        let mut info = HeaderInfo {
            global_variables: var_infos,
            ..Default::default()
        };

        let mut csv_dic = csvs
            .par_bridge()
            .filter_map(|csv| match csv {
                Ok(csv) => {
                    log::trace!("Load {}", csv.display());
                    let s = std::fs::read_to_string(&csv).ok()?;

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
                "ABL" | "BASE" | "CFLAG" | "EQUIP" | "TEQUIP" | "PALAM" | "EXP" | "EX" | "FLAG"
                | "TFLAG" | "TALENT" | "STAIN" | "SOURCE" | "TSTR" | "CSTR" | "STR" | "SAVESTR"
                | "GLOBAL" | "GLOBALS" | "TRAIN" => {
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
            let source = std::fs::read_to_string(&erh).unwrap();
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
            .par_bridge()
            .flat_map(|erb| {
                let erb = erb.unwrap();
                let source = std::fs::read_to_string(&erb).unwrap();
                let ctx = ParserContext::new(header_info.clone(), erb.to_str().unwrap().into());

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

        ctx = VmContext::new(header_info.clone(), config);

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
            tx.exit();
            log::error!("총 {}개의 에러가 발생했습니다.", diagnostic.labels.len());
            return;
        }
    }

    let vm = TerminalVm::new(function_dic, target_path.into());
    let _ = vm.start(&mut tx, &mut ctx);

    tx.exit();

    log::info!("Program Terminated");
}