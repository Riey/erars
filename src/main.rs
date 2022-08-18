use itertools::Itertools;
use parking_lot::Mutex;
use rayon::prelude::*;
use std::{sync::Arc, time::Instant};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use erars::{
    function::FunctionDic,
    ui::{ConsoleChannel, ConsoleMessage, EraApp, StdioBackend},
    vm::{TerminalVm, VmContext},
};
use erars_ast::VariableInfo;
use erars_compiler::{CompiledFunction, HeaderInfo, Lexer, ParserContext};
use hashbrown::HashMap;
use smol_str::SmolStr;

fn run(mut backend: impl EraApp) -> anyhow::Result<()> {
    let mut time = Instant::now();

    let chan = Arc::new(ConsoleChannel::new());

    let inner_chan = chan.clone();

    macro_rules! check_time {
        ($work:expr) => {
            let m = time.elapsed().as_millis();
            time = Instant::now();

            inner_chan.send_msg(ConsoleMessage::Print(format!("[{}]: {}ms", $work, m)));
            inner_chan.send_msg(ConsoleMessage::NewLine);
        };
    }

    #[allow(unused_assignments)]
    std::thread::spawn(move || {
        let mut function_dic = FunctionDic::new();
        let header_info;
        let mut ctx: VmContext;

        {
            check_time!("Initialize");

            let mut args = std::env::args();

            let target_path = if let Some(path) = args.nth(1) {
                path
            } else {
                ".".into()
            };

            let infos: HashMap<SmolStr, VariableInfo> =
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
            let mut diagnostic = Mutex::new(
                Diagnostic::error()
                    .with_code("E0001")
                    .with_message("Compile ERROR"),
            );

            let mut info = HeaderInfo {
                global_variables: infos,
                ..Default::default()
            };

            let csv_dic = csvs
                .par_bridge()
                .filter_map(|csv| match csv {
                    Ok(csv) => {
                        log::trace!("Load {}", csv.display());
                        let s = std::fs::read_to_string(&csv).ok()?;

                        Some((
                            csv.file_stem()
                                .unwrap()
                                .to_str()
                                .unwrap()
                                .to_ascii_uppercase(),
                            (csv, s),
                        ))
                    }
                    Err(_) => None,
                })
                .collect::<HashMap<_, _>>();

            check_time!("Load CSV");

            for (k, (path, v)) in csv_dic.iter() {
                match k.as_str() {
                    "ABL" | "BASE" | "CFLAG" | "EQUIP" | "TEQUIP" | "PALAM" | "EXP" | "EX"
                    | "FLAG" | "TFLAG" | "TALENT" | "STAIN" | "SOURCE" | "TSTR" | "CSTR"
                    | "STR" | "SAVESTR" | "GLOBAL" | "GLOBALS" => {
                        log::debug!("Merge {k}.CSV");
                        match info.merge_name_csv(k, v) {
                            Ok(()) => {}
                            Err((err, span)) => {
                                let file_id =
                                    files.lock().add(path.display().to_string(), v.clone());
                                diagnostic.lock().labels.push(
                                    Label::primary(file_id, span).with_message(format!("{}", err)),
                                );
                            }
                        }
                    }
                    "ITEM" => {
                        log::debug!("Merge ITEM.CSV");
                        match info.merge_item_csv(v) {
                            Ok(()) => {}
                            Err((err, span)) => {
                                let file_id = files.lock().add(format!("{k}.CSV"), v.clone());
                                diagnostic.lock().labels.push(
                                    Label::primary(file_id, span).with_message(format!("{}", err)),
                                );
                            }
                        }
                    }
                    other => {
                        if k.starts_with("CHARA") {
                            log::debug!("Merge {k}.CSV");
                            match info.merge_chara_csv(v) {
                                Ok(()) => {}
                                Err((err, span)) => {
                                    let file_id = files.lock().add(format!("{k}.CSV"), v.clone());
                                    diagnostic.lock().labels.push(
                                        Label::primary(file_id, span)
                                            .with_message(format!("{}", err)),
                                    );
                                }
                            }
                        } else {
                            log::warn!("Unknown csv name {other}");
                        }
                    }
                }
            }

            drop(csv_dic);

            check_time!("Merge CSV");

            for erh in erhs {
                let erh = erh.unwrap();
                let source = std::fs::read_to_string(&erh).unwrap();
                log::debug!("Parse {}", erh.display());

                match info.merge_header(&source) {
                    Ok(()) => (),
                    Err((err, span)) => {
                        let file_id = files
                            .get_mut()
                            .add(erh.to_str().unwrap().to_string(), source);
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

                    log::debug!("Parse {}", erb.display());

                    let program = ctx.parse(&mut Lexer::new(source.as_str()));

                    let program = match program {
                        Ok(p) => p,
                        Err((err, span)) => {
                            let file_id =
                                files.lock().add(erb.to_str().unwrap().to_string(), source);
                            diagnostic.lock().labels.push(
                                Label::primary(file_id, span).with_message(format!("{}", err)),
                            );
                            Vec::new()
                        }
                    };

                    log::debug!("Compile {}", erb.display());

                    program
                        .into_iter()
                        .map(|f| erars_compiler::compile(f).unwrap())
                        .collect_vec()
                })
                .collect::<Vec<CompiledFunction>>();

            ctx = VmContext::new(header_info.clone());

            for func in funcs {
                function_dic.insert_compiled_func(&mut ctx.var_mut(), func);
            }

            check_time!("Parse/Compile ERB");

            let diagnostic = diagnostic.into_inner();
            let files = files.into_inner();

            if !diagnostic.labels.is_empty() {
                let writer = StandardStream::stderr(ColorChoice::Always);
                let config = Config::default();
                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                    .unwrap();
                inner_chan.exit();
                log::error!("총 {}개의 에러가 발생했습니다.", diagnostic.labels.len());
                return;
            }
        }

        let vm = TerminalVm::new(function_dic);
        vm.start(&inner_chan, &mut ctx).unwrap();

        inner_chan.exit();

        log::info!("Program Terminated");
    });

    backend.run(chan)
}

fn main() {
    {
        use simplelog::*;
        CombinedLogger::init(vec![WriteLogger::new(
            LevelFilter::Debug,
            Config::default(),
            std::fs::File::create("erars.log").unwrap(),
        )])
        .unwrap();
    }

    log_panics::init();

    run(StdioBackend::new()).unwrap();
}
