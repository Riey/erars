use itertools::Itertools;
use parking_lot::Mutex;
use rayon::prelude::*;
use std::{path::PathBuf, sync::Arc, time::Instant};

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
    ui::{ConsoleChannel, ConsoleSender, EraApp, StdioBackend},
    vm::{TerminalVm, VmContext},
};
use erars_ast::{Value, VariableInfo};
use erars_compiler::{CompiledFunction, HeaderInfo, Lexer, ParserContext};
use hashbrown::HashMap;
use smol_str::SmolStr;

#[allow(unused_assignments)]
fn run_script(mut tx: ConsoleSender, target_path: String, inputs: Vec<Value>) {
    let mut time = Instant::now();

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
        let mut diagnostic =
            Mutex::new(Diagnostic::error().with_code("E0001").with_message("Compile ERROR"));

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
                        csv.file_stem().unwrap().to_str().unwrap().to_ascii_uppercase(),
                        (csv, s),
                    ))
                }
                Err(_) => None,
            })
            .collect::<HashMap<_, _>>();

        check_time!("Load CSV");

        for (k, (path, v)) in csv_dic.iter() {
            match k.as_str() {
                "ABL" | "BASE" | "CFLAG" | "EQUIP" | "TEQUIP" | "PALAM" | "EXP" | "EX" | "FLAG"
                | "TFLAG" | "TALENT" | "STAIN" | "SOURCE" | "TSTR" | "CSTR" | "STR" | "SAVESTR"
                | "GLOBAL" | "GLOBALS" | "TRAIN" => {
                    log::debug!("Merge {k}.CSV");
                    match info.merge_name_csv(k, v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            let file_id = files.lock().add(path.display().to_string(), v.clone());
                            diagnostic.lock().labels.push(
                                Label::primary(file_id, span).with_message(format!("{}", err)),
                            );
                        }
                    }
                }
                "_REPLACE" => {
                    log::debug!("Merge _REPLACE.CSV");
                    match info.merge_replace_csv(v) {
                        Ok(()) => {}
                        Err((err, span)) => {
                            let file_id = files.lock().add(format!("{k}.CSV"), v.clone());
                            diagnostic.lock().labels.push(
                                Label::primary(file_id, span).with_message(format!("{}", err)),
                            );
                        }
                    }
                    log::info!("Replace: {:?}", info.replace);
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
                                    Label::primary(file_id, span).with_message(format!("{}", err)),
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

                log::debug!("Parse {}", erb.display());

                let program = ctx.parse(&mut Lexer::new(source.as_str()));

                let program = match program {
                    Ok(p) => p,
                    Err((err, span)) => {
                        let file_id = files.lock().add(erb.to_str().unwrap().to_string(), source);
                        diagnostic
                            .lock()
                            .labels
                            .push(Label::primary(file_id, span).with_message(format!("{}", err)));
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

        for input in inputs {
            tx.push_input(input);
        }

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
            tx.exit();
            log::error!("총 {}개의 에러가 발생했습니다.", diagnostic.labels.len());
            return;
        }
    }

    let vm = TerminalVm::new(function_dic);
    let _ = vm.start(&mut tx, &mut ctx);

    tx.exit();

    log::info!("Program Terminated");
}

fn run(mut backend: impl EraApp, target_path: String, inputs: Vec<Value>) -> anyhow::Result<()> {
    let chan = Arc::new(ConsoleChannel::new());
    let tx = ConsoleSender::new(chan.clone());

    std::thread::spawn(move || run_script(tx, target_path, inputs));

    backend.run(chan)
}

#[derive(clap::Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(
        value_parser,
        default_value = ".",
        help = "ERA game path default is current path"
    )]
    target_path: String,

    #[clap(long, help = "Accept input value from file")]
    use_input: Option<PathBuf>,

    #[clap(
        long,
        default_value = "info",
        help = "Log level (error, warn, info, debug, trace)"
    )]
    log_level: String,

    #[clap(long, help = "Don't print logs")]
    quite: bool,
}

fn main() {
    use flexi_logger::*;

    let args: Args = clap::Parser::parse();

    let _handle = if args.quite {
        None
    } else {
        Some(
            Logger::try_with_str(&args.log_level)
                .unwrap()
                .rotate(
                    Criterion::AgeOrSize(Age::Day, 1024 * 1024),
                    Naming::Numbers,
                    Cleanup::KeepLogFiles(5),
                )
                .log_to_file(FileSpec::default().directory("logs").basename("erars"))
                .write_mode(WriteMode::BufferAndFlush)
                .create_symlink("last_log.log")
                .use_utc()
                .start()
                .unwrap(),
        )
    };

    log_panics::init();

    let inputs = match args.use_input {
        Some(input) => {
            ron::from_str::<Vec<Value>>(&std::fs::read_to_string(input).unwrap()).unwrap()
        }
        None => Vec::new(),
    };

    run(StdioBackend::new(), args.target_path, inputs).unwrap();
}
