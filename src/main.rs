use itertools::Itertools;
use parking_lot::Mutex;
use rayon::prelude::*;
use std::sync::Arc;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use eframe::NativeOptions;
use erars::{
    function::FunctionDic,
    ui::{ConsoleChannel, EraApp},
    vm::{TerminalVm, VmContext},
};
use erars_ast::VariableInfo;
use erars_compiler::{CompiledFunction, HeaderInfo, Lexer, ParserContext};
use hashbrown::HashMap;
use smol_str::SmolStr;

fn main() {
    log_panics::init();

    {
        use simplelog::*;
        CombinedLogger::init(vec![
            TermLogger::new(
                LevelFilter::Warn,
                Config::default(),
                TerminalMode::Mixed,
                ColorChoice::Auto,
            ),
            WriteLogger::new(
                LevelFilter::Info,
                Config::default(),
                std::fs::File::create("erars.log").unwrap(),
            ),
        ])
        .unwrap();
    }

    let chan = Arc::new(ConsoleChannel::new());
    let mut args = std::env::args();

    let target_path = if let Some(path) = args.nth(1) {
        path
    } else {
        ".".into()
    };

    let inner_chan = chan.clone();

    std::thread::spawn(move || {
        let infos: HashMap<SmolStr, VariableInfo> =
            serde_yaml::from_str(include_str!("./variable.yaml")).unwrap();

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

        let mut function_dic = FunctionDic::new();

        let mut files = Mutex::new(SimpleFiles::new());
        let mut diagnostic = Mutex::new(
            Diagnostic::error()
                .with_code("E0001")
                .with_message("Compile ERROR"),
        );

        let mut header_info = HeaderInfo {
            global_variables: infos,
            ..Default::default()
        };

        for erh in erhs {
            let erh = erh.unwrap();
            let source = std::fs::read_to_string(&erh).unwrap();
            log::info!("Parse {}", erh.display());

            match header_info.merge_header(&source) {
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

        let header_info = Arc::new(header_info);

        let funcs = erbs
            .par_bridge()
            .flat_map(|erb| {
                let ctx = ParserContext::new(header_info.clone());
                let erb = erb.unwrap();
                let source = std::fs::read_to_string(&erb).unwrap();

                log::info!("Parse {}", erb.display());

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

                log::info!("Compile {}", erb.display());

                program
                    .into_iter()
                    .map(|f| erars_compiler::compile(f).unwrap())
                    .collect_vec()
            })
            .collect::<Vec<CompiledFunction>>();

        for func in funcs {
            function_dic.insert_compiled_func(func);
        }

        let diagnostic = diagnostic.into_inner();
        let files = files.into_inner();

        if !diagnostic.labels.is_empty() {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = Config::default();
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                .unwrap();
            inner_chan.send_msg(erars::ui::ConsoleMessage::Exit);
            log::error!("총 {}개의 에러가 발생했습니다.", diagnostic.labels.len());
            return;
        }

        let mut ctx = VmContext::new(&header_info.global_variables);
        let vm = TerminalVm::new(function_dic, header_info);
        let ret = vm.start(&inner_chan, &mut ctx);

        if let Err(err) = ret {
            eprintln!("{}", err);
            inner_chan.send_msg(erars::ui::ConsoleMessage::Exit);
        }

        println!("Program Terminated");
    });

    let app = EraApp::new(chan);

    eframe::run_native(
        Box::new(app),
        NativeOptions {
            ..Default::default()
        },
    );
}
