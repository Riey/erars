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
use erars::{
    function::FunctionDic,
    ui::{ConsoleChannel, ConsoleMessage, EraApp, StdioBackend},
    vm::{TerminalVm, VmContext},
};
use erars_ast::VariableInfo;
use erars_compiler::{CompiledFunction, HeaderInfo, Lexer, ParserContext};
use hashbrown::HashMap;
use smol_str::SmolStr;

fn try_load_csv(info: &mut HeaderInfo, csv_dic: &HashMap<String, String>, var: &str) {
    match csv_dic.get(var) {
        Some(csv) => {
            log::debug!("Merge {var}.CSV");
            info.merge_name_csv(var, csv).ok();
        }
        None => {}
    }
}

fn run(mut backend: impl EraApp) -> anyhow::Result<()> {
    let chan = Arc::new(ConsoleChannel::new());

    let inner_chan = chan.clone();

    std::thread::spawn(move || {
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
                        s,
                    ))
                }
                Err(_) => None,
            })
            .collect::<HashMap<_, _>>();

        try_load_csv(&mut header_info, &csv_dic, "ABL");
        try_load_csv(&mut header_info, &csv_dic, "BASE");
        try_load_csv(&mut header_info, &csv_dic, "EQUIP");
        try_load_csv(&mut header_info, &csv_dic, "TEQUIP");
        try_load_csv(&mut header_info, &csv_dic, "PALAM");
        try_load_csv(&mut header_info, &csv_dic, "EXP");
        try_load_csv(&mut header_info, &csv_dic, "SOURCE");
        try_load_csv(&mut header_info, &csv_dic, "EX");
        try_load_csv(&mut header_info, &csv_dic, "FLAG");
        try_load_csv(&mut header_info, &csv_dic, "CFLAG");
        try_load_csv(&mut header_info, &csv_dic, "TFLAG");
        try_load_csv(&mut header_info, &csv_dic, "TALENT");
        try_load_csv(&mut header_info, &csv_dic, "ITEM");
        try_load_csv(&mut header_info, &csv_dic, "STAIN");

        try_load_csv(&mut header_info, &csv_dic, "TSTR");
        try_load_csv(&mut header_info, &csv_dic, "CSTR");
        try_load_csv(&mut header_info, &csv_dic, "STR");

        try_load_csv(&mut header_info, &csv_dic, "SAVESTR");
        try_load_csv(&mut header_info, &csv_dic, "GLOBAL");
        try_load_csv(&mut header_info, &csv_dic, "GLOBALS");
        // try_load_csv(&mut header_info, &target_path, "CDFLAG");

        for erh in erhs {
            let erh = erh.unwrap();
            let source = std::fs::read_to_string(&erh).unwrap();
            log::debug!("Parse {}", erh.display());

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
            // .into_iter()
            .par_bridge()
            .flat_map(|erb| {
                let ctx = ParserContext::new(header_info.clone());
                let erb = erb.unwrap();
                let source = std::fs::read_to_string(&erb).unwrap();

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
            inner_chan.exit();
            log::error!("총 {}개의 에러가 발생했습니다.", diagnostic.labels.len());
            return;
        }

        let mut ctx = VmContext::new(&header_info.global_variables);
        let vm = TerminalVm::new(function_dic, header_info);
        let ret = vm.start(&inner_chan, &mut ctx);

        if let Err(err) = ret {
            log::error!("{}", err);
            inner_chan.send_msg(ConsoleMessage::Print(format!("VM 에러 발생: {err}")));
        }

        log::info!("Program Terminated");
    });

    backend.run(chan)
}

fn main() {
    {
        use simplelog::*;
        CombinedLogger::init(vec![
            // TermLogger::new(
            //     LevelFilter::Info,
            //     Config::default(),
            //     TerminalMode::Mixed,
            //     ColorChoice::Auto,
            // ),
            WriteLogger::new(
                LevelFilter::Trace,
                Config::default(),
                std::fs::File::create("erars.log").unwrap(),
            ),
        ])
        .unwrap();
    }

    log_panics::init();

    run(StdioBackend::new()).unwrap();
}
