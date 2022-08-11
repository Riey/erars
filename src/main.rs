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
use erars_compiler::{Lexer, ParserContext};
use hashbrown::HashMap;
use smol_str::SmolStr;

fn main() {
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

        let mut files = SimpleFiles::new();
        let mut diagnostic = Diagnostic::error()
            .with_code("E0001")
            .with_message("Compile ERROR");

        let ctx = ParserContext::new(HashMap::default());

        for erb in erbs {
            let erb = erb.unwrap();

            let source = std::fs::read_to_string(&erb).unwrap();
            let program = ctx.parse(&mut Lexer::new(source.as_str()));
            let file_id = files.add(erb.to_str().unwrap().to_string(), source);

            let program = match program {
                Ok(p) => p,
                Err((err, span)) => {
                    diagnostic
                        .labels
                        .push(Label::primary(file_id, span).with_message(format!("{}", err)));
                    continue;
                }
            };

            eprintln!("Compile {}", erb.display());

            for func in program {
                let func = erars_compiler::compile(func).unwrap();

                function_dic.insert_compiled_func(func);
            }
        }

        if !diagnostic.labels.is_empty() {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = Config::default();
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                .unwrap();
            inner_chan.send_msg(erars::ui::ConsoleMessage::Exit);
            eprintln!("총 {}개의 에러가 발생했습니다.", diagnostic.labels.len());
            return;
        }

        let mut ctx = VmContext::new(&infos);
        let vm = TerminalVm::new(function_dic);
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
