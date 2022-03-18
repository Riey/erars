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
    vm::{TerminalVm, VariableInfo, VmContext},
};
use erars_compiler::{CompiledFunctionType, Event, EventType};
use hashbrown::HashMap;

fn main() {
    let chan = Arc::new(ConsoleChannel::new());

    let inner_chan = chan.clone();

    std::thread::spawn(move || {
        let infos: HashMap<String, VariableInfo> =
            serde_yaml::from_str(include_str!("./variable.yaml")).unwrap();

        let erbs = glob::glob_with(
            "ERB/**/*.ERB",
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )
        .unwrap();

        let mut function_dic = FunctionDic::new();

        let mut files = SimpleFiles::new();

        for erb in erbs {
            erb.map_err(anyhow::Error::from)
                .and_then(|erb| {
                    let source = std::fs::read_to_string(&erb).unwrap();
                    let program =
                        erars_compiler::parse_program(source.trim_start_matches("\u{feff}"));
                    let file_id = files.add(erb.to_str().unwrap().to_string(), source);

                    let program = match program {
                        Ok(p) => p,
                        Err((err, span)) => {
                            let diagnostic = Diagnostic::error()
                                .with_code("E0001")
                                .with_message("Compile ERROR")
                                .with_labels(vec![
                                    Label::primary(file_id, span).with_message(format!("{}", err))
                                ]);
                            let writer = StandardStream::stderr(ColorChoice::Always);
                            let config = Config::default();
                            codespan_reporting::term::emit(
                                &mut writer.lock(),
                                &config,
                                &files,
                                &diagnostic,
                            )
                            .unwrap();
                            return Ok(());
                        }
                    };

                    for func in program {
                        let func = erars_compiler::compile(func).unwrap();

                        match func.ty {
                            CompiledFunctionType::Event(ev) => {
                                function_dic.insert_event(ev, func.body)
                            }
                            CompiledFunctionType::Normal(label) => {
                                function_dic.insert_func(label, func.body)
                            }
                        }
                    }

                    Ok(())
                })
                .unwrap()
        }

        let mut ctx = VmContext::new(&infos);
        let vm = TerminalVm::new(function_dic);
        vm.start(&inner_chan, &mut ctx).unwrap();

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
