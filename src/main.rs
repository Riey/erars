use anyhow::anyhow;
use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use eframe::NativeOptions;
use erars::{
    function::FunctionDic,
    ui::{ConsoleChannel, EraApp},
    vm::{TerminalVm, VariableInfo, VmContext},
};
use hashbrown::HashMap;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    path: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let chan = Arc::new(ConsoleChannel::new());

    let inner_chan = chan.clone();

    std::thread::spawn(move || {
        let infos: HashMap<String, VariableInfo> =
            serde_yaml::from_str(include_str!("./variable.yaml")).unwrap();

        let erbs = glob::glob_with(
            &format!(
                "{}/ERB/**/*.ERB",
                args.path
                    .map(|p| p.display().to_string())
                    .unwrap_or(".".into())
            ),
            glob::MatchOptions {
                case_sensitive: false,
                require_literal_leading_dot: true,
                require_literal_separator: true,
            },
        )
        .unwrap();

        let mut function_dic = FunctionDic::new();

        for erb in erbs {
            let erb = erb.unwrap();
            erars::compiler::compile(
                std::fs::read_to_string(&erb)
                    .unwrap()
                    .trim_start_matches("\u{feff}"),
                &mut function_dic,
            )
            .map_err(|e| anyhow!("Failed to compile[{}]: {}", erb.display(), e))
            .unwrap();
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
