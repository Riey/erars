use std::sync::Arc;

use eframe::NativeOptions;
use erars::{
    instruction::{BeginType, Instruction},
    vm::{ConsoleChannel, EraApp, TerminalVm, VariableInfo, VmContext},
};
use hashbrown::HashMap;
use rayon::prelude::*;

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

        let functions: HashMap<String, Vec<Instruction>> = erbs
            .par_bridge()
            .flat_map(|erb| {
                erb.map_err(anyhow::Error::from)
                    .and_then(|erb| erars::compiler::compile(&std::fs::read_to_string(erb)?))
                    .unwrap()
            })
            .collect();

        std::fs::write("dump.txt", format!("{:#?}", functions)).unwrap();

        let mut ctx = VmContext::new(&infos);
        let vm = TerminalVm::new(functions);
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
