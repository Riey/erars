use erars::function::FunctionDic;
use erars::ui::{ConsoleChannel, ConsoleMessage};
use erars::vm::*;
use erars_compiler::{compile, ParserContext};

mod test_util;

#[test]
fn run_test() {
    {
        use simplelog::*;
        CombinedLogger::init(vec![TermLogger::new(
            LevelFilter::Warn,
            Config::default(),
            TerminalMode::Mixed,
            ColorChoice::Auto,
        )])
        .unwrap();
    }
    let erb_files = glob::glob("tests/run_tests/*.erb").unwrap();

    for erb_file in erb_files {
        let erb_file = erb_file.unwrap();
        let ron_file = erb_file.parent().unwrap().join(format!(
            "{}.ron",
            erb_file.file_stem().unwrap().to_str().unwrap()
        ));

        eprintln!("Run {}", erb_file.display());

        let ron_source = std::fs::read_to_string(ron_file).unwrap();
        let program =
            test_util::do_test(erb_file.to_str().unwrap(), ParserContext::parse_program_str);
        let mut dic = FunctionDic::new();

        for func in program {
            dic.insert_compiled_func(compile(func).unwrap());
        }

        eprintln!("FunctionDic: {dic:?}");

        let ret = test_runner(dic);
        let expected_ret: Vec<ConsoleMessage> = ron::from_str(&ron_source).unwrap();

        k9::assert_equal!(ret, expected_ret);
    }
}

fn test_runner(dic: FunctionDic) -> Vec<ConsoleMessage> {
    let infos = serde_yaml::from_str(include_str!("../src/variable.yaml")).unwrap();
    let mut ctx = VmContext::new(&infos);
    let vm = TerminalVm::new(dic, test_util::get_ctx("").header);
    let chan = ConsoleChannel::new();

    vm.start(&chan, &mut ctx).unwrap();
    chan.take_all_msg()
}
