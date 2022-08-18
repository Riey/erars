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
    let header = test_util::get_ctx("").header;

    for erb_file in erb_files {
        let mut ctx = VmContext::new(header.clone());
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
            dic.insert_compiled_func(ctx.var_mut(), compile(func).unwrap());
        }

        eprintln!("FunctionDic: {dic:#?}");
        eprintln!("Header: {header:#?}");

        let ret = test_runner(dic, ctx);
        let expected_ret: Vec<ConsoleMessage> = ron::from_str(&ron_source).unwrap();

        k9::assert_equal!(ret, expected_ret);
    }
}

fn test_runner(dic: FunctionDic, mut ctx: VmContext) -> Vec<ConsoleMessage> {
    let vm = TerminalVm::new(dic);
    let chan = ConsoleChannel::new();

    vm.start(&chan, &mut ctx).unwrap();
    chan.take_all_msg()
}
