use erars::function::FunctionDic;
use erars::ui::{ConsoleChannel, ConsoleMessage};
use erars::vm::*;
use erars_compiler::{compile, parse_program, VariableDic};
use std::sync::Arc;

#[test]
fn run_test() {
    let erb_files = glob::glob("tests/run_tests/*.erb").unwrap();
    let var = Arc::new(VariableDic::default());

    for erb_file in erb_files {
        let erb_file = erb_file.unwrap();
        let ron_file = erb_file.parent().unwrap().join(format!(
            "{}.ron",
            erb_file.file_stem().unwrap().to_str().unwrap()
        ));

        eprintln!("Run {}", erb_file.display());

        let erb_source = std::fs::read_to_string(&erb_file).unwrap();
        let ron_source = std::fs::read_to_string(ron_file).unwrap();
        let program = parse_program(&erb_source, &var).unwrap();
        let mut dic = FunctionDic::new();

        for func in program {
            dic.insert_compiled_func(&var, compile(func, &var).unwrap());
        }

        let ret = test_runner(dic, var.clone());
        let expected_ret: Vec<ConsoleMessage> = ron::from_str(&ron_source).unwrap();

        k9::assert_equal!(ret, expected_ret);
    }
}

fn test_runner(dic: FunctionDic, var: Arc<VariableDic>) -> Vec<ConsoleMessage> {
    let mut ctx = VmContext::new(var);
    let vm = TerminalVm::new(dic);
    let chan = ConsoleChannel::new();

    vm.start(&chan, &mut ctx).unwrap();
    chan.take_all_msg()
}
