use erars::function::FunctionDic;
use erars::ui::{ConsoleChannel, ConsoleMessage};
use erars::vm::*;
use erars_compiler::{compile, parse_program, VariableInterner};

#[test]
fn run_test() {
    let erb_files = glob::glob("tests/run_tests/*.erb").unwrap();
    let var = VariableInterner::with_default_variables();

    for erb_file in erb_files {
        let erb_file = erb_file.unwrap();
        let ron_file = erb_file.parent().unwrap().join(format!(
            "{}.ron",
            erb_file.file_stem().unwrap().to_str().unwrap()
        ));
        let erb_source = std::fs::read_to_string(&erb_file).unwrap();
        let ron_source = std::fs::read_to_string(ron_file).unwrap();
        let program = parse_program(&erb_source, &var).unwrap();
        let mut dic = FunctionDic::new();

        for func in program {
            dic.insert_compiled_func(compile(func, &var).unwrap());
        }

        let ret = test_runner(dic, var.clone());
        let expected_ret: Vec<ConsoleMessage> = ron::from_str(&ron_source).unwrap();

        k9::assert_equal!(ret, expected_ret);
    }
}

fn test_runner(dic: FunctionDic, var: VariableInterner) -> Vec<ConsoleMessage> {
    let infos = serde_yaml::from_str(include_str!("../src/variable.yaml")).unwrap();
    let mut ctx = VmContext::new(&infos);
    let vm = TerminalVm::new(dic, var);
    let chan = ConsoleChannel::new();

    vm.start(&chan, &mut ctx).unwrap();
    chan.take_all_msg()
}
