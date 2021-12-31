// const FONT: &[u8] = include_bytes!("../res/D2Coding-Ver1.3.2-20180524.ttc");

fn main() {
    let program = erars::compiler::compile("PRINTFORML MONEY:123 = %MONEY:123%").unwrap();

    dbg!(&program);

    let mut vm = erars::vm::TerminalVm::new();

    for line in program.iter() {
        vm.run(line).unwrap();
    }
}
