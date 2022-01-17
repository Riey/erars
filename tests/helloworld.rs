use erars::compiler::compile;
use erars::instruction::Instruction;
use erars::vm::*;

#[test]
fn helloworld() {
    let code = "@SYSTEM_TITLE\nPRINTL Hello, world!\nQUIT";

    let inst = compile(code).unwrap();

    k9::snapshot!(
        &inst,
        r#"
[
    (
        "SYSTEM_TITLE",
        [
            LoadStr(
                "Hello, world!",
            ),
            Print(
                NEWLINE,
            ),
            ListBegin,
            ListEnd,
            LoadStr(
                "QUIT",
            ),
            Command,
        ],
    ),
]
"#
    );

    k9::snapshot!(
        run_test(inst),
        r#"
[
    Print(
        "Hello, world!",
    ),
    NewLine,
    Exit,
]
"#
    );
}

fn run_test(inst: Vec<(String, Vec<Instruction>)>) -> Vec<ConsoleMessage> {
    let mut ctx = VmContext::new(&Default::default());
    let vm = TerminalVm::new(inst.into_iter().collect());
    let chan = ConsoleChannel::new();

    vm.start(&chan, &mut ctx).unwrap();
    chan.take_all_msg()
}
