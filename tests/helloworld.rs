use erars::compiler::compile;
use erars::instruction::Instruction;
use erars::vm::*;

#[test]
fn if_false() {
    let code = "@SYSTEM_TITLE\nIF 0\nPRINTL TRUE\nELSE\nPRINTL FALSE\nENDIF\nQUIT";
    let inst = compile(code).unwrap();

    k9::snapshot!(
        &inst,
        r#"
[
    (
        "SYSTEM_TITLE",
        [
            LoadInt(
                0,
            ),
            GotoIfNot(
                5,
            ),
            LoadStr(
                "TRUE",
            ),
            Print(
                NEWLINE,
            ),
            Goto(
                7,
            ),
            LoadStr(
                "FALSE",
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
        "FALSE",
    ),
    NewLine,
    Exit,
]
"#
    );
}

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
