use erars::compiler::compile;
use erars::function::FunctionDic;
use erars::ui::{ConsoleChannel, ConsoleMessage};
use erars::vm::*;

#[test]
fn comment() {
    let code = r#";≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡
;게임 개시용의 처리
;≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡
;=============================================================================
;「처음부터」 선택시의 처리
;=============================================================================
;｢CALL EVENT_NEXTDAY_T｣｢CALL EVENTCHECK_M｣을 추가
@EVENTFIRST"#;

    let mut dic = FunctionDic::new();
    compile(code, &mut dic).unwrap();
}

#[test]
fn conditional() {
    let code = "@SYSTEM_TITLE\nPRINTFORML 1 == 0 = %(1 == 1) ? \"TRUE\" # \"FALSE\"%";
    let mut dic = FunctionDic::new();

    compile(code, &mut dic).unwrap();

    k9::snapshot!(
        dic.get_func("SYSTEM_TITLE").unwrap(),
        r#"
[
    ListBegin,
    LoadStr(
        "1 == 0 = ",
    ),
    LoadInt(
        1,
    ),
    LoadInt(
        1,
    ),
    BinaryOperator(
        Equal,
    ),
    GotoIfNot(
        8,
    ),
    LoadStr(
        "TRUE",
    ),
    Goto(
        9,
    ),
    LoadStr(
        "FALSE",
    ),
    LoadStr(
        "",
    ),
    ListEnd,
    ConcatString,
    Print(
        NEWLINE,
    ),
]
"#
    );

    k9::snapshot!(
        run_test(dic),
        r#"
[
    Print(
        "1 == 0 = TRUE",
    ),
    NewLine,
]
"#
    );
}

#[test]
fn sub() {
    let code = "@SYSTEM_TITLE\nPRINTFORML 1 - 2 = {1 - 2}";
    let mut dic = FunctionDic::new();
    compile(code, &mut dic).unwrap();

    k9::snapshot!(
        dic.get_func("SYSTEM_TITLE").unwrap(),
        r#"
[
    ListBegin,
    LoadStr(
        "1 - 2 = ",
    ),
    LoadInt(
        1,
    ),
    LoadInt(
        2,
    ),
    BinaryOperator(
        Sub,
    ),
    LoadStr(
        "",
    ),
    ListEnd,
    ConcatString,
    Print(
        NEWLINE,
    ),
]
"#
    );

    k9::snapshot!(
        run_test(dic),
        r#"
[
    Print(
        "1 - 2 = -1",
    ),
    NewLine,
]
"#
    );
}

#[test]
fn add() {
    let code = "@SYSTEM_TITLE\nPRINTFORML 1 + 1 = {1 + 1}";
    let mut dic = FunctionDic::new();
    compile(code, &mut dic).unwrap();

    k9::snapshot!(
        dic.get_func("SYSTEM_TITLE").unwrap(),
        r#"
[
    ListBegin,
    LoadStr(
        "1 + 1 = ",
    ),
    LoadInt(
        1,
    ),
    LoadInt(
        1,
    ),
    BinaryOperator(
        Add,
    ),
    LoadStr(
        "",
    ),
    ListEnd,
    ConcatString,
    Print(
        NEWLINE,
    ),
]
"#
    );

    k9::snapshot!(
        run_test(dic),
        r#"
[
    Print(
        "1 + 1 = 2",
    ),
    NewLine,
]
"#
    );
}

#[test]
fn compare() {
    let code = "@SYSTEM_TITLE\nPRINTFORML {0 == 0}";
    let mut dic = FunctionDic::new();
    compile(code, &mut dic).unwrap();

    k9::snapshot!(
        dic.get_func("SYSTEM_TITLE").unwrap(),
        r#"
[
    ListBegin,
    LoadStr(
        "",
    ),
    LoadInt(
        0,
    ),
    LoadInt(
        0,
    ),
    BinaryOperator(
        Equal,
    ),
    LoadStr(
        "",
    ),
    ListEnd,
    ConcatString,
    Print(
        NEWLINE,
    ),
]
"#
    );

    k9::snapshot!(
        run_test(dic),
        r#"
[
    Print(
        "1",
    ),
    NewLine,
]
"#
    );
}

#[test]
fn if_false() {
    let code = "@SYSTEM_TITLE\nIF 0\nPRINTL TRUE\nELSE\nPRINTL FALSE\nENDIF\nQUIT";
    let mut dic = FunctionDic::new();
    compile(code, &mut dic).unwrap();

    k9::snapshot!(
        dic.get_func("SYSTEM_TITLE").unwrap(),
        r#"
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
]
"#
    );

    k9::snapshot!(
        run_test(dic),
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
    let mut dic = FunctionDic::new();
    compile(code, &mut dic).unwrap();

    k9::snapshot!(
        dic.get_func("SYSTEM_TITLE").unwrap(),
        r#"
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
]
"#
    );

    k9::snapshot!(
        run_test(dic),
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

fn run_test(dic: FunctionDic) -> Vec<ConsoleMessage> {
    let mut ctx = VmContext::new(&Default::default());
    let vm = TerminalVm::new(dic);
    let chan = ConsoleChannel::new();

    vm.start(&chan, &mut ctx).unwrap();
    chan.take_all_msg()
}
