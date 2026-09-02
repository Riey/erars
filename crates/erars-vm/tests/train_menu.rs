//! `printc_count` (emuera.config `PRINTCを並べる数`, Emuera `PrintCPerLine`):
//! the TRAIN command menu breaks the line after every `printc_count` PRINTC
//! items and `0` disables the break (`terminal_vm/executor.rs`,
//! `run_begin`, `BeginType::Train`). Drives the menu with a scripted
//! `SystemFunctions` that answers the one input request with `0` (COM0,
//! which QUITs).

use std::sync::Arc;

use erars_ast::{StrKey, Value};
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::{InputRequest, VirtualConsole};
use erars_vm::{console_config, FunctionDic, SystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = "@SYSTEM_TITLE\nBEGIN TRAIN\n\n@COM0\nQUIT\n";
const TRAIN_CSV: &str = "0,A\n1,B\n2,C\n3,D\n4,E\n";

/// Answers each input request with the next scripted value, then `None`.
struct Scripted(Vec<i64>);

impl SystemFunctions for Scripted {
    fn input(&mut self, _req: InputRequest) -> anyhow::Result<Option<Value>> {
        Ok(if self.0.is_empty() {
            None
        } else {
            Some(Value::Int(self.0.remove(0)))
        })
    }

    fn redraw(&mut self, _vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        Ok(())
    }
}

/// Runs the script with the given `printc_count` and returns the finished
/// console lines as text.
fn run_train_menu(printc_count: usize) -> Vec<String> {
    let mut info = HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    };
    info.merge_name_csv("TRAIN", TRAIN_CSV).unwrap();
    // every command is COM_ABLE without a COM_ABLEn function
    info.merge_replace_csv("COM_ABLE初期値,1").unwrap();
    let header = Arc::new(info);

    let config = EraConfig {
        printc_width: 25,
        printc_count,
        ..Default::default()
    };
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(Scripted(vec![0])),
        "sav".into(),
    );

    let parser = ParserContext::new(header.clone(), StrKey::new("TRAIN_MENU.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser.parse_program_str(SCRIPT).unwrap() {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    let vm = TerminalVm::new(dic, header);
    let ok = vm.start(&mut tx, &mut ctx);
    let lines: Vec<String> = tx.lines.iter().map(ToString::to_string).collect();
    assert!(ok, "VM error:\n{}", lines.join("\n"));
    lines
}

/// One `#[test]` for all three cases: `erars_ast::init_interner()` is
/// process-global, so the cases must not run on separate test threads.
#[test]
fn train_menu_breaks_lines_every_printc_count_items() {
    erars_ast::init_interner();
    // PRINTC item `{name}[{no:3}]`, right-aligned in a 25-cell field
    let item = |s: &str| format!("{s:>25}");

    // 2 per line: A B / C D / E
    k9::assert_equal!(
        run_train_menu(2),
        vec![
            item("A[  0]") + &item("B[  1]"),
            item("C[  2]") + &item("D[  3]"),
            item("E[  4]"),
        ]
    );

    // Emuera default 3 per line: A B C / D E
    k9::assert_equal!(
        run_train_menu(3),
        vec![
            item("A[  0]") + &item("B[  1]") + &item("C[  2]"),
            item("D[  3]") + &item("E[  4]"),
        ]
    );

    // 0 disables the break: one line
    k9::assert_equal!(
        run_train_menu(0),
        vec![["A[  0]", "B[  1]", "C[  2]", "D[  3]", "E[  4]"]
            .into_iter()
            .map(item)
            .collect::<String>()]
    );
}
