use std::sync::Arc;

use erars_compiler::{compile, EraConfig, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::*;
use flexi_logger::*;

mod test_util;

#[test]
fn run_test() {
    let _handle = Logger::try_with_str("trace")
        .unwrap()
        .rotate(
            Criterion::AgeOrSize(Age::Day, 1024 * 1024),
            Naming::Numbers,
            Cleanup::KeepLogFiles(5),
        )
        .log_to_file(FileSpec::default().directory("logs").basename("erars_test"))
        .write_mode(WriteMode::BufferAndFlush)
        .use_utc()
        .create_symlink("last_test_log.log")
        .start()
        .unwrap();

    erars_ast::init_interner();

    let erb_files = glob::glob("tests/run_tests/**/*.erb").unwrap();
    let header = test_util::get_ctx("").header.try_as_arc().unwrap();

    for erb_file in erb_files {
        let mut ctx = VmContext::new(
            header.clone(),
            Arc::new(EraConfig::from_text(include_str!("../emuera.config")).unwrap()),
            Box::new(NullSystemFunctions),
            "sav".into(),
        );
        let erb_file = erb_file.unwrap();
        let out_file = erb_file.parent().unwrap().join(format!(
            "{}.out",
            erb_file.file_stem().unwrap().to_str().unwrap()
        ));

        log::info!("Run {}", erb_file.display());

        let expected_ret = std::fs::read_to_string(out_file).unwrap();

        let program =
            test_util::do_test(erb_file.to_str().unwrap(), ParserContext::parse_program_str);
        let mut dic = FunctionDic::new();

        for func in program {
            dic.insert_compiled_func(
                &mut ctx.var,
                &ctx.header_info.default_local_size,
                compile(func).unwrap(),
            );
        }

        log::info!("FunctionDic: {dic:#?}");
        let ret = test_runner(dic, ctx);

        if ret != expected_ret {
            eprintln!("[x] {}", erb_file.display());
            k9::assert_equal!(ret, expected_ret);
        } else {
            eprintln!("[o] {}", erb_file.display());
        }
    }
}

fn test_runner(dic: FunctionDic, mut ctx: VmContext) -> String {
    let vm = TerminalVm::new(dic, ctx.header_info.clone());
    let mut tx = VirtualConsole::new(ctx.config.printc_width, ctx.config.max_log);

    let ok = vm.start(&mut tx, &mut ctx);

    // Check stack is empty if return success
    if ok {
        let leftover = ctx.return_func().unwrap().collect::<Vec<_>>();
        if !leftover.is_empty() {
            panic!("Function stack is not cleared: {leftover:?}");
        }
    }

    let mut out = String::new();

    use std::fmt::Write;
    for line in tx.lines_from(0).iter() {
        writeln!(out, "{}", line).unwrap();
    }

    writeln!(out, "{}", tx.last_line).unwrap();

    // Remove lastest newline
    out.pop();

    out
}
