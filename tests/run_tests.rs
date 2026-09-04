use std::path::Path;
use std::sync::Arc;

use erars_ast::Value;
use erars_compiler::{compile, EraConfig, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::*;
use flexi_logger::*;

mod test_util;

/// `<fixture dir>/emuera.config` when present, otherwise the repo-root
/// `emuera.config` (KOREAN). `tests/run_tests/jp/emuera.config` switches
/// that directory to JAPANESE.
fn fixture_config(erb_file: &Path) -> EraConfig {
    let local = erb_file.parent().unwrap().join("emuera.config");
    let text = match std::fs::read_to_string(&local) {
        Ok(text) => text,
        // Only "no per-directory config" falls back; a config that exists but
        // cannot be read is a broken fixture, not a KOREAN one.
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            include_str!("../emuera.config").to_owned()
        }
        Err(e) => panic!("reading {}: {e}", local.display()),
    };
    EraConfig::from_text(&text).unwrap()
}

/// A fixture with a sibling `<stem>.debug` marker file is parsed the way
/// Emuera's `-DEBUG` parses (`Program.cs:82-88`), so its `DEBUG_FUNC` lines
/// survive preprocessing instead of being dropped
/// (`Process.ScriptProc.cs:33-40`).
///
/// The marker is a separate file rather than a name convention because
/// `preprocessor/if_debug.erb` already ends in `_debug` and must keep running
/// in release mode — that is the whole point of it.
fn parse_fixture(erb_file: &Path) -> Vec<erars_ast::Function> {
    let path = erb_file.to_str().unwrap();
    if !erb_file.with_extension("debug").exists() {
        return test_util::do_test(path, ParserContext::parse_program_str);
    }

    let source = std::fs::read_to_string(path).unwrap();
    let ctx = test_util::get_ctx(path).with_debug(true);
    match ctx.parse_program_str(&source) {
        Ok(program) => program,
        Err((err, span)) => panic!("{path}: {err} at {span:?}"),
    }
}

/// Answers a fixture's input requests from a sibling `<stem>.in`, one line
/// per request.
///
/// [`NullSystemFunctions`] answers every request with `None`, which is fine
/// for a fixture that never asks but makes `INPUT`/`INPUTS` unreachable — and
/// the debug console can only be entered *as* an answer. A fixture without
/// the sidecar keeps the old behaviour exactly: `lines` stays empty and every
/// request is answered `None`.
struct FixtureInput {
    lines: std::collections::VecDeque<String>,
}

impl FixtureInput {
    fn new(erb_file: &Path) -> Self {
        let lines = match std::fs::read_to_string(erb_file.with_extension("in")) {
            Ok(text) => text.lines().map(str::to_owned).collect(),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Default::default(),
            Err(e) => panic!("reading {}.in: {e}", erb_file.display()),
        };
        Self { lines }
    }
}

impl SystemFunctions for FixtureInput {
    fn input(&mut self, req: erars_ui::InputRequest) -> anyhow::Result<Option<Value>> {
        let Some(line) = self.lines.pop_front() else {
            return Ok(None);
        };

        // The same seam `erars-stdio` provides: a line starting with `@` goes
        // to the engine verbatim whatever the request type, because only
        // `VmContext::input_redraw` can decide whether it is a console command
        // (`GameView/EmueraConsole.cs:1103-1110`).
        if !req.is_one && line.starts_with('@') {
            return Ok(Some(Value::String(line)));
        }

        Ok(match req.ty {
            erars_ui::InputRequestType::Int => Some(Value::Int(
                line.trim().parse().unwrap_or_else(|e| panic!("{line:?} is not an integer: {e}")),
            )),
            erars_ui::InputRequestType::Str => Some(Value::String(line)),
            _ => None,
        })
    }

    fn redraw(
        &mut self,
        _vconsole: &mut VirtualConsole,
        _painted: erars_vm::graphics::Painted<'_>,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

/// Where a fixture's `sav/` and `resources/` live.
///
/// Under the system temp directory rather than the repo, because `OUTPUTLOG`
/// and `@OUTPUT` write `emuera.log` into `sav_dir`'s *parent* — Emuera's
/// `WorkingDir` (`GameView/EmueraConsole.Print.cs:686-687`) — and a relative
/// `sav` would put that file in the checkout.
fn fixture_roots() -> (std::path::PathBuf, std::path::PathBuf) {
    let root = std::env::temp_dir().join("erars-run-tests");
    let sav = root.join("sav");
    std::fs::create_dir_all(&sav).unwrap();
    (sav, root.join("resources"))
}

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
    // `ERARS_FIXTURE=<substring>` narrows the sweep to matching fixtures, and a
    // leading `!` excludes them instead. The loop asserts on the first
    // mismatch, so without this a red fixture hides every fixture after it —
    // including, while a concurrent change is in flight, ones that have nothing
    // to do with it.
    let filter = std::env::var("ERARS_FIXTURE").unwrap_or_default();
    let (filter, exclude) = match filter.strip_prefix('!') {
        Some(rest) => (rest.to_owned(), true),
        None => (filter, false),
    };

    for erb_file in erb_files {
        let erb_file = erb_file.unwrap();
        if !filter.is_empty() && erb_file.to_string_lossy().contains(&filter) == exclude {
            continue;
        }
        let (sav, resources) = fixture_roots();
        let mut ctx = VmContext::new(
            header.clone(),
            Arc::new(fixture_config(&erb_file)),
            Box::new(FixtureInput::new(&erb_file)),
            sav,
            resources,
        );
        // The `.debug` marker is Emuera's `-Debug`, which gates the debug
        // window as well as the preprocessor (`Program.cs:219-220`).
        ctx.set_debug_mode(erb_file.with_extension("debug").exists());
        let out_file = erb_file.parent().unwrap().join(format!(
            "{}.out",
            erb_file.file_stem().unwrap().to_str().unwrap()
        ));

        log::info!("Run {}", erb_file.display());

        let expected_ret = std::fs::read_to_string(out_file).unwrap();

        let program = parse_fixture(&erb_file);
        let mut dic = FunctionDic::new();

        for func in program {
            dic.insert_compiled_func(
                &mut ctx.var,
                &ctx.header_info.default_local_size,
                compile(func).unwrap(),
            );
        }

        log::info!("FunctionDic: {dic:#?}");
        let (ret, reboot) = test_runner(dic, ctx);

        // `@REBOOT` is the only thing that raises this, and a `<stem>.reboot`
        // marker is how a fixture claims it. Asserted for every fixture, so a
        // command that set the flag by accident fails too.
        assert_eq!(
            reboot,
            erb_file.with_extension("reboot").exists(),
            "{}: VmContext::reboot_requested()",
            erb_file.display()
        );

        if ret != expected_ret {
            eprintln!("[x] {}", erb_file.display());
            k9::assert_equal!(ret, expected_ret);
        } else {
            eprintln!("[o] {}", erb_file.display());
        }
    }
}

fn test_runner(dic: FunctionDic, mut ctx: VmContext) -> (String, bool) {
    let vm = TerminalVm::new(dic, ctx.header_info.clone());
    let mut tx = VirtualConsole::new(&console_config(&ctx.config));

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

    // DEBUGPRINT*/DEBUGCLEAR write to Emuera's separate debug console, never to
    // the normal one (`EmueraConsole.cs:1837-1854`), so a fixture can only see
    // that output if it is rendered separately. Non-debug fixtures produce none
    // and are unaffected.
    if !tx.debug_lines().is_empty() {
        writeln!(out, "\n--- debug console ---").unwrap();
        for line in tx.debug_lines() {
            writeln!(out, "{line}").unwrap();
        }
        out.pop();
    }

    (out, ctx.reboot_requested())
}
