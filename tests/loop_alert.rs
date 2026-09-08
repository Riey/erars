//! Emuera's infinite-loop watchdog (`無限ループ警告までのミリ秒数`,
//! `InfiniteLoopAlertTime`, default 5000 ms — `Config/ConfigData.cs:71`).
//!
//! The trigger is engine logic a headless build can run; the response is not,
//! because Emuera's is a modal yes/no dialog (`GameProc/Process.cs:331-344`).
//! erars logs and continues, so these tests assert on the log.

mod test_util;

use std::sync::{Arc, LazyLock, Mutex};

use erars_compiler::{compile, EraConfig, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

/// `log` allows exactly one global logger, so every test in this file shares
/// this one and the tests below serialise on `RUN` to keep one run's warnings
/// out of another's assertion.
static WARNINGS: Mutex<Vec<String>> = Mutex::new(Vec::new());
static RUN: Mutex<()> = Mutex::new(());

struct Capture;

impl log::Log for Capture {
    fn enabled(&self, meta: &log::Metadata) -> bool {
        meta.level() <= log::Level::Warn
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            WARNINGS.lock().unwrap().push(record.args().to_string());
        }
    }

    fn flush(&self) {}
}

/// `log` allows one global logger per process, installed on first use.
static LOGGER: LazyLock<()> = LazyLock::new(|| {
    let _ = log::set_boxed_logger(Box::new(Capture));
    log::set_max_level(log::LevelFilter::Warn);
});

/// Runs `script` under `config` and returns the warnings it logged.
fn warnings_of(script: &str, config: EraConfig) -> Vec<String> {
    LazyLock::force(&LOGGER);
    let _guard = RUN.lock().unwrap();
    WARNINGS.lock().unwrap().clear();

    erars_ast::init_interner();
    let parser = ParserContext::new(
        test_util::get_ctx("LOOP_ALERT.ERB").header.try_as_arc().unwrap(),
        erars_ast::StrKey::new("LOOP_ALERT.ERB"),
    );
    let program = parser.parse_program_str(script).expect("script should parse");

    let root = std::env::temp_dir().join(format!("erars-loop-alert-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(root.join("sav")).unwrap();

    let header = parser.header.try_as_arc().unwrap();
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(NullSystemFunctions),
        root.join("sav"),
        root.join("resources"),
    );

    let mut dic = FunctionDic::new();
    for func in program {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    assert!(TerminalVm::new(dic, header).start(&mut tx, &mut ctx), "the script must finish");
    let _ = std::fs::remove_dir_all(&root);

    let out = WARNINGS.lock().unwrap().clone();
    out
}

/// A busy loop long enough for the watchdog to poll (its interval is Emuera's
/// 10,000, `GameProc/Process.ScriptProc.cs:23`) and to exceed a 1 ms limit.
const BUSY: &str = "\
@SYSTEM_TITLE
LOCAL = 0
WHILE LOCAL < 200000
	LOCAL += 1
WEND
PRINTL done
";

#[test]
fn a_long_run_without_input_warns_and_still_finishes() {
    let warnings = warnings_of(
        BUSY,
        {
            let mut config = EraConfig::default();
            config.infinite_loop_alert_time = 1;
            config
        },
    );

    let loop_warnings: Vec<_> =
        warnings.iter().filter(|w| w.contains("무한 루프 가능성")).collect();
    assert!(!loop_warnings.is_empty(), "expected a watchdog warning, got {warnings:?}");
    // The script ran to completion: erars never aborts on the watchdog, which
    // is where it deliberately departs from Emuera's modal dialog.
    assert!(loop_warnings[0].contains("SYSTEM_TITLE"), "got {loop_warnings:?}");
}

#[test]
fn zero_disables_the_watchdog() {
    // Emuera's `Config.InfiniteLoopAlertTime > 0` gate
    // (`GameProc/Process.ScriptProc.cs:24`): the same loop, no clock read, no
    // warning, however long it runs.
    let warnings = warnings_of(
        BUSY,
        {
            let mut config = EraConfig::default();
            config.infinite_loop_alert_time = 0;
            config
        },
    );

    assert!(
        !warnings.iter().any(|w| w.contains("무한 루프 가능성")),
        "the watchdog must not run when the key is 0, got {warnings:?}"
    );
}

#[test]
fn the_default_does_not_warn_about_a_short_run() {
    // 5000 ms is the shipped default (`Config/ConfigData.cs:71`), so an
    // ordinary script must never see this warning.
    let warnings = warnings_of(BUSY, EraConfig::default());

    assert!(
        !warnings.iter().any(|w| w.contains("무한 루프 가능성")),
        "got {warnings:?}"
    );
}
