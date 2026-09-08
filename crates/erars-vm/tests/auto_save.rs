//! `auto_save` (emuera.config `オートセーブを行なう`, Emuera `Config.AutoSave`):
//! `BEGIN SHOP` autosaves right after `EVENTSHOP` returns, before the shop
//! menu is shown (`endCallEventShop`/`beginAutoSave`,
//! `GameProc/Process.SystemProc.cs:625-644`). Default is `true`
//! (`ConfigData.cs:47`).
//!
//! erars's `Workflow::Begin` always unwinds the whole call stack back to
//! `TerminalVm::start`'s dispatch loop before `run_begin` runs again
//! (`terminal_vm.rs:353`, `terminal_vm/executor.rs::run_begin`), so every
//! `BeginType::Shop` call is reached exactly the way Emuera's
//! `state.calledWhenNormal = true` case is - there is no nested/reentrant
//! `BEGIN` dispatch in erars to gate the other half of Emuera's condition on.
//!
//! Absent an `@SYSTEM_AUTOSAVE` override, the autosave lands in Emuera's own
//! reserved slot (`AutoSaveIndex = 99`, `:805`) via erars's native
//! `.rsav.gz` writer - the same one `SAVEGAME` uses - never the
//! `SAVEDATA_EMUERA` exporter.

use std::sync::Arc;

use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = "@SYSTEM_TITLE\nBEGIN SHOP\n";

/// Runs the script with the given `auto_save` setting in a scratch sav dir
/// and returns whether the autosave slot (99) exists afterward.
fn run_and_check_autosave_slot(auto_save: bool) -> bool {
    erars_ast::init_interner();

    let info = HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    };
    let header = Arc::new(info);

    let config = EraConfig {
        auto_save,
        ..Default::default()
    };
    let sav_dir = std::env::temp_dir().join(format!(
        "erars-auto-save-test-{}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos(),
        auto_save,
    ));
    let _ = std::fs::remove_dir_all(&sav_dir);

    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(NullSystemFunctions),
        sav_dir.to_str().unwrap().into(),
        "resources".into(),
    );

    let parser = ParserContext::new(header.clone(), erars_ast::StrKey::new("AUTOSAVE.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser.parse_program_str(SCRIPT).unwrap() {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    let vm = TerminalVm::new(dic, header);
    // The scripted run has no more input once the shop menu asks for a
    // choice, so `vm.start` returning `false` here is expected - the
    // autosave itself already ran, before the menu's first input request.
    let _ = vm.start(&mut tx, &mut ctx);

    let exists = sav_dir.join("save99.rsav.gz").exists();
    let _ = std::fs::remove_dir_all(&sav_dir);
    exists
}

/// `erars_ast::init_interner()` is process-global, so both cases must run on
/// the same test thread.
#[test]
fn auto_save_writes_slot_99_on_begin_shop_only_when_enabled() {
    assert!(
        run_and_check_autosave_slot(true),
        "auto_save:true must write the autosave slot on BEGIN SHOP"
    );
    assert!(
        !run_and_check_autosave_slot(false),
        "auto_save:false must not write the autosave slot on BEGIN SHOP"
    );
}
