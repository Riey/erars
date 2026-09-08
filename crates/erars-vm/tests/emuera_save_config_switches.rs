//! `system_save_in_binary` / `system_save_in_utf8` (emuera.config
//! `セーブデータをバイナリ形式で保存する` / `セーブデータをUTF-8で保存する`,
//! Emuera `SystemSaveInBinary` / `SystemSaveInUTF8`): already wired before
//! this session, mirroring real Emuera's own writer-selection logic
//! (`SAVEDATA_EMUERA`, `terminal_vm/executor.rs`
//! `BuiltinCommand::SaveDataEmuera`). This test only closes the coverage gap
//! - no existing test exercised the config-driven branch - it does not
//! change the wiring.
//!
//! Both keys default to `false` (`ConfigData.cs:114,120`: text container,
//! non-Unicode encoding), matching real Emuera and leaving erars's native
//! `.rsav.gz` format (`SAVEDATA`/`SAVEGLOBAL`) untouched either way.

use std::sync::Arc;

use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const BINARY_MAGIC: [u8; 8] = [0x89, 0x45, 0x52, 0x41, 0x0D, 0x0A, 0x1A, 0x0A];
const UTF8_BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];

/// Runs `SAVEDATA_EMUERA 0, "d"` under the given switches and returns the
/// written `save00.sav` bytes.
fn run_and_read_emuera_save(binary: bool, utf8: bool) -> Vec<u8> {
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
        system_save_in_binary: binary,
        system_save_in_utf8: utf8,
        ..Default::default()
    };
    let sav_dir = std::env::temp_dir().join(format!(
        "erars-emuera-save-switches-test-{}-{}-{binary}-{utf8}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos(),
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

    let parser = ParserContext::new(header.clone(), erars_ast::StrKey::new("SAVESWITCH.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser
        .parse_program_str("@SYSTEM_TITLE\nSAVEDATA_EMUERA 0, \"d\"\n")
        .unwrap()
    {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    let vm = TerminalVm::new(dic, header);
    let ok = vm.start(&mut tx, &mut ctx);
    assert!(ok, "VM error");

    let bytes = std::fs::read(sav_dir.join("save00.sav")).expect("SAVEDATA_EMUERA must write save00.sav");
    let _ = std::fs::remove_dir_all(&sav_dir);
    bytes
}

/// `erars_ast::init_interner()` is process-global, so all four cases must
/// run on the same test thread.
#[test]
fn system_save_in_binary_and_utf8_pick_the_emuera_writer_format() {
    // binary:false, utf8:false (both defaults) - plain-text, non-Unicode.
    let bytes = run_and_read_emuera_save(false, false);
    assert!(!bytes.starts_with(&BINARY_MAGIC), "default must not be binary");
    assert!(!bytes.starts_with(&UTF8_BOM), "default must not be UTF-8");

    // binary:false, utf8:true - plain-text, UTF-8 BOM.
    let bytes = run_and_read_emuera_save(false, true);
    assert!(!bytes.starts_with(&BINARY_MAGIC));
    assert!(bytes.starts_with(&UTF8_BOM), "utf8:true must emit a BOM");

    // binary:true - binary container regardless of the text-encoding switch.
    let bytes = run_and_read_emuera_save(true, false);
    assert!(bytes.starts_with(&BINARY_MAGIC), "binary:true must emit the binary magic");

    let bytes = run_and_read_emuera_save(true, true);
    assert!(bytes.starts_with(&BINARY_MAGIC), "binary:true must emit the binary magic even with utf8:true");
}
