//! A committed save file, written by pre-`5dac019` code, loaded through the
//! real `LOADDATA` path on today's `VariableInfo` shape.
//!
//! `5dac019` ("perf(ast): shrink VariableInfo from 64 to 48 bytes") changed
//! `VariableInfo::size` from `Vec<u32>` to `tinyvec::ArrayVec<[u32; 3]>` and
//! `VariableInfo::init` from `Vec<Expr>` to `Option<Box<[Expr]>>`. The second
//! change silently discarded every save-data variable whose `init` was empty,
//! because a plain derived `Deserialize` decoded an old save's empty `init`
//! array as `Some(Box::new([]))` rather than `None`, and `load_variables`
//! compares `VariableInfo`s for exact equality before restoring a value
//! (`crates/erars-vm/src/variable.rs`). `9046fbf` fixed it with the
//! `deserialize_init` helper right after the `VariableInfo` struct in
//! `crates/erars-ast/src/variable.rs`.
//!
//! Unit tests already cover that fix directly by hand-encoding an old-shape
//! struct (`crates/erars-vm/src/variable.rs::load_variables_tests`). This
//! test is the belt to that unit test's suspenders: it drives a save file
//! that pre-`5dac019` code *actually wrote*, byte for byte, through the real
//! `LOADDATA` builtin command, so a future change to `VariableInfo`'s wire
//! shape that the unit tests miss still has to survive this fixture.
//!
//! ## Provenance — do not regenerate this fixture
//!
//! `tests/fixtures/pre_5dac019_save00.rsav.gz` was produced by checking out
//! commit `c50ed3d` (`perf(vm): replace SmallVec local-variable table with
//! exact-size boxed slices`, the parent of `5dac019`) into a scratch
//! worktree and running the exact script in [`SCRIPT`] below through
//! `SAVEDATA 0, "pre-5dac019 fixture"`, using a `HeaderInfo` built the same
//! way [`run`] builds one here (`variable.yaml` plus nothing else).
//!
//! **Never refresh this file when `VariableInfo`'s shape changes.** Its
//! entire value is being frozen at a wire shape the current code no longer
//! writes; a fixture regenerated on every format change would always decode
//! trivially and would catch nothing. If this test ever needs a fixture with
//! different values, generate a *new* file from the same pre-`5dac019`
//! commit and add it — never overwrite this one.
//!
//! If this test fails after an intentional `VariableInfo` field change: that
//! is `VariableInfo`'s wire shape changing incompatibly with saves already
//! on disk. See `deserialize_init` in `crates/erars-ast/src/variable.rs` for
//! the precedent — the same absent-vs-empty divergence it guards against may
//! now exist on a different field, and it needs the same treatment before
//! this test (or a real player's save) can pass again.

use std::sync::Arc;

use erars_ast::{get_interner, StrKey};
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

/// Declares the same local `SAVEDATA` variables the fixture was written
/// with — one with a non-empty initialiser (`LOCALINIT`), one with none
/// (`LOCALNOINIT`) — then loads slot 0. Must match the script the fixture
/// was generated with exactly; see the provenance note above.
const SCRIPT: &str = r#"@SYSTEM_TITLE
#DIM SAVEDATA LOCALINIT = 555
#DIM SAVEDATA LOCALNOINIT, 1
LOADDATA 0
"#;

/// Copies the committed fixture into a fresh scratch save directory (named
/// `save00.rsav.gz`, as `LOADDATA 0` expects — see
/// `crates/erars-vm/src/save.rs::make_save_file_name`) and loads it through
/// the real VM, then hands back the `VariableStorage` to assert against.
fn load_fixture_through_vm() -> erars_vm::VariableStorage {
    erars_ast::init_interner();

    let info = HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    };
    let header = Arc::new(info);

    let sav_dir = std::env::temp_dir().join(format!(
        "erars-pre-5dac019-fixture-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir_all(&sav_dir).expect("create scratch sav dir");
    std::fs::copy(
        concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/pre_5dac019_save00.rsav.gz"),
        sav_dir.join("save00.rsav.gz"),
    )
    .expect("copy committed fixture into scratch sav dir");

    let config = Arc::new(EraConfig::default());
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        config,
        Box::new(NullSystemFunctions),
        sav_dir.clone(),
        "resources".into(),
    );

    let parser = ParserContext::new(header.clone(), StrKey::new("FIXTURE.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser.parse_program_str(SCRIPT).unwrap() {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    let vm = TerminalVm::new(dic, header);
    // Ignore the result: the `Shop` loop's input-exhaustion error (`Value is
    // empty`, since `NullSystemFunctions::input` always answers `None`)
    // fires only after `LOADDATA` has already restored every value below.
    let _ok = vm.start(&mut tx, &mut ctx);

    let _ = std::fs::remove_dir_all(&sav_dir);

    ctx.var
}

/// Loads the frozen pre-`5dac019` save and asserts each planted value
/// survives, naming the expected numbers explicitly:
///
/// - `FLAG:1` (global, built-in, `init` empty) — `111`
/// - `MONEY:0` (global, built-in, `init` empty) — `999`
/// - `LOCALINIT` (function-local to `SYSTEM_TITLE`, `init` non-empty) — `333`
/// - `LOCALNOINIT:0` (function-local to `SYSTEM_TITLE`, `init` empty) — `222`
///
/// If any of these come back as their default (`0`) instead, `VariableInfo`'s
/// wire shape has changed incompatibly with this frozen fixture — see the
/// module doc comment above for the precedent and what to do about it.
#[test]
fn a_pre_5dac019_save_restores_every_planted_value() {
    let mut var = load_fixture_through_vm();

    let flag = get_interner().get_or_intern_static("FLAG");
    let money = get_interner().get_or_intern_static("MONEY");
    let system_title = get_interner().get_or_intern_static("SYSTEM_TITLE");
    let local_init = get_interner().get_or_intern_static("LOCALINIT");
    let local_no_init = get_interner().get_or_intern_static("LOCALNOINIT");

    assert_eq!(
        var.read_int(flag, &[1]).unwrap(),
        111,
        "FLAG:1 (global, empty init) did not survive the pre-5dac019 load"
    );
    assert_eq!(
        var.read_int(money, &[0]).unwrap(),
        999,
        "MONEY:0 (global, empty init) did not survive the pre-5dac019 load"
    );

    let (_, local_init_var) = var.get_local_var(system_title, local_init).unwrap();
    assert_eq!(
        local_init_var.assume_normal().as_int().unwrap()[0],
        333,
        "LOCALINIT (SAVEDATA local, non-empty init) did not survive the pre-5dac019 load"
    );

    let (_, local_no_init_var) = var.get_local_var(system_title, local_no_init).unwrap();
    assert_eq!(
        local_no_init_var.assume_normal().as_int().unwrap()[0],
        222,
        "LOCALNOINIT (SAVEDATA local, empty init) did not survive the pre-5dac019 load"
    );
}
