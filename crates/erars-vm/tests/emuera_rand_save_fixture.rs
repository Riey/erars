//! Loads a `RANDDATA`-carrying save **actually written by real Emuera**
//! through the real `LOADDATA` builtin command, then draws two more
//! `RAND:100000` values through erars and checks they continue the exact
//! sequence real Emuera would have produced from this save — the
//! end-to-end proof that `crates/erars-vm/src/emuera_rand.rs` makes the
//! `RANDDATA` save round trip meaningful, not just that the generator
//! matches in isolation.
//!
//! ## Provenance
//!
//! `tests/fixtures/emuera_saves/real_rand/randcap90_real.sav` was produced
//! by actually running `Emuera1818_kr3.exe` (eraTHYMKR corpus) under wine.
//! See `tests/fixtures/emuera_saves/real_rand/README.md` for the exact ERB
//! source, the full expected `RANDCAP` array, and why the two trailing
//! values this test asserts (`60789`, `83791`) are exactly what real
//! Emuera produced continuing from this save's `RANDDATA`.
//!
//! This mirrors `emuera_save_fixture.rs`'s pattern (real-Emuera-shaped
//! `.sav` through the real `LOADDATA` path) with one addition: after
//! loading, it exercises `RAND` itself, not just plain variable restore.

use std::sync::Arc;

use erars_ast::{get_interner, StrKey, VariableInfo};
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = r#"@SYSTEM_TITLE
LOADDATA 90
"#;

/// Real `variable.yaml` already declares `RANDDATA` at its true Emuera
/// shape (`int[625]`, see `crates/erars-vm/src/emuera_rand.rs`), so the
/// only override this fixture needs is `RANDCAP` itself — the capture
/// game's own custom savedata array
/// (`tests/fixtures/emuera_saves/real_rand/README.md`'s `#DIM SAVEDATA
/// RANDCAP, 20`), which no built-in declares.
fn fixture_header() -> Arc<HeaderInfo> {
    erars_ast::init_interner();
    let mut global_variables: hashbrown::HashMap<StrKey, VariableInfo> =
        serde_yaml::from_str(include_str!("../../erars-loader/src/variable.yaml")).unwrap();
    let mut size = tinyvec::ArrayVec::<[u32; 3]>::new();
    size.push(20);
    global_variables.insert(
        get_interner().get_or_intern_static("RANDCAP"),
        VariableInfo { is_savedata: true, size, ..Default::default() },
    );
    Arc::new(HeaderInfo { global_variables, ..Default::default() })
}

/// Copies the committed fixture into a fresh scratch save directory as
/// `save90.sav` — real Emuera's own numbered-slot name — so `LOADDATA 90`
/// must fall back to the Emuera-compatibility read path to find it, exactly
/// like `emuera_save_fixture.rs`.
fn load_fixture_through_vm() -> erars_vm::VariableStorage {
    let header = fixture_header();

    let sav_dir = std::env::temp_dir().join(format!(
        "erars-emuera-rand-save-fixture-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir_all(&sav_dir).expect("create scratch sav dir");
    std::fs::copy(
        concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../tests/fixtures/emuera_saves/real_rand/randcap90_real.sav"
        ),
        sav_dir.join("save90.sav"),
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
    // Ignore the result: `NullSystemFunctions::input` always answers `None`,
    // so whatever runs after `LOADDATA` eventually fails on input
    // exhaustion — that fires only after `LOADDATA` has already restored
    // every value below.
    let _ok = vm.start(&mut tx, &mut ctx);

    let _ = std::fs::remove_dir_all(&sav_dir);

    ctx.var
}

/// `RANDCAP` (a plain int array, nothing RNG-specific about restoring it)
/// survives `LOADDATA` intact — the ordinary variable-reconciliation half
/// of this fixture, checked first so a failure here doesn't get confused
/// with the `RAND`-continuation assertion below.
#[test]
fn a_real_emuera_rand_capture_restores_randcap_array() {
    let mut var = load_fixture_through_vm();
    let randcap = get_interner().get_or_intern_static("RANDCAP");
    let expected: [i64; 15] =
        [0, 1, 67, 0, 2, 95565, 3394868286, 40, 18, -5, 4119290769, 60789, 83791, 60789, 83791];
    for (i, &want) in expected.iter().enumerate() {
        assert_eq!(
            var.read_int(randcap, &[i as u32]).unwrap(),
            want,
            "RANDCAP:{i} did not survive LOADDATA"
        );
    }
}

/// The actual save-compat proof: `LOADDATA 90` restores `RANDDATA` from
/// this real-Emuera-written file, and `VariableStorage::load_serializable`
/// always ends in `init_rand()` — exactly as if the script had run
/// `INITRAND` right after `LOADDATA`, per Emuera's own documented
/// `DUMPRAND`-before-save / `INITRAND`-after-load convention
/// (`docs/research/emuera-wiki/excom.md:1530`). Drawing two more
/// `RAND:100000`-equivalent values through erars must therefore continue
/// the *exact* sequence real Emuera would have produced from this save:
/// `60789, 83791` — the same two values the save's own `RANDCAP:11,12`
/// already recorded from the original run, now reproduced from nothing but
/// the loaded `RANDDATA` state.
#[test]
fn a_real_emuera_rand_capture_continues_the_saved_sequence() {
    let mut var = load_fixture_through_vm();
    assert_eq!(var.next_rand(100000), 60789, "first post-LOADDATA draw did not continue the saved sequence");
    assert_eq!(var.next_rand(100000), 83791, "second post-LOADDATA draw did not continue the saved sequence");
}
