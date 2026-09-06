//! Loads a real Emuera-shaped `.sav` file — the plain-text format written by
//! actual Emuera's `VariableEvaluator::SaveToStream`, not erars's own
//! `.rsav.gz` format — through the real `LOADDATA` builtin command, proving
//! the whole discovery-plus-parse-plus-reconcile pipeline
//! (`crate::save::read_save_data_slot` → `save::emuera::sniff`/`parse` →
//! `save::emuera::build_local_data`) works end to end, not just at the
//! `save::emuera` unit level.
//!
//! ## Provenance
//!
//! `tests/fixtures/emuera_saves/save01_text_utf8.sav` was hand-assembled
//! (by a peer session, cross-checked against real Emuera's IL bytecode —
//! see `docs/research/2026-09-06-emuera-save-format.md`) from the save
//! format's documented grammar; it was **not** captured from a live Emuera
//! run. It exercises every group shape the text format supports for a local
//! save: string/int scalars, string/int 1D, and — the one genuinely tricky
//! case — an int 2D chara array (`RELATION`) whose second row had a trailing
//! zero column trimmed by (simulated) Emuera's own writer, so this test also
//! proves `save::emuera::place_2d`'s row-major padding survives the real
//! `LOADDATA` path, not just direct unit-level calls.
//!
//! Between the header and `__EMUERA_1808_STRAT__` the file also carries a
//! placeholder OLD block (all-empty scalars/arrays, in
//! `VariableCode`-derived positional order) — real Emuera's own reader
//! (`VariableEvaluator::LoadFromStream`, `.il:108953-109155`) reads this
//! block unconditionally before ever looking for the extended-block marker,
//! so any file lacking it is not a shape real Emuera ever produces. Every
//! value this test actually checks (`DAY`, `MONEY`, `FLAG`, ...) still comes
//! from the extended block below it: the merge is last-write-wins by name,
//! so the extended block's real values simply overwrite the OLD block's
//! empty placeholders for every name they share.
//!
//! This test's `HeaderInfo` is built by hand to match the fixture's own
//! shapes (`RELATION` as int 2D `[2, 3]`) rather than reusing
//! `erars-loader/src/variable.yaml`'s real built-in shapes (where `RELATION`
//! is a flat 1D array of size 1000) — the fixture was written to exercise
//! the format, not to model any particular real game's `variable.yaml`. A
//! separate, non-automated check against an actual game's real declared
//! shapes (eraTHYMKR / eramegaten_p_kr) is the project's acceptance
//! criterion for this feature, not this fixture.

use std::sync::Arc;

use erars_ast::{get_interner, StrKey, VariableInfo};
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = r#"@SYSTEM_TITLE
LOADDATA 1
"#;

fn dims(values: &[u32]) -> tinyvec::ArrayVec<[u32; 3]> {
    let mut v = tinyvec::ArrayVec::<[u32; 3]>::new();
    for &d in values {
        v.push(d);
    }
    v
}

fn info(is_chara: bool, is_str: bool, size: &[u32]) -> VariableInfo {
    VariableInfo {
        is_chara,
        is_str,
        is_savedata: true,
        size: dims(size),
        ..Default::default()
    }
}

/// Declares the *real* engine-required variable set
/// (`erars-loader/src/variable.yaml`, the same header
/// `pre_5dac019_save_fixture.rs` uses — the VM needs its built-ins like
/// `PALAMLV`/`TARGET` to exist regardless of what this test cares about),
/// with deliberate overrides for the names this fixture's own worked
/// example (`docs/research/2026-09-06-emuera-save-format.md`) happens to
/// share with real Emuera built-ins of a different shape:
///
/// - `RELATION`: custom int 2D `[2, 3]` instead of the real flat 1D
///   `[1000]`, matching what `save01_text_utf8.sav` actually contains, so
///   this test can prove genuine 2D reconciliation succeeds through the
///   real `LOADDATA` path.
/// - `DAY`/`MONEY`: plain 0D int scalars instead of the real 1D `[1000]`,
///   since the fixture writes them in the text format's scalar group, not
///   its 1D-array group.
/// - `MES`: declared at all (real `variable.yaml` has no savedata `MES`),
///   as a 0D string scalar, matching how the fixture writes it.
///
/// A real game using only its own stock declarations for these names would
/// have `DAY`/`MONEY`/`RELATION` gracefully skipped as a `DimensionMismatch`
/// instead of restored — also correct, but not what this test is checking.
fn fixture_header() -> Arc<HeaderInfo> {
    erars_ast::init_interner();
    let mut global_variables: hashbrown::HashMap<StrKey, VariableInfo> =
        serde_yaml::from_str(include_str!("../../erars-loader/src/variable.yaml")).unwrap();
    let mut decl = |name: &'static str, v: VariableInfo| {
        global_variables.insert(get_interner().get_or_intern_static(name), v);
    };
    decl("RELATION", info(true, false, &[2, 3]));
    decl("DAY", info(false, false, &[]));
    decl("MONEY", info(false, false, &[]));
    decl("MES", info(false, true, &[]));
    Arc::new(HeaderInfo {
        global_variables,
        ..Default::default()
    })
}

/// Copies the committed fixture into a fresh scratch save directory as
/// `save01.sav` — real Emuera's own numbered-slot name
/// (`save::emuera_save_file_name`), not erars's `.rsav.gz` name — so
/// `LOADDATA 1` must fall back to the Emuera-compatibility read path to find
/// it at all.
fn load_fixture_through_vm() -> erars_vm::VariableStorage {
    let header = fixture_header();

    let sav_dir = std::env::temp_dir().join(format!(
        "erars-emuera-save-fixture-{}-{}",
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
            "/../../tests/fixtures/emuera_saves/save01_text_utf8.sav"
        ),
        sav_dir.join("save01.sav"),
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
    // so whatever runs after `LOADDATA` eventually fails on input exhaustion
    // — that fires only after `LOADDATA` has already restored every value
    // below.
    let _ok = vm.start(&mut tx, &mut ctx);

    let _ = std::fs::remove_dir_all(&sav_dir);

    ctx.var
}

#[test]
fn a_real_emuera_shaped_local_save_boots_through_loaddata() {
    let mut var = load_fixture_through_vm();

    let day = get_interner().get_or_intern_static("DAY");
    let money = get_interner().get_or_intern_static("MONEY");
    let flag = get_interner().get_or_intern_static("FLAG");
    let mes = get_interner().get_or_intern_static("MES");
    let savestr = get_interner().get_or_intern_static("SAVESTR");

    assert_eq!(var.read_int(day, &[]).unwrap(), 15, "DAY did not survive LOADDATA");
    assert_eq!(var.read_int(money, &[]).unwrap(), 100, "MONEY did not survive LOADDATA");
    assert_eq!(var.read_int(flag, &[0]).unwrap(), 1, "FLAG:0 did not survive LOADDATA");
    assert_eq!(var.read_int(flag, &[1]).unwrap(), 1, "FLAG:1 did not survive LOADDATA");
    assert_eq!(var.read_int(flag, &[2]).unwrap(), 0, "FLAG:2 did not survive LOADDATA");
    assert_eq!(var.read_str(mes, &[]).unwrap(), "こんにちは", "MES did not survive LOADDATA");
    assert_eq!(
        var.read_str(savestr, &[0]).unwrap(),
        "store",
        "SAVESTR:0 did not survive LOADDATA"
    );

    // Character-scope: one character was loaded from the file's
    // characterCount, carrying NICKNAME/NO/CSTR/CFLAG/RELATION.
    assert_eq!(var.character_len(), 1, "character section did not create exactly one character");

    let nickname = get_interner().get_or_intern_static("NICKNAME");
    let no = get_interner().get_or_intern_static("NO");
    let cstr = get_interner().get_or_intern_static("CSTR");
    let cflag = get_interner().get_or_intern_static("CFLAG");
    let relation = get_interner().get_or_intern_static("RELATION");

    assert_eq!(
        var.read_str(nickname, &[0]).unwrap(),
        "EmuChan",
        "NICKNAME did not survive LOADDATA"
    );
    assert_eq!(var.read_int(no, &[0]).unwrap(), 7, "NO did not survive LOADDATA");
    assert_eq!(
        var.read_str(cstr, &[0, 0]).unwrap(),
        "hello",
        "CSTR:0 did not survive LOADDATA"
    );
    assert_eq!(
        var.read_str(cstr, &[0, 1]).unwrap(),
        "bye",
        "CSTR:1 did not survive LOADDATA"
    );
    assert_eq!(var.read_int(cflag, &[0, 0]).unwrap(), 1, "CFLAG:0 did not survive LOADDATA");
    assert_eq!(var.read_int(cflag, &[0, 1]).unwrap(), 9, "CFLAG:1 did not survive LOADDATA");

    // RELATION: declared 2x3, file wrote rows "3,0,0" and "0,5" — the
    // second row's trimmed trailing zero column must come back padded.
    let expected = [[3, 0, 0], [0, 5, 0]];
    for row in 0..2u32 {
        for col in 0..3u32 {
            assert_eq!(
                var.read_int(relation, &[0, row, col]).unwrap(),
                expected[row as usize][col as usize],
                "RELATION[{row}][{col}] did not survive LOADDATA"
            );
        }
    }
}
