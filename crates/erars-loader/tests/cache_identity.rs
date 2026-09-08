//! `game.era`'s only identity token used to be `VERSION_MAGIC`, which names
//! the file *format*. Nothing recorded what the cache had been compiled
//! *from*, so `--load` accepted a cache built under a different config or from
//! since-edited sources and ran the stale program without a word — the
//! accept-and-misread failure, not a crash.
//!
//! `system_ignore_string_set` (`文字列変数の代入に文字列式を強制する`) is the
//! key used here because it decides what *parses at all*: with it on, a plain
//! `=` on a string variable is refused at parse time
//! (`GameProc/Function/ArgumentBuilder.cs:778`). A cache compiled with it off
//! therefore contains a function that the flipped config cannot even compile,
//! which is as far apart as two caches get.
//!
//! Everything lives in one `#[test]`, and the *accept* side is asserted
//! through `read_header` rather than a full `load_script`: decoding a cache
//! installs its identifier table into the process-global interner, which
//! `Interner::restore` requires to be untouched, so a successful load cannot
//! follow a compile in the same process. The refusal paths reach neither —
//! that is exactly the ordering `load_script` now guarantees.

use erars_compiler::EraConfig;
use erars_loader::{cache_fingerprint, load_script, run_script, save_script};
use erars_vm::NullSystemFunctions;

struct ScratchDir(std::path::PathBuf);

impl Drop for ScratchDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn fixture(name: &str, erb: &str) -> ScratchDir {
    let dir = std::env::temp_dir()
        .join(format!("erars-cache-identity-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("ERB")).unwrap();
    std::fs::write(dir.join("ERB/MAIN.ERB"), erb).unwrap();
    ScratchDir(dir)
}

#[test]
fn a_cache_is_not_reused_across_a_parse_affecting_config_change() {
    let dir = fixture("flip", "@SYSTEM_TITLE\nLOCALS = hello\nPRINTL done\n");
    let path = dir.0.to_str().unwrap().to_owned();

    let permissive = EraConfig::default();
    assert!(
        !permissive.system_ignore_string_set,
        "文字列変数の代入に文字列式を強制する defaults to NO (Config/ConfigData.cs:115)"
    );
    let mut strict = EraConfig::default();
    strict.system_ignore_string_set = true;

    // The two configs must be distinguishable at all, or nothing below means
    // anything.
    assert_ne!(
        cache_fingerprint(&path, &permissive).unwrap(),
        cache_fingerprint(&path, &strict).unwrap(),
        "a parse-affecting key must change the cache fingerprint"
    );

    // Compile and cache under the permissive config.
    let (vm, ctx, _tx) = run_script(
        &path,
        Box::new(NullSystemFunctions),
        permissive.clone(),
        false,
        false,
        false,
    )
    .expect("compile failed");
    save_script(vm, ctx, &path).expect("save_script");
    assert!(std::path::Path::new(&path).join("game.era").exists());

    // The accept side: the cache just written must fingerprint-match the
    // config it was written with, so `load_script` above reaches its decode
    // rather than the refusal. (A full `load_script` cannot run here; see the
    // module comment.)
    let stored = erars_bytecode::read_header(
        &std::fs::read(std::path::Path::new(&path).join("game.era")).unwrap()[..],
    )
    .expect("header reads back");
    assert_eq!(
        stored,
        cache_fingerprint(&path, &permissive).unwrap(),
        "an unchanged cache must be accepted, not refused"
    );

    // Flipping the key must refuse the cache rather than run it.
    let flipped = unsafe { load_script(&path, Box::new(NullSystemFunctions), strict) };
    let err = flipped
        .err()
        .expect("a cache compiled under a different parse-affecting config must be refused")
        .to_string();
    assert!(
        err.contains("stale"),
        "the refusal must say the cache is stale, got: {err:?}"
    );

    // And an edited source must refuse it too, config untouched. The
    // fingerprint mixes each file's length and path, not just its mtime, so
    // this does not depend on filesystem timestamp granularity.
    std::fs::write(
        dir.0.join("ERB/MAIN.ERB"),
        "@SYSTEM_TITLE\nLOCALS = hello\nPRINTL done\nPRINTL and more\n",
    )
    .unwrap();
    let edited = unsafe { load_script(&path, Box::new(NullSystemFunctions), permissive) };
    let err = edited
        .err()
        .expect("a cache compiled from since-edited sources must be refused")
        .to_string();
    assert!(err.contains("stale"), "got: {err:?}");
}
