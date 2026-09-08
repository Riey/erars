//! `system_ignore_triple_symbol` (emuera.config `FORM中の三連記号を展開し
//! ない`, Emuera `Config.SystemIgnoreTripleSymbol`): whether a FORM string's
//! run of three identical `*`/`+`/`=`/`/`/`$` characters expands into a
//! character-name shorthand. Default is `false` (`Config/ConfigData.cs:110`)
//! - expansion *on* - matching eramaker
//! (`docs/research/emuera-wiki/config.md:236`).
//!
//! The five shorthands and what each reads
//! (`GameData/StrForm.cs:22-50`,`:60-83`, `Sub/LexicalAnalyzer.cs:1203-
//! 1218`):
//! - `***` -> `NAME(TARGET)`
//! - `+++` -> `CALLNAME(MASTER)`
//! - `===` -> `CALLNAME(PLAYER)`
//! - `///` -> `NAME(ASSI)`
//! - `$$$` -> `CALLNAME(TARGET)`
//!
//! erars previously had no implementation of this shorthand at all - the
//! five characters were always literal FORM text, which is what
//! `SystemIgnoreTripleSymbol:YES` (Emuera's non-default) asks for, and is
//! also what a `false` gate value must *not* silently become.

use std::sync::Arc;

use erars_ast::StrKey;
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = concat!(
    "@SYSTEM_TITLE\n",
    "ADDVOIDCHARA\n",
    "ADDVOIDCHARA\n",
    "NAME:0 '= \"Alice\"\n",
    "NAME:1 '= \"Bob\"\n",
    "CALLNAME:0 '= \"Ally\"\n",
    "CALLNAME:1 '= \"Bobby\"\n",
    "TARGET = 0\n",
    "MASTER = 1\n",
    "PLAYER = 0\n",
    "ASSI = 1\n",
    "PRINTFORML ***\n",
    "PRINTFORML +++\n",
    "PRINTFORML ===\n",
    "PRINTFORML ///\n",
    "PRINTFORML $$$\n",
    // A run longer than three greedily consumes exactly three at a time
    // (`Sub/StringStream.cs:152-157`'s `TripleSymbol()` only ever peeks
    // three ahead), so a fourth `*` is left over as plain text rather than
    // folded into the shorthand or left whole and unexpanded.
    "PRINTFORML ****\n",
);

/// Runs [`SCRIPT`] with the given `system_ignore_triple_symbol` setting and
/// returns the five `PRINTFORML` output lines.
fn run_and_collect_lines(system_ignore_triple_symbol: bool) -> Vec<String> {
    erars_ast::init_interner();

    let header = Arc::new(HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    });

    let config = EraConfig {
        system_ignore_triple_symbol,
        ..Default::default()
    };
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(NullSystemFunctions),
        "sav".into(),
        "resources".into(),
    );

    let parser = ParserContext::new(header.clone(), StrKey::new("TRIPLE_SYMBOL_TEST.ERB"))
        .with_ignore_triple_symbol(system_ignore_triple_symbol);
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

    lines.iter().map(|l| l.trim().to_string()).collect()
}

/// `erars_ast::init_interner()` is process-global, so both cases must run on
/// the same test thread.
#[test]
fn system_ignore_triple_symbol_controls_form_shorthand_expansion() {
    let expanded = run_and_collect_lines(false);
    assert_eq!(
        expanded,
        vec!["Alice", "Bobby", "Ally", "Bob", "Ally", "Alice*"],
        "default (system_ignore_triple_symbol:NO) must expand *** +++ === /// $$$ to \
         NAME(TARGET)/CALLNAME(MASTER)/CALLNAME(PLAYER)/NAME(ASSI)/CALLNAME(TARGET), and a \
         four-character run must expand only its first three characters"
    );

    let literal = run_and_collect_lines(true);
    assert_eq!(
        literal,
        vec!["***", "+++", "===", "///", "$$$", "****"],
        "system_ignore_triple_symbol:YES must leave the triple-symbol runs as literal text"
    );
}
