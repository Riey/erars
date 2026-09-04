use std::sync::Arc;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use erars_ast::StrKey;
use erars_compiler::{HeaderInfo, ParserContext, ParserResult};
use serde::de::DeserializeOwned;

/// A `HeaderInfo` carrying the engine's built-in globals, the way
/// `erars-loader` starts one (`crates/erars-loader/src/lib.rs`).
pub fn header_with_globals() -> HeaderInfo {
    HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../crates/erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    }
}

pub fn get_ctx(file_path: impl AsRef<str>) -> ParserContext<'static> {
    let mut info = header_with_globals();

    info.merge_name_csv("FLAG", include_str!("../CSV/FLAG.CSV")).unwrap();
    info.merge_name_csv("BASE", include_str!("../CSV/BASE.CSV")).unwrap();
    info.merge_name_csv("TRAIN", include_str!("../CSV/TRAIN.CSV"))
        .unwrap();
    info.merge_name_csv("TFLAG", include_str!("../CSV/TFLAG.CSV"))
        .unwrap();
    info.merge_name_csv("ABL", include_str!("../CSV/ABL.CSV")).unwrap();
    info.merge_name_csv("TALENT", include_str!("../CSV/TALENT.CSV"))
        .unwrap();
    info.merge_name_csv("MARK", include_str!("../CSV/MARK.CSV")).unwrap();
    info.merge_name_csv("EXP", include_str!("../CSV/EXP.CSV")).unwrap();
    info.merge_name_csv("PALAM", include_str!("../CSV/PALAM.CSV"))
        .unwrap();
    // `CDFLAG` is the one variable with two index tables, one per dimension
    // (`GameData/ConstantData.cs:1019-1039`).
    info.merge_name_csv("CDFLAG1", include_str!("../CSV/CDFLAG1.CSV"))
        .unwrap();
    info.merge_name_csv("CDFLAG2", include_str!("../CSV/CDFLAG2.CSV"))
        .unwrap();
    info.merge_str_csv(include_str!("../CSV/STR.CSV")).unwrap();

    info.merge_item_csv(include_str!("../CSV/ITEM.CSV")).unwrap();
    info.merge_chara_csv(include_str!("../CSV/CHARA3.CSV")).unwrap();
    info.merge_replace_csv(include_str!("../CSV/_Replace.CSV")).unwrap();
    info.merge_rename_csv(include_str!("../CSV/_Rename.CSV")).unwrap();
    info.merge_variable_size_csv(include_str!("../CSV/VariableSize.CSV"))
        .unwrap();
    info.merge_header("#DEFINE TRUE 1").unwrap();
    // A `#DEFINE` with no body is still a definition, and `[IF]` only asks
    // whether the name is in the macro dictionary
    // (`GameData/IdentifierDictionary.cs:470-477`).
    info.merge_header("#DEFINE EMPTY_MACRO").unwrap();

    ParserContext::new(Arc::new(info), StrKey::new(file_path.as_ref()))
}

// `tests/test_util.rs` is included by every integration target, so a helper
// only some of them call is "never used" in the others. `run_tests.rs` calls
// this one; `wiki_coverage.rs` builds its programs from strings and does not.
#[allow(dead_code)]
#[track_caller]
pub fn do_test<'p, T: std::fmt::Debug + Eq + DeserializeOwned>(
    path: &str,
    f: fn(&ParserContext<'p>, &str) -> ParserResult<T>,
) -> T {
    erars_ast::init_interner();
    let source = std::fs::read_to_string(path).unwrap();

    let ctx = get_ctx(path);
    let mut files = SimpleFiles::new();
    let file_id = files.add(path, &source);

    match f(&ctx, &source) {
        Ok(ret) => ret,
        Err((err, span)) => {
            let diagnostic = Diagnostic::error()
                .with_code("E0001")
                .with_message("Compile ERROR")
                .with_labels(vec![
                    Label::primary(file_id, span).with_message(format!("{}", err))
                ]);
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = Config::default();
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                .unwrap();
            panic!("Test failed");
        }
    }
}
