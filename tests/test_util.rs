use std::sync::Arc;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use erars_compiler::{HeaderInfo, ParserContext, ParserResult};
use serde::de::DeserializeOwned;
use smol_str::SmolStr;

pub fn get_ctx(file_path: impl Into<SmolStr>) -> ParserContext {
    let mut info = HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!("../src/variable.yaml")).unwrap(),
        ..Default::default()
    };

    info.merge_name_csv("FLAG", include_str!("../CSV/FLAG.CSV"))
        .unwrap();

    ParserContext::new(Arc::new(info), file_path.into())
}

#[track_caller]
pub fn do_test<T: std::fmt::Debug + Eq + DeserializeOwned>(
    path: &str,
    f: fn(&ParserContext, &str) -> ParserResult<T>,
) -> T {
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
