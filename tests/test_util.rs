use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use erars_compiler::{ParserResult, VariableInterner};
use serde::de::DeserializeOwned;

#[track_caller]
pub fn do_test<T: std::fmt::Debug + Eq + DeserializeOwned>(
    path: &str,
    f: fn(&str, &mut VariableInterner) -> ParserResult<T>,
) -> T {
    let var = &mut VariableInterner::with_default_variables();
    let source = std::fs::read_to_string(path).unwrap();
    let mut files = SimpleFiles::new();
    let file_id = files.add(path, &source);

    match f(&source, var) {
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
