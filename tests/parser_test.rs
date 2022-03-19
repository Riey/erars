use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use erars_compiler::{parse_body, parse_expr, parse_function, ParserResult};
use serde::de::DeserializeOwned;

fn do_test<T: std::fmt::Debug + Eq + DeserializeOwned>(
    path: &str,
    f: fn(&str) -> ParserResult<T>,
    source: &str,
    expected: &str,
) {
    let expected: T = ron::from_str(expected).unwrap();

    let mut files = SimpleFiles::new();
    let file_id = files.add(path, source);

    match f(source) {
        Ok(ret) => {
            k9::assert_equal!(ret, expected);
        }
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

fn run_test_set<T: std::fmt::Debug + Eq + DeserializeOwned>(
    f: fn(&str) -> ParserResult<T>,
    pattern: &str,
) {
    let erb_files = glob::glob(pattern).unwrap();

    for erb_file in erb_files {
        let erb_file = erb_file.unwrap();
        let ron_file = erb_file.parent().unwrap().join(format!(
            "{}.ron",
            erb_file.file_stem().unwrap().to_str().unwrap()
        ));

        eprintln!("Check {}", erb_file.display());

        let erb_source = std::fs::read_to_string(&erb_file).unwrap();
        let ron_source = std::fs::read_to_string(ron_file).unwrap();

        do_test(
            erb_file.as_os_str().to_str().unwrap(),
            f,
            &erb_source,
            &ron_source,
        );
    }
}

#[test]
fn parse_test() {
    run_test_set(parse_expr, "tests/parse_tests/exprs/*.erb");
    run_test_set(parse_body, "tests/parse_tests/bodys/*.erb");
    run_test_set(parse_function, "tests/parse_tests/functions/*.erb");
}
