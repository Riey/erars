use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};
use erars_compiler::{parse_body, parse_expr, Expr, ParserResult, Stmt};
use serde::{de::DeserializeOwned, Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
enum Content {
    Expr(Expr),
    Stmt(Stmt),
}

#[derive(Serialize, Deserialize)]
struct ParseConfig {
    content: Content,
}

fn do_test<T: std::fmt::Debug + Eq + DeserializeOwned>(
    f: fn(&str) -> ParserResult<T>,
    source: &str,
    expected: &str,
) {
    let expected: T = ron::from_str(expected).unwrap();

    let mut files = SimpleFiles::new();
    let file_id = files.add("test.erb", source);

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
        }
    }
}

#[test]
fn parse() {
    let erb_files = glob::glob("tests/parse_tests/*.erb").unwrap();

    for erb_file in erb_files {
        let erb_file = erb_file.unwrap();

        eprintln!("Check {}", erb_file.display());

        let erb_source = std::fs::read_to_string(erb_file).unwrap();
        let mut lines = erb_source.lines();

        let ty = &lines.next().unwrap()[2..];
        let output = &lines.next().unwrap()[2..];

        match ty {
            "Body" => {
                do_test(parse_body, &erb_source, output);
            }
            "Expr" => {
                do_test(parse_expr, &erb_source, output);
            }
            other => panic!("Unknown parse type: {}", other),
        }
    }
}
