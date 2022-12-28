use erars_compiler::PP_REGEX;
use erars_lexer::{Preprocessor, PreprocessorRegex};

#[test]
fn lex_test() {
    let mut pp = Preprocessor::new(&PP_REGEX, include_str!("../ERB/SYSTEM.ERB"));

    let mut line_buf = String::new();

    while let Some(line) = pp.next_line(&mut line_buf) {
        dbg!(line);
    }

    eprintln!("DONE");
}
