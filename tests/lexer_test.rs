use erars_compiler::PP_REGEX;
use erars_lexer::{Bump, Preprocessor};

#[test]
fn lex_test() {
    let mut pp = Preprocessor::new(&PP_REGEX, include_str!("../ERB/SYSTEM.ERB"));

    let mut b = Bump::new();

    while let Some(line) = pp.next_line(&mut b).unwrap() {
        dbg!(line);
    }

    eprintln!("DONE");
}
