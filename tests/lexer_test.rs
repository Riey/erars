use erars_compiler::PP_REGEX;
use erars_lexer::{Bump, Preprocessor};

#[test]
fn lex_test() {
    let default = Default::default();
    let mut pp = Preprocessor::new(&PP_REGEX, &default, include_str!("../ERB/SYSTEM.ERB"));

    let mut b = Bump::new();

    while let Some(line) = pp.next_line(&mut b).unwrap() {
        dbg!(line);
    }

    eprintln!("DONE");
}
