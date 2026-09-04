use erars_lexer::{Bump, Preprocessor};

#[test]
fn lex_test() {
    let rename = Default::default();
    let macros = Default::default();
    let mut pp =
        Preprocessor::new_erb(&rename, &macros, false, include_str!("../ERB/SYSTEM.ERB"));

    let mut b = Bump::new();

    while let Some(line) = pp.next_line(&mut b).unwrap() {
        dbg!(line);
    }

    eprintln!("DONE");
}
