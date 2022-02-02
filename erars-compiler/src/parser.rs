use crate::{PrintFlags, Stmt};

pub fn parse(s: &str) -> Vec<Stmt> {
    let lexer = crate::Lexer::new(s);
    crate::grammar::ProgramParser::new().parse(lexer).unwrap()
}

#[test]
fn hello_world() {
    k9::assert_equal!(
        parse("PRINTL Hello, world!"),
        &[Stmt::Print(
            PrintFlags::NEWLINE,
            "Hello, world!".to_string()
        )]
    );
}
