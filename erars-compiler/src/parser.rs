use crate::Stmt;

pub fn parse(s: &str) -> Vec<Stmt> {
    let lexer = crate::Lexer::new(s);
    crate::grammar::ProgramParser::new().parse(lexer).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hello_world() {
        k9::snapshot!(
            parse("PRINTL Hello, world!"),
            r#"
[
    Print(
        NEWLINE,
        "Hello, world!",
    ),
]
"#
        );
    }

    #[test]
    fn form_simple() {
        k9::snapshot!(
            parse("PRINTFORML 1 + 1 = {1 + 1}"),
            r#"
[
    PrintForm(
        NEWLINE,
        "1 + 1 = ",
        [
            (
                BinopExpr(
                    IntLit(
                        1,
                    ),
                    Add,
                    IntLit(
                        1,
                    ),
                ),
                "",
            ),
        ],
    ),
]
"#
        );
    }
}
