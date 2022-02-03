use crate::{Stmt, Function};

pub fn parse_function(s: &str) -> Function {
    let lexer = crate::Lexer::new(s);
    crate::grammar::FunctionParser::new().parse(lexer).unwrap()
}

pub fn parse_body(s: &str) -> Vec<Stmt> {
    let lexer = crate::Lexer::new(s);
    crate::grammar::BodyParser::new().parse(lexer).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_function() {
        k9::snapshot!(
            parse_function("@SYSTEM_TITLE\n#PRI\nPRINTL Hello, world!\n")
        );
    }

    #[test]
    fn hello_world() {
        k9::snapshot!(
            parse_body("PRINTL Hello, world!"),
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
            parse_body("PRINTFORML 1 + 1 = {1 + 1}"),
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
