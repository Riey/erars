use crate::{Expr, Function, Stmt};

pub fn parse_program(s: &str) -> Vec<Function> {
    let lexer = crate::Lexer::new(s);
    crate::grammar::ProgramParser::new().parse(lexer).unwrap()
}

pub fn parse_function(s: &str) -> Function {
    let lexer = crate::Lexer::new(s);
    crate::grammar::FunctionParser::new().parse(lexer).unwrap()
}

pub fn parse_expr(s: &str) -> Expr {
    let lexer = crate::Lexer::new(s);
    crate::grammar::ExprParser::new().parse(lexer).unwrap()
}

pub fn parse_body(s: &str) -> Vec<Stmt> {
    let lexer = crate::Lexer::new(s);
    crate::grammar::BodyParser::new().parse(lexer).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paran_expr() {
        k9::snapshot!(
            parse_expr("1 + 2 ? 1 + 2 * 3 # (5+1) / 2"),
            "
CondExpr(
    BinopExpr(
        IntLit(
            1,
        ),
        Add,
        IntLit(
            2,
        ),
    ),
    BinopExpr(
        IntLit(
            1,
        ),
        Add,
        BinopExpr(
            IntLit(
                2,
            ),
            Mul,
            IntLit(
                3,
            ),
        ),
    ),
    BinopExpr(
        BinopExpr(
            IntLit(
                5,
            ),
            Add,
            IntLit(
                1,
            ),
        ),
        Div,
        IntLit(
            2,
        ),
    ),
)
"
        );
    }

    #[test]
    fn cond_expr() {
        k9::snapshot!(
            parse_expr("1 ? 2 # 3"),
            "
CondExpr(
    IntLit(
        1,
    ),
    IntLit(
        2,
    ),
    IntLit(
        3,
    ),
)
"
        );
    }

    #[test]
    fn parse_simple_function() {
        k9::snapshot!(
            parse_function("@SYSTEM_TITLE\n#PRI\nPRINTL Hello, world!\n"),
            r#"
Function {
    header: FunctionHeader {
        name: "SYSTEM_TITLE",
        infos: [
            EventFlag(
                Pre,
            ),
        ],
    },
    body: [
        Print(
            NEWLINE,
            "Hello, world!",
        ),
    ],
}
"#
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
