use erars_ast::{Alignment, PrintFlags};
use erars_lexer::{CallJumpInfo, ErbLexer, JumpType, PrintType, Token};

#[test]
fn lex_text() {
    macro_rules! test_case {
        ($text:expr, $tok:expr) => {
            let mut lex = ErbLexer::new($text);
            k9::assert_equal!(lex.next_token(|name| matches!(name, "ARGS" | "LOCALS")).unwrap(), Some($tok));
        };
        ($text:expr, $tok:expr, $span:expr) => {
            let mut lex = ErbLexer::new($text);

            k9::assert_equal!(lex.next_token(|name| matches!(name, "ARGS" | "LOCALS")).unwrap(), Some($tok));
            k9::assert_equal!(lex.span(), $span);
        };
    }

    test_case!("@FUNCTION", Token::FunctionLine("FUNCTION"));
    test_case!("  @BLANK  ", Token::FunctionLine("BLANK"), 2..10);
    test_case!("$LABEL", Token::LabelLine("LABEL"));
    // test_case!("#HEADER", Token::HeaderLine("HEADER"));
    test_case!("[[PREPROCESS]]", Token::PreprocessLine("PREPROCESS"));
    test_case!("ALIGNMENT left", Token::Alignment(Alignment::Left));
    test_case!("REUSElastLine", Token::ReuseLastLine(""));
    test_case!("REUSELASTLINE ABC", Token::ReuseLastLine("ABC"));
    test_case!(
        "priNTL Foo",
        Token::PrintLine(PrintFlags::NEWLINE, PrintType::Plain, "Foo")
    );
    test_case!(
        "CAlL Function",
        Token::CallLine(
            CallJumpInfo {
                ty: JumpType::Call,
                is_try: false,
                is_catch: false,
                is_form: false,
                is_method: false,
            },
            "Function"
        )
    );
    test_case!(
        "TRYCALLF Function",
        Token::CallLine(
            CallJumpInfo {
                ty: JumpType::Call,
                is_try: true,
                is_catch: false,
                is_form: false,
                is_method: true,
            },
            "Function"
        )
    );
}
