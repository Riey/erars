mod test_util;
mod body {
    use crate::test_util::do_test;
    use erars_compiler::ParserContext;

    #[test]
    fn test_alignment() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/alignment.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Alignment(
            Left,
        ),
        0..14,
    ),
    StmtWithPos(
        Alignment(
            Center,
        ),
        15..31,
    ),
    StmtWithPos(
        Alignment(
            Right,
        ),
        32..47,
    ),
    StmtWithPos(
        Print(
            NEWLINE,
            FormText(
                {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
            ),
        ),
        49..80,
    ),
    StmtWithPos(
        Print(
            NEWLINE,
            FormText(
                {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
            ),
        ),
        81..104,
    ),
]
"#
        );
    }

    #[test]
    fn test_assign() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/assign.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Assign(
            Variable {
                var: "COUNT",
                func_extern: None,
                args: [
                    BinopExpr(
                        Int(
                            1,
                        ),
                        Add,
                        Int(
                            3,
                        ),
                    ),
                ],
            },
            None,
            BinopExpr(
                Int(
                    23,
                ),
                Add,
                Int(
                    45,
                ),
            ),
        ),
        0..21,
    ),
]
"#
        );
    }

    #[test]
    fn test_assign_add() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/assign_add.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Assign(
            Variable {
                var: "FLAG",
                func_extern: None,
                args: [
                    Int(
                        13,
                    ),
                ],
            },
            Some(
                BitOr,
            ),
            Int(
                2,
            ),
        ),
        0..12,
    ),
]
"#
        );
    }

    #[test]
    fn test_assign_str() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/assign_str.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCALS",
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                {Int(123)}456,
            ),
        ),
        0..17,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCALS",
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                {Var(Variable { var: "LOCAL", func_extern: None, args: [Int(0)] })}.{Method("TOSTR", [Var(Variable { var: "LOCAL", func_extern: None, args: [Int(1)] }), String("00")])},
            ),
        ),
        18..58,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCALS",
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                {FormText({BinopExpr(Int(1), Add, Int(1))})},
            ),
        ),
        59..78,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "NICKNAME",
                func_extern: None,
                args: [
                    Var(
                        Variable {
                            var: "MASTER",
                            func_extern: None,
                            args: [],
                        },
                    ),
                ],
            },
            None,
            FormText(
                {CondExpr(Var(Variable { var: "TALENT", func_extern: None, args: [Var(Variable { var: "MASTER", func_extern: None, args: [] }), Int(120)] }), FormText(신사), FormText(숙녀))},
            ),
        ),
        79..138,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCALS",
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                ,
            ),
        ),
        139..147,
    ),
]
"#
        );
    }

    #[test]
    fn test_call() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/call.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Call {
            name: String(
                "FOO",
            ),
            args: [
                Int(
                    123,
                ),
                Var(
                    Variable {
                        var: "A",
                        func_extern: None,
                        args: [
                            Int(
                                634,
                            ),
                        ],
                    },
                ),
                String(
                    "123",
                ),
            ],
            is_jump: false,
            try_body: [],
            catch_body: None,
        },
        0..28,
    ),
]
"#
        );
    }

    #[test]
    fn test_command() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/command.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Command(
            CustomDrawLine,
            [],
        ),
        0..17,
    ),
]
"
        );
    }

    #[test]
    fn test_for() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/for.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        For(
            Variable {
                var: "COUNT",
                func_extern: None,
                args: [],
            },
            Int(
                0,
            ),
            Int(
                10,
            ),
            Int(
                1,
            ),
            [
                StmtWithPos(
                    PrintList(
                        NEWLINE,
                        [
                            Var(
                                Variable {
                                    var: "COUNT",
                                    func_extern: None,
                                    args: [],
                                },
                            ),
                        ],
                    ),
                    21..34,
                ),
            ],
        ),
        0..39,
    ),
]
"#
        );
    }

    #[test]
    fn test_hello() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/hello.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Print(
            NEWLINE,
            String(
                "Hello, world!",
            ),
        ),
        0..20,
    ),
]
"#
        );
    }

    #[test]
    fn test_if() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/if.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        If(
            [
                (
                    BinopExpr(
                        Var(
                            Variable {
                                var: "A",
                                func_extern: None,
                                args: [],
                            },
                        ),
                        Greater,
                        Int(
                            1,
                        ),
                    ),
                    [
                        StmtWithPos(
                            Print(
                                (empty),
                                String(
                                    "A > 1",
                                ),
                            ),
                            13..24,
                        ),
                    ],
                ),
                (
                    BinopExpr(
                        Var(
                            Variable {
                                var: "A",
                                func_extern: None,
                                args: [],
                            },
                        ),
                        Equal,
                        Int(
                            1,
                        ),
                    ),
                    [
                        StmtWithPos(
                            Print(
                                (empty),
                                String(
                                    "A == 1",
                                ),
                            ),
                            43..55,
                        ),
                    ],
                ),
            ],
            [
                StmtWithPos(
                    Print(
                        (empty),
                        String(
                            "A < 1",
                        ),
                    ),
                    65..76,
                ),
            ],
        ),
        0..82,
    ),
]
"#
        );
    }

    #[test]
    fn test_number() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/number.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            None,
            Int(
                1,
            ),
        ),
        0..7,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            None,
            Int(
                1234,
            ),
        ),
        8..18,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            None,
            Int(
                65535,
            ),
        ),
        19..31,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            None,
            Int(
                0,
            ),
        ),
        32..41,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            None,
            Int(
                3,
            ),
        ),
        42..51,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            None,
            UnaryopExpr(
                Int(
                    65535,
                ),
                Minus,
            ),
        ),
        52..66,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            None,
            UnaryopExpr(
                Int(
                    0,
                ),
                Minus,
            ),
        ),
        67..78,
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            None,
            UnaryopExpr(
                Int(
                    3,
                ),
                Minus,
            ),
        ),
        79..90,
    ),
]
"#
        );
    }

    #[test]
    fn test_print_data() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/print_data.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        PrintData(
            (empty),
            None,
            [
                [
                    String(
                        "data",
                    ),
                ],
                [
                    FormText(
                        LOCAL={Var(Variable { var: "LOCAL", func_extern: None, args: [] })},
                    ),
                ],
                [
                    String(
                        "list",
                    ),
                    String(
                        "form",
                    ),
                ],
            ],
        ),
        0..119,
    ),
]
"#
        );
    }

    #[test]
    fn test_print_simple() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/print_simple.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Print(
            NEWLINE,
            FormText(
                1 + 1 = {BinopExpr(Int(1), Add, Int(1))},
            ),
        ),
        0..26,
    ),
    StmtWithPos(
        Print(
            NEWLINE | WAIT,
            FormText(
                {Method("조사처리", [Var(Variable { var: "CALLNAME", func_extern: None, args: [Method("GET_CHARA_M", [])] }), String("와")])}같이 온 걸 보니, 단단히 각오하고 온 것 같다,
            ),
        ),
        27..143,
    ),
    StmtWithPos(
        Print(
            NEWLINE,
            FormText(
                {Var(Variable { var: "CALLNAME", func_extern: None, args: [Var(Variable { var: "ARG", func_extern: None, args: [] })] })}의 교습 관찰 결과 완료  결과：임시 성과치 {Var(Variable { var: "LOCAL", func_extern: None, args: [Int(0)] })}에 의한 실제 성과치 {Var(Variable { var: "LOCAL", func_extern: None, args: [Int(2)] })}증가⇒{CondExpr(BinopExpr(Var(Variable { var: "LOCAL", func_extern: None, args: [Int(1)] }), Equal, Int(1)), FormText(성공), FormText(실패))}({Var(Variable { var: "CFLAG", func_extern: None, args: [Var(Variable { var: "ARG", func_extern: None, args: [] }), Int(693)] })}％) 작업 내용：{Var(Variable { var: "CFLAG", func_extern: None, args: [Var(Variable { var: "ARG", func_extern: None, args: [] }), Int(690)] })},
            ),
        ),
        144..372,
    ),
    StmtWithPos(
        Print(
            NEWLINE | WAIT,
            FormText(
                보지에서 애액을 흘렸{CondExpr(BinopExpr(Var(Variable { var: "TEQUIP", func_extern: None, args: [Int(42)] }), Equal, Int(0)), FormText(고, 작은 한숨을 토해냈), String(""))}다.,
            ),
        ),
        373..471,
    ),
]
"#
        );
    }

    #[test]
    fn test_selectcase() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/selectcase.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        SelectCase(
            Int(
                1,
            ),
            [
                (
                    [
                        Single(
                            Int(
                                0,
                            ),
                        ),
                    ],
                    [
                        StmtWithPos(
                            Print(
                                (empty),
                                String(
                                    "FOO",
                                ),
                            ),
                            32..41,
                        ),
                    ],
                ),
                (
                    [
                        To(
                            Int(
                                1,
                            ),
                            Int(
                                2,
                            ),
                        ),
                    ],
                    [
                        StmtWithPos(
                            Print(
                                (empty),
                                String(
                                    "BAR",
                                ),
                            ),
                            66..75,
                        ),
                    ],
                ),
            ],
            Some(
                [
                    StmtWithPos(
                        Print(
                            (empty),
                            String(
                                "BAZ",
                            ),
                        ),
                        97..106,
                    ),
                ],
            ),
        ),
        0..116,
    ),
]
"#
        );
    }

    #[test]
    fn test_sif() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/sif.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Sif(
            Int(
                12,
            ),
            StmtWithPos(
                Print(
                    (empty),
                    String(
                        "45",
                    ),
                ),
                11..19,
            ),
        ),
        0..19,
    ),
    StmtWithPos(
        Sif(
            BinopExpr(
                Var(
                    Variable {
                        var: "LOCALS",
                        func_extern: None,
                        args: [],
                    },
                ),
                NotEqual,
                String(
                    "",
                ),
            ),
            StmtWithPos(
                Print(
                    NEWLINE,
                    FormText(
                        {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
                    ),
                ),
                42..61,
            ),
        ),
        20..61,
    ),
    StmtWithPos(
        Print(
            (empty),
            String(
                "32",
            ),
        ),
        63..71,
    ),
]
"#
        );
    }

    #[test]
    fn test_times() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/times.erb"#,
                ParserContext::parse_body_str
            ),
            r#"
[
    StmtWithPos(
        Times(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [],
            },
            NotNan(
                123.33,
            ),
        ),
        0..20,
    ),
]
"#
        );
    }
}
mod expr {
    use crate::test_util::do_test;
    use erars_compiler::ParserContext;

    #[test]
    fn test_boolean() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/boolean.erb"#,
                ParserContext::parse_expr_str
            ),
            r#"
BinopExpr(
    BinopExpr(
        Var(
            Variable {
                var: "RESULT",
                func_extern: None,
                args: [],
            },
        ),
        Equal,
        Int(
            0,
        ),
    ),
    And,
    BinopExpr(
        Var(
            Variable {
                var: "TALENT",
                func_extern: None,
                args: [
                    Var(
                        Variable {
                            var: "MASTER",
                            func_extern: None,
                            args: [],
                        },
                    ),
                    Int(
                        998,
                    ),
                ],
            },
        ),
        Equal,
        Int(
            0,
        ),
    ),
)
"#
        );
    }

    #[test]
    fn test_complex_op() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/complex_op.erb"#,
                ParserContext::parse_expr_str
            ),
            r#"
BinopExpr(
    BinopExpr(
        Int(
            50,
        ),
        Mul,
        BinopExpr(
            Int(
                6,
            ),
            Sub,
            Var(
                Variable {
                    var: "ABL",
                    func_extern: None,
                    args: [
                        Var(
                            Variable {
                                var: "ARG",
                                func_extern: None,
                                args: [],
                            },
                        ),
                        Int(
                            10,
                        ),
                    ],
                },
            ),
        ),
    ),
    Add,
    BinopExpr(
        Var(
            Variable {
                var: "RAND",
                func_extern: None,
                args: [
                    Int(
                        10,
                    ),
                ],
            },
        ),
        Mul,
        Int(
            5,
        ),
    ),
)
"#
        );
    }

    #[test]
    fn test_cond() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/cond.erb"#,
                ParserContext::parse_expr_str
            ),
            "
CondExpr(
    Int(
        1,
    ),
    Int(
        2,
    ),
    Int(
        3,
    ),
)
"
        );
    }

    #[test]
    fn test_csv() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/csv.erb"#,
                ParserContext::parse_expr_str
            ),
            r#"
Var(
    Variable {
        var: "FLAG",
        func_extern: None,
        args: [
            Int(
                1,
            ),
        ],
    },
)
"#
        );
    }

    #[test]
    fn test_method() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/method.erb"#,
                ParserContext::parse_expr_str
            ),
            r#"
Method(
    "FOO",
    [
        Int(
            123,
        ),
        String(
            "BAR",
        ),
        Var(
            Variable {
                var: "LOCAL",
                func_extern: None,
                args: [
                    Int(
                        123,
                    ),
                ],
            },
        ),
    ],
)
"#
        );
    }

    #[test]
    fn test_plus() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/plus.erb"#,
                ParserContext::parse_expr_str
            ),
            "
BinopExpr(
    Int(
        1,
    ),
    Add,
    Int(
        1,
    ),
)
"
        );
    }

    #[test]
    fn test_plus_mul() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/plus_mul.erb"#,
                ParserContext::parse_expr_str
            ),
            "
BinopExpr(
    Int(
        1,
    ),
    Add,
    BinopExpr(
        Int(
            2,
        ),
        Mul,
        Int(
            3,
        ),
    ),
)
"
        );
    }

    #[test]
    fn test_plus_mul_paran() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/plus_mul_paran.erb"#,
                ParserContext::parse_expr_str
            ),
            "
BinopExpr(
    BinopExpr(
        Int(
            1,
        ),
        Add,
        Int(
            2,
        ),
    ),
    Mul,
    Int(
        3,
    ),
)
"
        );
    }

    #[test]
    fn test_str_literal() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/str_literal.erb"#,
                ParserContext::parse_expr_str
            ),
            r#"
String(
    "",
)
"#
        );
    }

    #[test]
    fn test_var_arg() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/var_arg.erb"#,
                ParserContext::parse_expr_str
            ),
            r#"
Var(
    Variable {
        var: "COUNT",
        func_extern: None,
        args: [
            Int(
                123,
            ),
        ],
    },
)
"#
        );
    }

    #[test]
    fn test_var_complex() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/var_complex.erb"#,
                ParserContext::parse_expr_str
            ),
            r#"
Var(
    Variable {
        var: "COUNT",
        func_extern: None,
        args: [
            Var(
                Variable {
                    var: "A",
                    func_extern: None,
                    args: [
                        Int(
                            123,
                        ),
                    ],
                },
            ),
            Int(
                123,
            ),
        ],
    },
)
"#
        );
    }

    #[test]
    fn test_var_empty() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/var_empty.erb"#,
                ParserContext::parse_expr_str
            ),
            r#"
Var(
    Variable {
        var: "COUNT",
        func_extern: None,
        args: [],
    },
)
"#
        );
    }
}
mod function {
    use crate::test_util::do_test;
    use erars_compiler::ParserContext;

    #[test]
    fn test_call() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/functions/call.erb"#,
                ParserContext::parse_function_str
            ),
            r#"
Function {
    header: FunctionHeader {
        file_path: "",
        name: "FOO",
        args: [],
        infos: [],
    },
    body: [
        StmtWithPos(
            Assign(
                Variable {
                    var: "LOCALS",
                    func_extern: None,
                    args: [],
                },
                None,
                FormText(
                    LABEL,
                ),
            ),
            6..20,
        ),
        StmtWithPos(
            Goto {
                label: String(
                    "LABEL",
                ),
                catch_body: None,
            },
            22..33,
        ),
        StmtWithPos(
            Goto {
                label: FormText(
                    {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
                ),
                catch_body: None,
            },
            33..51,
        ),
        StmtWithPos(
            Goto {
                label: FormText(
                    {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
                ),
                catch_body: Some(
                    [],
                ),
            },
            51..72,
        ),
        StmtWithPos(
            Goto {
                label: FormText(
                    {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
                ),
                catch_body: Some(
                    [
                        StmtWithPos(
                            Print(
                                NEWLINE,
                                String(
                                    "CATCH",
                                ),
                            ),
                            100..112,
                        ),
                    ],
                ),
            },
            72..121,
        ),
        StmtWithPos(
            Call {
                name: String(
                    "BAR",
                ),
                args: [],
                is_jump: false,
                try_body: [],
                catch_body: None,
            },
            123..132,
        ),
        StmtWithPos(
            Call {
                name: String(
                    "BAR",
                ),
                args: [],
                is_jump: false,
                try_body: [],
                catch_body: None,
            },
            132..144,
        ),
        StmtWithPos(
            Call {
                name: String(
                    "BAR",
                ),
                args: [],
                is_jump: false,
                try_body: [],
                catch_body: None,
            },
            144..171,
        ),
        StmtWithPos(
            Call {
                name: String(
                    "BAR",
                ),
                args: [],
                is_jump: true,
                try_body: [],
                catch_body: None,
            },
            172..181,
        ),
        StmtWithPos(
            Call {
                name: String(
                    "BAR",
                ),
                args: [],
                is_jump: true,
                try_body: [],
                catch_body: None,
            },
            181..193,
        ),
        StmtWithPos(
            Call {
                name: FormText(
                    BAR,
                ),
                args: [],
                is_jump: true,
                try_body: [],
                catch_body: None,
            },
            193..224,
        ),
        StmtWithPos(
            Label(
                "LABEL",
            ),
            226..233,
        ),
    ],
}
"#
        );
    }

    #[test]
    fn test_dim() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/functions/dim.erb"#,
                ParserContext::parse_function_str
            ),
            r#"
Function {
    header: FunctionHeader {
        file_path: "",
        name: "SYSTEM_TITLE",
        args: [],
        infos: [
            Dim(
                LocalVariable {
                    var: "FOO",
                    info: VariableInfo {
                        is_chara: false,
                        is_str: false,
                        is_global: false,
                        default_int: 0,
                        size: [],
                        init: [
                            Int(
                                2,
                            ),
                        ],
                    },
                },
            ),
        ],
    },
    body: [
        StmtWithPos(
            PrintList(
                (empty),
                [
                    Var(
                        Variable {
                            var: "FOO",
                            func_extern: None,
                            args: [],
                        },
                    ),
                ],
            ),
            27..37,
        ),
    ],
}
"#
        );
    }

    #[test]
    fn test_function() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/functions/function.erb"#,
                ParserContext::parse_function_str
            ),
            r#"
Function {
    header: FunctionHeader {
        file_path: "",
        name: "FOO",
        args: [],
        infos: [
            EventFlag(
                Pre,
            ),
        ],
    },
    body: [
        StmtWithPos(
            Print(
                NEWLINE,
                String(
                    "Hello",
                ),
            ),
            10..22,
        ),
        StmtWithPos(
            Print(
                NEWLINE,
                FormText(
                    {Int(123)},
                ),
            ),
            23..39,
        ),
    ],
}
"#
        );
    }

    #[test]
    fn test_juel() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/functions/juel.erb"#,
                ParserContext::parse_function_str
            ),
            r#"
Function {
    header: FunctionHeader {
        file_path: "",
        name: "COMMON_MOVE_JUEL",
        args: [
            (
                Variable {
                    var: "ARG",
                    func_extern: None,
                    args: [],
                },
                None,
            ),
            (
                Variable {
                    var: "ARG",
                    func_extern: None,
                    args: [
                        Int(
                            1,
                        ),
                    ],
                },
                None,
            ),
            (
                Variable {
                    var: "ARG",
                    func_extern: None,
                    args: [
                        Int(
                            2,
                        ),
                    ],
                },
                None,
            ),
            (
                Variable {
                    var: "ARG",
                    func_extern: None,
                    args: [
                        Int(
                            3,
                        ),
                    ],
                },
                None,
            ),
            (
                Variable {
                    var: "ARG",
                    func_extern: None,
                    args: [
                        Int(
                            4,
                        ),
                    ],
                },
                None,
            ),
        ],
        infos: [],
    },
    body: [
        StmtWithPos(
            Assign(
                Variable {
                    var: "LOCAL",
                    func_extern: None,
                    args: [
                        Int(
                            1,
                        ),
                    ],
                },
                None,
                Method(
                    "LIMIT",
                    [
                        BinopExpr(
                            Var(
                                Variable {
                                    var: "JUEL",
                                    func_extern: None,
                                    args: [
                                        Var(
                                            Variable {
                                                var: "ARG",
                                                func_extern: None,
                                                args: [],
                                            },
                                        ),
                                        Var(
                                            Variable {
                                                var: "ARG",
                                                func_extern: None,
                                                args: [
                                                    Int(
                                                        1,
                                                    ),
                                                ],
                                            },
                                        ),
                                    ],
                                },
                            ),
                            Add,
                            Var(
                                Variable {
                                    var: "ARG",
                                    func_extern: None,
                                    args: [
                                        Int(
                                            2,
                                        ),
                                    ],
                                },
                            ),
                        ),
                        Int(
                            0,
                        ),
                        BinopExpr(
                            UnaryopExpr(
                                Int(
                                    62,
                                ),
                                Minus,
                            ),
                            Sub,
                            Int(
                                1,
                            ),
                        ),
                    ],
                ),
            ),
            307..358,
        ),
        StmtWithPos(
            Assign(
                Variable {
                    var: "LOCAL",
                    func_extern: None,
                    args: [
                        Int(
                            2,
                        ),
                    ],
                },
                None,
                BinopExpr(
                    Var(
                        Variable {
                            var: "LOCAL",
                            func_extern: None,
                            args: [
                                Int(
                                    1,
                                ),
                            ],
                        },
                    ),
                    Sub,
                    Var(
                        Variable {
                            var: "JUEL",
                            func_extern: None,
                            args: [
                                Var(
                                    Variable {
                                        var: "ARG",
                                        func_extern: None,
                                        args: [],
                                    },
                                ),
                                Var(
                                    Variable {
                                        var: "ARG",
                                        func_extern: None,
                                        args: [
                                            Int(
                                                1,
                                            ),
                                        ],
                                    },
                                ),
                            ],
                        },
                    ),
                ),
            ),
            382..416,
        ),
        StmtWithPos(
            Assign(
                Variable {
                    var: "LOCALS",
                    func_extern: None,
                    args: [],
                },
                None,
                FormText(
                    {Var(Variable { var: "PALAMNAME", func_extern: None, args: [Var(Variable { var: "ARG", func_extern: None, args: [Int(1)] })] })}의 구슬{CondExpr(BinopExpr(BinopExpr(Var(Variable { var: "ARG", func_extern: None, args: [Int(4)] }), Sub, BinopExpr(Var(Variable { var: "ARG", func_extern: None, args: [] }), NotEqual, Var(Variable { var: "TARGET", func_extern: None, args: [] }))), LessOrEqual, Int(0)), FormText(({Var(Variable { var: "CALLNAME", func_extern: None, args: [Var(Variable { var: "ARG", func_extern: None, args: [] })] })})), FormText())} {CondExpr(BinopExpr(Method("SIGN", [Var(Variable { var: "LOCAL", func_extern: None, args: [Int(2)] })]), Equal, Int(1)), FormText(＋), FormText(－))} {Method("ABS", [Var(Variable { var: "LOCAL", func_extern: None, args: [Int(2)] })])},
                ),
            ),
            556..701,
        ),
        StmtWithPos(
            Assign(
                Variable {
                    var: "JUEL",
                    func_extern: None,
                    args: [
                        Var(
                            Variable {
                                var: "ARG",
                                func_extern: None,
                                args: [],
                            },
                        ),
                        Var(
                            Variable {
                                var: "ARG",
                                func_extern: None,
                                args: [
                                    Int(
                                        1,
                                    ),
                                ],
                            },
                        ),
                    ],
                },
                None,
                Var(
                    Variable {
                        var: "LOCAL",
                        func_extern: None,
                        args: [
                            Int(
                                1,
                            ),
                        ],
                    },
                ),
            ),
            727..753,
        ),
        StmtWithPos(
            If(
                [
                    (
                        BinopExpr(
                            Method(
                                "ABS",
                                [
                                    Var(
                                        Variable {
                                            var: "LOCAL",
                                            func_extern: None,
                                            args: [
                                                Int(
                                                    2,
                                                ),
                                            ],
                                        },
                                    ),
                                ],
                            ),
                            Greater,
                            Int(
                                0,
                            ),
                        ),
                        [
                            StmtWithPos(
                                SelectCase(
                                    Var(
                                        Variable {
                                            var: "ARG",
                                            func_extern: None,
                                            args: [
                                                Int(
                                                    3,
                                                ),
                                            ],
                                        },
                                    ),
                                    [
                                        (
                                            [
                                                Single(
                                                    Int(
                                                        0,
                                                    ),
                                                ),
                                            ],
                                            [
                                                StmtWithPos(
                                                    Print(
                                                        NEWLINE,
                                                        Var(
                                                            Variable {
                                                                var: "LOCALS",
                                                                func_extern: None,
                                                                args: [],
                                                            },
                                                        ),
                                                    ),
                                                    902..916,
                                                ),
                                                StmtWithPos(
                                                    Command(
                                                        Return,
                                                        [
                                                            Int(
                                                                1,
                                                            ),
                                                        ],
                                                    ),
                                                    929..938,
                                                ),
                                            ],
                                        ),
                                        (
                                            [
                                                Single(
                                                    Int(
                                                        1,
                                                    ),
                                                ),
                                            ],
                                            [
                                                StmtWithPos(
                                                    Print(
                                                        NEWLINE | WAIT,
                                                        Var(
                                                            Variable {
                                                                var: "LOCALS",
                                                                func_extern: None,
                                                                args: [],
                                                            },
                                                        ),
                                                    ),
                                                    991..1005,
                                                ),
                                                StmtWithPos(
                                                    Command(
                                                        Return,
                                                        [
                                                            Int(
                                                                1,
                                                            ),
                                                        ],
                                                    ),
                                                    1018..1027,
                                                ),
                                            ],
                                        ),
                                    ],
                                    Some(
                                        [
                                            StmtWithPos(
                                                Command(
                                                    Return,
                                                    [
                                                        Int(
                                                            0,
                                                        ),
                                                    ],
                                                ),
                                                1085..1094,
                                            ),
                                        ],
                                    ),
                                ),
                                829..1107,
                            ),
                        ],
                    ),
                ],
                [],
            ),
            805..1113,
        ),
    ],
}
"#
        );
    }
}
mod program {
    use crate::test_util::do_test;
    use erars_compiler::ParserContext;

    #[test]
    fn test_call_form() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/programs/call_form.erb"#,
                ParserContext::parse_program_str
            ),
            r#"
[
    Function {
        header: FunctionHeader {
            file_path: "",
            name: "SYSTEM_TITLE",
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Call {
                    name: FormText(
                        FOO_{Int(123)},
                    ),
                    args: [
                        Int(
                            345,
                        ),
                    ],
                    is_jump: false,
                    try_body: [],
                    catch_body: None,
                },
                14..38,
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            file_path: "",
            name: "FOO_123",
            args: [
                (
                    Variable {
                        var: "ARG",
                        func_extern: None,
                        args: [],
                    },
                    None,
                ),
            ],
            infos: [],
        },
        body: [
            StmtWithPos(
                Print(
                    (empty),
                    FormText(
                        FOO_{Var(Variable { var: "ARG", func_extern: None, args: [] })},
                    ),
                ),
                53..72,
            ),
        ],
    },
]
"#
        );
    }

    #[test]
    fn test_method_call() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/programs/method_call.erb"#,
                ParserContext::parse_program_str
            ),
            r#"
[
    Function {
        header: FunctionHeader {
            file_path: "",
            name: "FOO",
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Assign(
                    Variable {
                        var: "A",
                        func_extern: None,
                        args: [],
                    },
                    None,
                    Method(
                        "BAR",
                        [],
                    ),
                ),
                5..14,
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            file_path: "",
            name: "BAR",
            args: [],
            infos: [
                Function,
            ],
        },
        body: [
            StmtWithPos(
                Command(
                    Return,
                    [
                        Int(
                            123,
                        ),
                    ],
                ),
                31..43,
            ),
        ],
    },
]
"#
        );
    }

    #[test]
    fn test_simple() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/programs/simple.erb"#,
                ParserContext::parse_program_str
            ),
            r#"
[
    Function {
        header: FunctionHeader {
            file_path: "",
            name: "FOO",
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Print(
                    (empty),
                    String(
                        "foo",
                    ),
                ),
                5..14,
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            file_path: "",
            name: "FOO",
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Print(
                    (empty),
                    String(
                        "foo",
                    ),
                ),
                21..30,
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            file_path: "",
            name: "BAR",
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Print(
                    (empty),
                    String(
                        "bar",
                    ),
                ),
                37..46,
            ),
        ],
    },
]
"#
        );
    }
}
