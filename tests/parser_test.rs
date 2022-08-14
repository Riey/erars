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
    Alignment(
        Left,
    ),
    Alignment(
        Center,
    ),
    Alignment(
        Right,
    ),
    Print(
        NEWLINE,
        FormText(
            {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
        ),
    ),
    Print(
        NEWLINE,
        FormText(
            {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
        ),
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
    Command(
        CustomDrawLine,
        [],
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
        ],
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
    Print(
        NEWLINE,
        String(
            "Hello, world!",
        ),
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
                    Print(
                        (empty),
                        String(
                            "A > 1",
                        ),
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
                    Print(
                        (empty),
                        String(
                            "A == 1",
                        ),
                    ),
                ],
            ),
        ],
        [
            Print(
                (empty),
                String(
                    "A < 1",
                ),
            ),
        ],
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
    Print(
        NEWLINE,
        FormText(
            1 + 1 = {BinopExpr(Int(1), Add, Int(1))},
        ),
    ),
    Print(
        NEWLINE | WAIT,
        FormText(
            {Method("조사처리", [Var(Variable { var: "CALLNAME", func_extern: None, args: [Method("GET_CHARA_M", [])] }), String("와")])}같이 온 걸 보니, 단단히 각오하고 온 것 같다,
        ),
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
                    Print(
                        (empty),
                        String(
                            "FOO",
                        ),
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
                    Print(
                        (empty),
                        String(
                            "BAR",
                        ),
                    ),
                ],
            ),
        ],
        Some(
            [
                Print(
                    (empty),
                    String(
                        "BAZ",
                    ),
                ),
            ],
        ),
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
    Sif(
        Int(
            12,
        ),
        Print(
            (empty),
            String(
                "45",
            ),
        ),
    ),
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
        Print(
            NEWLINE,
            FormText(
                {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
            ),
        ),
    ),
    Print(
        (empty),
        String(
            "32",
        ),
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
                BinopExpr(
                    Int(
                        998,
                    ),
                    Equal,
                    Int(
                        0,
                    ),
                ),
            ],
        },
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
    Var(
        Variable {
            var: "RAND",
            func_extern: None,
            args: [
                BinopExpr(
                    Int(
                        10,
                    ),
                    Mul,
                    Int(
                        5,
                    ),
                ),
            ],
        },
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
        name: "FOO",
        args: [],
        infos: [],
    },
    body: [
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
        Goto {
            label: String(
                "LABEL",
            ),
            catch_body: None,
        },
        Goto {
            label: FormText(
                {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
            ),
            catch_body: None,
        },
        Goto {
            label: FormText(
                {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
            ),
            catch_body: Some(
                [],
            ),
        },
        Goto {
            label: FormText(
                {Var(Variable { var: "LOCALS", func_extern: None, args: [] })},
            ),
            catch_body: Some(
                [
                    Print(
                        NEWLINE,
                        String(
                            "CATCH",
                        ),
                    ),
                ],
            ),
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: false,
            try_body: [],
            catch_body: None,
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: false,
            try_body: [],
            catch_body: None,
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: false,
            try_body: [],
            catch_body: None,
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: true,
            try_body: [],
            catch_body: None,
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: true,
            try_body: [],
            catch_body: None,
        },
        Call {
            name: FormText(
                BAR,
            ),
            args: [],
            is_jump: true,
            try_body: [],
            catch_body: None,
        },
        Label(
            "LABEL",
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
        name: "SYSTEM_TITLE",
        args: [],
        infos: [
            Dim(
                LocalVariable {
                    var: "FOO",
                    info: VariableInfo {
                        is_chara: false,
                        is_str: false,
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
        name: "FOO",
        args: [],
        infos: [
            EventFlag(
                Pre,
            ),
        ],
    },
    body: [
        Print(
            NEWLINE,
            String(
                "Hello",
            ),
        ),
        Print(
            NEWLINE,
            FormText(
                {Int(123)},
            ),
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
                                BinopExpr(
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
                                    Add,
                                    Var(
                                        Variable {
                                            var: "ARG",
                                            func_extern: None,
                                            args: [],
                                        },
                                    ),
                                ),
                                Int(
                                    2,
                                ),
                            ],
                        },
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
            Var(
                Variable {
                    var: "LOCAL",
                    func_extern: None,
                    args: [
                        BinopExpr(
                            Int(
                                1,
                            ),
                            Sub,
                            Var(
                                Variable {
                                    var: "JUEL",
                                    func_extern: None,
                                    args: [],
                                },
                            ),
                        ),
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
        Assign(
            Variable {
                var: "LOCALS",
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                {Var(Variable { var: "PALAMNAME", func_extern: None, args: [Var(Variable { var: "ARG", func_extern: None, args: [Int(1)] })] })}의 구슬{CondExpr(Var(Variable { var: "ARG", func_extern: None, args: [BinopExpr(BinopExpr(Int(4), Sub, BinopExpr(Var(Variable { var: "ARG", func_extern: None, args: [] }), NotEqual, Var(Variable { var: "TARGET", func_extern: None, args: [] }))), LessOrEqual, Int(0))] }), FormText(({Var(Variable { var: "CALLNAME", func_extern: None, args: [Var(Variable { var: "ARG", func_extern: None, args: [] })] })})), FormText())} {CondExpr(BinopExpr(Method("SIGN", [Var(Variable { var: "LOCAL", func_extern: None, args: [Int(2)] })]), Equal, Int(1)), FormText(＋), FormText(－))} {Method("ABS", [Var(Variable { var: "LOCAL", func_extern: None, args: [Int(2)] })])},
            ),
        ),
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
                                        Command(
                                            Return,
                                            [
                                                Int(
                                                    1,
                                                ),
                                            ],
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
                                        Command(
                                            Return,
                                            [
                                                Int(
                                                    1,
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                            Some(
                                [
                                    Command(
                                        Return,
                                        [
                                            Int(
                                                0,
                                            ),
                                        ],
                                    ),
                                ],
                            ),
                        ),
                    ],
                ),
            ],
            [],
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
            name: "SYSTEM_TITLE",
            args: [],
            infos: [],
        },
        body: [
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
        ],
    },
    Function {
        header: FunctionHeader {
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
            Print(
                (empty),
                FormText(
                    FOO_{Var(Variable { var: "ARG", func_extern: None, args: [] })},
                ),
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
            name: "FOO",
            args: [],
            infos: [],
        },
        body: [
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
        ],
    },
    Function {
        header: FunctionHeader {
            name: "BAR",
            args: [],
            infos: [
                Function,
            ],
        },
        body: [
            Command(
                Return,
                [
                    Int(
                        123,
                    ),
                ],
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
            name: "FOO",
            args: [],
            infos: [],
        },
        body: [
            Print(
                (empty),
                String(
                    "foo",
                ),
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            name: "FOO",
            args: [],
            infos: [],
        },
        body: [
            Print(
                (empty),
                String(
                    "foo",
                ),
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            name: "BAR",
            args: [],
            infos: [],
        },
        body: [
            Print(
                (empty),
                String(
                    "bar",
                ),
            ),
        ],
    },
]
"#
        );
    }
}
