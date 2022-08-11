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
            {Var(Variable { var: "LOCALS", args: [] })},
        ),
    ),
    Print(
        NEWLINE,
        FormText(
            {Var(Variable { var: "LOCALS", args: [] })},
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
            args: [],
        },
        None,
        FormText(
            {Var(Variable { var: "LOCAL", args: [Int(0)] })}.{Method("TOSTR", [Var(Variable { var: "LOCAL", args: [Int(1)] }), String("00")])},
        ),
    ),
    Assign(
        Variable {
            var: "NICKNAME",
            args: [
                Var(
                    Variable {
                        var: "MASTER",
                        args: [],
                    },
                ),
            ],
        },
        None,
        FormText(
            {CondExpr(Var(Variable { var: "TALENT", args: [Var(Variable { var: "MASTER", args: [] }), Int(120)] }), FormText(신사), FormText(숙녀))},
        ),
    ),
    Assign(
        Variable {
            var: "LOCALS",
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
        catch: None,
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
            args: [],
        },
        None,
        UnaryopExpr(
            Int(
                1,
            ),
            Minus,
        ),
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
            "
[
    Print(
        NEWLINE,
        FormText(
            1 + 1 = {BinopExpr(Int(1), Add, Int(1))},
        ),
    ),
]
"
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
            args: [
                Var(
                    Variable {
                        var: "MASTER",
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
    UnaryopExpr(
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
                        args: [
                            Var(
                                Variable {
                                    var: "ARG",
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
        Plus,
    ),
    Add,
    Var(
        Variable {
            var: "RAND",
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
    "123",
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
        args: [
            Var(
                Variable {
                    var: "A",
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
            catch: None,
        },
        Goto {
            label: FormText(
                {Var(Variable { var: "LOCALS", args: [] })},
            ),
            catch: None,
        },
        Goto {
            label: FormText(
                {Var(Variable { var: "LOCALS", args: [] })},
            ),
            catch: Some(
                [],
            ),
        },
        Goto {
            label: FormText(
                {Var(Variable { var: "LOCALS", args: [] })},
            ),
            catch: Some(
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
            catch: None,
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: false,
            catch: None,
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: false,
            catch: None,
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: true,
            catch: None,
        },
        Call {
            name: String(
                "BAR",
            ),
            args: [],
            is_jump: true,
            catch: None,
        },
        Call {
            name: FormText(
                BAR,
            ),
            args: [],
            is_jump: true,
            catch: None,
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
        k9::snapshot!(do_test(
            r#"tests/parse_tests/functions/dim.erb"#,
            ParserContext::parse_function_str
        ));
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
        k9::snapshot!(do_test(
            r#"tests/parse_tests/functions/juel.erb"#,
            ParserContext::parse_function_str
        ));
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
                    FOO_{Int(123)}, 345,
                ),
                args: [],
                is_jump: false,
                catch: None,
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
                    FOO_{Var(Variable { var: "ARG", args: [] })},
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
