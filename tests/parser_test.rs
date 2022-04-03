mod test_util;
mod body {
    use crate::test_util::do_test;
    use erars_compiler::parse_body;

    #[test]
    fn test_alignment() {
        k9::snapshot!(
            do_test("tests/parse_tests/bodys/alignment.erb", parse_body),
            "
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
]
"
        );
    }

    #[test]
    fn test_assign() {
        k9::snapshot!(
            do_test("tests/parse_tests/bodys/assign.erb", parse_body),
            "
[
    Assign(
        Variable {
            var_idx: VariableIndex(
                21,
            ),
            args: [
                BinopExpr(
                    IntLit(
                        1,
                    ),
                    Add,
                    IntLit(
                        3,
                    ),
                ),
            ],
        },
        None,
        BinopExpr(
            IntLit(
                23,
            ),
            Add,
            IntLit(
                45,
            ),
        ),
    ),
]
"
        );
    }

    #[test]
    fn test_assign_add() {
        k9::snapshot!(
            do_test("tests/parse_tests/bodys/assign_add.erb", parse_body),
            "
[
    Assign(
        Variable {
            var_idx: VariableIndex(
                88,
            ),
            args: [
                IntLit(
                    13,
                ),
            ],
        },
        Some(
            BitOr,
        ),
        IntLit(
            2,
        ),
    ),
]
"
        );
    }

    #[test]
    fn test_assign_str() {
        k9::snapshot!(
            do_test("tests/parse_tests/bodys/assign_str.erb", parse_body),
            r#"
[
    Assign(
        Variable {
            var_idx: VariableIndex(
                18,
            ),
            args: [],
        },
        None,
        FormText(
             {IntLit(123)}456,
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                18,
            ),
            args: [],
        },
        None,
        FormText(
             {Var(Variable { var_idx: VariableIndex(17), args: [IntLit(0)] })}.{Method("TOSTR", [Var(Variable { var_idx: VariableIndex(17), args: [IntLit(1)] }), StringLit("00")])},
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                105,
            ),
            args: [
                Var(
                    Variable {
                        var_idx: VariableIndex(
                            23,
                        ),
                        args: [],
                    },
                ),
            ],
        },
        None,
        FormText(
             {CondExpr(Var(Variable { var_idx: VariableIndex(95), args: [Var(Variable { var_idx: VariableIndex(23), args: [] }), IntLit(120)] }), FormText(신사), FormText(숙녀))},
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                18,
            ),
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
            do_test("tests/parse_tests/bodys/call.erb", parse_body),
            r#"
[
    Call(
        "FOO",
        [
            IntLit(
                123,
            ),
            Var(
                Variable {
                    var_idx: VariableIndex(
                        54,
                    ),
                    args: [
                        IntLit(
                            634,
                        ),
                    ],
                },
            ),
            StringLit(
                "123",
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
            do_test("tests/parse_tests/bodys/hello.erb", parse_body),
            r#"
[
    Print(
        NEWLINE,
        StringLit(
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
            do_test("tests/parse_tests/bodys/if.erb", parse_body),
            r#"
[
    If(
        [
            (
                BinopExpr(
                    Var(
                        Variable {
                            var_idx: VariableIndex(
                                54,
                            ),
                            args: [],
                        },
                    ),
                    Greater,
                    IntLit(
                        1,
                    ),
                ),
                [
                    Print(
                        (empty),
                        StringLit(
                            "A > 1",
                        ),
                    ),
                ],
            ),
            (
                BinopExpr(
                    Var(
                        Variable {
                            var_idx: VariableIndex(
                                54,
                            ),
                            args: [],
                        },
                    ),
                    Equal,
                    IntLit(
                        1,
                    ),
                ),
                [
                    Print(
                        (empty),
                        StringLit(
                            "A == 1",
                        ),
                    ),
                ],
            ),
        ],
        Some(
            [
                Print(
                    (empty),
                    StringLit(
                        "A < 1",
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
    fn test_number() {
        k9::snapshot!(
            do_test("tests/parse_tests/bodys/number.erb", parse_body),
            "
[
    Assign(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        None,
        IntLit(
            1,
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        None,
        IntLit(
            1234,
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        None,
        IntLit(
            65535,
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        None,
        IntLit(
            0,
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        None,
        IntLit(
            4,
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        None,
        IntLit(
            -65535,
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        None,
        IntLit(
            0,
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        None,
        IntLit(
            -4,
        ),
    ),
]
"
        );
    }

    #[test]
    fn test_print_simple() {
        k9::snapshot!(
            do_test("tests/parse_tests/bodys/print_simple.erb", parse_body),
            "
[
    PrintForm(
        NEWLINE,
        1 + 1 = {BinopExpr(IntLit(1), Add, IntLit(1))},
    ),
]
"
        );
    }

    #[test]
    fn test_selectcase() {
        k9::snapshot!(
            do_test("tests/parse_tests/bodys/selectcase.erb", parse_body),
            r#"
[
    SelectCase(
        IntLit(
            1,
        ),
        [
            (
                [
                    Single(
                        IntLit(
                            0,
                        ),
                    ),
                ],
                [
                    Print(
                        (empty),
                        StringLit(
                            "FOO",
                        ),
                    ),
                ],
            ),
            (
                [
                    To(
                        IntLit(
                            1,
                        ),
                        IntLit(
                            2,
                        ),
                    ),
                ],
                [
                    Print(
                        (empty),
                        StringLit(
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
                    StringLit(
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
            do_test("tests/parse_tests/bodys/sif.erb", parse_body),
            r#"
[
    Sif(
        IntLit(
            12,
        ),
        Print(
            (empty),
            StringLit(
                "45",
            ),
        ),
    ),
    Print(
        (empty),
        StringLit(
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
            do_test("tests/parse_tests/bodys/times.erb", parse_body),
            "
[
    Times(
        Variable {
            var_idx: VariableIndex(
                17,
            ),
            args: [],
        },
        NotNan(
            123.33,
        ),
    ),
]
"
        );
    }
}
mod expr {
    use crate::test_util::do_test;
    use erars_compiler::parse_expr;

    #[test]
    fn test_boolean() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/boolean.erb", parse_expr),
            "
BinopExpr(
    BinopExpr(
        Var(
            Variable {
                var_idx: VariableIndex(
                    37,
                ),
                args: [],
            },
        ),
        Equal,
        IntLit(
            0,
        ),
    ),
    And,
    BinopExpr(
        Var(
            Variable {
                var_idx: VariableIndex(
                    95,
                ),
                args: [
                    Var(
                        Variable {
                            var_idx: VariableIndex(
                                23,
                            ),
                            args: [],
                        },
                    ),
                    IntLit(
                        998,
                    ),
                ],
            },
        ),
        Equal,
        IntLit(
            0,
        ),
    ),
)
"
        );
    }

    #[test]
    fn test_cond() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/cond.erb", parse_expr),
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
    fn test_method() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/method.erb", parse_expr),
            r#"
Method(
    "FOO",
    [
        IntLit(
            123,
        ),
        StringLit(
            "BAR",
        ),
        Var(
            Variable {
                var_idx: VariableIndex(
                    17,
                ),
                args: [
                    IntLit(
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
            do_test("tests/parse_tests/exprs/plus.erb", parse_expr),
            "
BinopExpr(
    IntLit(
        1,
    ),
    Add,
    IntLit(
        1,
    ),
)
"
        );
    }

    #[test]
    fn test_plus_mul() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/plus_mul.erb", parse_expr),
            "
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
)
"
        );
    }

    #[test]
    fn test_plus_mul_paran() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/plus_mul_paran.erb", parse_expr),
            "
BinopExpr(
    BinopExpr(
        IntLit(
            1,
        ),
        Add,
        IntLit(
            2,
        ),
    ),
    Mul,
    IntLit(
        3,
    ),
)
"
        );
    }

    #[test]
    fn test_str_literal() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/str_literal.erb", parse_expr),
            r#"
StringLit(
    "123",
)
"#
        );
    }

    #[test]
    fn test_var_arg() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/var_arg.erb", parse_expr),
            "
Var(
    Variable {
        var_idx: VariableIndex(
            21,
        ),
        args: [
            IntLit(
                123,
            ),
        ],
    },
)
"
        );
    }

    #[test]
    fn test_var_complex() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/var_complex.erb", parse_expr),
            "
Var(
    Variable {
        var_idx: VariableIndex(
            21,
        ),
        args: [
            Var(
                Variable {
                    var_idx: VariableIndex(
                        54,
                    ),
                    args: [
                        IntLit(
                            123,
                        ),
                    ],
                },
            ),
            IntLit(
                123,
            ),
        ],
    },
)
"
        );
    }

    #[test]
    fn test_var_empty() {
        k9::snapshot!(
            do_test("tests/parse_tests/exprs/var_empty.erb", parse_expr),
            "
Var(
    Variable {
        var_idx: VariableIndex(
            21,
        ),
        args: [],
    },
)
"
        );
    }
}
mod function {
    use crate::test_util::do_test;
    use erars_compiler::parse_function;

    #[test]
    fn test_function() {
        k9::snapshot!(
            do_test("tests/parse_tests/functions/function.erb", parse_function),
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
            StringLit(
                "Hello",
            ),
        ),
        PrintForm(
            NEWLINE,
            {IntLit(123)},
        ),
    ],
}
"#
        );
    }

    #[test]
    fn test_juel() {
        k9::snapshot!(
            do_test("tests/parse_tests/functions/juel.erb", parse_function),
            r#"
Function {
    header: FunctionHeader {
        name: "COMMON_MOVE_JUEL",
        args: [
            (
                Variable {
                    var_idx: VariableIndex(
                        19,
                    ),
                    args: [],
                },
                None,
            ),
            (
                Variable {
                    var_idx: VariableIndex(
                        19,
                    ),
                    args: [
                        IntLit(
                            1,
                        ),
                    ],
                },
                None,
            ),
            (
                Variable {
                    var_idx: VariableIndex(
                        19,
                    ),
                    args: [
                        IntLit(
                            2,
                        ),
                    ],
                },
                None,
            ),
            (
                Variable {
                    var_idx: VariableIndex(
                        19,
                    ),
                    args: [
                        IntLit(
                            3,
                        ),
                    ],
                },
                None,
            ),
            (
                Variable {
                    var_idx: VariableIndex(
                        19,
                    ),
                    args: [
                        IntLit(
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
                var_idx: VariableIndex(
                    17,
                ),
                args: [
                    IntLit(
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
                                var_idx: VariableIndex(
                                    41,
                                ),
                                args: [
                                    Var(
                                        Variable {
                                            var_idx: VariableIndex(
                                                19,
                                            ),
                                            args: [],
                                        },
                                    ),
                                    Var(
                                        Variable {
                                            var_idx: VariableIndex(
                                                19,
                                            ),
                                            args: [
                                                IntLit(
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
                                var_idx: VariableIndex(
                                    19,
                                ),
                                args: [
                                    IntLit(
                                        2,
                                    ),
                                ],
                            },
                        ),
                    ),
                    IntLit(
                        0,
                    ),
                    BinopExpr(
                        IntLit(
                            -9223372036854775808,
                        ),
                        Sub,
                        IntLit(
                            1,
                        ),
                    ),
                ],
            ),
        ),
        Assign(
            Variable {
                var_idx: VariableIndex(
                    17,
                ),
                args: [
                    IntLit(
                        2,
                    ),
                ],
            },
            None,
            BinopExpr(
                Var(
                    Variable {
                        var_idx: VariableIndex(
                            17,
                        ),
                        args: [
                            IntLit(
                                1,
                            ),
                        ],
                    },
                ),
                Sub,
                Var(
                    Variable {
                        var_idx: VariableIndex(
                            41,
                        ),
                        args: [
                            Var(
                                Variable {
                                    var_idx: VariableIndex(
                                        19,
                                    ),
                                    args: [],
                                },
                            ),
                            Var(
                                Variable {
                                    var_idx: VariableIndex(
                                        19,
                                    ),
                                    args: [
                                        IntLit(
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
        Assign(
            Variable {
                var_idx: VariableIndex(
                    18,
                ),
                args: [],
            },
            None,
            FormText(
                 {Var(Variable { var_idx: VariableIndex(11), args: [Var(Variable { var_idx: VariableIndex(19), args: [IntLit(1)] })] })}의 구슬{CondExpr(BinopExpr(BinopExpr(Var(Variable { var_idx: VariableIndex(19), args: [IntLit(4)] }), Sub, BinopExpr(Var(Variable { var_idx: VariableIndex(19), args: [] }), NotEqual, Var(Variable { var_idx: VariableIndex(22), args: [] }))), LessOrEqual, IntLit(0)), FormText(({Var(Variable { var_idx: VariableIndex(104), args: [Var(Variable { var_idx: VariableIndex(19), args: [] })] })})), FormText())} {CondExpr(BinopExpr(Method("SIGN", [Var(Variable { var_idx: VariableIndex(17), args: [IntLit(2)] })]), Equal, IntLit(1)), FormText(＋), FormText(－ ))} {Method("ABS", [Var(Variable { var_idx: VariableIndex(17), args: [IntLit(2)] })])},
            ),
        ),
        Assign(
            Variable {
                var_idx: VariableIndex(
                    41,
                ),
                args: [
                    Var(
                        Variable {
                            var_idx: VariableIndex(
                                19,
                            ),
                            args: [
                                Var(
                                    Variable {
                                        var_idx: VariableIndex(
                                            19,
                                        ),
                                        args: [
                                            IntLit(
                                                1,
                                            ),
                                        ],
                                    },
                                ),
                            ],
                        },
                    ),
                ],
            },
            None,
            Var(
                Variable {
                    var_idx: VariableIndex(
                        17,
                    ),
                    args: [
                        IntLit(
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
                                        var_idx: VariableIndex(
                                            17,
                                        ),
                                        args: [
                                            IntLit(
                                                2,
                                            ),
                                        ],
                                    },
                                ),
                            ],
                        ),
                        Greater,
                        IntLit(
                            0,
                        ),
                    ),
                    [
                        SelectCase(
                            Var(
                                Variable {
                                    var_idx: VariableIndex(
                                        19,
                                    ),
                                    args: [
                                        IntLit(
                                            3,
                                        ),
                                    ],
                                },
                            ),
                            [
                                (
                                    [
                                        Single(
                                            IntLit(
                                                0,
                                            ),
                                        ),
                                    ],
                                    [
                                        Print(
                                            NEWLINE,
                                            Var(
                                                Variable {
                                                    var_idx: VariableIndex(
                                                        18,
                                                    ),
                                                    args: [],
                                                },
                                            ),
                                        ),
                                        Return(
                                            [
                                                IntLit(
                                                    1,
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                                (
                                    [
                                        Single(
                                            IntLit(
                                                1,
                                            ),
                                        ),
                                    ],
                                    [
                                        Print(
                                            NEWLINE | WAIT,
                                            Var(
                                                Variable {
                                                    var_idx: VariableIndex(
                                                        18,
                                                    ),
                                                    args: [],
                                                },
                                            ),
                                        ),
                                        Return(
                                            [
                                                IntLit(
                                                    1,
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                            Some(
                                [
                                    Return(
                                        [
                                            IntLit(
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
            None,
        ),
    ],
}
"#
        );
    }
}
mod program {
    use crate::test_util::do_test;
    use erars_compiler::parse_program;

    #[test]
    fn test_call_form() {
        k9::snapshot!(
            do_test("tests/parse_tests/programs/call_form.erb", parse_program),
            r#"
[
    Function {
        header: FunctionHeader {
            name: "SYSTEM_TITLE",
            args: [],
            infos: [],
        },
        body: [
            CallForm(
                FOO_{IntLit(123)},
                [
                    IntLit(
                        345,
                    ),
                ],
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            name: "FOO_123",
            args: [
                (
                    Variable {
                        var_idx: VariableIndex(
                            19,
                        ),
                        args: [],
                    },
                    None,
                ),
            ],
            infos: [],
        },
        body: [
            PrintForm(
                (empty),
                FOO_{Var(Variable { var_idx: VariableIndex(19), args: [] })},
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
            do_test("tests/parse_tests/programs/method_call.erb", parse_program),
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
                    var_idx: VariableIndex(
                        54,
                    ),
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
            ReturnF(
                IntLit(
                    123,
                ),
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
            do_test("tests/parse_tests/programs/simple.erb", parse_program),
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
                StringLit(
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
                StringLit(
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
                StringLit(
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
