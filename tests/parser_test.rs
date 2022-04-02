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
                18,
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
                54,
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
                15,
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
                15,
            ),
            args: [],
        },
        None,
        FormText(
            {Var(Variable { var_idx: VariableIndex(14), args: [IntLit(0)] })}.{Method("TOSTR", [Var(Variable { var_idx: VariableIndex(14), args: [IntLit(1)] }), StringLit("00")])},
        ),
    ),
    Assign(
        Variable {
            var_idx: VariableIndex(
                70,
            ),
            args: [
                Var(
                    Variable {
                        var_idx: VariableIndex(
                            20,
                        ),
                        args: [],
                    },
                ),
            ],
        },
        None,
        FormText(
            {CondExpr(Var(Variable { var_idx: VariableIndex(61), args: [Var(Variable { var_idx: VariableIndex(20), args: [] }), IntLit(120)] }), FormText(신사), FormText(숙녀))},
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
                        48,
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
        "Hello, world!",
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
                                48,
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
                        "A > 1",
                    ),
                ],
            ),
            (
                BinopExpr(
                    Var(
                        Variable {
                            var_idx: VariableIndex(
                                48,
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
                        "A == 1",
                    ),
                ],
            ),
        ],
        Some(
            [
                Print(
                    (empty),
                    "A < 1",
                ),
            ],
        ),
    ),
]
"#
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
            "45",
        ),
    ),
    Print(
        (empty),
        "32",
    ),
]
"#
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
                    34,
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
                    61,
                ),
                args: [
                    Var(
                        Variable {
                            var_idx: VariableIndex(
                                20,
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
                    14,
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
            18,
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
            18,
        ),
        args: [
            Var(
                Variable {
                    var_idx: VariableIndex(
                        48,
                    ),
                    args: [],
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
            18,
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
            "Hello",
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
                            16,
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
                FOO_{Var(Variable { var_idx: VariableIndex(16), args: [] })},
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
                        48,
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
                "foo",
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
                "foo",
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
                "bar",
            ),
        ],
    },
]
"#
        );
    }
}
