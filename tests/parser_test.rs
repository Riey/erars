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
            "
[
    StmtWithPos(
        Alignment(
            Left,
        ),
        ScriptPosition {
            line: 0,
        },
    ),
    StmtWithPos(
        Alignment(
            Center,
        ),
        ScriptPosition {
            line: 1,
        },
    ),
    StmtWithPos(
        Alignment(
            Right,
        ),
        ScriptPosition {
            line: 2,
        },
    ),
    StmtWithPos(
        Print(
            NEWLINE,
            FormText(
                {Var(Variable { var: LOCALS, func_extern: None, args: [] })},
            ),
        ),
        ScriptPosition {
            line: 4,
        },
    ),
    StmtWithPos(
        Print(
            NEWLINE,
            FormText(
                {Var(Variable { var: LOCALS, func_extern: None, args: [] })},
            ),
        ),
        ScriptPosition {
            line: 5,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_assign() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/assign.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Assign(
            Variable {
                var: COUNT,
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
        ScriptPosition {
            line: 0,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_assign_add() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/assign_add.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Assign(
            Variable {
                var: FLAG,
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
        ScriptPosition {
            line: 0,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_assign_str() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/assign_str.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Assign(
            Variable {
                var: LOCALS,
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                {Int(123)}456,
            ),
        ),
        ScriptPosition {
            line: 0,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCALS,
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                {Var(Variable { var: LOCAL, func_extern: None, args: [Int(0)] })}.{BuiltinMethod(ToStr, [Var(Variable { var: LOCAL, func_extern: None, args: [Int(1)] }), String(00)])},
            ),
        ),
        ScriptPosition {
            line: 1,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCALS,
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                {FormText({BinopExpr(Int(1), Add, Int(1))})},
            ),
        ),
        ScriptPosition {
            line: 2,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: NICKNAME,
                func_extern: None,
                args: [
                    Var(
                        Variable {
                            var: MASTER,
                            func_extern: None,
                            args: [],
                        },
                    ),
                ],
            },
            None,
            FormText(
                {CondExpr(Var(Variable { var: TALENT, func_extern: None, args: [Var(Variable { var: MASTER, func_extern: None, args: [] }), Int(120)] }), FormText(신사), FormText(숙녀))},
            ),
        ),
        ScriptPosition {
            line: 3,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCALS,
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                ,
            ),
        ),
        ScriptPosition {
            line: 4,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCALS,
                func_extern: None,
                args: [
                    BuiltinMethod(
                        ToInt,
                        [
                            Var(
                                Variable {
                                    var: CHOICES,
                                    func_extern: None,
                                    args: [
                                        Int(
                                            1,
                                        ),
                                    ],
                                },
                            ),
                        ],
                    ),
                ],
            },
            None,
            FormText(
                ,
            ),
        ),
        ScriptPosition {
            line: 5,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCALS,
                func_extern: None,
                args: [],
            },
            None,
            FormText(
                -{Method(조사처리, [Var(Variable { var: CALLNAME, func_extern: None, args: [BuiltinMethod(ToInt, [Var(Variable { var: CHOICES, func_extern: None, args: [Int(1)] })])] }), String(이)])} 기억하고 있는 아르카나의 목록-,
            ),
        ),
        ScriptPosition {
            line: 6,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_call() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/call.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Call {
            name: String(
                FOO,
            ),
            args: [
                Int(
                    123,
                ),
                Var(
                    Variable {
                        var: A,
                        func_extern: None,
                        args: [
                            Int(
                                634,
                            ),
                        ],
                    },
                ),
                String(
                    123,
                ),
            ],
            is_jump: false,
            is_method: false,
            try_body: [],
            catch_body: None,
        },
        ScriptPosition {
            line: 0,
        },
    ),
]
"
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
            [
                String(
                    =,
                ),
            ],
        ),
        ScriptPosition {
            line: 0,
        },
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
            "
[
    StmtWithPos(
        For(
            Variable {
                var: COUNT,
                func_extern: None,
                args: [],
            },
            (
                Int(
                    0,
                ),
                Int(
                    10,
                ),
                Int(
                    1,
                ),
            ),
            [
                StmtWithPos(
                    PrintList(
                        NEWLINE,
                        [
                            Var(
                                Variable {
                                    var: COUNT,
                                    func_extern: None,
                                    args: [],
                                },
                            ),
                        ],
                    ),
                    ScriptPosition {
                        line: 1,
                    },
                ),
            ],
        ),
        ScriptPosition {
            line: 0,
        },
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
            "
[
    StmtWithPos(
        Print(
            NEWLINE,
            String(
                Hello, world!,
            ),
        ),
        ScriptPosition {
            line: 0,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_if() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/if.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        If(
            [
                (
                    BinopExpr(
                        Var(
                            Variable {
                                var: A,
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
                                    A > 1,
                                ),
                            ),
                            ScriptPosition {
                                line: 1,
                            },
                        ),
                    ],
                ),
                (
                    BinopExpr(
                        Var(
                            Variable {
                                var: A,
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
                                    A == 1,
                                ),
                            ),
                            ScriptPosition {
                                line: 3,
                            },
                        ),
                    ],
                ),
            ],
            [
                StmtWithPos(
                    Print(
                        (empty),
                        String(
                            A < 1,
                        ),
                    ),
                    ScriptPosition {
                        line: 5,
                    },
                ),
            ],
        ),
        ScriptPosition {
            line: 0,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_number() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/number.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Assign(
            Variable {
                var: LOCAL,
                func_extern: None,
                args: [],
            },
            None,
            Int(
                1,
            ),
        ),
        ScriptPosition {
            line: 0,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCAL,
                func_extern: None,
                args: [],
            },
            None,
            Int(
                1234,
            ),
        ),
        ScriptPosition {
            line: 1,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCAL,
                func_extern: None,
                args: [],
            },
            None,
            Int(
                65535,
            ),
        ),
        ScriptPosition {
            line: 2,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCAL,
                func_extern: None,
                args: [],
            },
            None,
            Int(
                1,
            ),
        ),
        ScriptPosition {
            line: 3,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCAL,
                func_extern: None,
                args: [],
            },
            None,
            Int(
                3,
            ),
        ),
        ScriptPosition {
            line: 4,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCAL,
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
        ScriptPosition {
            line: 5,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCAL,
                func_extern: None,
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
        ScriptPosition {
            line: 6,
        },
    ),
    StmtWithPos(
        Assign(
            Variable {
                var: LOCAL,
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
        ScriptPosition {
            line: 7,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_print_data() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/print_data.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        PrintData(
            (empty),
            None,
            [
                [
                    String(
                        data,
                    ),
                ],
                [
                    FormText(
                        LOCAL={Var(Variable { var: LOCAL, func_extern: None, args: [] })},
                    ),
                ],
                [
                    String(
                        list,
                    ),
                    String(
                        form,
                    ),
                ],
            ],
        ),
        ScriptPosition {
            line: 0,
        },
    ),
]
"
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
    StmtWithPos(
        Print(
            NEWLINE,
            FormText(
                1 + 1 = {BinopExpr(Int(1), Add, Int(1))},
            ),
        ),
        ScriptPosition {
            line: 0,
        },
    ),
    StmtWithPos(
        Print(
            NEWLINE | WAIT,
            FormText(
                {Method(조사처리, [Var(Variable { var: CALLNAME, func_extern: None, args: [Method(GET_CHARA_M, [])] }), String(와)])} 같이 온 걸 보니, 단단히 각오하고 온 것 같다,
            ),
        ),
        ScriptPosition {
            line: 1,
        },
    ),
    StmtWithPos(
        Print(
            NEWLINE,
            FormText(
                {Var(Variable { var: CALLNAME, func_extern: None, args: [Var(Variable { var: ARG, func_extern: None, args: [] })] })}의 교습 관찰 결과 완료  결과：임시 성과치 {Var(Variable { var: LOCAL, func_extern: None, args: [Int(0)] })}에 의한 실제 성과치 {Var(Variable { var: LOCAL, func_extern: None, args: [Int(2)] })} 증가⇒{CondExpr(BinopExpr(Var(Variable { var: LOCAL, func_extern: None, args: [Int(1)] }), Equal, Int(1)), FormText(성공), FormText(실패))}({Var(Variable { var: CFLAG, func_extern: None, args: [Var(Variable { var: ARG, func_extern: None, args: [] }), Int(693)] })}％) 작업 내용：{Var(Variable { var: CFLAG, func_extern: None, args: [Var(Variable { var: ARG, func_extern: None, args: [] }), Int(690)] })},
            ),
        ),
        ScriptPosition {
            line: 2,
        },
    ),
    StmtWithPos(
        Print(
            NEWLINE | WAIT,
            FormText(
                보지에서 애액을 흘렸{CondExpr(BinopExpr(Var(Variable { var: TEQUIP, func_extern: None, args: [Int(42)] }), Equal, Int(0)), FormText(고, 작은 한숨을 토해냈), String())}다.,
            ),
        ),
        ScriptPosition {
            line: 3,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_printc() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/printc.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Print(
            LEFT_ALIGN,
            String(
                LC,
            ),
        ),
        ScriptPosition {
            line: 0,
        },
    ),
    StmtWithPos(
        Print(
            RIGHT_ALIGN,
            String(
                C,
            ),
        ),
        ScriptPosition {
            line: 1,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_repeat() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/repeat.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Repeat(
            BuiltinVar(
                CharaNum,
                [],
            ),
            [
                StmtWithPos(
                    Assign(
                        Variable {
                            var: MARK,
                            func_extern: None,
                            args: [
                                Var(
                                    Variable {
                                        var: COUNT,
                                        func_extern: None,
                                        args: [],
                                    },
                                ),
                                Int(
                                    98,
                                ),
                            ],
                        },
                        None,
                        Int(
                            0,
                        ),
                    ),
                    ScriptPosition {
                        line: 1,
                    },
                ),
                StmtWithPos(
                    Sif(
                        BinopExpr(
                            Var(
                                Variable {
                                    var: COUNT,
                                    func_extern: None,
                                    args: [],
                                },
                            ),
                            Equal,
                            Var(
                                Variable {
                                    var: MASTER,
                                    func_extern: None,
                                    args: [],
                                },
                            ),
                        ),
                        StmtWithPos(
                            Continue,
                            ScriptPosition {
                                line: 4,
                            },
                        ),
                    ),
                    ScriptPosition {
                        line: 3,
                    },
                ),
                StmtWithPos(
                    Sif(
                        BinopExpr(
                            Var(
                                Variable {
                                    var: CFLAG,
                                    func_extern: None,
                                    args: [
                                        Var(
                                            Variable {
                                                var: COUNT,
                                                func_extern: None,
                                                args: [],
                                            },
                                        ),
                                        Int(
                                            4,
                                        ),
                                    ],
                                },
                            ),
                            Equal,
                            Int(
                                1,
                            ),
                        ),
                        StmtWithPos(
                            Continue,
                            ScriptPosition {
                                line: 7,
                            },
                        ),
                    ),
                    ScriptPosition {
                        line: 6,
                    },
                ),
            ],
        ),
        ScriptPosition {
            line: 0,
        },
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
            "
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
                                    FOO,
                                ),
                            ),
                            ScriptPosition {
                                line: 2,
                            },
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
                                    BAR,
                                ),
                            ),
                            ScriptPosition {
                                line: 4,
                            },
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
                                BAZ,
                            ),
                        ),
                        ScriptPosition {
                            line: 6,
                        },
                    ),
                ],
            ),
        ),
        ScriptPosition {
            line: 0,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_sif() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/sif.erb"#,
                ParserContext::parse_body_str
            ),
            "
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
                        45,
                    ),
                ),
                ScriptPosition {
                    line: 1,
                },
            ),
        ),
        ScriptPosition {
            line: 0,
        },
    ),
    StmtWithPos(
        Sif(
            BinopExpr(
                Var(
                    Variable {
                        var: LOCALS,
                        func_extern: None,
                        args: [],
                    },
                ),
                NotEqual,
                String(
                    ,
                ),
            ),
            StmtWithPos(
                Print(
                    NEWLINE,
                    FormText(
                        {Var(Variable { var: LOCALS, func_extern: None, args: [] })},
                    ),
                ),
                ScriptPosition {
                    line: 3,
                },
            ),
        ),
        ScriptPosition {
            line: 2,
        },
    ),
    StmtWithPos(
        Print(
            (empty),
            String(
                32,
            ),
        ),
        ScriptPosition {
            line: 5,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_times() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/times.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Times(
            Variable {
                var: LOCAL,
                func_extern: None,
                args: [],
            },
            NotNan(
                123.33,
            ),
        ),
        ScriptPosition {
            line: 0,
        },
    ),
]
"
        );
    }

    #[test]
    fn test_trailing_comma() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/bodys/trailing_comma.erb"#,
                ParserContext::parse_body_str
            ),
            "
[
    StmtWithPos(
        Call {
            name: String(
                CALLFUNC,
            ),
            args: [
                Int(
                    1,
                ),
                Int(
                    2,
                ),
            ],
            is_jump: false,
            is_method: false,
            try_body: [],
            catch_body: None,
        },
        ScriptPosition {
            line: 0,
        },
    ),
]
"
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
            "
BinopExpr(
    BinopExpr(
        Var(
            Variable {
                var: RESULT,
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
                var: TALENT,
                func_extern: None,
                args: [
                    Var(
                        Variable {
                            var: MASTER,
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
"
        );
    }

    #[test]
    fn test_complex_op() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/complex_op.erb"#,
                ParserContext::parse_expr_str
            ),
            "
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
                    var: ABL,
                    func_extern: None,
                    args: [
                        Var(
                            Variable {
                                var: ARG,
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
        BuiltinVar(
            Rand,
            [
                Int(
                    10,
                ),
            ],
        ),
        Mul,
        Int(
            5,
        ),
    ),
)
"
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
            "
Var(
    Variable {
        var: FLAG,
        func_extern: None,
        args: [
            Int(
                1,
            ),
        ],
    },
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
            "
Method(
    FOO,
    [
        Int(
            123,
        ),
        String(
            BAR,
        ),
        Var(
            Variable {
                var: LOCAL,
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
"
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
            "
String(
    ,
)
"
        );
    }

    #[test]
    fn test_trailing_comma() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/trailing_comma.erb"#,
                ParserContext::parse_expr_str
            ),
            "
Method(
    METHOD,
    [
        Int(
            1,
        ),
        Int(
            2,
        ),
    ],
)
"
        );
    }

    #[test]
    fn test_var_arg() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/var_arg.erb"#,
                ParserContext::parse_expr_str
            ),
            "
Var(
    Variable {
        var: COUNT,
        func_extern: None,
        args: [
            Int(
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
            do_test(
                r#"tests/parse_tests/exprs/var_complex.erb"#,
                ParserContext::parse_expr_str
            ),
            "
Var(
    Variable {
        var: COUNT,
        func_extern: None,
        args: [
            Var(
                Variable {
                    var: A,
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
"
        );
    }

    #[test]
    fn test_var_empty() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/exprs/var_empty.erb"#,
                ParserContext::parse_expr_str
            ),
            "
Var(
    Variable {
        var: COUNT,
        func_extern: None,
        args: [],
    },
)
"
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
            "
Function {
    header: FunctionHeader {
        file_path: tests/parse_tests/functions/call.erb,
        name: FOO,
        args: [],
        infos: [],
    },
    body: [
        StmtWithPos(
            Assign(
                Variable {
                    var: LOCALS,
                    func_extern: None,
                    args: [],
                },
                None,
                FormText(
                    LABEL,
                ),
            ),
            ScriptPosition {
                line: 2,
            },
        ),
        StmtWithPos(
            Goto {
                label: String(
                    LABEL,
                ),
                catch_body: None,
            },
            ScriptPosition {
                line: 4,
            },
        ),
        StmtWithPos(
            Goto {
                label: FormText(
                    {Var(Variable { var: LOCALS, func_extern: None, args: [] })},
                ),
                catch_body: None,
            },
            ScriptPosition {
                line: 5,
            },
        ),
        StmtWithPos(
            Goto {
                label: FormText(
                    {Var(Variable { var: LOCALS, func_extern: None, args: [] })},
                ),
                catch_body: Some(
                    [],
                ),
            },
            ScriptPosition {
                line: 6,
            },
        ),
        StmtWithPos(
            Goto {
                label: FormText(
                    {Var(Variable { var: LOCALS, func_extern: None, args: [] })},
                ),
                catch_body: Some(
                    [
                        StmtWithPos(
                            Print(
                                NEWLINE,
                                String(
                                    CATCH,
                                ),
                            ),
                            ScriptPosition {
                                line: 9,
                            },
                        ),
                    ],
                ),
            },
            ScriptPosition {
                line: 7,
            },
        ),
        StmtWithPos(
            Call {
                name: String(
                    BAR,
                ),
                args: [],
                is_jump: false,
                is_method: false,
                try_body: [],
                catch_body: None,
            },
            ScriptPosition {
                line: 12,
            },
        ),
        StmtWithPos(
            Call {
                name: String(
                    BAR,
                ),
                args: [],
                is_jump: false,
                is_method: false,
                try_body: [],
                catch_body: Some(
                    [],
                ),
            },
            ScriptPosition {
                line: 13,
            },
        ),
        StmtWithPos(
            Call {
                name: String(
                    BAR,
                ),
                args: [],
                is_jump: false,
                is_method: false,
                try_body: [],
                catch_body: Some(
                    [],
                ),
            },
            ScriptPosition {
                line: 14,
            },
        ),
        StmtWithPos(
            Call {
                name: String(
                    BAR,
                ),
                args: [],
                is_jump: true,
                is_method: false,
                try_body: [],
                catch_body: None,
            },
            ScriptPosition {
                line: 17,
            },
        ),
        StmtWithPos(
            Call {
                name: String(
                    BAR,
                ),
                args: [],
                is_jump: true,
                is_method: false,
                try_body: [],
                catch_body: Some(
                    [],
                ),
            },
            ScriptPosition {
                line: 18,
            },
        ),
        StmtWithPos(
            Call {
                name: FormText(
                    BAR,
                ),
                args: [],
                is_jump: true,
                is_method: false,
                try_body: [],
                catch_body: Some(
                    [],
                ),
            },
            ScriptPosition {
                line: 19,
            },
        ),
        StmtWithPos(
            Call {
                name: String(
                    BAZ,
                ),
                args: [
                    CondExpr(
                        Int(
                            1,
                        ),
                        FormText(
                            『촉촉한 상남자』,
                        ),
                        FormText(
                            『가랑비 공주』,
                        ),
                    ),
                ],
                is_jump: false,
                is_method: false,
                try_body: [],
                catch_body: None,
            },
            ScriptPosition {
                line: 23,
            },
        ),
        StmtWithPos(
            Label(
                LABEL,
            ),
            ScriptPosition {
                line: 25,
            },
        ),
    ],
}
"
        );
    }

    #[test]
    fn test_dim() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/functions/dim.erb"#,
                ParserContext::parse_function_str
            ),
            "
Function {
    header: FunctionHeader {
        file_path: tests/parse_tests/functions/dim.erb,
        name: SYSTEM_TITLE,
        args: [],
        infos: [
            Dim(
                LocalVariable {
                    var: FOO,
                    info: VariableInfo {
                        is_chara: false,
                        is_str: false,
                        is_global: false,
                        is_savedata: false,
                        is_dynamic: false,
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
                            var: FOO,
                            func_extern: None,
                            args: [],
                        },
                    ),
                ],
            ),
            ScriptPosition {
                line: 2,
            },
        ),
    ],
}
"
        );
    }

    #[test]
    fn test_function() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/functions/function.erb"#,
                ParserContext::parse_function_str
            ),
            "
Function {
    header: FunctionHeader {
        file_path: tests/parse_tests/functions/function.erb,
        name: FOO,
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
                    Hello,
                ),
            ),
            ScriptPosition {
                line: 2,
            },
        ),
        StmtWithPos(
            Print(
                NEWLINE,
                FormText(
                    {Int(123)},
                ),
            ),
            ScriptPosition {
                line: 3,
            },
        ),
    ],
}
"
        );
    }

    #[test]
    fn test_juel() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/functions/juel.erb"#,
                ParserContext::parse_function_str
            ),
            "
Function {
    header: FunctionHeader {
        file_path: tests/parse_tests/functions/juel.erb,
        name: COMMON_MOVE_JUEL,
        args: [
            (
                Variable {
                    var: ARG,
                    func_extern: None,
                    args: [],
                },
                None,
            ),
            (
                Variable {
                    var: ARG,
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
                    var: ARG,
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
                    var: ARG,
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
                    var: ARG,
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
                    var: LOCAL,
                    func_extern: None,
                    args: [
                        Int(
                            1,
                        ),
                    ],
                },
                None,
                BuiltinMethod(
                    Limit,
                    [
                        BinopExpr(
                            Var(
                                Variable {
                                    var: JUEL,
                                    func_extern: None,
                                    args: [
                                        Var(
                                            Variable {
                                                var: ARG,
                                                func_extern: None,
                                                args: [],
                                            },
                                        ),
                                        Var(
                                            Variable {
                                                var: ARG,
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
                                    var: ARG,
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
            ScriptPosition {
                line: 6,
            },
        ),
        StmtWithPos(
            Assign(
                Variable {
                    var: LOCAL,
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
                            var: LOCAL,
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
                            var: JUEL,
                            func_extern: None,
                            args: [
                                Var(
                                    Variable {
                                        var: ARG,
                                        func_extern: None,
                                        args: [],
                                    },
                                ),
                                Var(
                                    Variable {
                                        var: ARG,
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
            ScriptPosition {
                line: 8,
            },
        ),
        StmtWithPos(
            Assign(
                Variable {
                    var: LOCALS,
                    func_extern: None,
                    args: [],
                },
                None,
                FormText(
                    {BuiltinVar(PalamName, [Var(Variable { var: ARG, func_extern: None, args: [Int(1)] })])}의 구슬{CondExpr(BinopExpr(BinopExpr(Var(Variable { var: ARG, func_extern: None, args: [Int(4)] }), Sub, BinopExpr(Var(Variable { var: ARG, func_extern: None, args: [] }), NotEqual, Var(Variable { var: TARGET, func_extern: None, args: [] }))), LessOrEqual, Int(0)), FormText(({Var(Variable { var: CALLNAME, func_extern: None, args: [Var(Variable { var: ARG, func_extern: None, args: [] })] })})), FormText())} {CondExpr(BinopExpr(BuiltinMethod(Sign, [Var(Variable { var: LOCAL, func_extern: None, args: [Int(2)] })]), Equal, Int(1)), FormText(＋), FormText(－))} {BuiltinMethod(Abs, [Var(Variable { var: LOCAL, func_extern: None, args: [Int(2)] })])},
                ),
            ),
            ScriptPosition {
                line: 12,
            },
        ),
        StmtWithPos(
            Assign(
                Variable {
                    var: JUEL,
                    func_extern: None,
                    args: [
                        Var(
                            Variable {
                                var: ARG,
                                func_extern: None,
                                args: [],
                            },
                        ),
                        Var(
                            Variable {
                                var: ARG,
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
                        var: LOCAL,
                        func_extern: None,
                        args: [
                            Int(
                                1,
                            ),
                        ],
                    },
                ),
            ),
            ScriptPosition {
                line: 14,
            },
        ),
        StmtWithPos(
            If(
                [
                    (
                        BinopExpr(
                            BuiltinMethod(
                                Abs,
                                [
                                    Var(
                                        Variable {
                                            var: LOCAL,
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
                                            var: ARG,
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
                                                                var: LOCALS,
                                                                func_extern: None,
                                                                args: [],
                                                            },
                                                        ),
                                                    ),
                                                    ScriptPosition {
                                                        line: 20,
                                                    },
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
                                                    ScriptPosition {
                                                        line: 21,
                                                    },
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
                                                                var: LOCALS,
                                                                func_extern: None,
                                                                args: [],
                                                            },
                                                        ),
                                                    ),
                                                    ScriptPosition {
                                                        line: 24,
                                                    },
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
                                                    ScriptPosition {
                                                        line: 25,
                                                    },
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
                                                ScriptPosition {
                                                    line: 28,
                                                },
                                            ),
                                        ],
                                    ),
                                ),
                                ScriptPosition {
                                    line: 17,
                                },
                            ),
                        ],
                    ),
                ],
                [],
            ),
            ScriptPosition {
                line: 16,
            },
        ),
    ],
}
"
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
            "
[
    Function {
        header: FunctionHeader {
            file_path: tests/parse_tests/programs/call_form.erb,
            name: SYSTEM_TITLE,
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
                    is_method: false,
                    try_body: [],
                    catch_body: None,
                },
                ScriptPosition {
                    line: 1,
                },
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            file_path: tests/parse_tests/programs/call_form.erb,
            name: FOO_123,
            args: [
                (
                    Variable {
                        var: ARG,
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
                        FOO_{Var(Variable { var: ARG, func_extern: None, args: [] })},
                    ),
                ),
                ScriptPosition {
                    line: 4,
                },
            ),
        ],
    },
]
"
        );
    }

    #[test]
    fn test_method_call() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/programs/method_call.erb"#,
                ParserContext::parse_program_str
            ),
            "
[
    Function {
        header: FunctionHeader {
            file_path: tests/parse_tests/programs/method_call.erb,
            name: FOO,
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Assign(
                    Variable {
                        var: A,
                        func_extern: None,
                        args: [],
                    },
                    None,
                    Method(
                        BAR,
                        [],
                    ),
                ),
                ScriptPosition {
                    line: 1,
                },
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            file_path: tests/parse_tests/programs/method_call.erb,
            name: BAR,
            args: [],
            infos: [
                Function,
            ],
        },
        body: [
            StmtWithPos(
                Command(
                    ReturnF,
                    [
                        Int(
                            123,
                        ),
                    ],
                ),
                ScriptPosition {
                    line: 5,
                },
            ),
        ],
    },
]
"
        );
    }

    #[test]
    fn test_simple() {
        k9::snapshot!(
            do_test(
                r#"tests/parse_tests/programs/simple.erb"#,
                ParserContext::parse_program_str
            ),
            "
[
    Function {
        header: FunctionHeader {
            file_path: tests/parse_tests/programs/simple.erb,
            name: FOO,
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Print(
                    (empty),
                    String(
                        foo,
                    ),
                ),
                ScriptPosition {
                    line: 1,
                },
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            file_path: tests/parse_tests/programs/simple.erb,
            name: FOO,
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Print(
                    (empty),
                    String(
                        foo,
                    ),
                ),
                ScriptPosition {
                    line: 4,
                },
            ),
        ],
    },
    Function {
        header: FunctionHeader {
            file_path: tests/parse_tests/programs/simple.erb,
            name: BAR,
            args: [],
            infos: [],
        },
        body: [
            StmtWithPos(
                Print(
                    (empty),
                    String(
                        bar,
                    ),
                ),
                ScriptPosition {
                    line: 7,
                },
            ),
        ],
    },
]
"
        );
    }
}
