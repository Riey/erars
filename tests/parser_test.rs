mod test_util;
mod body {
    use crate::test_util::do_test;
    use erars_compiler::ParserContext;

    #[test]
    fn test_alignment() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\alignment.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_assign() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\assign.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_assign_add() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\assign_add.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_assign_str() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\assign_str.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_call() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\call.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_command() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\command.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_hello() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\hello.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_if() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\if.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_number() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\number.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_print_simple() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\print_simple.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_selectcase() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\selectcase.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_sif() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\sif.erb"#,
            ParserContext::parse_body_str
        ));
    }

    #[test]
    fn test_times() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\bodys\times.erb"#,
            ParserContext::parse_body_str
        ));
    }
}
mod expr {
    use crate::test_util::do_test;
    use erars_compiler::ParserContext;

    #[test]
    fn test_boolean() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\boolean.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_complex_op() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\complex_op.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_cond() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\cond.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_method() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\method.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_plus() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\plus.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_plus_mul() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\plus_mul.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_plus_mul_paran() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\plus_mul_paran.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_str_literal() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\str_literal.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_var_arg() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\var_arg.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_var_complex() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\var_complex.erb"#,
            ParserContext::parse_expr_str
        ));
    }

    #[test]
    fn test_var_empty() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\exprs\var_empty.erb"#,
            ParserContext::parse_expr_str
        ));
    }
}
mod function {
    use crate::test_util::do_test;
    use erars_compiler::ParserContext;

    #[test]
    fn test_call() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\functions\call.erb"#,
            ParserContext::parse_function_str
        ));
    }

    #[test]
    fn test_dim() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\functions\dim.erb"#,
            ParserContext::parse_function_str
        ));
    }

    #[test]
    fn test_function() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\functions\function.erb"#,
            ParserContext::parse_function_str
        ));
    }

    #[test]
    fn test_juel() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\functions\juel.erb"#,
            ParserContext::parse_function_str
        ));
    }
}
mod program {
    use crate::test_util::do_test;
    use erars_compiler::ParserContext;

    #[test]
    fn test_call_form() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\programs\call_form.erb"#,
            ParserContext::parse_program_str
        ));
    }

    #[test]
    fn test_method_call() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\programs\method_call.erb"#,
            ParserContext::parse_program_str
        ));
    }

    #[test]
    fn test_simple() {
        k9::snapshot!(do_test(
            r#"tests\parse_tests\programs\simple.erb"#,
            ParserContext::parse_program_str
        ));
    }
}
