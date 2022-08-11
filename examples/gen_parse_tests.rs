use std::fs;
use std::io::Write;
use std::path::Path;

fn main() -> std::io::Result<()> {
    let test_dir = "./tests";
    let test_path = Path::new(test_dir).join("parser_test.rs");
    let mut file = fs::File::create(&test_path)?;

    writeln!(file, "mod test_util;")?;

    write_mod(
        &mut file,
        "body",
        &format!("{}/parse_tests/bodys/*.erb", test_dir),
        "parse_body_str",
    )?;
    write_mod(
        &mut file,
        "expr",
        &format!("{}/parse_tests/exprs/*.erb", test_dir),
        "parse_expr_str",
    )?;
    write_mod(
        &mut file,
        "function",
        &format!("{}/parse_tests/functions/*.erb", test_dir),
        "parse_function_str",
    )?;
    write_mod(
        &mut file,
        "program",
        &format!("{}/parse_tests/programs/*.erb", test_dir),
        "parse_program_str",
    )?;

    file.flush()?;

    std::process::Command::new("rustfmt")
        .arg(&test_path)
        .spawn()?;

    Ok(())
}

fn write_mod(
    file: &mut std::fs::File,
    name: &str,
    pattern: &str,
    parse_func: &str,
) -> std::io::Result<()> {
    writeln!(file, "mod {} {{ ", name)?;
    writeln!(file, "use crate::test_util::do_test;")?;
    writeln!(file, "use erars_compiler::ParserContext;")?;

    for expr in glob::glob(pattern).unwrap() {
        let expr = expr.unwrap();

        writeln!(file)?;
        writeln!(file, "#[test]")?;
        writeln!(
            file,
            "fn test_{}() {{",
            expr.file_stem().unwrap().to_str().unwrap()
        )?;
        writeln!(
            file,
            "k9::snapshot!(do_test(r#\"{}\"#, ParserContext::{}));",
            expr.display().to_string().replace("\\", "/"),
            parse_func
        )?;
        writeln!(file, "}}")?;
        writeln!(file)?;
    }

    writeln!(file, "}}")?;

    Ok(())
}
