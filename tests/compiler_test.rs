use erars_compiler::{compile, parse_program, CompiledFunction};

#[test]
fn compile_test() {
    let erb_files = glob::glob("tests/compile_tests/*.erb").unwrap();

    for erb_file in erb_files {
        let erb_file = erb_file.unwrap();
        let ron_file = erb_file.parent().unwrap().join(format!(
            "{}.ron",
            erb_file.file_stem().unwrap().to_str().unwrap()
        ));
        let erb_source = std::fs::read_to_string(erb_file).unwrap();
        let ron_source = std::fs::read_to_string(ron_file).unwrap();
        let program = parse_program(&erb_source).unwrap();
        let compiled_functions = program
            .into_iter()
            .map(compile)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        let expected_functions: Vec<CompiledFunction> = ron::from_str(&ron_source).unwrap();
        k9::assert_equal!(compiled_functions, expected_functions);
    }
}
