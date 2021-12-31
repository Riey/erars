// const FONT: &[u8] = include_bytes!("../res/D2Coding-Ver1.3.2-20180524.ttc");

fn main() -> anyhow::Result<()> {
    let erbs = glob::glob_with(
        "ERB/**/*.ERB",
        glob::MatchOptions {
            case_sensitive: false,
            require_literal_leading_dot: true,
            require_literal_separator: true,
        },
    )
    .unwrap();

    for erb in erbs {
        let program = erars::compiler::compile(&std::fs::read_to_string(erb?)?)?;

        dbg!(&program);
    }

    Ok(())
}
