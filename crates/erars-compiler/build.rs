use erars_lexer::{InstructionCode, IntoEnumIterator, SharpCode};
use regex_automata::dfa::dense;

fn main() {
    println!("cargo:rerun-if-changed=../erars-lexer");

    let patterns = InstructionCode::iter()
        .map(|c| if c == InstructionCode::PRINT {
            format!("^{c}(?-u:\\w*)")
        } else {
            format!("^{c}(?-u:\\b)")
        })
        .collect::<Vec<_>>();

    let bytes = dense::Builder::new()
        .syntax(
            regex_automata::SyntaxConfig::new()
                .case_insensitive(true)
                .utf8(false)
                .multi_line(false),
        )
        .build_many(&patterns)
        .unwrap()
        .to_bytes_little_endian()
        .0;

    std::fs::write("./inst_re.dfa", &bytes).unwrap();

    let patterns = SharpCode::iter()
        .map(|c| format!("^{c}(?-u:\\b)"))
        .collect::<Vec<_>>();

    let bytes = dense::Builder::new()
        .syntax(
            regex_automata::SyntaxConfig::new()
                .case_insensitive(true)
                .utf8(false)
                .multi_line(false),
        )
        .build_many(&patterns)
        .unwrap()
        .to_bytes_little_endian()
        .0;

    std::fs::write("./sharp_re.dfa", &bytes).unwrap();
}
