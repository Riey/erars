use std::iter;

use erars_lexer::{InstructionCode, IntoEnumIterator, SharpCode, SquareCode};
use regex_automata::dfa::{dense, regex};

fn main() {
    println!("cargo:rerun-if-changed=../erars-lexer");

    let syntax = regex_automata::SyntaxConfig::new()
        .case_insensitive(true)
        .utf8(false)
        .multi_line(false);

    let dense_config = dense::Config::new().minimize(true);

    let patterns = InstructionCode::iter()
        .map(|c| {
            if c == InstructionCode::PRINT {
                format!("^{c}(?-u:\\w*)")
            } else {
                format!("^{c}(?-u:\\b)")
            }
        })
        .collect::<Vec<_>>();

    let (mut bytes, pad) = dense::Builder::new()
        .syntax(syntax)
        .configure(dense_config)
        .build_many(&patterns)
        .unwrap()
        .to_bytes_little_endian();
    bytes.extend(iter::repeat(0u8).take(pad));

    std::fs::write("./inst.dfa", &bytes).unwrap();

    let patterns = SharpCode::iter()
        .map(|c| format!("^{c}(?-u:\\b)"))
        .collect::<Vec<_>>();

    let bytes = dense::Builder::new()
        .syntax(syntax)
        .configure(dense_config)
        .build_many(&patterns)
        .unwrap()
        .to_bytes_little_endian()
        .0;

    std::fs::write("./sharp.dfa", &bytes).unwrap();

    let patterns = SquareCode::iter()
        .map(|c| {
            if c == SquareCode::IF {
                format!("^\\[ *{c} +")
            } else {
                format!("^\\[ *{c} *\\]")
            }
        })
        .collect::<Vec<_>>();

    let re = regex::Builder::new()
        .syntax(syntax)
        .dense(dense_config)
        .build_many(&patterns)
        .unwrap();

    let fwd = re.forward().to_bytes_little_endian().0;
    let rev = re.reverse().to_bytes_little_endian().0;

    std::fs::write("./square.fwd.dfa", &fwd).unwrap();
    std::fs::write("./square.rev.dfa", &rev).unwrap();

    let pattern = "\\n[\\r\\t ]*\\[ *ENDIF *\\]";

    let re = regex::Builder::new()
        .syntax(syntax.multi_line(true))
        .dense(dense_config)
        .build(pattern)
        .unwrap();

    let fwd = re.forward().to_bytes_little_endian().0;
    let rev = re.reverse().to_bytes_little_endian().0;

    std::fs::write("./endif.fwd.dfa", &fwd).unwrap();
    std::fs::write("./endif.rev.dfa", &rev).unwrap();

    let pattern = "\\n[\\r\\t ]*\\[ *SKIPEND *\\]";

    let re = regex::Builder::new()
        .syntax(syntax.multi_line(true))
        .dense(dense_config)
        .build(pattern)
        .unwrap();

    let fwd = re.forward().to_bytes_little_endian().0;
    let rev = re.reverse().to_bytes_little_endian().0;

    std::fs::write("./skipend.fwd.dfa", &fwd).unwrap();
    std::fs::write("./skipend.rev.dfa", &rev).unwrap();
}
