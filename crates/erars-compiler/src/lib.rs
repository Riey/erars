mod compiler;
mod error;
mod instruction;
mod parser;

use include_bytes_aligned::include_bytes_aligned;
use once_cell::sync::Lazy;

pub static PP_REGEX: Lazy<PreprocessorRegex> = Lazy::new(|| {
    PreprocessorRegex::from_bytes(
        include_bytes_aligned!(4, "../inst.dfa"),
        include_bytes_aligned!(4, "../sharp.dfa"),
        include_bytes_aligned!(4, "../square.fwd.dfa"),
        include_bytes_aligned!(4, "../square.rev.dfa"),
        include_bytes_aligned!(4, "../endif.fwd.dfa"),
        include_bytes_aligned!(4, "../endif.rev.dfa"),
        include_bytes_aligned!(4, "../skipend.fwd.dfa"),
        include_bytes_aligned!(4, "../skipend.rev.dfa"),
    )
});

pub use compiler::{compile, compile_expr, compile_stmt, CompiledFunction};
pub use erars_lexer::{Bump, Preprocessor, PreprocessorRegex};
pub use error::{CompileError, CompileResult, ParserError, ParserResult};
pub use instruction::Instruction;
pub use logos::Lexer;
pub use parser::{
    normal_form_str, CharacterTemplate, DefaultLocalVarSize, EraConfig, EraConfigKey, HeaderInfo,
    Language, ParserContext, ReplaceInfo,
};
