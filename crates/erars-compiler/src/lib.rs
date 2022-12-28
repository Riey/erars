mod compiler;
mod error;
mod instruction;
mod parser;

use erars_lexer::PreprocessorRegex;
use once_cell::sync::Lazy;

pub static PP_REGEX: Lazy<PreprocessorRegex> = Lazy::new(|| {
    PreprocessorRegex::from_bytes(
        include_bytes!("../inst_re.dfa"),
        include_bytes!("../sharp_re.dfa"),
    )
});

pub use compiler::{compile, compile_expr, compile_stmt, CompiledFunction};
pub use erars_lexer::Preprocessor;
pub use error::{CompileError, CompileResult, ParserError, ParserResult};
pub use instruction::Instruction;
pub use logos::Lexer;
pub use parser::{
    normal_form_str, CharacterTemplate, DefaultLocalVarSize, EraConfig, EraConfigKey, HeaderInfo,
    Language, ParserContext, ReplaceInfo,
};
