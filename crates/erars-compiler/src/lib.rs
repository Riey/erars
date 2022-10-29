mod compiler;
mod error;
mod instruction;
mod parser;

pub use compiler::{compile, compile_expr, compile_stmt, CompiledFunction};
pub use error::{CompileError, CompileResult, ParserError, ParserResult};
pub use instruction::Instruction;
pub use logos::Lexer;
pub use parser::{
    normal_form_str, CharacterTemplate, DefaultLocalVarSize, EraConfig, EraConfigKey, HeaderInfo,
    Language, ParserContext, ReplaceInfo,
};
