mod compiler;
mod error;
mod instruction;
mod parser;

pub use compiler::{compile, compile_expr, compile_stmt, CompiledErb, CompiledFunction};
pub use erars_lexer::{Bump, Preprocessor};
pub use error::{CompileError, CompileResult, ParserError, ParserResult, ParserWarning};
pub use instruction::Instruction;
pub use logos::Lexer;
pub use parser::{
    normal_form_str, CharacterTemplate, DefaultLocalVarSize, DimDecl, EraConfig, EraConfigKey,
    HeaderInfo, HeaderInfoRef, IdentifierNotFound, Language, ParserContext, PendingDim,
    ReplaceInfo, TextDrawingMode,
};
