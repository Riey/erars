mod compiler;
mod error;
mod instruction;
mod parser;

pub use compiler::{compile, CompiledFunction};
pub use error::{CompileError, CompileResult, ParserError, ParserResult};
pub use instruction::Instruction;
pub use logos::Lexer;
pub use parser::{HeaderInfo, ParserContext};
