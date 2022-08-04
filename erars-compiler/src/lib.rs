mod compiler;
mod error;
mod instruction;
mod parser;

pub use compiler::{compile, CompiledFunction};
pub use error::{CompileError, CompileResult, ParseError, ParseResult};
pub use instruction::Instruction;
pub use logos::Lexer;
pub use parser::ParserContext;
