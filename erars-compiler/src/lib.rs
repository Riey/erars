mod ast;
mod compiler;
mod error;
mod event;
mod instruction;
mod lexer;
mod location;
mod operator;
mod parser;
mod token;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);

pub use crate::{
    ast::{Expr, Function, FunctionHeader, FunctionInfo, Stmt},
    compiler::compile,
    error::{LexicalError, LexicalResult},
    event::{Event, EventFlags, EventType},
    instruction::Instruction,
    lexer::Lexer,
    location::{Source, SourceLocation, SourceLocationMessage},
    operator::{BinaryOperator, UnaryOperator},
    parser::parse_body,
    token::{Alignment, PrintFlags, Token},
};

pub use source_span::Span;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
