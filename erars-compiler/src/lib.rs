mod ast;
mod compiler;
mod error;
mod event;
mod instruction;
mod location;
mod operator;
mod parser;

pub use crate::{
    ast::{Expr, Function, FunctionHeader, FunctionInfo, Stmt},
    compiler::compile,
    error::{ParserError, ParserResult},
    event::{Event, EventFlags, EventType},
    instruction::Instruction,
    location::{Source, SourceLocation, SourceLocationMessage},
    operator::{BinaryOperator, UnaryOperator},
    parser::{
        parse_body, parse_expr, parse_function, parse_program, Alignment, Parser, PrintFlags,
    },
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
