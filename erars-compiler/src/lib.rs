mod ast;
mod compiler;
mod error;
mod event;
mod function;
mod instruction;
mod operator;
mod parser;

pub use crate::{
    ast::{Expr, Function, FunctionHeader, FunctionInfo, Stmt},
    compiler::compile,
    error::{ParserError, ParserResult},
    event::{Event, EventFlags, EventType},
    function::{EventCollection, FunctionDic},
    instruction::Instruction,
    operator::{BinaryOperator, UnaryOperator},
    parser::{
        parse_body, parse_expr, parse_function, parse_program, Alignment, Parser, PrintFlags,
    },
};
