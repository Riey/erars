mod ast;
mod error;
mod lexer;
mod location;
mod parser;
mod token;
// lalrpop_util::lalrpop_mod!(grammar);

pub use crate::{
    error::{LexicalError, LexicalResult},
    lexer::Lexer,
    location::{Source, SourceLocation, SourceLocationMessage},
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
