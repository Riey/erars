use source_span::Span;
use std::fmt;

pub type LexicalResult<T> = Result<T, LexicalError>;

#[derive(thiserror::Error)]
pub enum LexicalError {
    #[error("예상치 못한 토큰 `{0}`가 발견되었습니다. {1}")]
    UnexpectedToken(String, Span),
    #[error("올바르지 못한 숫자값입니다. {0}")]
    InvalidNumber(Span),
    #[error("올바르지 못한 심볼입니다. {0}")]
    InvalidSymbol(Span),
}

impl fmt::Debug for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
