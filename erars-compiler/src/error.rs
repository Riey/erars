use source_span::Span;
use std::fmt;

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(thiserror::Error)]
pub enum ParserError {
    #[error("잘못된 코드가 발견되었습니다. `{0}` {1}")]
    InvalidCode(String, Span),
    #[error("예상치 못한 토큰 `{0}`가 발견되었습니다. {1}")]
    UnexpectedToken(String, Span),
}

impl fmt::Debug for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
