use codespan::Span;
use std::fmt;

pub type ParserResult<T> = Result<T, (ParserError, Span)>;

#[derive(thiserror::Error)]
pub enum ParserError {
    #[error("잘못된 코드가 발견되었습니다. `{0}`")]
    InvalidCode(String),
    #[error("예상치 못 한 토큰 `{0}`가 발견되었습니다.")]
    UnexpectedToken(String),
    #[error("토큰 `{0}`를 찾을 수 없습니다.")]
    MissingToken(String),
}

impl fmt::Debug for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
