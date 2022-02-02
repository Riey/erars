use source_span::Span;

pub type LexicalResult<T> = Result<T, LexicalError>;

#[derive(Debug, thiserror::Error)]
pub enum LexicalError {
    #[error("예상치 못한 토큰 `{0}`가 발견되었습니다. {1}")]
    UnexpectedToken(String, Span),
    #[error("올바르지 못한 숫자값입니다. {0}")]
    InvalidNumber(Span),
}
