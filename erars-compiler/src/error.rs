use source_span::Span;

pub type LexicalResult<T> = Result<T, LexicalError>;

#[derive(Debug, thiserror::Error)]
pub enum LexicalError {
    #[error("예상치 못한 토큰 `{0}`가 발견되었습니다. {1}")]
    UnexpectedToken(String, Span),
}
