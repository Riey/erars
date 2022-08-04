use std::fmt;

pub type ParseResult<T> = Result<T, ParseError>;
pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Assert error on {1:?}: {0}")]
    Assert(String, std::ops::Range<usize>),
    #[error("Parsing error on {1:?}: {0}")]
    Other(String, std::ops::Range<usize>),
}

impl ParseError {
    pub fn span(&self) -> std::ops::Range<usize> {
        match self {
            ParseError::Assert(_, span) | ParseError::Other(_, span) => span.clone(),
        }
    }
}

#[derive(thiserror::Error)]
pub enum CompileError {
    #[error("중복된 GOTO 라벨입니다.")]
    DuplicatedGotoLabel,
    #[error("루프가 아닌곳에서 CONTINUE가 사용됐습니다.")]
    ContinueNotLoop,
    #[error("루프가 아닌곳에서 BREAK가 사용됐습니다.")]
    BreakNotLoop,
    #[error("FOR문의 형식이 잘못됐습니다.")]
    InvalidFor,
}

impl fmt::Debug for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
