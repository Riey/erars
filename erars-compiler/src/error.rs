use std::{fmt, ops::Range};

pub type ParserResult<T> = Result<T, (ParserError, Range<usize>)>;
pub type CompileResult<T> = Result<T, CompileError>;

#[derive(thiserror::Error)]
pub enum ParserError {
    #[error("알수없는 변수가 발견되었습니다. `{0}`")]
    UnknownVariable(String),
    #[error("잘못된 코드가 발견되었습니다. `{0}`")]
    InvalidCode(String),
    #[error("예상치 못 한 토큰 `{0}`가 발견되었습니다.")]
    UnexpectedToken(String),
    #[error("토큰 `{0}`를 찾을 수 없습니다.")]
    MissingToken(String),
    #[error("코드가 끝났습니다")]
    Eof,
}

impl fmt::Debug for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
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
}

impl fmt::Debug for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
