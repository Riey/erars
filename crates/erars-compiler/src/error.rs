pub type ParserResult<T> = Result<T, ParserError>;
pub type CompileResult<T> = Result<T, CompileError>;

pub type ParserError = (String, std::ops::Range<usize>);

#[derive(thiserror::Error, Debug)]
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
