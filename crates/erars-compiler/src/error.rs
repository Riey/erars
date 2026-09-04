use std::fmt;

use erars_ast::{BuiltinCommand, BuiltinMethod};

pub type ParserResult<T> = Result<T, ParserError>;
pub type CompileResult<T> = Result<T, CompileError>;

pub type ParserError = (String, std::ops::Range<usize>);

/// A warning plus the Emuera warning level it is reported at. Level 1 is what
/// the `[…]` preprocessor emits (`GameProc/ErbLoader.cs:154-171`, `:239-252`),
/// level 2 what the line compiler emits (`:1041-1058`);
/// `表示する最低警告レベル` filters on it (`GameData/ParserMediator.cs:26`).
pub type ParserWarning = (String, std::ops::Range<usize>, u8);

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
    #[error("메소드 {0}의 {1}번째 인수가 없습니다.")]
    NoArgumentForMethod(BuiltinMethod, usize),
    #[error("커맨드 {0}의 {1}번째 인수가 없습니다.")]
    NoArgumentForCommand(BuiltinCommand, usize),
}

impl fmt::Debug for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
