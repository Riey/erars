use crate::PrintFlags;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Print(PrintFlags, String),
}
