use std::ops::Range;

use crate::{
    command::ParseContext, Expr, Function, FunctionHeader, FunctionInfo, GlobalIndex, ParserError,
    ParserResult, Stmt, UnaryOperator, Variable, VariableDic, VariableInfo,
};
use bitflags::bitflags;
use either::Either;
use ordered_float::NotNan;
use serde::{Deserialize, Serialize};
use strum::EnumString;

option_set::option_set! {
    pub struct PrintFlags: UpperSnake + u32 {
        const NEWLINE = 0x1;
        const WAIT = 0x2;
        const LEFT_ALIGN = 0x4;
        const RIGHT_ALIGN = 0x8;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize, EnumString)]
pub enum Alignment {
    #[strum(to_string = "LEFT")]
    Left,
    #[strum(to_string = "CENTER")]
    Center,
    #[strum(to_string = "RIGHT")]
    Right,
}

impl Default for Alignment {
    fn default() -> Self {
        Alignment::Left
    }
}

pub fn parse_program(s: &str, var: &VariableDic) -> ParserResult<Vec<Function>> {
    Ok(crate::command::era_program(var)(s).unwrap().1)
}

pub fn parse_body(s: &str, var: &VariableDic) -> ParserResult<Vec<Stmt>> {
    Ok(
        crate::command::body(&ParseContext::new(var, var.insert_func("TEMP")))(s)
            .unwrap()
            .1,
    )
}

pub fn parse_function(s: &str, var: &VariableDic) -> ParserResult<Function> {
    Ok(crate::command::function(var)(s).unwrap().1)
}

pub fn parse_expr(s: &str, var: &VariableDic) -> ParserResult<Expr> {
    Ok(
        crate::command::expr(&ParseContext::new(var, var.insert_func("TEMP")))(s)
            .unwrap()
            .1,
    )
}
