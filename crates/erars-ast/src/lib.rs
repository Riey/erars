mod alignment;
mod ast;
mod command;
mod event;
mod operator;
mod value;
mod variable;

pub use alignment::Alignment;
pub use ast::*;
pub use command::*;
pub use event::*;
pub use operator::*;
pub use ordered_float::NotNan;
pub use value::Value;
pub use variable::*;

pub type StrKey = lasso::Spur;
pub type Interner = lasso::ThreadedRodeo<StrKey>;

pub fn var_name_alias(var: &str) -> &str {
    match var {
        "MAXBASE" | "UPBASE" | "DOWNBASE" | "LOSEBASE" => "BASE",
        "GOTJUEL" | "JUEL" | "UP" | "DOWN" => "PALAM",
        "ITEMSALES" | "ITEMPRICE" => "ITEM",
        "NOWEX" => "EX",
        _ => var,
    }
}
