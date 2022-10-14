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

use once_cell::sync::Lazy;

pub type Interner = lasso::ThreadedRodeo<StrKey>;

pub static GLOBAL_INTERNER: Lazy<Interner> = Lazy::new(|| Interner::new());

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct StrKey(lasso::Spur);

impl StrKey {
    pub fn resolve(self) -> &'static str {
        GLOBAL_INTERNER.resolve(&self)
    }

    pub fn new(s: &str) -> Self {
        GLOBAL_INTERNER.get_or_intern(s)
    }
}

impl std::fmt::Debug for StrKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(GLOBAL_INTERNER.resolve(&self))
    }
}

impl std::fmt::Display for StrKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl serde::Serialize for StrKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.resolve().serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for StrKey {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(Self::new(&s))
    }
}

unsafe impl lasso::Key for StrKey {
    #[inline(always)]
    fn into_usize(self) -> usize {
        self.0.into_usize()
    }

    #[inline(always)]
    fn try_from_usize(int: usize) -> Option<Self> {
        lasso::Spur::try_from_usize(int).map(Self)
    }
}

pub fn var_name_alias(var: &str) -> &str {
    match var {
        "MAXBASE" | "UPBASE" | "DOWNBASE" | "LOSEBASE" => "BASE",
        "GOTJUEL" | "JUEL" | "UP" | "DOWN" => "PALAM",
        "ITEMSALES" | "ITEMPRICE" => "ITEM",
        "NOWEX" => "EX",
        _ => var,
    }
}
