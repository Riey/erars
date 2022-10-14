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
pub use value::{InlineValue, Value};
pub use variable::*;

pub type Interner = lasso::ThreadedRodeo<StrKey>;

static mut GLOBAL_INTERNER: Option<Interner> = None;

pub fn get_interner() -> &'static Interner {
    let opt: &'static Option<Interner> = unsafe { &GLOBAL_INTERNER };
    match opt {
        Some(ref i) => i,
        None => panic!(),
    }
}

pub unsafe fn update_interner(new: Interner) {
    GLOBAL_INTERNER = Some(new);
}

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct StrKey(lasso::Spur);

impl StrKey {
    pub fn from_u32(n: u32) -> Self {
        assert_ne!(n, 0);
        unsafe { std::mem::transmute(n) }
    }

    #[inline]
    pub fn to_u32(self) -> u32 {
        unsafe { std::mem::transmute(self) }
    }

    pub fn resolve(self) -> &'static str {
        get_interner().resolve(&self)
    }

    pub fn new(s: &str) -> Self {
        get_interner().get_or_intern(s)
    }
}

impl std::fmt::Debug for StrKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(get_interner().resolve(&self))
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
