mod alignment;
mod ast;
mod command;
mod event;
mod intern_cache;
mod literal_store;
mod operator;
mod value;
mod variable;

pub use alignment::Alignment;
pub use ast::*;
pub use command::*;
pub use event::*;
pub use intern_cache::intern_cached;
pub use literal_store::{
    append_intern, intern_literal, literal_store_len, literal_store_strings, reset_literal_store,
    restore_literals, LIT_BIT, LIT_CAP,
};
pub use operator::*;
pub use ordered_float::NotNan;
pub use value::{InlineValue, Value};
pub use variable::*;

pub type Interner = lasso::ThreadedRodeo<StrKey>;

static mut GLOBAL_INTERNER: Option<Interner> = None;
static INIT_ONCE: std::sync::Once = std::sync::Once::new();

pub fn get_interner() -> &'static Interner {
    #[allow(static_mut_refs)]
    let opt: &'static Option<Interner> = unsafe { &GLOBAL_INTERNER };
    match opt {
        Some(ref i) => i,
        None => panic!("Call init_interner or update_interner first!"),
    }
}

pub fn init_interner() {
    unsafe {
        update_interner(Interner::new());
    }
}

pub unsafe fn update_interner(new: Interner) {
    INIT_ONCE.call_once(|| {
        GLOBAL_INTERNER = Some(new);
    });
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
        self.0.into_inner().get()
    }

    /// The string this key stands for, whichever half of the store holds it.
    #[inline]
    pub fn resolve(self) -> &'static str {
        let n = self.to_u32();

        if n & LIT_BIT != 0 {
            literal_store::resolve_literal(n & !LIT_BIT)
        } else {
            get_interner().resolve(&self)
        }
    }

    /// Whether this key indexes the literal store rather than the interner.
    #[inline]
    pub fn is_literal(self) -> bool {
        self.to_u32() & LIT_BIT != 0
    }

    /// The interned key for the same string.
    ///
    /// A literal key is only ever resolved, so two occurrences of one sentence
    /// may well hold different keys. Anything that uses a key as an *identity*
    /// — the name of a function to call, of a variable to bind, of a label to
    /// jump to — has to ask for the interner's answer, which is unique.
    pub fn to_global(self) -> StrKey {
        if self.is_literal() {
            crate::intern_cached(self.resolve())
        } else {
            self
        }
    }

    pub fn new(s: &str) -> Self {
        get_interner().get_or_intern(s)
    }
}

impl std::fmt::Debug for StrKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.resolve())
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
        "GOTJUEL" | "JUEL" | "UP" | "DOWN" | "CUP" | "CDOWN" => "PALAM",
        "ITEMSALES" | "ITEMPRICE" => "ITEM",
        "NOWEX" => "EX",
        _ => var,
    }
}
