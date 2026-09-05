mod alignment;
mod ast;
mod command;
mod event;
mod intern_cache;
mod interner;
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

/// Global, deduplicating store for identifiers. See [`interner`] for why it
/// keeps only one concurrent map where `lasso::ThreadedRodeo` kept two, and
/// [`literal_store`] for the sibling structure that answers the same
/// question for string literals, which need no map at all.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Interner;

impl Interner {
    pub fn new() -> Self {
        Interner
    }

    /// Get the key for a string, interning it if it does not yet exist.
    #[inline]
    pub fn get_or_intern<T: AsRef<str>>(&self, val: T) -> StrKey {
        interner::get_or_intern(val.as_ref())
    }

    /// Get the key for a `'static` string, interning it if it does not yet
    /// exist. Unlike `lasso::ThreadedRodeo::get_or_intern_static` this still
    /// copies the bytes into the arena — every call site is a compile-time
    /// constant asked for at most a handful of times per file, so the copy
    /// this trades for a second storage scheme is not worth avoiding.
    #[inline]
    pub fn get_or_intern_static(&self, val: &'static str) -> StrKey {
        interner::get_or_intern(val)
    }

    /// The key for a string that is already interned, without interning it.
    #[inline]
    pub fn get<T: AsRef<str>>(&self, val: T) -> Option<StrKey> {
        interner::get(val.as_ref())
    }

    #[inline]
    pub fn resolve<'a>(&'a self, key: &StrKey) -> &'a str {
        interner::resolve(key.to_u32())
    }

    pub fn len(&self) -> usize {
        interner::len()
    }

    pub fn is_empty(&self) -> bool {
        interner::len() == 0
    }

    /// Bytes the identifier arena has reserved. Unlike
    /// `lasso::ThreadedRodeo::current_memory_usage`, which this replaces,
    /// this can never balloon under multi-threaded contention: see
    /// [`interner::ARENA_BYTES`].
    pub fn current_memory_usage(&self) -> usize {
        interner::current_memory_usage()
    }

    /// Every registered identifier with its key, for [`crate::literal_store`]
    /// style round-tripping through a `game.era`. See
    /// [`interner::restore`] for the reader's half.
    pub fn iter(&self) -> impl Iterator<Item = (StrKey, &'static str)> {
        interner::iter()
    }

    /// Refill the store from `(key, string)` pairs an earlier [`Self::iter`]
    /// produced.
    pub fn restore(&self, pairs: &[(u32, &str)]) {
        interner::restore(pairs)
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

static mut GLOBAL_INTERNER: Option<Interner> = None;
static INIT_ONCE: std::sync::Once = std::sync::Once::new();

pub fn get_interner() -> &'static Interner {
    #[allow(static_mut_refs)]
    let opt: &'static Option<Interner> = unsafe { &GLOBAL_INTERNER };
    match opt {
        Some(i) => i,
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct StrKey(std::num::NonZeroU32);

impl StrKey {
    pub fn from_u32(n: u32) -> Self {
        Self(std::num::NonZeroU32::new(n).expect("StrKey must be non-zero"))
    }

    #[inline]
    pub fn to_u32(self) -> u32 {
        self.0.get()
    }

    /// The string this key stands for, whichever half of the store holds it.
    #[inline]
    pub fn resolve(self) -> &'static str {
        let n = self.to_u32();

        if n & LIT_BIT != 0 {
            literal_store::resolve_literal(n & !LIT_BIT)
        } else {
            interner::resolve(n)
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
        interner::get_or_intern(s)
    }
}

/// The default key resolves to whatever the first identifier ever registered
/// in this process turns out to be — a placeholder, like
/// `lasso::Spur::default()` before it, never meant to be resolved on its own.
impl Default for StrKey {
    fn default() -> Self {
        Self::from_u32(1)
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


pub fn var_name_alias(var: &str) -> &str {
    match var {
        "MAXBASE" | "UPBASE" | "DOWNBASE" | "LOSEBASE" => "BASE",
        "GOTJUEL" | "JUEL" | "UP" | "DOWN" | "CUP" | "CDOWN" => "PALAM",
        "ITEMSALES" | "ITEMPRICE" => "ITEM",
        "NOWEX" => "EX",
        _ => var,
    }
}
