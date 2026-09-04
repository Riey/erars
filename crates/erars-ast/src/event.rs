use enum_map::Enum;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumString, FromRepr, IntoStaticStr};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Event {
    pub ty: EventType,
    pub flags: EventFlags,
}

impl Event {
    pub const fn none(ty: EventType) -> Self {
        Self {
            ty,
            flags: EventFlags::None,
        }
    }

    pub const fn pre(ty: EventType) -> Self {
        Self {
            ty,
            flags: EventFlags::Pre,
        }
    }

    pub const fn later(ty: EventType) -> Self {
        Self {
            ty,
            flags: EventFlags::Later,
        }
    }

    pub const fn single(ty: EventType) -> Self {
        Self {
            ty,
            flags: EventFlags::Single,
        }
    }
}

/// Emuera's `#PRI`/`#LATER`/`#SINGLE`/`#ONLY` (`LogicalLineParser.cs:36-144`),
/// which decide the order the bodies of one event name run in
/// (`LabelDictionary.cs:99-115`: only, pri, normal, later).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EventFlags {
    None,
    Pre,
    Later,
    Single,
    /// `#ONLY`: this body runs first and the event ends when it returns
    /// (`Process.State.cs:399-400` — `called.IsOnly` → `FinishEvent()`), so no
    /// other body of the same event runs at all.
    Only,
}

#[derive(
    Enum,
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Hash,
    Display,
    EnumString,
    IntoStaticStr,
    Serialize,
    Deserialize,
    FromRepr,
)]
#[strum(use_phf)]
#[repr(u32)]
pub enum EventType {
    #[strum(to_string = "EVENTFIRST")]
    First,
    #[strum(to_string = "EVENTSHOP")]
    Shop,
    #[strum(to_string = "EVENTBUY")]
    Buy,
    #[strum(to_string = "EVENTCOM")]
    Com,
    #[strum(to_string = "EVENTCOMEND")]
    ComEnd,
    #[strum(to_string = "EVENTEND")]
    End,
    #[strum(to_string = "EVENTTURNEND")]
    TurnEnd,
    #[strum(to_string = "EVENTLOAD")]
    Load,
    #[strum(to_string = "EVENTTRAIN")]
    Train,
}
