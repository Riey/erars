#[allow(non_camel_case_types)]
#[derive(
    strum::Display,
    strum::IntoStaticStr,
    strum::EnumIter,
    strum::EnumCount,
    num_derive::FromPrimitive,
    num_derive::ToPrimitive,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
)]
pub enum SharpCode {
    DEFINE,

    DIM,
    DIMS,

    LOCALSIZE,
    LOCALSSIZE,

    PRI,
    LATER,
    SINGLE,
}
