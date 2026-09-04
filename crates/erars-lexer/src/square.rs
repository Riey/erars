/// The `[…]` preprocessor directives, spelled the way Emuera's
/// `PPState.AddKeyWord` switches on them (`GameProc/ErbLoader.cs:150-280`).
///
/// That switch reads the raw identifier, so the match is case-**sensitive**:
/// `[if_debug]` is not `IF_DEBUG` but an unrecognised preprocessor, warned
/// about and ignored. `strum::EnumString` matches the variant name exactly,
/// which is what that needs.
#[allow(non_camel_case_types)]
#[derive(
    strum::Display,
    strum::IntoStaticStr,
    strum::EnumString,
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
)]
pub enum SquareCode {
    SKIPSTART,
    IF,
    IF_DEBUG,
    // Appended below, never reordered.
    IF_NDEBUG,
    ELSEIF,
    ELSE,
    ENDIF,
    SKIPEND,
}
