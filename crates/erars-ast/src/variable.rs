use crate::{Expr, StrKey};
use serde::{Deserialize, Serialize};
use strum::{Display, EnumString, IntoStaticStr};

/// This variables are readonly system variables
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, Display, EnumString, IntoStaticStr, Serialize, Deserialize,
)]
#[strum(serialize_all = "UPPERCASE")]
#[strum(use_phf)]
#[repr(u32)]
pub enum BuiltinVariable {
    CharaNum,
    LineCount,

    Rand,

    #[strum(to_string = "LASTLOAD_VERSION")]
    LastLoadVersion,
    #[strum(to_string = "LASTLOAD_NO")]
    LastLoadNo,
    #[strum(to_string = "LASTLOAD_TEXT")]
    LastLoadText,

    /// Emuera spells this `GAMEBASE_GAMECODE`
    /// (`GameData/Variable/VariableData.cs:305`); `GAMEBASE_CODE` stays
    /// readable because erars only ever offered that spelling.
    #[strum(to_string = "GAMEBASE_GAMECODE", serialize = "GAMEBASE_CODE")]
    GamebaseCode,
    #[strum(to_string = "GAMEBASE_VERSION")]
    GamebaseVersion,
    #[strum(to_string = "GAMEBASE_ALLOWVERSION")]
    GamebaseAllowVersion,
    #[strum(to_string = "GAMEBASE_DEFAULTCHARA")]
    GamebaseDefaultChara,
    #[strum(to_string = "GAMEBASE_NOITEM")]
    GamebaseNoItem,
    #[strum(to_string = "GAMEBASE_YEAR")]
    GamebaseYear,
    #[strum(to_string = "GAMEBASE_AUTHOR")]
    GamebaseAuthor,
    #[strum(to_string = "GAMEBASE_TITLE")]
    GamebaseTitle,
    #[strum(to_string = "GAMEBASE_INFO")]
    GamebaseInfo,

    /// The baked `DRAWLINE` bar (`VariableToken.cs:1573-1584`, returning
    /// `EmueraConsole.getDefStBar`).
    #[strum(to_string = "DRAWLINESTR")]
    DrawLineStr,

    /// Emuera `ISTIMEOUTToken` (`VariableToken.cs:1658-1669`), reporting
    /// `EmueraConsole.isTimeout`: whether the last timed input expired.
    #[strum(to_string = "ISTIMEOUT")]
    IsTimeout,
    /// Emuera `MONEYLABEL_Token` (`VariableToken.cs:1560-1571`), returning
    /// `Config.MoneyLabel` — the `お金の単位` replacement, `$` by default.
    #[strum(to_string = "MONEYLABEL")]
    MoneyLabel,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Variable {
    pub var: StrKey,
    pub func_extern: Option<StrKey>,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct LocalVariable {
    pub var: StrKey,
    pub info: VariableInfo,
}

/// `size`/`init` were `Vec<u32>`/`Vec<Expr>` (64 bytes total) until this
/// comment was added; both are now inline-friendly wire-compatible
/// replacements, cutting `size_of::<VariableInfo>()` to 48 bytes with no
/// behaviour change and no save-file/`game.era` migration needed. Measured
/// across erars' two largest test corpora (16,859 and 125,549 functions):
///
/// - `size` never holds more than 3 elements in real data, and never can:
///   [`VariableInfo::calculate_single_idx`] below only has match arms for
///   0/1/2/3-element size slices and panics on anything longer, so the
///   language itself caps array declarations at 3 dimensions. `size` is
///   therefore `tinyvec::ArrayVec<[u32; 3]>` rather than `Vec<u32>`: same
///   16 bytes as a fat pointer, but inline, eliminating a heap allocation
///   for effectively every variable declared (measured: >99.9% of real
///   `#DIM`s have exactly one dimension). `finish_dim`
///   (`erars-compiler/src/parser.rs`) turns a >3-dimension `#DIM` into a
///   proper compile error before this type is ever asked to hold one.
///   `tinyvec::ArrayVec<[u32; 3]>` with the `serde` feature serialises
///   byte-identical to `Vec<u32>` via `rmp_serde` in both directions and
///   for both the empty and populated case (verified directly against
///   `rmp_serde`, not assumed from crate docs) — every existing `game.era`
///   bytecode cache and every existing player save file
///   (`erars-vm/src/save.rs`'s `SerializableVariableStorage` /
///   `SerializableGlobalVariableStorage`, which embed `VariableInfo`
///   directly) keeps loading unchanged.
/// - `init` is bounded by nothing (real data goes up to 100 elements) but
///   is almost always empty (>99.9% for locals; up to ~37% non-empty for
///   globals on the larger corpus, so it cannot be dropped). It is
///   `Option<Box<[Expr]>>` rather than `Vec<Expr>`: 16 bytes instead of 24,
///   with no allocation-behaviour change (an empty `Vec` never allocated
///   either). The wire format is *not* byte-identical for the empty case —
///   `None` encodes as a single MessagePack `nil` where an empty `Vec`
///   encoded as an empty array — but every existing save file still loads:
///   an old save's empty-array `init` deserialises into `Some(&[])` rather
///   than `None`, which every reader here treats identically to `None`
///   (see [`VariableInfo::init_exprs`]). No custom `Serialize`/`Deserialize`
///   impl or version bump was needed for either field.
///
/// Packing the 7 `bool`s above into one `bitflags` byte was measured and
/// rejected: with `size`/`init` at their current widths, the 7 loose bytes
/// already hide inside padding `default_int`'s 8-byte alignment already
/// forces, so packing them saves exactly zero bytes (measured with
/// `size_of`, not assumed) while adding a dependency and touching a lot of
/// callers for no benefit.
#[derive(Clone, Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
pub struct VariableInfo {
    pub is_chara: bool,
    pub is_str: bool,
    pub is_global: bool,
    pub is_const: bool,
    pub is_ref: bool,
    pub is_savedata: bool,
    pub is_dynamic: bool,
    pub default_int: i64,
    pub size: tinyvec::ArrayVec<[u32; 3]>,
    pub init: Option<Box<[Expr]>>,
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }

    pub fn full_size(&self) -> usize {
        self.size.iter().copied().product::<u32>() as usize
    }

    /// `init`'s expressions as a plain slice, treating `None` and
    /// `Some(&[])` identically — an old save's empty-array `init` can
    /// deserialise into the latter (see the field doc comment above), and
    /// every reader wants "no initialiser" either way.
    pub fn init_exprs(&self) -> &[Expr] {
        self.init.as_deref().unwrap_or(&[])
    }

    pub fn calculate_single_idx(&self, idxs: &[u32]) -> (Option<u32>, u32) {
        match (self.is_chara, self.size.as_slice(), idxs) {
            (true, [..], []) => (None, 0),
            (false, [..], []) => (None, 0),

            (true, [], [chara, ..]) => (Some(*chara), 0),

            (true, [_], [idx]) => (None, *idx),
            (true, [_], [chara, idx, ..]) => (Some(*chara), *idx),

            (true, [_, _], [idx]) => (None, *idx),
            (true, [_, w], [y, idx]) => (None, *w * *y + *idx),
            (true, [_, w], [chara, y, idx, ..]) => (Some(*chara), *w * *y + *idx),

            (false, [..], [idx]) => (None, *idx),

            (false, [_, w], [y, idx, ..]) => (None, *w * *y + *idx),

            (false, [_, _, w], [y, idx]) => (None, *w * *y + *idx),
            (false, [_, h, w], [z, y, idx]) => (None, *z * *w * *h + *w * *y + *idx),
            other => panic!("Invalid index for variable, {other:?}"),
        }
    }
}

#[test]
fn index_test() {
    let info = VariableInfo {
        size: tinyvec::array_vec!([u32; 3] => 1000, 1000, 1000),
        ..Default::default()
    };

    k9::assert_equal!(info.calculate_single_idx(&[]), (None, 0));
    k9::assert_equal!(info.calculate_single_idx(&[1]), (None, 1));
    k9::assert_equal!(info.calculate_single_idx(&[1, 1]), (None, 1001));
    k9::assert_equal!(info.calculate_single_idx(&[2, 1, 1]), (None, 2001001));
}
