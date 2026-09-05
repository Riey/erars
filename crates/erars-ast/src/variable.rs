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
///   `rmp_serde`, not assumed from crate docs) — every existing wire
///   consumer keeps loading unchanged: `.sav`/`global.rsav`
///   (`erars-vm/src/save.rs`'s `SerializableVariableStorage`/
///   `SerializableGlobalVariableStorage`, embedding `VariableInfo`
///   directly) and `game.era` (the `HeaderInfo`/`VariableInfo` blob
///   `erars-loader/src/lib.rs` appends via `rmp_serde` right after the
///   `erars-bytecode`-encoded function bytecode — that crate never
///   serialises a `VariableInfo` itself, so grepping it alone is
///   misleading).
/// - `init` is bounded by nothing (real data goes up to 100 elements) but
///   is almost always empty (>99.9% for locals; up to ~37% non-empty for
///   globals on the larger corpus, so it cannot be dropped). It is
///   `Option<Box<[Expr]>>` rather than `Vec<Expr>`: 16 bytes instead of 24,
///   with no allocation-behaviour change (an empty `Vec` never allocated
///   either). The wire format is *not* byte-identical for the empty case —
///   `None` encodes as a single MessagePack `nil` where an empty `Vec`
///   encoded as an empty array — so a plain derived `Deserialize` would let
///   an old save's empty-array `init` come back as `Some(Box::new([]))`
///   rather than `None`. That distinction has to stay unobservable, not
///   merely "treated the same" by whichever reader remembers to check
///   ([`VariableInfo::init_exprs`]): `VariableStorage::load_variables`
///   (`erars-vm/src/variable.rs`) restores a saved variable only when its
///   whole `VariableInfo` compares equal to the freshly parsed one via this
///   struct's derived `PartialEq`, and `None != Some(Box::new([]))` under
///   that derive — an unnormalised `init` would make every saved variable
///   with an empty initialiser silently reset to its default instead of
///   restoring the player's value. `init`'s `#[serde(deserialize_with =
///   ...)]` below collapses a deserialized empty sequence to `None` so that
///   this can never arise in the first place, and the one other place that
///   builds a `VariableInfo` by hand from a possibly-empty initialiser list
///   (`erars-compiler/src/parser/expr.rs`'s `dim_line`) normalises the same
///   way. No version bump was needed for either field.
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
    #[serde(deserialize_with = "deserialize_init")]
    pub init: Option<Box<[Expr]>>,
}

/// Collapses a deserialized `init` that arrived as an empty sequence — what
/// every save file and `game.era` written before `init` became
/// `Option<Box<[Expr]>>` wrote for "no initialiser" — down to `None`, so
/// `Some(Box::new([]))` can never arise from deserializing an old file. See
/// the struct doc comment above for why this has to happen here rather than
/// merely at the few places that read `init` back out.
fn deserialize_init<'de, D>(deserializer: D) -> Result<Option<Box<[Expr]>>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    Ok(Option::<Box<[Expr]>>::deserialize(deserializer)?.filter(|exprs| !exprs.is_empty()))
}

impl VariableInfo {
    pub fn arg_len(&self) -> usize {
        self.size.len() + self.is_chara as usize
    }

    pub fn full_size(&self) -> usize {
        self.size.iter().copied().product::<u32>() as usize
    }

    /// `init`'s expressions as a plain slice. `init` should never actually
    /// be `Some(&[])` — `deserialize_init` and `dim_line` both normalise an
    /// empty initialiser to `None` before it reaches a `VariableInfo` — but
    /// this still treats the two identically as a defensive fallback, since
    /// "no initialiser" is what every caller here wants for either shape.
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

/// Reproduces the exact wire shape pre-`VariableInfo`-shrink code wrote:
/// `init: Vec<Expr>` at the same struct position, encoded positionally by
/// `rmp_serde`'s default (non-`struct_map`) `Serializer` — the same default
/// encoding every save file and `game.era`'s trailing `VariableInfo` blob
/// use (see the struct doc comment above for exactly where each is
/// written). Field names never reach the wire in that mode, only order, so
/// this only needs to match `VariableInfo`'s current field order and
/// count.
#[derive(Serialize)]
struct OldShapeVariableInfo {
    is_chara: bool,
    is_str: bool,
    is_global: bool,
    is_const: bool,
    is_ref: bool,
    is_savedata: bool,
    is_dynamic: bool,
    default_int: i64,
    size: Vec<u32>,
    init: Vec<Expr>,
}

/// The bug this guards: a save file written before the `VariableInfo`
/// shrink always wrote `init` as an array (empty when there was no
/// initialiser, never absent). Deserializing that through a plain derived
/// `Deserialize` produces `Some(Box::new([]))`, not `None` — and
/// `VariableStorage::load_variables` compares the *whole* `VariableInfo` by
/// derived equality against the freshly parsed one to decide whether a
/// saved variable's value should be restored. `None != Some(Box::new([]))`
/// under that derive, so every saved variable with an empty initialiser
/// (i.e. nearly all of them) would silently fail the equality check and
/// get reset to its default instead of restored. `deserialize_init` exists
/// so this can never reach that comparison.
#[test]
fn deserializing_an_old_shape_empty_init_compares_equal_to_a_fresh_one() {
    let old = OldShapeVariableInfo {
        is_chara: false,
        is_str: false,
        is_global: true,
        is_const: false,
        is_ref: false,
        is_savedata: true,
        is_dynamic: false,
        default_int: 0,
        size: vec![10],
        init: Vec::new(),
    };
    let bytes = rmp_serde::to_vec(&old).expect("old-shape struct encodes");
    let deserialized: VariableInfo =
        rmp_serde::from_slice(&bytes).expect("new VariableInfo decodes the old wire shape");

    let freshly_parsed = VariableInfo {
        is_global: true,
        is_savedata: true,
        size: tinyvec::array_vec!([u32; 3] => 10),
        init: None,
        ..Default::default()
    };

    k9::assert_equal!(deserialized, freshly_parsed);
    assert!(deserialized.init.is_none(), "empty init must normalise to None, not Some(&[])");
}

/// Audits `size` for the same class of bug `init` had: `size` was
/// `Vec<u32>`, is now `tinyvec::ArrayVec<[u32; 3]>`, and — unlike `init` —
/// is not wrapped in an `Option`, so there is no second representation
/// ("absent" vs. "empty") for an old file's empty array to land on: both
/// the old `Vec<u32>` and the new `ArrayVec` decode an empty MessagePack
/// array as an empty collection and nothing else, and compare equal
/// element-by-element. Confirmed here rather than assumed, now that
/// `VariableInfo`'s equality is known to be load-bearing
/// (`VariableStorage::load_variables`) and not merely descriptive: an old
/// save's scalar (zero-dimension) variable — `size: []` — must still
/// compare equal to a freshly parsed one after the shrink.
#[test]
fn deserializing_an_old_shape_empty_size_compares_equal_to_a_fresh_one() {
    let old = OldShapeVariableInfo {
        is_chara: false,
        is_str: false,
        is_global: false,
        is_const: false,
        is_ref: false,
        is_savedata: true,
        is_dynamic: false,
        default_int: 7,
        size: Vec::new(),
        init: Vec::new(),
    };
    let bytes = rmp_serde::to_vec(&old).expect("old-shape struct encodes");
    let deserialized: VariableInfo =
        rmp_serde::from_slice(&bytes).expect("new VariableInfo decodes the old wire shape");

    let freshly_parsed = VariableInfo {
        is_savedata: true,
        default_int: 7,
        size: tinyvec::ArrayVec::new(),
        init: None,
        ..Default::default()
    };

    k9::assert_equal!(deserialized, freshly_parsed);
}

/// The complementary risk to the two tests above: `size` is a
/// `tinyvec::ArrayVec<[u32; 3]>`, and `ArrayVec::push` past its capacity
/// panics rather than growing — a process abort on malformed input, not a
/// diagnostic, if `size`'s `Deserialize` impl ever pushed one element per
/// sequence item without checking capacity first. A save file, `game.era`'s
/// trailing `VariableInfo` blob, or hand-edited `variable.yaml` carrying a
/// corrupted or future *4*-dimension `size` (the language itself never
/// writes more than 3, per
/// `VariableInfo::calculate_single_idx`'s match arms and
/// `finish_dim`'s `bail!` in `erars-compiler/src/parser.rs`) is exactly such
/// malformed input.
///
/// Confirmed directly rather than assumed: `tinyvec` 1.13.2's
/// `ArrayVecVisitor::visit_seq` (`tinyvec-1.13.2/src/arrayvec.rs:2140`)
/// checks `new_arrayvec.len() >= new_arrayvec.capacity()` *before* every
/// push and returns a proper `serde::de::Error::invalid_length` instead —
/// so this already can't panic, by construction of the dependency rather
/// than anything erars itself guarantees. That guarantee is worth pinning
/// with a real encode/decode round trip: a dependency version bump could
/// change `ArrayVec`'s `Deserialize` impl without erars' own code changing
/// at all, and the failure mode that would actually hurt a player is not a
/// panic but silent truncation — dropping the fourth dimension and loading
/// the save as if it only ever had three, corrupting the array's shape with
/// no diagnostic. Asserting `Err` specifically (not just "does not panic")
/// is what rules that out.
#[test]
fn deserializing_an_over_length_size_sequence_is_an_error_not_a_panic_or_truncation() {
    let over_long = OldShapeVariableInfo {
        is_chara: false,
        is_str: false,
        is_global: false,
        is_const: false,
        is_ref: false,
        is_savedata: false,
        is_dynamic: false,
        default_int: 0,
        size: vec![1, 2, 3, 4],
        init: Vec::new(),
    };
    let bytes = rmp_serde::to_vec(&over_long).expect("over-length struct encodes");

    let err = rmp_serde::from_slice::<VariableInfo>(&bytes)
        .expect_err("a 4-dimension size must be rejected, not silently truncated to 3");

    let message = err.to_string();
    assert!(
        message.contains("invalid length"),
        "expected tinyvec's ArrayVec capacity check (`invalid length`) to fire, got: {message}"
    );
}
