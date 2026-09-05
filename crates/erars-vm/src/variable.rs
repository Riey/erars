use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Write as _},
    sync::Arc,
};

use anyhow::{anyhow, bail, ensure, Result};
use enum_map::{Enum, EnumMap};
use erars_ast::{get_interner, EventType, Interner, StrKey, Value, VariableInfo};
use erars_compiler::{CharacterTemplate, HeaderInfo};
use hashbrown::HashMap;
use rand::SeedableRng;
use rand_chacha::ChaCha20Rng;
use serde::{Deserialize, Serialize};

use erars_ui::VirtualConsole;
use strum::{Display, IntoStaticStr};

use crate::{
    context::FunctionIdentifier, SerializableGlobalVariableStorage, SerializableVariableStorage,
};

macro_rules! set_var {
    ($self:expr, $name:ident, $value:expr) => {
        *$self.ref_int(KnownVariableNames::$name, &[])? = $value;
    };
    (@all $self:expr, $name:ident, $value:expr) => {
        match $self.get_var(KnownVariableNames::$name)?.1 {
            UniformVariable::Character(ref mut cvar) => {
                for var in cvar {
                    var.as_int()?.fill($value);
                }
            }
            UniformVariable::Normal(ref mut var) => {
                var.as_int()?.fill($value);
            }
        }
    };
}

/// The three entry shapes Emuera's `GetCharacterDataString` produces.
#[derive(Clone, Copy)]
pub enum DataEntryStyle {
    /// `{name}LV{value} ` — PRINT_ABL, PRINT_MARK.
    Level,
    /// `{name}{value} ` — PRINT_EXP.
    Value,
    /// `[{name}]`, run together with no separator — PRINT_TALENT.
    Bracket,
}

/// A single function's `#DIM` locals.
///
/// Across real corpora (eraTHYMKR, eraMegaten) practically every function
/// carries exactly 4 entries — the builtin `LOCAL`/`LOCALS`/`ARG`/`ARGS`
/// locals `erars-vm/src/function.rs` installs whenever `VariableSize.csv`
/// enables the matching default size — plus a usually-empty tail of explicit
/// `#DIM`s (measured: p90 total entries == 4 on both corpora; only 5.05% of
/// eraMegaten's 125,549 functions and 0.04% of eraTHYMKR's 16,859 exceed 4;
/// *none* have fewer than 4). A `hashbrown::HashMap` per function turned
/// that population into one small heap allocation per function, padded to a
/// power-of-two bucket count with its own control-byte array.
///
/// A table is built once, from a complete entry list, when its owning
/// function is loaded (`VariableStorage::insert_local_table`) and never
/// grows afterward — every other access site only reads or mutates existing
/// entries in place (`get_mut`/`get_many_mut`/`iter_mut`), never inserts a
/// new key. That makes an exact-size, non-growable allocation a strict win
/// over anything carrying spare capacity: an inline/`SmallVec`-style
/// representation was measured across `INLINE_CAP` values of 0, 1, 2 and 4
/// to *lose* here specifically because this population's near-universal
/// floor of 4 entries means almost every function spills to the heap
/// regardless of inline capacity, so a larger inline array only inflates
/// every `LocalVarTable`'s fixed size — inflating the outer
/// `HashMap<StrKey, LocalVarTable>` — without ever avoiding an allocation.
/// A boxed slice pays no such tax: it carries no capacity field (unlike a
/// growable `Vec`/`SmallVec`, whose extra `capacity: usize` word exists only
/// to support future growth this table never does) and no inline array
/// (`Box<[T]>` is a 16-byte fat pointer regardless of `T`), so
/// `size_of::<LocalVarTable>()` is 32 bytes versus 336 for an
/// inline-capacity-4 `SmallVec` pair, the values array's capacity alone
/// dwarfing everything once its 4 inline `(VariableInfo,
/// Option<UniformVariable>)` slots, each 80 bytes, are counted (measured
/// with `size_of`, not estimated — this changes whenever `VariableInfo`'s
/// own size does).
///
/// Keys and values are parallel arrays (struct-of-arrays) rather than one
/// array of `(key, info, var)` tuples: a name lookup then only scans 4-32
/// bytes of `StrKey`s, never the 80-byte `(VariableInfo,
/// Option<UniformVariable>)` payload, which matters because every access
/// site here (`is_local_var`/`check_var_exists` before `get_local_var`)
/// already does a lookup-then-fetch pair. Entries are kept unsorted and
/// looked up with a linear scan rather than a sorted binary search or a
/// small `HashMap`. Measured directly (median of 3 runs,
/// `taskset -c 24-31`, 2M iterations per `n`, worst-case miss lookup):
///
/// | n  | linear    | binary   | hash     |
/// |----|-----------|----------|----------|
/// | 1  | ~0.3-0.6ns | ~0.2ns   | ~1.1ns   |
/// | 4  | ~0.5-0.6ns | ~0.8ns   | ~1.2ns   |
/// | 8  | ~0.9-1.0ns | ~1.2-1.3ns | ~1.2ns |
/// | 16 | ~1.6-1.8ns | ~1.7-2.2ns | ~1.2-1.4ns |
/// | 32 | ~3.0-3.2ns | ~2.2-2.5ns | ~1.2ns   |
/// | 64 | ~6.1-6.2ns | ~2.7-2.9ns | ~1.2-1.3ns |
/// | 90 | ~11.1-11.3ns | ~3.5-3.8ns | ~1.2ns |
///
/// Linear wins outright through n=8 and is roughly a wash with binary
/// search at n=16 (hashing is already ahead by then). It falls behind
/// binary search by n=32 and keeps falling — 2.2x slower at n=64, 3x
/// slower (and ~9x slower than hashed) at n=90, the largest population
/// either corpus actually has. Linear stays the implementation anyway
/// because 99.85% of functions (125,356 of 125,549 on the larger corpus)
/// sit at n≤16, where it is competitive-to-superior; the n=90 tail is 193
/// functions (0.15%) paying a real but absolutely tiny ~11ns, not the
/// dominant cost of a lookup that also has to hash/probe a `HashMap<StrKey,
/// LocalVarTable>` to find this table in the first place.
#[derive(Clone, Default, Debug)]
pub struct LocalVarTable {
    keys: Box<[StrKey]>,
    values: Box<[(VariableInfo, Option<UniformVariable>)]>,
}

impl LocalVarTable {
    #[inline]
    fn position(&self, key: StrKey) -> Option<usize> {
        self.keys.iter().position(|&k| k == key)
    }

    pub fn contains_key(&self, key: StrKey) -> bool {
        self.position(key).is_some()
    }

    pub fn get_mut(&mut self, key: StrKey) -> Option<&mut (VariableInfo, Option<UniformVariable>)> {
        match self.position(key) {
            Some(i) => Some(&mut self.values[i]),
            None => None,
        }
    }

    /// Builds from a complete entry list, deduplicating by key with the
    /// *last* value for a repeated key winning — matching the semantics of
    /// repeatedly calling `HashMap::insert` with the same key, which this
    /// replaced. Real corpora rely on that: a handful of functions
    /// explicitly re-`#DIM` a name that's also auto-injected as a builtin
    /// (`LOCAL`/`LOCALS`/`ARG`/`ARGS`), and `insert_local_table` reuses this
    /// to merge in the rare case a function name is compiled more than
    /// once. Populations here are tiny (near-universally 4, up to ~90 in
    /// the extreme, per this module's doc comment above), so an O(n^2)
    /// scan is negligible and — unlike a `HashMap`-based dedup — allocates
    /// nothing extra for the overwhelmingly common case with no duplicate
    /// key at all.
    fn from_entries(mut entries: Vec<(StrKey, VariableInfo)>) -> Self {
        let mut i = 0;
        while i < entries.len() {
            let key = entries[i].0;
            if entries[i + 1..].iter().any(|(k, _)| *k == key) {
                entries.remove(i);
            } else {
                i += 1;
            }
        }
        let (keys, values): (Vec<StrKey>, Vec<(VariableInfo, Option<UniformVariable>)>) =
            entries.into_iter().map(|(key, info)| (key, (info, None))).unzip();
        Self { keys: keys.into_boxed_slice(), values: values.into_boxed_slice() }
    }

    pub fn iter(&self) -> impl Iterator<Item = (StrKey, &(VariableInfo, Option<UniformVariable>))> {
        self.keys.iter().copied().zip(self.values.iter())
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (StrKey, &mut (VariableInfo, Option<UniformVariable>))> {
        self.keys.iter().copied().zip(self.values.iter_mut())
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut (VariableInfo, Option<UniformVariable>)> {
        self.values.iter_mut()
    }

    /// Mutable access to two distinct entries at once.
    ///
    /// Panics if `key1 == key2`, mirroring the previous
    /// `hashbrown::HashMap::get_many_mut`'s panic-on-duplicate-keys behavior.
    pub fn get_many_mut(
        &mut self,
        [key1, key2]: [StrKey; 2],
    ) -> [Option<&mut (VariableInfo, Option<UniformVariable>)>; 2] {
        match (self.position(key1), self.position(key2)) {
            (Some(i1), Some(i2)) => {
                assert_ne!(i1, i2, "duplicate keys found");
                let (a, b) = if i1 < i2 {
                    let (left, right) = self.values.split_at_mut(i2);
                    (&mut left[i1], &mut right[0])
                } else {
                    let (left, right) = self.values.split_at_mut(i1);
                    (&mut right[0], &mut left[i2])
                };
                [Some(a), Some(b)]
            }
            (Some(i1), None) => [Some(&mut self.values[i1]), None],
            (None, Some(i2)) => [None, Some(&mut self.values[i2])],
            (None, None) => [None, None],
        }
    }
}

#[derive(Clone)]
pub struct VariableStorage {
    interner: &'static Interner,
    header: Arc<HeaderInfo>,
    character_len: u32,
    rng: ChaCha20Rng,
    variables: HashMap<StrKey, (VariableInfo, UniformVariable)>,
    local_variables: HashMap<StrKey, LocalVarTable>,
    known_variables: EnumMap<KnownVariableNames, StrKey>,
    event_keys: EnumMap<EventType, StrKey>,
}

impl VariableStorage {
    pub fn new(header: Arc<HeaderInfo>, infos: &HashMap<StrKey, VariableInfo>) -> Self {
        let mut variables = HashMap::new();

        for (k, v) in infos {
            variables.insert(*k, (v.clone(), UniformVariable::new(&header, v)));
        }

        let interner = get_interner();

        Self {
            character_len: 0,
            header,
            rng: ChaCha20Rng::from_entropy(),
            variables,
            local_variables: HashMap::new(),
            known_variables: enum_map::enum_map! {
                v => interner.get_or_intern_static(<&str>::from(v)),
            },
            event_keys: enum_map::enum_map! {
                v => interner.get_or_intern_static(<&str>::from(v)),
            },
            interner,
        }
    }

    #[inline]
    pub fn header(&self) -> &HeaderInfo {
        &self.header
    }

    #[inline]
    pub fn interner(&self) -> &'static Interner {
        &self.interner
    }

    #[inline]
    pub fn event_key(&self, ty: EventType) -> StrKey {
        self.event_keys[ty]
    }

    #[inline]
    pub fn known_key(&self, var: KnownVariableNames) -> StrKey {
        self.known_variables[var]
    }

    #[inline]
    pub fn resolve_key(&self, key: StrKey) -> &'static str {
        key.resolve()
    }

    pub fn check_var_exists(&self, fn_name: StrKey, name: StrKey) -> bool {
        self.variables.contains_key(&name)
            || self.local_variables.get(&fn_name).map_or(false, |v| v.contains_key(name))
    }

    pub fn clear_dynamic_vars(&mut self, name: StrKey) {
        if let Some(local_dic) = self.local_variables.get_mut(&name) {
            for (_, (info, var)) in local_dic.iter_mut() {
                if info.is_dynamic {
                    // remove dynamic variable
                    *var = None;
                }
            }
        }
    }

    pub fn local_infos(
        &self,
    ) -> impl Iterator<Item = (StrKey, Vec<(StrKey, &'_ VariableInfo)>)> + '_ {
        self.local_variables.iter().map(|(func_name, vars)| {
            (*func_name, vars.iter().map(|(key, (info, _))| (key, info)).collect())
        })
    }

    fn load_variables(
        &mut self,
        mut variables: HashMap<StrKey, (VariableInfo, UniformVariable)>,
        mut local_variables: HashMap<StrKey, HashMap<StrKey, (VariableInfo, UniformVariable)>>,
        is_global: bool,
    ) {
        self.variables.iter_mut().for_each(|(name, (info, var))| {
            if info.is_global != is_global {
                return;
            }
            if let Some((sav_info, sav_var)) = variables.remove(name) {
                if *info == sav_info {
                    *var = sav_var;
                    return;
                }
            }
            *var = UniformVariable::with_character_len(&self.header, info, self.character_len);
        });

        self.local_variables.iter_mut().for_each(|(fn_name, vars)| {
            let Some(mut sav_vars) = local_variables.remove(fn_name) else {
                vars.values_mut().for_each(|v| {
                    if v.0.is_global == is_global {
                        v.1 = None;
                    }
                });
                return;
            };

            vars.iter_mut().for_each(|(name, (info, var))| {
                if info.is_global != is_global {
                    return;
                }
                if let Some((sav_info, sav_var)) = sav_vars.remove(&name) {
                    if *info == sav_info {
                        *var = Some(sav_var);
                        return;
                    }
                }
                *var = None;
            });
        });
    }

    pub fn load_global_serializable(
        &mut self,
        sav: SerializableGlobalVariableStorage,
        header: &HeaderInfo,
    ) -> Result<()> {
        self.load_variables(sav.variables, sav.local_variables, true);
        self.init_rand();
        self.init(header)?;

        Ok(())
    }

    pub fn load_serializable(
        &mut self,
        sav: SerializableVariableStorage,
        header: &HeaderInfo,
    ) -> Result<()> {
        self.character_len = sav.character_len;
        self.rng = SeedableRng::from_seed(sav.rand_seed);

        self.load_variables(sav.variables, sav.local_variables, false);
        self.init_rand();
        self.init(header)?;

        Ok(())
    }

    fn extract_var(
        &self,
        is_global: bool,
    ) -> (
        HashMap<StrKey, (VariableInfo, UniformVariable)>,
        HashMap<StrKey, HashMap<StrKey, (VariableInfo, UniformVariable)>>,
    ) {
        let this_vars = self.variables.iter();
        let this_local_vars = self.local_variables.iter();

        let variables = this_vars
            .filter_map(|(name, (info, var))| {
                if info.is_global == is_global {
                    if info.is_savedata {
                        Some((*name, (info.clone(), var.clone())))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        let local_variables = this_local_vars
            .filter_map(|(fn_name, vars)| {
                let vars: HashMap<_, _> = vars
                    .iter()
                    .filter_map(|(name, (info, var))| {
                        if info.is_global == is_global {
                            let var = if info.is_savedata {
                                var.clone()?
                            } else {
                                return None;
                            };
                            Some((name, (info.clone(), var)))
                        } else {
                            None
                        }
                    })
                    .collect();

                if vars.is_empty() {
                    None
                } else {
                    Some((*fn_name, vars))
                }
            })
            .collect();

        (variables, local_variables)
    }

    pub fn get_serializable(
        &self,
        header: &HeaderInfo,
        description: String,
    ) -> SerializableVariableStorage {
        let (variables, local_variables) = self.extract_var(false);

        SerializableVariableStorage {
            code: header.gamebase.code,
            version: header.gamebase.version,
            variables,
            description,
            local_variables,
            character_len: self.character_len,
            rand_seed: self.rng.get_seed(),
        }
    }

    pub fn get_global_serializable(
        &self,
        header: &HeaderInfo,
    ) -> SerializableGlobalVariableStorage {
        let (variables, local_variables) = self.extract_var(true);

        SerializableGlobalVariableStorage {
            code: header.gamebase.code,
            version: header.gamebase.version,
            variables,
            local_variables,
        }
    }

    pub fn rng(&mut self) -> &mut impl rand::Rng {
        &mut self.rng
    }

    pub fn dump_rand(&mut self) {
        let seed_arr = self.rng.get_seed();
        let seed = bytemuck::cast_slice(&seed_arr);
        let data = self.get_var("RANDDATA").unwrap().1.assume_normal().as_int().unwrap();
        data.copy_from_slice(seed);
    }

    pub fn init_rand(&mut self) {
        let mut seed_arr = [0u8; 32];
        let seed = self.get_var("RANDDATA").unwrap().1.assume_normal().as_int().unwrap();
        seed_arr.copy_from_slice(bytemuck::cast_slice(seed));
        self.rng = ChaCha20Rng::from_seed(seed_arr);
    }

    pub fn randomize(&mut self, val: i64) {
        self.rng = ChaCha20Rng::seed_from_u64(val as u64);
    }

    fn upcheck_internal(
        tx: &mut VirtualConsole,
        palam_name: &BTreeMap<u32, StrKey>,
        palam: &mut [i64],
        up: &mut [i64],
        down: &mut [i64],
    ) -> Result<()> {
        itertools::multizip((palam.iter_mut(), up.iter_mut(), down.iter_mut()))
            .enumerate()
            .for_each(|(no, (p, u, d))| {
                if *u == 0 && *d == 0 {
                    return;
                }

                let name = palam_name
                    .get(&(no as u32))
                    .map(|s| s.resolve())
                    .unwrap_or("");

                tx.print(format!("{name} {p}"));

                if *u != 0 {
                    tx.print(format!("+{u}"));
                }

                if *d != 0 {
                    tx.print(format!("-{d}"));
                }

                *p += *u;
                *p -= *d;

                tx.print_line(format!("={p}"));
            });

        up.fill(0);
        down.fill(0);

        Ok(())
    }

    /// `UPCHECK` — Emuera `UpdateInUpcheck`
    /// (`GameData/Variable/VariableEvaluator.cs:1538-1592`). An out-of-range
    /// `TARGET` jumps straight to the `end:` label, so the parameter walk is
    /// skipped **but UP and DOWN are still cleared**.
    pub fn upcheck(
        &mut self,
        tx: &mut VirtualConsole,
        idx: u32,
        palam_name: &BTreeMap<u32, StrKey>,
    ) -> Result<()> {
        let registered = idx < self.character_len;
        let (palam, up, down) = self.get_var3(
            KnownVariableNames::Palam,
            KnownVariableNames::Up,
            KnownVariableNames::Down,
        )?;

        if !registered {
            up.1.assume_normal().as_int()?.fill(0);
            down.1.assume_normal().as_int()?.fill(0);
            return Ok(());
        }

        let palam = palam.1.assume_chara(idx).as_int()?;
        let up = up.1.assume_normal().as_int()?;
        let down = down.1.assume_normal().as_int()?;

        Self::upcheck_internal(tx, palam_name, palam, up, down)
    }

    /// `CUPCHECK` — Emuera `CUpdateInUpcheck`
    /// (`GameData/Variable/VariableEvaluator.cs:1594-1599`). Unlike `UPCHECK`
    /// this one returns before the `end:` label, so an out-of-range target
    /// leaves CUP and CDOWN untouched.
    pub fn cupcheck(
        &mut self,
        tx: &mut VirtualConsole,
        idx: u32,
        palam_name: &BTreeMap<u32, StrKey>,
    ) -> Result<()> {
        if idx >= self.character_len {
            return Ok(());
        }

        let (palam, up, down) = self.get_var3(
            KnownVariableNames::Palam,
            KnownVariableNames::Cup,
            KnownVariableNames::Cdown,
        )?;

        let palam = palam.1.assume_chara(idx).as_int()?;
        let up = up.1.assume_chara(idx).as_int()?;
        let down = down.1.assume_chara(idx).as_int()?;

        Self::upcheck_internal(tx, palam_name, palam, up, down)
    }

    /// A `PRINTC` run ends with Emuera's `PrintFlush(false)`: emit the pending
    /// line, but never an empty one.
    fn flush_printc(tx: &mut VirtualConsole) {
        if !tx.line_is_empty() {
            tx.new_line();
        }
    }

    fn assume_registered(&self, idx: u32) -> Result<()> {
        ensure!(
            idx < self.character_len,
            "등록되지 않은 캐릭터 {idx}를 참조했습니다"
        );
        Ok(())
    }

    /// PRINT_ABL / PRINT_TALENT / PRINT_MARK / PRINT_EXP — Emuera
    /// `VariableEvaluator.GetCharacterDataString` (`:891-975`). One dense walk
    /// of the indices the character's array and the name table share, skipping
    /// zero values and nameless slots, then a single line.
    pub fn print_chara_data(
        &mut self,
        tx: &mut VirtualConsole,
        idx: u32,
        value_var: impl StrKeyLike,
        name_var: impl StrKeyLike,
        style: DataEntryStyle,
    ) -> Result<()> {
        self.assume_registered(idx)?;
        let ((_, values), (_, names)) = self.get_var2(value_var, name_var)?;
        let values = values.assume_chara(idx).as_int()?;
        let names = names.assume_normal().as_str()?;

        let mut out = String::new();
        for (value, name) in values.iter().zip(names.iter()) {
            if *value == 0 || name.is_empty() {
                continue;
            }
            match style {
                DataEntryStyle::Level => {
                    let _ = write!(out, "{name}LV{value} ");
                }
                DataEntryStyle::Value => {
                    let _ = write!(out, "{name}{value} ");
                }
                DataEntryStyle::Bracket => {
                    let _ = write!(out, "[{name}]");
                }
            }
        }

        tx.print_line(out);
        Ok(())
    }

    /// PRINT_PALAM — Emuera `Process.ScriptProc.cs:188-210` plus
    /// `VariableEvaluator.GetCharacterParamString` (`:976-1027`): a ten-cell
    /// bar graded by `PALAMLV`, laid out `printc_count` cells to the line.
    ///
    /// Emuera hard-codes the range `0..100` ("100 and up are the negative
    /// beads, don't show them") and indexes `PALAMNAME` unchecked; here a
    /// shorter `PALAM` ends the walk and a missing name reads as empty, so a
    /// resized game cannot crash the VM.
    pub fn print_palam(
        &mut self,
        tx: &mut VirtualConsole,
        idx: u32,
        printc_count: usize,
    ) -> Result<()> {
        self.assume_registered(idx)?;
        let ((_, palam), (_, names), (_, lv)) = self.get_var3(
            KnownVariableNames::Palam,
            "PALAMNAME",
            KnownVariableNames::PalamLv,
        )?;
        let palam = palam.assume_chara(idx).as_int()?;
        let names = names.assume_normal().as_str()?;
        let lv = lv.assume_normal().as_int()?;

        // `paramlv[0]` is unused; the four thresholds pick the bar character.
        let border_at = |i: usize| lv.get(i).copied().unwrap_or(0);

        let mut cell = String::new();
        let mut count = 0;
        for (no, param) in palam.iter().take(100).enumerate() {
            let name = names.get(no).map(String::as_str).unwrap_or("");
            if *param == 0 && name.is_empty() {
                continue;
            }

            let mut c = '-';
            let mut border = border_at(1);
            for next in 2..=4 {
                if *param < border {
                    break;
                }
                c = ['=', '>', '*'][next - 2];
                border = border_at(next);
            }

            cell.clear();
            cell.push_str(name);
            cell.push('[');
            if border <= 0 || border <= *param {
                cell.extend(std::iter::repeat(c).take(10));
            } else if *param <= 0 {
                cell.extend(std::iter::repeat('.').take(10));
            } else {
                // Emuera multiplies `unchecked`; the clamp only bites if that
                // wraps, where Emuera would throw out of `Append`.
                let filled = (param.wrapping_mul(10) / border).clamp(0, 10) as usize;
                cell.extend(std::iter::repeat(c).take(filled));
                cell.extend(std::iter::repeat('.').take(10 - filled));
            }
            cell.push(']');
            let _ = write!(cell, "{param:>6}");

            tx.printrc(&cell);
            count += 1;
            if printc_count > 0 && count % printc_count == 0 {
                Self::flush_printc(tx);
            }
        }

        Self::flush_printc(tx);
        Ok(())
    }

    /// PRINT_ITEM — Emuera `VariableEvaluator.GetHavingItemsString`
    /// (`:850-872`). The two labels are Emuera's own literals, not `_Replace`
    /// entries.
    pub fn print_item(&mut self, tx: &mut VirtualConsole) -> Result<()> {
        let ((_, items), (_, names)) = self.get_var2("ITEM", "ITEMNAME")?;
        let items = items.assume_normal().as_int()?;
        let names = names.assume_normal().as_str()?;

        let mut out = String::from("所持アイテム：");
        let mut count = 0;
        for (item, name) in items.iter().zip(names.iter()) {
            if *item == 0 {
                continue;
            }
            count += 1;
            let _ = write!(out, "{name}({item}) ");
        }
        if count == 0 {
            out.push_str("なし");
        }

        tx.print_line(out);
        Ok(())
    }

    /// PRINT_SHOPITEM — Emuera `Process.ScriptProc.cs:217-245`: every item
    /// with a non-zero `ITEMSALES` and a name, priced from `ITEMPRICE`, laid
    /// out `printc_count` cells to the line.
    pub fn print_shop_item(
        &mut self,
        tx: &mut VirtualConsole,
        printc_count: usize,
        money_unit: &str,
        unit_forward: bool,
    ) -> Result<()> {
        let ((_, sales), (_, names), (_, prices)) =
            self.get_var3("ITEMSALES", "ITEMNAME", "ITEMPRICE")?;
        let sales = sales.assume_normal().as_int()?;
        let names = names.assume_normal().as_str()?;
        let prices = prices.assume_normal().as_int()?;

        let len = sales.len().min(names.len()).min(prices.len());
        let mut cell = String::new();
        let mut count = 0;
        for no in 0..len {
            // Emuera `ItemSales` also demands a name: an unnamed slot is not
            // for sale however non-zero `ITEMSALES` is.
            if sales[no] == 0 || names[no].is_empty() {
                continue;
            }

            cell.clear();
            let (name, price) = (&names[no], prices[no]);
            let _ = if unit_forward {
                write!(cell, "[{no}] {name}({money_unit}{price})")
            } else {
                write!(cell, "[{no}] {name}({price}{money_unit})")
            };

            tx.printlc(&cell);
            count += 1;
            if printc_count > 0 && count % printc_count == 0 {
                Self::flush_printc(tx);
            }
        }

        Self::flush_printc(tx);
        Ok(())
    }

    pub fn prepare_train_data(&mut self) -> Result<()> {
        self.reset_var(KnownVariableNames::Up)?;
        self.reset_var(KnownVariableNames::Down)?;
        self.reset_var(KnownVariableNames::LoseBase)?;
        self.reset_var(KnownVariableNames::Cup)?;
        self.reset_var(KnownVariableNames::Cdown)?;
        self.reset_var(KnownVariableNames::DownBase)?;

        Ok(())
    }

    pub fn reset_train_data(&mut self) -> Result<()> {
        set_var!(self, AssiPlay, 0);
        set_var!(self, PrevCom, -1);
        set_var!(self, NextCom, -1);

        set_var!(@all self, Tflag, 0);
        set_var!(@all self, Tequip, 0);
        set_var!(@all self, Palam, 0);
        set_var!(@all self, Stain, 0);
        set_var!(@all self, Source, 0);
        set_var!(@all self, GotJuel, 0);

        Ok(())
    }

    pub fn get_result(&mut self) -> i64 {
        self.read_int(KnownVariableNames::Result, &[]).unwrap()
    }

    pub fn get_results(&mut self) -> String {
        self.read_str(KnownVariableNames::ResultS, &[]).unwrap()
    }

    pub fn set_result(&mut self, i: i64) {
        log::debug!("set result {i}");
        *self.ref_int(KnownVariableNames::Result, &[]).unwrap() = i;
    }

    pub fn set_results(&mut self, s: String) {
        log::debug!("set results {s}");
        *self.ref_str(KnownVariableNames::ResultS, &[]).unwrap() = s;
    }

    pub fn character_len(&self) -> u32 {
        self.character_len
    }

    /// Insert one function's complete set of `#DIM` locals at once.
    ///
    /// Callers already have every entry in hand before this is called
    /// (`function.rs::insert_compiled_func` collects them across its info
    /// loop and builtin defaults; the `game.era` bytecode loader already
    /// deserializes a whole-function `Vec`), so this never needs to grow a
    /// table one entry at a time. It still merges into any table already
    /// present for `func`, rather than overwriting it outright: a handful
    /// of real functions are compiled more than once (the same name
    /// defined in more than one source), and the previous per-entry
    /// `HashMap`-based insertion transparently accumulated every call's
    /// entries into one table — replacing wholesale here would silently
    /// drop an earlier call's locals. `LocalVarTable::from_entries`
    /// already resolves same-key collisions with last-value-wins, so
    /// appending the new entries after the existing table's own gives the
    /// same result a second `HashMap::insert` pass would have.
    pub fn insert_local_table(&mut self, func: StrKey, entries: Vec<(StrKey, VariableInfo)>) {
        match self.local_variables.get(&func) {
            Some(existing) => {
                let mut combined: Vec<(StrKey, VariableInfo)> =
                    existing.iter().map(|(key, (info, _))| (key, info.clone())).collect();
                combined.extend(entries);
                self.local_variables.insert(func, LocalVarTable::from_entries(combined));
            }
            None => {
                self.local_variables.insert(func, LocalVarTable::from_entries(entries));
            }
        }
    }

    /// Reserve space for `additional` more functions' local-variable tables.
    ///
    /// Each entry's value is now a `LocalVarTable` (bigger than the old
    /// `HashMap`'s 40-byte handle, since up to 4 locals live inline) so
    /// letting this table grow one `entry().or_default()` at a time forces
    /// repeated whole-table rehashes, each copying an increasingly heavy
    /// per-row payload — call this once with the known function count
    /// (callers already have it before their insertion loop) to avoid that.
    pub fn reserve_local_functions(&mut self, additional: usize) {
        self.local_variables.reserve(additional);
    }

    pub fn ref_int(&mut self, name: impl StrKeyLike, args: &[u32]) -> Result<&mut i64> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(&mut var.as_int()?[idx as usize])
    }

    pub fn ref_local_int(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<&mut i64> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(&mut var.as_int()?[idx as usize])
    }

    pub fn ref_maybe_local_int(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<&mut i64> {
        if self.is_local_var(func_name, name) {
            self.ref_local_int(func_name, name, args)
        } else {
            self.ref_int(name, args)
        }
    }

    pub fn ref_str(&mut self, name: impl StrKeyLike, args: &[u32]) -> Result<&mut String> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(&mut var.as_str()?[idx as usize])
    }

    pub fn ref_local_str(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<&mut String> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(&mut var.as_str()?[idx as usize])
    }

    pub fn ref_maybe_local_str(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<&mut String> {
        if self.is_local_var(func_name, name) {
            self.ref_local_str(func_name, name, args)
        } else {
            self.ref_str(name, args)
        }
    }

    pub fn read_int(&mut self, name: impl StrKeyLike, args: &[u32]) -> Result<i64> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(var.as_int()?[idx as usize])
    }

    pub fn read_local_int(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<i64> {
        let (_, var, idx) = self.index_local_var(func_name, name, args)?;
        Ok(var.as_int()?[idx as usize])
    }

    pub fn read_maybe_local_int(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<i64> {
        if self.is_local_var(func_name, name) {
            self.read_local_int(func_name, name, args)
        } else {
            self.read_int(name, args)
        }
    }

    pub fn read_str(&mut self, name: impl StrKeyLike, args: &[u32]) -> Result<String> {
        let (_, var, idx) = self.index_var(name, args)?;
        Ok(var.as_str()?[idx as usize].clone())
    }

    pub fn index_var(
        &mut self,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, u32)> {
        let name = name.get_key(self);
        let target_key = self.known_key(KnownVariableNames::Target);

        let target = if name != target_key {
            self.read_int(target_key, &[])?
        } else {
            // NEED for break recursion
            -1
        };

        let (info, var) = self.get_var(name)?;

        let (c_idx, idx) = info.calculate_single_idx(args);

        let vm_var = match var {
            UniformVariable::Character(cvar) => {
                let c_idx = c_idx.unwrap_or_else(|| target as u32);
                cvar.get_mut(c_idx as usize).ok_or_else(|| {
                    anyhow!("Variable {name:?} Character index {c_idx} not exists")
                })?
            }
            UniformVariable::Normal(var) => var,
        };

        Ok((info, vm_var, idx))
    }

    pub fn index_local_var(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, u32)> {
        let func_name = func_name.get_key(self);
        let name = name.get_key(self);

        let target = self.read_int("TARGET", &[])?;

        let (info, var) = self.get_local_var(func_name, name)?;

        let (c_idx, idx) = info.calculate_single_idx(args);

        let vm_var = match var {
            UniformVariable::Character(cvar) => {
                let c_idx = c_idx.unwrap_or(target as u32);
                cvar.get_mut(c_idx as usize).ok_or_else(|| {
                    anyhow!("Variable {name:?}@{func_name:?} Character index {c_idx} not exists",)
                })?
            }
            UniformVariable::Normal(var) => var,
        };

        Ok((info, vm_var, idx))
    }

    pub fn index_maybe_local_var(
        &mut self,
        func_name: impl StrKeyLike,
        name: impl StrKeyLike,
        args: &[u32],
    ) -> Result<(&mut VariableInfo, &mut VmVariable, u32)> {
        if self.is_local_var(func_name, name) {
            self.index_local_var(func_name, name, args)
        } else {
            self.index_var(name, args)
        }
    }

    pub fn get_local_var(
        &mut self,
        func_name: impl StrKeyLike,
        var: impl StrKeyLike,
    ) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        let func_name = func_name.get_key(self);
        let var = var.get_key(self);
        let (info, var) = self
            .local_variables
            .get_mut(&func_name)
            .unwrap()
            .get_mut(var)
            .ok_or_else(|| anyhow!("Variable {:?} is not exists", var))?;

        let var = var.get_or_insert_with(|| {
            UniformVariable::with_character_len(&self.header, info, self.character_len)
        });
        Ok((info, var))
    }

    pub fn is_local_var(&self, func: impl StrKeyLike, var: impl StrKeyLike) -> bool {
        match self.local_variables.get(&func.get_key(self)) {
            Some(v) => v.contains_key(var.get_key(self)),
            None => false,
        }
    }

    pub fn get_maybe_local_var2(
        &mut self,
        func1: impl StrKeyLike,
        var1: impl StrKeyLike,
        func2: impl StrKeyLike,
        var2: impl StrKeyLike,
    ) -> Result<[(&mut VariableInfo, &mut UniformVariable); 2]> {
        let func1_name = func1.get_key(self);
        let func2_name = func2.get_key(self);
        let var1 = var1.get_key(self);
        let var2 = var2.get_key(self);

        match (
            self.is_local_var(func1_name, var1),
            self.is_local_var(func2_name, var2),
        ) {
            (true, true) if func1_name == func2_name => {
                let [Some((info1, var1)), Some((info2, var2))] = self
                    .local_variables
                    .get_mut(&func1_name)
                    .unwrap()
                    .get_many_mut([var1, var2])
                else {
                    bail!("Variable {var1:?} and {var2:?} are not exist")
                };

                let var1 = var1.get_or_insert_with(|| {
                    UniformVariable::with_character_len(&self.header, info1, self.character_len)
                });
                let var2 = var2.get_or_insert_with(|| {
                    UniformVariable::with_character_len(&self.header, info2, self.character_len)
                });

                Ok([(info1, var1), (info2, var2)])
            }
            (true, true) => {
                let [Some(dic1), Some(dic2)] =
                    self.local_variables.get_many_mut([&func1_name, &func2_name])
                else {
                    bail!("No function name")
                };

                let (info1, var1) = dic1
                    .get_mut(var1)
                    .ok_or_else(|| anyhow!("Variable {var1:?} is not exist"))?;

                let (info2, var2) = dic2
                    .get_mut(var2)
                    .ok_or_else(|| anyhow!("Variable {var2:?} is not exist"))?;

                let var1 = var1.get_or_insert_with(|| {
                    UniformVariable::with_character_len(&self.header, info1, self.character_len)
                });
                let var2 = var2.get_or_insert_with(|| {
                    UniformVariable::with_character_len(&self.header, info2, self.character_len)
                });

                Ok([(info1, var1), (info2, var2)])
            }
            (false, false) => {
                let [Some((info1, var1)), Some((info2, var2))] =
                    self.variables.get_many_mut([&var1, &var2])
                else {
                    bail!("Variable {var1:?} and {var2:?} are not exist")
                };

                Ok([(info1, var1), (info2, var2)])
            }
            (var1_is_local, _) => {
                let (local_var, global_var, func_name) = if var1_is_local {
                    (var1, var2, func1_name)
                } else {
                    (var2, var1, func2_name)
                };

                let (local_info, local_var) = self
                    .local_variables
                    .get_mut(&func_name)
                    .unwrap()
                    .get_mut(local_var)
                    .ok_or_else(|| anyhow!("Variable {local_var:?} is not exist"))?;

                let local_var = local_var.get_or_insert_with(|| {
                    UniformVariable::with_character_len(
                        &self.header,
                        local_info,
                        self.character_len,
                    )
                });

                let (info, var) = self
                    .variables
                    .get_mut(&global_var)
                    .ok_or_else(|| anyhow!("Variable {global_var:?} is not exist"))?;

                if var1_is_local {
                    Ok([(local_info, local_var), (info, var)])
                } else {
                    Ok([(info, var), (local_info, local_var)])
                }
            }
        }
    }

    pub fn get_maybe_local_var(
        &mut self,
        func: impl StrKeyLike,
        var: impl StrKeyLike,
    ) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        if self.is_local_var(func, var) {
            self.get_local_var(func, var)
        } else {
            self.get_var(var)
        }
    }

    pub fn reset_var(&mut self, var: impl StrKeyLike) -> Result<()> {
        let (info, var) = self.get_var(var)?;

        ensure!(!info.is_const, "Cannot reset const variable");

        if info.is_str {
            match var {
                UniformVariable::Character(c) => {
                    c.iter_mut().for_each(|v| v.as_str().unwrap().fill(String::new()))
                }
                UniformVariable::Normal(v) => v.as_str().unwrap().fill(String::new()),
            }
        } else {
            match var {
                UniformVariable::Character(c) => {
                    c.iter_mut().for_each(|v| v.as_int().unwrap().fill(info.default_int))
                }
                UniformVariable::Normal(v) => v.as_int().unwrap().fill(info.default_int),
            }
        }

        Ok(())
    }

    pub fn get_var(
        &mut self,
        var: impl StrKeyLike,
    ) -> Result<(&mut VariableInfo, &mut UniformVariable)> {
        let var = var.get_key(self);
        let (l, r) = self
            .variables
            .get_mut(&var)
            .ok_or_else(|| anyhow!("Variable {var:?} is not exists"))?;

        Ok((l, r))
    }

    pub fn get_var2(
        &mut self,
        v1: impl StrKeyLike,
        v2: impl StrKeyLike,
    ) -> Result<(
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
    )> {
        match self.variables.get_many_mut([&v1.get_key(self), &v2.get_key(self)]) {
            [Some((ll, lr)), Some((rl, rr))] => Ok(((ll, lr), (rl, rr))),
            _ => {
                bail!("Variable {v1:?} or {v2:?} is not exists");
            }
        }
    }

    pub fn get_var3(
        &mut self,
        v1: impl StrKeyLike,
        v2: impl StrKeyLike,
        v3: impl StrKeyLike,
    ) -> Result<(
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
        (&mut VariableInfo, &mut UniformVariable),
    )> {
        match self
            .variables
            .get_many_mut([&v1.get_key(self), &v2.get_key(self), &v3.get_key(self)])
        {
            [Some((l1, r1)), Some((l2, r2)), Some((l3, r3))] => Ok(((l1, r1), (l2, r2), (l3, r3))),
            _ => {
                bail!("Variable {v1:?} or {v2:?} or {v3:?} is not exists");
            }
        }
    }

    pub fn reset_data(&mut self, header: &HeaderInfo) -> Result<()> {
        self.character_len = 0;
        for var in self.variables.values_mut() {
            var.1 = UniformVariable::new(header, &var.0);
        }
        self.init(header)?;

        Ok(())
    }

    pub fn swap_chara(&mut self, a: u32, b: u32) {
        self.variables.values_mut().for_each(|(_, var)| {
            var.swap_chara(a, b);
        });
    }

    pub fn add_chara(&mut self) {
        self.character_len += 1;
        self.variables.values_mut().for_each(|(info, var)| {
            var.add_chara(&self.header, info);
        });
    }

    pub fn add_copy_chara(&mut self, idx: u32) {
        self.character_len += 1;
        self.variables.values_mut().for_each(|(_, var)| {
            var.add_copy_chara(idx);
        });
    }

    pub fn copy_chara(&mut self, from: u32, to: u32) {
        self.variables.values_mut().for_each(|(_, var)| {
            var.copy_chara(from, to);
        });
    }

    pub fn del_chara(&mut self, idx: u32) {
        self.character_len -= 1;
        self.variables.values_mut().for_each(|(_, var)| {
            var.del_chara(idx);
        });
    }

    /// `DELCHARA` with several arguments — remove every listed character.
    pub fn del_chara_list(&mut self, list: &BTreeSet<u32>) {
        self.character_len -= list.len() as u32;
        self.variables.values_mut().for_each(|(_, var)| {
            var.del_chara_list(list);
        });
    }

    /// `PICKUPCHARA` — keep only the listed characters.
    pub fn pickup_chara(&mut self, list: &BTreeSet<u32>) {
        self.character_len = list.len() as u32;
        self.variables.values_mut().for_each(|(_, var)| {
            var.pickup_chara(list);
        });
    }

    /// `DELALLCHARA` — Emuera `VariableEvaluator.DelAllCharacter`.
    pub fn del_all_chara(&mut self) {
        self.character_len = 0;
        self.variables.values_mut().for_each(|(_, var)| {
            if let UniformVariable::Character(charas) = var {
                charas.clear();
            }
        });
    }

    /// Every savedata character variable of `idx`, as a `SAVECHARA` row.
    pub fn extract_chara(&self, idx: u32) -> HashMap<StrKey, VmVariable> {
        self.variables
            .iter()
            .filter_map(|(name, (info, var))| {
                if !info.is_savedata {
                    return None;
                }
                match var {
                    UniformVariable::Character(charas) => {
                        Some((*name, charas.get(idx as usize)?.clone()))
                    }
                    UniformVariable::Normal(_) => None,
                }
            })
            .collect()
    }

    /// Overwrite character `idx` from a `SAVECHARA` row.
    ///
    /// Variables the save does not know keep the value they were created with,
    /// and a stored row longer or shorter than the current variable is copied
    /// element-wise up to the shorter length — a CSV resize between saving and
    /// loading must not lose the whole variable.
    pub fn restore_chara(&mut self, idx: u32, row: HashMap<StrKey, VmVariable>) {
        for (name, saved) in row {
            let Some((_, UniformVariable::Character(charas))) = self.variables.get_mut(&name)
            else {
                continue;
            };
            let Some(cur) = charas.get_mut(idx as usize) else {
                continue;
            };

            if !cur.overwrite_from(saved) {
                log::warn!("LOADCHARA: 변수 {name}의 타입이 세이브와 다릅니다");
            }
        }
    }

    /// `SAVEVAR` — the whole array of one savable global variable.
    ///
    /// Emuera saves `var.GetArray()` and its `SP_SAVEVAR` builder rejects
    /// character, private, local, const, pseudo and reference variables
    /// (`ArgumentBuilder.cs:2008-2019`). Its load path resolves names in the
    /// global scope only (`LoadVariableBinary` passes a null function scope),
    /// so a function-scoped variable could never round-trip and is rejected
    /// here too.
    pub fn extract_global_var(&self, name: StrKey) -> Result<VmVariable> {
        let Some((info, var)) = self.variables.get(&name) else {
            bail!("전역 변수 {name}를 찾을 수 없습니다");
        };
        ensure!(!info.is_const, "상수 {name}는 저장할 수 없습니다");
        ensure!(!info.is_ref, "참조 변수 {name}는 저장할 수 없습니다");

        match var {
            UniformVariable::Normal(v) => Ok(v.clone()),
            UniformVariable::Character(_) => bail!("캐릭터 변수 {name}는 저장할 수 없습니다"),
        }
    }

    /// `LOADVAR` — write one saved array back.
    ///
    /// Emuera's `LoadVariableBinary` silently skips a name that is no longer a
    /// plain global variable, so a stale file can never corrupt state.
    pub fn restore_global_var(&mut self, name: StrKey, saved: VmVariable) {
        let Some((info, UniformVariable::Normal(cur))) = self.variables.get_mut(&name) else {
            log::warn!("LOADVAR: 변수 {name}를 불러올 수 없습니다");
            return;
        };
        if info.is_const || info.is_ref {
            log::warn!("LOADVAR: 변수 {name}를 불러올 수 없습니다");
            return;
        }
        if !cur.overwrite_from(saved) {
            log::warn!("LOADVAR: 변수 {name}의 타입이 세이브와 다릅니다");
        }
    }

    /// `RESETGLOBAL` — Emuera `VariableData.SetDefaultGlobalValue`: every
    /// global variable, built-in `GLOBAL`/`GLOBALS` or `#DIM GLOBAL`, goes back
    /// to its declared default. The saved global file is untouched.
    pub fn reset_global_data(&mut self) {
        let header = self.header.clone();

        for (info, var) in self.variables.values_mut() {
            if info.is_global {
                *var = UniformVariable::new(&header, info);
            }
        }

        // Dropping a function-scoped global makes `get_local_var` rebuild it
        // from its declaration on the next access.
        for vars in self.local_variables.values_mut() {
            for (info, var) in vars.values_mut() {
                if info.is_global {
                    *var = None;
                }
            }
        }
    }

    pub fn get_chara(&mut self, target: i64) -> Result<Option<usize>> {
        let (_, no_var) = self.get_var(KnownVariableNames::No)?;
        match no_var {
            UniformVariable::Character(c) => {
                for (idx, var) in c.iter_mut().enumerate() {
                    if var.as_int()?[0] == target {
                        return Ok(Some(idx));
                    }
                }

                Ok(None)
            }
            UniformVariable::Normal(_) => bail!("NO can't be normal variable"),
        }
    }

    /// Emuera `GetChara_UseSp` (`GameData/Variable/VariableEvaluator.cs:1321-1338`):
    /// the first *registered* character whose `NO` matches and whose SP-ness
    /// agrees with the request, where being an SP character at run time means
    /// `CFLAG:0` is non-zero. `GETSPCHARA` is the only caller; `GETCHARA`
    /// keeps ignoring SP-ness, as it always has.
    pub fn get_chara_with_sp(&mut self, target: i64, want_sp: bool) -> Result<Option<usize>> {
        for idx in 0..self.character_len() {
            let no = self
                .get_var(KnownVariableNames::No)?
                .1
                .assume_chara(idx)
                .as_int()?[0];
            if no != target {
                continue;
            }
            let is_sp = self.get_var("CFLAG")?.1.assume_chara(idx).as_int()?[0] != 0;
            if is_sp == want_sp {
                return Ok(Some(idx as usize));
            }
        }

        Ok(None)
    }

    pub fn init(&mut self, header: &HeaderInfo) -> Result<()> {
        macro_rules! set {
            ($name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_normal().as_int()?;
                let arr = &header.replace.$field;

                var[..arr.len()].copy_from_slice(arr);
            };
        }

        set!(KnownVariableNames::PalamLv, palamlv_init);
        set!(KnownVariableNames::ExpLv, explv_init);

        // Init RANDDATA with fresh rng
        self.dump_rand();
        self.get_var("RELATION")?.0.default_int = header.replace.relation_init;
        *self.ref_int("PBAND", &[])? = header.replace.pband_init;

        let str = self.get_var("STR")?.1.assume_normal().as_str()?;
        for (n, s) in header.str_templates.iter() {
            str[*n as usize] = s.clone();
        }

        const NAMES: &[(&str, &str)] = &[
            ("ABLNAME", "ABL"),
            ("BASENAME", "BASE"),
            ("TALENTNAME", "TALENT"),
            ("ITEMNAME", "ITEM"),
            ("FLAGNAME", "FLAG"),
            ("EXNAME", "EX"),
            ("EXPNAME", "EXP"),
            ("CFLAGNAME", "CFLAG"),
            ("CSTRNAME", "CSTR"),
            ("STRNAME", "STR"),
            ("TSTRNAME", "TSTR"),
            ("EQUIPNAME", "EQUIP"),
            ("TEQUIPNAME", "TEQUIP"),
            ("TRAINNAME", "TRAIN"),
            ("PALAMNAME", "PALAM"),
            ("SOURCENAME", "SOURCE"),
            ("STAINNAME", "STAIN"),
            ("TCVARNAME", "TCVAR"),
            ("GLOBALNAME", "GLOBAL"),
            ("GLOBALSNAME", "GLOBALS"),
            ("MARKNAME", "MARK"),
            ("SAVESTRNAME", "SAVESTR"),
            ("TFLAGNAME", "TFLAG"),
            ("CDFLAGNAME1", "CDFLAG1"),
            ("CDFLAGNAME2", "CDFLAG2"),
        ];

        for (var_name, var) in NAMES {
            let var = self.interner().get_or_intern_static(var);
            let arr = self.get_var(*var_name)?.1.assume_normal().as_str()?;
            if let Some(var_name_var) = header.var_name_var.get(&var) {
                for (idx, name) in var_name_var.iter() {
                    if *idx as usize >= arr.len() {
                        log::error!(
                            "Index out of range: `{}` {} is out range of {}",
                            var_name,
                            idx,
                            arr.len()
                        );
                        continue;
                    }
                    arr[*idx as usize] = name.to_string();
                }
            }
        }

        let price = self.get_var("ITEMPRICE")?.1.assume_normal().as_int()?;
        for (idx, value) in header.item_price.iter() {
            price[*idx as usize] = *value as i64;
        }

        Ok(())
    }

    pub fn set_character_template(&mut self, idx: u32, template: &CharacterTemplate) -> Result<()> {
        macro_rules! set {
            (@int $name:expr, $field:ident) => {
                self.get_var($name)?.1.assume_chara(idx).as_int()?[0] = template.$field as i64;
            };
            (@str $name:expr, $field:ident) => {
                self.get_var($name)?.1.assume_chara(idx).as_str()?[0] = template.$field.clone();
            };
            (@intarr $name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_chara(idx).as_int()?;

                for (k, v) in template.$field.iter() {
                    var[*k as usize] = *v as i64;
                }
            };
            (@strarr $name:expr, $field:ident) => {
                let var = self.get_var($name)?.1.assume_chara(idx).as_str()?;

                for (k, v) in template.$field.iter() {
                    var[*k as usize] = v.clone();
                }
            };
        }

        set!(@int "NO", no);
        set!(@int "ISASSI", is_assi);

        set!(@str "NAME", name);
        set!(@str "CALLNAME", call_name);
        set!(@str "NICKNAME", nick_name);

        set!(@intarr "ABL", abl);
        set!(@intarr "MAXBASE", base);
        set!(@intarr "BASE", base);
        set!(@intarr "EXP", exp);
        set!(@intarr "EX", ex);
        set!(@intarr "MARK", mark);
        set!(@intarr "TALENT", talent);
        set!(@intarr "CFLAG", cflag);
        // `装着物`/`EQUIP` and `珠`/`JUEL` are chara-CSV keys in Emuera
        // (`GameData/ConstantData.cs:1585-1598`) and were parsed into the
        // template but never stamped onto the character.
        set!(@intarr "EQUIP", equip);
        set!(@intarr "JUEL", juel);
        set!(@intarr "RELATION", relation);

        set!(@strarr "CSTR", cstr);

        Ok(())
    }
}

#[cfg(test)]
mod load_variables_tests {
    use super::*;
    use erars_compiler::HeaderInfo;

    fn savedata_info() -> VariableInfo {
        VariableInfo {
            is_global: false,
            is_savedata: true,
            default_int: 0,
            size: tinyvec::array_vec!([u32; 3] => 1),
            ..Default::default()
        }
    }

    /// Mirrors the exact wire shape pre-`VariableInfo`-shrink code wrote —
    /// `init: Vec<Expr>` at the same struct position — so that serializing
    /// this and deserializing it back into today's `VariableInfo` (through
    /// `rmp_serde`, the same encoder `erars-vm/src/save.rs` uses for every
    /// save file) reproduces precisely what loading a pre-existing save
    /// produces, `deserialize_init` included, rather than a hand-picked
    /// stand-in for it.
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
        init: Vec<erars_ast::Expr>,
    }

    fn old_shape_saved_info(info: &VariableInfo) -> VariableInfo {
        let old = OldShapeVariableInfo {
            is_chara: info.is_chara,
            is_str: info.is_str,
            is_global: info.is_global,
            is_const: info.is_const,
            is_ref: info.is_ref,
            is_savedata: info.is_savedata,
            is_dynamic: info.is_dynamic,
            default_int: info.default_int,
            size: info.size.to_vec(),
            init: Vec::new(),
        };
        let bytes = rmp_serde::to_vec(&old).expect("old-shape struct encodes");
        rmp_serde::from_slice(&bytes).expect("VariableInfo decodes the old wire shape")
    }

    /// Defends the actual bug: `VariableStorage::load_variables` restores a
    /// saved variable only when `*info == sav_info` (this file, above). A
    /// save file written before the `VariableInfo` shrink always
    /// serialised `init` as an array, empty when there was no initialiser —
    /// deserializing that through a plain derived `Deserialize` (i.e.
    /// without `deserialize_init`) comes back `Some(Box::new([]))`, not
    /// `None`, and `None != Some(Box::new([]))` under the derived equality
    /// `load_variables` uses. Before `deserialize_init` existed, that made
    /// this test fail: the saved value was silently discarded and the
    /// variable reset to its default. `old_shape_saved_info` reproduces the
    /// old save's actual bytes (not a hand-picked stand-in for them) via a
    /// real `rmp_serde` round trip, and this drives them through the real
    /// `load_variables` a save/load actually calls, not a mock of it.
    #[test]
    fn an_old_shape_saved_variable_with_empty_init_restores_its_value() {
        erars_ast::init_interner();
        let header = Arc::new(HeaderInfo::default());
        let name = get_interner().get_or_intern_static("TESTSAVEDATA");

        let fresh_info = savedata_info();
        let mut infos = HashMap::new();
        infos.insert(name, fresh_info.clone());
        let mut storage = VariableStorage::new(header.clone(), &infos);

        let old_shape_info = old_shape_saved_info(&fresh_info);
        assert_eq!(
            old_shape_info, fresh_info,
            "an old save's empty init must normalise to compare equal to a freshly parsed one"
        );

        let saved_value = 42;
        let mut sav_var = UniformVariable::new(&header, &old_shape_info);
        sav_var.assume_normal().as_int().unwrap()[0] = saved_value;

        let mut sav_variables = HashMap::new();
        sav_variables.insert(name, (old_shape_info, sav_var));

        storage.load_variables(sav_variables, HashMap::new(), false);

        let restored = storage.get_var(name).unwrap().1.assume_normal().as_int().unwrap()[0];
        assert_eq!(
            restored, saved_value,
            "an old-shape save's empty-init variable must restore its value, not reset to default"
        );
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum VmVariable {
    Int(Vec<i64>),
    Str(Vec<String>),
}

impl VmVariable {
    pub fn new(header: &HeaderInfo, info: &VariableInfo) -> Self {
        let size = info.full_size();

        let mut ret = match info.is_str {
            false => Self::Int(vec![info.default_int; size]),
            true => Self::Str(vec![String::new(); size]),
        };

        for (idx, init_var) in info.init_exprs().iter().enumerate() {
            let _ = ret.set(idx as u32, header.const_eval_log_error(init_var));
        }

        ret
    }

    /// Overwrite from a saved array, element-wise up to the shorter length: a
    /// CSV resize between saving and loading must lose only the tail, never the
    /// whole variable. `false` when the saved array has the other type.
    pub fn overwrite_from(&mut self, saved: Self) -> bool {
        match (self, saved) {
            (Self::Int(cur), Self::Int(saved)) => {
                let len = cur.len().min(saved.len());
                cur[..len].copy_from_slice(&saved[..len]);
                true
            }
            (Self::Str(cur), Self::Str(saved)) => {
                for (cur, saved) in cur.iter_mut().zip(saved) {
                    *cur = saved;
                }
                true
            }
            _ => false,
        }
    }

    pub fn get(&self, idx: u32) -> Result<Value> {
        match self {
            Self::Int(i) => i
                .get(idx as usize)
                .ok_or_else(|| anyhow!("Variable out of range {} over {}", idx, i.len()))
                .copied()
                .map(Value::Int),
            Self::Str(i) => i
                .get(idx as usize)
                .ok_or_else(|| anyhow!("Variable out of range {} over {}", idx, i.len()))
                .cloned()
                .map(Value::String),
        }
    }

    pub fn set_or_default(&mut self, idx: u32, value: Option<Value>) -> Result<()> {
        match value {
            Some(v) => self.set(idx, v)?,
            None => match self {
                Self::Int(i) => {
                    *i.get_mut(idx as usize)
                        .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = 0;
                }
                Self::Str(i) => {
                    *i.get_mut(idx as usize)
                        .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = String::new();
                }
            },
        }

        Ok(())
    }

    pub fn set(&mut self, idx: u32, value: impl Into<Value>) -> Result<()> {
        match (self, value.into()) {
            (Self::Int(i), Value::Int(n)) => {
                *i.get_mut(idx as usize)
                    .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = n;
            }
            // auto convert int to string
            (Self::Str(i), Value::Int(n)) => {
                *i.get_mut(idx as usize)
                    .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = n.to_string();
            }
            (Self::Str(i), Value::String(s)) => {
                *i.get_mut(idx as usize)
                    .ok_or_else(|| anyhow!("Variable out of range {}", idx))? = s;
            }
            _ => bail!("Variable type mismatched"),
        }

        Ok(())
    }

    pub fn as_int(&mut self) -> Result<&mut Vec<i64>> {
        match self {
            Self::Int(i) => Ok(i),
            _ => bail!("Variable type is not Int"),
        }
    }

    pub fn as_str(&mut self) -> Result<&mut Vec<String>> {
        match self {
            Self::Str(i) => Ok(i),
            _ => bail!("Variable type is not Str"),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum UniformVariable {
    Normal(VmVariable),
    Character(Vec<VmVariable>),
}

impl UniformVariable {
    pub fn new(header: &HeaderInfo, info: &VariableInfo) -> Self {
        match info.is_chara {
            false => UniformVariable::Normal(VmVariable::new(header, info)),
            true => UniformVariable::Character(Vec::new()),
        }
    }

    pub fn with_character_len(
        header: &HeaderInfo,
        info: &VariableInfo,
        character_len: u32,
    ) -> Self {
        match info.is_chara {
            false => UniformVariable::Normal(VmVariable::new(header, info)),
            true => UniformVariable::Character(vec![
                VmVariable::new(header, info);
                character_len as usize
            ]),
        }
    }

    pub fn as_vm_var(&mut self, chara_no: u32) -> &mut VmVariable {
        match self {
            UniformVariable::Character(c) => &mut c[chara_no as usize],
            UniformVariable::Normal(v) => v,
        }
    }

    pub fn reset(&mut self, header: &HeaderInfo, info: &VariableInfo) {
        {
            match self {
                UniformVariable::Normal(var) => *var = VmVariable::new(header, info),
                UniformVariable::Character(cvar) => {
                    cvar.iter_mut().for_each(|var| {
                        *var = VmVariable::new(header, info);
                    });
                }
            }
        }
    }

    pub fn assume_normal(&mut self) -> &mut VmVariable {
        if let Self::Normal(v) = self {
            v
        } else {
            panic!("Variable is not normal variable")
        }
    }

    pub fn assume_chara_vec(&mut self) -> &mut Vec<VmVariable> {
        if let Self::Character(c) = self {
            c
        } else {
            panic!("Variable is not character variable")
        }
    }

    pub fn assume_chara(&mut self, idx: u32) -> &mut VmVariable {
        if let Self::Character(c) = self {
            &mut c[idx as usize]
        } else {
            panic!("Variable is not character variable")
        }
    }

    pub fn swap_chara(&mut self, a: u32, b: u32) {
        if let Self::Character(c) = self {
            c.swap(a as usize, b as usize);
        }
    }

    pub fn copy_chara(&mut self, a: u32, b: u32) {
        if let Self::Character(c) = self {
            let tmp = c[a as usize].clone();
            c[b as usize] = tmp;
        }
    }

    pub fn add_chara(&mut self, header: &HeaderInfo, info: &VariableInfo) {
        if let Self::Character(c) = self {
            c.push(VmVariable::new(header, info));
        }
    }

    pub fn add_copy_chara(&mut self, idx: u32) {
        if let Self::Character(c) = self {
            let prev_c = c[idx as usize].clone();
            c.push(prev_c);
        }
    }

    pub fn del_chara(&mut self, idx: u32) {
        if let Self::Character(c) = self {
            c.remove(idx as usize);
        }
    }

    /// Remove every character in `list`.
    pub fn del_chara_list(&mut self, list: &BTreeSet<u32>) {
        if let Self::Character(c) = self {
            // Descending, so an earlier removal cannot shift a later index.
            for i in list.iter().rev() {
                c.remove(*i as usize);
            }
        }
    }

    /// Drop every character *not* in `list`, keeping ascending index order.
    pub fn pickup_chara(&mut self, list: &BTreeSet<u32>) {
        if let Self::Character(c) = self {
            for i in (0..c.len()).rev() {
                if !list.contains(&(i as u32)) {
                    c.remove(i);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Enum, IntoStaticStr, Display)]
#[strum(use_phf)]
#[strum(serialize_all = "UPPERCASE")]
pub enum KnownVariableNames {
    No,
    Count,
    NextCom,
    PrevCom,
    SelectCom,
    Master,
    Target,
    Assi,
    AssiPlay,
    Local,
    LocalS,
    Arg,
    ArgS,
    Result,
    ResultS,
    Palam,
    PalamLv,
    Exp,
    ExpLv,
    Up,
    Down,
    Cup,
    Cdown,
    Stain,
    #[allow(non_camel_case_types)]
    SaveData_Text,

    Base,
    DownBase,
    LoseBase,

    Train,
    Tflag,
    Tcvar,
    Tequip,
    Source,
    Juel,
    GotJuel,
}

pub trait StrKeyLike: Debug + Copy {
    fn get_key(self, var: &VariableStorage) -> StrKey;

    fn resolve_key(self, var: &VariableStorage) -> &str {
        var.resolve_key(self.get_key(var))
    }
}

impl StrKeyLike for StrKey {
    #[inline(always)]
    fn get_key(self, _: &VariableStorage) -> StrKey {
        self
    }
}

impl<'a> StrKeyLike for &'a String {
    fn get_key(self, var: &VariableStorage) -> StrKey {
        var.interner().get_or_intern(self)
    }
}

impl<'a> StrKeyLike for &'a str {
    fn get_key(self, var: &VariableStorage) -> StrKey {
        var.interner().get_or_intern(self)
    }
}

impl StrKeyLike for KnownVariableNames {
    #[inline(always)]
    fn get_key(self, var: &VariableStorage) -> StrKey {
        var.known_key(self)
    }
}

impl StrKeyLike for EventType {
    #[inline(always)]
    fn get_key(self, var: &VariableStorage) -> StrKey {
        var.event_key(self)
    }
}

impl StrKeyLike for FunctionIdentifier {
    fn get_key(self, var: &VariableStorage) -> StrKey {
        match self {
            FunctionIdentifier::Normal(key) => key,
            FunctionIdentifier::Event(ty) => ty.get_key(var),
        }
    }
}
