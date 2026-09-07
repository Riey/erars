pub(crate) mod emuera;

use anyhow::{bail, ensure, Context, Result};
use erars_ast::{StrKey, VariableInfo};
use erars_compiler::HeaderInfo;
use flate2::{read, write};
use hashbrown::HashMap;
use itertools::Either;
use serde::{Deserialize, Serialize};
use std::{
    io::{Cursor, Read, Write},
    path::Path,
};

use crate::{SaveList, UniformVariable, VmVariable};

/// A file's payload once its container format has been resolved: either
/// erars's own `rmp_serde` blob (streamed straight through, unread until a
/// `to_*_data` call actually deserialises it), or a real Emuera save
/// [`emuera::sniff`] recognised and [`emuera::parse`] already fully parsed
/// (Emuera's text grammar has no msgpack-style "parse later" split — the
/// whole file has to be read to find the fields at all).
enum SaveBody {
    Native(Box<dyn Read + Send>),
    Emuera(emuera::EmueraSaveData),
}

pub struct RawSaveData {
    pub description: String,
    pub code: u32,
    pub version: u32,
    body: SaveBody,
}

impl RawSaveData {
    const MAGIC: [u8; 4] = [0x01, 0x02, 0xFF, 0xFE];

    /// `header` is only consulted for a foreign Emuera body — this game's
    /// currently declared variables are what a foreign save's names and
    /// shapes get reconciled against (see the `save::emuera` module doc
    /// comment). A same-format load ignores it entirely, same as before.
    pub fn to_local_data(self, header: &HeaderInfo) -> Result<SerializableVariableStorage> {
        let (description, code, version) = (self.description, self.code, self.version);
        let mut ret = match self.body {
            SaveBody::Native(data) => rmp_serde::from_read(data)?,
            SaveBody::Emuera(data) => emuera::build_local_data(data, header).0,
        };
        ret.description = description;
        ret.code = code;
        ret.version = version;
        Ok(ret)
    }

    pub fn to_global_data(self, header: &HeaderInfo) -> Result<SerializableGlobalVariableStorage> {
        let (code, version) = (self.code, self.version);
        let mut ret = match self.body {
            SaveBody::Native(data) => rmp_serde::from_read(data)?,
            SaveBody::Emuera(data) => emuera::build_global_data(data, header).0,
        };
        ret.code = code;
        ret.version = version;
        Ok(ret)
    }

    pub fn to_chara_data(self) -> anyhow::Result<SerializableCharaData> {
        let (description, code, version) = (self.description, self.code, self.version);
        let SaveBody::Native(data) = self.body else {
            // `read_chara_data` already refuses a recognised Emuera
            // `chara_*.dat` before it ever reaches here (see its own doc
            // comment) — this pass never parses one, so this arm is
            // unreachable in practice, not a silently-accepted foreign body.
            bail!("Emuera chara data import is not supported");
        };
        let mut ret: SerializableCharaData = rmp_serde::from_read(data)?;
        ret.description = description;
        ret.code = code;
        ret.version = version;
        Ok(ret)
    }

    pub fn to_var_data(self) -> anyhow::Result<SerializableVarData> {
        let (description, code, version) = (self.description, self.code, self.version);
        let SaveBody::Native(data) = self.body else {
            // See `to_chara_data`: `read_var_data` already refuses a
            // recognised Emuera `var_*.dat` before this point.
            bail!("Emuera var data import is not supported");
        };
        let mut ret: SerializableVarData = rmp_serde::from_read(data)?;
        ret.description = description;
        ret.code = code;
        ret.version = version;
        Ok(ret)
    }

    /// Parses an already-decompressed, confirmed-native stream: reads the
    /// magic/code/version/description preamble `write_dat_header` wrote and
    /// leaves the rest of `data` for a `to_*_data` call to deserialise.
    fn from_native_read(mut data: Box<dyn Read + Send>) -> Result<Self> {
        let buf = &mut [0u8; 4];
        data.read_exact(buf)?;
        if buf != &Self::MAGIC {
            log::error!("Invalid MAGIC {buf:?}");
            bail!("Invalid MAGIC");
        }

        data.read_exact(buf)?;
        let code = u32::from_le_bytes(*buf);
        data.read_exact(buf)?;
        let version = u32::from_le_bytes(*buf);

        data.read_exact(buf)?;
        let str_len = u32::from_le_bytes(*buf) as usize;

        let mut description = vec![0; str_len];
        data.read_exact(&mut description)?;

        let description = String::from_utf8(description).expect("Invalid UTF-8");

        Ok(Self {
            description,
            code,
            version,
            body: SaveBody::Native(data),
        })
    }

    /// Parses an already-confirmed-native, uncompressed byte buffer (chara/
    /// var `.dat` files are never gzip-wrapped, unlike numbered/global
    /// saves — see `write_chara_data`/`write_var_data`).
    fn from_native_bytes(bytes: Vec<u8>) -> Result<Self> {
        ensure!(bytes.starts_with(&Self::MAGIC), "Invalid MAGIC");
        Self::from_native_read(Box::new(Cursor::new(bytes)))
    }

    fn from_emuera_bytes(
        bytes: &[u8],
        encoding: &'static encoding_rs::Encoding,
        is_global: bool,
    ) -> Result<Self> {
        match emuera::sniff(bytes, encoding) {
            Some(variant) => {
                let (data, code, version, description) =
                    emuera::parse(variant, bytes, encoding, is_global)?;
                Ok(Self {
                    description,
                    code,
                    version,
                    body: SaveBody::Emuera(data),
                })
            }
            None => bail!("인식할 수 없는 세이브 파일 형식입니다"),
        }
    }

    /// Resolves `bytes` (the whole file's raw content) into either erars's
    /// own format or a real Emuera save. `native_gzip` says whether erars's
    /// own format is gzip-compressed for this particular file — numbered
    /// `save{idx:02}.rsav.gz` is, `global.rsav` is not (`write_global_data`:
    /// "don't compress global data since it's pretty small"). A real Emuera
    /// save is never gzip-compressed either way, so `native_gzip` only ever
    /// gates the *native* branch. `encoding`/`is_global` are only consulted
    /// for a foreign Emuera body — see `from_emuera_bytes`.
    fn from_bytes(
        bytes: Vec<u8>,
        native_gzip: bool,
        encoding: &'static encoding_rs::Encoding,
        is_global: bool,
    ) -> Result<Self> {
        if native_gzip {
            let mut decoder = read::GzDecoder::new(Cursor::new(bytes));
            let mut decompressed = Vec::new();
            let decoded = decoder.read_to_end(&mut decompressed).is_ok();
            let bytes = decoder.into_inner().into_inner();

            if decoded && decompressed.starts_with(&Self::MAGIC) {
                return Self::from_native_read(Box::new(Cursor::new(decompressed)));
            }

            return Self::from_emuera_bytes(&bytes, encoding, is_global);
        }

        if bytes.starts_with(&Self::MAGIC) {
            return Self::from_native_read(Box::new(Cursor::new(bytes)));
        }

        Self::from_emuera_bytes(&bytes, encoding, is_global)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct SerializableVariableStorage {
    #[serde(skip)]
    pub description: String,
    #[serde(skip)]
    pub code: u32,
    #[serde(skip)]
    pub version: u32,
    pub character_len: u32,
    pub rand_seed: [u8; 32],
    pub variables: HashMap<StrKey, (VariableInfo, UniformVariable)>,
    pub local_variables: HashMap<StrKey, HashMap<StrKey, (VariableInfo, UniformVariable)>>,
}

impl SerializableVariableStorage {
    pub fn write_to(&self, mut out: impl Write) -> Result<()> {
        write_dat_header(&mut out, self.code, self.version, &self.description)?;
        rmp_serde::encode::write(&mut out, &self)?;
        Ok(())
    }
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct SerializableGlobalVariableStorage {
    #[serde(skip)]
    pub code: u32,
    #[serde(skip)]
    pub version: u32,
    pub variables: HashMap<StrKey, (VariableInfo, UniformVariable)>,
    pub local_variables: HashMap<StrKey, HashMap<StrKey, (VariableInfo, UniformVariable)>>,
}

impl SerializableGlobalVariableStorage {
    pub fn write_to(&self, mut out: impl Write) -> Result<()> {
        write_dat_header(&mut out, self.code, self.version, "")?;
        rmp_serde::encode::write(&mut out, &self)?;
        Ok(())
    }
}

/// `SAVECHARA` payload — the character-variable rows of the saved characters,
/// in the order the script listed them.
///
/// Emuera writes one `CharacterData` blob per character; erars keeps chara data
/// column-wise (`name -> Vec<VmVariable>` indexed by character), so a row is
/// materialised as `name -> that character's VmVariable`. Only `is_savedata`
/// character variables are stored, matching the normal save path.
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct SerializableCharaData {
    #[serde(skip)]
    pub description: String,
    #[serde(skip)]
    pub code: u32,
    #[serde(skip)]
    pub version: u32,
    pub charas: Vec<HashMap<StrKey, VmVariable>>,
}

/// `SAVEVAR` payload — whole arrays of the named global variables.
///
/// Emuera writes `WriteWithKey(var.Name, var.GetArray())` per variable
/// (`VariableEvaluator.SaveVariable`) and loads them back by name, skipping
/// anything that is no longer a plain global (`VariableData.LoadVariableBinary`).
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct SerializableVarData {
    #[serde(skip)]
    pub description: String,
    #[serde(skip)]
    pub code: u32,
    #[serde(skip)]
    pub version: u32,
    pub variables: HashMap<StrKey, VmVariable>,
}

/// The `RawSaveData` preamble: magic, unique code, version, description.
fn write_dat_header(out: &mut impl Write, code: u32, version: u32, description: &str) -> Result<()> {
    out.write_all(&RawSaveData::MAGIC)?;
    out.write_all(&code.to_le_bytes())?;
    out.write_all(&version.to_le_bytes())?;
    out.write_all(&(description.len() as u32).to_le_bytes())?;
    out.write_all(description.as_bytes())?;
    Ok(())
}

impl SerializableCharaData {
    pub fn write_to(&self, mut out: impl Write) -> Result<()> {
        write_dat_header(&mut out, self.code, self.version, &self.description)?;
        rmp_serde::encode::write(&mut out, &self)?;
        Ok(())
    }
}

impl SerializableVarData {
    pub fn write_to(&self, mut out: impl Write) -> Result<()> {
        write_dat_header(&mut out, self.code, self.version, &self.description)?;
        rmp_serde::encode::write(&mut out, &self)?;
        Ok(())
    }
}

/// Emuera `CheckDatFilename`.
///
/// `SaveChara` calls it and then throws the result away, so a name with a path
/// separator escapes the save directory there. We reject it: the name comes
/// straight from the script and nothing in the corpus relies on writing
/// outside the save directory.
fn check_dat_filename(name: &str) -> Result<()> {
    ensure!(!name.is_empty(), "파일명이 비어있습니다");
    ensure!(
        !name.contains(|c: char| {
            c.is_control() || matches!(c, '/' | '\\' | ':' | '*' | '?' | '"' | '<' | '>' | '|')
        }),
        "파일명에 사용할 수 없는 문자가 있습니다: {name}"
    );
    Ok(())
}

fn create_sav_dir(sav_path: &Path) -> Result<()> {
    if !sav_path.exists() {
        std::fs::create_dir_all(sav_path)?;
    }
    Ok(())
}

fn make_save_file_name(idx: u32) -> String {
    format!("save{idx:02}.rsav.gz")
}

/// Real Emuera's own numbered-slot name (`getSaveDataPath`, `.il:107770`) —
/// never written by erars, only ever read as a fallback when no native slot
/// exists (see `read_save_data_slot`).
fn emuera_save_file_name(idx: u32) -> String {
    format!("save{idx:02}.sav")
}

static GLOBAL_SAVE_FILE_NAME: &str = "global.rsav";

/// Real Emuera's own global-save name (`getSaveDataPathG`, `.il:107868`).
static EMUERA_GLOBAL_SAVE_FILE_NAME: &str = "global.sav";

/// Reads `path` whole, unless it is a real Emuera save recognised by
/// [`emuera::sniff`] — `chara_*.dat`/`var_*.dat` compatibility is out of
/// scope for this pass (see the `save::emuera` module doc comment), so such
/// a file is refused with a distinct, actionable error rather than either
/// silently ignored (`Ok(None)`, indistinguishable from "no such file") or
/// failed with a generic parse error a player can't act on. A missing file
/// is `Ok(None)`; any other unrecognised content is returned as-is for the
/// caller's own native parse attempt to fail on.
fn read_bytes_rejecting_emuera(
    path: &Path,
    kind: &str,
    encoding: &'static encoding_rs::Encoding,
) -> Result<Option<Vec<u8>>> {
    let Ok(bytes) = std::fs::read(path) else {
        return Ok(None);
    };

    if !bytes.starts_with(&RawSaveData::MAGIC) {
        if let Some(variant) = emuera::sniff(&bytes, encoding) {
            bail!(
                "{}은(는) 실제 Emuera가 작성한 {kind} 파일({variant:?})입니다. \
                 erars는 이 파일 형식을 지원하지 않습니다.",
                path.display()
            );
        }
    }

    Ok(Some(bytes))
}

/// Emuera `getSaveDataPathC`: `chara_<name>.dat` beside the numbered saves.
pub fn write_chara_data(sav_path: &Path, name: &str, sav: &SerializableCharaData) -> Result<()> {
    check_dat_filename(name)?;
    create_sav_dir(sav_path)?;

    sav.write_to(std::fs::File::create(sav_path.join(format!("chara_{name}.dat")))?)
        .context("Serialize chara sav")
}

pub fn read_chara_data(
    sav_path: &Path,
    name: &str,
    encoding: &'static encoding_rs::Encoding,
) -> Result<Option<RawSaveData>> {
    check_dat_filename(name)?;

    let path = sav_path.join(format!("chara_{name}.dat"));
    let Some(bytes) = read_bytes_rejecting_emuera(&path, "캐릭터 저장(SAVECHARA)", encoding)? else {
        return Ok(None);
    };

    Ok(RawSaveData::from_native_bytes(bytes).ok())
}

/// Emuera `getSaveDataPathV`: `var_<name>.dat`.
pub fn write_var_data(sav_path: &Path, name: &str, sav: &SerializableVarData) -> Result<()> {
    check_dat_filename(name)?;
    create_sav_dir(sav_path)?;

    sav.write_to(std::fs::File::create(sav_path.join(format!("var_{name}.dat")))?)
        .context("Serialize var sav")
}

pub fn read_var_data(
    sav_path: &Path,
    name: &str,
    encoding: &'static encoding_rs::Encoding,
) -> Result<Option<RawSaveData>> {
    check_dat_filename(name)?;

    let path = sav_path.join(format!("var_{name}.dat"));
    let Some(bytes) = read_bytes_rejecting_emuera(&path, "변수 저장(SAVEVAR)", encoding)? else {
        return Ok(None);
    };

    Ok(RawSaveData::from_native_bytes(bytes).ok())
}

/// Emuera `VariableEvaluator.GetDatFiles` (`VariableEvaluator.cs:1786-1809`):
/// the names of the `chara_<pattern>.dat` (or `var_<pattern>.dat`) files in the
/// save directory, with the fixed prefix and the `.dat` extension stripped.
/// `pattern` is a Windows search pattern, so `*` and `?` are wildcards.
///
/// Emuera inherits `Directory.GetFiles`' unspecified order; erars sorts, which
/// makes `FIND_CHARADATA`'s `RESULTS` deterministic across filesystems.
pub fn find_dat_files(sav_path: &Path, chara: bool, pattern: &str) -> Vec<String> {
    let prefix = if chara { "chara_" } else { "var_" };
    let Ok(dir) = std::fs::read_dir(sav_path) else {
        return Vec::new();
    };

    let mut names: Vec<String> = dir
        .flatten()
        .filter_map(|entry| {
            let name = entry.file_name().into_string().ok()?;
            let name = name.strip_suffix(".dat")?.strip_prefix(prefix)?;

            (!name.is_empty() && wildcard_match(pattern, name)).then(|| name.to_owned())
        })
        .collect();

    names.sort_unstable();
    names
}

/// Windows search-pattern matching: `*` for any run of characters, `?` for
/// exactly one. Backtracking is linear — `star` remembers the last `*`.
fn wildcard_match(pattern: &str, name: &str) -> bool {
    let (pat, text) = (pattern.as_bytes(), name.as_bytes());
    let (mut p, mut t) = (0, 0);
    let mut star: Option<(usize, usize)> = None;

    loop {
        match pat.get(p) {
            Some(b'*') => {
                star = Some((p, t));
                p += 1;
            }
            Some(&c) if t < text.len() && (c == b'?' || c == text[t]) => {
                p += 1;
                t += 1;
            }
            _ if t == text.len() && p == pat.len() => return true,
            // Mismatch: give the last `*` one more character to swallow.
            _ => match star {
                Some((sp, st)) if st < text.len() => {
                    p = sp + 1;
                    t = st + 1;
                    star = Some((sp, t));
                }
                _ => return false,
            },
        }
    }
}

#[test]
fn wildcard_match_test() {
    assert!(wildcard_match("*", "anything"));
    assert!(wildcard_match("*", ""));
    assert!(wildcard_match("a*c", "abbbc"));
    assert!(wildcard_match("a?c", "abc"));
    assert!(!wildcard_match("a?c", "ac"));
    assert!(!wildcard_match("a*c", "abbbd"));
    assert!(wildcard_match("save*", "save01"));
    assert!(!wildcard_match("save*", "load01"));
    assert!(wildcard_match("*01", "save01"));
    assert!(wildcard_match("a*b*c", "axxbyyc"));
}

/// Emuera `GetSaveDataPathText`: `{dir}txt{index:00}.txt`.
///
/// `force_savdir` selects Emuera's un-redirected save directory; erars has a
/// single `sav_dir` with no redirection, so the two are the same path.
fn text_file_path(sav_path: &Path, idx: u32) -> std::path::PathBuf {
    sav_path.join(format!("txt{idx:02}.txt"))
}

/// `SAVETEXT`. `false` on any IO failure, as Emuera's `catch { return 0; }`.
pub fn write_text_data(sav_path: &Path, idx: u32, text: &str) -> bool {
    create_sav_dir(sav_path).is_ok()
        && std::fs::write(text_file_path(sav_path, idx), text).is_ok()
}

/// `LOADTEXT`. An empty string for a missing or unreadable file.
pub fn read_text_data(sav_path: &Path, idx: u32) -> String {
    std::fs::read_to_string(text_file_path(sav_path, idx)).unwrap_or_default()
}

pub fn write_save_data(sav_path: &Path, idx: u32, sav: &SerializableVariableStorage) -> Result<()> {
    create_sav_dir(sav_path)?;

    let mut file = std::fs::File::create(sav_path.join(make_save_file_name(idx)))?;
    let mut encoder = write::GzEncoder::new(&mut file, flate2::Compression::fast());

    sav.write_to(&mut encoder).context("Serialize sav")?;

    Ok(())
}

pub fn delete_save_data(sav_path: &Path, idx: u32) -> Result<()> {
    create_sav_dir(sav_path)?;

    // `DELDATA` (excom.md:1128-1131): "not an error even if the file does
    // not exist" — matches .NET's `File.Delete`, a silent no-op for a
    // missing path, unlike `std::fs::remove_file`.
    if let Err(err) = std::fs::remove_file(sav_path.join(make_save_file_name(idx))) {
        if err.kind() != std::io::ErrorKind::NotFound {
            return Err(err.into());
        }
    }

    Ok(())
}

/// Picks between erars's own native slot and a real Emuera save occupying
/// the same logical slot (`native_name`/`emuera_name` — a numbered
/// `save{idx:02}.rsav.gz`/`save{idx:02}.sav` pair, or the global pair): the
/// native file always wins if both exist, logged so a player who somehow
/// has both is not left wondering which one erars actually loaded. Neither
/// existing is `Ok(None)`, matching a missing single file's own long-standing
/// meaning ("no such slot", not an error).
fn read_save_data_slot(
    sav_path: &Path,
    native_name: &str,
    emuera_name: &str,
    native_gzip: bool,
    is_global: bool,
    encoding: &'static encoding_rs::Encoding,
) -> Result<Option<RawSaveData>> {
    let native_path = sav_path.join(native_name);
    let emuera_path = sav_path.join(emuera_name);
    let native_exists = native_path.exists();
    let emuera_exists = emuera_path.exists();

    if native_exists && emuera_exists {
        log::warn!(
            "{}와(과) {}이(가) 모두 존재합니다: erars 자체 저장 파일을 우선 사용합니다.",
            native_path.display(),
            emuera_path.display(),
        );
    }

    let (path, native_gzip) = if native_exists {
        (native_path, native_gzip)
    } else if emuera_exists {
        (emuera_path, false)
    } else {
        return Ok(None);
    };

    let bytes = std::fs::read(&path)?;
    Ok(RawSaveData::from_bytes(bytes, native_gzip, encoding, is_global).ok())
}

pub fn read_save_data(
    sav_path: &Path,
    idx: u32,
    encoding: &'static encoding_rs::Encoding,
) -> Result<Option<RawSaveData>> {
    read_save_data_slot(
        sav_path,
        &make_save_file_name(idx),
        &emuera_save_file_name(idx),
        true,
        false,
        encoding,
    )
}

pub fn write_global_data(sav_path: &Path, sav: &SerializableGlobalVariableStorage) -> Result<()> {
    create_sav_dir(sav_path)?;

    // Don't compress global data since it's pretty small
    sav.write_to(
        std::fs::File::create(sav_path.join(GLOBAL_SAVE_FILE_NAME))
            .context("Create global sav file")?,
    )?;

    Ok(())
}

pub fn read_global_data(
    sav_path: &Path,
    encoding: &'static encoding_rs::Encoding,
) -> Result<Option<RawSaveData>> {
    read_save_data_slot(
        sav_path,
        GLOBAL_SAVE_FILE_NAME,
        EMUERA_GLOBAL_SAVE_FILE_NAME,
        false,
        true,
        encoding,
    )
}

/// Exports this game's current local save state to real Emuera's own
/// on-disk format at the same numbered slot `SAVEDATA` uses — an opt-in,
/// player-triggered path back out of erars (see the `save::emuera::write`
/// module doc comment), never written automatically. `binary` picks the
/// container variant and mirrors Emuera's own `SystemSaveInBinary`; for
/// the text variant, `text_encoding`/`encoding` mirror `SystemSaveInUTF8`
/// exactly as they already do for [`load_local_list`]'s own text reads.
pub fn write_emuera_save_data(
    sav_path: &Path,
    idx: u32,
    sav: &SerializableVariableStorage,
    binary: bool,
    text_encoding: emuera::write::TextEncodingChoice,
    encoding: &'static encoding_rs::Encoding,
) -> Result<()> {
    create_sav_dir(sav_path)?;

    let data = emuera::export_local(sav);
    let bytes = if binary {
        emuera::write::write_binary(&data, false, sav.code, sav.version, &sav.description)
    } else {
        emuera::write::write_text(
            &data,
            false,
            sav.code,
            sav.version,
            &sav.description,
            text_encoding,
            encoding,
        )?
    };
    std::fs::write(sav_path.join(emuera_save_file_name(idx)), bytes)
        .context("Write Emuera-format save file")?;

    Ok(())
}

/// As [`write_emuera_save_data`], for the global save (`SAVEGLOBAL`'s
/// counterpart) — real Emuera's own `global.sav`.
pub fn write_emuera_global_data(
    sav_path: &Path,
    sav: &SerializableGlobalVariableStorage,
    binary: bool,
    text_encoding: emuera::write::TextEncodingChoice,
    encoding: &'static encoding_rs::Encoding,
) -> Result<()> {
    create_sav_dir(sav_path)?;

    let data = emuera::export_global(sav)?;
    let bytes = if binary {
        emuera::write::write_binary(&data, true, sav.code, sav.version, "")
    } else {
        emuera::write::write_text(&data, true, sav.code, sav.version, "", text_encoding, encoding)?
    };
    std::fs::write(sav_path.join(EMUERA_GLOBAL_SAVE_FILE_NAME), bytes)
        .context("Write Emuera-format global save file")?;

    Ok(())
}

#[cfg(feature = "multithread")]
use rayon::prelude::*;

pub fn load_local_list(
    sav_path: &Path,
    encoding: &'static encoding_rs::Encoding,
) -> anyhow::Result<SaveList> {
    let sav_idxs = 0..100;
    #[cfg(not(feature = "multithread"))]
    let iter = sav_idxs.into_iter();
    #[cfg(feature = "multithread")]
    let iter = sav_idxs.into_par_iter();

    iter.filter_map(|idx| {
        read_save_data(sav_path, idx, encoding)
            .transpose()
            .map(|sav| sav.map(|sav| (idx, Either::Right(sav))))
    })
    .collect::<anyhow::Result<_>>()
}

