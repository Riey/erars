use anyhow::{bail, ensure, Context, Result};
use erars_ast::{StrKey, VariableInfo};
use flate2::{read, write};
use hashbrown::HashMap;
use itertools::Either;
use serde::{Deserialize, Serialize};
use std::{
    io::{BufReader, Read, Write},
    path::Path,
};

use crate::{SaveList, UniformVariable, VmVariable};

pub struct RawSaveData {
    pub description: String,
    pub code: u32,
    pub version: u32,
    pub data: Box<dyn Read + Send>,
}

impl RawSaveData {
    const MAGIC: [u8; 4] = [0x01, 0x02, 0xFF, 0xFE];

    pub fn to_local_data(self) -> anyhow::Result<SerializableVariableStorage> {
        let mut ret: SerializableVariableStorage = rmp_serde::from_read(self.data)?;
        ret.description = self.description;
        ret.code = self.code;
        ret.version = self.version;
        Ok(ret)
    }

    pub fn to_global_data(self) -> anyhow::Result<SerializableGlobalVariableStorage> {
        let mut ret: SerializableGlobalVariableStorage = rmp_serde::from_read(self.data)?;
        ret.code = self.code;
        ret.version = self.version;
        Ok(ret)
    }

    pub fn to_chara_data(self) -> anyhow::Result<SerializableCharaData> {
        let mut ret: SerializableCharaData = rmp_serde::from_read(self.data)?;
        ret.description = self.description;
        ret.code = self.code;
        ret.version = self.version;
        Ok(ret)
    }

    pub fn to_var_data(self) -> anyhow::Result<SerializableVarData> {
        let mut ret: SerializableVarData = rmp_serde::from_read(self.data)?;
        ret.description = self.description;
        ret.code = self.code;
        ret.version = self.version;
        Ok(ret)
    }

    pub fn from_read(mut data: Box<dyn Read + Send>) -> Result<Self> {
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
            data,
        })
    }

    pub fn from_file(file: std::fs::File) -> Result<Self> {
        Self::from_read(Box::new(std::io::BufReader::new(file)))
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

static GLOBAL_SAVE_FILE_NAME: &str = "global.rsav";

/// Emuera `getSaveDataPathC`: `chara_<name>.dat` beside the numbered saves.
pub fn write_chara_data(sav_path: &Path, name: &str, sav: &SerializableCharaData) -> Result<()> {
    check_dat_filename(name)?;
    create_sav_dir(sav_path)?;

    sav.write_to(std::fs::File::create(sav_path.join(format!("chara_{name}.dat")))?)
        .context("Serialize chara sav")
}

pub fn read_chara_data(sav_path: &Path, name: &str) -> Result<Option<RawSaveData>> {
    check_dat_filename(name)?;

    let Ok(file) = std::fs::File::open(sav_path.join(format!("chara_{name}.dat"))) else {
        return Ok(None);
    };

    Ok(RawSaveData::from_file(file).ok())
}

/// Emuera `getSaveDataPathV`: `var_<name>.dat`.
pub fn write_var_data(sav_path: &Path, name: &str, sav: &SerializableVarData) -> Result<()> {
    check_dat_filename(name)?;
    create_sav_dir(sav_path)?;

    sav.write_to(std::fs::File::create(sav_path.join(format!("var_{name}.dat")))?)
        .context("Serialize var sav")
}

pub fn read_var_data(sav_path: &Path, name: &str) -> Result<Option<RawSaveData>> {
    check_dat_filename(name)?;

    let Ok(file) = std::fs::File::open(sav_path.join(format!("var_{name}.dat"))) else {
        return Ok(None);
    };

    Ok(RawSaveData::from_file(file).ok())
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

    std::fs::remove_file(sav_path.join(make_save_file_name(idx)))?;

    Ok(())
}

pub fn read_save_data(sav_path: &Path, idx: u32) -> Result<Option<RawSaveData>> {
    let file = sav_path.join(make_save_file_name(idx));

    let compressed = match std::fs::File::open(&file) {
        Ok(file) => file,
        Err(_) => return Ok(None),
    };
    let decoder = read::GzDecoder::new(BufReader::new(compressed));

    Ok(RawSaveData::from_read(Box::new(decoder)).ok())
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

pub fn read_global_data(sav_path: &Path) -> Result<Option<RawSaveData>> {
    let file = sav_path.join(GLOBAL_SAVE_FILE_NAME);

    let Ok(file) = std::fs::File::open(&file) else { return Ok(None); };

    Ok(RawSaveData::from_file(file).ok())
}

#[cfg(feature = "multithread")]
use rayon::prelude::*;

pub fn load_local_list(sav_path: &Path) -> anyhow::Result<SaveList> {
    let sav_idxs = 0..100;
    #[cfg(not(feature = "multithread"))]
    let iter = sav_idxs.into_iter();
    #[cfg(feature = "multithread")]
    let iter = sav_idxs.into_par_iter();

    iter.filter_map(|idx| {
        read_save_data(sav_path, idx)
            .transpose()
            .map(|sav| sav.map(|sav| (idx, Either::Right(sav))))
    })
    .collect::<anyhow::Result<_>>()
}
