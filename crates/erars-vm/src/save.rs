use anyhow::{bail, Context, Result};
use erars_ast::{StrKey, VariableInfo};
use flate2::{read, write};
use hashbrown::HashMap;
use itertools::Either;
use serde::{Deserialize, Serialize};
use std::{
    io::{BufReader, Read, Write},
    path::Path,
};

use crate::{SaveList, UniformVariable};

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
        out.write_all(&RawSaveData::MAGIC)?;
        out.write_all(&self.code.to_le_bytes())?;
        out.write_all(&self.version.to_le_bytes())?;
        out.write_all(&(self.description.len() as u32).to_le_bytes())?;
        out.write_all(self.description.as_bytes())?;
        rmp_serde::encode::write(&mut out, &self).unwrap();
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
        out.write_all(&RawSaveData::MAGIC)?;
        out.write_all(&self.code.to_le_bytes())?;
        out.write_all(&self.version.to_le_bytes())?;
        out.write_all(&0u32.to_le_bytes())?;
        rmp_serde::encode::write(&mut out, &self)?;
        Ok(())
    }
}

fn make_save_file_name(idx: u32) -> String {
    format!("save{idx:02}.rsav.gz")
}

static GLOBAL_SAVE_FILE_NAME: &str = "global.rsav";

pub fn write_save_data(sav_path: &Path, idx: u32, sav: &SerializableVariableStorage) -> Result<()> {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path)?;
    }

    let mut file = std::fs::File::create(sav_path.join(make_save_file_name(idx)))?;
    let mut encoder = write::GzEncoder::new(&mut file, flate2::Compression::fast());

    sav.write_to(&mut encoder).context("Serialize sav")?;

    Ok(())
}

pub fn delete_save_data(sav_path: &Path, idx: u32) -> Result<()> {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path)?;
    }

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
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path)?;
    }

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
