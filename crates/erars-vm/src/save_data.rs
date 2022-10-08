use erars_compiler::HeaderInfo;
use flate2::read;
use flate2::write;
use std::path::Path;

use super::{
    variable::{SerializableGlobalVariableStorage, SerializableVariableStorage},
    VariableStorage,
};

fn make_save_file_name(idx: i64) -> String {
    format!("save{idx:02}.msgpack.gz")
}

static GLOBAL_SAVE_FILE_NAME: &str = "global.msgpack";

pub fn write_save_data(
    sav_path: &Path,
    idx: i64,
    var: &VariableStorage,
    header: &HeaderInfo,
    description: String,
) {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path).ok();
    }

    let mut file = std::fs::File::create(sav_path.join(make_save_file_name(idx))).unwrap();
    let mut encoder = write::GzEncoder::new(&mut file, flate2::Compression::fast());

    rmp_serde::encode::write(&mut encoder, &var.get_serializable(header, description)).unwrap();
}

pub fn delete_save_data(sav_path: &Path, idx: i64) -> std::io::Result<()> {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path)?;
    }

    std::fs::remove_file(sav_path.join(make_save_file_name(idx)))
}

pub fn read_save_data(
    sav_path: &Path,
    header: &HeaderInfo,
    idx: i64,
) -> Result<SerializableVariableStorage, i64> {
    let file = sav_path.join(make_save_file_name(idx));

    let ret = if !file.exists() {
        1
    } else {
        match std::fs::File::open(file) {
            Ok(compressed) => {
                let mut decoder = read::GzDecoder::new(compressed);
                match rmp_serde::from_read::<_, SerializableVariableStorage>(&mut decoder) {
                    Ok(ret) => {
                        if ret.code != header.gamebase.code {
                            // Invalid code
                            2
                        } else if ret.version < header.gamebase.allow_version {
                            // Version is too low
                            3
                        } else {
                            return Ok(ret);
                        }
                    }
                    Err(err) => {
                        log::error!("Save load error: {err}");
                        4
                    }
                }
            }
            Err(err) => {
                log::error!("File read error: {err}");
                4
            }
        }
    };

    Err(ret)
}

pub fn write_global_data(sav_path: &Path, var: &VariableStorage, header: &HeaderInfo) {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path).ok();
    }

    // Don't compress global data since it's pretty small
    std::fs::write(
        sav_path.join(GLOBAL_SAVE_FILE_NAME),
        &rmp_serde::to_vec(&var.get_global_serializable(header)).unwrap(),
    )
    .unwrap();
}

pub fn read_global_data(
    sav_path: &Path,
    header: &HeaderInfo,
) -> Result<SerializableGlobalVariableStorage, i64> {
    let file = sav_path.join(GLOBAL_SAVE_FILE_NAME);

    let ret = if !file.exists() {
        1
    } else {
        match std::fs::read(file) {
            Ok(s) => {
                match rmp_serde::from_read::<_, SerializableGlobalVariableStorage>(s.as_slice()) {
                    Ok(ret) => {
                        if ret.code != header.gamebase.code {
                            // Invalid code
                            2
                        } else if ret.version < header.gamebase.allow_version {
                            // Version is too low
                            3
                        } else {
                            return Ok(ret);
                        }
                    }
                    Err(err) => {
                        log::error!("Save load error: {err}");
                        4
                    }
                }
            }
            Err(err) => {
                log::error!("File read error: {err}");
                4
            }
        }
    };

    Err(ret)
}
