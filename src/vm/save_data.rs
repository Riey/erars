use flate2::read;
use flate2::write;
use std::path::Path;

use super::{
    variable::{SerializableGlobalVariableStorage, SerializableVariableStorage},
    VariableStorage,
};

pub fn write_save_data(sav_path: &Path, idx: i64, var: &VariableStorage, description: String) {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path).ok();
    }

    let mut file =
        std::fs::File::create(sav_path.join(format!("save{idx:02}.msgpack.gz"))).unwrap();
    let mut encoder = write::GzEncoder::new(&mut file, flate2::Compression::fast());

    rmp_serde::encode::write(&mut encoder, &var.get_serializable(description)).unwrap();
}

pub fn read_save_data(sav_path: &Path, idx: i64) -> Result<SerializableVariableStorage, i64> {
    let file = sav_path.join(format!("save{idx:02}.msgpack.gz"));

    let ret = if !file.exists() {
        1
    } else {
        match std::fs::File::open(file) {
            Ok(compressed) => {
                let mut decoder = read::GzDecoder::new(compressed);
                match rmp_serde::from_read::<_, SerializableVariableStorage>(&mut decoder) {
                    Ok(ret) => return Ok(ret),
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

pub fn write_global_data(sav_path: &Path, var: &VariableStorage) {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path).ok();
    }

    // Don't compress global data since it's pretty small
    std::fs::write(
        sav_path.join(format!("global.msgpack")),
        &rmp_serde::to_vec(&var.get_global_serializable()).unwrap(),
    )
    .unwrap();
}

pub fn read_global_data(sav_path: &Path) -> Result<SerializableGlobalVariableStorage, i64> {
    let file = sav_path.join(format!("global.msgpack"));

    let ret = if !file.exists() {
        1
    } else {
        match std::fs::read(file) {
            Ok(s) => {
                match rmp_serde::from_read::<_, SerializableGlobalVariableStorage>(s.as_slice()) {
                    Ok(ret) => return Ok(ret),
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
