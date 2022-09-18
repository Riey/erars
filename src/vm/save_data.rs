use std::path::Path;

use super::{
    variable::{SerializableGlobalVariableStorage, SerializableVariableStorage},
    VariableStorage,
};

pub fn write_save_data(sav_path: &Path, idx: i64, var: &VariableStorage, description: String) {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path).ok();
    }

    std::fs::write(
        sav_path.join(format!("save{idx:02}.msgpack")),
        &rmp_serde::to_vec(&var.get_serializable(description)).unwrap(),
    )
    .unwrap();
}

pub fn read_save_data(sav_path: &Path, idx: i64) -> Result<SerializableVariableStorage, i64> {
    let file = sav_path.join(format!("save{idx:02}.msgpack"));

    let ret = if !file.exists() {
        1
    } else {
        match std::fs::read(file) {
            Ok(s) => match rmp_serde::from_read::<_, SerializableVariableStorage>(s.as_slice()) {
                Ok(ret) => return Ok(ret),
                Err(err) => {
                    log::error!("Save load error: {err}");
                    4
                }
            },
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
