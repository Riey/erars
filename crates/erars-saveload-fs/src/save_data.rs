use flate2::read;
use flate2::write;
use std::path::Path;

use erars_vm::{SerializableGlobalVariableStorage, SerializableVariableStorage};

fn make_save_file_name(idx: u32) -> String {
    format!("save{idx:02}.msgpack.gz")
}

static GLOBAL_SAVE_FILE_NAME: &str = "global.msgpack";

pub fn write_save_data(sav_path: &Path, idx: u32, sav: &SerializableVariableStorage) {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path).ok();
    }

    let mut file = std::fs::File::create(sav_path.join(make_save_file_name(idx))).unwrap();
    let mut encoder = write::GzEncoder::new(&mut file, flate2::Compression::fast());

    rmp_serde::encode::write(&mut encoder, &sav).unwrap();
}

pub fn delete_save_data(sav_path: &Path, idx: u32) -> std::io::Result<()> {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path)?;
    }

    std::fs::remove_file(sav_path.join(make_save_file_name(idx)))
}

pub fn read_save_data(sav_path: &Path, idx: u32) -> Option<SerializableVariableStorage> {
    let file = sav_path.join(make_save_file_name(idx));

    let compressed = std::fs::File::open(file).ok()?;
    let mut decoder = read::GzDecoder::new(compressed);
    rmp_serde::from_read::<_, SerializableVariableStorage>(&mut decoder).ok()
}

pub fn write_global_data(sav_path: &Path, sav: &SerializableGlobalVariableStorage) {
    if !sav_path.exists() {
        std::fs::create_dir(&sav_path).ok();
    }

    // Don't compress global data since it's pretty small
    std::fs::write(
        sav_path.join(GLOBAL_SAVE_FILE_NAME),
        &rmp_serde::to_vec(&sav).unwrap(),
    )
    .ok();
}

pub fn read_global_data(sav_path: &Path) -> Option<SerializableGlobalVariableStorage> {
    let file = sav_path.join(GLOBAL_SAVE_FILE_NAME);
    let s = std::fs::read(file).ok()?;
    rmp_serde::from_read::<_, SerializableGlobalVariableStorage>(s.as_slice()).ok()
}
