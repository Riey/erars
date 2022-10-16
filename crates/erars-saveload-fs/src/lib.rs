mod save_data;

#[cfg(feature = "multithread")]
use rayon::prelude::*;

use std::path::PathBuf;

use erars_vm::SaveLoadManager;

pub struct FsSaveManager {
    pub sav_path: PathBuf,
}

impl FsSaveManager {
    pub fn new(sav_path: PathBuf) -> Self {
        Self { sav_path }
    }
}

impl SaveLoadManager for FsSaveManager {
    fn load_local_list(&self) -> erars_vm::SaveList {
        let sav_idxs = 0..100;
        #[cfg(not(feature = "multithread"))]
        let iter = sav_idxs.into_iter();
        #[cfg(feature = "multithread")]
        let iter = sav_idxs.into_par_iter();

        iter.filter_map(|idx| save_data::read_save_data(&self.sav_path, idx).map(|sav| (idx, sav)))
            .collect()
    }

    fn load_local(&self, idx: u32) -> Option<erars_vm::SerializableVariableStorage> {
        save_data::read_save_data(&self.sav_path, idx)
    }

    fn load_global(&self) -> Option<erars_vm::SerializableGlobalVariableStorage> {
        save_data::read_global_data(&self.sav_path)
    }

    fn save_local(&self, idx: u32, sav: &erars_vm::SerializableVariableStorage) {
        save_data::write_save_data(&self.sav_path, idx, sav)
    }

    fn remove_local(&self, idx: u32) {
        save_data::delete_save_data(&self.sav_path, idx).ok();
    }

    fn save_global(&self, sav: &erars_vm::SerializableGlobalVariableStorage) {
        save_data::write_global_data(&self.sav_path, sav);
    }

    fn clone_manager(&self) -> Box<dyn SaveLoadManager> {
        Box::new(Self::new(self.sav_path.clone()))
    }
}
