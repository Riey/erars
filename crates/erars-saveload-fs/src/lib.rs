mod save_data;

#[cfg(feature = "multithread")]
use rayon::prelude::*;

use std::path::Path;

pub use save_data::{
    delete_save_data, read_global_data, read_save_data, write_global_data, write_save_data,
};

pub fn load_local_list(sav_path: &Path) -> anyhow::Result<erars_vm::SaveList> {
    let sav_idxs = 0..100;
    #[cfg(not(feature = "multithread"))]
    let iter = sav_idxs.into_iter();
    #[cfg(feature = "multithread")]
    let iter = sav_idxs.into_par_iter();

    iter.filter_map(|idx| {
        save_data::read_save_data(sav_path, idx)
            .transpose()
            .map(|sav| sav.map(|sav| (idx, sav)))
    })
    .collect::<anyhow::Result<_>>()
}
