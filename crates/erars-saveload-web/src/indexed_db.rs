use erars_vm::SaveLoadManager;
use wasm_bindgen::{JsError, JsValue};
use web_sys::{IdbDatabase, IdbObjectStore};

#[derive(Clone)]
pub struct IndexedDbManager {
    store: IdbObjectStore,
}

impl IndexedDbManager {
    pub fn new(store: IdbObjectStore) -> Self {
        Self {
            store,
        }
    }
}

const GLOBAL_SAV_NAME: &str = "erars_sav_global";

impl SaveLoadManager for IndexedDbManager {
    fn load_local_list(&self) -> erars_vm::SaveList {
        (0..100)
            .into_iter()
            .filter_map(|idx| self.load_local(idx).map(|sav| (idx, sav)))
            .collect()
    }

    fn load_local(&self, idx: u32) -> Option<erars_vm::SerializableVariableStorage> {
        let req = self.store.get(&JsValue::from(idx)).ok()?;
        req.
        None
    }

    fn load_global(&self) -> Option<erars_vm::SerializableGlobalVariableStorage> {
        let value = self.storage.get_item(GLOBAL_SAV_NAME).expect("Get sav item")?;
        serde_json::from_str(&value).ok()
    }

    fn save_local(&self, idx: u32, sav: &erars_vm::SerializableVariableStorage) {
        self.storage
            .set_item(
                &make_save_key(idx),
                serde_json::to_string(sav).unwrap().as_str(),
            )
            .expect("Set sav item");
    }

    fn remove_local(&self, idx: u32) {
        self.storage
            .remove_item(&make_save_key(idx))
            .expect("Remove sav item");
    }

    fn save_global(&self, sav: &erars_vm::SerializableGlobalVariableStorage) {
        self.storage
            .set_item(
                GLOBAL_SAV_NAME,
                serde_json::to_string(sav).unwrap().as_str(),
            )
            .expect("Set sav item");
    }

    fn clone_manager(&self) -> Box<dyn SaveLoadManager> {
        Box::new(self.clone())
    }
}