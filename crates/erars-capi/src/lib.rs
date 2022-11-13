use erars_ast::Value;
use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
use erars_vm::{
    SaveList, SerializableGlobalVariableStorage, SerializableVariableStorage, SystemFunctions,
};
use std::{collections::VecDeque, path::PathBuf};
use std::ptr;
use std::path::Path;

#[no_mangle]
pub unsafe extern "stdcall" fn erars_new(
    target_path: *const u8,
    target_path_len: u32,
    call_backs: CapiCallbacks,
) -> u8 {
    let target_path = std::slice::from_raw_parts(target_path, target_path_len as _);
    let Ok(target_path) = std::str::from_utf8(target_path) else { return 1; };

    let config = erars_loader::load_config(target_path);
    let system = Box::new(CapiFrontend::new(
        Path::new(target_path).join("sav"),
        call_backs,
        VecDeque::new(),
    ));
    let Ok((vm, mut ctx, mut vconsole)) = erars_loader::run_script(target_path, system, config, false) else { return 2; };

    pollster::block_on(vm.start(&mut vconsole, &mut ctx));

    0
}

#[repr(C)]
pub struct CapiCallbacks {
    pub draw_cb: extern "stdcall" fn(ptr: *const u8, len: usize),
    pub input_cb: extern "stdcall" fn(is_str: u8, ret: &mut *const u8, ret_len: &mut usize),
}

pub struct CapiFrontend {
    call_backs: CapiCallbacks,
    sav_path: PathBuf,
    from: usize,
    out_buf: Vec<u8>,
    inputs: VecDeque<Value>,
}

impl CapiFrontend {
    pub fn new(sav_path: PathBuf, call_backs: CapiCallbacks, inputs: VecDeque<Value>) -> Self {
        Self {
            sav_path,
            call_backs,
            from: 0,
            out_buf: Vec::new(),
            inputs,
        }
    }

    fn draw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        if vconsole.need_rebuild {
            self.from = vconsole.top_index;
        }

        let ret = vconsole.make_serializable(self.from);
        serde_json::to_writer(&mut self.out_buf, &ret)?;
        (self.call_backs.draw_cb)(self.out_buf.as_ptr(), self.out_buf.len());

        self.from += ret.lines.len();
        vconsole.need_rebuild = false;

        Ok(())
    }
}

#[async_trait::async_trait(?Send)]
impl SystemFunctions for CapiFrontend {
    async fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        if !self.inputs.is_empty() {
            if matches!(req.ty, InputRequestType::Int | InputRequestType::Str) {
                return Ok(self.inputs.pop_front());
            } else {
                return Ok(None);
            }
        }

        loop {
            let mut ret = ptr::null();
            let mut ret_len = 0;
            (self.call_backs.input_cb)(req.ty as u8, &mut ret, &mut ret_len);
            let s = unsafe { std::str::from_utf8(std::slice::from_raw_parts(ret, ret_len))? };

            match req.ty {
                InputRequestType::Int => match s.trim().parse::<i64>() {
                    Ok(i) => {
                        log::info!("[stdio] <- {i}");
                        break Ok(Some(Value::Int(i)));
                    }
                    Err(_) => {
                        continue;
                    }
                },
                InputRequestType::Str => {
                    log::info!("[stdio] <- \"{s}\"");
                    break Ok(Some(Value::String(s.into())));
                }
                InputRequestType::AnyKey
                | InputRequestType::EnterKey
                | InputRequestType::ForceEnterKey => {
                    log::info!("[stdio] <- \"\"");
                    break Ok(None);
                }
            }
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        if !vconsole.need_rebuild && self.from == vconsole.line_count() && vconsole.line_is_empty()
        {
            // skip redraw
            return Ok(());
        }
        self.draw(vconsole)
    }
    async fn load_local_list(&mut self) -> anyhow::Result<SaveList> {
        erars_saveload_fs::load_local_list(&self.sav_path)
    }
    async fn load_local(
        &mut self,
        idx: u32,
    ) -> anyhow::Result<Option<SerializableVariableStorage>> {
        erars_saveload_fs::read_save_data(&self.sav_path, idx)
    }
    async fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>> {
        erars_saveload_fs::read_global_data(&self.sav_path)
    }
    async fn save_local(
        &mut self,
        idx: u32,
        sav: SerializableVariableStorage,
    ) -> anyhow::Result<()> {
        erars_saveload_fs::write_save_data(&self.sav_path, idx, &sav)
    }
    async fn remove_local(&mut self, idx: u32) -> anyhow::Result<()> {
        erars_saveload_fs::delete_save_data(&self.sav_path, idx)
    }
    async fn save_global(&mut self, sav: SerializableGlobalVariableStorage) -> anyhow::Result<()> {
        erars_saveload_fs::write_global_data(&self.sav_path, &sav)
    }
}
