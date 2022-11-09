use std::{collections::HashMap, sync::Arc};

use anyhow::bail;
use erars_ast::{StrKey, Value, VariableInfo};
use erars_compiler::{EraConfig, HeaderInfo};
use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
use erars_vm::{
    SaveList, SerializableGlobalVariableStorage, SerializableVariableStorage, TerminalVm, VmContext,
};
use js_sys::Promise;
use serde::Serialize;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;

#[wasm_bindgen(typescript_custom_section)]
const ITEXT_STYLE: &'static str = r#"
interface ISystemCallbacks {
    input: (input_req: any) => Promise<bigint | string>;
    redraw: (console: any);

    load_local: (idx: number) => Promise<string | null>;
    load_global: () => Promise<string | null>;
    save_local: (idx: number, sav: string) => Promise<void>;
    save_global: (sav: string) => Promise<void>;
    remove_local: (idx: number) => Promise<void>;
}
"#;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "ISystemCallbacks")]
    #[derive(Clone)]
    pub type ISystemCallbacks;

    #[wasm_bindgen(method)]
    fn input(this: &ISystemCallbacks, input_req: JsValue) -> Promise;
    #[wasm_bindgen(method)]
    fn redraw(this: &ISystemCallbacks, console: JsValue);

    #[wasm_bindgen(method)]
    fn load_local(this: &ISystemCallbacks, idx: u32) -> Promise;
    #[wasm_bindgen(method)]
    fn load_global(this: &ISystemCallbacks) -> Promise;
    #[wasm_bindgen(method)]
    fn save_local(this: &ISystemCallbacks, idx: u32, sav: String) -> Promise;
    #[wasm_bindgen(method)]
    fn save_global(this: &ISystemCallbacks, sav: String) -> Promise;
    #[wasm_bindgen(method)]
    fn remove_local(this: &ISystemCallbacks, idx: u32) -> Promise;
}

pub struct WasmSystem {
    callbacks: ISystemCallbacks,
    from: usize,
}

const JS_SERIALIZER: serde_wasm_bindgen::Serializer = serde_wasm_bindgen::Serializer::new()
    .serialize_large_number_types_as_bigints(true)
    .serialize_maps_as_objects(false)
    .serialize_missing_as_null(false);

#[async_trait::async_trait(?Send)]
impl erars_vm::SystemFunctions for WasmSystem {
    async fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        let req_js = req.serialize(&JS_SERIALIZER).expect("Serialize InputRequest");

        loop {
            let value = JsFuture::from(self.callbacks.input(req_js.clone()))
                .await
                .map_err(|err| anyhow::anyhow!("Js error: {err:?}"))?;

            match req.ty {
                InputRequestType::Int => match i64::try_from(value) {
                    Ok(n) => return Ok(Some(Value::Int(n))),
                    _ => {
                        log::error!("Invalid input");
                    }
                },
                InputRequestType::Str => match value.as_string() {
                    Some(s) => return Ok(Some(Value::String(s))),
                    _ => {
                        log::error!("Invalid input");
                    }
                },
                _ => return Ok(None),
            }
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        let console = self.make_console_status(vconsole);
        self.callbacks.redraw(console);
        Ok(())
    }

    async fn load_local_list(&mut self) -> anyhow::Result<SaveList> {
        let mut list = SaveList::new();

        for idx in 0..100 {
            if let Some(sav) = self.load_local(idx).await? {
                list.insert(idx, sav);
            }
        }

        Ok(list)
    }
    async fn load_local(
        &mut self,
        idx: u32,
    ) -> anyhow::Result<Option<SerializableVariableStorage>> {
        let ret = JsFuture::from(self.callbacks.load_local(idx))
            .await
            .map_err(|err| anyhow::anyhow!("Js error: {err:?}"))?;

        if ret.is_null() {
            Ok(None)
        } else if let Some(ret) = ret.as_string() {
            let ret = serde_json::from_str(&ret)?;
            Ok(Some(ret))
        } else {
            bail!("Invalid save return {ret:?}");
        }
    }
    async fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>> {
        let ret = JsFuture::from(self.callbacks.load_global())
            .await
            .map_err(|err| anyhow::anyhow!("Js error: {err:?}"))?;
        if ret.is_null() {
            Ok(None)
        } else if let Some(ret) = ret.as_string() {
            let ret = serde_json::from_str(&ret)?;
            Ok(Some(ret))
        } else {
            bail!("Invalid save return {ret:?}");
        }
    }
    async fn save_local(
        &mut self,
        idx: u32,
        sav: SerializableVariableStorage,
    ) -> anyhow::Result<()> {
        JsFuture::from(self.callbacks.save_local(idx, serde_json::to_string(&sav)?))
            .await
            .map_err(|err| anyhow::anyhow!("Js error: {err:?}"))?;
        Ok(())
    }
    async fn remove_local(&mut self, idx: u32) -> anyhow::Result<()> {
        JsFuture::from(self.callbacks.remove_local(idx))
            .await
            .map_err(|err| anyhow::anyhow!("Js error: {err:?}"))?;
        Ok(())
    }
    async fn save_global(&mut self, sav: SerializableGlobalVariableStorage) -> anyhow::Result<()> {
        JsFuture::from(self.callbacks.save_global(serde_json::to_string(&sav)?))
            .await
            .map_err(|err| anyhow::anyhow!("Js error: {err:?}"))?;
        Ok(())
    }
}

impl WasmSystem {
    pub fn new(callbacks: ISystemCallbacks) -> Self {
        Self { callbacks, from: 0 }
    }

    pub fn make_console_status(&mut self, vconsole: &mut VirtualConsole) -> JsValue {
        if vconsole.need_rebuild {
            self.from = vconsole.top_index;
        }

        let console = vconsole.make_serializable(self.from);
        let lines = console.lines.len();

        let value = console.serialize(&JS_SERIALIZER).expect("Serialize Console falied");

        self.from += lines;
        vconsole.need_rebuild = false;

        value
    }
}

#[wasm_bindgen]
pub fn init_logger() {
    log_panics::init();
    wasm_logger::init(wasm_logger::Config::default());
}

#[wasm_bindgen]
pub struct ErarsContext {
    vm: TerminalVm,
    ctx: VmContext,
    vconsole: VirtualConsole,
}

#[wasm_bindgen]
impl ErarsContext {
    #[wasm_bindgen(constructor)]
    pub fn new(mut era_file: &[u8], config_text: &str, callbacks: ISystemCallbacks) -> Self {
        log::info!("File length: {}", era_file.len());
        let config = EraConfig::from_text(config_text).expect("Parse config file");
        log::info!("Config: {config:?}");
        let dic = unsafe { erars_bytecode::read_from(&mut era_file).expect("Read era file") };
        let (header, local_infos): (HeaderInfo, HashMap<StrKey, Vec<(StrKey, VariableInfo)>>) =
            rmp_serde::decode::from_slice(&mut era_file).expect("Read game data");

        let mut ctx = VmContext::new(
            Arc::new(header),
            Arc::new(config),
            Box::new(WasmSystem::new(callbacks)),
        );

        for (key, vars) in local_infos {
            for var in vars {
                ctx.var.add_local_info(key, var.0, var.1);
            }
        }

        Self {
            vm: TerminalVm { dic },
            vconsole: VirtualConsole::new(ctx.config.printc_width, ctx.config.max_log),
            ctx,
        }
    }

    pub fn get_config(&self) -> JsValue {
        serde::Serialize::serialize(
            &*self.ctx.config,
            &serde_wasm_bindgen::Serializer::json_compatible(),
        )
        .expect("Serialize falied")
    }

    pub async fn run(&mut self) -> bool {
        self.vm.start(&mut self.vconsole, &mut self.ctx).await
    }
}
