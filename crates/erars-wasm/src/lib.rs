use std::{collections::HashMap, sync::Arc};

use erars_ast::{StrKey, Value, VariableInfo};
use erars_compiler::{EraConfig, HeaderInfo};
use erars_ui::{ConsoleSerde, InputRequest, InputRequestType, VirtualConsole};
use erars_vm::{
    SaveList, SerializableGlobalVariableStorage, SerializableVariableStorage, TerminalVm,
    VmContext, VmResult,
};
use js_sys::Promise;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;

#[wasm_bindgen(typescript_custom_section)]
const ITEXT_STYLE: &'static str = r#"
interface ISystemCallbacks {
    input: (console: any) => Promise<int | string>;
    redraw: (console: any) => Promise<void>;
}
"#;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "ISystemCallbacks")]
    #[derive(Clone)]
    pub type ISystemCallbacks;

    #[wasm_bindgen(method)]
    fn input(this: &ISystemCallbacks, console: JsValue) -> Promise;
    #[wasm_bindgen(method)]
    fn redraw(this: &ISystemCallbacks, console: JsValue) -> Promise;
}

#[derive(Clone)]
pub struct WasmSystem {
    callbacks: ISystemCallbacks,
    from: usize,
}

#[async_trait::async_trait(?Send)]
impl erars_vm::SystemFunctions for WasmSystem {
    async fn input(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
    ) -> anyhow::Result<Option<Value>> {
        let console = self.make_console_status(Some(&req), vconsole);
        loop {
            let value = JsFuture::from(self.callbacks.input(console.clone()))
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

    async fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        let console = self.make_console_status(None, vconsole);
        JsFuture::from(self.callbacks.redraw(console))
            .await
            .map_err(|err| anyhow::anyhow!("Js error: {err:?}"))?;
        Ok(())
    }

    async fn load_local_list(&mut self) -> anyhow::Result<SaveList> {
        todo!()
    }
    async fn load_local(
        &mut self,
        idx: u32,
    ) -> anyhow::Result<Option<SerializableVariableStorage>> {
        todo!()
    }
    async fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>> {
        todo!()
    }
    async fn save_local(
        &mut self,
        idx: u32,
        sav: &SerializableVariableStorage,
    ) -> anyhow::Result<()> {
        todo!()
    }
    async fn remove_local(&mut self, idx: u32) -> anyhow::Result<()> {
        todo!()
    }
    async fn save_global(&mut self, sav: &SerializableGlobalVariableStorage) -> anyhow::Result<()> {
        todo!()
    }

    fn clone_functions(&self) -> Box<dyn erars_vm::SystemFunctions> {
        Box::new(self.clone())
    }
}

impl WasmSystem {
    pub fn new(callbacks: ISystemCallbacks) -> Self {
        Self { callbacks, from: 0 }
    }

    pub fn make_console_status(
        &mut self,
        input_req: Option<&InputRequest>,
        vconsole: &mut VirtualConsole,
    ) -> JsValue {
        if vconsole.need_rebuild {
            self.from = vconsole.top_index;
        }

        let console = vconsole.make_serializable(input_req, self.from);
        let lines = console.lines.len();

        let value = serde::Serialize::serialize(
            &console,
            &serde_wasm_bindgen::Serializer::json_compatible(),
        )
        .expect("Serializa falied");

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

    pub async fn run(&mut self) -> bool {
        self.vm.start(&mut self.vconsole, &mut self.ctx).await
    }
}
