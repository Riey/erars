use std::{collections::HashMap, sync::Arc};

use erars_ast::{StrKey, VariableInfo};
use erars_compiler::{EraConfig, HeaderInfo};
use erars_ui::{ConsoleSerde, InputRequest, InputRequestType, VirtualConsole};
use erars_vm::{TerminalVm, VmContext, VmResult};
use wasm_bindgen::prelude::*;

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
    input_req: Option<(InputRequest, bool)>,
    from: usize,
}

#[wasm_bindgen]
impl ErarsContext {
    #[wasm_bindgen(constructor)]
    pub fn new(mut era_file: &[u8], config_text: &str) -> Self {
        log::info!("File length: {}", era_file.len());
        let config = EraConfig::from_text(config_text).expect("Parse config file");
        log::info!("Config: {config:?}");
        let dic = unsafe { erars_bytecode::read_from(&mut era_file).expect("Read era file") };
        let (header, local_infos): (HeaderInfo, HashMap<StrKey, Vec<(StrKey, VariableInfo)>>) =
            rmp_serde::decode::from_slice(&mut era_file).expect("Read game data");

        let mut ctx = VmContext::new(
            Arc::new(header),
            Arc::new(config),
            Box::new(erars_saveload_web::LocalStorageManager::new().expect("Get LocalStorage")),
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
            input_req: None,
            from: 0,
        }
    }

    pub fn set_input(&mut self, s: String) -> bool {
        match self.input_req.as_ref() {
            Some((req, set_result)) if req.ty == InputRequestType::Int => match s.parse::<i64>() {
                Ok(i) => {
                    let set_result = *set_result;
                    self.input_req = None;
                    if set_result {
                        self.ctx.var.set_result(i);
                    } else {
                        self.ctx.push(i);
                    }
                    true
                }
                Err(err) => {
                    log::error!("Invalid input: {err}");
                    false
                }
            },
            Some((req, set_result)) if req.ty == InputRequestType::Str => {
                let set_result = *set_result;
                self.input_req = None;
                if set_result {
                    self.ctx.var.set_results(s);
                } else {
                    self.ctx.push(s);
                }
                true
            }
            _ => false,
        }
    }

    pub fn run(&mut self) -> JsValue {
        if self.input_req.is_some() {
            log::error!("Input is required current_req: {:?}", self.input_req);
            return JsValue::NULL;
        }

        let exited = match self.vm.run_state(&mut self.vconsole, &mut self.ctx) {
            VmResult::Input { req, set_result } => {
                if req.timeout.is_some() {
                    log::error!("TODO: timeout input");
                }
                self.input_req = Some((req, set_result));
                false
            }
            VmResult::Redraw => false,
            VmResult::Exit => true,
        };

        #[derive(serde::Serialize)]
        struct Ret<'a> {
            #[serde(flatten)]
            console: ConsoleSerde<'a>,
            exited: bool,
        }

        let console = self
            .vconsole
            .make_serializable(self.input_req.as_ref().map(|(r, _)| r), self.from);
        let lines = console.lines.len();

        if self.vconsole.need_rebuild {
            self.from = self.vconsole.top_index;
        }

        let ret = serde::Serialize::serialize(
            &Ret { exited, console },
            &serde_wasm_bindgen::Serializer::json_compatible(),
        )
        .expect("Serialize failed");

        self.from += lines;
        self.vconsole.need_rebuild = false;

        ret
    }
}
