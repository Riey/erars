use erars_loader::{load_script, run_script};

use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
use erars_vm::{TerminalVm, VmContext, VmResult};

#[repr(u32)]
pub enum ErarsReturn {
    Exit = 0,

    Input = 1,
    InputTimeout = 2,

    Redraw = 10,

    Error = 100,
}

pub struct ErarsContext {
    vm: TerminalVm,
    ctx: VmContext,
    vconsole: VirtualConsole,
    display_str: String,
    input_buf: Vec<u8>,
    input_req: Option<(InputRequest, bool)>,
}

#[no_mangle]
pub unsafe extern "C" fn era_prepare_input_buf(ctx: &mut ErarsContext, size: usize) {
    ctx.input_buf.resize(size, 0);
}

#[no_mangle]
pub unsafe extern "C" fn era_input_buf(ctx: &mut ErarsContext) -> *mut u8 {
    ctx.input_buf.as_mut_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn era_drop(ctx: Option<Box<ErarsContext>>) {
    drop(ctx);
}

#[no_mangle]
pub unsafe extern "C" fn era_display_ptr(ctx: &ErarsContext) -> *const u8 {
    ctx.display_str.as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn era_display_len(ctx: &ErarsContext) -> usize {
    ctx.display_str.len()
}

#[no_mangle]
pub unsafe extern "C" fn era_new(
    target_path: *const u8,
    target_len: usize,
    is_load: i32,
) -> Option<Box<ErarsContext>> {
    let slice = std::slice::from_raw_parts(target_path, target_len);
    let target_path = std::str::from_utf8(slice).ok()?;

    let (vm, ctx, vconsole) = if is_load != 0 {
        load_script(target_path.into(), Vec::new()).ok()?
    } else {
        run_script(target_path.into(), Vec::new()).ok()?
    };

    Some(Box::new(ErarsContext {
        vm,
        ctx,
        vconsole,
        display_str: String::new(),
        input_buf: Vec::new(),
        input_req: None,
    }))
}

#[no_mangle]
pub unsafe extern "C" fn era_push_input_int(ctx: &mut ErarsContext, int: i64) -> i32 {
    match ctx.input_req {
        Some((
            InputRequest {
                ty: InputRequestType::Int,
                ..
            },
            set_result,
        )) => {
            if set_result {
                ctx.ctx.var.set_result(int);
            } else {
                ctx.ctx.push(int);
            }
            ctx.input_req = None;
            0
        }
        _ => -1,
    }
}

#[no_mangle]
pub unsafe extern "C" fn era_push_input_str(ctx: &mut ErarsContext, len: usize) -> i32 {
    match ctx.input_req {
        Some((
            InputRequest {
                ty: InputRequestType::Str,
                ..
            },
            set_result,
        )) => {
            if ctx.input_buf.len() < len {
                return -2;
            }
            let s = match std::str::from_utf8(&ctx.input_buf[..len]) {
                Ok(s) => s,
                Err(_) => return -3,
            }
            .to_string();

            if set_result {
                ctx.ctx.var.set_results(s);
            } else {
                ctx.ctx.push(s);
            }

            ctx.input_req = None;
            0
        }
        _ => -1,
    }
}

#[no_mangle]
pub unsafe extern "C" fn era_run(ctx: &mut ErarsContext) -> ErarsReturn {
    match ctx.vm.run_state(&mut ctx.vconsole, &mut ctx.ctx) {
        VmResult::Redraw => {
            ctx.display_str = serde_json::to_string(ctx.vconsole.lines()).unwrap();
            ErarsReturn::Redraw
        }
        VmResult::NeedInput { req, set_result } => {
            let ret = if req.timeout.is_some() {
                ErarsReturn::InputTimeout
            } else {
                ErarsReturn::Input
            };
            ctx.input_req = Some((req, set_result));
            ret
        }
        VmResult::Exit => ErarsReturn::Exit,
    }
}
