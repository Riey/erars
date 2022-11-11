use std::sync::Arc;

use erars_proxy_system::ProxyReceiver;
use erars_ui::VirtualConsole;
use erars_vm::{VmContext, TerminalVm};

pub struct EraContext {
    proxy: ProxyReceiver,
}

#[no_mangle]
pub unsafe extern "stdcall" fn erars_new(target_path: *const u8, target_path_len: u32) -> Option<Box<EraContext>> {
    let target_path = std::slice::from_raw_parts(target_path, target_path_len as _);
    let Ok(target_path) = std::str::from_utf8(target_path) else { return None; };

    let config = erars_loader::load_config(target_path);
    let (system, proxy) = erars_proxy_system::new_proxy(Arc::new(|| ()));
    let Ok((vm, mut ctx, mut vconsole)) = erars_loader::run_script(target_path, system, config, false) else { return None; };

    std::thread::spawn(move || {
        vm.start(&mut vconsole, &mut ctx);
    });

    Some(Box::new(EraContext { proxy }))
}

#[no_mangle]
pub unsafe extern "stdcall" fn erars_delete(_: Option<Box<EraContext>>) {}

#[no_mangle]
pub unsafe extern "stdcall" fn erars_recv_msg(ctx: Option<&EraContext>, redraw_cb: fn(), input_cb: fn(i32)) {

}
