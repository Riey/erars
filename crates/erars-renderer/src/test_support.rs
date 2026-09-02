//! Shared helpers for tests.

use std::sync::{Mutex, MutexGuard, OnceLock};

/// Serialize tests that create a wgpu device. Software adapters (llvmpipe) can
/// fail or render incompletely when several devices are built concurrently, so
/// every GPU-touching test holds this lock for its duration.
pub fn gpu_lock() -> MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap_or_else(|e| e.into_inner())
}

/// The running test's name (cargo names each test thread after the test).
pub fn test_name() -> String {
    std::thread::current()
        .name()
        .unwrap_or("<unnamed test>")
        .to_string()
}

/// A headless wgpu device, or `None` after printing `SKIP <test>: no wgpu
/// adapter` on stderr. With `ERARS_REQUIRE_GPU=1` (CI with lavapipe) the
/// missing adapter is a test failure instead of a skip.
pub fn gpu_device() -> Option<(wgpu::Device, wgpu::Queue)> {
    let instance = wgpu::Instance::default();
    let adapter =
        pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions::default()));
    let device = adapter.and_then(|adapter| {
        pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                label: Some("erars-test"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::downlevel_defaults(),
            },
            None,
        ))
        .ok()
    });
    if device.is_none() {
        let name = test_name();
        if std::env::var_os("ERARS_REQUIRE_GPU").is_some_and(|v| v == "1") {
            panic!("{name}: ERARS_REQUIRE_GPU=1 but no wgpu adapter is available");
        }
        eprintln!("SKIP {name}: no wgpu adapter");
    }
    device
}
