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
