//! `cfg(feature = "inst-counter")`-gated dynamic (execution-frequency)
//! instruction histogram, for comparison against `jit_census`'s *static*
//! (occurrence-in-the-compiled-stream) histogram. A hot loop executed a
//! million times and a cold menu branch executed once weigh the same in a
//! static census; they do not at runtime, and only a real, played session
//! run through the actual executor can show the difference.
//!
//! `record` is called from [`crate::terminal_vm::executor::run_instruction`]
//! on every dispatch. With the feature off, both functions below compile to
//! nothing: no storage, no branch, no call -- this must never cost anything
//! in a timing build, which is the entire reason it is not simply always on.
//! `dump` is called once, by the embedding binary (`erars-stdio`), after the
//! game loop returns.

#[cfg(feature = "inst-counter")]
mod imp {
    use std::sync::atomic::{AtomicU64, Ordering};

    use erars_compiler::InstructionType;

    // `InstructionType` is `#[repr(u8)]` with sparse discriminants up to
    // `Debug = 255` (see `erars-compiler/src/instruction.rs`) -- a flat
    // 256-slot table indexed by the raw discriminant is simpler and cheaper
    // than a `HashMap` behind a lock, at the cost of 2KB of always-static
    // memory the feature already opts into.
    const SLOTS: usize = 256;
    static COUNTS: [AtomicU64; SLOTS] = {
        #[allow(clippy::declare_interior_mutable_const)]
        const ZERO: AtomicU64 = AtomicU64::new(0);
        [ZERO; SLOTS]
    };

    #[inline]
    pub fn record(ty: InstructionType) {
        COUNTS[ty as usize].fetch_add(1, Ordering::Relaxed);
    }

    pub fn dump() {
        let mut rows: Vec<(InstructionType, u64)> = COUNTS
            .iter()
            .enumerate()
            .filter_map(|(i, c)| {
                let c = c.load(Ordering::Relaxed);
                if c == 0 {
                    return None;
                }
                // SAFETY: every nonzero slot was written by `record` above,
                // whose only caller passes a real `InstructionType`'s own
                // `as usize` discriminant -- `i` is therefore always one of
                // that enum's defined values, never an arbitrary byte.
                let ty = unsafe { std::mem::transmute::<u8, InstructionType>(i as u8) };
                Some((ty, c))
            })
            .collect();
        rows.sort_by(|a, b| b.1.cmp(&a.1));

        let total: u64 = rows.iter().map(|(_, c)| c).sum();
        eprintln!("\n=== dynamic (executed) instruction histogram, {total} total dispatches ===");
        for (ty, count) in &rows {
            eprintln!("{ty:<20} {count:>12}  {:.4}%", *count as f64 / total as f64 * 100.0);
        }
    }
}

#[cfg(not(feature = "inst-counter"))]
mod imp {
    use erars_compiler::InstructionType;

    #[inline(always)]
    pub fn record(_ty: InstructionType) {}

    #[inline(always)]
    pub fn dump() {}
}

pub use imp::{dump, record};
