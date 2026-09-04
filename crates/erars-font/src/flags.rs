//! Synthetic-style flags shared by the shaper (`text.rs`), the layout and the
//! rasteriser. Lives in its own module so the GPU-free layers never import
//! the GPU module (spec Component 4, critique R34).

bitflags::bitflags! {
    /// Set by `FontChain::resolve` when no real bold / italic face of the
    /// resolved family exists; the rasteriser then emboldens / skews the
    /// outline instead (spec Component 6).
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
    pub struct RasterFlags: u8 {
        const BOLD_SYNTH = 1;
        const ITALIC_SYNTH = 2;
    }
}

#[cfg(test)]
mod tests {
    use super::RasterFlags;

    #[test]
    fn flags_are_independent_bits() {
        assert_eq!(RasterFlags::BOLD_SYNTH.bits(), 1);
        assert_eq!(RasterFlags::ITALIC_SYNTH.bits(), 2);
        assert!(RasterFlags::empty().is_empty());
        assert_eq!(RasterFlags::default(), RasterFlags::empty());
        let both = RasterFlags::BOLD_SYNTH | RasterFlags::ITALIC_SYNTH;
        assert!(both.contains(RasterFlags::BOLD_SYNTH));
        assert!(both.contains(RasterFlags::ITALIC_SYNTH));
        assert_eq!(both.bits(), 3);
    }
}
