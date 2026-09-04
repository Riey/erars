//! erars GUI renderer: Emuera-parity fixed-cell text layout on wgpu.
//!
//! The binary (`main.rs`) is a thin CLI over these modules; keeping them in a
//! library lets `tests/tui.rs` compile a game, lay it out and render it
//! through exactly the code the window uses.
//!
//! Font selection, shaping and glyph rasterisation are GPU-free and live in
//! [`erars_font`], because `erars-vm`'s `GDRAWTEXT` needs the same code and
//! cannot depend on this crate. They are re-exported here unchanged, so
//! `crate::font::…` / `crate::text::…` / `crate::flags::…` still name them.

pub mod app;
pub mod draw;
pub mod gpu;
pub mod headless;
pub mod images;
pub mod layout;
/// The atlas half of the rasteriser, plus the whole GPU-free half re-exported.
pub mod raster;
#[doc(hidden)]
pub mod test_support;

pub use erars_font::{flags, font, text};
