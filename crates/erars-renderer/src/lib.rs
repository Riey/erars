//! erars GUI renderer: Emuera-parity fixed-cell text layout on wgpu.
//!
//! The binary (`main.rs`) is a thin CLI over these modules; keeping them in a
//! library lets `tests/tui.rs` compile a game, lay it out and render it
//! through exactly the code the window uses.

pub mod app;
pub mod draw;
pub mod flags;
pub mod font;
pub mod gpu;
pub mod headless;
pub mod layout;
pub mod raster;
#[doc(hidden)]
pub mod test_support;
pub mod text;
