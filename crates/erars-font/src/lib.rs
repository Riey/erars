//! The CPU half of erars' text stack: font selection, shaping and glyph
//! rasterisation, with no GPU and no VM in sight.
//!
//! Two front-ends need exactly this code and cannot share it any other way:
//!
//! * `erars-renderer` lays the console out on a fixed cell grid and uploads
//!   the glyph images to a wgpu atlas ([`text::Shaper`], [`raster::rasterize`]);
//! * `erars-vm` implements `GDRAWTEXT`, which draws GDI+ text straight into an
//!   ARGB bitmap it owns ([`text_image::TextRasterizer::draw`]) — and must
//!   not depend on the renderer, which depends on it.
//!
//! Everything here is deterministic given the same font database, so the
//! renderer's layout goldens and the VM's `GDRAWTEXT` tests can both pin
//! numbers derived from the bundled Noto Sans Mono.

pub mod flags;
pub mod font;
pub mod raster;
pub mod text;
pub mod text_image;
