//! Offscreen rendering for headless environments (SSH/CI, no display server)
//! and the `--headless-shot` CLI.
//!
//! `render_frame` draws a `ConsoleFrame` through the same path as the window
//! (`layout::layout_frame_no_sweep` → `draw::build_instances` →
//! `gpu::{create_quad_pipeline, nearest_sampler, FrameDraw}`) into an
//! `Rgba8Unorm` texture and reads it
//! back, so pixel positions can be asserted without a display. The target is
//! linear (the window path clears an sRGB surface), so headless bytes are
//! compared with each other and with exact 0/255 masks — never with the window.
//!
//! View math (spec Component 5) with `scroll_rows = 0`: `strip_h = line_h`,
//! `view_h = height − strip_h`, row `r` at `view_h − (bottom_row − r + 1)·line_h`,
//! so slack appears at the top. The input strip shows `> {input}_` in
//! `frame.fore_color` on the bottom `line_h` rows when `input` is `Some`.

use std::io::Write;
use std::path::{Path, PathBuf};

use erars_compiler::EraConfig;
use erars_proxy_system::ConsoleFrame;
use erars_ui::width::WidthTable;
use wgpu::util::DeviceExt;

use crate::draw::{build_instances, cbg_quads, input_line, ImageCtx, View};
use crate::font::{FontChain, FontConfig};
use crate::gpu::{
    create_quad_pipeline, linear_sampler, nearest_sampler, DrawGroup, Filter, FrameDraw, Globals,
};
use crate::images::ImageTextures;
use crate::layout::{layout_frame_no_sweep, layout_no_sweep, Geometry};
use crate::raster::GlyphRaster;
use crate::text::{CellMetrics, Shaper};

/// A rendered RGBA8 image (row-major, 4 bytes/pixel, no row padding).
pub struct Rendered {
    pub width: u32,
    pub height: u32,
    pub rgba: Vec<u8>,
}

impl Rendered {
    pub fn pixel(&self, x: u32, y: u32) -> [u8; 4] {
        let i = ((y * self.width + x) * 4) as usize;
        [self.rgba[i], self.rgba[i + 1], self.rgba[i + 2], self.rgba[i + 3]]
    }

    /// The bytes of rows `[y0, y1)`, clamped to the image (empty when the band
    /// starts at or past its end).
    pub fn band(&self, y0: u32, y1: u32) -> &[u8] {
        let stride = self.width as usize * 4;
        let y1 = y1.min(self.height) as usize;
        let y0 = (y0 as usize).min(y1);
        &self.rgba[y0 * stride..y1 * stride]
    }

    /// Per column: does any pixel in rows `[y0, y1)` have a channel ≥ `min`?
    pub fn ink_columns(&self, y0: u32, y1: u32, min: u8) -> Vec<bool> {
        let mut cols = vec![false; self.width as usize];
        for y in y0..y1.min(self.height) {
            for x in 0..self.width {
                let [r, g, b, _] = self.pixel(x, y);
                if r.max(g).max(b) >= min {
                    cols[x as usize] = true;
                }
            }
        }
        cols
    }

    /// Bounding box `[x_min, y_min, x_max_excl, y_max_excl]` of pixels with a
    /// channel ≥ `min` inside `[x0, x1) × [y0, y1)`, or `None` if there are none.
    pub fn ink_bbox(&self, x0: u32, x1: u32, y0: u32, y1: u32, min: u8) -> Option<[u32; 4]> {
        let mut bb: Option<[u32; 4]> = None;
        for y in y0..y1.min(self.height) {
            for x in x0..x1.min(self.width) {
                let [r, g, b, _] = self.pixel(x, y);
                if r.max(g).max(b) < min {
                    continue;
                }
                bb = Some(match bb {
                    None => [x, y, x + 1, y + 1],
                    Some([a, b2, c, d]) => [a.min(x), b2.min(y), c.max(x + 1), d.max(y + 1)],
                });
            }
        }
        bb
    }

    /// Sum of pixel luminance over the rows `[y0, y1)` for each column x.
    pub fn column_ink(&self, y0: u32, y1: u32) -> Vec<f32> {
        let y1 = y1.min(self.height);
        let mut prof = vec![0.0f32; self.width as usize];
        for y in y0..y1 {
            for x in 0..self.width {
                let [r, g, b, _] = self.pixel(x, y);
                prof[x as usize] += 0.299 * r as f32 + 0.587 * g as f32 + 0.114 * b as f32;
            }
        }
        prof
    }

    /// Rightmost column whose ink exceeds `threshold`, or 0 if none.
    pub fn ink_right_edge(prof: &[f32], threshold: f32) -> usize {
        prof.iter().rposition(|&v| v > threshold).unwrap_or(0)
    }
}

/// Why a headless render could not be produced. `render_frame*` return these
/// instead of panicking: an oversized frame is rejected before any wgpu call
/// that would trip validation, so the CLI can report it and exit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RenderError {
    /// This machine has no wgpu adapter at all.
    NoAdapter,
    /// The requested frame is larger than the adapter's maximum texture size.
    TooLarge { width: u32, height: u32, max: u32 },
}

impl std::fmt::Display for RenderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoAdapter => f.write_str("no GPU adapter available for headless rendering"),
            Self::TooLarge { width, height, max } => write!(
                f,
                "frame {width}x{height} exceeds the adapter's max texture size {max}"
            ),
        }
    }
}

impl std::error::Error for RenderError {}

/// The default adapter/device without a surface. `None` when this machine has
/// no wgpu adapter (tests skip or fail through `test_support::gpu_device`).
///
/// The downlevel limits are kept (the atlas page size is tied to them) except
/// for the texture dimensions, which are raised to whatever the adapter really
/// supports: a game may configure a window wider or taller than the downlevel
/// 2048 px ceiling, and the offscreen target is that size.
pub fn request_device() -> Option<(wgpu::Device, wgpu::Queue)> {
    let instance = wgpu::Instance::default();
    let adapter =
        pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions::default()))?;
    pollster::block_on(adapter.request_device(
        &wgpu::DeviceDescriptor {
            label: Some("erars-headless"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::downlevel_defaults()
                .using_resolution(adapter.limits()),
        },
        None,
    ))
    .ok()
}

/// The shaper for a game: fonts from the configured family → `<game>/font`
/// → `ERARS_FONT_DIR` → the language's fixed-pitch CJK list → the bundled
/// font (spec Component 3), cell metrics from the primary face at scale 1
/// (headless has no window scale; the window applies its real scale factor
/// through `Shaper::set_metrics`). Shared by `--headless-shot` and the app.
pub fn shaper_for(config: &EraConfig, game_dir: &Path) -> Shaper {
    let mut chain = FontChain::new(&FontConfig {
        family: &config.font_family,
        game_dir,
        extra_dir: std::env::var_os("ERARS_FONT_DIR").map(PathBuf::from),
        lang: config.lang,
    });
    let primary = chain.font(chain.primary());
    let m = CellMetrics::from_primary(&primary, config.font_size, config.line_height, 1.0);
    Shaper::new(chain, WidthTable::new(config.lang.encoding()), m)
}

/// Render `frame` into a `content_w × height` image with `scroll_rows = 0`
/// (bitmap strikes on). [`RenderError::NoAdapter`] if this machine has no GPU,
/// [`RenderError::TooLarge`] if the frame exceeds the adapter's texture size.
pub fn render_frame(
    shaper: &mut Shaper,
    frame: &ConsoleFrame,
    content_w: u32,
    height: u32,
    input: Option<&str>,
    hover: Option<usize>,
) -> Result<Rendered, RenderError> {
    render_frame_opts(shaper, frame, content_w, height, input, hover, true)
}

/// [`render_frame`] with the `--no-bitmap-strikes` switch.
pub fn render_frame_opts(
    shaper: &mut Shaper,
    frame: &ConsoleFrame,
    content_w: u32,
    height: u32,
    input: Option<&str>,
    hover: Option<usize>,
    use_bitmap_strikes: bool,
) -> Result<Rendered, RenderError> {
    let (device, queue) = request_device().ok_or(RenderError::NoAdapter)?;
    render_frame_on(
        &device,
        &queue,
        shaper,
        frame,
        content_w,
        height,
        input,
        hover,
        use_bitmap_strikes,
    )
}

/// [`render_frame`] on an existing device. `hover` indexes `Layout.buttons`
/// of the log layout and recolours that fragment with `frame.hl_color` (draw
/// time only, nothing moves).
///
/// The frame size is checked against `device.limits()` first, so an oversized
/// request comes back as [`RenderError::TooLarge`] rather than aborting inside
/// wgpu's texture validation.
#[allow(clippy::too_many_arguments)]
pub fn render_frame_on(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    shaper: &mut Shaper,
    frame: &ConsoleFrame,
    content_w: u32,
    height: u32,
    input: Option<&str>,
    hover: Option<usize>,
    use_bitmap_strikes: bool,
) -> Result<Rendered, RenderError> {
    let content_w = content_w.max(1);
    let height = height.max(1);
    let max = device.limits().max_texture_dimension_2d;
    if content_w > max || height > max {
        return Err(RenderError::TooLarge {
            width: content_w,
            height,
            max,
        });
    }
    let m = *shaper.metrics();
    let g = Geometry::new(content_w, m);
    let mut raster = GlyphRaster::new(device, use_bitmap_strikes);
    let hl = frame.hl_color.0;

    // Log rows: bottom-anchored above the input strip.
    let strip_h = m.line_h;
    let view = View {
        scroll_rows: 0,
        view_h: height.saturating_sub(strip_h),
        strip_h,
    };
    let log = layout_frame_no_sweep(&frame.lines, &frame.islands, &g, shaper);
    // A one-shot render is an animation's *first* draw, which is exactly when
    // Emuera latches `StartTime` and shows frame 0
    // (`Content/CroppedImage.cs:229-235`), so `now_ms = 0` is both faithful
    // and deterministic.
    let images = ImageCtx {
        store: &frame.images,
        now_ms: 0,
    };
    let fg = frame.fore_color.0;
    let mut quads = build_instances(
        &log, &view, hover, hl, fg, &mut raster, device, queue, shaper, images,
    );

    // Input strip: one line laid out on its own, drawn on the bottom `line_h` rows.
    if let Some(input) = input {
        let line = input_line(input, frame.fore_color.0);
        let strip = layout_no_sweep(std::slice::from_ref(&line), &g, shaper);
        let strip_quads = build_instances(
            &strip,
            &view.strip(),
            None,
            hl,
            fg,
            &mut raster,
            device,
            queue,
            shaper,
            images,
        );
        quads.merge(strip_quads);
    }
    // One sweep per rendered frame, after both layouts: sweeping between them
    // would drop the log's entries (see `layout_no_sweep`).
    shaper.sweep();
    // The plane is client-absolute, so it joins after both layouts. No cursor
    // in a one-shot render, so no button is selected.
    quads.merge(cbg_quads(&frame.cbg, view.view_h as i32, -1, images));
    quads.fit_pages(raster.page_count());

    let mut textures = ImageTextures::new();
    textures.sync(device, queue, &frame.images, &quads.bitmaps());

    let glyphs = raster.pages_with(&quads.glyphs);
    let under = textures.pages_with(&quads.under);
    let inline = textures.pages_with(&quads.images);
    let over = textures.pages_with(&quads.over);
    // Placed boxes and island overlays, lowest slice first (`Quads::overlays`).
    let overlays: Vec<_> = quads
        .overlays
        .iter()
        .map(|s| (raster.pages_with(&s.glyphs), textures.pages_with(&s.images)))
        .collect();
    let mut groups = vec![
        DrawGroup {
            filter: Filter::Linear,
            pages: &under,
        },
        DrawGroup {
            filter: Filter::Nearest,
            pages: &glyphs,
        },
        DrawGroup {
            filter: Filter::Linear,
            pages: &inline,
        },
        DrawGroup {
            filter: Filter::Linear,
            pages: &over,
        },
    ];
    for (glyphs, images) in &overlays {
        groups.push(DrawGroup {
            filter: Filter::Nearest,
            pages: glyphs,
        });
        groups.push(DrawGroup {
            filter: Filter::Linear,
            pages: images,
        });
    }
    let rgba = draw_offscreen(device, queue, &groups, frame.bg_color.0, content_w, height);
    Ok(Rendered {
        width: content_w,
        height,
        rgba,
    })
}

/// Clear to `bg` (linear target, so the bytes come back exactly), draw
/// `groups` in order with the sampler each one's filter names, and read the
/// texture back without row padding.
fn draw_offscreen(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    groups: &[DrawGroup<'_>],
    bg: [u8; 3],
    width: u32,
    height: u32,
) -> Vec<u8> {
    let format = wgpu::TextureFormat::Rgba8Unorm;
    let target = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("headless-target"),
        size: wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
        view_formats: &[],
    });
    let target_view = target.create_view(&wgpu::TextureViewDescriptor::default());

    let (pipeline, bind_group_layout) = create_quad_pipeline(device, format);
    let globals_buf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("globals"),
        contents: bytemuck::bytes_of(&Globals {
            screen: [width as f32, height as f32],
            _pad: [0.0; 2],
        }),
        usage: wgpu::BufferUsages::UNIFORM,
    });
    let sampler = nearest_sampler(device);
    let image_sampler = linear_sampler(device);
    let mut draw = FrameDraw::default();
    for group in groups {
        draw.push_pages(
            device,
            &bind_group_layout,
            &globals_buf,
            match group.filter {
                Filter::Nearest => &sampler,
                Filter::Linear => &image_sampler,
            },
            group.pages,
        );
    }

    // bytes_per_row must be a multiple of 256 for texture->buffer copies.
    // The row strides stay u32 (`bytes_per_row` wants one); the buffer size and
    // the readback offsets are computed wider so a large frame cannot wrap.
    let unpadded = width * 4;
    let padded = unpadded.div_ceil(256) * 256;
    let readback = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("readback"),
        size: padded as u64 * height as u64,
        usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
        mapped_at_creation: false,
    });

    let mut encoder =
        device.create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });
    {
        let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("headless-pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &target_view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color {
                        r: bg[0] as f64 / 255.0,
                        g: bg[1] as f64 / 255.0,
                        b: bg[2] as f64 / 255.0,
                        a: 1.0,
                    }),
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });
        draw.draw(&mut pass, &pipeline);
    }
    encoder.copy_texture_to_buffer(
        wgpu::ImageCopyTexture {
            texture: &target,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: wgpu::TextureAspect::All,
        },
        wgpu::ImageCopyBuffer {
            buffer: &readback,
            layout: wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(padded),
                rows_per_image: Some(height),
            },
        },
        wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
    );
    queue.submit(Some(encoder.finish()));

    let slice = readback.slice(..);
    let (tx, rx) = std::sync::mpsc::channel();
    slice.map_async(wgpu::MapMode::Read, move |r| {
        let _ = tx.send(r);
    });
    device.poll(wgpu::Maintain::Wait);
    // Not a `RenderError`: the buffer is `MAP_READ`, the copy was submitted and
    // the device polled to completion, so `map_async` can only fail on device
    // loss — which every other wgpu call here would already have panicked on.
    rx.recv()
        .expect("map_async callback")
        .expect("readback buffer map");

    let mapped = slice.get_mapped_range();
    let (unpadded, padded) = (unpadded as usize, padded as usize);
    let mut rgba = vec![0u8; unpadded * height as usize];
    for y in 0..height as usize {
        let (src, dst) = (y * padded, y * unpadded);
        rgba[dst..dst + unpadded].copy_from_slice(&mapped[src..src + unpadded]);
    }
    drop(mapped);
    readback.unmap();
    rgba
}

/// Append one PNG chunk: length, type, data, CRC-32 over type + data.
fn png_chunk(out: &mut Vec<u8>, kind: &[u8; 4], data: &[u8]) {
    out.extend_from_slice(&(data.len() as u32).to_be_bytes());
    let mut hasher = crc32fast::Hasher::new();
    hasher.update(kind);
    hasher.update(data);
    out.extend_from_slice(kind);
    out.extend_from_slice(data);
    out.extend_from_slice(&hasher.finalize().to_be_bytes());
}

/// Encode an RGBA8 buffer as a minimal PNG: signature, IHDR, one IDAT holding
/// the zlib stream of filter-0 scanlines, IEND. No `png` crate needed.
pub fn encode_png(width: u32, height: u32, rgba: &[u8]) -> Vec<u8> {
    // PNG's IHDR forbids a zero dimension, and a zero stride would panic deep
    // inside `chunks_exact`; say so here instead. `render_frame*` never produce
    // one (both dimensions are `max(1)`).
    assert!(width > 0 && height > 0, "encode_png: {width}x{height} has no pixels");
    let stride = width as usize * 4;
    assert_eq!(rgba.len(), stride * height as usize, "rgba size");
    let mut out = Vec::with_capacity(rgba.len() / 4 + 64);
    out.extend_from_slice(&[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]);
    let mut ihdr = Vec::with_capacity(13);
    ihdr.extend_from_slice(&width.to_be_bytes());
    ihdr.extend_from_slice(&height.to_be_bytes());
    ihdr.extend_from_slice(&[8, 6, 0, 0, 0]); // bit depth 8, RGBA, deflate, filter 0, no interlace
    png_chunk(&mut out, b"IHDR", &ihdr);
    let mut enc = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::default());
    for row in rgba.chunks_exact(stride) {
        enc.write_all(&[0]).expect("in-memory zlib write");
        enc.write_all(row).expect("in-memory zlib write");
    }
    let idat = enc.finish().expect("in-memory zlib finish");
    png_chunk(&mut out, b"IDAT", &idat);
    png_chunk(&mut out, b"IEND", &[]);
    out
}

/// Write a render as PNG — viewable anywhere, small enough to `scp` back.
pub fn write_png(path: &str, img: &Rendered) -> std::io::Result<()> {
    std::fs::write(path, encode_png(img.width, img.height, &img.rgba))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use std::sync::Arc;

    use erars_ui::image::ImageGeometry;

    use erars_ast::{Alignment, Value};
    use erars_compiler::Language;
    use erars_ui::width::WidthTable;
    use erars_ui::{ConsoleLine, ConsoleLinePart};

    use crate::font::{FontChain, StyleKey};
    use crate::layout::{layout, Layout, Row};
    use crate::test_support::{
        self as ts, bundled_font, frame, gpu_device, gpu_lock, style, text_line,
    };
    use crate::text::CellMetrics;

    /// A column counts as inked when some pixel in the band has a channel ≥ this.
    const INK: u8 = 32;
    const WHITE: [u8; 3] = [255, 255, 255];

    fn jp_shaper(files: &[PathBuf]) -> Shaper {
        ts::test_shaper(files, Language::Japanese, 18, 19)
    }

    fn geometry(shaper: &Shaper, content_w: u32) -> Geometry {
        Geometry::new(content_w, *shaper.metrics())
    }

    fn render(
        shaper: &mut Shaper,
        dev: &(wgpu::Device, wgpu::Queue),
        fr: &ConsoleFrame,
        w: u32,
        h: u32,
        input: Option<&str>,
        hover: Option<usize>,
    ) -> Rendered {
        render_frame_on(&dev.0, &dev.1, shaper, fr, w, h, input, hover, true)
            .expect("render within the adapter's texture limits")
    }

    /// Screen y of row `r` in a `height`-tall render with `scroll_rows = 0`,
    /// re-derived from spec Component 5 (`view_h − (bottom_row − r + 1)·line_h`)
    /// on purpose — independent of `draw::View::row_y`.
    fn row_y(rows: usize, r: usize, height: u32, line_h: u32) -> u32 {
        let view_h = height - line_h;
        let bottom = rows - 1;
        view_h - (bottom - r + 1) as u32 * line_h
    }

    /// Screen-space cell boxes of a laid-out row: (x_start, x_end_excl, text).
    fn boxes(row: &Row, m: &CellMetrics) -> Vec<(u32, u32, String)> {
        row.clusters
            .iter()
            .map(|c| {
                let x = (m.shift as i32 + row.x0 + c.x).max(0) as u32;
                (x, x + c.cells as u32 * m.half_w, c.text.to_string())
            })
            .collect()
    }

    fn in_glyph_box(bx: &[(u32, u32, String)], x: u32) -> bool {
        bx.iter()
            .any(|(a, b, t)| x >= *a && x < *b && !t.trim().is_empty())
    }

    /// The "perfect fallback" invariant in pixels: every inked column of row
    /// `r`'s band lies inside a non-blank cell box of row `r` — or of row
    /// `r−1`, because a tall font's glyphs overflow the row below (spec
    /// Component 4: no clamping to `line_h`). Every row must have ink of its own.
    fn assert_ink_in_boxes(img: &Rendered, lay: &Layout, m: &CellMetrics, height: u32) {
        let rows = lay.rows.len();
        for (r, row) in lay.rows.iter().enumerate() {
            let y0 = row_y(rows, r, height, m.line_h);
            let own = boxes(row, m);
            let above = if r > 0 { boxes(&lay.rows[r - 1], m) } else { Vec::new() };
            let ink = img.ink_columns(y0, y0 + m.line_h, INK);
            let mut own_ink = 0usize;
            for (x, &inked) in ink.iter().enumerate() {
                if !inked {
                    continue;
                }
                let x = x as u32;
                if in_glyph_box(&own, x) {
                    own_ink += 1;
                } else if !in_glyph_box(&above, x) {
                    panic!(
                        "row {r}: ink at x={x} outside every glyph box of rows {r} and {}: {own:?}",
                        r.saturating_sub(1)
                    );
                }
            }
            assert!(own_ink > 0, "row {r} has no ink inside its own boxes");
        }
    }

    fn checker(w: u32, h: u32) -> Vec<u8> {
        let mut rgba = vec![0u8; (w * h * 4) as usize];
        for y in 0..h {
            for x in 0..w {
                let i = ((y * w + x) * 4) as usize;
                rgba[i] = (x * 37) as u8;
                rgba[i + 1] = (y * 91) as u8;
                rgba[i + 2] = ((x + y) % 2 * 255) as u8;
                rgba[i + 3] = 255;
            }
        }
        rgba
    }

    /// Signature, IHDR fields, exactly IHDR/IDAT/IEND, and a valid CRC-32
    /// (over type + data) on every chunk.
    #[test]
    fn png_chunks_are_well_formed() {
        let (w, h) = (7u32, 3u32);
        let png = encode_png(w, h, &checker(w, h));
        assert_eq!(&png[..8], &[0x89, b'P', b'N', b'G', 0x0D, 0x0A, 0x1A, 0x0A]);
        let mut pos = 8;
        let mut kinds = Vec::new();
        while pos < png.len() {
            let len = u32::from_be_bytes(png[pos..pos + 4].try_into().unwrap()) as usize;
            let kind = &png[pos + 4..pos + 8];
            let data = &png[pos + 8..pos + 8 + len];
            let crc = u32::from_be_bytes(png[pos + 8 + len..pos + 12 + len].try_into().unwrap());
            let mut hasher = crc32fast::Hasher::new();
            hasher.update(kind);
            hasher.update(data);
            assert_eq!(hasher.finalize(), crc, "bad CRC in {}", String::from_utf8_lossy(kind));
            if kind == b"IHDR" {
                assert_eq!(len, 13);
                assert_eq!(&data[..8], &[0, 0, 0, 7, 0, 0, 0, 3]);
                // 8-bit RGBA, deflate, filter 0, no interlace
                assert_eq!(&data[8..], &[8, 6, 0, 0, 0]);
            }
            if kind == b"IEND" {
                assert_eq!(len, 0);
            }
            kinds.push(String::from_utf8_lossy(kind).into_owned());
            pos += 12 + len;
        }
        assert_eq!(pos, png.len());
        assert_eq!(kinds, ["IHDR", "IDAT", "IEND"]);
    }

    /// The single IDAT inflates to `height` filter-0 scanlines of the input rows.
    #[test]
    fn png_idat_inflates_to_filter0_scanlines() {
        use std::io::Read;
        let (w, h) = (5u32, 4u32);
        let rgba = checker(w, h);
        let png = encode_png(w, h, &rgba);
        // 8 signature + 25 (IHDR chunk) = 33: IDAT length at 33, type at 37, data at 41.
        let len = u32::from_be_bytes(png[33..37].try_into().unwrap()) as usize;
        assert_eq!(&png[37..41], b"IDAT");
        let mut raw = Vec::new();
        flate2::read::ZlibDecoder::new(&png[41..41 + len])
            .read_to_end(&mut raw)
            .unwrap();
        let stride = 1 + (w * 4) as usize;
        assert_eq!(raw.len(), stride * h as usize);
        for (y, line) in raw.chunks_exact(stride).enumerate() {
            assert_eq!(line[0], 0, "scanline {y} filter byte");
            let row = &rgba[y * (w * 4) as usize..(y + 1) * (w * 4) as usize];
            assert_eq!(&line[1..], row, "scanline {y}");
        }
    }

    /// `bg_color` is honoured: an empty frame is a solid image of it, byte-exact.
    #[test]
    fn bg_colour_fills_the_image() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let mut fr = frame(vec![]);
        fr.bg_color = erars_ui::Color([10, 20, 30]);
        let img = render(&mut shaper, &dev, &fr, 64, 40, None, None);
        assert_eq!((img.width, img.height), (64, 40));
        assert!(
            img.rgba.chunks_exact(4).all(|p| p == [10, 20, 30, 255]),
            "not a solid bg fill"
        );
    }

    /// One row (`hill`: no glyph reaches below the baseline, which is the
    /// strip's first pixel row) + input in a 3-row-tall image: slack row at the
    /// top (rows are bottom-anchored above the strip), the text row in the middle, the strip
    /// `> abc_` in the default colour on the bottom `line_h` rows; every strip
    /// pixel is grey (fore 192) and inside a cell box of the strip line;
    /// `input = None` leaves the strip empty.
    #[test]
    fn input_strip_is_drawn_at_the_bottom() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let lh = m.line_h;
        let fr = frame(vec![text_line("hill", WHITE)]);
        let (w, h) = (120, 3 * lh);
        let img = render(&mut shaper, &dev, &fr, w, h, Some("abc"), None);
        let any_ink =
            |img: &Rendered, y0: u32, y1: u32| img.ink_columns(y0, y1, INK).iter().any(|&b| b);
        assert!(!any_ink(&img, 0, lh), "slack row must be empty");
        assert!(any_ink(&img, lh, 2 * lh), "text row missing");
        assert!(any_ink(&img, 2 * lh, 3 * lh), "input strip missing");
        for y in 2 * lh..3 * lh {
            for x in 0..w {
                let [r, g, b, _] = img.pixel(x, y);
                assert!(r == g && g == b && r <= 192, "strip pixel ({x},{y}) = {:?}", (r, g, b));
            }
        }
        // Strip ink lies inside the boxes of "> abc_" and touches > a b c (the
        // bundled font's `_` sits below the baseline = the strip's last row → clipped).
        let strip_line = text_line("> abc_", [192, 192, 192]);
        let strip_lay = layout(
            std::slice::from_ref(&strip_line),
            &geometry(&shaper, w),
            &mut shaper,
        );
        let bx = boxes(&strip_lay.rows[0], &m);
        let ink = img.ink_columns(2 * lh, 3 * lh, INK);
        let mut touched: Vec<&str> = Vec::new();
        for (x, &inked) in ink.iter().enumerate() {
            if !inked {
                continue;
            }
            let (_, _, t) = bx
                .iter()
                .find(|(a, b, t)| (x as u32) >= *a && (x as u32) < *b && !t.trim().is_empty())
                .unwrap_or_else(|| {
                    panic!("strip ink at x={x} outside the strip's glyph boxes {bx:?}")
                });
            if touched.last() != Some(&t.as_str()) {
                touched.push(t.as_str());
            }
        }
        assert!(touched.starts_with(&[">", "a", "b", "c"]), "strip glyphs with ink: {touched:?}");
        assert!(touched.len() <= 5, "strip glyphs with ink: {touched:?}");
        let none = render(&mut shaper, &dev, &fr, w, h, None, None);
        assert!(!any_ink(&none, 2 * lh, 3 * lh), "strip drawn without input");
    }

    /// Spec Component 5 "View state": a frame with more rows than fit is
    /// clipped at the *top* and the newest row is anchored just above the input
    /// strip. Row `i` is `i` spaces then `|`, so the inked cell names the row:
    /// 40 rows in a 480 px frame leave 24 visible (rows 16..39), and nothing
    /// from rows 0..15 may be drawn anywhere in the row area.
    #[test]
    fn rows_beyond_the_view_are_clipped_at_the_top() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let (lh, hw, shift) = (m.line_h, m.half_w, m.shift);
        assert_eq!((lh, hw, shift), (19, 11, 3));
        let rows = 40usize;
        let (w, h) = (480, 480);
        let fr = frame(
            (0..rows)
                .map(|i| text_line(&format!("{}|", " ".repeat(i)), WHITE))
                .collect(),
        );
        let img = render(&mut shaper, &dev, &fr, w, h, Some(""), None);

        // 480 − 19 px strip = 461 px of row area = 24 whole rows.
        let view_h = h - lh;
        let visible = (view_h / lh) as usize;
        assert_eq!((view_h, visible), (461, 24));
        let first = rows - visible; // 16: the oldest row still on screen
        let cell_x = |cell: usize| shift + cell as u32 * hw;

        // Nothing of rows 0..15 is drawn: their cells are blank over the whole
        // row area. (The strip below it draws "> _" in cells 0..2.)
        let log_ink = img.ink_columns(0, view_h, INK);
        for (x, &inked) in log_ink.iter().enumerate() {
            assert!(
                !inked || x as u32 >= cell_x(first),
                "ink at x={x} belongs to a clipped row (< cell {first} at x={})",
                cell_x(first)
            );
        }
        // Every visible row is where `View::row_y` puts it, with its own cell
        // inked; only the row above it may spill into its band.
        for r in first..rows {
            let y0 = row_y(rows, r, h, lh);
            let ink = img.ink_columns(y0, y0 + lh, INK);
            let own = (cell_x(r), cell_x(r + 1));
            assert!(
                (own.0..own.1).any(|x| ink[x as usize]),
                "row {r} (band {y0}..{}) has no ink in its own cell",
                y0 + lh
            );
            for (x, &inked) in ink.iter().enumerate() {
                let x = x as u32;
                assert!(
                    !inked || (x >= cell_x(r - 1) && x < own.1),
                    "row {r}: ink at x={x} outside cells {} and {r}",
                    r - 1
                );
            }
        }
        // The oldest visible row sits in the 5 px-slack band at the top, the
        // newest directly above the strip.
        assert_eq!(row_y(rows, first, h, lh), 5);
        assert_eq!(row_y(rows, rows - 1, h, lh), view_h - lh);
    }

    /// Spec Testing §5: a box-drawing frame over an ASCII ruler. With the
    /// bundled font as primary (0.6 em → half_w 11) every JIS box character is
    /// a 2-cell, 22 px box holding a centred 10.8 px glyph; all ink must land
    /// inside `[shift + k·half_w, shift + (k+cells)·half_w)` of the cluster
    /// that owns it (or spill straight down from the row above), and the
    /// spaced row proves nothing leaks sideways into a blank cell.
    #[test]
    fn box_frame_ink_lands_in_cells() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        assert_eq!((m.font_px, m.half_w, m.line_h, m.shift), (18, 11, 19, 3));
        let fr = frame(vec![
            text_line("01234567", WHITE),
            text_line("┏━━┓", WHITE),
            text_line("┃    ┃", WHITE),
            text_line("┗━━┛", WHITE),
            text_line("┏ ━ ┓", WHITE),
        ]);
        let (w, h) = (200, 6 * m.line_h);
        let img = render(&mut shaper, &dev, &fr, w, h, None, None);
        let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
        assert_eq!(lay.rows.len(), 5);
        // JP widths: box characters are 2 cells, so every row is 8 cells wide.
        for row in &lay.rows {
            let cells: u32 = row.clusters.iter().map(|c| c.cells as u32).sum();
            assert_eq!(cells, 8, "row {} cells", row.line);
        }
        assert_ink_in_boxes(&img, &lay, &m, h);
    }

    /// Identical lines render to byte-identical row bands: glyph origins are
    /// integer pixels and the sampler is Nearest, so nothing drifts per row.
    /// Rows 1–3 are compared (each has an identical row above it spilling the
    /// same descenders into its band; row 0 has none).
    #[test]
    fn identical_rows_are_byte_identical() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let lh = shaper.metrics().line_h;
        let fr = frame(vec![text_line("Abc123Xyz┏━┓", WHITE); 4]);
        let h = 5 * lh;
        let img = render(&mut shaper, &dev, &fr, 300, h, None, None);
        let b1 = img.band(lh, 2 * lh);
        assert!(b1.iter().any(|&v| v != 0), "no ink rendered");
        assert_eq!(b1, img.band(2 * lh, 3 * lh), "row 2 differs from row 1");
        assert_eq!(b1, img.band(3 * lh, 4 * lh), "row 3 differs from row 1");
    }

    /// A glyph whose natural advance exceeds its box (`a > w`: the bundled
    /// 10.8 px glyphs in a pinned 9 px cell) is rescaled, and one that fits
    /// (`a ≤ w`: 2-cell `α`/`°`/`→` in an 18 px box) is centred — neither may
    /// put ink outside its box.
    #[test]
    fn rescaled_and_centred_glyphs_stay_in_their_boxes() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        shaper.set_metrics(CellMetrics {
            scale: 1.0,
            font_px: 18,
            half_w: 9,
            line_h: 19,
            baseline: 15,
            shift: 3,
        });
        let m = *shaper.metrics();
        let fr = frame(vec![text_line("MMMM W W", WHITE), text_line("αα°→", WHITE)]);
        let (w, h) = (200, 3 * m.line_h);
        let img = render(&mut shaper, &dev, &fr, w, h, None, None);
        let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
        assert_eq!(
            lay.rows[0].clusters.iter().map(|c| c.cells).collect::<Vec<_>>(),
            [1; 8]
        );
        assert_eq!(
            lay.rows[1].clusters.iter().map(|c| c.cells).collect::<Vec<_>>(),
            [2, 2, 2, 2]
        );
        assert_ink_in_boxes(&img, &lay, &m, h);
    }

    fn button(text: &str, gen: u32, v: i64) -> ConsoleLinePart {
        ConsoleLinePart::Button(vec![(text.to_string(), style(WHITE))], gen, Value::Int(v))
    }

    /// Hover is colour-only at draw time: `hover = Some(i)` changes pixels only
    /// inside `buttons[i]`'s columns, every changed pixel takes the focus colour
    /// (white → yellow keeps r/g, zeroes b), and `hover = None` is byte-stable.
    #[test]
    fn hover_recolours_only_the_hovered_button() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let line = ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![
                ConsoleLinePart::Text("pick: ".into(), style(WHITE)),
                button("[1] go", 1, 1),
                ConsoleLinePart::Text(" ".into(), style(WHITE)),
                button("[2] stop", 1, 2),
            ],
        };
        let fr = frame(vec![line]);
        let (w, h) = (300, 2 * m.line_h);
        let base = render(&mut shaper, &dev, &fr, w, h, None, None);
        let again = render(&mut shaper, &dev, &fr, w, h, None, None);
        assert_eq!(base.rgba, again.rgba, "unhovered render is not byte-stable");
        let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
        assert_eq!(lay.buttons.len(), 2);
        let mut hovered: Vec<Rendered> = Vec::new();
        for i in 0..2 {
            let img = render(&mut shaper, &dev, &fr, w, h, None, Some(i));
            let b = &lay.buttons[i];
            let bx0 = (m.shift as i32 + lay.rows[b.row].x0 + b.x) as u32;
            let bx1 = bx0 + b.w;
            let mut changed = 0usize;
            for y in 0..h {
                for x in 0..w {
                    let (p, q) = (base.pixel(x, y), img.pixel(x, y));
                    if p == q {
                        continue;
                    }
                    changed += 1;
                    assert!(
                        x >= bx0 && x < bx1,
                        "hover {i}: pixel ({x},{y}) changed outside its box [{bx0},{bx1})"
                    );
                    assert_eq!(
                        (q[0], q[1], q[2]),
                        (p[0], p[1], 0),
                        "hover {i}: pixel ({x},{y}) is not the focus colour"
                    );
                }
            }
            assert!(changed > 0, "hover {i} changed nothing");
            hovered.push(img);
        }
        assert_ne!(
            hovered[0].rgba, hovered[1].rgba,
            "hovering different buttons must differ"
        );
    }

    /// Same frame test with a real CJK font as the *fallback* (the bundled
    /// font stays primary, half_w 11): 18 px CJK glyphs are centred in their
    /// 22 px boxes and must not leak; the CJK row really resolves to the
    /// fallback face, not to the primary's `.notdef`.
    #[test]
    fn box_frame_ink_lands_in_cells_cjk() {
        let _gpu = gpu_lock();
        let Some(cjk) = ts::require_cjk_font() else { return };
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font(), cjk]);
        let m = *shaper.metrics();
        assert_eq!(
            (m.font_px, m.half_w, m.line_h, m.shift),
            (18, 11, 19, 3),
            "bundled font must stay primary"
        );
        let primary = shaper.chain().primary();
        for c in ['漢', 'あ', '한'] {
            let (id, _) = shaper.chain().resolve(c, &StyleKey::plain());
            assert_ne!(id, primary, "{c} must come from the CJK fallback");
        }
        let fr = frame(vec![
            text_line("01234567", WHITE),
            text_line("漢字한글", WHITE),
            text_line("┏━━┓", WHITE),
            text_line("あ い", WHITE),
            text_line("Aあ漢B", WHITE),
        ]);
        let (w, h) = (200, 6 * m.line_h);
        let img = render(&mut shaper, &dev, &fr, w, h, None, None);
        let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
        assert_eq!(
            lay.rows[1].clusters.iter().map(|c| c.cells).collect::<Vec<_>>(),
            [2, 2, 2, 2]
        );
        assert_eq!(
            lay.rows[4].clusters.iter().map(|c| c.cells).collect::<Vec<_>>(),
            [1, 2, 2, 1]
        );
        assert_ink_in_boxes(&img, &lay, &m, h);
    }

    #[test]
    fn identical_rows_are_byte_identical_cjk() {
        let _gpu = gpu_lock();
        let Some(cjk) = ts::require_cjk_font() else { return };
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font(), cjk]);
        let lh = shaper.metrics().line_h;
        let fr = frame(vec![text_line("漢字한글┏━┓Ab", WHITE); 4]);
        let h = 5 * lh;
        let img = render(&mut shaper, &dev, &fr, 300, h, None, None);
        let b1 = img.band(lh, 2 * lh);
        assert!(b1.iter().any(|&v| v != 0), "no ink rendered");
        assert_eq!(b1, img.band(2 * lh, 3 * lh));
        assert_eq!(b1, img.band(3 * lh, 4 * lh));
    }

    /// Size and popcount of the exact `ppem` strike of `c` in `font`
    /// (ttf-parser route, packed 1-bit).
    fn strike_stats(font: &cosmic_text::Font, c: char, ppem: u16) -> (u32, u32, u32) {
        use cosmic_text::ttf_parser::RasterImageFormat;
        let face = font.rustybuzz();
        let gid = face.glyph_index(c).expect("cmap");
        let img = face.glyph_raster_image(gid, ppem).expect("strike");
        assert_eq!(img.pixels_per_em, ppem, "{c}: strike ppem");
        assert_eq!(img.format, RasterImageFormat::BitmapMonoPacked, "{c}: strike format");
        let n = img.width as usize * img.height as usize;
        let pop = (0..n)
            .filter(|&i| (img.data[i >> 3] >> (7 - (i & 7))) & 1 == 1)
            .count() as u32;
        (img.width as u32, img.height as u32, pop)
    }

    /// MS Gothic at 18 px draws its embedded 1-bit strikes: every pixel in a
    /// glyph's cell box is 0 or 255 in white text, the white count equals the
    /// strike's set bits, `あ`/`漢`/`─` fill an 18×18 box and `A`/`═` a 9×18 one,
    /// and the 19th (slack) row stays empty.
    #[test]
    fn msgothic_18px_uses_bitmap_strikes() {
        let _gpu = gpu_lock();
        let Some(ms) = ts::msgothic_font() else { return };
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[ms]);
        let m = *shaper.metrics();
        assert_eq!((m.font_px, m.half_w, m.line_h, m.baseline, m.shift), (18, 9, 19, 15, 3));
        let fr = frame(vec![text_line("Aあ漢─═", WHITE)]);
        let (w, h) = (120, 2 * m.line_h);
        let img = render(&mut shaper, &dev, &fr, w, h, None, None);
        let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
        let primary = {
            let id = shaper.chain().primary();
            shaper.chain().font(id)
        };
        let row = &lay.rows[0];
        let expected_cells = [1u8, 2, 2, 2, 1];
        assert_eq!(row.clusters.len(), 5);
        for (c, cells) in row.clusters.iter().zip(expected_cells) {
            assert_eq!(c.cells, cells, "{:?} cells", c.text);
            let ch = c.text.chars().next().unwrap();
            let x0 = (m.shift as i32 + row.x0 + c.x) as u32;
            let x1 = x0 + c.cells as u32 * m.half_w;
            let (sw, sh, pop) = strike_stats(&primary, ch, 18);
            assert_eq!((sw, sh), (c.cells as u32 * 9, 18), "{ch}: strike size");
            assert!(pop > 0, "{ch}: empty strike");
            let mut white = 0u32;
            for y in 0..m.font_px {
                for x in x0..x1 {
                    let [r, g, b, _] = img.pixel(x, y);
                    assert!(
                        matches!((r, g, b), (0, 0, 0) | (255, 255, 255)),
                        "{ch}: pixel ({x},{y}) = {:?} is not 0/255",
                        (r, g, b)
                    );
                    if r == 255 {
                        white += 1;
                    }
                }
            }
            assert_eq!(white, pop, "{ch}: white pixels != strike popcount");
            assert!(
                img.ink_bbox(x0, x1, m.font_px, m.line_h, 1).is_none(),
                "{ch}: ink in the line slack row"
            );
        }
        assert_ink_in_boxes(&img, &lay, &m, h);
    }

    /// 23 px has no exact strike (ttf-parser would hand back the 22 ppem one):
    /// the outline path is used, so anti-aliased intermediate values appear.
    #[test]
    fn msgothic_23px_uses_outlines() {
        let _gpu = gpu_lock();
        let Some(ms) = ts::msgothic_font() else { return };
        let Some(dev) = gpu_device() else { return };
        let mut shaper = ts::test_shaper(&[ms], Language::Japanese, 23, 24);
        let m = *shaper.metrics();
        assert_eq!((m.font_px, m.half_w, m.line_h, m.baseline, m.shift), (23, 12, 24, 20, 3));
        let fr = frame(vec![text_line("Aあ", WHITE)]);
        let (w, h) = (120, 2 * m.line_h);
        let img = render(&mut shaper, &dev, &fr, w, h, None, None);
        let lay = layout(&fr.lines, &geometry(&shaper, w), &mut shaper);
        let row = &lay.rows[0];
        let mut grey = 0usize;
        for c in &row.clusters {
            let x0 = (m.shift as i32 + row.x0 + c.x) as u32;
            let x1 = x0 + c.cells as u32 * m.half_w;
            assert!(
                img.ink_bbox(x0, x1, 0, m.line_h, 1).is_some(),
                "{:?}: nothing drawn",
                c.text
            );
            for y in 0..m.line_h {
                for x in x0..x1 {
                    let [r, _, _, _] = img.pixel(x, y);
                    if r > 0 && r < 255 {
                        grey += 1;
                    }
                }
            }
        }
        assert!(grey > 0, "no anti-aliased pixels — a strike was used at 23 px");
        assert_ink_in_boxes(&img, &lay, &m, h);
    }

    /// GPU-free companion: the 18 ppem strike of `あ` is exact, 18×18, packed;
    /// 23 ppem yields the *nearest* (22) strike, which the raster layer
    /// (`raster::strike_image`) rejects while accepting the exact one and
    /// decoding it to a 0/255 mask placed at top = baseline (15).
    #[test]
    fn msgothic_strike_metadata_gpu_free() {
        use cosmic_text::ttf_parser::RasterImageFormat;
        let Some(ms) = ts::msgothic_font() else { return };
        let mut chain = FontChain::from_files(&[ms], Language::Japanese);
        let font = chain.font(chain.primary());
        let face = font.rustybuzz();
        let gid = face.glyph_index('あ').expect("あ in cmap");
        assert_ne!(gid.0, 0);
        let img = face.glyph_raster_image(gid, 18).expect("18 ppem strike");
        assert_eq!(img.pixels_per_em, 18);
        assert_eq!((img.width, img.height, img.x, img.y), (18, 18, 0, -3));
        assert_eq!(img.format, RasterImageFormat::BitmapMonoPacked);
        let near = face.glyph_raster_image(gid, 23).expect("nearest strike");
        assert_eq!(near.pixels_per_em, 22, "ttf-parser picks the nearest strike");
        assert!(
            crate::raster::strike_image(&font, gid.0, 23).is_none(),
            "22 ppem strike must be rejected for 23 px"
        );
        let mask = crate::raster::strike_image(&font, gid.0, 18).expect("exact strike accepted");
        assert_eq!(
            (mask.width, mask.height, mask.left, mask.top, mask.color),
            (18, 18, 0, 15, false)
        );
        let alphas: Vec<u8> = mask.rgba.chunks_exact(4).map(|p| p[3]).collect();
        assert!(alphas.iter().all(|&a| a == 0 || a == 255), "mask must be 0/255");
        assert_eq!(
            alphas.iter().filter(|&&a| a == 255).count(),
            61,
            "あ @18 has 61 set bits"
        );
    }

    /// The classifier and MS Gothic agree on the JIS box set: the 32 JIS X 0208
    /// box characters are 2 cells / a full em, `═`/`║` 1 cell / half an em.
    #[test]
    fn msgothic_jis_box_drawing_is_full_width_and_double_lines_half() {
        let Some(ms) = ts::msgothic_font() else { return };
        let widths = WidthTable::new(Language::Japanese.encoding());
        let mut chain = FontChain::from_files(&[ms], Language::Japanese);
        let font = chain.font(chain.primary());
        let face = font.rustybuzz();
        let upem = face.units_per_em() as u32;
        assert_eq!(upem, 256);
        let adv =
            |c: char| face.glyph_hor_advance(face.glyph_index(c).expect("cmap")).unwrap() as u32;
        let jis = "─│┌┐┘└├┬┤┴┼━┃┏┓┛┗┣┳┫┻╋┠┯┨┷┿┝┰┥┸╂";
        assert_eq!(jis.chars().count(), 32);
        for c in jis.chars() {
            assert_eq!(widths.char_cells(c), 2, "{c} cells");
            assert_eq!(adv(c), upem, "{c} advance");
        }
        for c in ['═', '║'] {
            assert_eq!(widths.char_cells(c), 1, "{c} cells");
            assert_eq!(adv(c) * 2, upem, "{c} advance");
        }
    }

    /// A frame larger than the adapter's texture ceiling is an error, not a
    /// panic inside wgpu validation — and a 3000 px wide frame (over the
    /// downlevel 2048 default) renders now that `request_device` takes the
    /// texture dimensions from the adapter.
    #[test]
    fn oversized_frame_is_rejected_and_wide_frames_render() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let fr = frame(vec![text_line("wide", WHITE)]);
        let max = dev.0.limits().max_texture_dimension_2d;

        let too_wide =
            render_frame_on(&dev.0, &dev.1, &mut shaper, &fr, max + 1, 64, None, None, true);
        let Err(err) = too_wide else {
            panic!("a frame past the adapter's ceiling must be rejected, not rendered")
        };
        assert_eq!(
            err,
            RenderError::TooLarge {
                width: max + 1,
                height: 64,
                max
            }
        );
        assert!(
            err.to_string().contains(&max.to_string()),
            "error message must name the limit: {err}"
        );

        if max >= 3000 {
            let lh = shaper.metrics().line_h;
            let img = render(&mut shaper, &dev, &fr, 3000, 480, None, None);
            assert_eq!((img.width, img.height), (3000, 480));
            let ink = img.ink_columns(480 - 2 * lh, 480 - lh, INK);
            assert!(ink.iter().any(|&b| b), "nothing drawn in a 3000 px wide frame");
        } else {
            eprintln!(
                "SKIP {}: adapter max texture size is {max} (< 3000)",
                ts::test_name()
            );
        }
    }

    // ---- the inline image layer -------------------------------------------

    /// A deliberately asymmetric source: 8×4 (not square, so a transpose
    /// cannot pass) with four distinct quadrant colours (so a mirror or an
    /// axis swap cannot pass either).
    const IMG_TL: [u8; 3] = [255, 0, 0];
    const IMG_TR: [u8; 3] = [0, 255, 0];
    const IMG_BL: [u8; 3] = [0, 0, 255];
    const IMG_BR: [u8; 3] = [255, 255, 0];

    fn quad_bitmap() -> Arc<erars_ui::image::ImageBitmap> {
        let (w, h) = (8u32, 4u32);
        let argb = |c: [u8; 3]| {
            0xFF00_0000 | ((c[0] as u32) << 16) | ((c[1] as u32) << 8) | c[2] as u32
        };
        let mut px = Vec::with_capacity((w * h) as usize);
        for y in 0..h {
            for x in 0..w {
                px.push(argb(match (x < w / 2, y < h / 2) {
                    (true, true) => IMG_TL,
                    (false, true) => IMG_TR,
                    (true, false) => IMG_BL,
                    (false, false) => IMG_BR,
                }));
            }
        }
        Arc::new(erars_ui::image::ImageBitmap::new(
            w,
            h,
            px.into_boxed_slice(),
            1,
        ))
    }

    /// One console line holding a single inline image of the whole 8×4 bitmap,
    /// sized by `height` px like `<img src='…' height='16px'>`, plus the store
    /// the renderer samples.
    fn image_frame(height_px: i32) -> (ConsoleFrame, ImageGeometry) {
        image_frame_wh(None, height_px)
    }

    /// The same, with an explicit `width=` — negative to mirror.
    fn image_frame_wh(width_px: Option<i32>, height_px: i32) -> (ConsoleFrame, ImageGeometry) {
        use erars_ui::image::{
            ImageGeometry, ImageSampler, InlineImage, InlineSprite, MixedNum, Rect as SrcRect,
        };

        let bitmap = quad_bitmap();
        let (w, h) = (bitmap.width, bitmap.height);
        let sprite = InlineSprite {
            sampler: ImageSampler::Single {
                bitmap: 7,
                src: SrcRect {
                    x: 0,
                    y: 0,
                    width: w as i32,
                    height: h as i32,
                },
            },
            width: w,
            height: h,
            pos_x: 0,
            pos_y: 0,
        };
        let geometry = ImageGeometry::new(
            18,
            w,
            h,
            width_px.map(|num| MixedNum { num, is_px: true }),
            Some(MixedNum {
                num: height_px,
                is_px: true,
            }),
            None,
        );
        let image = InlineImage {
            name: "QUAD".into(),
            button: None,
            mask: None,
            sprite,
            geometry,
            alt: "<img src='QUAD'>".into(),
        };
        let line = ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Image(Arc::new(image))],
        };
        let fr = frame(vec![line]);
        fr.images.publish(7, bitmap);
        (fr, geometry)
    }

    /// The image really reaches the framebuffer, **scaled**: an 8×4 source
    /// drawn into a 32×16 destination box (4× on both axes, which the geometry
    /// derives from `height=16px` and the 2:1 aspect ratio, so this is the
    /// `ConsoleImagePart` arithmetic and not a 1:1 blit).
    ///
    /// Asserted at known coordinates: the destination box starts at
    /// `m.shift` (3) on the row's top pixel, each quadrant centre carries its
    /// own source colour, and the pixels just outside the box are still
    /// background. Because the quadrants differ on both axes, a mirror, a
    /// transpose or a UV swap changes at least one of the four.
    #[test]
    fn a_scaled_inline_image_lands_on_the_right_pixels() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let (fr, geo) = image_frame(16);
        assert_eq!(
            (geo.dest_width, geo.dest_height, geo.width),
            (32, 16, 32),
            "height=16px on an 8×4 sprite must give a 32×16 destination box"
        );

        // One row, bottom-anchored above a one-line input strip: row 0 sits at
        // y = 0 in a 2·line_h tall frame.
        let (w, h) = (64, 2 * m.line_h);
        let img = render(&mut shaper, &dev, &fr, w, h, None, None);
        assert_eq!(row_y(1, 0, h, m.line_h), 0);

        let x0 = m.shift; // 3: base_x + PlacedImage::x (0) + geo.dest_x (0)
        let y0 = 0;
        let (dw, dh) = (geo.dest_width as u32, geo.dest_height as u32);
        let at = |x: u32, y: u32| {
            let [r, g, b, a] = img.pixel(x, y);
            assert_eq!(a, 255, "image pixel ({x},{y}) must be opaque");
            [r, g, b]
        };

        // Quadrant centres: dest pixel (px+0.5) maps to source (px+0.5)/4, so
        // every centre lands strictly inside one source quadrant.
        for (qx, qy, want, name) in [
            (0, 0, IMG_TL, "top-left"),
            (1, 0, IMG_TR, "top-right"),
            (0, 1, IMG_BL, "bottom-left"),
            (1, 1, IMG_BR, "bottom-right"),
        ] {
            let x = x0 + qx * dw / 2 + dw / 4;
            let y = y0 + qy * dh / 2 + dh / 4;
            assert_eq!(at(x, y), want, "{name} quadrant centre at ({x},{y})");
        }

        // The box's own corners, one pixel inside: still the corner colours.
        assert_eq!(at(x0, y0), IMG_TL, "box top-left corner");
        assert_eq!(at(x0 + dw - 1, y0), IMG_TR, "box top-right corner");
        assert_eq!(at(x0, y0 + dh - 1), IMG_BL, "box bottom-left corner");
        assert_eq!(at(x0 + dw - 1, y0 + dh - 1), IMG_BR, "box bottom-right");

        // …and nothing outside it. `bg_color` is black in `frame()`.
        for (x, y, why) in [
            (x0 - 1, y0, "left of the box"),
            (x0 + dw, y0, "right of the box"),
            (x0, y0 + dh, "below the box"),
        ] {
            assert_eq!(
                img.pixel(x, y),
                [0, 0, 0, 255],
                "{why}: ({x},{y}) must still be background"
            );
        }

        // A 1:1 blit would leave this pixel — inside the 32×16 box but well
        // outside an 8×4 one — at the background colour. x=24 samples source
        // 6.125 and y=10 samples 2.625, both between two same-coloured texel
        // centres, so the expected colour is exact.
        assert_eq!(at(x0 + 24, y0 + 10), IMG_BR, "scaling really happened");
    }

    /// The image sampler is `Linear`, as GDI+'s default `InterpolationMode`
    /// is: across the source's vertical colour boundary (source x = 4, i.e.
    /// dest x = 16 relative to the box), a destination pixel whose source
    /// coordinate stays between two same-coloured texel centres is pure, while
    /// the one that straddles the boundary blends the two.
    ///
    /// Dest px `k` samples source `(k + 0.5) / 4`: k=13 → 3.375, between the
    /// texel centres 2.5 and 3.5 (both red) → pure red; k=15 → 3.875, between
    /// 3.5 (red) and 4.5 (green) at weight 0.375 → ≈(159, 96, 0). `Nearest`
    /// would make both pure.
    #[test]
    fn inline_images_are_sampled_bilinearly() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let (fr, _) = image_frame(16);
        let img = render(&mut shaper, &dev, &fr, 64, 2 * m.line_h, None, None);
        let x0 = m.shift;
        let y = 4; // inside the top half, so the boundary is x only

        let [r, g, b, _] = img.pixel(x0 + 13, y);
        assert_eq!([r, g, b], IMG_TL, "x=13 samples two red texels");

        let [r, g, b, _] = img.pixel(x0 + 15, y);
        assert_eq!(b, 0, "the blend is between red and green only");
        assert!(
            (0 < r && r < 255) && (0 < g && g < 255),
            "x=15 must blend red and green (got {:?}); Nearest would be pure",
            (r, g, b)
        );
        let near = |v: u8, want: i32| (v as i32 - want).abs() <= 3;
        assert!(
            near(r, 159) && near(g, 96),
            "x=15 blend weight 0.375 → ≈(159, 96, 0), got {:?}",
            (r, g, b)
        );
    }

    /// A negative `width=`/`height=` **mirrors in place**: Emuera leaves the
    /// destination extents signed and only flips the layout scalars
    /// (`ConsoleImagePart.cs:105-116`), so GDI+ draws the same box with the
    /// source reversed on that axis.
    ///
    /// Asserted against the upright render of the same image: the box lands on
    /// exactly the same pixels, and every quadrant holds its opposite's
    /// colour. Only a true mirror does both — a shifted box would move the
    /// corners, and a UV that mirrored without the `dest_x`/`dest_y` fixup
    /// would draw somewhere else entirely.
    #[test]
    fn a_negative_extent_mirrors_the_inline_image_in_place() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let (w, h) = (64, 2 * m.line_h);

        let (upright, geo) = image_frame(16);
        let (dw, dh) = (geo.dest_width as u32, geo.dest_height as u32);
        let upright = render(&mut shaper, &dev, &upright, w, h, None, None);

        // `width=-32px`: dest_width stays -32 and dest_x becomes +32, which is
        // the same box.
        let (flipped_x, geo_x) = image_frame_wh(Some(-32), 16);
        assert_eq!((geo_x.dest_width, geo_x.dest_x, geo_x.width), (-32, 32, 32));
        let flipped_x = render(&mut shaper, &dev, &flipped_x, w, h, None, None);

        // `height=-16px` with an explicit positive width, so only the vertical
        // axis flips: an *implied* width would inherit the negative height's
        // sign through the aspect ratio (`:87-102` runs before the fixup) and
        // mirror both ways.
        let (flipped_y, geo_y) = image_frame_wh(Some(32), -16);
        assert_eq!((geo_y.dest_height, geo_y.dest_y, geo_y.top), (-16, 16, 0));
        let flipped_y = render(&mut shaper, &dev, &flipped_y, w, h, None, None);

        let x0 = m.shift;
        for (qx, qy) in [(0, 0), (1, 0), (0, 1), (1, 1)] {
            let x = x0 + qx * dw / 2 + dw / 4;
            let y = qy * dh / 2 + dh / 4;
            let mirror_x = x0 + (1 - qx) * dw / 2 + dw / 4;
            let mirror_y = (1 - qy) * dh / 2 + dh / 4;
            assert_eq!(
                flipped_x.pixel(x, y),
                upright.pixel(mirror_x, y),
                "negative width: ({x},{y}) must hold the colour from x={mirror_x}"
            );
            assert_eq!(
                flipped_y.pixel(x, y),
                upright.pixel(x, mirror_y),
                "negative height: ({x},{y}) must hold the colour from y={mirror_y}"
            );
        }

        // The box itself did not move: the pixels just outside it are still
        // background in both mirrored renders.
        for (x, y, why) in [
            (x0 - 1, 0, "left of the box"),
            (x0 + dw, 0, "right of the box"),
            (x0, dh, "below the box"),
        ] {
            assert_eq!(flipped_x.pixel(x, y), [0, 0, 0, 255], "negative width, {why}");
            assert_eq!(flipped_y.pixel(x, y), [0, 0, 0, 255], "negative height, {why}");
        }
    }

    /// End-to-end through the artefact the operator sees: render a scaled
    /// image, encode it with [`write_png`], read the file back, inflate it and
    /// assert the same known coordinates on the decoded pixels. A PNG that
    /// decodes to the wrong colours is a failed proof even if the framebuffer
    /// was right.
    #[test]
    fn the_written_png_decodes_to_the_scaled_image() {
        use std::io::Read;

        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let (fr, geo) = image_frame(16);
        let (w, h) = (64, 2 * m.line_h);
        let rendered = render(&mut shaper, &dev, &fr, w, h, None, None);

        let path = std::env::temp_dir().join(format!("erars-{}.png", ts::test_name()));
        let path = path.to_str().expect("utf-8 temp path");
        write_png(path, &rendered).expect("write the shot");
        let png = std::fs::read(path).expect("read the shot back");
        std::fs::remove_file(path).ok();

        // 8 signature + 25 IHDR: the encoder writes exactly IHDR/IDAT/IEND
        // (`png_chunks_are_well_formed`), so IDAT's length is at 33.
        assert_eq!(&png[37..41], b"IDAT");
        let len = u32::from_be_bytes(png[33..37].try_into().unwrap()) as usize;
        let mut raw = Vec::new();
        flate2::read::ZlibDecoder::new(&png[41..41 + len])
            .read_to_end(&mut raw)
            .expect("inflate IDAT");
        let stride = 1 + (w * 4) as usize;
        assert_eq!(raw.len(), stride * h as usize, "one scanline per row");

        let px = |x: u32, y: u32| -> [u8; 3] {
            let line = &raw[y as usize * stride..(y as usize + 1) * stride];
            assert_eq!(line[0], 0, "scanline {y} must be filter 0");
            let i = 1 + x as usize * 4;
            assert_eq!(line[i + 3], 255, "decoded ({x},{y}) must be opaque");
            [line[i], line[i + 1], line[i + 2]]
        };
        let x0 = m.shift;
        let (dw, dh) = (geo.dest_width as u32, geo.dest_height as u32);
        assert_eq!(px(x0 + dw / 4, dh / 4), IMG_TL, "decoded top-left");
        assert_eq!(px(x0 + 3 * dw / 4, dh / 4), IMG_TR, "decoded top-right");
        assert_eq!(px(x0 + dw / 4, 3 * dh / 4), IMG_BL, "decoded bottom-left");
        assert_eq!(px(x0 + 3 * dw / 4, 3 * dh / 4), IMG_BR, "decoded bottom-right");
        assert_eq!(px(x0 + dw, 0), [0, 0, 0], "decoded background right of the box");
    }

    // ---- the console-background plane -------------------------------------

    /// Magenta, and nothing else in these tests is magenta.
    const CBG: [u8; 3] = [255, 0, 255];
    /// The plane's box: `x = 8` so all four edges have a testable outside.
    const CBG_X: u32 = 8;
    const CBG_W: u32 = 40;

    /// A uniform source, so every pixel inside a drawn box is exactly this
    /// colour whatever the sampler's filter does — the scaling arithmetic is
    /// already proved by the inline-image tests, and this test is about
    /// paint order.
    fn solid_bitmap(color: [u8; 3]) -> Arc<erars_ui::image::ImageBitmap> {
        let argb =
            0xFF00_0000 | ((color[0] as u32) << 16) | ((color[1] as u32) << 8) | color[2] as u32;
        Arc::new(erars_ui::image::ImageBitmap::new(
            4,
            4,
            vec![argb; 16].into_boxed_slice(),
            1,
        ))
    }

    /// `lines` of text plus one plane entry at `zdepth`, sized to cover
    /// exactly the bottom log row.
    fn cbg_frame(lines: Vec<ConsoleLine>, zdepth: i32, line_h: u32) -> ConsoleFrame {
        use erars_ui::cbg::CbgLayer;
        use erars_ui::image::{ImageSampler, InlineSprite, Rect as SrcRect};

        let mut fr = frame(lines);
        fr.images.publish(9, solid_bitmap(CBG));
        let sprite = InlineSprite {
            sampler: ImageSampler::Single {
                bitmap: 9,
                src: SrcRect {
                    x: 0,
                    y: 0,
                    width: 4,
                    height: 4,
                },
            },
            width: CBG_W,
            height: line_h,
            pos_x: 0,
            pos_y: 0,
        };
        let mut plane = CbgLayer::default();
        plane.set_image(sprite, CBG_X as i32, 0, zdepth);
        fr.cbg = Arc::new(plane);
        fr
    }

    /// One plane entry holding the whole 8×4 quad bitmap in a 32×16 box at
    /// `y`, in front of the text.
    fn cbg_quad_frame(y: i32) -> ConsoleFrame {
        use erars_ui::cbg::CbgLayer;
        use erars_ui::image::{ImageSampler, InlineSprite, Rect as SrcRect};

        let mut fr = frame(vec![text_line("", WHITE)]);
        fr.images.publish(7, quad_bitmap());
        let sprite = InlineSprite {
            sampler: ImageSampler::Single {
                bitmap: 7,
                src: SrcRect {
                    x: 0,
                    y: 0,
                    width: 8,
                    height: 4,
                },
            },
            width: 32,
            height: 16,
            pos_x: 0,
            pos_y: 0,
        };
        let mut plane = CbgLayer::default();
        plane.set_image(sprite, CBG_X as i32, y, -1);
        fr.cbg = Arc::new(plane);
        fr
    }

    /// The plane is clipped at the console area's bottom edge, not at the
    /// framebuffer's: Emuera paints it on `MainPicBox`, whose height *is*
    /// `ClientHeight` (`GameView/EmueraConsole.cs:238`), so it can never
    /// reach the input box — while erars draws its input strip into the same
    /// surface, so the edge has to be applied by hand (`draw.rs`,
    /// `clip_below`).
    ///
    /// Two renders of the same entry, 8 px apart: the lower one is cut at the
    /// edge, and its surviving rows must be *pixel-identical* to the same rows
    /// of the higher one. That is what pins the UV window to the clipped box —
    /// scaling one without the other would stretch or shift the image
    /// instead of trimming it.
    #[test]
    fn the_plane_is_clipped_at_the_console_areas_bottom_edge() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let (w, h) = (64, 3 * m.line_h);
        let view_h = h - m.line_h;

        let flush = render(&mut shaper, &dev, &cbg_quad_frame(0), w, h, None, None);
        let over = render(&mut shaper, &dev, &cbg_quad_frame(8), w, h, None, None);

        for dy in 0..8 {
            for x in CBG_X..CBG_X + 32 {
                assert_eq!(
                    over.pixel(x, view_h - 8 + dy),
                    flush.pixel(x, view_h - 16 + dy),
                    "clipped row {dy} at x={x} must sample exactly what it did unclipped"
                );
            }
        }
        // The 8 rows the entry would have covered below the console area.
        for y in view_h..view_h + 8 {
            for x in [CBG_X, CBG_X + 16, CBG_X + 31] {
                assert_eq!(
                    over.pixel(x, y),
                    [0, 0, 0, 255],
                    "({x},{y}) is below the console area and must stay background"
                );
            }
        }
        // …and the entry really did reach that far: its top edge moved down by
        // the same 8 px, so this is a clip and not a smaller draw.
        let top = view_h - 16;
        assert_eq!(
            over.pixel(CBG_X, top + 8),
            flush.pixel(CBG_X, top),
            "the box's own top row moved down by 8"
        );
        assert_eq!(
            over.pixel(CBG_X, top + 7),
            [0, 0, 0, 255],
            "one pixel above the moved box must be background"
        );
    }

    /// The plane draws on both sides of the text: a positive `zdepth` is
    /// behind it and a negative one in front, which is Emuera's merged depth
    /// loop (`GameView/EmueraConsole.cs:1557-1599`) and the only way anything
    /// in erars can cover a glyph.
    ///
    /// Both renders share one fully-covered glyph pixel, found in a text-only
    /// render, so the two assertions differ *only* in the sign of the depth.
    #[test]
    fn the_plane_draws_behind_or_in_front_of_the_text_by_depth() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let (w, h) = (64, 3 * m.line_h);
        let row = row_y(1, 0, h, m.line_h);

        let text = || vec![text_line("MMMM", WHITE)];
        let plain = render(&mut shaper, &dev, &frame(text()), w, h, None, None);
        let (ix, iy) = (row..row + m.line_h)
            .flat_map(|y| (CBG_X..CBG_X + CBG_W).map(move |x| (x, y)))
            .find(|&(x, y)| plain.pixel(x, y) == [255, 255, 255, 255])
            .expect("some glyph pixel under the plane's box must be fully covered");

        let back = render(
            &mut shaper,
            &dev,
            &cbg_frame(text(), 2, m.line_h),
            w,
            h,
            None,
            None,
        );
        let front = render(
            &mut shaper,
            &dev,
            &cbg_frame(text(), -2, m.line_h),
            w,
            h,
            None,
            None,
        );
        assert_eq!(
            back.pixel(ix, iy),
            [255, 255, 255, 255],
            "zdepth 2 must stay behind the glyph at ({ix},{iy})"
        );
        let [r, g, b, _] = front.pixel(ix, iy);
        assert_eq!(
            [r, g, b], CBG,
            "zdepth -2 must cover the glyph at ({ix},{iy})"
        );

        // The plane drew at all, on both sides of the text: a box pixel the
        // glyphs never reach (the row's top-left corner, left of `m.shift`).
        for (img, name) in [(&back, "zdepth 2"), (&front, "zdepth -2")] {
            let [r, g, b, _] = img.pixel(CBG_X, row);
            assert_eq!([r, g, b], CBG, "{name} must paint its box");
        }
    }

    /// The box is placed in client pixels from the **bottom-left** corner of
    /// the console area (`:1573`, `y + ClientHeight - DestBaseSize.Height`),
    /// and the text layout does not move it: an entry at `y = 0` sized to one
    /// line sits on the last log row whether the log holds one line or twenty.
    ///
    /// Measured as the exact box: every corner is the plane's colour and
    /// every neighbour one pixel outside it is not. A plane measured against
    /// the whole surface instead of the console area would sit a line lower
    /// and fail the bottom edge; one drawn from the top-left would miss
    /// entirely.
    #[test]
    fn the_plane_sits_on_the_console_areas_bottom_left_corner() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);
        let m = *shaper.metrics();
        let (w, h) = (64, 6 * m.line_h);
        let view_h = h - m.line_h;
        let top = view_h - m.line_h;

        for rows in [1usize, 20] {
            let lines = (0..rows).map(|_| text_line("MMMM", WHITE)).collect();
            // In front of the text, so a box pixel cannot be a glyph pixel
            // and the bounds are unambiguous however the log is laid out.
            let img = render(
                &mut shaper,
                &dev,
                &cbg_frame(lines, -1, m.line_h),
                w,
                h,
                None,
                None,
            );
            let is_cbg = |x: u32, y: u32| {
                let [r, g, b, _] = img.pixel(x, y);
                [r, g, b] == CBG
            };
            for (x, y, corner) in [
                (CBG_X, top, "top-left"),
                (CBG_X + CBG_W - 1, top, "top-right"),
                (CBG_X, view_h - 1, "bottom-left"),
                (CBG_X + CBG_W - 1, view_h - 1, "bottom-right"),
            ] {
                assert!(is_cbg(x, y), "{rows} rows: {corner} corner ({x},{y})");
            }
            for (x, y, side) in [
                (CBG_X - 1, top, "left of"),
                (CBG_X + CBG_W, top, "right of"),
                (CBG_X, top - 1, "above"),
                (CBG_X, view_h, "below"),
            ] {
                assert!(
                    !is_cbg(x, y),
                    "{rows} rows: ({x},{y}) is {side} the box and must not be painted"
                );
            }
        }
    }

    // -----------------------------------------------------------------------
    // Positioned `<div>` boxes and island overlays
    // (`_Library/EvilMask/ConsoleDivPart.cs`)
    // -----------------------------------------------------------------------

    /// A render tall enough for exactly five log rows above the input strip,
    /// so row `r` of a five-line log starts at `r · line_h` with no top slack
    /// and every asserted coordinate is a plain multiple of 19.
    const DIV_H: u32 = 19 * 6;
    const DIV_VIEW_H: u32 = DIV_H - 19;
    const DIV_W: u32 = 200;

    fn div_part(
        anchor: erars_ui::DivAnchor,
        (x, y): (i32, i32),
        (width, height): (Option<u32>, Option<u32>),
        style: erars_ui::DivBox,
        lines: Vec<ConsoleLine>,
    ) -> ConsoleLinePart {
        ConsoleLinePart::Div(Arc::new(erars_ui::ConsoleDiv {
            anchor,
            x,
            y,
            width,
            height,
            style,
            lines,
            alt_head: String::new(),
        }))
    }

    /// One console line holding a single `w`×`h` block of flat `color`,
    /// published under `id`: a box's content whose every pixel is known, so a
    /// placement can be asserted at exact coordinates instead of "some ink".
    fn block_line(
        store: &erars_ui::image::ImageStore,
        id: u32,
        color: [u8; 3],
        w: u32,
        h: u32,
    ) -> ConsoleLine {
        use erars_ui::image::{
            ImageGeometry, ImageSampler, InlineImage, InlineSprite, MixedNum, Rect as SrcRect,
        };

        store.publish(id, solid_bitmap(color));
        let sprite = InlineSprite {
            sampler: ImageSampler::Single {
                bitmap: id,
                src: SrcRect {
                    x: 0,
                    y: 0,
                    width: 4,
                    height: 4,
                },
            },
            width: 4,
            height: 4,
            pos_x: 0,
            pos_y: 0,
        };
        let px = |num| Some(MixedNum { num, is_px: true });
        ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Image(Arc::new(InlineImage {
                name: "BLOCK".into(),
                button: None,
                mask: None,
                sprite,
                geometry: ImageGeometry::new(18, 4, 4, px(w as i32), px(h as i32), None),
                alt: "<img src='BLOCK'>".into(),
            }))],
        }
    }

    /// One line whose only part is `div`.
    fn div_line(div: ConsoleLinePart) -> ConsoleLine {
        ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![div],
        }
    }

    const RED: [u8; 3] = [255, 0, 0];
    const GREEN: [u8; 3] = [0, 255, 0];
    const BLUE: [u8; 3] = [0, 0, 255];

    fn is(img: &Rendered, x: u32, y: u32, color: [u8; 3]) -> bool {
        let [r, g, b, _] = img.pixel(x, y);
        [r, g, b] == color
    }

    /// The bug this whole slice fixes: eramegaten_p_kr reserves blank lines
    /// with `PRINTL` and lifts a picture into them with a negative `ypos`
    /// (`PRINT_EVENT_PICTURE.ERB:12-70`). The box hangs off the row it was
    /// printed on (`ConsoleDivPart.cs:142`), so its content lands exactly 38
    /// px — two rows — above that row's top, and the part advances no pen
    /// (`:47`), so the row it sits on is untouched.
    #[test]
    fn a_relative_div_lifts_its_content_above_its_own_row() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);

        let fr = frame(Vec::new());
        let block = block_line(&fr.images, 11, RED, 20, 10);
        let fr = ConsoleFrame {
            lines: vec![
                text_line("", WHITE),
                text_line("", WHITE),
                div_line(div_part(
                    erars_ui::DivAnchor::Relative,
                    (10, -38),
                    (Some(40), Some(19)),
                    erars_ui::DivBox::default(),
                    vec![block],
                )),
            ],
            ..fr
        };
        let img = render(&mut shaper, &dev, &fr, DIV_W, DIV_H, None, None);

        // Three rows, bottom-anchored: row 2 starts at 95 − 19 = 76, and the
        // box's content origin is `shift + 10 = 13` across, `76 − 38 = 38` down.
        assert_eq!(row_y(3, 2, DIV_H, 19), 76);
        for (x, y) in [(13, 38), (32, 38), (13, 47), (32, 47)] {
            assert!(is(&img, x, y, RED), "block corner ({x},{y})");
        }
        for (x, y, side) in [
            (12, 38, "left of"),
            (33, 38, "right of"),
            (13, 37, "above"),
            (13, 48, "below"),
        ] {
            assert!(!is(&img, x, y, RED), "({x},{y}) is {side} the block");
        }
        // Nothing was drawn on the row the box was printed on.
        assert!(!is(&img, 13, 76, RED));
    }

    /// `display: absolute-lefttop` (`GameView/HtmlManager.cs:1155-1160`)
    /// measures from the console area's top-left, so the content lands at
    /// `(xpos, ypos)` plus nothing else — no `shift`, no row.
    #[test]
    fn an_absolute_lefttop_div_lands_at_its_own_coordinates() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);

        let fr = frame(Vec::new());
        let block = block_line(&fr.images, 12, GREEN, 20, 10);
        let fr = ConsoleFrame {
            lines: vec![div_line(div_part(
                erars_ui::DivAnchor::LeftTop,
                (17, 5),
                (Some(40), Some(19)),
                erars_ui::DivBox::default(),
                vec![block],
            ))],
            ..fr
        };
        let img = render(&mut shaper, &dev, &fr, DIV_W, DIV_H, None, None);

        for (x, y) in [(17, 5), (36, 5), (17, 14), (36, 14)] {
            assert!(is(&img, x, y, GREEN), "block corner ({x},{y})");
        }
        for (x, y, side) in [
            (16, 5, "left of"),
            (37, 5, "right of"),
            (17, 4, "above"),
            (17, 15, "below"),
        ] {
            assert!(!is(&img, x, y, GREEN), "({x},{y}) is {side} the block");
        }
    }

    /// `display: absolute-leftbottom` with the corpus's negative `ypos`
    /// measures up from the console area's bottom edge, so the content lands
    /// at `view_h + ypos = 95 − 30 = 65`.
    ///
    /// DELIBERATE: this fork computes `MainPicBox.Height − PointY − Height`
    /// (`ConsoleDivPart.cs:143`), i.e. a positive `ypos` measured down from
    /// the bottom of the box. Every corpus site passes a negative one, built
    /// by `CONVERT_YPOS_TOP_TO_BUTTOM.ERB` as `ypos + (−height·100 + 100)`,
    /// which only lands correctly under the rule asserted here.
    #[test]
    fn an_absolute_leftbottom_div_measures_up_from_the_bottom_edge() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);

        let fr = frame(Vec::new());
        let block = block_line(&fr.images, 13, BLUE, 20, 10);
        let fr = ConsoleFrame {
            lines: vec![div_line(div_part(
                erars_ui::DivAnchor::LeftBottom,
                (7, -30),
                (Some(40), Some(19)),
                erars_ui::DivBox::default(),
                vec![block],
            ))],
            ..fr
        };
        let img = render(&mut shaper, &dev, &fr, DIV_W, DIV_H, None, None);

        assert_eq!(DIV_VIEW_H, 95);
        for (x, y) in [(7, 65), (26, 65), (7, 74), (26, 74)] {
            assert!(is(&img, x, y, BLUE), "block corner ({x},{y})");
        }
        for (x, y, side) in [
            (6, 65, "left of"),
            (27, 65, "right of"),
            (7, 64, "above"),
            (7, 75, "below"),
        ] {
            assert!(!is(&img, x, y, BLUE), "({x},{y}) is {side} the block");
        }
    }

    /// `HTML_PRINT_ISLAND` overlays cover the log, and a later entry covers an
    /// earlier one — `MESSAGE_POPUP.ERB` dims the screen with layer 98 under
    /// its popup on layer 99, and `SYSTEM_DUNGEON.ERB:2630-2641` stacks two
    /// islands *on the same layer*, which is why `ConsoleFrame::islands` is a
    /// paint-ordered list and not a map.
    #[test]
    fn island_entries_cover_the_log_and_each_other_in_order() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);

        // Five lines fill the row area exactly, so line 0 occupies y 0..19 —
        // the rows an island at (0, 0) covers.
        let lines = || {
            let mut v = vec![text_line("MMMM", WHITE)];
            v.extend((0..4).map(|_| text_line("", WHITE)));
            v
        };
        let bare = render(&mut shaper, &dev, &frame(lines()), DIV_W, DIV_H, None, None);
        assert_eq!(row_y(5, 0, DIV_H, 19), 0);

        // An empty box paints only its frame: a flat fill 20 px wide first,
        // then a narrower one over it.
        let island = |w: u32, color: [u8; 3]| {
            vec![div_line(div_part(
                erars_ui::DivAnchor::LeftTop,
                (0, 0),
                (Some(w), Some(19)),
                erars_ui::DivBox {
                    background: Some(erars_ui::Color(color)),
                    ..erars_ui::DivBox::default()
                },
                Vec::new(),
            ))]
        };
        let fr = ConsoleFrame {
            islands: vec![(98, island(20, BLUE)), (98, island(10, RED))],
            ..frame(lines())
        };
        let img = render(&mut shaper, &dev, &fr, DIV_W, DIV_H, None, None);

        // The log really had ink under both boxes.
        let under = bare.ink_columns(0, 19, INK);
        assert!(under[3..10].iter().any(|&c| c), "no log ink under the 2nd island");
        assert!(under[10..20].iter().any(|&c| c), "no log ink under the 1st island");
        // The later entry wins where they overlap, at pinned pixels.
        assert!(is(&img, 2, 10, RED), "the 2nd island must be on top at (2,10)");
        assert!(is(&img, 9, 18, RED));
        assert!(is(&img, 10, 10, BLUE), "the 1st island shows where the 2nd ends");
        assert!(is(&img, 19, 0, BLUE));
        // Neither box reaches past its own width, and the log survives there.
        assert!(!is(&img, 20, 10, BLUE));
        assert_eq!(
            &under[20..40],
            &img.ink_columns(0, 19, INK)[20..40],
            "the islands disturbed the log outside their boxes"
        );
    }

    /// The content clip (`ConsoleDivPart.cs:159`): a child wider and taller
    /// than the box is cut at the padding rect on both axes, and only there.
    #[test]
    fn a_sized_div_clips_its_content_on_both_axes() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);

        let fr = frame(Vec::new());
        // 40×20 of content in a 20×6 box at (10, 10).
        let block = block_line(&fr.images, 14, GREEN, 40, 20);
        let fr = ConsoleFrame {
            lines: vec![div_line(div_part(
                erars_ui::DivAnchor::LeftTop,
                (10, 10),
                (Some(20), Some(6)),
                erars_ui::DivBox::default(),
                vec![block],
            ))],
            ..fr
        };
        let img = render(&mut shaper, &dev, &fr, DIV_W, DIV_H, None, None);

        for (x, y) in [(10, 10), (29, 10), (10, 15), (29, 15)] {
            assert!(is(&img, x, y, GREEN), "kept corner ({x},{y})");
        }
        for (x, y, why) in [
            (30, 10, "past the box width"),
            (39, 10, "past the box width"),
            (10, 16, "past the box height"),
            (10, 29, "past the box height"),
        ] {
            assert!(
                !is(&img, x, y, GREEN),
                "({x},{y}) is {why} and must be clipped"
            );
        }
        assert_eq!(
            img.ink_bbox(0, DIV_W, 0, DIV_VIEW_H, INK),
            Some([10, 10, 30, 16]),
            "the block's ink is exactly the box's content rect [x0, y0, x1, y1)"
        );
    }

    /// `BoxBorder.DrawBorder` (`ConsoleDivPart.cs:150`,
    /// `_Library/EvilMask/Shape.cs:19-107`): the background fills the rect the
    /// margin leaves, then each edge is painted `border[e]` thick inside it,
    /// in `bcolor[e]` or — when that edge has none — the frame's fore colour
    /// (`Shape.cs:63`).
    #[test]
    fn a_div_paints_its_background_and_four_borders() {
        let _gpu = gpu_lock();
        let Some(dev) = gpu_device() else { return };
        let mut shaper = jp_shaper(&[bundled_font()]);

        const FILL: [u8; 3] = [0, 0, 128];
        let style = erars_ui::DivBox {
            margin: [1, 1, 1, 1],
            border: [2, 3, 4, 5],
            padding: [0; 4],
            border_color: [Some(erars_ui::Color(RED)), None, Some(erars_ui::Color(GREEN)), Some(erars_ui::Color(BLUE))],
            background: Some(erars_ui::Color(FILL)),
        };
        let fr = ConsoleFrame {
            lines: vec![div_line(div_part(
                erars_ui::DivAnchor::LeftTop,
                (20, 20),
                (Some(30), Some(24)),
                style,
                Vec::new(),
            ))],
            ..frame(Vec::new())
        };
        let img = render(&mut shaper, &dev, &fr, DIV_W, DIV_H, None, None);

        // The 1 px margin leaves a 28 × 22 rect at (21, 21): x 21..49, y 21..43.
        const FG: [u8; 3] = [192, 192, 192];
        for (x, y, want, what) in [
            (30, 21, RED, "top border, first row"),
            (30, 22, RED, "top border, last row"),
            (30, 23, FILL, "background under the top border"),
            (30, 42, GREEN, "bottom border, last row"),
            (30, 39, GREEN, "bottom border, first row"),
            (30, 38, FILL, "background above the bottom border"),
            (21, 30, BLUE, "left border, first column"),
            (25, 30, BLUE, "left border, last column"),
            (26, 30, FILL, "background right of the left border"),
            (48, 30, FG, "right border, last column, in the fore colour"),
            (46, 30, FG, "right border, first column"),
            (45, 30, FILL, "background left of the right border"),
            // Left and right are full bands drawn over top and bottom.
            (21, 21, BLUE, "top-left corner"),
            (48, 42, FG, "bottom-right corner"),
        ] {
            let [r, g, b, _] = img.pixel(x, y);
            assert_eq!([r, g, b], want, "({x},{y}): {what}");
        }
        // The margin is outside everything the box paints.
        for (x, y) in [(20, 21), (21, 20), (49, 30), (30, 43)] {
            assert_eq!(img.pixel(x, y), [0, 0, 0, 255], "margin at ({x},{y})");
        }
    }
}
