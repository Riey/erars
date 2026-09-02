//! Offscreen rendering for headless environments (SSH/CI, no display server)
//! and the `--headless-shot` CLI.
//!
//! `render_frame` draws a `ConsoleFrame` through the same path as the window
//! (`layout::layout` → `draw::build_instances` → `gpu::{create_quad_pipeline,
//! nearest_sampler, FrameDraw}`) into an `Rgba8Unorm` texture and reads it
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

use crate::draw::{build_instances, input_line, merge_pages, View};
use crate::font::{FontChain, FontConfig};
use crate::gpu::{create_quad_pipeline, nearest_sampler, FrameDraw, Globals, Instance};
use crate::layout::{layout_no_sweep, Geometry};
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
    let log = layout_no_sweep(&frame.lines, &g, shaper);
    let mut pages = build_instances(&log, &view, hover, hl, &mut raster, device, queue, shaper);

    // Input strip: one line laid out on its own, drawn on the bottom `line_h` rows.
    if let Some(input) = input {
        let line = input_line(input, frame.fore_color.0);
        let strip = layout_no_sweep(std::slice::from_ref(&line), &g, shaper);
        let strip_pages = build_instances(
            &strip,
            &view.strip(),
            None,
            hl,
            &mut raster,
            device,
            queue,
            shaper,
        );
        merge_pages(&mut pages, strip_pages);
    }
    // One sweep per rendered frame, after both layouts: sweeping between them
    // would drop the log's entries (see `layout_no_sweep`).
    shaper.sweep();
    if pages.len() < raster.page_count() {
        pages.resize_with(raster.page_count(), Vec::new);
    }

    let rgba = draw_offscreen(
        device,
        queue,
        &raster,
        &pages,
        frame.bg_color.0,
        content_w,
        height,
    );
    Ok(Rendered {
        width: content_w,
        height,
        rgba,
    })
}

/// Clear to `bg` (linear target, so the bytes come back exactly), draw every
/// page's instances against that page's atlas view with the `Nearest`
/// sampler, and read the texture back without row padding.
fn draw_offscreen(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    raster: &GlyphRaster,
    pages: &[Vec<Instance>],
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
    let draw = FrameDraw::new(
        device,
        &bind_group_layout,
        &globals_buf,
        &sampler,
        &raster.pages_with(pages),
    );

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

    use erars_ast::Value;
    use erars_compiler::Language;
    use erars_ui::width::WidthTable;

    use crate::font::{FontChain, StyleKey};
    use crate::layout::{layout, Layout, Row};
    use erars_ast::Alignment;
    use erars_ui::{ConsoleLine, ConsoleLinePart};
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
}
