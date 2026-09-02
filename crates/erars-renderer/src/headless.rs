//! Offscreen rendering for headless environments (SSH/CI, no display server).
//!
//! Renders a `ConsoleFrame`'s lines to an RGBA pixel buffer using the same
//! font/grid/atlas/shader path as the on-screen renderer, so alignment can be
//! asserted programmatically without an X server. See the tests below.

use cosmic_text::SwashCache;
use erars_ui::ConsoleLine;
use wgpu::util::DeviceExt;

use crate::atlas::GlyphAtlas;
use crate::draw::build_instances_legacy;
use crate::font::FontCtx;
use crate::gpu::{create_quad_pipeline, Globals};
use crate::grid::Grid;

/// A rendered RGBA8 image (row-major, 4 bytes/pixel, no row padding).
pub struct Rendered {
    pub width: u32,
    pub height: u32,
    pub rgba: Vec<u8>,
}

impl Rendered {
    /// Sum of pixel luminance over the rows `[y0, y1)` for each column x.
    /// Used to find where ink lands horizontally.
    pub fn column_ink(&self, y0: u32, y1: u32) -> Vec<f32> {
        let y1 = y1.min(self.height);
        let mut prof = vec![0.0f32; self.width as usize];
        for y in y0..y1 {
            let row = (y * self.width * 4) as usize;
            for x in 0..self.width as usize {
                let i = row + x * 4;
                let (r, g, b) = (
                    self.rgba[i] as f32,
                    self.rgba[i + 1] as f32,
                    self.rgba[i + 2] as f32,
                );
                prof[x] += 0.299 * r + 0.587 * g + 0.114 * b;
            }
        }
        prof
    }

    /// Rightmost column whose ink exceeds `threshold`, or 0 if none.
    pub fn ink_right_edge(prof: &[f32], threshold: f32) -> usize {
        prof.iter()
            .rposition(|&v| v > threshold)
            .unwrap_or(0)
    }
}

/// Render `lines` to an RGBA buffer of `width`x`height` on a headless GPU.
/// Returns `None` if no GPU adapter is available (so tests can skip).
pub fn render_lines(
    font: &mut FontCtx,
    lines: &[ConsoleLine],
    width: u32,
    height: u32,
) -> Option<Rendered> {
    let instance = wgpu::Instance::default();
    let adapter =
        pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions::default()))?;
    let (device, queue) =
        pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor::default(), None)).ok()?;

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

    let (pipeline, bind_group_layout) = create_quad_pipeline(&device, format);
    let mut atlas = GlyphAtlas::new(&device);
    let mut swash = SwashCache::new();

    let cols = ((width as f32 / font.cell_w).floor() as usize).max(1);
    let grid = Grid::build(font, lines, cols, None, None, [255, 255, 0]);
    let instances = build_instances_legacy(
        &device,
        &queue,
        &mut font.font_system,
        &mut swash,
        &mut atlas,
        &grid,
        0.0,
    );

    let globals_buf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("globals"),
        contents: bytemuck::bytes_of(&Globals {
            screen: [width as f32, height as f32],
            _pad: [0.0; 2],
        }),
        usage: wgpu::BufferUsages::UNIFORM,
    });
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        ..Default::default()
    });
    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("bg"),
        layout: &bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: globals_buf.as_entire_binding(),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::TextureView(&atlas.view),
            },
            wgpu::BindGroupEntry {
                binding: 2,
                resource: wgpu::BindingResource::Sampler(&sampler),
            },
        ],
    });
    let instance_buf = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
        label: Some("instances"),
        contents: bytemuck::cast_slice(&instances),
        usage: wgpu::BufferUsages::VERTEX,
    });

    // bytes_per_row must be a multiple of 256 for texture->buffer copies.
    let unpadded = width * 4;
    let padded = unpadded.div_ceil(256) * 256;
    let readback = device.create_buffer(&wgpu::BufferDescriptor {
        label: Some("readback"),
        size: (padded * height) as u64,
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
                    load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });
        if !instances.is_empty() {
            pass.set_pipeline(&pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.set_vertex_buffer(0, instance_buf.slice(..));
            pass.draw(0..6, 0..instances.len() as u32);
        }
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
    rx.recv().ok()?.ok()?;

    let mapped = slice.get_mapped_range();
    let mut rgba = vec![0u8; (unpadded * height) as usize];
    for y in 0..height as usize {
        let src = y * padded as usize;
        let dst = y * unpadded as usize;
        rgba[dst..dst + unpadded as usize]
            .copy_from_slice(&mapped[src..src + unpadded as usize]);
    }
    drop(mapped);
    readback.unmap();

    Some(Rendered {
        width,
        height,
        rgba,
    })
}

/// Write an RGBA buffer as a binary PPM (P6) — viewable with most image tools,
/// handy for eyeballing a headless render over SSH (`scp` it back).
pub fn write_ppm(path: &str, img: &Rendered) -> std::io::Result<()> {
    use std::io::Write;
    let mut f = std::io::BufWriter::new(std::fs::File::create(path)?);
    write!(f, "P6\n{} {}\n255\n", img.width, img.height)?;
    for px in img.rgba.chunks_exact(4) {
        f.write_all(&px[..3])?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use erars_ast::Alignment;
    use erars_ui::{Color, ConsoleLinePart, FontStyle, TextStyle};

    fn line(s: &str) -> ConsoleLine {
        ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Text(
                s.to_string(),
                TextStyle {
                    color: Color([255, 255, 255]),
                    font_family: "".into(),
                    font_style: FontStyle::NORMAL,
                },
            )],
        }
    }

    /// Text must not spread past its grid columns: a run of N half-width glyphs
    /// must end near N*cell_w. This is the pixel-level guard against font
    /// advances leaking into glyph positions (the bug that made text loose).
    #[test]
    fn text_stays_within_its_columns() {
        let _gpu = crate::test_support::gpu_lock();
        let mut font = FontCtx::new("", 18, 19);
        let cols = 12usize;
        let w = 400u32;
        let h = (font.cell_h * 2.0) as u32 + 4;
        let Some(img) = render_lines(&mut font, &[line(&"M".repeat(cols))], w, h) else {
            eprintln!("no GPU adapter; skipping");
            return;
        };
        let prof = img.column_ink(0, font.cell_h as u32 + 2);
        let threshold = prof.iter().cloned().fold(0.0f32, f32::max) * 0.15;
        let right = Rendered::ink_right_edge(&prof, threshold) as f32;
        let expected = cols as f32 * font.cell_w;
        assert!(
            right <= expected + font.cell_w && right >= expected - 2.0 * font.cell_w,
            "{cols} glyphs ended at x={right}, expected ~{expected} (cell_w={})",
            font.cell_w
        );
    }

    /// Identical lines must render to identical ink columns — i.e. column N is
    /// at the same x on every row. Proves vertical alignment / determinism.
    #[test]
    fn identical_rows_align_vertically() {
        let _gpu = crate::test_support::gpu_lock();
        let mut font = FontCtx::new("", 18, 19);
        let ch = font.cell_h as u32;
        let w = 400u32;
        let h = ch * 3 + 6;
        let txt = "Abc123Xyz";
        let Some(img) = render_lines(&mut font, &[line(txt), line(txt), line(txt)], w, h) else {
            eprintln!("no GPU adapter; skipping");
            return;
        };
        let p0 = img.column_ink(0, ch);
        let p1 = img.column_ink(ch, ch * 2);
        let total: f32 = p0.iter().sum();
        if total < 1.0 {
            eprintln!("no ink rendered; skipping");
            return;
        }
        // Normalised L1 difference between the two row bands' ink profiles.
        let diff: f32 = p0.iter().zip(&p1).map(|(a, b)| (a - b).abs()).sum();
        let rel = diff / total;
        assert!(
            rel < 0.05,
            "row ink profiles differ by {:.1}% — columns not aligned",
            rel * 100.0
        );
    }

    /// The core terminal property: a full-width CJK glyph occupies exactly two
    /// cells and lines up with Latin columns. Renders 8 half-width digits over
    /// 4 full-width ideographs (both = 8 cells) and asserts their ink ends at
    /// the same x. This is what the mixed-font / advance-leak bugs broke.
    /// Skips if no coherent CJK monospace is installed (the bundled font is
    /// Latin-only).
    #[test]
    fn cjk_fills_two_cells_aligned_with_latin() {
        let _gpu = crate::test_support::gpu_lock();
        // Prefer a coherent CJK monospace so Latin and CJK share 1:2 metrics.
        let mut font = FontCtx::with_candidates(
            &[
                "Sarasa Mono K",
                "Sarasa Mono J",
                "Noto Sans Mono CJK KR",
                "Noto Sans Mono CJK JP",
                "Noto Sans Mono CJK SC",
                "GulimChe",
                "MS Gothic",
            ],
            18,
            19,
        );
        let ch = font.cell_h as u32;
        let w = 400u32;
        let h = ch * 2 + 6;
        let latin = line("00000000"); // 8 half-width cells
        let cjk = line("永永永永"); // 4 full-width = 8 cells

        let Some(img) = render_lines(&mut font, &[latin, cjk], w, h) else {
            eprintln!("no GPU adapter; skipping");
            return;
        };
        let p_latin = img.column_ink(0, ch);
        let p_cjk = img.column_ink(ch, ch * 2);
        if p_cjk.iter().sum::<f32>() < 1.0 {
            eprintln!("no CJK glyph available (Latin-only fonts); skipping");
            return;
        }

        let edge = |prof: &[f32]| {
            let thr = prof.iter().cloned().fold(0.0f32, f32::max) * 0.15;
            Rendered::ink_right_edge(prof, thr) as f32
        };
        let right_latin = edge(&p_latin);
        let right_cjk = edge(&p_cjk);
        assert!(
            (right_latin - right_cjk).abs() <= font.cell_w,
            "8 Latin cells end at x={right_latin} but 4 CJK end at x={right_cjk} \
             (cell_w={}) — CJK is not exactly 2 cells / not aligned to the grid",
            font.cell_w
        );
    }
}
