use cosmic_text::{FontSystem, SwashCache};

use crate::atlas::GlyphAtlas;
use crate::gpu::Instance;
use crate::grid::Grid;

/// Build GPU instances for a grid: all glyph quads, positioned in screen
/// space. `scroll_y` is subtracted from content-space y.
pub fn build_instances(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    font_system: &mut FontSystem,
    swash: &mut SwashCache,
    atlas: &mut GlyphAtlas,
    grid: &Grid,
    scroll_y: f32,
) -> Vec<Instance> {
    let mut out = Vec::with_capacity(grid.glyphs.len());

    for g in &grid.glyphs {
        let Some(region) = atlas.get(device, queue, font_system, swash, g.cache_key) else {
            continue;
        };
        let mode = if region.color { 2u32 } else { 1u32 };
        out.push(Instance {
            rect: [
                g.x_px + region.offset[0],
                g.y_px - region.offset[1] - scroll_y,
                region.size[0],
                region.size[1],
            ],
            uv: region.uv,
            color: [
                g.color[0] as f32 / 255.0,
                g.color[1] as f32 / 255.0,
                g.color[2] as f32 / 255.0,
                1.0,
            ],
            mode,
            _pad: [0; 3],
        });
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::FontCtx;
    use erars_ast::Alignment;
    use erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};

    fn headless() -> Option<(wgpu::Device, wgpu::Queue)> {
        let instance = wgpu::Instance::default();
        let adapter = pollster::block_on(
            instance.request_adapter(&wgpu::RequestAdapterOptions::default()),
        )?;
        pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor::default(), None)).ok()
    }

    #[test]
    fn produces_one_instance_per_visible_glyph() {
        let Some((device, queue)) = headless() else {
            eprintln!("no GPU; skipping");
            return;
        };
        let mut ctx = FontCtx::new("", 18, 19);
        let mut swash = SwashCache::new();
        let mut atlas = GlyphAtlas::new(&device);
        let line = ConsoleLine {
            align: Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Text(
                "abc".into(),
                TextStyle {
                    color: Color([255, 255, 255]),
                    font_family: "".into(),
                    font_style: FontStyle::NORMAL,
                },
            )],
        };
        let grid = Grid::build(&mut ctx, &[line], 30, None, None, [255, 255, 0]);
        let instances = build_instances(
            &device, &queue, &mut ctx.font_system, &mut swash, &mut atlas, &grid, 0.0,
        );
        assert_eq!(instances.len(), 3);
    }
}
