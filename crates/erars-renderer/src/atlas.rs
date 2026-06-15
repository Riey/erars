use std::collections::HashMap;

use cosmic_text::{CacheKey, FontSystem, SwashCache, SwashContent};
use etagere::{size2, AllocId, AtlasAllocator};

/// UV + placement info for a rasterized glyph.
#[derive(Clone, Copy, Debug)]
pub struct AtlasRegion {
    /// UV rect in [0,1]: x, y, w, h.
    pub uv: [f32; 4],
    /// Glyph bitmap size in px.
    pub size: [f32; 2],
    /// Left/top bearing from the pen origin (placement offsets).
    pub offset: [f32; 2],
    /// true if RGBA (color) glyph, false if alpha mask.
    pub color: bool,
    #[allow(dead_code)]
    alloc: AllocId,
}

const ATLAS_SIZE: u32 = 2048;

/// A single-page glyph atlas backed by an RGBA wgpu texture. Alpha-mask
/// glyphs are stored as white with coverage in the alpha channel, so one
/// texture serves both mask and color glyphs.
pub struct GlyphAtlas {
    allocator: AtlasAllocator,
    map: HashMap<CacheKey, Option<AtlasRegion>>,
    pub texture: wgpu::Texture,
    pub view: wgpu::TextureView,
    size: u32,
}

impl GlyphAtlas {
    pub fn new(device: &wgpu::Device) -> Self {
        let size = ATLAS_SIZE;
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("glyph-atlas"),
            size: wgpu::Extent3d {
                width: size,
                height: size,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8Unorm,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });
        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
        Self {
            allocator: AtlasAllocator::new(size2(size as i32, size as i32)),
            map: HashMap::new(),
            texture,
            view,
            size,
        }
    }

    /// Get (rasterizing on demand) the atlas region for a glyph.
    /// Returns None for empty glyphs (e.g. space).
    pub fn get(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        font_system: &mut FontSystem,
        swash: &mut SwashCache,
        key: CacheKey,
    ) -> Option<AtlasRegion> {
        if let Some(cached) = self.map.get(&key) {
            return *cached;
        }
        let region = self.rasterize(device, queue, font_system, swash, key);
        self.map.insert(key, region);
        region
    }

    fn rasterize(
        &mut self,
        _device: &wgpu::Device,
        queue: &wgpu::Queue,
        font_system: &mut FontSystem,
        swash: &mut SwashCache,
        key: CacheKey,
    ) -> Option<AtlasRegion> {
        let image = swash.get_image_uncached(font_system, key)?;
        let w = image.placement.width;
        let h = image.placement.height;
        if w == 0 || h == 0 {
            return None;
        }
        let is_color = matches!(image.content, SwashContent::Color);

        // Convert to RGBA8.
        let mut rgba = vec![0u8; (w * h * 4) as usize];
        match image.content {
            SwashContent::Mask => {
                for (i, a) in image.data.iter().enumerate() {
                    rgba[i * 4] = 255;
                    rgba[i * 4 + 1] = 255;
                    rgba[i * 4 + 2] = 255;
                    rgba[i * 4 + 3] = *a;
                }
            }
            SwashContent::Color => {
                rgba.copy_from_slice(&image.data);
            }
            SwashContent::SubpixelMask => {
                for (i, chunk) in image.data.chunks_exact(4).enumerate() {
                    rgba[i * 4] = 255;
                    rgba[i * 4 + 1] = 255;
                    rgba[i * 4 + 2] = 255;
                    rgba[i * 4 + 3] = chunk[3];
                }
            }
        }

        let alloc = self.allocator.allocate(size2(w as i32 + 1, h as i32 + 1))?;
        let rect = alloc.rectangle;
        let (x, y) = (rect.min.x as u32, rect.min.y as u32);

        queue.write_texture(
            wgpu::ImageCopyTexture {
                texture: &self.texture,
                mip_level: 0,
                origin: wgpu::Origin3d { x, y, z: 0 },
                aspect: wgpu::TextureAspect::All,
            },
            &rgba,
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(w * 4),
                rows_per_image: Some(h),
            },
            wgpu::Extent3d {
                width: w,
                height: h,
                depth_or_array_layers: 1,
            },
        );

        let s = self.size as f32;
        Some(AtlasRegion {
            uv: [x as f32 / s, y as f32 / s, w as f32 / s, h as f32 / s],
            size: [w as f32, h as f32],
            offset: [image.placement.left as f32, image.placement.top as f32],
            color: is_color,
            alloc: alloc.id,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::FontCtx;
    use crate::text::CellShaper;
    use erars_ui::{Color, FontStyle, TextStyle};

    fn headless_device() -> Option<(wgpu::Device, wgpu::Queue)> {
        let instance = wgpu::Instance::default();
        let adapter = pollster::block_on(
            instance.request_adapter(&wgpu::RequestAdapterOptions::default()),
        )?;
        pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor::default(), None)).ok()
    }

    #[test]
    fn rasterizes_a_glyph() {
        let Some((device, queue)) = headless_device() else {
            eprintln!("no GPU adapter; skipping");
            return;
        };
        let mut ctx = FontCtx::new("", 18, 19);
        let mut swash = SwashCache::new();
        let mut atlas = GlyphAtlas::new(&device);
        let style = TextStyle {
            color: Color([255, 255, 255]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        };
        let run = CellShaper::shape_run(&mut ctx, "A", &style, 0);
        let key = run.glyphs[0].cache_key;
        let region = atlas.get(&device, &queue, &mut ctx.font_system, &mut swash, key);
        assert!(region.is_some(), "glyph 'A' should rasterize to a region");
    }
}
