//! The multi-page glyph atlas (spec Component 6).
//!
//! The rasterisation itself is GPU-free and lives in [`erars_font::raster`],
//! which this module re-exports so every `crate::raster::…` path in the
//! renderer keeps resolving: `erars-vm` needs the same glyph images to
//! composite `GDRAWTEXT` into an ARGB bitmap, and it must not depend on wgpu
//! (or on this crate). What is left here is the part that needs a device.
//!
//! Atlas pages are `PAGE_SIZE`² `Rgba8Unorm` textures: mask glyphs are stored
//! white with coverage in alpha, colour glyphs as straight RGBA. A full page
//! spawns a new one; `draw.rs` buckets instances per page. Glyph quads are
//! placed on integer pixels and sampled with `FilterMode::Nearest`.

use std::collections::HashMap;

use cosmic_text::Font;
use etagere::AtlasAllocator;
use swash::scale::ScaleContext;

pub use erars_font::raster::*;

use crate::gpu::Instance;

struct PageTexture {
    texture: wgpu::Texture,
    view: wgpu::TextureView,
}

fn create_page(device: &wgpu::Device, index: usize) -> PageTexture {
    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some(&format!("glyph-atlas-{index}")),
        size: wgpu::Extent3d {
            width: PAGE_SIZE,
            height: PAGE_SIZE,
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
    PageTexture { texture, view }
}

/// Rasterizes glyphs on demand and keeps them in a multi-page atlas.
pub struct GlyphRaster {
    ctx: ScaleContext,
    /// One allocator per page; `allocs.len() == pages.len()` between calls.
    allocs: Vec<AtlasAllocator>,
    pages: Vec<PageTexture>,
    map: HashMap<RasterKey, Option<AtlasRegion>>,
    use_bitmap_strikes: bool,
    warned_oversize: bool,
}

impl GlyphRaster {
    /// Creates page 0. `use_bitmap_strikes = false` is the `--no-bitmap-strikes` CLI flag.
    pub fn new(device: &wgpu::Device, use_bitmap_strikes: bool) -> Self {
        Self {
            ctx: ScaleContext::new(),
            allocs: vec![new_allocator()],
            pages: vec![create_page(device, 0)],
            map: HashMap::new(),
            use_bitmap_strikes,
            warned_oversize: false,
        }
    }

    pub fn use_bitmap_strikes(&self) -> bool {
        self.use_bitmap_strikes
    }

    pub fn page_count(&self) -> usize {
        self.pages.len()
    }

    pub fn page_view(&self, page: usize) -> &wgpu::TextureView {
        &self.pages[page].view
    }

    /// Every page's texture view, in page order.
    pub fn page_views(&self) -> Vec<&wgpu::TextureView> {
        self.pages.iter().map(|p| &p.view).collect()
    }

    /// Pair every page's texture view with its instance bucket (the output of
    /// `draw::build_instances`), ready for `GpuContext::render` / `FrameDraw::new`.
    /// Buckets beyond the page count (none in practice) are dropped.
    pub fn pages_with<'a>(
        &'a self,
        buckets: &'a [Vec<Instance>],
    ) -> Vec<(&'a wgpu::TextureView, &'a [Instance])> {
        debug_assert_eq!(
            buckets.len(),
            self.pages.len(),
            "one instance bucket per atlas page"
        );
        self.pages
            .iter()
            .zip(buckets)
            .map(|(p, b)| (&p.view, b.as_slice()))
            .collect()
    }

    /// Cached result for `key` without rasterizing: `None` = never seen,
    /// `Some(None)` = known blank / unrasterizable, `Some(Some(r))` = in the atlas.
    pub fn lookup(&self, key: &RasterKey) -> Option<Option<AtlasRegion>> {
        self.map.get(key).copied()
    }

    /// Region for `key`, rasterizing and uploading on first use. `None` for
    /// blank glyphs (space) and for glyphs that cannot be rasterized or fit.
    pub fn get(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        font: &Font,
        key: RasterKey,
    ) -> Option<AtlasRegion> {
        if let Some(cached) = self.map.get(&key) {
            return *cached;
        }
        let region = rasterize(&mut self.ctx, font, key, self.use_bitmap_strikes)
            .filter(|img| !img.is_empty())
            .and_then(|img| self.upload(device, queue, &img));
        self.map.insert(key, region);
        region
    }

    fn upload(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        img: &GlyphImage,
    ) -> Option<AtlasRegion> {
        let Some((page, x, y)) = place(&mut self.allocs, img.width, img.height) else {
            if !self.warned_oversize {
                self.warned_oversize = true;
                log::warn!(
                    "glyph image {}x{} exceeds an atlas page ({PAGE_SIZE}²); skipped",
                    img.width,
                    img.height
                );
            }
            return None;
        };
        while self.pages.len() <= page {
            let index = self.pages.len();
            self.pages.push(create_page(device, index));
        }
        queue.write_texture(
            wgpu::ImageCopyTexture {
                texture: &self.pages[page].texture,
                mip_level: 0,
                origin: wgpu::Origin3d { x, y, z: 0 },
                aspect: wgpu::TextureAspect::All,
            },
            &img.rgba,
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(img.width * 4),
                rows_per_image: Some(img.height),
            },
            wgpu::Extent3d {
                width: img.width,
                height: img.height,
                depth_or_array_layers: 1,
            },
        );
        let s = PAGE_SIZE as f32;
        Some(AtlasRegion {
            page,
            uv: [
                x as f32 / s,
                y as f32 / s,
                img.width as f32 / s,
                img.height as f32 / s,
            ],
            size: [img.width, img.height],
            left: img.left,
            top: img.top,
            color: img.color,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::path::Path;
    use std::sync::Arc;

    use cosmic_text::{fontdb, FontSystem};
    use erars_font::flags::RasterFlags;

    const BUNDLED: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf");

    /// Load one face of a font file straight through fontdb/cosmic-text — no
    /// system fonts, no locale, independent of `FontChain`.
    fn load_face(path: &Path, index: u32) -> Arc<Font> {
        let mut db = fontdb::Database::new();
        db.load_font_data(std::fs::read(path).expect("read font file"));
        let id = db
            .faces()
            .find(|f| f.index == index)
            .expect("face index present")
            .id;
        let mut fs = FontSystem::new_with_locale_and_db("en-US".to_owned(), db);
        fs.get_font(id).expect("face loads")
    }

    fn gid(font: &Font, c: char) -> u16 {
        font.rustybuzz().glyph_index(c).expect("glyph present").0
    }

    #[test]
    fn get_uploads_a_from_the_bundled_font() {
        let _gpu = crate::test_support::gpu_lock();
        let Some((device, queue)) = crate::test_support::gpu_device() else {
            return;
        };
        let font = load_face(Path::new(BUNDLED), 0);
        let mut raster = GlyphRaster::new(&device, true);
        assert!(raster.use_bitmap_strikes());
        assert_eq!(raster.page_count(), 1);
        let key = RasterKey::new(font.id(), gid(&font, 'A'), 18.0, RasterFlags::empty());
        assert_eq!(raster.lookup(&key), None, "nothing cached yet");
        let region = raster
            .get(&device, &queue, &font, key)
            .expect("'A' rasterizes");
        assert_eq!(raster.lookup(&key), Some(Some(region)));
        assert_eq!(region.page, 0);
        assert!(!region.color);
        assert!(region.size[0] > 0 && region.size[1] > 0);
        assert!(region.uv.iter().all(|v| (0.0..=1.0).contains(v)));
        assert_eq!(
            raster.get(&device, &queue, &font, key),
            Some(region),
            "second lookup is served from the map"
        );
        let space = RasterKey::new(font.id(), gid(&font, ' '), 18.0, RasterFlags::empty());
        assert_eq!(
            raster.get(&device, &queue, &font, space),
            None,
            "blank glyphs take no atlas space"
        );
        assert_eq!(
            raster.lookup(&space),
            Some(None),
            "…but the blank result is cached"
        );
        let bold = RasterKey::new(font.id(), gid(&font, 'A'), 18.0, RasterFlags::BOLD_SYNTH);
        let b = raster.get(&device, &queue, &font, bold).unwrap();
        assert_ne!(b.uv, region.uv, "a different key gets its own region");
        assert!(b.size[0] > region.size[0]);
        assert_eq!(raster.page_views().len(), 1);
        let buckets = vec![vec![Instance {
            rect: [0.0; 4],
            uv: region.uv,
            color: [1.0; 4],
            mode: 1,
            _pad: [0; 3],
        }]];
        let pages = raster.pages_with(&buckets);
        assert_eq!(pages.len(), 1);
        assert_eq!(pages[0].1.len(), 1);
    }
}
