//! GPU textures for the inline image layer: one texture per published bitmap,
//! re-uploaded only when its `generation` moves.
//!
//! Deliberately *not* the glyph atlas ([`crate::raster`]):
//!
//! * a `resources/` sheet is already shared by every sprite that crops it
//!   (`erars_vm::resources` decodes one parent per file path), so the texture
//!   count is the number of *sheets* a frame touches, not the number of
//!   sprites — atlas packing would save nothing;
//! * console art is routinely larger than one atlas page (`PAGE_SIZE`² =
//!   2048²), which packing cannot help with at all;
//! * glyphs are placed on integer pixels and must be sampled `Nearest`, while
//!   an image is scaled by `ConsoleImagePart`'s arithmetic and wants
//!   `Linear` — GDI+ never sets `InterpolationMode`, so Emuera scales
//!   bilinearly. The sampler lives in the bind group, i.e. per texture, so
//!   sharing pages with glyphs would make one of the two wrong.

use std::collections::HashMap;

use erars_ui::image::{BitmapId, ImageStore};

/// One bitmap's texture, valid for the `generation` it was uploaded from.
struct Cached {
    generation: u64,
    view: wgpu::TextureView,
}

/// Per-bitmap texture cache. Lives as long as the renderer; `sync` is the only
/// mutator, so a texture cannot be in use by a frame that did not ask for it.
#[derive(Default)]
pub struct ImageTextures {
    map: HashMap<BitmapId, Cached>,
}

impl ImageTextures {
    pub fn new() -> Self {
        Self::default()
    }

    /// Bring the cache in line with `store` for exactly the bitmaps in
    /// `wanted`, and drop every texture no frame asked for.
    ///
    /// Uploading is keyed on `ImageBitmap::generation`, which the VM bumps on
    /// every publish of changed pixels, so a static image costs one upload for
    /// the whole run and a `GDISPOSE`d id frees its texture on the next frame.
    pub fn sync(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        store: &ImageStore,
        wanted: &[BitmapId],
    ) {
        self.map.retain(|id, _| wanted.contains(id));
        for &id in wanted {
            let Some(bitmap) = store.get(id) else {
                // Published between layout and draw is impossible (the frame
                // holds the store), so this only happens for a bitmap that was
                // never published: nothing to upload, and `view` returns
                // `None`, which drops the quad.
                self.map.remove(&id);
                continue;
            };
            if self
                .map
                .get(&id)
                .is_some_and(|c| c.generation == bitmap.generation)
            {
                continue;
            }
            if bitmap.width == 0 || bitmap.height == 0 {
                continue;
            }

            let texture = device.create_texture(&wgpu::TextureDescriptor {
                label: Some(&format!("image-{id}")),
                size: wgpu::Extent3d {
                    width: bitmap.width,
                    height: bitmap.height,
                    depth_or_array_layers: 1,
                },
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                // `ImageBitmap` packs `0xAARRGGBB`, so a little-endian host
                // already holds B, G, R, A — `Bgra8Unorm` is the same bytes
                // and the upload is a reinterpretation, not a conversion.
                format: wgpu::TextureFormat::Bgra8Unorm,
                usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                view_formats: &[],
            });

            #[cfg(target_endian = "little")]
            let bytes: &[u8] = bytemuck::cast_slice(&bitmap.pixels);
            #[cfg(target_endian = "big")]
            let owned: Vec<u8> = bitmap
                .pixels
                .iter()
                .flat_map(|p| {
                    let [a, r, g, b] = p.to_be_bytes();
                    [b, g, r, a]
                })
                .collect();
            #[cfg(target_endian = "big")]
            let bytes: &[u8] = &owned;

            queue.write_texture(
                wgpu::ImageCopyTexture {
                    texture: &texture,
                    mip_level: 0,
                    origin: wgpu::Origin3d::ZERO,
                    aspect: wgpu::TextureAspect::All,
                },
                bytes,
                wgpu::ImageDataLayout {
                    offset: 0,
                    bytes_per_row: Some(bitmap.width * 4),
                    rows_per_image: Some(bitmap.height),
                },
                wgpu::Extent3d {
                    width: bitmap.width,
                    height: bitmap.height,
                    depth_or_array_layers: 1,
                },
            );

            self.map.insert(
                id,
                Cached {
                    generation: bitmap.generation,
                    view: texture.create_view(&wgpu::TextureViewDescriptor::default()),
                },
            );
        }
    }

    /// The texture view for `id`, or `None` when it is not cached — an image
    /// whose bitmap was never published simply does not draw, which is
    /// Emuera's `img.IsCreated` guard (`GameView/ConsoleImagePart.cs:200`).
    pub fn view(&self, id: BitmapId) -> Option<&wgpu::TextureView> {
        self.map.get(&id).map(|c| &c.view)
    }

    /// Pair every batch with its texture view, dropping batches whose bitmap
    /// is not cached. The result feeds `FrameDraw::push_pages`.
    pub fn pages_with<'a>(
        &'a self,
        batches: &'a [crate::draw::ImageBatch],
    ) -> Vec<(&'a wgpu::TextureView, &'a [crate::gpu::Instance])> {
        batches
            .iter()
            .filter_map(|b| Some((self.view(b.bitmap)?, b.instances.as_slice())))
            .collect()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}
