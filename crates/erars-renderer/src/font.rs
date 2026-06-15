use cosmic_text::{fontdb, Attrs, Buffer, Family, FontSystem, Metrics, Shaping};

/// Owns the cosmic-text FontSystem plus the bundled fallback, and the
/// fixed cell size derived from the default monospace font.
pub struct FontCtx {
    pub font_system: FontSystem,
    /// Width of one grid cell in pixels (advance of an ASCII glyph).
    pub cell_w: f32,
    /// Height of one grid cell in pixels (config line_height).
    pub cell_h: f32,
    /// Font pixel size.
    pub font_size: f32,
    /// Default family name string.
    pub default_family: String,
}

/// Bundled Latin monospace fallback, always available regardless of OS.
const BUNDLED_FONT: &[u8] = include_bytes!("../assets/NotoSansMono-Regular.ttf");

impl FontCtx {
    pub fn new(default_family: &str, font_size: u32, line_height: u32) -> Self {
        let mut db = fontdb::Database::new();
        db.load_system_fonts();
        db.load_font_data(BUNDLED_FONT.to_vec());

        let locale = sys_locale::get_locale().unwrap_or_else(|| String::from("en-US"));
        let mut font_system = FontSystem::new_with_locale_and_db(locale, db);

        let font_size = font_size as f32;
        let cell_w = measure_cell_w(&mut font_system, default_family, font_size);

        Self {
            font_system,
            cell_w,
            cell_h: line_height as f32,
            font_size,
            default_family: default_family.to_string(),
        }
    }
}

/// Measure the advance of a representative ASCII glyph ("0") at this size.
fn measure_cell_w(font_system: &mut FontSystem, family: &str, font_size: f32) -> f32 {
    let mut buffer = Buffer::new(font_system, Metrics::new(font_size, font_size));
    let attrs = if family.is_empty() {
        Attrs::new().family(Family::Monospace)
    } else {
        Attrs::new().family(Family::Name(family))
    };
    buffer.set_text(font_system, "0", attrs, Shaping::Advanced);
    buffer.shape_until_scroll(font_system, false);
    let mut w = 0.0_f32;
    for run in buffer.layout_runs() {
        for glyph in run.glyphs.iter() {
            w += glyph.w;
        }
    }
    if w <= 0.0 {
        font_size * 0.6 // safety fallback if measurement failed
    } else {
        w
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cell_metrics_are_positive() {
        let ctx = FontCtx::new("", 18, 19);
        assert!(ctx.cell_w > 0.0, "cell_w must be positive, got {}", ctx.cell_w);
        assert_eq!(ctx.cell_h, 19.0);
        assert_eq!(ctx.font_size, 18.0);
    }
}
