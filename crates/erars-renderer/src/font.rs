use cosmic_text::{fontdb, Attrs, Buffer, Family, FontSystem, Metrics, Shaping};

/// Owns the cosmic-text FontSystem plus the bundled fallback, and the cell
/// size derived from the default monospace font.
///
/// All public pixel fields ([`cell_w`](Self::cell_w), [`cell_h`](Self::cell_h),
/// [`font_size`](Self::font_size)) are in *physical* pixels: logical sizes from
/// the config multiplied by the display scale factor. This keeps glyphs crisp
/// on HiDPI/Retina displays, where the surface is also sized in physical px.
pub struct FontCtx {
    pub font_system: FontSystem,
    /// Width of one grid cell in physical pixels (advance of an ASCII glyph).
    pub cell_w: f32,
    /// Height of one grid cell in physical pixels (config line_height).
    pub cell_h: f32,
    /// Font size in physical pixels.
    pub font_size: f32,
    /// Default family name string.
    pub default_family: String,

    logical_font_size: f32,
    logical_line_height: f32,
    scale: f32,
}

/// Bundled Latin monospace fallback, always available regardless of OS.
const BUNDLED_FONT: &[u8] = include_bytes!("../assets/NotoSansMono-Regular.ttf");

/// Directories that may hold Windows CJK fallback fonts (MS Gothic, Malgun, …).
/// On real Windows these are also covered by `load_system_fonts`; the extra
/// entries let a Linux user with a mounted Windows partition reuse them.
const WINDOWS_FONT_DIRS: &[&str] = &[
    r"C:\Windows\Fonts",
    "/win/Windows/Fonts",
    "/mnt/c/Windows/Fonts",
    "/c/Windows/Fonts",
];

/// Fallback font file names to pull from a Windows fonts directory. cosmic-text
/// then chooses among them per glyph via its built-in script fallback. Matched
/// case-insensitively. Covers Japanese (gothic/Yu Gothic/Meiryo), Korean
/// (Malgun/Gulim) and Simplified Chinese (SimSun).
const WINDOWS_FALLBACK_FONTS: &[&str] = &[
    "msgothic.ttc",
    "yugothr.ttc",
    "meiryo.ttc",
    "malgun.ttf",
    "gulim.ttc",
    "simsun.ttc",
];

impl FontCtx {
    pub fn new(default_family: &str, font_size: u32, line_height: u32) -> Self {
        let mut db = fontdb::Database::new();
        db.load_system_fonts();
        db.load_font_data(BUNDLED_FONT.to_vec());
        load_windows_fallback_fonts(&mut db);

        let locale = sys_locale::get_locale().unwrap_or_else(|| String::from("en-US"));
        let font_system = FontSystem::new_with_locale_and_db(locale, db);

        let mut ctx = Self {
            font_system,
            cell_w: 0.0,
            cell_h: 0.0,
            font_size: 0.0,
            default_family: default_family.to_string(),
            logical_font_size: font_size as f32,
            logical_line_height: line_height as f32,
            scale: 1.0,
        };
        ctx.recompute();
        ctx
    }

    /// Update the display scale factor and recompute physical metrics.
    /// Returns true if the scale actually changed.
    pub fn set_scale(&mut self, scale: f32) -> bool {
        let scale = if scale.is_finite() && scale > 0.0 { scale } else { 1.0 };
        if (scale - self.scale).abs() < f32::EPSILON {
            return false;
        }
        self.scale = scale;
        self.recompute();
        true
    }

    fn recompute(&mut self) {
        self.font_size = self.logical_font_size * self.scale;
        self.cell_h = self.logical_line_height * self.scale;
        let family = self.default_family.clone();
        self.cell_w = measure_cell_w(&mut self.font_system, &family, self.font_size);
    }
}

/// Load known Windows CJK fallback fonts into `db` from any available Windows
/// fonts directory. The directory can be overridden with `ERARS_FONT_DIR`
/// (loaded in full). Missing directories/files are silently skipped.
fn load_windows_fallback_fonts(db: &mut fontdb::Database) {
    if let Some(dir) = std::env::var_os("ERARS_FONT_DIR") {
        log::info!("Loading fonts from ERARS_FONT_DIR={:?}", dir);
        db.load_fonts_dir(dir);
    }

    for dir in WINDOWS_FONT_DIRS {
        let path = std::path::Path::new(dir);
        let Ok(entries) = std::fs::read_dir(path) else {
            continue;
        };
        for entry in entries.flatten() {
            let name = entry.file_name();
            let lower = name.to_string_lossy().to_ascii_lowercase();
            if WINDOWS_FALLBACK_FONTS.contains(&lower.as_str()) {
                match db.load_font_file(entry.path()) {
                    Ok(()) => log::info!("Loaded fallback font {}", entry.path().display()),
                    Err(e) => log::warn!("Failed to load {}: {e}", entry.path().display()),
                }
            }
        }
    }
}

/// Measure the advance of a representative ASCII glyph ("0") at this size.
fn measure_cell_w(font_system: &mut FontSystem, family: &str, font_size: f32) -> f32 {
    let mut buffer = Buffer::new(font_system, Metrics::new(font_size, font_size));
    // A buffer needs a layout area or `layout_runs` yields nothing.
    buffer.set_size(font_system, Some(font_size * 8.0), Some(font_size * 2.0));
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
