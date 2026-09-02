//! Shared helpers for tests: GPU gating, font gating and console fixtures.
//!
//! Skips are loud: every gate prints `SKIP <test>: <reason>` on stderr, and
//! `ERARS_REQUIRE_GPU=1` / `ERARS_REQUIRE_CJK_FONT=1` turn the corresponding
//! skip into a panic so CI cannot pass by running nothing. (`cargo test`
//! captures the output of passing tests — use `-- --nocapture` to see SKIPs.)

use std::path::PathBuf;
use std::sync::{Mutex, MutexGuard, OnceLock};

use cosmic_text::fontdb;
use erars_ast::Alignment;
use erars_compiler::Language;
use erars_proxy_system::ConsoleFrame;
use erars_ui::width::WidthTable;
use erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};

use crate::font::FontChain;
use crate::text::{CellMetrics, Shaper};

/// Serialize tests that create a wgpu device. Software adapters (lavapipe) can
/// fail or render incompletely when several devices are built concurrently, so
/// every GPU-touching test holds this lock for its duration.
pub fn gpu_lock() -> MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap_or_else(|e| e.into_inner())
}

/// The running test's name: libtest runs each test on a thread named after it
/// (`headless::tests::box_frame_ink_lands_in_cells`).
pub fn test_name() -> String {
    std::thread::current()
        .name()
        .unwrap_or("<unnamed test>")
        .to_string()
}

fn env_is_1(var: &str) -> bool {
    std::env::var_os(var).is_some_and(|v| v == "1")
}

/// A headless device, or `None` after printing `SKIP <test>: no wgpu adapter`.
/// With `ERARS_REQUIRE_GPU=1` (CI with lavapipe) the missing adapter panics.
pub fn gpu_device() -> Option<(wgpu::Device, wgpu::Queue)> {
    match crate::headless::request_device() {
        Some(d) => Some(d),
        None => {
            let name = test_name();
            if env_is_1("ERARS_REQUIRE_GPU") {
                panic!("{name}: ERARS_REQUIRE_GPU=1 but no wgpu adapter is available");
            }
            eprintln!("SKIP {name}: no wgpu adapter");
            None
        }
    }
}

/// The bundled Latin monospace — the only font the GPU-enforced tests use.
pub const BUNDLED_FONT_PATH: &str =
    concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf");

pub fn bundled_font() -> PathBuf {
    PathBuf::from(BUNDLED_FONT_PATH)
}

/// Family names that count as a usable CJK monospace for the `_cjk` tests.
const CJK_FAMILIES: &[&str] = &[
    "Noto Sans Mono CJK JP",
    "Noto Sans Mono CJK KR",
    "Noto Sans Mono CJK SC",
    "Noto Sans Mono CJK TC",
    "Sarasa Mono J",
    "Sarasa Mono K",
    "Sarasa Mono SC",
    "Sarasa Mono TC",
];

/// The file of the first face in `db` advertising one of `families`
/// (case-insensitive, any name language), searched in `families` order.
/// Upright regular faces are preferred (the Noto CJK family ships one TTC
/// per weight and fontdb's load order is machine-dependent); any weight or
/// style is accepted only when no regular face advertises the family.
fn font_file_for(db: &fontdb::Database, families: &[&str]) -> Option<PathBuf> {
    fn search(
        db: &fontdb::Database,
        families: &[&str],
        accept: impl Fn(&fontdb::FaceInfo) -> bool,
    ) -> Option<PathBuf> {
        families.iter().find_map(|fam| {
            db.faces().filter(|face| accept(face)).find_map(|face| {
                let hit = face
                    .families
                    .iter()
                    .any(|(name, _)| name.eq_ignore_ascii_case(fam));
                if !hit {
                    return None;
                }
                match &face.source {
                    fontdb::Source::File(p) | fontdb::Source::SharedFile(p, _) => Some(p.clone()),
                    fontdb::Source::Binary(_) => None,
                }
            })
        })
    }
    search(db, families, |face| {
        face.weight == fontdb::Weight::NORMAL && face.style == fontdb::Style::Normal
    })
    .or_else(|| search(db, families, |_| true))
}

/// A system CJK monospace font file for the `_cjk` tests, or `None` after
/// `SKIP <test>: no CJK monospace font installed`. `ERARS_REQUIRE_CJK_FONT=1`
/// turns the skip into a failure.
pub fn require_cjk_font() -> Option<PathBuf> {
    let mut db = fontdb::Database::new();
    db.load_system_fonts();
    let found = font_file_for(&db, CJK_FAMILIES);
    if found.is_none() {
        let name = test_name();
        if env_is_1("ERARS_REQUIRE_CJK_FONT") {
            panic!("{name}: ERARS_REQUIRE_CJK_FONT=1 but no CJK monospace font is installed");
        }
        eprintln!("SKIP {name}: no CJK monospace font installed");
    }
    found
}

/// `msgothic.ttc` from `ERARS_FONT_DIR` (the directory containing it, or the
/// file itself), or `None` after a SKIP line. Opt-in only: the font is
/// proprietary and never present in CI, so there is no REQUIRE variable.
pub fn msgothic_font() -> Option<PathBuf> {
    let found = std::env::var_os("ERARS_FONT_DIR").and_then(|d| {
        let p = PathBuf::from(d);
        let file = if p.is_file() { p } else { p.join("msgothic.ttc") };
        let is_ms = file
            .file_name()
            .and_then(|n| n.to_str())
            .is_some_and(|n| n.eq_ignore_ascii_case("msgothic.ttc"));
        (is_ms && file.is_file()).then_some(file)
    });
    if found.is_none() {
        eprintln!(
            "SKIP {}: msgothic.ttc not found under ERARS_FONT_DIR",
            test_name()
        );
    }
    found
}

/// A shaper over exactly `files` (no system fonts, no locale) with cell
/// metrics taken from the primary face at scale 1.
pub fn test_shaper(files: &[PathBuf], lang: Language, font_size: u32, line_height: u32) -> Shaper {
    let mut chain = FontChain::from_files(files, lang);
    let primary = chain.font(chain.primary());
    let m = CellMetrics::from_primary(&primary, font_size, line_height, 1.0);
    Shaper::new(chain, WidthTable::new(lang.encoding()), m)
}

pub fn style(color: [u8; 3]) -> TextStyle {
    TextStyle {
        color: Color(color),
        font_family: "".into(),
        font_style: FontStyle::NORMAL,
    }
}

pub fn text_line(s: &str, color: [u8; 3]) -> ConsoleLine {
    ConsoleLine {
        align: Alignment::Left,
        button_start: None,
        parts: vec![ConsoleLinePart::Text(s.to_string(), style(color))],
    }
}

/// Black background, Emuera's yellow focus colour, grey (192) default text.
pub fn frame(lines: Vec<ConsoleLine>) -> ConsoleFrame {
    ConsoleFrame {
        bg_color: Color([0, 0, 0]),
        hl_color: Color([255, 255, 0]),
        fore_color: Color([192, 192, 192]),
        lines,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name_is_the_test_path() {
        assert_eq!(test_name(), "test_support::tests::test_name_is_the_test_path");
    }

    #[test]
    fn bundled_font_exists_and_makes_the_documented_metrics() {
        assert!(std::path::Path::new(BUNDLED_FONT_PATH).is_file());
        let shaper = test_shaper(&[bundled_font()], Language::Japanese, 18, 19);
        let m = *shaper.metrics();
        assert_eq!((m.font_px, m.half_w, m.line_h, m.baseline, m.shift), (18, 11, 19, 19, 3));
    }
}
