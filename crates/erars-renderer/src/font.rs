//! Font loading and per-character fallback.
//!
//! `FontChain` (spec Component 3) is the new API: an ordered list of faces,
//! a per-`(char, StyleKey)` resolution cache and real-vs-synthetic bold/italic
//! selection (spec Component 3).

use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};

use cosmic_text::{fontdb, ttf_parser, Font, FontSystem};
use erars_compiler::Language;
use erars_ui::{FontStyle, TextStyle};
use smol_str::SmolStr;

use crate::flags::RasterFlags;

/// Bundled Latin monospace fallback, always available regardless of OS.
pub const BUNDLED_FONT: &[u8] = include_bytes!("../assets/NotoSansMono-Regular.ttf");

/// Path of the bundled font on disk (for `FontChain::from_files` in tests
/// and layout goldens; never calls `load_system_fonts`).
pub fn bundled_font_path() -> PathBuf {
    PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf"))
}

/// Where the fonts come from (spec Component 3).
pub struct FontConfig<'a> {
    /// `emuera.config` フォント名; may be empty (no configured family).
    pub family: &'a str,
    /// `<game>/font/*.ttf|ttc|otf|otc` is loaded (Emuera.EM behaviour).
    pub game_dir: &'a Path,
    /// `ERARS_FONT_DIR` (the caller reads the environment).
    pub extra_dir: Option<PathBuf>,
    pub lang: Language,
}

/// The shaping-relevant part of a `TextStyle`: colour, underline and strike
/// are not resolution inputs. `family` is the part's SETFONT family ("" =
/// the configured chain).
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StyleKey {
    pub family: SmolStr,
    pub bold: bool,
    pub italic: bool,
}

impl StyleKey {
    pub fn from(style: &TextStyle) -> Self {
        Self {
            family: style.font_family.clone(),
            bold: style.font_style.contains(FontStyle::BOLD),
            italic: style.font_style.contains(FontStyle::ITALIC),
        }
    }

    /// No SETFONT family, regular weight and posture.
    pub fn plain() -> Self {
        Self {
            family: SmolStr::default(),
            bold: false,
            italic: false,
        }
    }
}

/// Fixed-pitch CJK families tried (in order) for a game language, after the
/// configured family and the game / extra font directories (spec Component 3;
/// the Windows-localised names are aliases fontdb also exposes).
pub fn language_candidates(lang: Language) -> &'static [&'static str] {
    match lang {
        Language::Japanese => &[
            "MS Gothic",
            "ＭＳ ゴシック",
            "Sarasa Mono J",
            "Noto Sans Mono CJK JP",
        ],
        Language::Korean => &[
            "D2Coding",
            "NanumGothicCoding",
            "GulimChe",
            "굴림체",
            "DotumChe",
            "돋움체",
            "Sarasa Mono K",
            "Noto Sans Mono CJK KR",
        ],
        Language::ChineseHans => &[
            "NSimSun",
            "Sarasa Mono SC",
            "Sarasa Mono TC",
            "Noto Sans Mono CJK SC",
            "Noto Sans Mono CJK TC",
        ],
        Language::ChineseHant => &[
            "NSimSun",
            "Sarasa Mono TC",
            "Sarasa Mono SC",
            "Noto Sans Mono CJK TC",
            "Noto Sans Mono CJK SC",
        ],
    }
}

/// Case-insensitive family comparison (Unicode lowercase, so `ＭＳ ゴシック`
/// compares byte-exact while `ms gothic` matches `MS Gothic`).
pub fn family_eq(a: &str, b: &str) -> bool {
    a.chars()
        .flat_map(char::to_lowercase)
        .eq(b.chars().flat_map(char::to_lowercase))
}

/// Regular face (upright, weight nearest 400, then load order) of the family
/// `name`, scanning `FaceInfo.families` (name ID 16 in every language, or ID 1
/// when the font has no ID 16). `Database::query` is not used: it is exact
/// and case-sensitive.
pub fn find_family(db: &fontdb::Database, name: &str) -> Option<fontdb::ID> {
    db.faces()
        .filter(|f| f.families.iter().any(|(n, _)| family_eq(n, name)))
        .min_by_key(|f| {
            (
                f.style != fontdb::Style::Normal,
                f.weight.0.abs_diff(fontdb::Weight::NORMAL.0),
            )
        })
        .map(|f| f.id)
}

/// Weight ≥ 600 (SemiBold) counts as a real bold face.
fn is_bold(f: &fontdb::FaceInfo) -> bool {
    f.weight >= fontdb::Weight::SEMIBOLD
}

/// Italic or Oblique counts as a real italic face.
fn is_italic(f: &fontdb::FaceInfo) -> bool {
    f.style != fontdb::Style::Normal
}

/// How many of the requested attributes `f` satisfies, or `None` when it is
/// bold / italic although the part did not ask for it.
fn style_score(f: &fontdb::FaceInfo, style: &StyleKey) -> Option<u8> {
    let (b, i) = (is_bold(f), is_italic(f));
    if (b && !style.bold) || (i && !style.italic) {
        return None;
    }
    Some(u8::from(b && style.bold) + u8::from(i && style.italic))
}

/// First upright weight-400 face of `ids` (load order), else the first id.
fn first_regular(db: &fontdb::Database, ids: &[fontdb::ID]) -> Option<fontdb::ID> {
    ids.iter()
        .copied()
        .find(|&id| {
            db.face(id)
                .map_or(false, |f| !is_italic(f) && f.weight == fontdb::Weight::NORMAL)
        })
        .or_else(|| ids.first().copied())
}

/// cmap coverage through a loaded `Font` (`rustybuzz::Face` derefs to
/// `ttf_parser::Face`). The `!= 0` guard matters: format-4 delta segments can
/// yield `GlyphId(0)` (e.g. U+FFFF in the bundled font).
pub fn font_covers(font: &Font, c: char) -> bool {
    font.rustybuzz().glyph_index(c).map_or(false, |g| g.0 != 0)
}

/// cmap coverage without materialising a `Font` (database-wide scans; faces
/// are only turned into `Font`s once chosen — critique R20).
pub fn face_covers(db: &fontdb::Database, id: fontdb::ID, c: char) -> bool {
    db.with_face_data(id, |data, index| {
        ttf_parser::Face::parse(data, index)
            .ok()
            .and_then(|face| face.glyph_index(c))
            .map_or(false, |g| g.0 != 0)
    })
    .unwrap_or(false)
}

/// Register the bundled Noto Sans Mono (zero-copy, static bytes).
fn load_bundled(db: &mut fontdb::Database) -> Vec<fontdb::ID> {
    let data: Arc<dyn AsRef<[u8]> + Send + Sync> = Arc::new(BUNDLED_FONT);
    db.load_font_source(fontdb::Source::Binary(data)).to_vec()
}

/// Load every `ttf|ttc|otf|otc` under `dir` (recursive, sorted by path so the
/// order is deterministic) and return the face ids in load order. Unlike
/// `Database::load_fonts_dir` this reports which ids came from the directory.
pub fn load_dir(db: &mut fontdb::Database, dir: &Path) -> Vec<fontdb::ID> {
    let mut ids = Vec::new();
    let Ok(read_dir) = std::fs::read_dir(dir) else {
        return ids;
    };
    let mut entries: Vec<PathBuf> = read_dir.flatten().map(|e| e.path()).collect();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            ids.extend(load_dir(db, &path));
            continue;
        }
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .map(|e| e.to_ascii_lowercase());
        if matches!(ext.as_deref(), Some("ttf" | "ttc" | "otf" | "otc")) {
            let loaded = db.load_font_source(fontdb::Source::File(path.clone()));
            if loaded.is_empty() {
                log::warn!("No font faces loaded from {}", path.display());
            }
            ids.extend(loaded);
        }
    }
    ids
}

/// Ordered per-character font fallback over a fontdb database.
///
/// Chain order: SETFONT family of the part (per `resolve` call) → configured
/// family → faces from `<game>/font/` → faces from `ERARS_FONT_DIR` →
/// per-language fixed-pitch CJK candidates → bundled Noto Sans Mono →
/// (lazily) every other face in load order.
pub struct FontChain {
    /// fontdb + `Font` loading only; `Buffer`/`Attrs` are never used.
    font_system: FontSystem,
    /// Ordered candidates, primary first (regular faces only, deduplicated).
    chain: Vec<fontdb::ID>,
    /// Metrics source; verified loadable at construction.
    primary: fontdb::ID,
    cache: HashMap<(char, StyleKey), (fontdb::ID, RasterFlags)>,
    /// SETFONT families already reported as missing (warn once).
    warned_families: HashSet<SmolStr>,
    /// Faces that failed to load (warn once).
    failed_faces: HashSet<fontdb::ID>,
}

/// Faces grouped by origin, in chain order.
#[derive(Default)]
struct Seeds {
    configured: Vec<fontdb::ID>,
    game_dir: Vec<fontdb::ID>,
    extra_dir: Vec<fontdb::ID>,
    bundled: Vec<fontdb::ID>,
}

impl FontChain {
    /// System fonts + `<game>/font/` + `ERARS_FONT_DIR` + the bundled font.
    pub fn new(cfg: &FontConfig) -> Self {
        let mut db = fontdb::Database::new();
        db.load_system_fonts();
        let mut seeds = Seeds::default();
        let game_font_dir = cfg.game_dir.join("font");
        if game_font_dir.is_dir() {
            seeds.game_dir = load_dir(&mut db, &game_font_dir);
            log::info!(
                "Loaded {} face(s) from {}",
                seeds.game_dir.len(),
                game_font_dir.display()
            );
        }
        if let Some(dir) = &cfg.extra_dir {
            seeds.extra_dir = load_dir(&mut db, dir);
            log::info!(
                "Loaded {} face(s) from ERARS_FONT_DIR={}",
                seeds.extra_dir.len(),
                dir.display()
            );
        }
        seeds.bundled = load_bundled(&mut db);
        if !cfg.family.is_empty() {
            match find_family(&db, cfg.family) {
                Some(id) => seeds.configured.push(id),
                None => log::warn!(
                    "Configured font family {:?} is not installed; using the per-language chain",
                    cfg.family
                ),
            }
        }
        Self::build(db, seeds, cfg.lang)
    }

    /// Tests and goldens: exactly these files (in order), no system fonts, no
    /// locale. The files play the role of `ERARS_FONT_DIR` faces; with an
    /// empty list the bundled font is loaded so the chain is never empty.
    pub fn from_files(files: &[PathBuf], lang: Language) -> Self {
        let mut db = fontdb::Database::new();
        let mut seeds = Seeds::default();
        for path in files {
            let ids = db.load_font_source(fontdb::Source::File(path.clone()));
            if ids.is_empty() {
                log::warn!("No font faces loaded from {}", path.display());
            }
            seeds.extra_dir.extend(ids);
        }
        Self::build(db, seeds, lang)
    }

    fn build(db: fontdb::Database, seeds: Seeds, lang: Language) -> Self {
        let lang_faces: Vec<fontdb::ID> = language_candidates(lang)
            .iter()
            .filter_map(|name| find_family(&db, name))
            .collect();
        let mut chain: Vec<fontdb::ID> = Vec::new();
        for id in seeds
            .configured
            .iter()
            .chain(&seeds.game_dir)
            .chain(&seeds.extra_dir)
            .chain(&lang_faces)
            .chain(&seeds.bundled)
        {
            if !chain.contains(id) {
                chain.push(*id);
            }
        }
        // Primary: first present of configured → game dir → extra dir →
        // language list → bundled (regular face preferred inside a directory).
        let primary_candidates: Vec<fontdb::ID> = seeds
            .configured
            .iter()
            .copied()
            .chain(first_regular(&db, &seeds.game_dir))
            .chain(first_regular(&db, &seeds.extra_dir))
            .chain(lang_faces.iter().copied())
            .chain(seeds.bundled.iter().copied())
            .collect();
        // The locale only feeds cosmic-text's Buffer fallback, which is unused.
        let mut font_system = FontSystem::new_with_locale_and_db(String::from("en-US"), db);
        let mut primary = primary_candidates
            .iter()
            .copied()
            .find(|id| font_system.get_font(*id).is_some());
        if primary.is_none() {
            // Nothing loadable (e.g. `from_files(&[])`): fall back to the bundled font.
            let ids = load_bundled(font_system.db_mut());
            primary = ids
                .iter()
                .copied()
                .find(|id| font_system.get_font(*id).is_some());
            chain.extend(ids);
        }
        let primary = primary.expect("bundled font always loads");
        if let Some(info) = font_system.db().face(primary) {
            log::info!(
                "Primary font: {:?} (face {})",
                info.families.first().map(|f| f.0.as_str()).unwrap_or(""),
                info.index
            );
        }
        Self {
            font_system,
            chain,
            primary,
            cache: HashMap::new(),
            warned_families: HashSet::new(),
            failed_faces: HashSet::new(),
        }
    }

    pub fn primary(&self) -> fontdb::ID {
        self.primary
    }

    pub fn db(&self) -> &fontdb::Database {
        self.font_system.db()
    }

    /// Regular face of a family present in the database (case-insensitive).
    pub fn find_family(&self, name: &str) -> Option<fontdb::ID> {
        find_family(self.font_system.db(), name)
    }

    #[cfg(test)]
    pub(crate) fn cache_len(&self) -> usize {
        self.cache.len()
    }
}

impl FontChain {
    /// First chain font whose cmap covers `c`. With `bold`/`italic` set, a real
    /// bold/italic face of that family is preferred; if none exists the regular
    /// face is returned with BOLD_SYNTH / ITALIC_SYNTH set. If no chain font
    /// covers `c`, the first face in the whole database that does; else the
    /// primary (renders .notdef). Results are cached per `(char, StyleKey)`.
    pub fn resolve(&mut self, c: char, style: &StyleKey) -> (fontdb::ID, RasterFlags) {
        let key = (c, style.clone());
        if let Some(hit) = self.cache.get(&key) {
            return *hit;
        }
        let regular = self.resolve_regular(c, &style.family);
        let result = self.apply_style(regular, c, style);
        self.cache.insert(key, result);
        result
    }

    /// Owned `Arc` so callers can keep borrowing the chain mutably. A face
    /// that fails to load is drawn with the primary font (warned once per face).
    pub fn font(&mut self, id: fontdb::ID) -> Arc<Font> {
        if let Some(font) = self.font_system.get_font(id) {
            return font;
        }
        if self.failed_faces.insert(id) {
            log::warn!("Font face {id} failed to load; drawing with the primary font");
        }
        self.font_system
            .get_font(self.primary)
            .expect("primary font verified at construction")
    }

    /// The regular face for `c`: SETFONT family → chain → database-wide scan
    /// in load order → primary.
    fn resolve_regular(&mut self, c: char, family: &str) -> fontdb::ID {
        if !family.is_empty() {
            match find_family(self.font_system.db(), family) {
                Some(id) => {
                    if self.loaded_covers(id, c) {
                        return id;
                    }
                }
                None => {
                    if self.warned_families.insert(SmolStr::new(family)) {
                        log::warn!(
                            "SETFONT family {family:?} is not installed; using the default chain"
                        );
                    }
                }
            }
        }
        for i in 0..self.chain.len() {
            let id = self.chain[i];
            if self.loaded_covers(id, c) {
                return id;
            }
        }
        // Database-wide fallback: coverage is read from the raw face data
        // (`with_face_data`); only the chosen face is materialised, and one
        // that fails to load is skipped.
        let others: Vec<fontdb::ID> = self
            .font_system
            .db()
            .faces()
            .map(|f| f.id)
            .filter(|id| !self.chain.contains(id))
            .collect();
        for id in others {
            if face_covers(self.font_system.db(), id, c)
                && self.font_system.get_font(id).is_some()
            {
                return id;
            }
        }
        self.primary
    }

    /// Coverage through the materialised `Font` (chain fonts are few and are
    /// needed for shaping anyway).
    fn loaded_covers(&mut self, id: fontdb::ID, c: char) -> bool {
        self.font_system
            .get_font(id)
            .map_or(false, |font| font_covers(&font, c))
    }

    /// Real bold / italic selection. Among the other faces that share a
    /// family name with `regular`, cover `c`, load, and carry no bold/italic
    /// the part did not ask for, the one satisfying most requested attributes
    /// wins (ties: load order); whatever it still lacks is flagged synthetic.
    /// With no such face the regular face carries the flags.
    fn apply_style(
        &mut self,
        regular: fontdb::ID,
        c: char,
        style: &StyleKey,
    ) -> (fontdb::ID, RasterFlags) {
        if !style.bold && !style.italic {
            return (regular, RasterFlags::empty());
        }
        let db = self.font_system.db();
        let family: Vec<String> = db
            .face(regular)
            .map(|f| f.families.iter().map(|(n, _)| n.clone()).collect())
            .unwrap_or_default();
        let mut candidates: Vec<(u8, fontdb::ID)> = db
            .faces()
            .filter(|f| f.id != regular)
            .filter(|f| {
                f.families
                    .iter()
                    .any(|(n, _)| family.iter().any(|m| family_eq(n, m)))
            })
            .filter_map(|f| style_score(f, style).map(|s| (s, f.id)))
            .filter(|(s, _)| *s > 0)
            .collect();
        // stable: equal scores keep load order
        candidates.sort_by(|a, b| b.0.cmp(&a.0));
        let mut chosen = regular;
        for (_, id) in candidates {
            if face_covers(self.font_system.db(), id, c)
                && self.font_system.get_font(id).is_some()
            {
                chosen = id;
                break;
            }
        }
        let mut flags = RasterFlags::empty();
        if let Some(info) = self.font_system.db().face(chosen) {
            if style.bold && !is_bold(info) {
                flags |= RasterFlags::BOLD_SYNTH;
            }
            if style.italic && !is_italic(info) {
                flags |= RasterFlags::ITALIC_SYNTH;
            }
        }
        (chosen, flags)
    }
}

#[cfg(test)]
mod chain_tests {
    use super::*;
    use erars_ui::Color;

    fn bundled_chain() -> FontChain {
        FontChain::from_files(&[bundled_font_path()], Language::Japanese)
    }

    fn key(family: &str, bold: bool, italic: bool) -> StyleKey {
        StyleKey { family: SmolStr::new(family), bold, italic }
    }

    /// Fresh per-test scratch directory (tests run in parallel).
    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("erars-font-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn source_path(db: &fontdb::Database, id: fontdb::ID) -> PathBuf {
        match &db.face(id).expect("face exists").source {
            fontdb::Source::File(p) | fontdb::Source::SharedFile(p, _) => p.clone(),
            fontdb::Source::Binary(_) => panic!("expected a file-backed face"),
        }
    }

    /// `$ERARS_FONT_DIR/msgothic.ttc` when present (opt-in, never in CI).
    /// Otherwise prints a SKIP line, or panics when `ERARS_REQUIRE_CJK_FONT=1`
    /// insists on the font being there (spec Testing §5 gating).
    fn msgothic(test: &str) -> Option<PathBuf> {
        let path = std::env::var_os("ERARS_FONT_DIR")
            .map(|dir| PathBuf::from(dir).join("msgothic.ttc"))
            .filter(|p| p.is_file());
        if path.is_none() {
            let msg = format!("SKIP {test}: $ERARS_FONT_DIR/msgothic.ttc not found");
            if std::env::var_os("ERARS_REQUIRE_CJK_FONT").is_some_and(|v| v == "1") {
                panic!("{msg} (ERARS_REQUIRE_CJK_FONT=1)");
            }
            eprintln!("{msg}");
        }
        path
    }

    fn face_info(weight: u16, style: fontdb::Style) -> fontdb::FaceInfo {
        fontdb::FaceInfo {
            id: fontdb::ID::dummy(),
            source: fontdb::Source::Binary(Arc::new(Vec::<u8>::new())),
            index: 0,
            families: vec![("Test".to_string(), fontdb::Language::English_UnitedStates)],
            post_script_name: "Test".to_string(),
            style,
            weight: fontdb::Weight(weight),
            stretch: fontdb::Stretch::Normal,
            monospaced: true,
        }
    }

    #[test]
    fn bundled_is_primary_and_family_matches_case_insensitively() {
        let chain = bundled_chain();
        let primary = chain.primary();
        let info = chain.db().face(primary).unwrap();
        assert_eq!(info.families[0].0, "Noto Sans Mono");
        assert_eq!(chain.find_family("noto sans mono"), Some(primary));
        assert_eq!(chain.find_family("NOTO SANS MONO"), Some(primary));
        assert_eq!(chain.find_family("No Such Font"), None);
    }

    #[test]
    fn covered_chars_resolve_to_primary_without_flags() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        for c in ['A', ' ', '┏', '━', '═', '░', '█'] {
            assert_eq!(
                chain.resolve(c, &StyleKey::plain()),
                (primary, RasterFlags::empty()),
                "{c:?}"
            );
            assert!(font_covers(&chain.font(primary), c), "{c:?} must be in the bundled cmap");
        }
    }

    #[test]
    fn uncovered_chars_fall_back_to_primary_notdef() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        for c in ['あ', '한', '漢'] {
            let (id, flags) = chain.resolve(c, &StyleKey::plain());
            assert_eq!((id, flags), (primary, RasterFlags::empty()), "{c:?}");
            assert!(!font_covers(&chain.font(id), c), "{c:?} is not in Noto Sans Mono");
        }
    }

    #[test]
    fn glyph_id_zero_is_not_coverage() {
        let mut chain = bundled_chain();
        let font = chain.font(chain.primary());
        // format-4 delta segment: ttf-parser answers Some(GlyphId(0)) for U+FFFF
        assert_eq!(font.rustybuzz().glyph_index('\u{FFFF}'), Some(ttf_parser::GlyphId(0)));
        assert!(!font_covers(&font, '\u{FFFF}'));
        assert!(!face_covers(chain.db(), chain.primary(), '\u{FFFF}'));
        assert!(face_covers(chain.db(), chain.primary(), 'A'));
    }

    #[test]
    fn missing_bold_italic_faces_are_synthesised() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        assert_eq!(chain.resolve('A', &key("", true, false)), (primary, RasterFlags::BOLD_SYNTH));
        assert_eq!(chain.resolve('A', &key("", false, true)), (primary, RasterFlags::ITALIC_SYNTH));
        assert_eq!(
            chain.resolve('A', &key("", true, true)),
            (primary, RasterFlags::BOLD_SYNTH | RasterFlags::ITALIC_SYNTH)
        );
        // an uncovered character still reports the requested synthesis
        assert_eq!(chain.resolve('あ', &key("", true, false)), (primary, RasterFlags::BOLD_SYNTH));
    }

    #[test]
    fn unknown_setfont_family_uses_default_chain() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        assert_eq!(
            chain.resolve('A', &key("Nope Sans", false, false)),
            (primary, RasterFlags::empty())
        );
        assert_eq!(
            chain.resolve('A', &key("Nope Sans", true, false)),
            (primary, RasterFlags::BOLD_SYNTH)
        );
        // a SETFONT family that exists is honoured (here it is the primary itself)
        assert_eq!(
            chain.resolve('A', &key("noto sans mono", false, false)),
            (primary, RasterFlags::empty())
        );
    }

    #[test]
    fn resolve_is_cached_per_char_and_style() {
        let mut chain = bundled_chain();
        assert_eq!(chain.cache_len(), 0);
        chain.resolve('A', &StyleKey::plain());
        chain.resolve('A', &StyleKey::plain());
        assert_eq!(chain.cache_len(), 1);
        chain.resolve('A', &key("", true, false));
        chain.resolve('B', &StyleKey::plain());
        assert_eq!(chain.cache_len(), 3);
    }

    #[test]
    fn font_returns_the_requested_face() {
        let mut chain = bundled_chain();
        let primary = chain.primary();
        let font = chain.font(primary);
        assert_eq!(font.id(), primary);
        assert_eq!(font.rustybuzz().units_per_em(), 1000);
    }

    #[test]
    fn style_key_from_text_style_ignores_colour_underline_strike() {
        let style = TextStyle {
            color: Color([1, 2, 3]),
            font_family: SmolStr::new("MS Gothic"),
            font_style: FontStyle::BOLD | FontStyle::UNDERLINE | FontStyle::STRIKELINE,
        };
        assert_eq!(StyleKey::from(&style), key("MS Gothic", true, false));
        let italic = TextStyle {
            color: Color([9, 9, 9]),
            font_family: SmolStr::default(),
            font_style: FontStyle::ITALIC,
        };
        assert_eq!(StyleKey::from(&italic), key("", false, true));
        assert_eq!(StyleKey::plain(), key("", false, false));
    }

    #[test]
    fn language_candidates_start_with_the_emuera_defaults() {
        assert_eq!(language_candidates(Language::Japanese)[0], "MS Gothic");
        assert_eq!(language_candidates(Language::Korean)[0], "D2Coding");
        assert_eq!(language_candidates(Language::ChineseHans)[0], "NSimSun");
        assert_eq!(language_candidates(Language::ChineseHant)[0], "NSimSun");
        assert!(language_candidates(Language::Korean).contains(&"GulimChe"));
        assert!(language_candidates(Language::Japanese).contains(&"Noto Sans Mono CJK JP"));
    }

    #[test]
    fn family_eq_is_unicode_case_insensitive() {
        assert!(family_eq("MS Gothic", "ms gothic"));
        assert!(family_eq("ＭＳ ゴシック", "ＭＳ ゴシック"));
        assert!(family_eq("Sarasa Mono K", "SARASA MONO K"));
        assert!(!family_eq("MS Gothic", "MS PGothic"));
        assert!(!family_eq("MS Gothic", "MS Gothic "));
    }

    #[test]
    fn style_score_counts_matches_and_rejects_unrequested_styles() {
        let regular = face_info(400, fontdb::Style::Normal);
        let bold = face_info(700, fontdb::Style::Normal);
        let semibold = face_info(600, fontdb::Style::Normal);
        let medium = face_info(500, fontdb::Style::Normal);
        let italic = face_info(400, fontdb::Style::Italic);
        let oblique = face_info(400, fontdb::Style::Oblique);
        let bold_italic = face_info(700, fontdb::Style::Italic);
        let b = key("", true, false);
        let i = key("", false, true);
        let bi = key("", true, true);
        assert_eq!(style_score(&regular, &b), Some(0));
        assert_eq!(style_score(&bold, &b), Some(1));
        assert_eq!(style_score(&semibold, &b), Some(1));
        assert_eq!(style_score(&medium, &b), Some(0), "500 is not bold");
        assert_eq!(style_score(&italic, &b), None, "italic not requested");
        assert_eq!(style_score(&bold_italic, &b), None);
        assert_eq!(style_score(&italic, &i), Some(1));
        assert_eq!(style_score(&oblique, &i), Some(1));
        assert_eq!(style_score(&bold, &i), None);
        assert_eq!(style_score(&bold_italic, &bi), Some(2));
        assert_eq!(style_score(&bold, &bi), Some(1));
        assert_eq!(style_score(&italic, &bi), Some(1));
        assert_eq!(style_score(&regular, &bi), Some(0));
    }

    #[test]
    fn load_dir_is_recursive_sorted_and_extension_filtered() {
        let dir = scratch("load-dir");
        std::fs::create_dir_all(dir.join("sub")).unwrap();
        std::fs::write(dir.join("b.TTF"), BUNDLED_FONT).unwrap();
        std::fs::write(dir.join("sub").join("a.otf"), BUNDLED_FONT).unwrap();
        std::fs::write(dir.join("readme.txt"), b"not a font").unwrap();
        std::fs::write(dir.join("broken.ttf"), b"garbage").unwrap();
        let mut db = fontdb::Database::new();
        let ids = load_dir(&mut db, &dir);
        let names: Vec<String> = ids
            .iter()
            .map(|id| {
                let path = source_path(&db, *id);
                path.strip_prefix(&dir).unwrap().to_string_lossy().into_owned()
            })
            .collect();
        assert_eq!(names, vec!["b.TTF", "sub/a.otf"]);
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn from_files_empty_falls_back_to_bundled() {
        let mut chain = FontChain::from_files(&[], Language::Korean);
        let primary = chain.primary();
        assert_eq!(chain.db().face(primary).unwrap().families[0].0, "Noto Sans Mono");
        assert_eq!(chain.resolve('A', &StyleKey::plain()), (primary, RasterFlags::empty()));
    }

    /// `<game>/font/` faces precede the language list, so a game-shipped font
    /// is the primary even with system fonts loaded.
    #[test]
    fn game_font_dir_face_becomes_primary() {
        let game_dir = scratch("game-dir");
        std::fs::create_dir_all(game_dir.join("font")).unwrap();
        std::fs::write(game_dir.join("font").join("zz.ttf"), BUNDLED_FONT).unwrap();
        let mut chain = FontChain::new(&FontConfig {
            family: "",
            game_dir: &game_dir,
            extra_dir: None,
            lang: Language::Korean,
        });
        let primary = chain.primary();
        assert_eq!(source_path(chain.db(), primary), game_dir.join("font").join("zz.ttf"));
        assert_eq!(chain.resolve('A', &StyleKey::plain()), (primary, RasterFlags::empty()));
        std::fs::remove_dir_all(&game_dir).unwrap();
    }

    #[test]
    fn extra_dir_precedes_language_list_and_configured_family_wins() {
        let extra = scratch("extra-dir");
        std::fs::write(extra.join("extra.ttf"), BUNDLED_FONT).unwrap();
        let game_dir = scratch("extra-dir-game");
        let chain = FontChain::new(&FontConfig {
            family: "",
            game_dir: &game_dir,
            extra_dir: Some(extra.clone()),
            lang: Language::Japanese,
        });
        assert_eq!(source_path(chain.db(), chain.primary()), extra.join("extra.ttf"));
        // a configured family that is present outranks the directories
        let chain = FontChain::new(&FontConfig {
            family: "noto sans mono",
            game_dir: &game_dir,
            extra_dir: Some(extra.clone()),
            lang: Language::Japanese,
        });
        let info = chain.db().face(chain.primary()).unwrap();
        assert_eq!(info.families[0].0, "Noto Sans Mono");
        std::fs::remove_dir_all(&extra).unwrap();
        std::fs::remove_dir_all(&game_dir).unwrap();
    }

    /// Needs an installed family with upright regular and bold faces (DejaVu
    /// Sans Mono, Liberation Mono, …); prints SKIP otherwise.
    #[test]
    fn real_bold_face_is_preferred_over_synthesis() {
        let game_dir = scratch("real-bold");
        let mut chain = FontChain::new(&FontConfig {
            family: "",
            game_dir: &game_dir,
            extra_dir: None,
            lang: Language::Korean,
        });
        // lower-cased first family name → (upright regular ids, upright bold ids)
        let mut by_family: std::collections::BTreeMap<String, (Vec<fontdb::ID>, Vec<fontdb::ID>)> =
            Default::default();
        for f in chain.db().faces() {
            let Some((name, _)) = f.families.first() else { continue };
            if is_italic(f) {
                continue;
            }
            let entry = by_family.entry(name.to_lowercase()).or_default();
            if f.weight == fontdb::Weight::NORMAL {
                entry.0.push(f.id);
            } else if is_bold(f) {
                entry.1.push(f.id);
            }
        }
        let pick = by_family.iter().find_map(|(name, (regular, bold))| {
            let r = regular.iter().copied().find(|&id| face_covers(chain.db(), id, 'A'))?;
            let b = bold.iter().copied().find(|&id| face_covers(chain.db(), id, 'A'))?;
            Some((name.clone(), r, b))
        });
        let Some((name, _regular, _bold)) = pick else {
            eprintln!(
                "SKIP real_bold_face_is_preferred_over_synthesis: \
                 no installed family has upright regular + bold faces"
            );
            std::fs::remove_dir_all(&game_dir).unwrap();
            return;
        };
        let (plain_id, plain_flags) = chain.resolve('A', &key(&name, false, false));
        let plain = chain.db().face(plain_id).unwrap();
        assert!(!is_bold(plain) && !is_italic(plain), "{name}: SETFONT regular");
        assert_eq!(plain_flags, RasterFlags::empty());

        let (bold_id, bold_flags) = chain.resolve('A', &key(&name, true, false));
        let bold = chain.db().face(bold_id).unwrap();
        assert!(bold.families.iter().any(|(n, _)| family_eq(n, &name)), "{name}: same family");
        assert!(is_bold(bold) && !is_italic(bold), "{name}: real upright bold face");
        assert_eq!(bold_flags, RasterFlags::empty(), "{name}: nothing synthesised");
        assert_ne!(bold_id, plain_id);

        // bold + italic: a bold face is used even when no bold-italic exists;
        // only the missing posture is synthesised.
        let (bi_id, bi_flags) = chain.resolve('A', &key(&name, true, true));
        let bi = chain.db().face(bi_id).unwrap();
        assert!(is_bold(bi), "{name}: bold face for bold+italic");
        assert!(!bi_flags.contains(RasterFlags::BOLD_SYNTH));
        assert_eq!(bi_flags.contains(RasterFlags::ITALIC_SYNTH), !is_italic(bi));
        std::fs::remove_dir_all(&game_dir).unwrap();
    }

    #[test]
    fn msgothic_both_family_names_match() {
        let Some(ms) = msgothic("msgothic_both_family_names_match") else { return };
        let mut chain = FontChain::from_files(&[ms, bundled_font_path()], Language::Japanese);
        let primary = chain.primary();
        let info = chain.db().face(primary).unwrap();
        assert_eq!(info.index, 0, "face 0 of msgothic.ttc is MS Gothic");
        assert!(info.monospaced);
        let names: Vec<&str> = info.families.iter().map(|f| f.0.as_str()).collect();
        assert!(names.contains(&"MS Gothic") && names.contains(&"ＭＳ ゴシック"), "{names:?}");
        assert_eq!(chain.find_family("MS Gothic"), Some(primary));
        assert_eq!(chain.find_family("ＭＳ ゴシック"), Some(primary));
        assert_eq!(chain.find_family("ms gothic"), Some(primary));
        assert_ne!(chain.find_family("MS PGothic"), Some(primary));
        for c in ['A', 'あ', '漢', '─', '═', '║', '░'] {
            assert_eq!(
                chain.resolve(c, &StyleKey::plain()),
                (primary, RasterFlags::empty()),
                "{c:?}"
            );
        }
        // nothing in this database covers Hangul: primary .notdef
        let (id, _) = chain.resolve('한', &StyleKey::plain());
        assert_eq!(id, primary);
        assert!(!font_covers(&chain.font(id), '한'));
        assert_eq!(chain.resolve('あ', &key("", true, false)), (primary, RasterFlags::BOLD_SYNTH));
    }

    /// With system fonts: あ stays on MS Gothic, 한 goes to whatever face
    /// covers it (a chain CJK font or the database-wide fallback), never MS Gothic.
    #[test]
    fn msgothic_resolves_kana_and_hangul_falls_elsewhere() {
        let Some(ms) = msgothic("msgothic_resolves_kana_and_hangul_falls_elsewhere") else {
            return;
        };
        let game_dir = scratch("msgothic-game");
        std::fs::create_dir_all(game_dir.join("font")).unwrap();
        std::fs::copy(&ms, game_dir.join("font").join("msgothic.ttc")).unwrap();
        let mut chain = FontChain::new(&FontConfig {
            family: "",
            game_dir: &game_dir,
            extra_dir: None,
            lang: Language::Japanese,
        });
        let ms_id = chain.find_family("MS Gothic").expect("game font dir loaded");
        assert_eq!(chain.primary(), ms_id);
        for c in ['A', 'あ', '─', '═'] {
            assert_eq!(
                chain.resolve(c, &StyleKey::plain()),
                (ms_id, RasterFlags::empty()),
                "{c:?}"
            );
        }
        let (id, flags) = chain.resolve('한', &StyleKey::plain());
        let ids: Vec<fontdb::ID> = chain.db().faces().map(|f| f.id).collect();
        let any_hangul = ids.iter().any(|&id| face_covers(chain.db(), id, '한'));
        if any_hangul {
            assert_ne!(id, ms_id, "Hangul must not be drawn with MS Gothic's .notdef");
            assert!(font_covers(&chain.font(id), '한'));
            assert_eq!(flags, RasterFlags::empty());
        } else {
            assert_eq!(id, ms_id);
        }
        std::fs::remove_dir_all(&game_dir).unwrap();
    }
}
