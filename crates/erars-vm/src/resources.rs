//! The `resources/` startup loader — Emuera `AppContents.LoadContents`
//! (`Content/AppContents.cs:96-150`) and `AppContents.CreateFromCsv`
//! (`:180-315`).
//!
//! Every CSV under `resources/` declares sprites over parent image files. The
//! loader is deliberately forgiving: a missing directory, an unreadable file,
//! a malformed row and a missing image are all *warnings*, and one bad row
//! never stops the rest. Emuera goes further and wraps the whole walk in
//! `catch { return false; }` (`:143-147`), which is why nothing here returns
//! an error either.
//!
//! Two deliberate divergences, both forced by the host:
//!
//! * **Paths are resolved case-insensitively.** Emuera upper-cases the CSV's
//!   own directory and the image name (`:112`, `:184-185`) and hands the result
//!   to `File.Exists`, which is case-insensitive on Windows. On Linux that
//!   upper-cased path matches nothing, so [`resolve_path`] walks the components
//!   and compares case-insensitively. Emuera's key is the upper-cased string,
//!   so the dedup key here is the *resolved* path, which is the same identity
//!   relation with a different spelling.
//! * **Parent bitmaps live above `i32::MAX`.** Emuera keeps them in
//!   `resourceDic`, keyed by path, entirely separate from `GCREATE`'s
//!   `Dictionary<int, GraphicsImage>` (`AppContents.cs:16-20`). erars has one
//!   `id -> bitmap` map, so resource ids are allocated downward from
//!   `u32::MAX`; `graphics_id` caps every script-supplied id at `i32::MAX`
//!   (`terminal_vm/executor.rs:902-910`, matching Emuera's `int` key), so the
//!   two ranges provably cannot meet.

use crate::graphics::{GraphicsStore, Rect};
use std::path::{Path, PathBuf};

/// One `ParserMediator.Warn` from the resource walk. Non-fatal by
/// construction: the caller reports it and carries on.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResourceWarning {
    /// The CSV's file name, as Emuera's `ScriptPosition` carries it
    /// (`AppContents.cs:113`, `:125`).
    pub file: String,
    /// 1-based line number within that CSV.
    pub line: usize,
    pub message: String,
}

impl std::fmt::Display for ResourceWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.file, self.line, self.message)
    }
}

/// Load every `resources/**/*.csv`, in path order.
///
/// A missing `resources/` directory is success with nothing to do
/// (`AppContents.cs:98-99`). The return value is the warning list; sprites and
/// parent bitmaps are installed into `graphics`.
pub fn load(
    graphics: &mut GraphicsStore,
    content_dir: &Path,
    encoding: &'static encoding_rs::Encoding,
) -> Vec<ResourceWarning> {
    let mut warnings = Vec::new();
    if !content_dir.is_dir() {
        return warnings;
    }

    let mut csvs = Vec::new();
    collect_csvs(content_dir, &mut csvs);
    // `Directory.GetFiles` order is whatever the filesystem hands back; erars
    // sorts so a duplicate-name warning (first definition wins) is
    // reproducible, the same reason the ERB walk is sorted.
    csvs.sort();

    for path in &csvs {
        load_one(graphics, path, encoding, &mut warnings);
    }

    warnings
}

/// `Directory.GetFiles(dir, "*.csv", SearchOption.AllDirectories)` plus the
/// re-check at `AppContents.cs:107-108`, which exists because the Windows
/// `*.csv` glob also matches `.csvx`.
fn collect_csvs(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_csvs(&path, out);
        } else if path
            .extension()
            .is_some_and(|e| e.eq_ignore_ascii_case("csv"))
        {
            out.push(path);
        }
    }
}

fn load_one(
    graphics: &mut GraphicsStore,
    path: &Path,
    encoding: &'static encoding_rs::Encoding,
    warnings: &mut Vec<ResourceWarning>,
) {
    let Ok(bytes) = std::fs::read(path) else {
        // Inside Emuera's `catch`, which abandons the whole walk; erars only
        // abandons this file.
        return;
    };
    // `File.ReadAllLines(filepath, Config.Encode)` — the game's own encoding.
    let (text, _, _) = encoding.decode(&bytes);

    let file = path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let dir = path.parent().unwrap_or(Path::new("."));

    // Frame-append mode: the name of the open declaration, and whether that
    // declaration actually made it into the dictionary. Reset per file, as the
    // local at `AppContents.cs:110`.
    //
    // The `live` half exists because Emuera assigns `currentAnime` *before* the
    // duplicate-name check (`:118` then `:130-140`): a second `ANIME` row for
    // an existing name is disposed and yet still becomes the open declaration,
    // so its frame rows are appended to a dead object — no effect, no warning.
    // erars keeps sprites by name, so appending would hit the *first*
    // declaration; the flag reproduces the dead-object behaviour instead.
    let mut current_anime: Option<(String, bool)> = None;

    for (index, line) in text.lines().enumerate() {
        let line_no = index + 1;
        let mut warn = |message: String| {
            warnings.push(ResourceWarning {
                file: file.clone(),
                line: line_no,
                message,
            })
        };

        let str = line.trim();
        if str.is_empty() || str.starts_with(';') {
            continue;
        }
        let tokens: Vec<&str> = str.split(',').collect();

        let open = current_anime
            .as_ref()
            .map(|(name, live)| (name.as_str(), *live));

        match create_from_csv(graphics, &tokens, dir, open, &mut warn) {
            Row::Nothing => {}
            // A frame row consumed by the open declaration leaves it open
            // (`:299-307` returns null without clearing `currentAnime`).
            Row::Frame => {}
            Row::Sprite { name, installed } => {
                // `item as SpriteAnime` is null for a plain sprite, which ends
                // frame-append mode (`:118`).
                current_anime = None;
                if !installed {
                    warn(format!("同名のリソースがすでに作成されています:{name}"));
                }
            }
            Row::Anime { name, installed } => {
                current_anime = Some((name.clone(), installed));
                if !installed {
                    warn(format!("同名のリソースがすでに作成されています:{name}"));
                }
            }
        }
    }
}

/// What one CSV row produced.
enum Row {
    /// Nothing at all — a silently ignored row, or a warned-about one.
    Nothing,
    /// A frame was appended to the open animation, or swallowed by a dead one.
    Frame,
    /// A plain sprite row. `installed` is false when the name was taken, which
    /// is Emuera's `resourceImageDictionary.ContainsKey` branch (`:130-140`).
    Sprite { name: String, installed: bool },
    /// An animation declaration row.
    Anime { name: String, installed: bool },
}

/// `AppContents.CreateFromCsv` (`Content/AppContents.cs:180-315`).
fn create_from_csv(
    graphics: &mut GraphicsStore,
    tokens: &[&str],
    dir: &Path,
    current_anime: Option<(&str, bool)>,
    warn: &mut impl FnMut(String),
) -> Row {
    // Under two columns is not a row at all, and says nothing (`:182-183`).
    if tokens.len() < 2 {
        return Row::Nothing;
    }
    let name = upper(tokens[0].trim());
    // DELIBERATE-looking but faithful: arg2 is *not* trimmed, matching `:185`
    // — only `tokens[0]` gets a `.Trim()`. A trailing space therefore becomes
    // part of the file name and the row warns about a missing file. §5.10.
    let arg2 = upper(tokens[1]);
    if name.is_empty() || arg2.is_empty() {
        return Row::Nothing;
    }

    if arg2 == "ANIME" {
        if tokens.len() < 4 {
            warn("アニメーションスプライトのサイズが宣言されていません".into());
            return Row::Nothing;
        }
        let (Some(w), Some(h)) = (parse_int(tokens[2]), parse_int(tokens[3])) else {
            warn("アニメーションスプライトのサイズの指定が適切ではありません".into());
            return Row::Nothing;
        };
        if w <= 0 || h <= 0 || w > crate::MAX_IMAGE_SIZE as i32 || h > crate::MAX_IMAGE_SIZE as i32
        {
            warn("アニメーションスプライトのサイズの指定が適切ではありません".into());
            return Row::Nothing;
        }
        let installed = graphics.sprite_anime_create(name.clone(), w as u32, h as u32);
        return Row::Anime { name, installed };
    }

    // `arg2.IndexOf('.') < 0` (`:212-216`): the extension is how a frame row
    // is told apart from a declaration, not a format check.
    if !arg2.contains('.') {
        warn(format!("第二引数に拡張子がありません:{arg2}"));
        return Row::Nothing;
    }

    let gid = match graphics.resource_bitmap(dir, &arg2) {
        Ok(gid) => gid,
        Err(ResourceImageError::NotFound) => {
            warn(format!("指定された画像ファイルが見つかりませんでした:{arg2}"));
            return Row::Nothing;
        }
        Err(ResourceImageError::Undecodable) => {
            warn(format!("指定されたファイルの読み込みに失敗しました:{arg2}"));
            return Row::Nothing;
        }
        Err(ResourceImageError::TooLarge(gid)) => {
            // Warned about and then used anyway: a shipped game already had an
            // oversize variant (`:236-243`).
            warn(format!(
                "指定された画像ファイルの大きさが大きすぎます(幅及び高さを{}px以下にすることを強く推奨します):{arg2}",
                crate::MAX_IMAGE_SIZE
            ));
            gid
        }
    };

    let (parent_w, parent_h) = (graphics.width(gid), graphics.height(gid));
    let mut rect = Rect::new(0, 0, parent_w as i32, parent_h as i32);
    let mut pos = (0i32, 0i32);
    let mut delay = 1000i64;

    // `name, parentname, x, y, w, h, offset_x, offset_y, delay`.
    if tokens.len() >= 6 {
        let xywh = [
            parse_int(tokens[2]),
            parse_int(tokens[3]),
            parse_int(tokens[4]),
            parse_int(tokens[5]),
        ];
        // DELIBERATE-looking but faithful: if any of the four fails to parse,
        // Emuera keeps the whole-parent default *silently* (`:269`, the `if
        // (sccs)` with no else). A typo in a rect therefore yields the full
        // image, not a diagnostic.
        if let [Some(x), Some(y), Some(w), Some(h)] = xywh {
            rect = Rect::new(x, y, w, h);
            if w <= 0 || h <= 0 {
                warn(format!("スプライトの高さ又は幅には正の値のみ指定できます:{name}"));
                return Row::Nothing;
            }
            if !rect.intersects_size(parent_w, parent_h) {
                warn(format!("親画像の範囲外を参照しています:{name}"));
                return Row::Nothing;
            }
        }

        if tokens.len() >= 8 {
            if let (Some(x), Some(y)) = (parse_int(tokens[6]), parse_int(tokens[7])) {
                pos = (x, y);
            }
            if tokens.len() >= 9 {
                // An unparsable delay silently keeps 1000 (`:290-297`).
                if let Some(parsed) = parse_int(tokens[8]) {
                    if parsed <= 0 {
                        warn(format!("フレーム表示時間には正の値のみ指定できます:{name}"));
                        return Row::Nothing;
                    }
                    delay = parsed as i64;
                }
            }
        }
    }

    // A row whose name matches the open declaration is a frame, not a sprite
    // (`:299-307`).
    if let Some((open, live)) = current_anime {
        if open == name {
            // A dead declaration swallows its frames silently, as appending to
            // a disposed `SpriteAnime` does.
            if live && !graphics.sprite_anime_add_frame(&name, gid, rect, pos.0, pos.1, delay) {
                warn(format!("アニメーションスプライトのフレームの追加に失敗しました:{arg2}"));
                return Row::Nothing;
            }
            return Row::Frame;
        }
    }

    let installed = graphics.sprite_create_at(name.clone(), gid, rect, pos.0, pos.1);
    Row::Sprite { name, installed }
}

/// Why a parent image could not be used.
pub enum ResourceImageError {
    NotFound,
    Undecodable,
    /// Decoded, installed, and over `MAX_IMAGE_SIZE` on an axis. Emuera warns
    /// and uses it anyway, so the id comes back with the error.
    TooLarge(u32),
}

/// `tokens[0].Trim().ToUpper()` / `tokens[1].ToUpper()`.
fn upper(s: &str) -> String {
    if s.is_ascii() {
        s.to_ascii_uppercase()
    } else {
        s.to_uppercase()
    }
}

/// `int.TryParse(s, out _)` with the default `NumberStyles.Integer`: leading
/// and trailing whitespace and a leading sign are allowed, nothing else. This
/// is why the CSV's un-trimmed numeric columns still parse.
fn parse_int(s: &str) -> Option<i32> {
    s.trim().parse::<i32>().ok()
}

/// Resolve `name`, written with Windows separators against `dir`, on a
/// case-sensitive filesystem.
///
/// Emuera upper-cases both halves and relies on Windows to match; here each
/// component is compared case-insensitively against the real directory
/// entries. An exact hit short-circuits, so the common case costs one `stat`.
pub fn resolve_path(dir: &Path, name: &str) -> Option<PathBuf> {
    let direct = dir.join(name.replace('\\', "/"));
    if direct.is_file() {
        return Some(direct);
    }

    let mut at = dir.to_path_buf();
    let components: Vec<&str> = name
        .split(['\\', '/'])
        .filter(|c| !c.is_empty() && *c != ".")
        .collect();
    let last = components.len().checked_sub(1)?;

    for (i, component) in components.into_iter().enumerate() {
        if component == ".." {
            if !at.pop() {
                return None;
            }
            continue;
        }
        let entries = std::fs::read_dir(&at).ok()?;
        let found = entries.flatten().find(|e| {
            e.file_name()
                .to_str()
                .is_some_and(|n| n.eq_ignore_ascii_case(component))
        })?;
        at = found.path();
        if i == last {
            return at.is_file().then_some(at);
        }
        if !at.is_dir() {
            return None;
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graphics::GraphicsStore;

    /// A 4x2 PNG whose four left pixels are red and four right pixels blue,
    /// written to `dir/name`.
    fn write_png(dir: &Path, name: &str, w: u32, h: u32) -> PathBuf {
        let path = dir.join(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        let mut img = image::RgbaImage::new(w, h);
        for (x, _y, px) in img.enumerate_pixels_mut() {
            *px = if x < w / 2 {
                image::Rgba([255, 0, 0, 255])
            } else {
                image::Rgba([0, 0, 255, 255])
            };
        }
        img.save(&path).unwrap();
        path
    }

    fn tmp(tag: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("erars-res-{tag}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn load_utf8(dir: &Path) -> (GraphicsStore, Vec<ResourceWarning>) {
        let mut g = GraphicsStore::default();
        let w = load(&mut g, dir, encoding_rs::UTF_8);
        (g, w)
    }

    /// The parent bitmap of a still sprite.
    fn parent_of(g: &GraphicsStore, name: &str) -> u32 {
        match g.sprite(name).expect("sprite").sampler {
            erars_ui::image::ImageSampler::Single { bitmap, .. } => bitmap,
            erars_ui::image::ImageSampler::Anime { .. } => panic!("{name} is animated"),
        }
    }

    fn frame_count(g: &GraphicsStore, name: &str) -> usize {
        match &g.sprite(name).expect("sprite").sampler {
            erars_ui::image::ImageSampler::Anime { frames, .. } => frames.len(),
            erars_ui::image::ImageSampler::Single { .. } => panic!("{name} is not animated"),
        }
    }

    #[test]
    fn missing_directory_is_not_an_error() {
        let (g, warnings) = load_utf8(Path::new("/nonexistent/erars/resources"));
        assert!(warnings.is_empty());
        assert!(g.sprite("ANY").is_none());
    }

    #[test]
    fn whole_parent_is_the_default_rect() {
        let dir = tmp("whole");
        write_png(&dir, "pic.png", 40, 20);
        std::fs::write(dir.join("a.csv"), "FACE,pic.png\n").unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings, vec![]);
        let s = g.sprite("FACE").expect("sprite registered");
        assert_eq!((s.width, s.height), (40, 20));
    }

    #[test]
    fn name_is_upper_cased_and_lookup_is_case_insensitive() {
        let dir = tmp("upper");
        write_png(&dir, "pic.png", 8, 8);
        std::fs::write(dir.join("a.csv"), " face , pic.png\n").unwrap();

        let (g, warnings) = load_utf8(&dir);
        // arg2 keeps its leading space, so the file is not found: `:185` does
        // not trim the second column.
        assert_eq!(warnings.len(), 1, "{warnings:?}");
        assert!(warnings[0].message.contains("見つかりませんでした"));
        assert!(g.sprite("FACE").is_none());

        std::fs::write(dir.join("a.csv"), "face,pic.png\n").unwrap();
        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings, vec![]);
        assert!(g.sprite("face").is_some(), "lookup folds case");
        assert!(g.sprite("FACE").is_some());
    }

    #[test]
    fn comments_blanks_and_short_rows_are_silent() {
        let dir = tmp("silent");
        write_png(&dir, "pic.png", 8, 8);
        std::fs::write(
            dir.join("a.csv"),
            "; a comment\n\n   \nONLYONECOLUMN\n,pic.png\nGOOD,pic.png\n",
        )
        .unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings, vec![], "none of those rows may warn");
        assert!(g.sprite("GOOD").is_some());
    }

    #[test]
    fn missing_extension_and_missing_file_warn_with_their_line() {
        let dir = tmp("warn");
        std::fs::write(dir.join("a.csv"), "A,noext\nB,gone.png\n").unwrap();

        let (_, warnings) = load_utf8(&dir);
        assert_eq!(warnings.len(), 2);
        assert_eq!((warnings[0].line, warnings[1].line), (1, 2));
        assert!(warnings[0].message.contains("第二引数に拡張子がありません"));
        assert!(warnings[1].message.contains("見つかりませんでした"));
        assert_eq!(warnings[0].file, "a.csv");
    }

    #[test]
    fn rect_and_offset_columns() {
        let dir = tmp("rect");
        write_png(&dir, "pic.png", 40, 20);
        std::fs::write(
            dir.join("a.csv"),
            "CROP,pic.png,4,2,10,6\nOFF,pic.png,0,0,10,6,3,5\n",
        )
        .unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings, vec![]);
        let crop = g.sprite("CROP").unwrap();
        assert_eq!((crop.width, crop.height), (10, 6));
        let off = g.sprite("OFF").unwrap();
        assert_eq!((off.pos_x, off.pos_y), (3, 5));
    }

    #[test]
    fn unparsable_rect_silently_keeps_the_whole_parent() {
        let dir = tmp("badrect");
        write_png(&dir, "pic.png", 40, 20);
        // `int.TryParse` fails on `x`, so `if (sccs)` is skipped and the
        // default rect stands, with no warning at all (`:266-282`).
        std::fs::write(dir.join("a.csv"), "FULL,pic.png,4,2,x,6\n").unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings, vec![]);
        let s = g.sprite("FULL").unwrap();
        assert_eq!((s.width, s.height), (40, 20));
    }

    #[test]
    fn negative_size_and_out_of_range_rect_warn() {
        let dir = tmp("badsize");
        write_png(&dir, "pic.png", 40, 20);
        std::fs::write(
            dir.join("a.csv"),
            "NEG,pic.png,0,0,-4,6\nOOR,pic.png,100,100,4,4\n",
        )
        .unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings.len(), 2, "{warnings:?}");
        assert!(warnings[0].message.contains("正の値のみ"));
        assert!(warnings[1].message.contains("親画像の範囲外"));
        assert!(g.sprite("NEG").is_none());
        assert!(g.sprite("OOR").is_none());
    }

    #[test]
    fn duplicate_name_keeps_the_first() {
        let dir = tmp("dup");
        write_png(&dir, "pic.png", 40, 20);
        std::fs::write(
            dir.join("a.csv"),
            "SAME,pic.png,0,0,10,6\nSAME,pic.png,0,0,20,12\n",
        )
        .unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("同名のリソースが"));
        let s = g.sprite("SAME").unwrap();
        assert_eq!((s.width, s.height), (10, 6), "first definition wins");
    }

    #[test]
    fn anime_declaration_then_frames() {
        let dir = tmp("anime");
        write_png(&dir, "pic.png", 40, 20);
        std::fs::write(
            dir.join("a.csv"),
            "WALK,ANIME,16,16\nWALK,pic.png,0,0,16,16,0,0,125\nWALK,pic.png,16,0,16,16,0,0,125\nSTILL,pic.png\nWALK,pic.png,0,0,16,16,0,0,125\n",
        )
        .unwrap();

        let (g, warnings) = load_utf8(&dir);
        // The last row is not a frame: `STILL` closed frame-append mode, so it
        // is a *new sprite* named WALK, which already exists -> one warning.
        assert_eq!(warnings.len(), 1, "{warnings:?}");
        assert!(warnings[0].message.contains("同名のリソースが"));
        assert_eq!(warnings[0].line, 5);

        let walk = g.sprite("WALK").unwrap();
        assert_eq!((walk.width, walk.height), (16, 16));
        assert_eq!(frame_count(&g, "WALK"), 2, "two frames appended");
        assert!(g.sprite("STILL").is_some());
    }

    #[test]
    fn anime_without_a_size_warns() {
        let dir = tmp("animebad");
        std::fs::write(dir.join("a.csv"), "A,ANIME\nB,ANIME,0,16\n").unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings.len(), 2);
        assert!(warnings[0].message.contains("宣言されていません"));
        assert!(warnings[1].message.contains("適切ではありません"));
        assert!(g.sprite("A").is_none());
        assert!(g.sprite("B").is_none());
    }

    #[test]
    fn frame_delay_must_be_positive() {
        let dir = tmp("delay");
        write_png(&dir, "pic.png", 40, 20);
        std::fs::write(
            dir.join("a.csv"),
            "W,ANIME,16,16\nW,pic.png,0,0,16,16,0,0,0\n",
        )
        .unwrap();

        let (_, warnings) = load_utf8(&dir);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("フレーム表示時間"));
    }

    #[test]
    fn nested_directories_and_case_folded_paths() {
        let dir = tmp("nested");
        write_png(&dir, "Sub/Pic.PNG", 12, 4);
        // Emuera would look for `.../SUB\PIC.PNG`; the loader has to find it.
        std::fs::write(dir.join("Sub/a.csv"), "N,pic.png\n").unwrap();
        std::fs::write(dir.join("b.csv"), "M,Sub\\Pic.PNG\n").unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings, vec![], "both spellings resolve");
        assert!(g.sprite("N").is_some(), "relative to the CSV's directory");
        assert!(g.sprite("M").is_some(), "backslash and mixed case");
    }

    #[test]
    fn one_parent_is_decoded_once_for_many_sprites() {
        let dir = tmp("share");
        write_png(&dir, "pic.png", 40, 20);
        std::fs::write(
            dir.join("a.csv"),
            "A,pic.png,0,0,10,10\nB,pic.png,10,0,10,10\nC,PIC.PNG,20,0,10,10\n",
        )
        .unwrap();

        let (g, warnings) = load_utf8(&dir);
        assert_eq!(warnings, vec![]);
        let ids: Vec<_> = ["A", "B", "C"].iter().map(|n| parent_of(&g, n)).collect();
        assert_eq!(ids[0], ids[1]);
        assert_eq!(ids[1], ids[2], "case-folded path is the same parent");
    }

    #[test]
    fn resource_ids_cannot_collide_with_gcreate() {
        let dir = tmp("ids");
        write_png(&dir, "pic.png", 8, 8);
        std::fs::write(dir.join("a.csv"), "A,pic.png\n").unwrap();

        let (g, _) = load_utf8(&dir);
        let id = parent_of(&g, "A");
        assert!(
            id > i32::MAX as u32,
            "resource parents live above every script-reachable id: {id}"
        );
    }
}
