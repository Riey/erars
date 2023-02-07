use memmap2::MmapOptions;
use std::path::Path;

/// Read string from path respect BOM
pub fn read_file(path: &Path) -> std::io::Result<String> {
    let file = std::fs::File::open(path)?;
    let src = unsafe { MmapOptions::new().populate().map(&file) }.unwrap();
    let mut src = &*src;

    use unicode_bom::Bom;

    let bom = Bom::from(src);
    src = &src[bom.len()..];
    let encoding = match bom {
        Bom::Null => encoding_rs::SHIFT_JIS,
        Bom::Utf16Be => encoding_rs::UTF_16BE,
        Bom::Utf16Le => encoding_rs::UTF_16LE,
        Bom::Utf8 => unsafe {
            return Ok(std::str::from_utf8_unchecked(src).to_string());
        },
        other => panic!("Unsupported BOM {other}"),
    };

    Ok(encoding.decode_without_bom_handling(src).0.to_string())
}
