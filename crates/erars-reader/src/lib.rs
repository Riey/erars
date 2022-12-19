use std::io::{BufRead, BufReader, Read, Seek, SeekFrom};
use std::path::Path;

/// Read string from path respect BOM
pub fn read_file(path: &Path) -> std::io::Result<String> {
    let mut file = std::fs::File::open(path)?;

    use unicode_bom::Bom;

    let bom = Bom::from(&mut file);
    file.seek(SeekFrom::Start(bom.len() as _))?;
    let encoding = match bom {
        Bom::Null => encoding_rs::SHIFT_JIS,
        Bom::Utf16Be => encoding_rs::UTF_16BE,
        Bom::Utf16Le => encoding_rs::UTF_16LE,
        Bom::Utf8 => {
            let mut out = String::with_capacity(file.metadata()?.len() as usize);
            file.read_to_string(&mut out)?;
            return Ok(out);
        }
        other => panic!("Unsupported BOM {other}"),
    };

    let mut decoder = encoding.new_decoder();

    let len = decoder
        .max_utf8_buffer_length(file.metadata()?.len() as usize)
        .expect("File is too large");

    let mut reader = BufReader::new(file);
    let mut out = String::with_capacity(len);

    loop {
        let src = reader.fill_buf()?;

        if src.is_empty() {
            break;
        }

        let (_ret, size, had_errors) = decoder.decode_to_string(src, &mut out, false);

        if had_errors {
            log::error!("Invalid text detected from {}", path.display());
        }

        reader.consume(size);
    }

    Ok(out)
}
