pub fn lines<'a>(
    s: &'a str,
) -> impl Iterator<Item = (impl Iterator<Item = &'a str>, std::ops::Range<usize>)> {
    let begin = s.as_ptr() as usize;
    s.trim_start_matches("\u{FEFF}").lines().filter_map(move |s| {
        let start = s.as_ptr() as usize - begin;

        let s = match s.split_once([';', '；']) {
            Some((left, _)) => left,
            None => s,
        }
        .trim();

        if s.is_empty() {
            None
        } else {
            let end = start + s.len();

            Some((s.split(',').map(|p| p.trim()), start..end))
        }
    })
}
