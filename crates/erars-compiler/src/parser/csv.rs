// pub struct CsvParser<'s> {
//     s: &'s str,
// }

// impl<'s> CsvParser<'s> {
//     pub fn new(s: &'s str) -> Self {
//         Self { s }
//     }

//     pub fn next_name(&mut self) -> &'s str {
//         let (name, _) = self.s.split_once(',').unwrap();
//         self.s = &self.s[name.len() + 1..];

//         name.trim()
//     }

//     pub fn next_number(&mut self) -> Option<i32> {
//         let (number, _) = self.s.split_once(',').unwrap();
//         self.s = &self.s[number.len() + 1..];

//         number.trim().parse().ok()
//     }
// }

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
