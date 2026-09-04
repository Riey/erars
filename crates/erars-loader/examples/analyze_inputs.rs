//! What the players actually do: usage statistics over the input logs
//! [`erars_vm::InputLogger`] leaves in a game's `logs/` directory.
//!
//! ```text
//! cargo run --example analyze_inputs -- <inputs_*.jsonl | logs dir> ...
//! ```
//!
//! Every section answers a question about *where the session goes*: which
//! functions ask for input, what the answer nearly always is, and how long the
//! user takes to give it. The closing suggestions are read straight off those
//! numbers — a hot function is one that a measured share of the waits come
//! from, not a guess.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use erars_ast::Value;
use erars_vm::InputLogEntry;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    if args.is_empty() || args.iter().any(|a| a == "-h" || a == "--help") {
        eprintln!("usage: analyze_inputs <inputs_*.jsonl | logs dir> ...");
        std::process::exit(if args.is_empty() { 2 } else { 0 });
    }

    let mut paths = Vec::new();
    for arg in &args {
        match collect(Path::new(arg)) {
            Ok(found) if found.is_empty() => eprintln!("{arg}: holds no input log"),
            Ok(found) => paths.extend(found),
            Err(err) => {
                eprintln!("{arg}: {err}");
                std::process::exit(1);
            }
        }
    }
    paths.sort();
    paths.dedup();

    let mut sessions = Vec::new();
    for path in paths {
        match InputLogEntry::read_log(&path) {
            Ok(entries) if entries.is_empty() => eprintln!("{}: no inputs", path.display()),
            Ok(entries) => sessions.push(Session { path, entries }),
            Err(err) => eprintln!("{}: {err}", path.display()),
        }
    }

    if sessions.is_empty() {
        eprintln!("Nothing to analyze.");
        std::process::exit(1);
    }

    report(&sessions);
}

/// Every log `path` names: the file itself, or the `inputs_*.jsonl` of a
/// directory.
///
/// `last_inputs.jsonl` is not picked up from a directory — it is a link to one
/// of the files already there, and following it would count that session
/// twice. Named on the command line it is read like any other file, which is
/// the point of having it.
fn collect(path: &Path) -> std::io::Result<Vec<PathBuf>> {
    if !path.is_dir() {
        return Ok(vec![path.to_owned()]);
    }

    let mut found = Vec::new();

    for entry in std::fs::read_dir(path)? {
        let entry = entry?.path();
        let name = entry.file_name().and_then(|n| n.to_str()).unwrap_or_default();

        if name.starts_with("inputs_") && name.ends_with(".jsonl") {
            found.push(entry);
        }
    }

    Ok(found)
}

struct Session {
    path: PathBuf,
    entries: Vec<InputLogEntry>,
}

impl Session {
    /// How long the session ran, as its last input saw it. `elapsed_ms` is
    /// monotonic, so this survives a clock change mid-session.
    fn duration_ms(&self) -> u64 {
        self.entries.last().map_or(0, |e| e.elapsed_ms)
    }

    fn name(&self) -> &str {
        self.path.file_name().and_then(|n| n.to_str()).unwrap_or("?")
    }
}

/// Everything one function's waits amount to.
struct FuncStats<'a> {
    function: &'a str,
    latencies: Vec<u64>,
    /// Answer text to how often that function got it.
    answers: HashMap<String, usize>,
    /// How many of the waits carried no value — a keypress or a raw event.
    /// A fast answer means something different there than at a prompt.
    keypress: usize,
}

impl<'a> FuncStats<'a> {
    fn count(&self) -> usize {
        self.latencies.len()
    }

    /// The function mostly waits for a key rather than for a value.
    fn is_keypress(&self) -> bool {
        self.keypress * 2 > self.count()
    }
}

fn report(sessions: &[Session]) {
    let entries: Vec<&InputLogEntry> = sessions.iter().flat_map(|s| s.entries.iter()).collect();
    let total = entries.len();
    let session_ms: u64 = sessions.iter().map(Session::duration_ms).sum();
    let waiting_ms: u64 = entries.iter().map(|e| e.latency_ms).sum();

    let mut latencies: Vec<u64> = entries.iter().map(|e| e.latency_ms).collect();
    latencies.sort_unstable();

    println!("=== Sessions ===");
    for session in sessions {
        println!(
            "  {:<34} {:>6} inputs  over {}",
            session.name(),
            session.entries.len(),
            duration(session.duration_ms())
        );
    }

    println!();
    println!("=== Totals ===");
    println!("  inputs          {total}");
    println!("  session time    {}", duration(session_ms));
    println!(
        "  waiting on user {} ({})",
        duration(waiting_ms),
        share(waiting_ms as usize, session_ms.max(1) as usize)
    );
    if session_ms > 0 {
        println!(
            "  input rate      {:.1}/min",
            total as f64 * 60_000.0 / session_ms as f64
        );
    }

    println!();
    println!("=== By request type ===");
    let mut by_type: HashMap<&str, Vec<u64>> = HashMap::new();
    for entry in &entries {
        by_type
            .entry(entry.request_type.as_str())
            .or_default()
            .push(entry.latency_ms);
    }
    for (ty, mut times) in ranked(by_type, |times: &Vec<u64>| times.len()) {
        times.sort_unstable();
        println!(
            "  {:<14} {:>6}  {:>6}  median {:>8}  {}",
            ty,
            times.len(),
            share(times.len(), total),
            duration(percentile(&times, 50.0)),
            bar(times.len(), total)
        );
    }

    let funcs = func_stats(&entries);

    println!();
    println!("=== Top functions requesting input ===");
    for stats in funcs.iter().take(12) {
        let mut times = stats.latencies.clone();
        times.sort_unstable();
        println!(
            "  {:<28} {:>6}  {:>6}  avg {:>8}  median {:>8}",
            elide(stats.function, 28),
            stats.count(),
            share(stats.count(), total),
            duration(mean(&stats.latencies)),
            duration(percentile(&times, 50.0)),
        );
        if let Some((answer, hits)) = top(&stats.answers) {
            println!(
                "      most often {} ({} of {})",
                elide(answer, 40),
                share(hits, stats.count()),
                stats.count()
            );
        }
    }
    if funcs.len() > 12 {
        println!("  ... and {} more functions", funcs.len() - 12);
    }

    println!();
    println!("=== Most frequent responses ===");
    let mut answers: HashMap<String, usize> = HashMap::new();
    let mut answered_by: HashMap<String, HashMap<&str, usize>> = HashMap::new();
    let mut answered = 0usize;
    for entry in &entries {
        let Some(text) = answer_text(entry) else {
            continue;
        };
        answered += 1;
        *answers.entry(text.clone()).or_default() += 1;
        let by_func = answered_by.entry(text).or_default();
        *by_func.entry(entry.function.as_str()).or_default() += 1;
    }
    if answered == 0 {
        println!("  (no wait carried a value; every input was a keypress)");
    }
    for (text, hits) in ranked(answers, |n| *n).into_iter().take(12) {
        let from = answered_by
            .get(&text)
            .and_then(top)
            .map_or_else(String::new, |(func, n)| {
                format!("  mostly in {} ({n})", elide(func, 24))
            });
        println!(
            "  {:<20} {:>6}  {:>6}{from}",
            elide(&text, 20),
            hits,
            share(hits, answered)
        );
    }

    println!();
    println!("=== Latency (user think time) ===");
    if latencies.is_empty() {
        println!("  (nothing recorded)");
    } else {
        println!("  min     {:>10}", duration(latencies[0]));
        println!("  median  {:>10}", duration(percentile(&latencies, 50.0)));
        println!("  mean    {:>10}", duration(mean(&latencies)));
        println!("  p95     {:>10}", duration(percentile(&latencies, 95.0)));
        println!("  max     {:>10}", duration(latencies[latencies.len() - 1]));
    }

    let follow = follow_ons(sessions);

    println!();
    println!("=== Follow-on functions ===");
    if follow.is_empty() {
        println!("  (no input was ever followed by one from another function)");
    }
    for ((from, to), hits) in follow.iter().take(8) {
        println!(
            "  {:<26} -> {:<26} {:>6}",
            elide(from, 26),
            elide(to, 26),
            hits
        );
    }

    println!();
    println!("=== Suggestions ===");
    let notes = suggestions(total, &funcs, &follow, session_ms, waiting_ms);
    if notes.is_empty() {
        println!("  Nothing stands out yet — a longer log gives more to go on.");
    }
    for note in notes {
        println!("  - {note}");
    }
}

fn func_stats<'a>(entries: &[&'a InputLogEntry]) -> Vec<FuncStats<'a>> {
    let mut by_func: HashMap<&str, FuncStats> = HashMap::new();

    for entry in entries {
        let function = if entry.function.is_empty() {
            "(outside a function)"
        } else {
            entry.function.as_str()
        };

        let stats = by_func.entry(function).or_insert_with(|| FuncStats {
            function,
            latencies: Vec::new(),
            answers: HashMap::new(),
            keypress: 0,
        });

        stats.latencies.push(entry.latency_ms);
        stats.keypress += usize::from(is_keypress_wait(entry));
        if let Some(text) = answer_text(entry) {
            *stats.answers.entry(text).or_default() += 1;
        }
    }

    let mut stats: Vec<FuncStats> = by_func.into_values().collect();
    stats.sort_by(|a, b| b.count().cmp(&a.count()).then_with(|| a.function.cmp(b.function)));
    stats
}

/// A wait the player *clears* rather than answers: a keypress, or the raw
/// event INPUTMOUSEKEY reports. How fast one of these goes by says how the
/// player is reading, not what they decided.
fn is_keypress_wait(entry: &InputLogEntry) -> bool {
    matches!(
        entry.request_type.as_str(),
        "AnyKey" | "EnterKey" | "ForceEnterKey" | "MouseKey"
    )
}

/// Which function asked for the *next* input after each one, per session.
///
/// A pair that repeats is a path the player walks: the menu they come back to,
/// the confirmation that always follows a choice.
fn follow_ons(sessions: &[Session]) -> Vec<((String, String), usize)> {
    let mut pairs: HashMap<(String, String), usize> = HashMap::new();

    for session in sessions {
        for pair in session.entries.windows(2) {
            if pair[0].function == pair[1].function {
                continue;
            }
            let key = (pair[0].function.clone(), pair[1].function.clone());
            *pairs.entry(key).or_default() += 1;
        }
    }

    ranked(pairs, |n| *n)
}

/// The answer as the analysis groups it: an integer stands for itself, a
/// string is quoted, and a mouse or key event is named by its key code.
fn answer_text(entry: &InputLogEntry) -> Option<String> {
    if let Some(value) = &entry.value {
        return Some(match value {
            Value::Int(i) => i.to_string(),
            Value::String(s) => format!("{s:?}"),
        });
    }

    entry.mouse_key.as_ref().map(|ev| format!("key {}", ev.code))
}

fn suggestions(
    total: usize,
    funcs: &[FuncStats<'_>],
    follow: &[((String, String), usize)],
    session_ms: u64,
    waiting_ms: u64,
) -> Vec<String> {
    /// Below this many inputs a function's numbers are noise.
    const ENOUGH: usize = 8;

    let mut notes = Vec::new();

    let hot: Vec<&FuncStats> = funcs
        .iter()
        .take_while(|stats| stats.count() * 100 / total.max(1) >= 15)
        .collect();
    if !hot.is_empty() {
        let named = hot
            .iter()
            .map(|stats| format!("{} {}", stats.function, share(stats.count(), total)))
            .collect::<Vec<_>>()
            .join(", ");
        notes.push(format!(
            "{} of every input comes from {named}: the frames those print, and the \
             work they do before printing, are where a faster engine would be felt.",
            share(hot.iter().map(|s| s.count()).sum(), total)
        ));
    }

    for stats in funcs.iter().take(12) {
        let count = stats.count();
        if count < ENOUGH {
            continue;
        }

        if let Some((answer, hits)) = top(&stats.answers) {
            if hits * 100 / count >= 60 {
                notes.push(format!(
                    "{} is answered {} {} of the time: worth making it the \
                     default a TINPUT falls back on, or the first choice offered.",
                    stats.function,
                    elide(answer, 24),
                    share(hits, count)
                ));
            }
        }

        let mut times = stats.latencies.clone();
        times.sort_unstable();
        let (median, p95) = (percentile(&times, 50.0), percentile(&times, 95.0));

        // Only for the waits the player *clears*: an `INPUT` answered in
        // 100ms is a decision already made, not a page being skipped, and
        // the frequent-answer note above already says so.
        if stats.is_keypress() && p95 < 400 {
            notes.push(format!(
                "{}'s waits are cleared within {} even at p95 — the player is \
                 holding the key down rather than reading. The MESSKIP path \
                 through it is the one to keep cheap.",
                stats.function,
                duration(p95),
            ));
        } else if median > 8_000 {
            notes.push(format!(
                "{} keeps the player thinking for {} at the median: its prompt is \
                 where an explanation would pay for itself.",
                stats.function,
                duration(median)
            ));
        }
    }

    if let Some(((from, to), hits)) = follow.first() {
        let from_count = funcs
            .iter()
            .find(|s| s.function == from.as_str())
            .map_or(0, FuncStats::count);
        if *hits >= ENOUGH && from_count > 0 && hits * 100 / from_count >= 50 {
            notes.push(format!(
                "{to} follows {from} {} of the time ({hits}x): whatever {to} \
                 computes first can be prepared while {from} is still waiting.",
                share(*hits, from_count)
            ));
        }
    }

    if session_ms > 0 && total > 0 {
        let idle = waiting_ms * 100 / session_ms;
        if idle >= 80 {
            notes.push(format!(
                "{idle}% of the session is the engine waiting on a human. Startup and \
                 the frames around an input are worth optimising; raw throughput is not \
                 what anyone is feeling."
            ));
        } else if idle <= 40 {
            notes.push(format!(
                "only {idle}% of the session is spent waiting on the player, so the rest \
                 is the engine's own work — the hot functions above are where it goes."
            ));
        }
    }

    notes
}

/// `counts` as a list, heaviest first. Ties break on the key, so two runs
/// over the same log print the same table.
fn ranked<K: Ord, V>(counts: HashMap<K, V>, weight: impl Fn(&V) -> usize) -> Vec<(K, V)> {
    let mut ranked: Vec<(K, V)> = counts.into_iter().collect();
    ranked.sort_by(|a, b| weight(&b.1).cmp(&weight(&a.1)).then_with(|| a.0.cmp(&b.0)));
    ranked
}

/// The most frequent key, ties broken by name so the output is stable.
fn top<K: Ord>(counts: &HashMap<K, usize>) -> Option<(&K, usize)> {
    counts
        .iter()
        .max_by(|a, b| a.1.cmp(b.1).then_with(|| b.0.cmp(a.0)))
        .map(|(k, n)| (k, *n))
}

fn mean(values: &[u64]) -> u64 {
    if values.is_empty() {
        return 0;
    }
    values.iter().sum::<u64>() / values.len() as u64
}

/// The `p`th percentile of an already sorted slice, by nearest rank.
fn percentile(sorted: &[u64], p: f64) -> u64 {
    if sorted.is_empty() {
        return 0;
    }
    let rank = (p / 100.0 * (sorted.len() - 1) as f64).round() as usize;
    sorted[rank.min(sorted.len() - 1)]
}

fn share(part: usize, whole: usize) -> String {
    if whole == 0 {
        return "-".into();
    }
    format!("{:.1}%", part as f64 * 100.0 / whole as f64)
}

fn bar(part: usize, whole: usize) -> String {
    if whole == 0 {
        return String::new();
    }
    "#".repeat((part * 24 / whole).max(usize::from(part > 0)))
}

/// A human duration: milliseconds under a second, seconds under a minute,
/// `1m02s` above.
fn duration(ms: u64) -> String {
    match ms {
        ms if ms < 1_000 => format!("{ms}ms"),
        ms if ms < 60_000 => format!("{:.1}s", ms as f64 / 1000.0),
        ms => format!("{}m{:02}s", ms / 60_000, ms % 60_000 / 1000),
    }
}

/// `text` on one line and clipped to `width` characters, so one pasted essay
/// of an INPUTS answer cannot wreck the table.
fn elide(text: &str, width: usize) -> String {
    let mut out = String::new();

    for (n, c) in text.chars().enumerate() {
        if n + 1 >= width {
            out.push('…');
            break;
        }
        out.push(if c.is_control() { ' ' } else { c });
    }

    out
}
