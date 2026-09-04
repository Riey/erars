//! Every input wait the engine answers, on disk: what was asked, what the
//! user answered, and how long they took to answer it.
//!
//! The point is analysis, not debugging. A session leaves two files side by
//! side in the game's `logs/` directory:
//!
//! * `inputs_<stamp>.jsonl` — one [`InputLogEntry`] per line, which
//!   `cargo run --example analyze_inputs` turns into usage statistics.
//! * `inputs_<stamp>.ron` — the same answers as a bare `Vec<Value>`, which is
//!   exactly what `erars-stdio --use-input` replays, so any recorded session
//!   can be re-run.
//!
//! Both are written by the VM thread through one [`InputLogger`] handle held
//! by [`VmContext`](crate::VmContext). Recording is best-effort throughout: a
//! log that cannot be opened, or that fails a write halfway through a session,
//! disables itself and never turns into a game-visible error.

use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, BufWriter, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use erars_ast::Value;
use erars_ui::{InputRequestType, MouseKeyEvent};
use serde::{Deserialize, Serialize};

/// Set to anything but `0`, `false` or the empty string to record nothing at
/// all — the opt-out for a player who would rather not leave a trace of what
/// they typed.
pub const NO_INPUT_LOG_ENV: &str = "ERARS_NO_INPUT_LOG";

/// The stable name for the newest session's log, next to the timestamped
/// files. A symlink where the platform has them, a hard link otherwise.
pub const LAST_INPUTS: &str = "last_inputs.jsonl";

/// The INPUTMOUSEKEY answer as the log stores it: the event itself, whose six
/// `RESULT` fields are already the whole of what the front end reported.
pub type MouseKeyEventLog = MouseKeyEvent;

/// One answered input wait, as one line of `inputs_<stamp>.jsonl`.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct InputLogEntry {
    /// Wall clock at the moment the answer arrived, `2026-09-04T13:01:23.456Z`.
    pub timestamp: String,
    /// Milliseconds from the start of the session, which orders entries
    /// across a clock change.
    pub elapsed_ms: u64,
    /// Milliseconds the front end took to answer: the user's think time.
    pub latency_ms: u64,
    /// The function that asked — the top of the call stack — or the empty
    /// string for a wait outside any function.
    pub function: String,
    /// [`InputRequestType`] by name; see [`request_type_name`].
    pub request_type: String,
    /// The wait was a ONEINPUT-family one.
    pub is_one: bool,
    /// The value answered, absent for a keypress wait and for INPUTMOUSEKEY.
    pub value: Option<Value>,
    /// The event answered, present only for INPUTMOUSEKEY.
    pub mouse_key: Option<MouseKeyEventLog>,
}

impl InputLogEntry {
    /// Read one `.jsonl` log.
    ///
    /// A line that does not parse is skipped with a warning: a session killed
    /// mid-write leaves a partial last line, and one truncated entry must not
    /// cost the analysis the rest of the file.
    pub fn read_log(path: &Path) -> io::Result<Vec<Self>> {
        let mut entries = Vec::new();

        for line in BufReader::new(File::open(path)?).lines() {
            let line = line?;
            if line.trim().is_empty() {
                continue;
            }
            match serde_json::from_str(&line) {
                Ok(entry) => entries.push(entry),
                Err(err) => {
                    log::warn!("{}: skipping unreadable log line: {err}", path.display())
                }
            }
        }

        Ok(entries)
    }
}

/// What the front end answered a wait with.
#[derive(Debug, Clone, Copy)]
pub enum InputAnswer<'a> {
    /// A value wait. `None` is a keypress wait, which carries no value.
    Value(Option<&'a Value>),
    /// INPUTMOUSEKEY: one raw mouse or key event.
    MouseKey(&'a MouseKeyEvent),
}

/// One answered input wait, handed to [`InputLogger::log`].
#[derive(Debug, Clone, Copy)]
pub struct InputEvent<'a> {
    /// The function that asked, i.e. the top of the call stack.
    pub function: &'a str,
    pub request_type: InputRequestType,
    /// The wait was a ONEINPUT-family one.
    pub is_one: bool,
    /// How long the front end took to answer.
    pub latency: Duration,
    pub answer: InputAnswer<'a>,
}

/// The name the log gives an [`InputRequestType`], and the one the analyzer
/// groups by. Spelled out rather than derived from `Debug` so a rename of the
/// enum cannot silently split a log's history in two.
pub fn request_type_name(ty: InputRequestType) -> &'static str {
    match ty {
        InputRequestType::AnyKey => "AnyKey",
        InputRequestType::EnterKey => "EnterKey",
        InputRequestType::ForceEnterKey => "ForceEnterKey",
        InputRequestType::Int => "Int",
        InputRequestType::Str => "Str",
        InputRequestType::MouseKey => "MouseKey",
    }
}

/// A handle on the session's input log. Cheap to clone, and every clone
/// writes to the same files under one lock, so a front end that answers from
/// another thread can share the VM's.
///
/// The default is a disabled logger, which is what a [`VmContext`] built
/// outside `erars-loader` — a test, a bench — gets.
#[derive(Clone, Default)]
pub struct InputLogger {
    /// `None` once and for all when logging is off; the whole logger is then
    /// a null pointer's worth of state and [`InputLogger::log`] returns
    /// before it can allocate.
    state: Option<Arc<Mutex<State>>>,
}

impl InputLogger {
    /// A logger that records nothing.
    pub fn disabled() -> Self {
        Self { state: None }
    }

    /// Open the session's log in `dir`, or in `<target_path>/logs` when `dir`
    /// is `None`, creating the directory if it does not exist.
    ///
    /// Never fails: [`NO_INPUT_LOG_ENV`], an unwritable directory or a full
    /// disk all give back a disabled logger, because a game must not refuse to
    /// start over its own telemetry.
    pub fn open(dir: Option<PathBuf>, target_path: &Path) -> Self {
        if env_disabled(std::env::var_os(NO_INPUT_LOG_ENV)) {
            log::info!("Input logging is off ({NO_INPUT_LOG_ENV})");
            return Self::disabled();
        }

        let dir = dir.unwrap_or_else(|| target_path.join("logs"));

        match State::create(&dir) {
            Ok(state) => {
                log::info!(
                    "Input log: {} (replay with --use-input {})",
                    state.jsonl_path.display(),
                    state.replay_path.display()
                );
                Self {
                    state: Some(Arc::new(Mutex::new(state))),
                }
            }
            Err(err) => {
                log::warn!("Can't open an input log in {}: {err}", dir.display());
                Self::disabled()
            }
        }
    }

    /// Whether anything is being recorded. Worth asking before building the
    /// arguments for [`InputLogger::log`], which resolving a function name is.
    pub fn is_enabled(&self) -> bool {
        self.state.is_some()
    }

    /// The `.jsonl` this session is writing, if any.
    pub fn jsonl_path(&self) -> Option<PathBuf> {
        self.with_state(|state| state.jsonl_path.clone())
    }

    /// The `--use-input` replay file this session is writing, if any.
    pub fn replay_path(&self) -> Option<PathBuf> {
        self.with_state(|state| state.replay_path.clone())
    }

    /// Record one answered wait. Both files are on disk by the time this
    /// returns, so a session that ends in a kill still has everything but the
    /// wait it was sitting in.
    pub fn log(&self, event: InputEvent<'_>) {
        self.with_state(|state| state.write(event));
    }

    fn with_state<T>(&self, f: impl FnOnce(&mut State) -> T) -> Option<T> {
        let state = self.state.as_ref()?;
        // A panic while the lock was held can have left a half-written line,
        // and nothing else: the writers' own bookkeeping is repaired by the
        // next append. Stepping over the poison keeps a crash in the VM from
        // turning into a second one here.
        let mut state = match state.lock() {
            Ok(state) => state,
            Err(poison) => poison.into_inner(),
        };
        Some(f(&mut state))
    }
}

struct State {
    /// Start of the session, for `elapsed_ms`.
    started: Instant,
    jsonl: BufWriter<File>,
    jsonl_path: PathBuf,
    replay: ReplayWriter,
    replay_path: PathBuf,
    /// A write failed. The session's log is abandoned rather than warned
    /// about once per input for the rest of the run.
    broken: bool,
}

impl State {
    fn create(dir: &Path) -> io::Result<Self> {
        fs::create_dir_all(dir)?;

        let stem = free_stem(dir, &file_stamp(time::OffsetDateTime::now_utc()));
        let jsonl_path = stem.with_extension("jsonl");
        let replay_path = stem.with_extension("ron");

        let jsonl = BufWriter::new(File::create(&jsonl_path)?);
        let replay = ReplayWriter::create(&replay_path)?;

        if let Err(err) = link_last(dir, &jsonl_path) {
            log::warn!("Can't point {LAST_INPUTS} at this session: {err}");
        }

        Ok(Self {
            started: Instant::now(),
            jsonl,
            jsonl_path,
            replay,
            replay_path,
            broken: false,
        })
    }

    fn write(&mut self, event: InputEvent<'_>) {
        if self.broken {
            return;
        }

        let entry = InputLogEntry {
            timestamp: iso_timestamp(time::OffsetDateTime::now_utc()),
            elapsed_ms: self.started.elapsed().as_millis() as u64,
            latency_ms: event.latency.as_millis() as u64,
            function: event.function.to_owned(),
            request_type: request_type_name(event.request_type).to_owned(),
            is_one: event.is_one,
            value: match event.answer {
                InputAnswer::Value(value) => value.cloned(),
                InputAnswer::MouseKey(_) => None,
            },
            mouse_key: match event.answer {
                InputAnswer::Value(_) => None,
                InputAnswer::MouseKey(ev) => Some(ev.clone()),
            },
        };

        if let Err(err) = self.append(&entry, replay_value(&event)) {
            log::warn!("Input log write failed, recording nothing further: {err}");
            self.broken = true;
        }
    }

    fn append(&mut self, entry: &InputLogEntry, replay: Option<Value>) -> io::Result<()> {
        // `to_writer` emits a token at a time, which is what the `BufWriter`
        // is for; the flush is per entry because a game is far more often
        // killed than shut down, and an input a second is nothing to write.
        serde_json::to_writer(&mut self.jsonl, entry)?;
        self.jsonl.write_all(b"\n")?;
        self.jsonl.flush()?;

        if let Some(value) = replay {
            self.replay.push(&value)?;
        }

        Ok(())
    }
}

/// The `--use-input` replay file, appended in place.
///
/// A RON list needs its `]`, so a file that is to stay replayable *during* the
/// session cannot simply be appended to, and rewriting the whole list per
/// input is quadratic in a long one. `body_len` remembers where the
/// terminator begins: an append seeks there, writes the value and puts the
/// terminator back, which keeps the file a valid `Vec<Value>` after every
/// input at constant cost.
struct ReplayWriter {
    file: File,
    body_len: u64,
}

impl ReplayWriter {
    const TERMINATOR: &'static [u8] = b"]\n";
    const HEADER: &'static [u8] = b"[\n";

    fn create(path: &Path) -> io::Result<Self> {
        let mut file = File::create(path)?;
        file.write_all(Self::HEADER)?;

        let mut this = Self {
            file,
            body_len: Self::HEADER.len() as u64,
        };
        this.terminate()?;

        Ok(this)
    }

    fn push(&mut self, value: &Value) -> io::Result<()> {
        let line = format!(
            "    {},\n",
            ron::to_string(value).map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?
        );

        self.file.seek(SeekFrom::Start(self.body_len))?;
        self.file.write_all(line.as_bytes())?;
        self.body_len += line.len() as u64;

        self.terminate()
    }

    /// Close the list at the cursor, which `push` left at `body_len`.
    ///
    /// The truncation matters after a failed `push`: that one left `body_len`
    /// where it was, so this write lands *inside* a fragment it has to cut
    /// away for the file to parse again.
    fn terminate(&mut self) -> io::Result<()> {
        self.file.write_all(Self::TERMINATOR)?;
        self.file.set_len(self.body_len + Self::TERMINATOR.len() as u64)?;
        self.file.flush()
    }
}

/// The value `--use-input` has to hold for this answer to be replayed.
///
/// A front end fed from a replay file pops one value per *value* wait and one
/// per INPUTMOUSEKEY, and answers a keypress wait out of thin air without
/// consuming anything (`erars-stdio/src/stdio_frontend.rs:131-138,221-243`).
/// So a keypress contributes nothing to the file, and a mouse or key event
/// contributes the code the front end derived — which it takes back verbatim
/// when the integer is replayed.
fn replay_value(event: &InputEvent<'_>) -> Option<Value> {
    match event.answer {
        InputAnswer::Value(value) => match event.request_type {
            InputRequestType::Int | InputRequestType::Str => value.cloned(),
            _ => None,
        },
        InputAnswer::MouseKey(ev) => Some(Value::Int(ev.code)),
    }
}

/// `inputs_<stamp>`, with a `-N` suffix once a session started in the same
/// second has claimed the name, so two front ends launched together do not
/// truncate each other's log.
fn free_stem(dir: &Path, stamp: &str) -> PathBuf {
    for n in 0.. {
        let stem = match n {
            0 => dir.join(format!("inputs_{stamp}")),
            n => dir.join(format!("inputs_{stamp}-{n}")),
        };

        if !stem.with_extension("jsonl").exists() && !stem.with_extension("ron").exists() {
            return stem;
        }
    }

    unreachable!("0.. is not exhausted")
}

/// Point [`LAST_INPUTS`] at `target`.
///
/// A symlink to the bare file name, so the whole `logs/` directory can be
/// moved or copied and the link still resolves. Where symlinks need a
/// privilege the engine has no business asking for, a hard link is the same
/// thing to a reader.
fn link_last(dir: &Path, target: &Path) -> io::Result<()> {
    let link = dir.join(LAST_INPUTS);

    if let Err(err) = fs::remove_file(&link) {
        if err.kind() != io::ErrorKind::NotFound {
            return Err(err);
        }
    }

    #[cfg(unix)]
    {
        let name = target.file_name().unwrap_or(target.as_os_str());
        std::os::unix::fs::symlink(name, &link)
    }
    #[cfg(not(unix))]
    {
        fs::hard_link(target, &link)
    }
}

/// `2026-09-04T13:01:23.456Z`. Hand-formatted, like `GETTIMES`
/// (`terminal_vm::executor::get_times`), because this build of `time` carries
/// no format descriptions.
fn iso_timestamp(now: time::OffsetDateTime) -> String {
    format!(
        "{year:04}-{month:02}-{day:02}T{hour:02}:{minute:02}:{second:02}.{milli:03}Z",
        year = now.year(),
        month = now.month() as u8,
        day = now.day(),
        hour = now.hour(),
        minute = now.minute(),
        second = now.second(),
        milli = now.millisecond(),
    )
}

/// `2026-09-04_13-01-23`, the shape `flexi_logger` gives the engine log next
/// to these (`erars-stdio/src/main.rs:64-77`), so a session's two logs sort
/// together. UTC for the same reason that one is.
fn file_stamp(now: time::OffsetDateTime) -> String {
    format!(
        "{year:04}-{month:02}-{day:02}_{hour:02}-{minute:02}-{second:02}",
        year = now.year(),
        month = now.month() as u8,
        day = now.day(),
        hour = now.hour(),
        minute = now.minute(),
        second = now.second(),
    )
}

/// Whether [`NO_INPUT_LOG_ENV`] asks for logging to be off: set to anything
/// but `0`, `false` or nothing at all.
///
/// Takes the value rather than reading it so the rule is testable without
/// mutating an environment the rest of the test binary shares.
fn env_disabled(flag: Option<OsString>) -> bool {
    match flag {
        Some(flag) => !matches!(flag.to_string_lossy().trim(), "" | "0" | "false"),
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::VecDeque;

    fn logger_in(dir: &Path) -> InputLogger {
        let logger = InputLogger::open(Some(dir.to_owned()), Path::new("/nonexistent"));
        assert!(logger.is_enabled(), "logger refused {}", dir.display());
        logger
    }

    fn log_value(logger: &InputLogger, ty: InputRequestType, value: Option<Value>, ms: u64) {
        logger.log(InputEvent {
            function: "SHOP",
            request_type: ty,
            is_one: false,
            latency: Duration::from_millis(ms),
            answer: InputAnswer::Value(value.as_ref()),
        });
    }

    #[test]
    fn env_var_decides() {
        assert!(!env_disabled(None));
        assert!(env_disabled(Some("1".into())));
        assert!(env_disabled(Some("true".into())));
        assert!(!env_disabled(Some("0".into())));
        assert!(!env_disabled(Some("false".into())));
        assert!(!env_disabled(Some("".into())));
    }

    #[test]
    fn disabled_logger_writes_nothing() {
        let logger = InputLogger::disabled();

        assert!(!logger.is_enabled());
        assert_eq!(logger.jsonl_path(), None);
        assert_eq!(logger.replay_path(), None);

        // The whole point of the null state: a log call on it is inert.
        log_value(&logger, InputRequestType::Int, Some(Value::Int(1)), 10);
    }

    /// The directory is created on demand, and both files land in it.
    #[test]
    fn creates_log_dir() {
        let root = tempfile::tempdir().unwrap();
        let dir = root.path().join("logs");
        let logger = logger_in(&dir);

        log_value(&logger, InputRequestType::Int, Some(Value::Int(7)), 100);

        let jsonl = logger.jsonl_path().unwrap();
        assert_eq!(jsonl.parent(), Some(dir.as_path()));
        assert!(jsonl.exists());
        assert!(logger.replay_path().unwrap().exists());
        assert!(dir.join(LAST_INPUTS).exists(), "{LAST_INPUTS} is missing");
        assert_eq!(
            InputLogEntry::read_log(&dir.join(LAST_INPUTS)).unwrap().len(),
            1,
            "{LAST_INPUTS} does not read back as the session's log"
        );
    }

    #[test]
    fn records_every_kind_of_answer() {
        let root = tempfile::tempdir().unwrap();
        let logger = logger_in(root.path());

        log_value(&logger, InputRequestType::Int, Some(Value::Int(3)), 1200);
        log_value(
            &logger,
            InputRequestType::Str,
            Some(Value::String("레이무".into())),
            700,
        );
        log_value(&logger, InputRequestType::AnyKey, None, 50);
        logger.log(InputEvent {
            function: "SYSTEM_TITLE",
            request_type: InputRequestType::MouseKey,
            is_one: true,
            latency: Duration::from_millis(20),
            answer: InputAnswer::MouseKey(&MouseKeyEvent {
                kind: 3,
                code: 13,
                x: 13,
                ..MouseKeyEvent::default()
            }),
        });

        let entries = InputLogEntry::read_log(&logger.jsonl_path().unwrap()).unwrap();
        assert_eq!(entries.len(), 4);

        let kinds: Vec<&str> = entries.iter().map(|e| e.request_type.as_str()).collect();
        assert_eq!(kinds, ["Int", "Str", "AnyKey", "MouseKey"]);

        assert_eq!(entries[0].value, Some(Value::Int(3)));
        assert_eq!(entries[0].latency_ms, 1200);
        assert_eq!(entries[0].function, "SHOP");
        assert!(!entries[0].is_one);
        assert!(entries[0].timestamp.ends_with('Z'), "{:?}", entries[0]);

        assert_eq!(entries[1].value, Some(Value::String("레이무".into())));

        // A keypress wait carries no value, and is still worth an entry: it is
        // where the reading time goes.
        assert_eq!(entries[2].value, None);
        assert_eq!(entries[2].mouse_key, None);

        assert!(entries[3].is_one);
        assert_eq!(entries[3].value, None);
        assert_eq!(entries[3].mouse_key.as_ref().unwrap().code, 13);
        assert_eq!(entries[3].function, "SYSTEM_TITLE");

        // Elapsed is measured from the session, so it cannot go backwards.
        assert!(entries.windows(2).all(|w| w[0].elapsed_ms <= w[1].elapsed_ms));
    }

    /// The `.ron` holds exactly what `erars-stdio --use-input` has to pop,
    /// and holds it after every single input rather than at the end.
    #[test]
    fn replay_file_is_valid_after_every_input() {
        let root = tempfile::tempdir().unwrap();
        let logger = logger_in(root.path());
        let replay = logger.replay_path().unwrap();

        let parse = |path: &Path| -> VecDeque<Value> {
            ron::from_str(&fs::read_to_string(path).unwrap())
                .unwrap_or_else(|err| panic!("{} is not a replay file: {err}", path.display()))
        };

        assert!(parse(&replay).is_empty(), "a fresh log replays as nothing");

        log_value(&logger, InputRequestType::Int, Some(Value::Int(101)), 40);
        assert_eq!(parse(&replay), [Value::Int(101)]);

        // Not a value wait: `input` answers it without popping anything, so
        // recording one would shift every later answer by one.
        log_value(&logger, InputRequestType::EnterKey, None, 40);
        log_value(&logger, InputRequestType::AnyKey, None, 40);
        assert_eq!(parse(&replay), [Value::Int(101)]);

        log_value(
            &logger,
            InputRequestType::Str,
            Some(Value::String("a \"quoted\" name".into())),
            40,
        );
        logger.log(InputEvent {
            function: "WAIT_KEY",
            request_type: InputRequestType::MouseKey,
            is_one: false,
            latency: Duration::from_millis(40),
            answer: InputAnswer::MouseKey(&MouseKeyEvent {
                kind: 3,
                code: 27,
                ..MouseKeyEvent::default()
            }),
        });

        assert_eq!(
            parse(&replay),
            [
                Value::Int(101),
                // The escaping is RON's, not ours.
                Value::String("a \"quoted\" name".into()),
                // What `input_mouse_key` turns back into key 27.
                Value::Int(27),
            ]
        );
    }

    /// Two sessions in the same second keep their own files.
    #[test]
    fn concurrent_sessions_do_not_collide() {
        let root = tempfile::tempdir().unwrap();
        let first = logger_in(root.path());
        let second = logger_in(root.path());

        assert_ne!(first.jsonl_path(), second.jsonl_path());
        assert_ne!(first.replay_path(), second.replay_path());

        log_value(&first, InputRequestType::Int, Some(Value::Int(1)), 10);
        log_value(&second, InputRequestType::Int, Some(Value::Int(2)), 10);

        for (logger, expected) in [(&first, 1), (&second, 2)] {
            let entries = InputLogEntry::read_log(&logger.jsonl_path().unwrap()).unwrap();
            assert_eq!(entries.len(), 1);
            assert_eq!(entries[0].value, Some(Value::Int(expected)));
        }
    }

    /// One log, several threads: the clones share the files and the lock.
    #[test]
    fn shared_across_threads() {
        let root = tempfile::tempdir().unwrap();
        let logger = logger_in(root.path());

        std::thread::scope(|s| {
            for _ in 0..4 {
                let logger = logger.clone();
                s.spawn(move || {
                    for i in 0..25 {
                        log_value(&logger, InputRequestType::Int, Some(Value::Int(i)), 1);
                    }
                });
            }
        });

        assert_eq!(
            InputLogEntry::read_log(&logger.jsonl_path().unwrap()).unwrap().len(),
            100
        );
        let replayed: Vec<Value> =
            ron::from_str(&fs::read_to_string(logger.replay_path().unwrap()).unwrap()).unwrap();
        assert_eq!(replayed.len(), 100);
    }

    /// A truncated tail — a session that was killed mid-write — costs its own
    /// entry and nothing else.
    #[test]
    fn partial_last_line_is_skipped() {
        let root = tempfile::tempdir().unwrap();
        let logger = logger_in(root.path());
        let path = logger.jsonl_path().unwrap();

        log_value(&logger, InputRequestType::Int, Some(Value::Int(1)), 10);
        drop(logger);

        let mut raw = fs::read_to_string(&path).unwrap();
        raw.push_str("{\"timestamp\":\"2026-09-04T13");
        fs::write(&path, raw).unwrap();

        let entries = InputLogEntry::read_log(&path).unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].value, Some(Value::Int(1)));
    }
}
