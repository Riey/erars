//! Runtime coverage harness for the mirrored Emuera wiki name lists.
//!
//! Re-run with a single command:
//!
//! ```text
//! cargo test -p erars --test wiki_coverage -- --nocapture
//! ```
//!
//! It rewrites `docs/research/emuera-wiki/coverage.md` as a name-by-name table
//! and then asserts that the *absent* set is a subset of [`EXCLUDED`].
//!
//! **Why this exists.** Parse-time acceptance is not evidence of
//! implementation. An unknown `NAME(...)` becomes `Expr::Method` at
//! `crates/erars-compiler/src/parser/expr.rs:540` and only faults when the
//! line is actually reached, so a source-table diff of `BuiltinMethod` against
//! the wiki produces false positives in both directions. Every verdict here
//! comes from driving a real program through
//! preprocess → parse → compile → `FunctionDic` → `TerminalVm::start`.
//!
//! Three outcomes are distinguished, and telling the second from the first is
//! the whole difficulty — *an argument or type error proves the name resolved*:
//!
//! * `absent` — the name resolved to nothing: `[lexer] Unknown line`
//!   (`crates/erars-lexer/src/lib.rs:845`, `:912`), `[lexer] Unknown sharp
//!   line` (`:826`), `Function X is not exists`
//!   (`crates/erars-vm/src/function.rs:337`) or `Variable X is not exists`
//!   (`crates/erars-vm/src/variable.rs:832`, `:995`).
//! * `present` — the name resolved and then refused the probe: an arity error,
//!   a type error, a missing block terminator, a semantic refusal.
//! * `ran` — the name resolved and the line completed.
//!
//! A probe ladder is walked per name and the *best* outcome wins, so `ran`
//! is reported whenever any plausible call shape works. `absent` therefore
//! means every shape resolved to nothing.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::path::PathBuf;
use std::sync::Arc;

use erars_ast::Value;
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::{InputRequest, InputRequestType, VirtualConsole};
use erars_vm::*;

mod test_util;

const INDEX: &str = "docs/research/emuera-wiki/index.md";
const REPORT: &str = "docs/research/emuera-wiki/coverage.md";

/// The image / HTML / live-input family, owned by a concurrent session. Listed
/// in the report, never implemented here. The assertion at the end of
/// [`wiki_runtime_coverage`] allows exactly these to be absent, so it keeps
/// passing when that session lands them.
const EXCLUDED: &[&str] = &[
    "HTML_ESCAPE",
    "HTML_TOPLAINTEXT",
    "HTML_GETPRINTEDSTR",
    "HTML_POPPRINTINGSTR",
    "CBGSETG",
    "CBGSETSPRITE",
    "CBGCLEAR",
    "CBGCLEARBUTTON",
    "CBGREMOVERANGE",
    "CBGREMOVEBMAP",
    "CBGSETBMAPG",
    "CBGSETBUTTONSPRITE",
    "SPRITEGETCOLOR",
    "GFILLRECTANGLE",
    "GETKEY",
    "GETKEYTRIGGERED",
    "MOUSEX",
    "MOUSEY",
    "ISACTIVE",
    "CHARATU",
];

// ---------------------------------------------------------------------------
// verdicts
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Verdict {
    Absent,
    Present,
    Ran,
}

impl Verdict {
    fn tag(self) -> &'static str {
        match self {
            Verdict::Absent => "absent",
            Verdict::Present => "present",
            Verdict::Ran => "ran",
        }
    }
}

/// What one probe program did.
enum Outcome {
    Parse(String),
    Compile(String),
    VmError(String),
    /// The probe made erars panic. The name resolved — a panic is reached only
    /// after dispatch — so this counts as `present`, but it is a defect in its
    /// own right and the report lists it separately.
    Panic(String),
    Ran(String),
}

impl Outcome {
    fn text(&self) -> &str {
        match self {
            Outcome::Parse(s)
            | Outcome::Compile(s)
            | Outcome::VmError(s)
            | Outcome::Panic(s)
            | Outcome::Ran(s) => s,
        }
    }

    fn stage(&self) -> &'static str {
        match self {
            Outcome::Parse(_) => "parse",
            Outcome::Compile(_) => "compile",
            Outcome::VmError(_) => "vm",
            Outcome::Panic(_) => "**panic**",
            Outcome::Ran(_) => "ran",
        }
    }
}

/// Is `msg` one of the "this name resolved to nothing" diagnostics? The four
/// global ones, plus any section-specific marks the probe carries.
fn is_absent(name: &str, marks: &'static [&'static str], msg: &str) -> bool {
    if marks.iter().any(|m| msg.contains(m)) {
        return true;
    }
    let msg = msg.to_ascii_uppercase();
    let name = name.trim_start_matches(['#', '@']).to_ascii_uppercase();
    msg.contains("[LEXER] UNKNOWN LINE")
        || msg.contains("[LEXER] UNKNOWN SHARP LINE")
        || msg.contains(&format!("FUNCTION {name} IS NOT EXISTS"))
        || msg.contains(&format!("VARIABLE {name} IS NOT EXISTS"))
}

/// The verdict for one probe, plus the detail to report. Returns `Absent` for
/// a run that succeeded without producing the effect the probe demanded: an
/// accepted-and-ignored directive is not an implemented directive.
fn classify(name: &str, p: &Probe, o: &Outcome) -> (Verdict, String) {
    match o {
        Outcome::Ran(out) => match &p.expect {
            Some(e) if !out.contains(e.must.as_str()) => (
                Verdict::Absent,
                format!("accepted with no effect (wanted `{}`): {out}", e.must),
            ),
            Some(Expect {
                must_not: Some(bad),
                ..
            }) if out.contains(bad.as_str()) => (
                Verdict::Absent,
                format!("wrong branch taken (`{bad}` should not run): {out}"),
            ),
            _ => (Verdict::Ran, out.clone()),
        },
        Outcome::Panic(msg) => (Verdict::Present, msg.clone()),
        other => {
            let msg = other.text();
            if is_absent(name, p.absent_marks, msg) {
                (Verdict::Absent, msg.to_owned())
            } else {
                (Verdict::Present, msg.to_owned())
            }
        }
    }
}

// ---------------------------------------------------------------------------
// runner
// ---------------------------------------------------------------------------

thread_local! {
    static PANIC_MSG: std::cell::RefCell<Option<String>> = const { std::cell::RefCell::new(None) };
}

/// Answers the first input request with one fixed line, the rest with
/// [`FOLLOW_UP`]. Emuera's debug console is driven exactly this way:
/// `PressEnterKey` spots a leading `@`, executes the line as a debug command,
/// and leaves the pending input request unconsumed
/// (`GameView/EmueraConsole.cs:1103-1110`). Feeding `@REBOOT` through a normal
/// `INPUTS` is therefore a real runtime test of whether that path exists.
struct ScriptedInput {
    first: String,
    used: bool,
}

/// What every request after the first is answered with. A debug console
/// re-issues the request it intercepted, so the probe has to have a second
/// answer ready or it would spin.
const FOLLOW_UP: &str = "ZZFOLLOWUP";

impl SystemFunctions for ScriptedInput {
    fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        let text = if self.used {
            FOLLOW_UP
        } else {
            self.used = true;
            self.first.as_str()
        };
        Ok(match req.ty {
            InputRequestType::Int => text.parse::<i64>().ok().map(Value::Int),
            InputRequestType::Str => Some(Value::String(text.to_owned())),
            _ => None,
        })
    }

    fn redraw(
        &mut self,
        _vconsole: &mut VirtualConsole,
        _painted: erars_vm::graphics::Painted<'_>,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

struct Runner {
    header: Arc<HeaderInfo>,
    config: Arc<EraConfig>,
    sav: PathBuf,
    resources: PathBuf,
    /// Every panicking probe, `(name, probe, message)`. Collected globally
    /// because the per-name ladder keeps only its best outcome, and a panic
    /// that a later probe shape papers over is still a defect.
    panics: std::cell::RefCell<Vec<(String, String, String)>>,
}

impl Runner {
    fn new() -> Self {
        erars_ast::init_interner();
        let root = std::env::temp_dir().join("erars-wiki-coverage");
        let sav = root.join("sav");
        let resources = root.join("resources");
        std::fs::create_dir_all(&sav).unwrap();
        std::fs::create_dir_all(&resources).unwrap();
        // The section (g) resource rows need a real `resources/` tree: one
        // parent bitmap plus a CSV in both formats
        // (`docs/research/emuera-wiki/index.md:769-770`).
        std::fs::write(resources.join("zzimg.png"), PNG_8X8).unwrap();
        std::fs::write(
            resources.join("zzsprite.csv"),
            "ZZSPR,zzimg.png,1,2,4,5,6,7\n\
             ZZANI,ANIME,4,5\n\
             ZZANI,zzimg.png,0,0,4,5,0,0,100\n",
        )
        .unwrap();
        Runner {
            header: test_util::get_ctx("").header.try_as_arc().unwrap(),
            config: Arc::new(EraConfig::default()),
            sav,
            resources,
            panics: std::cell::RefCell::new(Vec::new()),
        }
    }

    /// One probe, with a panic caught and reported rather than aborting the
    /// sweep. A panic on script input is a defect in its own right — Emuera
    /// answers every one of these with a script error — so the report names
    /// the panic site.
    fn exec(&self, p: &Probe) -> Outcome {
        PANIC_MSG.with(|c| *c.borrow_mut() = None);
        let prev = std::panic::take_hook();
        std::panic::set_hook(Box::new(|info| {
            let at = info
                .location()
                .map(|l| format!("{}:{}", l.file(), l.line()))
                .unwrap_or_default();
            let what = info
                .payload()
                .downcast_ref::<&str>()
                .map(|s| (*s).to_owned())
                .or_else(|| info.payload().downcast_ref::<String>().cloned())
                .unwrap_or_default();
            PANIC_MSG.with(|c| *c.borrow_mut() = Some(format!("panicked at {at}: {what}")));
        }));
        let ret = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            self.exec_inner(p)
        }));
        std::panic::set_hook(prev);
        match ret {
            Ok(o) => o,
            Err(_) => Outcome::Panic(
                PANIC_MSG
                    .with(|c| c.borrow().clone())
                    .unwrap_or_else(|| "panicked".to_owned()),
            ),
        }
    }

    fn exec_inner(&self, p: &Probe) -> Outcome {
        let header = match &p.header {
            Some(Ok(h)) => h.clone(),
            Some(Err(e)) => return Outcome::Parse(format!("CSV refused: {e}")),
            None => self.header.clone(),
        };
        let pctx = ParserContext::new(header.clone(), erars_ast::StrKey::new("probe.erb"))
            .with_debug(p.debug);

        let program = match pctx.parse_program_str(&p.program) {
            Ok(prog) => prog,
            Err((msg, _)) => return Outcome::Parse(msg),
        };

        let sys: Box<dyn SystemFunctions> = match &p.input {
            Some(text) => Box::new(ScriptedInput {
                first: text.clone(),
                used: false,
            }),
            None => Box::new(NullSystemFunctions),
        };
        let mut ctx = VmContext::new(
            header,
            self.config.clone(),
            sys,
            self.sav.clone(),
            self.resources.clone(),
        );
        if p.load_resources {
            let warnings =
                erars_vm::resources::load(&mut ctx.graphics, &self.resources, encoding_rs::UTF_8);
            if !warnings.is_empty() {
                return Outcome::Parse(format!(
                    "resources/ refused: {}",
                    warnings.iter().map(|w| w.to_string()).collect::<Vec<_>>().join(" / ")
                ));
            }
        }
        let mut dic = FunctionDic::new();
        for func in program {
            let compiled = match compile(func) {
                Ok(c) => c,
                Err(e) => return Outcome::Compile(e.to_string()),
            };
            dic.insert_compiled_func(
                &mut ctx.var,
                &ctx.header_info.default_local_size,
                compiled,
            );
        }

        let vm = TerminalVm::new(dic, ctx.header_info.clone());
        let mut tx = VirtualConsole::new(&console_config(&ctx.config));
        let ok = vm.start(&mut tx, &mut ctx);

        let mut out = String::new();
        for line in tx.lines_from(0).iter() {
            let _ = writeln!(out, "{line}");
        }
        let _ = write!(out, "{}", tx.last_line);

        if ok {
            Outcome::Ran(one_line(&out))
        } else {
            let err = out
                .lines()
                .find(|l| l.contains("VM error occurred:"))
                .map(|l| l.trim().to_owned())
                .unwrap_or_else(|| one_line(&out));
            Outcome::VmError(err)
        }
    }
}

/// Flatten console output to one line, losing nothing.
///
/// It must lose nothing: [`classify`] searches this text for the probe's
/// expected marker, and a probe whose output runs long — `@CONFIG` prints the
/// whole config — would otherwise have its evidence clipped away and be
/// reported absent. Clipping belongs to the report, in [`clip`].
fn one_line(s: &str) -> String {
    s.lines()
        .map(str::trim_end)
        .filter(|l| !l.is_empty())
        .collect::<Vec<_>>()
        .join(" / ")
}

/// Shorten a detail for one table cell.
fn clip(s: &str) -> String {
    if s.chars().count() > 160 {
        let mut t: String = s.chars().take(157).collect();
        t.push_str("...");
        t
    } else {
        s.to_owned()
    }
}

// ---------------------------------------------------------------------------
// probe ladders
// ---------------------------------------------------------------------------

/// A probe: the whole program text, plus the fragment shown in the report.
struct Probe {
    shown: String,
    program: String,
    debug: bool,
    /// Text the run must (and must not) leave in the console. A probe that is
    /// *accepted* but leaves no trace counts as absent, because that is
    /// exactly the signature of an unimplemented preprocessor directive: an
    /// unknown `[...]` code only produces a warning
    /// (`crates/erars-lexer/src/lib.rs:602-680`), never an error, so the line
    /// is silently dropped and the program "succeeds".
    expect: Option<Expect>,
    /// Section-specific messages that mean "this name resolved to nothing",
    /// on top of the four global diagnostics.
    absent_marks: &'static [&'static str],
    /// Header override, for probes that need extra CSV data merged in. An
    /// `Err` is a CSV erars' own parser refused, and is reported as such.
    header: Option<Result<Arc<HeaderInfo>, String>>,
    /// Answer every input request with this text instead of nothing.
    input: Option<String>,
    /// Run the `resources/` startup loader before the program, the way
    /// `erars-loader` does (`crates/erars-loader/src/lib.rs:556-570`).
    load_resources: bool,
}

struct Expect {
    must: String,
    must_not: Option<String>,
}

impl Probe {
    fn new(shown: String, program: String) -> Probe {
        Probe {
            shown,
            program,
            debug: false,
            expect: None,
            absent_marks: &[],
            header: None,
            input: None,
            load_resources: false,
        }
    }

    fn debug(mut self) -> Probe {
        self.debug = true;
        self
    }

    fn expect(mut self, must: impl Into<String>) -> Probe {
        self.expect = Some(Expect {
            must: must.into(),
            must_not: None,
        });
        self
    }

    fn expect_only(mut self, must: impl Into<String>, must_not: impl Into<String>) -> Probe {
        self.expect = Some(Expect {
            must: must.into(),
            must_not: Some(must_not.into()),
        });
        self
    }

    fn absent_marks(mut self, marks: &'static [&'static str]) -> Probe {
        self.absent_marks = marks;
        self
    }

    fn header(mut self, header: Result<Arc<HeaderInfo>, String>) -> Probe {
        self.header = Some(header);
        self
    }

    fn input(mut self, text: impl Into<String>) -> Probe {
        self.input = Some(text.into());
        self
    }

    fn load_resources(mut self) -> Probe {
        self.load_resources = true;
        self
    }
}

fn plain(body: &str) -> Probe {
    Probe::new(
        body.replace('\n', " ⏎ "),
        format!("@SYSTEM_TITLE\n{body}\n"),
    )
}

/// A body that must run at most once even if it restarts the function.
fn guarded(body: &str) -> Probe {
    Probe::new(
        body.replace('\n', " ⏎ "),
        format!("@SYSTEM_TITLE\nSIF FLAG:99 != 0\n\tRETURN 0\nFLAG:99 = 1\n{body}\n"),
    )
}

/// A probe that supplies its own whole program, function headers included.
fn whole(shown: &str, program: &str) -> Probe {
    Probe::new(shown.replace('\n', " ⏎ "), program.to_owned())
}

/// Argument lists tried at a line-head instruction, cheapest first.
const INST_ARGS: &[&str] = &[
    "",
    " 0",
    " 0, 0",
    " 0, 0, 0",
    " 0, 0, 0, 0",
    " 0, 0, 0, 0, 0",
    " \"X\"",
    " \"X\", 0",
    " \"X\", 0, 0",
    " \"X\", \"Y\"",
    " FLAG",
    " FLAG, 0",
    " FLAG, 0, 0",
    " FLAG, 0, 0, 0",
    " FLAG, 0, 0, 0, 0",
    " FLAG, \"X\"",
    " CFLAG",
    " CFLAG, 0",
];

fn instruction_probes(name: &str) -> Vec<Probe> {
    // The only probe body that can re-enter its own function.
    if name == "RESTART" {
        return vec![guarded("RESTART")];
    }
    // `SIF` binds the next statement; a bare one is a different error.
    if name == "SIF" {
        return vec![plain("SIF 0\n\tPRINTL no\nPRINTL yes")];
    }
    // Block openers: give the terminator so the block is well formed.
    let block: &[(&str, &str)] = &[
        ("REPEAT", "REPEAT 1\nPRINTL r\nREND"),
        ("FOR", "FOR LOCAL, 0, 1\nPRINTL f\nNEXT"),
        ("WHILE", "WHILE 0\nPRINTL w\nWEND"),
        ("DO", "DO\nPRINTL d\nLOOP 0"),
        ("SELECTCASE", "SELECTCASE 1\nCASE 1\nPRINTL c\nENDSELECT"),
        ("CASE", "SELECTCASE 1\nCASE 1\nPRINTL c\nENDSELECT"),
        (
            "CASEELSE",
            "SELECTCASE 1\nCASE 2\nCASEELSE\nPRINTL e\nENDSELECT",
        ),
        ("ENDSELECT", "SELECTCASE 1\nCASE 1\nENDSELECT"),
        ("CATCH", "TRYCCALL NOSUCH\nCATCH\nPRINTL caught\nENDCATCH"),
        (
            "ENDCATCH",
            "TRYCCALL NOSUCH\nCATCH\nPRINTL caught\nENDCATCH",
        ),
        ("TRYCALLLIST", "TRYCALLLIST\nFUNC NOSUCH\nENDFUNC"),
        ("TRYJUMPLIST", "TRYJUMPLIST\nFUNC NOSUCH\nENDFUNC"),
        ("TRYGOTOLIST", "TRYGOTOLIST\nFUNC NOSUCH\nENDFUNC"),
        ("FUNC", "TRYCALLLIST\nFUNC NOSUCH\nENDFUNC"),
        ("ENDFUNC", "TRYCALLLIST\nFUNC NOSUCH\nENDFUNC"),
        ("NEXT", "FOR LOCAL, 0, 1\nNEXT"),
        ("REND", "REPEAT 1\nREND"),
        ("WEND", "WHILE 0\nWEND"),
        ("LOOP", "DO\nLOOP 0"),
        ("ELSE", "IF 0\nELSE\nPRINTL e\nENDIF"),
        ("ELSEIF", "IF 0\nELSEIF 1\nPRINTL e\nENDIF"),
        ("ENDIF", "IF 1\nPRINTL i\nENDIF"),
        ("BREAK", "REPEAT 1\nBREAK\nREND"),
        ("CONTINUE", "REPEAT 1\nCONTINUE\nREND"),
        ("DATA", "PRINTDATAL\nDATA a\nENDDATA"),
        ("DATAFORM", "PRINTDATAL\nDATAFORM a{1}\nENDDATA"),
        (
            "DATALIST",
            "PRINTDATAL\nDATALIST\nDATA a\nDATA b\nENDLIST\nENDDATA",
        ),
        (
            "ENDLIST",
            "PRINTDATAL\nDATALIST\nDATA a\nENDLIST\nENDDATA",
        ),
        ("ENDDATA", "PRINTDATAL\nDATA a\nENDDATA"),
        ("STRDATA", "STRDATA LOCALS\nDATA a\nENDDATA\nPRINTL %LOCALS%"),
        ("PRINTDATA", "PRINTDATA\nDATA a\nENDDATA"),
    ];
    if let Some((_, body)) = block.iter().find(|(n, _)| *n == name) {
        return vec![plain(body)];
    }
    // The PRINTDATA mask expands to nine names, all block openers.
    if let Some(rest) = name.strip_prefix("PRINTDATA") {
        return vec![plain(&format!("PRINTDATA{rest}\nDATA a\nENDDATA"))];
    }
    // Names the blind argument ladder cannot drive to completion: they need a
    // callee, a label, a live character, a matching terminator, an answered
    // input request or a specific literal. A bare-name arity error only proves
    // the name *resolved*, which is exactly the classification that could hide
    // an unimplemented arm, so each of these gets one hand-built call that has
    // to run and print. The blind ladder is still appended: if a hand-built
    // shape is wrong, the probe degrades to the old evidence instead of
    // silently reporting a gap.
    //
    // (name, body, extra top-level functions, input answer)
    let driven: &[(&str, &str, &str, &str)] = &[
        // CHARA3.CSV, the template `test_util::get_ctx` merges, is number 3.
        ("ADDCHARA", "ADDCHARA 3\nPRINTFORML [{CHARANUM}]", "", ""),
        (
            "ADDCOPYCHARA",
            "ADDCHARA 3\nADDCOPYCHARA 0\nPRINTFORML [{CHARANUM}]",
            "",
            "",
        ),
        (
            "ALIGNMENT",
            "ALIGNMENT CENTER\nCURRENTALIGN\nPRINTFORML [%RESULTS%]",
            "",
            "",
        ),
        (
            "ARRAYMSORT",
            "FLAG:0 = 2\nFLAG:1 = 1\nARRAYMSORT FLAG, TFLAG\nPRINTFORML [{FLAG:0}]",
            "",
            "",
        ),
        ("BARL", "BARL 3, 10, 10", "", ""),
        ("BARSTR", "BARSTR 3, 10, 10\nPRINTFORML [%RESULTS%]", "", ""),
        ("BEGIN", "", "", ""),
        ("CALL", "CALL ZZT\nPRINTFORML [after]", "@ZZT\nPRINTL in", ""),
        (
            "CALLF",
            "CALLF ZZF\nPRINTFORML [after]",
            "@ZZF\n#FUNCTION\nRETURNF 1",
            "",
        ),
        (
            "CALLFORM",
            "CALLFORM ZZ{1}\nPRINTFORML [after]",
            "@ZZ1\nPRINTL in",
            "",
        ),
        (
            "CALLFORMF",
            "CALLFORMF ZZ{1}\nPRINTFORML [after]",
            "@ZZ1\n#FUNCTION\nRETURNF 1",
            "",
        ),
        (
            "CALLEVENT",
            "CALLEVENT EVENTFIRST\nPRINTFORML [after]",
            "@EVENTFIRST\nPRINTL ev",
            "",
        ),
        (
            "COPYCHARA",
            "ADDCHARA 3\nADDCHARA 3\nCOPYCHARA 0, 1\nPRINTFORML [{CHARANUM}]",
            "",
            "",
        ),
        (
            "CVARSET",
            "ADDCHARA 3\nCVARSET CFLAG, 0, 7\nPRINTFORML [{CFLAG:0:0}]",
            "",
            "",
        ),
        (
            "DELCHARA",
            "ADDCHARA 3\nDELCHARA 0\nPRINTFORML [{CHARANUM}]",
            "",
            "",
        ),
        (
            "ENDNOSKIP",
            "NOSKIP\nPRINTL n\nENDNOSKIP\nPRINTFORML [after]",
            "",
            "",
        ),
        ("GCREATE", "GCREATE 1, 4, 4\nPRINTFORML [{GCREATED(1)}]", "", ""),
        (
            "GDRAWG",
            "GCREATE 1, 4, 4\nGCREATE 2, 4, 4\nGDRAWG 1, 2, 0, 0, 4, 4, 0, 0, 4, 4\n\
             PRINTFORML [after]",
            "",
            "",
        ),
        (
            "GSETFONT",
            "GCREATE 1, 4, 4\nGSETFONT 1, \"\", 12\nPRINTFORML [after]",
            "",
            "",
        ),
        (
            "GOTO",
            "GOTO ZZL\nPRINTL skipped\n$ZZL\nPRINTFORML [arrived]",
            "",
            "",
        ),
        // LOADDATA has no shape that completes without a real save to load, and
        // a successful load ends in `Workflow::Begin(BeginType::Shop)`
        // (`executor.rs:555`), whose `SHOW_SHOP` input loop never returns. So
        // the marker is printed from `@EVENTSHOP`, which `run_begin` calls
        // before entering that loop (`:851`), and `QUIT` ends the run cleanly.
        (
            "LOADDATA",
            "SAVEDATA 0, \"zzdesc\"\nLOADDATA 0\nPRINTL notreached",
            "@EVENTSHOP\nPRINTFORML [loaded]\nQUIT",
            "",
        ),
        (
            "GOTOFORM",
            "GOTOFORM ZZ{1}\nPRINTL skipped\n$ZZ1\nPRINTFORML [arrived]",
            "",
            "",
        ),
        ("INPUT", "INPUT\nPRINTFORML [{RESULT}]", "", "7"),
        ("INPUTS", "INPUTS\nPRINTFORML [%RESULTS%]", "", "zz"),
        ("ONEINPUT", "ONEINPUT\nPRINTFORML [{RESULT}]", "", "7"),
        ("ONEINPUTS", "ONEINPUTS\nPRINTFORML [%RESULTS%]", "", "z"),
        ("TINPUT", "TINPUT 1, 7\nPRINTFORML [{RESULT}]", "", "5"),
        ("TINPUTS", "TINPUTS 1, \"d\"\nPRINTFORML [%RESULTS%]", "", "zz"),
        ("TONEINPUT", "TONEINPUT 1, 7\nPRINTFORML [{RESULT}]", "", "5"),
        (
            "TONEINPUTS",
            "TONEINPUTS 1, \"d\"\nPRINTFORML [%RESULTS%]",
            "",
            "z",
        ),
        ("JUMP", "JUMP ZZT", "@ZZT\nPRINTFORML [jumped]", ""),
        ("JUMPFORM", "JUMPFORM ZZ{1}", "@ZZ1\nPRINTFORML [jumped]", ""),
        (
            "PICKUPCHARA",
            "ADDCHARA 3\nADDCHARA 3\nPICKUPCHARA 1\nPRINTFORML [{CHARANUM}]",
            "",
            "",
        ),
        (
            "REPLACE",
            "REPLACE \"aXb\", \"X\", \"-\"\nPRINTFORML [%RESULTS%]",
            "",
            "",
        ),
        (
            "RESET_STAIN",
            "ADDCHARA 3\nRESET_STAIN 0\nPRINTFORML [{STAIN:0:2}]",
            "",
            "",
        ),
        (
            "SAVECHARA",
            "ADDCHARA 3\nSAVECHARA \"zzchara\", \"zzdesc\", 0\nPRINTFORML [after]",
            "",
            "",
        ),
        // SET(BG)COLORBYNAME takes raw FORM text, not a quoted expression, so a
        // quoted literal looks up a colour name that includes the quotes.
        // `css_color::Srgb` parses the CSS names, so bare `red` resolves.
        (
            "SETBGCOLORBYNAME",
            "SETBGCOLORBYNAME red\nPRINTFORML [after]",
            "",
            "",
        ),
        (
            "SETCOLORBYNAME",
            "SETCOLORBYNAME red\nPRINTFORML [after]",
            "",
            "",
        ),
        (
            "SKIPDISP",
            "SKIPDISP 1\nPRINTL hidden\nSKIPDISP 0\nPRINTFORML [after]",
            "",
            "",
        ),
        (
            "SPLIT",
            "SPLIT \"a,b\", \",\", LOCALS\nPRINTFORML [%LOCALS:0%]",
            "",
            "",
        ),
        (
            "SPRITEANIMECREATE",
            "SPRITEANIMECREATE \"zzani\", 4, 4\nPRINTFORML [{SPRITECREATED(\"zzani\")}]",
            "",
            "",
        ),
        (
            "SPRITEANIMEADDFRAME",
            "GCREATE 1, 4, 4\nSPRITEANIMECREATE \"zzani\", 4, 4\n\
             SPRITEANIMEADDFRAME \"zzani\", 1, 0, 0, 4, 4, 0, 0, 100\nPRINTFORML [after]",
            "",
            "",
        ),
        (
            "SWAP",
            "FLAG:0 = 1\nFLAG:1 = 2\nSWAP FLAG:0, FLAG:1\nPRINTFORML [{FLAG:0}]",
            "",
            "",
        ),
        (
            "SWAPCHARA",
            "ADDCHARA 3\nADDCHARA 3\nSWAPCHARA 0, 1\nPRINTFORML [{CHARANUM}]",
            "",
            "",
        ),
        (
            "TRYCCALL",
            "ADDCHARA 3\nTRYCCALL ZZT\nCATCH\nPRINTL c\nENDCATCH\nPRINTFORML [after]",
            "@ZZT\nPRINTL in",
            "",
        ),
        (
            "TRYCCALLFORM",
            "ADDCHARA 3\nTRYCCALLFORM ZZ{1}\nCATCH\nPRINTL c\nENDCATCH\nPRINTFORML [after]",
            "@ZZ1\nPRINTL in",
            "",
        ),
        (
            "TRYCJUMP",
            "ADDCHARA 3\nTRYCJUMP ZZT\nCATCH\nPRINTL c\nENDCATCH",
            "@ZZT\nPRINTFORML [after]",
            "",
        ),
        (
            "TRYCJUMPFORM",
            "ADDCHARA 3\nTRYCJUMPFORM ZZ{1}\nCATCH\nPRINTL c\nENDCATCH",
            "@ZZ1\nPRINTFORML [after]",
            "",
        ),
        (
            "TRYCGOTO",
            "TRYCGOTO ZZL\nCATCH\nPRINTL c\nENDCATCH\n$ZZL\nPRINTFORML [after]",
            "",
            "",
        ),
        (
            "TRYCGOTOFORM",
            "TRYCGOTOFORM ZZ{1}\nCATCH\nPRINTL c\nENDCATCH\n$ZZ1\nPRINTFORML [after]",
            "",
            "",
        ),
    ];
    if let Some((_, body, extra, input)) = driven.iter().find(|(n, _, _, _)| *n == name) {
        // `BEGIN TITLE` re-enters the probe's own function, like RESTART.
        let mut probes = if name == "BEGIN" {
            vec![guarded("BEGIN TITLE")]
        } else {
            let program = format!("@SYSTEM_TITLE\n{body}\n{extra}");
            let mut p = whole(body, &program);
            if !input.is_empty() {
                p = p.input(*input);
            }
            vec![p]
        };
        probes.extend(INST_ARGS.iter().map(|a| plain(&format!("{name}{a}"))));
        return probes;
    }
    if name.starts_with("PRINT") {
        return print_probes(name);
    }
    INST_ARGS
        .iter()
        .map(|a| plain(&format!("{name}{a}")))
        .collect()
}

/// The PRINT family: one payload per `PrintType`, so the probe is a real print
/// rather than an arity error.
fn print_probes(name: &str) -> Vec<Probe> {
    // These three take a real expression, not raw print text.
    match name {
        "PRINT_IMG" => return vec![plain("PRINT_IMG \"x\"")],
        "PRINT_RECT" => return vec![plain("PRINT_RECT 1")],
        "PRINT_SPACE" => return vec![plain("PRINT_SPACE 1")],
        _ => {}
    }
    let up = name.to_ascii_uppercase();
    let base = up.trim_start_matches("PRINT");
    let base = base.strip_prefix("SINGLE").unwrap_or(base);
    let payload = if base.starts_with("BUTTON") {
        " \"lit\", 1"
    } else if base.starts_with("FORMS") {
        " \"lit{1}\""
    } else if base.starts_with("FORM") {
        " lit{1}"
    } else if base.starts_with('V') {
        " 1"
    } else if base.starts_with('S') {
        " \"lit\""
    } else {
        " lit"
    };
    vec![plain(&format!("{name}{payload}")), plain(name)]
}

/// Argument lists tried in an expression slot, cheapest first.
const FN_ARGS: &[&str] = &[
    "",
    "0",
    "0, 0",
    "0, 0, 0",
    "0, 0, 0, 0",
    "\"X\"",
    "\"X\", 0",
    "\"X\", 0, 0",
    "\"X\", \"Y\"",
    "\"FLAG\"",
    "\"FLAG\", 0",
    "FLAG",
    "FLAG, 0",
    "FLAG, 0, 0",
    "CFLAG",
    "CFLAG, 0",
];

/// Method probes: the wiki signature first, then a generic ladder. Both an
/// integer slot `{…}` and a string slot `%…%` are tried, because a type
/// mismatch is not absence.
///
/// **Never** probe with `RESULTS = NAME(...)`. Assignment to a string
/// variable takes its right-hand side as a raw FORM literal
/// (`crates/erars-lexer/src/lib.rs` assignment path, Emuera
/// `GameProc/LogicalLine.cs` string-assign), so `RESULTS = ZZNOSUCH(0)`
/// happily stores the *text* `ZZNOSUCH(0)` and every unknown name would look
/// implemented. `harness_controls` pins this.
fn function_probes(name: &str, sig: Option<&str>) -> Vec<Probe> {
    let mut out = Vec::new();
    let mut push = |args: &str| {
        out.push(plain(&format!("PRINTFORML [{{{name}({args})}}]")));
        out.push(plain(&format!("PRINTFORML [%{name}({args})%]")));
    };
    if let Some(args) = sig {
        push(args);
    }
    for args in FN_ARGS {
        push(args);
    }
    out
}

/// Variable probes: every index arity, in both an integer and a string slot.
/// Character variables need a registered character before they can be read,
/// so each shape is retried behind `ADDDEFCHARA`.
fn variable_probes(name: &str) -> Vec<Probe> {
    let mut out = Vec::new();
    for idx in ["", ":0", ":0:0", ":0:0:0"] {
        out.push(plain(&format!("PRINTFORML [{{{name}{idx}}}]")));
        out.push(plain(&format!("PRINTFORML [%{name}{idx}%]")));
    }
    for idx in ["", ":0", ":0:0", ":0:0:0"] {
        out.push(plain(&format!(
            "ADDDEFCHARA\nPRINTFORML [{{{name}{idx}}}]"
        )));
        out.push(plain(&format!(
            "ADDDEFCHARA\nPRINTFORML [%{name}{idx}%]"
        )));
    }
    out
}

/// `#…` directives. Each probe *uses* what the directive declares, so a
/// silently ignored directive shows up as absent rather than as a clean run.
fn sharp_probes(name: &str) -> Vec<Probe> {
    let p = |shown: &str, program: &str, must: &'static str| whole(shown, program).expect(must);
    match name {
        // `#DIM`/`#DIMS` declare a function-local array; write and read it back.
        "#DIM" => vec![p(
            "#DIM X, 3 → X:2",
            "@SYSTEM_TITLE\n#DIM X, 3\nX:2 = 7\nPRINTFORML [{X:2}]\n",
            "[7]",
        )],
        "#DIMS" => vec![p(
            "#DIMS S, 3 → S:2",
            // Assignment to a string variable takes its right side as a raw
            // FORM literal, in erars and in Emuera alike, so quoting `q` here
            // would store the quotes with it.
            "@SYSTEM_TITLE\n#DIMS S, 3\nS:2 = q\nPRINTFORML [%S:2%]\n",
            "[q]",
        )],
        // `#LOCALSIZE`/`#LOCALSSIZE` resize LOCAL/LOCALS past their default 100.
        "#LOCALSIZE" => vec![p(
            "#LOCALSIZE 200 → LOCAL:199",
            "@SYSTEM_TITLE\n#LOCALSIZE 200\nLOCAL:199 = 5\nPRINTFORML [{LOCAL:199}]\n",
            "[5]",
        )],
        "#LOCALSSIZE" => vec![p(
            "#LOCALSSIZE 200 → LOCALS:199",
            "@SYSTEM_TITLE\n#LOCALSSIZE 200\nLOCALS:199 = z\nPRINTFORML [%LOCALS:199%]\n",
            "[z]",
        )],
        // `#FUNCTION`/`#FUNCTIONS` make the function usable in an expression.
        "#FUNCTION" => vec![p(
            "#FUNCTION + RETURNF 3 → {F()}",
            "@SYSTEM_TITLE\nPRINTFORML [{F()}]\n\n@F\n#FUNCTION\nRETURNF 3\n",
            "[3]",
        )],
        "#FUNCTIONS" => vec![p(
            "#FUNCTIONS + RETURNF \"s\" → %F()%",
            "@SYSTEM_TITLE\nPRINTFORML [%F()%]\n\n@F\n#FUNCTIONS\nRETURNF \"s\"\n",
            "[s]",
        )],
        // `#DEFINE` is an ERH/function-header macro; try both placements.
        "#DEFINE" => vec![
            p(
                "#DEFINE MYMAC 42 → {MYMAC}",
                "@SYSTEM_TITLE\n#DEFINE MYMAC 42\nPRINTFORML [{MYMAC}]\n",
                "[42]",
            ),
            p(
                "#DEFINE above @SYSTEM_TITLE",
                "#DEFINE MYMAC 42\n@SYSTEM_TITLE\nPRINTFORML [{MYMAC}]\n",
                "[42]",
            ),
        ],
        // `#ONLY` makes one definition of an event function the only one that
        // runs (`GameData/Function/FunctionLabelLine.cs`, exfunc). The plain
        // body must not appear.
        "#ONLY" => vec![whole(
            "#ONLY on the second @EVENTFIRST",
            "@SYSTEM_TITLE\nCALLEVENT EVENTFIRST\n\n\
             @EVENTFIRST\nPRINTL WRONG\n\n\
             @EVENTFIRST\n#ONLY\nPRINTL ONLYRAN\n",
        )
        .expect_only("ONLYRAN", "WRONG")],
        _ => vec![whole(name, &format!("@SYSTEM_TITLE\n{name}\n"))],
    }
}

/// `[…]` bracket directives. An unknown bracket code only *warns*
/// (`crates/erars-lexer/src/lib.rs:602-680`) and the line is dropped, so these
/// probes have to check which branch actually ran.
fn square_probes(name: &str) -> Vec<Probe> {
    // `test_util::get_ctx` defines TRUE and EMPTY_MACRO; ZZUNDEF is defined
    // nowhere, and `[IF]` asks only whether the name is in the macro table.
    const SKIP: &str = "@SYSTEM_TITLE\n[SKIPSTART]\nPRINTL WRONG\n[SKIPEND]\nPRINTL AFTER\n";
    const IF: &str =
        "@SYSTEM_TITLE\n[IF EMPTY_MACRO]\nPRINTL TAKEN\n[ELSE]\nPRINTL WRONG\n[ENDIF]\n";
    const ELSE: &str =
        "@SYSTEM_TITLE\n[IF ZZUNDEF]\nPRINTL WRONG\n[ELSE]\nPRINTL TAKEN\n[ENDIF]\n";
    const ELSEIF: &str = "@SYSTEM_TITLE\n[IF ZZUNDEF]\nPRINTL WRONG\n\
         [ELSEIF EMPTY_MACRO]\nPRINTL TAKEN\n[ELSE]\nPRINTL WRONG\n[ENDIF]\n";
    match name {
        "SKIPSTART" | "SKIPEND" => {
            vec![whole(&format!("[{name}] block"), SKIP).expect_only("AFTER", "WRONG")]
        }
        "IF" => vec![whole("[IF EMPTY_MACRO]", IF).expect_only("TAKEN", "WRONG")],
        "ELSE" => vec![whole("[ELSE] on an undefined macro", ELSE).expect_only("TAKEN", "WRONG")],
        "ELSEIF" => vec![whole("[ELSEIF EMPTY_MACRO]", ELSEIF).expect_only("TAKEN", "WRONG")],
        "ENDIF" => vec![whole("[ENDIF] closing [IF]", IF).expect_only("TAKEN", "WRONG")],
        // `--debug` is the only switch these two read
        // (`crates/erars-compiler/src/parser.rs:2273-2276`).
        "IF_DEBUG" => vec![whole(
            "[IF_DEBUG] with --debug",
            "@SYSTEM_TITLE\n[IF_DEBUG]\nPRINTL TAKEN\n[ENDIF]\n",
        )
        .debug()
        .expect("TAKEN")],
        "IF_NDEBUG" => vec![whole(
            "[IF_NDEBUG] without --debug",
            "@SYSTEM_TITLE\n[IF_NDEBUG]\nPRINTL TAKEN\n[ENDIF]\n",
        )
        .expect("TAKEN")],
        _ => vec![whole(
            &format!("[{name}]"),
            &format!("@SYSTEM_TITLE\n[{name}]\nPRINTL AFTER\n"),
        )],
    }
}

/// erars refuses an unknown config name with Emuera's own wording
/// (`crates/erars-vm/src/terminal_vm/executor.rs:2820-2822`), which is this
/// section's absence signature.
const CONFIG_ABSENT: &[&str] = &["적절한 컨피그 이름이 아닙니다", "適切なコンフィグ名ではありません"];

fn config_probes(key: &str) -> Vec<Probe> {
    vec![
        plain(&format!("PRINTFORML [{{GETCONFIG(\"{key}\")}}]")).absent_marks(CONFIG_ABSENT),
        plain(&format!("PRINTFORML [%GETCONFIGS(\"{key}\")%]")).absent_marks(CONFIG_ABSENT),
    ]
}

/// Emuera's `GETCONFIG`/`GETCONFIGS` reach only these keys; everything else in
/// the config file falls to the `default` arm of `GetConfigValueInERB` and is
/// refused with 「{0}の値を取得することはできません」
/// (`Config/ConfigData.cs:485-559`). A key outside this set is therefore *not*
/// an erars gap when erars refuses it.
const GETCONFIG_KEYS: &[&str] = &[
    "オートセーブを行なう",
    "単位の位置",
    "ウィンドウ幅",
    "PRINTCを並べる数",
    "PRINTCの文字数",
    "フォントサイズ",
    "一行の高さ",
    "表示するセーブデータ数",
    "販売アイテム数",
    "COM_ABLE初期値",
    "文字色",
    "背景色",
    "選択中文字色",
    "履歴文字色",
    "PBANDの初期値",
    "RELATIONの初期値",
    "フォント名",
    "お金の単位",
    "起動時簡略表示",
    "DRAWLINE文字",
    "システムメニュー0",
    "システムメニュー1",
    "時間切れ表示",
    "BAR文字1",
    "BAR文字2",
    "描画インターフェース",
];

/// Debug commands. Emuera reads them from the *input line*: `PressEnterKey`
/// spots a leading `@`, runs `doSystemCommand`, and re-issues the same input
/// request without consuming it (`GameView/EmueraConsole.cs:1103-1110`,
/// `:1321-1390`). Only the generic fall-through is gated on
/// 「デバッグコマンドを使用する」; `@DEBUG` is gated on `-Debug` and the other
/// four on nothing at all. So the probe answers an `INPUTS` with the command
/// and asks what reached `RESULTS`.
fn debug_probes(name: &str) -> Vec<Probe> {
    // The decisive evidence is what the *script* received, which is why the
    // marker is tagged: `doSystemCommand` echoes the command line to the
    // console before running it (`:1336-1338`), so the bare command text
    // appears in the output of a working console too.
    //
    // `@EXIT` and `@REBOOT` close the window (`:1357-1361`,
    // `Forms/MainWindow.cs:807-812`), so for those two the evidence is that
    // the echo happened and the statement after the `INPUTS` never did. Every
    // other command re-issues the request, which `FOLLOW_UP` then answers.
    let expect = if name == "@EXIT" || name == "@REBOOT" {
        (name.to_owned(), "RESULTS=[".to_owned())
    } else {
        (format!("RESULTS=[{FOLLOW_UP}]"), format!("RESULTS=[{name}]"))
    };

    vec![whole(
        &format!("INPUTS answered with `{name}`"),
        "@SYSTEM_TITLE\nINPUTS\nPRINTFORML RESULTS=[%RESULTS%]\n",
    )
    .input(name)
    .expect_only(expect.0, expect.1)]
}

// ---------------------------------------------------------------------------
// section (g): CSV files and column layouts
// ---------------------------------------------------------------------------

/// An 8×8 RGB PNG, written into the temp `resources/` directory so the sprite
/// rows have a real parent bitmap to slice.
const PNG_8X8: &[u8] = &[
    0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x08, 0x08, 0x02, 0x00, 0x00, 0x00, 0x4b, 0x6d, 0x29,
    0xdc, 0x00, 0x00, 0x00, 0x25, 0x49, 0x44, 0x41, 0x54, 0x78, 0xda, 0x63, 0x60, 0x60, 0x60, 0x50,
    0x50, 0x50, 0x70, 0x70, 0x70, 0x48, 0x48, 0x48, 0x68, 0x68, 0x68, 0x58, 0xb0, 0x60, 0xc1, 0x81,
    0x03, 0x07, 0x1e, 0x3c, 0x78, 0xc0, 0x30, 0xb4, 0x24, 0x00, 0x58, 0x99, 0x54, 0x01, 0x9d, 0xd7,
    0x23, 0x85, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82,
];

/// A header carrying the engine globals, one always-present character
/// template so `ADDCHARA 0` works, and whatever the row under test merges.
/// An `Err` means erars' own CSV parser refused the file, which the report
/// shows as a `parse` outcome rather than as a missing feature.
fn csv_header(
    merge: impl FnOnce(&mut HeaderInfo) -> erars_compiler::ParserResult<()>,
) -> Result<Arc<HeaderInfo>, String> {
    let mut info = test_util::header_with_globals();
    if let Err((msg, _)) = info.merge_chara_csv("番号,0\n名前,ZZCHARA\n") {
        return Err(msg);
    }
    match merge(&mut info) {
        Ok(()) => Ok(Arc::new(info)),
        Err((msg, _)) => Err(msg),
    }
}

/// One CSV-layout row: merge exactly that CSV, then read the value back
/// through a real VM run. Reading it back is the point — a merge that parses
/// and discards is indistinguishable from no merge at all.
fn csv_row(
    label: &str,
    merge: impl FnOnce(&mut HeaderInfo) -> erars_compiler::ParserResult<()>,
    body: &str,
    want: &str,
) -> (String, Vec<Probe>) {
    (
        label.to_owned(),
        vec![whole(body, &format!("@SYSTEM_TITLE\n{body}\n"))
            .header(csv_header(merge))
            .expect(want)],
    )
}

/// Every variable the wiki's exetc entry calls CSV-backed, with the name CSV
/// Emuera reads its index names from (`GameData/ConstantData.cs:892-1090`).
/// `ITEM` comes from `Item.csv` through `merge_item_csv`, `STR`'s names come
/// from `STRNAME.CSV` (`crates/erars-loader/src/lib.rs:316`), `CDFLAG` has one
/// table per dimension, and `RELATION` indexes by *character name* out of
/// `chara*.csv` (`ConstantData.cs:694-699`, `:1063`).
const CSV_BACKED: &[(&str, &str, bool)] = &[
    ("ABL", "ABL", false),
    ("BASE", "BASE", false),
    ("CDFLAG", "CDFLAG1", false),
    ("CDOWN", "PALAM", false),
    ("CFLAG", "CFLAG", false),
    ("CSTR", "CSTR", true),
    ("CUP", "PALAM", false),
    ("DOWN", "PALAM", false),
    ("DOWNBASE", "BASE", false),
    ("EQUIP", "EQUIP", false),
    ("EX", "EX", false),
    ("EXP", "EXP", false),
    ("FLAG", "FLAG", false),
    ("GLOBAL", "GLOBAL", false),
    ("GLOBALS", "GLOBALS", true),
    ("GOTJUEL", "PALAM", false),
    ("ITEM", "ITEM", false),
    ("ITEMPRICE", "ITEM", false),
    ("ITEMSALES", "ITEM", false),
    ("JUEL", "PALAM", false),
    ("LOSEBASE", "BASE", false),
    ("MARK", "MARK", false),
    ("MAXBASE", "BASE", false),
    ("NOWEX", "EX", false),
    ("PALAM", "PALAM", false),
    ("RELATION", "chara*", false),
    ("SAVESTR", "SAVESTR", true),
    ("SOURCE", "SOURCE", false),
    ("STAIN", "STAIN", false),
    ("STR", "STRNAME", true),
    ("TALENT", "TALENT", false),
    ("TCVAR", "TCVAR", false),
    ("TEQUIP", "TEQUIP", false),
    ("TFLAG", "TFLAG", false),
    ("TSTR", "TSTR", true),
    ("UP", "PALAM", false),
];

/// The index-name row for one CSV-backed variable: write through the CSV name
/// and read back through the number it must have resolved to. Reading back
/// through the *number* is what makes this a test of the CSV mapping rather
/// than of identifier parsing.
fn csv_backed_row(var: &'static str, source: &'static str, is_str: bool) -> (String, Vec<Probe>) {
    // Assignment to a string variable takes its right side as a raw FORM
    // literal, so `= "ZZV"` would store the quotes as part of the value.
    let set = if is_str { "ZZV" } else { "9" };
    let want = if is_str { "[ZZV]" } else { "[9]" };
    let read = |idx: &str| {
        if is_str {
            format!("PRINTFORML [%{var}:{idx}%]")
        } else {
            format!("PRINTFORML [{{{var}:{idx}}}]")
        }
    };

    let bodies: Vec<String> = match (var, source) {
        // `ITEMPRICE` is `__UNCHANGEABLE__` in Emuera too
        // (`GameData/Variable/VariableCode.cs:96`, an `Int1DConstantToken` at
        // `GameData/Variable/VariableData.cs:259`), so the write half of this
        // row is not a gap in either implementation. Read the price the CSV
        // gave index 1 back out *through the name* instead: that is still a
        // test of the name table and nothing else.
        ("ITEMPRICE", _) => vec![format!("PRINTFORML [{{{var}:ZZIDX}}]")],
        // `RELATION:x:<charaname>`; the name resolves to the template number,
        // so template 1 is registered as the second character.
        (_, "chara*") => vec![format!(
            "ADDCHARA 0\nADDCHARA 1\nRELATION:0:ZZIDX = 9\n{}",
            read("0:1")
        )],
        // `CDFLAG` carries a name at dimension 1 and 2, one table each.
        (_, "CDFLAG1") => vec![
            format!("ADDCHARA 0\nCDFLAG:0:ZZIDX:0 = 9\n{}", read("0:1:0")),
            format!("ADDCHARA 0\nCDFLAG:ZZIDX:0 = 9\n{}", read("1:0")),
        ],
        _ => vec![
            // non-character variable: the name sits at dimension 0
            format!("{var}:ZZIDX = {set}\n{}", read("1")),
            // character variable, character named explicitly
            format!("ADDCHARA 0\n{var}:0:ZZIDX = {set}\n{}", read("0:1")),
            // character variable, implicit TARGET
            format!("ADDCHARA 0\nTARGET = 0\n{var}:ZZIDX = {set}\n{}", read("1")),
        ],
    };

    let probes = bodies
        .into_iter()
        .map(|body| {
            let header = match source {
                "chara*" => csv_header(|i| i.merge_chara_csv("番号,1\n名前,ZZIDX\n")),
                "ITEM" => csv_header(|i| i.merge_item_csv("1,ZZIDX,55")),
                // `STRNAME.CSV` is the file name; the table it fills is keyed
                // `STR` (`crates/erars-loader/src/lib.rs:316`, Emuera
                // `ConstantData.cs:1035-1039`).
                "STRNAME" => csv_header(|i| i.merge_name_csv("STR", "1,ZZIDX")),
                other => {
                    let other = other.to_owned();
                    csv_header(move |i| i.merge_name_csv(&other, "1,ZZIDX"))
                }
            };
            whole(&body, &format!("@SYSTEM_TITLE\n{body}\n"))
                .header(header)
                // The read-only row proves the mapping with the value the CSV
                // itself supplied, not with one the probe wrote.
                .expect(if var == "ITEMPRICE" { "[55]" } else { want })
        })
        .collect();

    (format!("{var} ← {source}.csv index names"), probes)
}

/// Every row of section (g): the eramaker CSV column layouts, the
/// `_replace.csv` settings, the `resources/` sprite formats, and the
/// CSV-backed array variables.
fn csv_rows() -> Vec<(String, Vec<Probe>)> {
    let mut rows = Vec::new();

    // --- GameBase.csv, one row per wiki column ---------------------------
    let gamebase: &[(&str, &str, &str, &str)] = &[
        ("コード", "コード,7", "PRINTFORML [{GAMEBASE_GAMECODE}]", "[7]"),
        (
            "バージョン",
            "バージョン,1234",
            "PRINTFORML [{GAMEBASE_VERSION}]",
            "[1234]",
        ),
        (
            "タイトル",
            "タイトル,ZZTITLE",
            "PRINTFORML [%GAMEBASE_TITLE%]",
            "[ZZTITLE]",
        ),
        (
            "作者",
            "作者,ZZAUTHOR",
            "PRINTFORML [%GAMEBASE_AUTHOR%]",
            "[ZZAUTHOR]",
        ),
        (
            "製作年",
            "製作年,ZZYEAR",
            "PRINTFORML [%GAMEBASE_YEAR%]",
            "[ZZYEAR]",
        ),
        (
            "追加情報",
            "追加情報,ZZINFO",
            "PRINTFORML [%GAMEBASE_INFO%]",
            "[ZZINFO]",
        ),
        (
            "最初からいるキャラ",
            "最初からいるキャラ,2",
            "PRINTFORML [{GAMEBASE_DEFAULTCHARA}]",
            "[2]",
        ),
        (
            "アイテムなし",
            "アイテムなし,3",
            "PRINTFORML [{GAMEBASE_NOITEM}]",
            "[3]",
        ),
        (
            "バージョン違い認める",
            "バージョン違い認める,4",
            "PRINTFORML [{GAMEBASE_ALLOWVERSION}]",
            "[4]",
        ),
    ];
    for (col, csv, body, want) in gamebase {
        let csv = csv.to_string();
        rows.push(csv_row(
            &format!("GameBase.csv · {col}"),
            move |i| i.merge_gamebase_csv(&csv),
            body,
            want,
        ));
    }

    // --- the (番号, 名前) name CSVs, read back through their NAME array ---
    let name_csvs: &[(&str, &str)] = &[
        ("Palam.csv", "PALAM"),
        ("Abl.csv", "ABL"),
        ("Talent.csv", "TALENT"),
        ("Mark.csv", "MARK"),
        ("Exp.csv", "EXP"),
        ("Train.csv", "TRAIN"),
    ];
    for (file, var) in name_csvs {
        let var_owned = var.to_string();
        rows.push(csv_row(
            &format!("{file} · 番号 + 名前 → {var}NAME"),
            move |i| i.merge_name_csv(&var_owned, "1,ZZIDX"),
            &format!("PRINTFORML [%{var}NAME:1%]"),
            "[ZZIDX]",
        ));
    }
    rows.push(csv_row(
        "Item.csv · アイテム番号 + アイテム名 → ITEMNAME",
        |i| i.merge_item_csv("1,ZZIDX,55"),
        "PRINTFORML [%ITEMNAME:1%]",
        "[ZZIDX]",
    ));
    rows.push(csv_row(
        "Item.csv · 値段 → ITEMPRICE",
        |i| i.merge_item_csv("1,ZZIDX,55"),
        "PRINTFORML [{ITEMPRICE:1}]",
        "[55]",
    ));
    rows.push(csv_row(
        "Str.csv · 文字列番号 + 文字列 → STR",
        |i| i.merge_str_csv("1,ZZSTR"),
        "PRINTFORML [%STR:1%]",
        "[ZZSTR]",
    ));

    // --- CharaXX.csv, one row per wiki column ---------------------------
    let chara: &[(&str, &str, &str, &str)] = &[
        ("番号", "番号,5\n", "ADDCHARA 5\nPRINTFORML [{NO:0}]", "[5]"),
        (
            "名前",
            "番号,5\n名前,ZZNAME\n",
            "ADDCHARA 5\nPRINTFORML [%NAME:0%]",
            "[ZZNAME]",
        ),
        (
            "呼び名",
            "番号,5\n呼び名,ZZCALL\n",
            "ADDCHARA 5\nPRINTFORML [%CALLNAME:0%]",
            "[ZZCALL]",
        ),
        (
            "基礎",
            "番号,5\n基礎,1,42\n",
            "ADDCHARA 5\nPRINTFORML [{BASE:0:1}]",
            "[42]",
        ),
        (
            "能力",
            "番号,5\n能力,1,42\n",
            "ADDCHARA 5\nPRINTFORML [{ABL:0:1}]",
            "[42]",
        ),
        (
            "素質",
            "番号,5\n素質,1\n",
            "ADDCHARA 5\nPRINTFORML [{TALENT:0:1}]",
            "[1]",
        ),
        (
            "経験",
            "番号,5\n経験,1,42\n",
            "ADDCHARA 5\nPRINTFORML [{EXP:0:1}]",
            "[42]",
        ),
        (
            "相性",
            "番号,5\n相性,0,42\n",
            "ADDCHARA 5\nPRINTFORML [{RELATION:0:0}]",
            "[42]",
        ),
        (
            "助手",
            "番号,5\n助手,1\n",
            "ADDCHARA 5\nPRINTFORML [{ISASSI:0}]",
            "[1]",
        ),
        (
            "フラグ",
            "番号,5\nフラグ,1,42\n",
            "ADDCHARA 5\nPRINTFORML [{CFLAG:0:1}]",
            "[42]",
        ),
    ];
    for (col, csv, body, want) in chara {
        let csv = csv.to_string();
        rows.push(csv_row(
            &format!("CharaXX.csv · {col}"),
            move |i| i.merge_chara_csv(&csv),
            body,
            want,
        ));
    }

    // --- _replace.csv, one row per wiki setting -------------------------
    let replace: &[(&str, &str, &str, &str)] = &[
        (
            "お金の単位",
            "お金の単位,ZZM",
            "PRINTFORML [%GETCONFIGS(\"お金の単位\")%]",
            "[ZZM]",
        ),
        (
            "単位の位置",
            "単位の位置,後",
            "PRINTFORML [{GETCONFIG(\"単位の位置\")}]",
            "[0]",
        ),
        (
            "起動時簡略表示",
            "起動時簡略表示,ZZS",
            "PRINTFORML [%GETCONFIGS(\"起動時簡略表示\")%]",
            "[ZZS]",
        ),
        (
            "販売アイテム数",
            "販売アイテム数,42",
            "PRINTFORML [{GETCONFIG(\"販売アイテム数\")}]",
            "[42]",
        ),
        (
            "DRAWLINE文字",
            "DRAWLINE文字,=",
            "PRINTFORML [%GETCONFIGS(\"DRAWLINE文字\")%]",
            "[=]",
        ),
        (
            "BAR文字1",
            "BAR文字1,#",
            "PRINTFORML [%GETCONFIGS(\"BAR文字1\")%]",
            "[#]",
        ),
        (
            "BAR文字2",
            "BAR文字2,_",
            "PRINTFORML [%GETCONFIGS(\"BAR文字2\")%]",
            "[_]",
        ),
        (
            "システムメニュー0",
            "システムメニュー0,ZZ0",
            "PRINTFORML [%GETCONFIGS(\"システムメニュー0\")%]",
            "[ZZ0]",
        ),
        (
            "システムメニュー1",
            "システムメニュー1,ZZ1",
            "PRINTFORML [%GETCONFIGS(\"システムメニュー1\")%]",
            "[ZZ1]",
        ),
        (
            "COM_ABLE初期値",
            "COM_ABLE初期値,3",
            "PRINTFORML [{GETCONFIG(\"COM_ABLE初期値\")}]",
            "[3]",
        ),
        (
            "汚れの初期値",
            "汚れの初期値,9/8/7/6/5",
            // Emuera applies `StainDefault` only from `ResetStain`
            // (`GameData/Variable/VariableEvaluator.cs:1651-1660`); adding a
            // character does not seed STAIN.
            "ADDCHARA 0\nRESET_STAIN 0\nPRINTFORML [{STAIN:0:2}]",
            "[7]",
        ),
        (
            "時間切れ表示",
            "時間切れ表示,ZZT",
            "PRINTFORML [%GETCONFIGS(\"時間切れ表示\")%]",
            "[ZZT]",
        ),
        (
            "EXPLVの初期値",
            "EXPLVの初期値,0/11/22/33",
            "PRINTFORML [{EXPLV:2}]",
            "[22]",
        ),
        (
            "PALAMLVの初期値",
            "PALAMLVの初期値,0/11/22/33",
            "PRINTFORML [{PALAMLV:2}]",
            "[22]",
        ),
        (
            "PBANDの初期値",
            "PBANDの初期値,7",
            "PRINTFORML [{GETCONFIG(\"PBANDの初期値\")}]",
            "[7]",
        ),
        (
            "RELATIONの初期値",
            "RELATIONの初期値,6",
            "PRINTFORML [{GETCONFIG(\"RELATIONの初期値\")}]",
            "[6]",
        ),
    ];
    for (key, csv, body, want) in replace {
        let csv = csv.to_string();
        rows.push(csv_row(
            &format!("_replace.csv · {key}"),
            move |i| i.merge_replace_csv(&csv),
            body,
            want,
        ));
    }

    // --- resources CSV formats -----------------------------------------
    rows.push((
        "resources · Sprite (リソース名, 元ファイル名, x, y, w, h, posx, posy)".to_owned(),
        vec![whole(
            "SPRITEWIDTH/HEIGHT/POSX/POSY of `ZZSPR,zzimg.png,1,2,4,5,6,7`",
            "@SYSTEM_TITLE\nPRINTFORML [{SPRITEWIDTH(\"ZZSPR\")}/{SPRITEHEIGHT(\"ZZSPR\")}\
             /{SPRITEPOSX(\"ZZSPR\")}/{SPRITEPOSY(\"ZZSPR\")}]\n",
        )
        .load_resources()
        .expect("[4/5/6/7]")],
    ));
    rows.push((
        "resources · Animated sprite (ANIME header + frame rows)".to_owned(),
        vec![whole(
            "SPRITECREATED/WIDTH/HEIGHT of `ZZANI,ANIME,4,5` + one frame",
            "@SYSTEM_TITLE\nPRINTFORML [{SPRITECREATED(\"ZZANI\")}\
             /{SPRITEWIDTH(\"ZZANI\")}/{SPRITEHEIGHT(\"ZZANI\")}]\n",
        )
        .load_resources()
        .expect("[1/4/5]")],
    ));

    // --- CSV-backed array variables ------------------------------------
    for (var, source, is_str) in CSV_BACKED {
        rows.push(csv_backed_row(var, source, *is_str));
    }

    rows
}

// ---------------------------------------------------------------------------
// index.md extraction
// ---------------------------------------------------------------------------

/// Expands the wiki's `NAME(|A|B)(|C)` alternation masks.
fn expand_mask(head: &str) -> Vec<String> {
    let mut acc = vec![String::new()];
    let mut rest = head;
    while let Some(open) = rest.find('(') {
        let (lit, tail) = rest.split_at(open);
        let close = match tail.find(')') {
            Some(c) => c,
            None => break,
        };
        let alts: Vec<&str> = tail[1..close].split('|').collect();
        acc = acc
            .into_iter()
            .flat_map(|p| {
                alts.iter()
                    .map(move |a| format!("{p}{lit}{a}"))
                    .collect::<Vec<_>>()
            })
            .collect();
        rest = &tail[close + 1..];
    }
    for p in acc.iter_mut() {
        p.push_str(rest);
    }
    acc.retain(|p| !p.is_empty());
    acc
}

fn backticked(line: &str) -> Option<&str> {
    let rest = line.strip_prefix("- `")?;
    let end = rest.find('`')?;
    Some(&rest[..end])
}

struct Index {
    /// name -> the wiki's own argument text (for the report)
    instructions: BTreeMap<String, String>,
    /// name -> synthesised argument list from the wiki signature
    functions: BTreeMap<String, (String, String)>,
    variables: BTreeMap<String, String>,
    sharps: Vec<String>,
    squares: Vec<String>,
    configs: Vec<String>,
    debugs: Vec<String>,
}

fn parse_index(text: &str) -> Index {
    let mut ix = Index {
        instructions: BTreeMap::new(),
        functions: BTreeMap::new(),
        variables: BTreeMap::new(),
        sharps: Vec::new(),
        squares: Vec::new(),
        configs: Vec::new(),
        debugs: Vec::new(),
    };
    let mut section = ' ';
    for line in text.lines() {
        if let Some(rest) = line.strip_prefix("## (") {
            section = rest.chars().next().unwrap();
            continue;
        }
        match section {
            'a' => {
                if let Some(item) = backticked(line) {
                    let head = item.split_whitespace().next().unwrap_or(item);
                    let args = item[head.len()..].trim().to_owned();
                    for n in expand_mask(head) {
                        ix.instructions.entry(n).or_insert_with(|| args.clone());
                    }
                } else if let Some(rest) = line.strip_prefix("- ") {
                    // eramaerb's bare-name bullets have no backticks in some rows
                    let n = rest.split(" — ").next().unwrap_or(rest).trim();
                    if !n.is_empty() && n.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
                        ix.instructions.entry(n.to_owned()).or_default();
                    }
                }
            }
            'b' => {
                if let Some(item) = backticked(line) {
                    if let Some((name, args, sig)) = parse_signature(item) {
                        ix.functions.insert(name, (args, sig));
                    }
                }
            }
            'c' => {
                if let Some(rest) = line.strip_prefix("| ") {
                    let mut cols = rest.split(" | ");
                    let name = cols.next().unwrap_or("").trim();
                    let ty = cols.next().unwrap_or("").trim_end_matches(" |").trim();
                    // `(heading only)` marks a wiki group heading such as
                    // `LASTLOAD_`, which stands over LASTLOAD_NO/TEXT/VERSION
                    // and is not itself a variable (index.md:539).
                    if name.chars().all(|c| c.is_ascii_uppercase() || c == '_')
                        && !name.is_empty()
                        && ty != "type / source"
                        && !ty.starts_with("(heading only)")
                    {
                        ix.variables.insert(name.to_owned(), ty.to_owned());
                    }
                } else if let Some(rest) = line.strip_prefix("From **eramavar** (eramaker-era list") {
                    if let Some((_, names)) = rest.split_once("): ") {
                        for n in names.split(", ") {
                            let n = n.trim();
                            if !n.is_empty() {
                                ix.variables
                                    .entry(n.to_owned())
                                    .or_insert_with(|| "eramavar".to_owned());
                            }
                        }
                    }
                }
            }
            'd' => {
                if let Some(item) = backticked(line) {
                    let head = item.split_whitespace().next().unwrap_or(item);
                    if head.starts_with('#') {
                        if !ix.sharps.iter().any(|s| s == head) {
                            ix.sharps.push(head.to_owned());
                        }
                    } else if head.starts_with('[') {
                        let n = item.trim_start_matches('[').trim_end_matches(']');
                        let n = n.split_whitespace().next().unwrap_or(n);
                        if !ix.squares.iter().any(|s| s == n) {
                            ix.squares.push(n.to_owned());
                        }
                    }
                }
            }
            'e' => {
                if let Some(rest) = line.strip_prefix("- ") {
                    if let Some(key) = rest.strip_suffix(" — config") {
                        ix.configs.push(key.to_owned());
                    }
                }
            }
            'f' => {
                if let Some(item) = backticked(line) {
                    ix.debugs.push(item.to_owned());
                }
            }
            _ => {}
        }
    }
    ix
}

/// `int STRFIND(str str, str find, int start = 0)` →
/// `("STRFIND", "\"X\", \"X\", 0", the verbatim signature)`.
fn parse_signature(item: &str) -> Option<(String, String, String)> {
    let open = item.find('(')?;
    let close = item.rfind(')')?;
    let head = item[..open].trim();
    let name = head.rsplit_once(' ')?.1.trim().to_owned();
    let params = &item[open + 1..close];
    let mut args = Vec::new();
    for p in params.split(',') {
        let p = p.trim();
        if p.is_empty() {
            continue;
        }
        let ty = p.split_whitespace().next().unwrap_or(p);
        args.push(match ty {
            "int" => "0",
            "str" | "string" => "\"X\"",
            "var" => "FLAG",
            _ => "0",
        });
    }
    Some((name, args.join(", "), item.to_owned()))
}

// ---------------------------------------------------------------------------
// the test
// ---------------------------------------------------------------------------

struct Row {
    name: String,
    /// Set when the "absent" verdict is an artefact of the wiki index rather
    /// than a missing feature; such rows are reported as `n/a` and excluded
    /// from the gap set.
    noise: Option<&'static str>,
    note: String,
    verdict: Verdict,
    probe: String,
    stage: &'static str,
    detail: String,
}

impl Row {
    fn tag(&self) -> &'static str {
        if self.noise.is_some() {
            "n/a"
        } else {
            self.verdict.tag()
        }
    }

    /// A row that counts against coverage.
    fn is_gap(&self) -> bool {
        self.verdict == Verdict::Absent && self.noise.is_none()
    }
}

fn best(runner: &Runner, name: &str, probes: Vec<Probe>) -> Row {
    let mut best: Option<Row> = None;
    for p in probes {
        let o = runner.exec(&p);
        if let Outcome::Panic(msg) = &o {
            runner
                .panics
                .borrow_mut()
                .push((name.to_owned(), p.shown.clone(), msg.clone()));
        }
        let (verdict, detail) = classify(name, &p, &o);
        let row = Row {
            name: name.to_owned(),
            noise: None,
            note: String::new(),
            verdict,
            probe: p.shown,
            stage: o.stage(),
            detail,
        };
        let improved = best.as_ref().map_or(true, |b| row.verdict > b.verdict);
        let decisive = row.verdict == Verdict::Ran;
        if improved {
            best = Some(row);
        }
        if decisive {
            break;
        }
        // `[lexer] Unknown line` is name-only: no argument list can change it.
        if let Some(b) = best.as_ref() {
            if b.verdict == Verdict::Absent && b.detail.contains("[lexer] Unknown") {
                break;
            }
        }
    }
    best.unwrap()
}

fn section_table(out: &mut String, title: &str, rows: &[Row]) {
    let _ = writeln!(out, "## {title}\n");
    let ran = rows.iter().filter(|r| r.verdict == Verdict::Ran).count();
    let present = rows.iter().filter(|r| r.verdict == Verdict::Present).count();
    let gaps = rows.iter().filter(|r| r.is_gap()).count();
    let noise = rows.iter().filter(|r| r.noise.is_some()).count();
    let _ = writeln!(
        out,
        "{} rows — **{ran} ran**, {present} present (probe refused), \
         **{gaps} absent**, {noise} n/a (not a name — see the note column).\n",
        rows.len()
    );
    let _ = writeln!(out, "| name | verdict | probe | stage | observed |");
    let _ = writeln!(out, "|---|---|---|---|---|");
    for r in rows {
        let note = r.noise.map(str::to_owned).unwrap_or_else(|| r.note.clone());
        let _ = writeln!(
            out,
            "| `{}`{} | {} | `{}` | {} | {} |",
            r.name,
            if note.is_empty() {
                String::new()
            } else {
                format!(" — {note}")
            },
            r.tag(),
            r.probe.replace('|', "\\|"),
            r.stage,
            clip(&r.detail).replace('|', "\\|"),
        );
    }
    let _ = writeln!(out);
}

/// The harness is only worth its verdicts if it can still say "absent".
/// These controls pin the classifier against names that certainly do and
/// certainly do not exist, so a regression that made every probe look present
/// (or every probe look absent) fails loudly instead of silently reporting
/// full coverage.
#[test]
fn harness_controls() {
    let runner = Runner::new();

    // Absent: nothing in the lexer table, the method table or the variable
    // table answers to these.
    for (name, probes) in [
        ("ZZNOSUCHINSTRUCTION", instruction_probes("ZZNOSUCHINSTRUCTION")),
        ("ZZNOSUCHFUNCTION", function_probes("ZZNOSUCHFUNCTION", Some("0"))),
        ("ZZNOSUCHVARIABLE", variable_probes("ZZNOSUCHVARIABLE")),
    ] {
        let row = best(&runner, name, probes);
        assert_eq!(
            row.verdict,
            Verdict::Absent,
            "control {name} should be absent, got {:?} from `{}`: {}",
            row.verdict,
            row.probe,
            row.detail
        );
    }

    // Present and running.
    for (name, probes) in [
        ("PRINTL", instruction_probes("PRINTL")),
        ("TOSTR", function_probes("TOSTR", Some("0"))),
        ("RESULT", variable_probes("RESULT")),
    ] {
        let row = best(&runner, name, probes);
        assert_eq!(
            row.verdict,
            Verdict::Ran,
            "control {name} should run, got {:?} from `{}`: {}",
            row.verdict,
            row.probe,
            row.detail
        );
    }

    // Present but refused: `SETCOLORBYNAME` exists and rejects the argument.
    // The distinction this asserts — refusal is not absence — is the whole
    // point of the harness.
    let row = best(&runner, "SETCOLORBYNAME", vec![plain("SETCOLORBYNAME \"nosuchcolour\"")]);
    assert_eq!(row.verdict, Verdict::Present, "{}", row.detail);

    // Known harness limitation, pinned so it cannot rot: `parse_print_left`
    // (crates/erars-lexer/src/utils.rs:287-318) discards the unparsed
    // remainder of a `PRINT`-prefixed word instead of rejecting it, so erars
    // silently accepts `PRINTZZNOSUCH` as a plain `PRINT`. Emuera enumerates
    // every PRINT variant as its own `FunctionCode`
    // (GameProc/Function/FunctionIdentifier.cs) and rejects the rest.
    // Consequence: this harness cannot prove absence for any name starting
    // with `PRINT`.
    let row = best(&runner, "PRINTZZNOSUCH", vec![plain("PRINTZZNOSUCH x")]);
    assert_eq!(
        row.verdict,
        Verdict::Ran,
        "PRINT-prefix over-acceptance changed; revisit the note in \
         docs/research/emuera-wiki/coverage.md and §5 of the gap document"
    );
}

/// Names the wiki index lists that are not Emuera names at all. Each one is a
/// scraping artefact, verified by reading `index.md` at the cited line, and is
/// reported as `n/a` rather than counted as a gap.
fn noise(section: char, name: &str, ix: &Index) -> Option<&'static str> {
    match (section, name) {
        // index.md:274 — a tutorial code fence prints this placeholder string.
        ('a', "AIUAIUAIUAIUAIUAIUAIUAIUAIUAIU") => {
            Some("tutorial placeholder text, not an instruction")
        }
        // index.md:120 — the wiki page misspells CSVJUEL.
        ('a', "CSVJULE") => Some("wiki misspelling of `CSVJUEL`, which runs"),
        // The eramaerb list is scraped from tutorial code fences, so plain
        // variable reads appear alongside real instructions.
        ('a', n) if ix.variables.contains_key(n) => {
            Some("a variable, not an instruction; the wiki's eramaerb list scrapes code fences")
        }
        _ => None,
    }
}

#[test]
fn wiki_runtime_coverage() {
    let runner = Runner::new();
    let index_text = std::fs::read_to_string(INDEX).unwrap();
    let ix = parse_index(&index_text);

    let mut report = String::new();
    let _ = writeln!(
        report,
        "# Emuera wiki ⇄ erars **runtime** coverage\n\n\
         Generated by `cargo test -p erars --test wiki_coverage -- --nocapture`;\n\
         do not hand-edit. Names come from `index.md`, mechanically.\n\n\
         Every verdict is a real run through preprocess → parse → compile →\n\
         `FunctionDic` → `TerminalVm::start(@SYSTEM_TITLE)`, never a source-table diff:\n\
         an unknown `NAME(...)` parses fine as `Expr::Method`\n\
         (`crates/erars-compiler/src/parser/expr.rs:540`) and only faults when executed.\n\n\
         * `absent` — the name resolved to nothing: `[lexer] Unknown line`\n\
         (`crates/erars-lexer/src/lib.rs:845`, `:912`), `[lexer] Unknown sharp line` (`:826`),\n\
         `Function X is not exists` (`crates/erars-vm/src/function.rs:337`) or\n\
         `Variable X is not exists` (`crates/erars-vm/src/variable.rs:832`, `:995`).\n\
         * `present` — the name resolved and then refused this probe (arity, type,\n\
         semantics). **An argument error is evidence of presence.**\n\
         * `ran` — the name resolved and the line completed.\n\
         * `n/a` — the wiki index row is not an Emuera name (scraping artefact).\n\n\
         A ladder of probe shapes is tried per name and the best outcome wins, so\n\
         `absent` means *every* shape resolved to nothing.\n\n\
         **Known limitation.** `parse_print_left`\n\
         (`crates/erars-lexer/src/utils.rs:287-318`) discards the unparsed remainder of a\n\
         `PRINT`-prefixed word, so erars accepts `PRINTZZNOSUCH` as a plain `PRINT`.\n\
         This harness therefore cannot prove absence for any `PRINT*` name;\n\
         `harness_controls` pins the behaviour.\n"
    );

    // (a) instructions
    let mut inst_rows: Vec<Row> = Vec::new();
    for name in ix.instructions.keys() {
        let mut row = best(&runner, name, instruction_probes(name));
        if row.verdict == Verdict::Absent {
            row.noise = noise('a', name, &ix);
        }
        inst_rows.push(row);
    }
    section_table(&mut report, "(a) Instructions — line head", &inst_rows);

    // (b) methods
    let mut fn_rows: Vec<Row> = Vec::new();
    for (name, (args, _sig)) in ix.functions.iter() {
        let mut row = best(&runner, name, function_probes(name, Some(args)));
        if row.verdict == Verdict::Absent {
            row.noise = noise('b', name, &ix);
        }
        fn_rows.push(row);
    }
    section_table(&mut report, "(b) In-expression functions", &fn_rows);

    // (c) variables
    let mut var_rows: Vec<Row> = Vec::new();
    for name in ix.variables.keys() {
        let mut row = best(&runner, name, variable_probes(name));
        if row.verdict == Verdict::Absent {
            row.noise = noise('c', name, &ix);
        }
        var_rows.push(row);
    }
    section_table(&mut report, "(c) Variables and constants", &var_rows);

    // (d) preprocessor directives. `Probe::expect` is what makes this section
    // meaningful: an unknown bracket code only warns
    // (`crates/erars-lexer/src/lib.rs:602-680`) and the line is dropped, so a
    // clean run proves nothing on its own.
    let mut sharp_rows: Vec<Row> = Vec::new();
    for name in &ix.sharps {
        sharp_rows.push(best(&runner, name, sharp_probes(name)));
    }
    for name in &ix.squares {
        let mut row = best(&runner, &format!("[{name}]"), square_probes(name));
        row.name = format!("[{name}]");
        sharp_rows.push(row);
    }
    section_table(&mut report, "(d) Preprocessor directives", &sharp_rows);

    // (e) config keys. Emuera's `GETCONFIG` reaches only 26 of the 75 keys the
    // wiki lists; the rest hit the `default` arm of `GetConfigValueInERB`
    // (`Config/ConfigData.cs:552-557`) and are refused there too. A refusal is
    // therefore only a gap for a key in `GETCONFIG_KEYS`.
    let mut config_rows: Vec<Row> = Vec::new();
    let mut over_accepted: Vec<String> = Vec::new();
    for key in &ix.configs {
        let mut row = best(&runner, key, config_probes(key));
        let emuera_reachable = GETCONFIG_KEYS.contains(&key.as_str());
        if !emuera_reachable {
            if row.verdict == Verdict::Absent {
                row.noise = Some("Emuera refuses this key too (`NotAllowGetConfigValue`)");
            } else {
                row.note =
                    "**divergence**: Emuera refuses this key (`NotAllowGetConfigValue`)".to_owned();
                over_accepted.push(key.clone());
            }
        }
        config_rows.push(row);
    }
    section_table(
        &mut report,
        "(e) Config keys — via `GETCONFIG` / `GETCONFIGS`",
        &config_rows,
    );
    let reachable_here = ix
        .configs
        .iter()
        .filter(|k| GETCONFIG_KEYS.contains(&k.as_str()))
        .count();
    let _ = writeln!(
        report,
        "Emuera's `GETCONFIG`/`GETCONFIGS` reach exactly {} config keys: every other key\n\
         falls to the `default` arm of `GetConfigValueInERB` and is refused with\n\
         `NotAllowGetConfigValue` (`Config/ConfigData.cs:485-559`). {} of those {} keys\n\
         appear in this section's list of {}; the remaining {} are `_replace.csv`\n\
         settings and are measured in section (g). So {} of the keys here are refused by\n\
         Emuera too, and a refusal on them is correct — reported `n/a`, not a gap.\n\n\
         erars answers {} key(s) Emuera refuses: it resolves the whole `EraConfigKey`\n\
         space (`crates/erars-compiler/src/parser.rs:787`) instead of Emuera's 26-case\n\
         switch: {}\n",
        GETCONFIG_KEYS.len(),
        reachable_here,
        GETCONFIG_KEYS.len(),
        ix.configs.len(),
        GETCONFIG_KEYS.len() - reachable_here,
        ix.configs.len() - reachable_here,
        over_accepted.len(),
        if over_accepted.is_empty() {
            "—".to_owned()
        } else {
            over_accepted
                .iter()
                .map(|k| format!("`{k}`"))
                .collect::<Vec<_>>()
                .join(", ")
        }
    );

    // (f) debug commands.
    let mut debug_rows: Vec<Row> = Vec::new();
    for name in &ix.debugs {
        debug_rows.push(best(&runner, name, debug_probes(name)));
    }
    section_table(&mut report, "(f) Debug commands", &debug_rows);
    let _ = writeln!(
        report,
        "Emuera runs these in the front end: `PressEnterKey` sees the entered line,\n\
         spots a leading `@`, calls `doSystemCommand`, and leaves the pending input\n\
         request unconsumed (`GameView/EmueraConsole.cs:1103-1110`, `:1321-1390`).\n\
         erars runs them one level lower, in `VmContext::input_redraw`\n\
         (`crates/erars-vm/src/context.rs:161-237`,\n\
         `crates/erars-vm/src/debug_console.rs`), because the VM — not the front\n\
         end — owns the input loop; every front end therefore gets the console.\n\
         Each probe answers an `INPUTS` with the command. The command text alone\n\
         proves nothing, since `doSystemCommand` echoes the line before running it\n\
         (`:1336-1338`); the evidence is what the *script* received. `@CONFIG`,\n\
         `@DEBUG` and `@OUTPUT` leave `RESULTS` holding the answer to the re-issued\n\
         request, and `@EXIT`/`@REBOOT` end the run before the next statement.\n\
         Where the console's effects differ from the Windows Forms originals — the\n\
         read-only `@CONFIG` listing, `@DEBUG` without the variable-watch tab,\n\
         `@REBOOT` requesting a reload nothing re-runs yet — §5.16 of\n\
         `docs/research/2026-09-03-emuera-command-gap.md` names each one.\n"
    );

    // (g) CSV files and column layouts.
    let mut csv_row_out: Vec<Row> = Vec::new();
    for (label, probes) in csv_rows() {
        csv_row_out.push(best(&runner, &label, probes));
    }
    section_table(&mut report, "(g) CSV files & column layouts", &csv_row_out);

    // Panicking probes. Emuera answers each of these with a script error, so a
    // panic is a defect even though the name is present.
    {
        let panics = runner.panics.borrow();
        let mut seen: BTreeSet<(&str, &str)> = BTreeSet::new();
        let uniq: Vec<_> = panics
            .iter()
            .filter(|(n, _, m)| seen.insert((n.as_str(), m.as_str())))
            .collect();
        let _ = writeln!(
            report,
            "## Probes that panicked\n\n\
             {} distinct (name, panic) pairs. A panic on script input is a defect: \
             Emuera answers each of these with a recoverable script error.\n",
            uniq.len()
        );
        let _ = writeln!(report, "| name | probe | panic |");
        let _ = writeln!(report, "|---|---|---|");
        for (n, p, m) in uniq {
            let _ = writeln!(
                report,
                "| `{n}` | `{}` | {} |",
                p.replace('|', "\\|"),
                m.replace('|', "\\|")
            );
        }
        let _ = writeln!(report);
    }

    std::fs::write(REPORT, &report).unwrap();

    let gaps: BTreeSet<&str> = inst_rows
        .iter()
        .chain(fn_rows.iter())
        .chain(var_rows.iter())
        .chain(sharp_rows.iter())
        .chain(config_rows.iter())
        .chain(debug_rows.iter())
        .chain(csv_row_out.iter())
        .filter(|r| r.is_gap())
        .map(|r| r.name.as_str())
        .collect();
    let allowed: BTreeSet<&str> = EXCLUDED.iter().copied().collect();
    let unexpected: Vec<&str> = gaps.difference(&allowed).copied().collect();
    eprintln!("absent: {gaps:?}");
    eprintln!("panicking probes: {}", runner.panics.borrow().len());
    assert!(
        runner.panics.borrow().is_empty(),
        "probes panicked; Emuera answers each of these with a recoverable script error: {:?}",
        runner.panics.borrow()
    );
    assert!(
        unexpected.is_empty(),
        "names absent at runtime and not owned elsewhere: {unexpected:?}"
    );
}
