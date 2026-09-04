//! The `[…]` preprocessor and the `#`-directive diagnostics.
//!
//! Emuera's authority for every expectation here is cited at the test. All of
//! these warnings are its level 1: the line is dropped, the load continues,
//! and the function still compiles — so each test pins both the message and
//! the line it is attributed to, because a warning on the wrong line is how a
//! skipped region silently shifts everything after it.

use std::sync::Arc;

use erars_ast::StrKey;
use erars_compiler::{Bump, HeaderInfo, ParserContext};

/// `StrKey` needs the process-wide interner, and every test here builds one
/// `HeaderInfo`.
fn init() {
    static INIT: std::sync::Once = std::sync::Once::new();
    INIT.call_once(erars_ast::init_interner);
}

/// A context with `TRUE` and `EMPTY_MACRO` defined, the latter with no body —
/// which `#DEFINE` accepts (`GameProc/HeaderFileLoader.cs:184-191`) and which
/// `[IF]` must still treat as defined, since `GetMacro` only asks the
/// dictionary for the name (`GameData/IdentifierDictionary.cs:470-477`).
fn ctx(debug_mode: bool) -> ParserContext<'static> {
    init();

    let mut info = HeaderInfo::default();
    info.merge_header("#DEFINE TRUE 1").unwrap();
    info.merge_header("#DEFINE EMPTY_MACRO").unwrap();

    ParserContext::new(Arc::new(info), StrKey::new("test.erb")).with_debug(debug_mode)
}

/// Every diagnostic of one source, as `(message, 1-based line)`.
fn diagnostics(src: &str, debug_mode: bool) -> (Vec<(String, usize)>, Vec<(String, usize)>) {
    let ctx = ctx(debug_mode);
    let mut pp = ctx.preprocessor(src);
    let mut b = Bump::new();

    let erb = ctx.parse_and_compile(&mut pp, &mut b).expect("hard parse failure");

    let line_of = |span: std::ops::Range<usize>| src[..span.start].matches('\n').count() + 1;

    (
        erb.warnings
            .into_iter()
            .map(|(m, s, level)| {
                // Every `[…]` diagnostic is one of Emuera's level-1 warnings
                // (`GameProc/ErbLoader.cs:154-171`, `:239-252`), which is what
                // `表示する最低警告レベル:2` filters out.
                assert_eq!(level, 1, "{m}");
                (m, line_of(s))
            })
            .collect(),
        erb.errors.into_iter().map(|(m, s)| (m, line_of(s))).collect(),
    )
}

fn warnings(src: &str) -> Vec<(String, usize)> {
    let (warnings, errors) = diagnostics(src, false);
    assert_eq!(errors, Vec::new(), "expected no error-level diagnostic");
    warnings
}

/// The functions a source defines, by name, in order.
fn function_names(src: &str, debug_mode: bool) -> Vec<String> {
    ctx(debug_mode)
        .parse_program_str(src)
        .unwrap()
        .into_iter()
        .map(|f| f.header.name.resolve().to_string())
        .collect()
}

/// The statement count of a source's single function.
fn body_len(src: &str, debug_mode: bool) -> usize {
    ctx(debug_mode).parse_function_str(src).unwrap().body.len()
}

// ---------------------------------------------------------------------------
// `[…]` regions: what survives
// ---------------------------------------------------------------------------

/// `[IF]` with a defined macro keeps its body; `GetMacro(token2) != null`
/// (`GameProc/ErbLoader.cs:192-200`).
#[test]
fn if_with_a_defined_macro_keeps_the_body() {
    assert_eq!(
        function_names("@MAIN\n[IF TRUE]\n@INSIDE\n[ENDIF]\n@AFTER\n", false),
        ["MAIN", "INSIDE", "AFTER"]
    );
}

/// An empty `#DEFINE` body is still a definition.
#[test]
fn if_with_an_empty_macro_keeps_the_body() {
    assert_eq!(
        function_names("@MAIN\n[IF EMPTY_MACRO]\n@INSIDE\n[ENDIF]\n", false),
        ["MAIN", "INSIDE"]
    );
}

/// An undefined name disables the region, and `[ELSE]` then runs
/// (`Disabled = done`, `GameProc/ErbLoader.cs:222-238`).
#[test]
fn if_else_takes_exactly_one_branch() {
    assert_eq!(
        function_names(
            "@MAIN\n[IF UNDEFINED]\n@THEN\n[ELSE]\n@ELSE\n[ENDIF]\n",
            false
        ),
        ["MAIN", "ELSE"]
    );
    assert_eq!(
        function_names("@MAIN\n[IF TRUE]\n@THEN\n[ELSE]\n@ELSE\n[ENDIF]\n", false),
        ["MAIN", "THEN"]
    );
}

/// `[ELSEIF]` is only reached when no earlier branch was taken: `done` latches
/// (`Disabled = done || GetMacro(token2) == null`).
#[test]
fn elseif_is_skipped_once_a_branch_was_taken() {
    assert_eq!(
        function_names(
            "@MAIN\n[IF TRUE]\n@FIRST\n[ELSEIF TRUE]\n@SECOND\n[ELSE]\n@THIRD\n[ENDIF]\n",
            false
        ),
        ["MAIN", "FIRST"]
    );
    assert_eq!(
        function_names(
            "@MAIN\n[IF UNDEFINED]\n@FIRST\n[ELSEIF TRUE]\n@SECOND\n[ELSE]\n@THIRD\n[ENDIF]\n",
            false
        ),
        ["MAIN", "SECOND"]
    );
}

/// A nested region restores the enclosing state at `[ENDIF]` — and a nested
/// `[IF]` whose macro *is* defined re-enables lines inside a disabled region.
/// DELIBERATE-looking but faithful: `AddKeyWord` assigns
/// `Disabled = GetMacro(token2) == null` outright (`ErbLoader.cs:205-206`),
/// never `|=`, and only `skip` forces the region shut (`:278-279`). So the
/// nested body is read and the outer `[ENDIF]` puts the disable back.
#[test]
fn a_nested_if_is_independent_of_the_enclosing_state() {
    assert_eq!(
        function_names(
            "@MAIN\n[IF UNDEFINED]\n[IF TRUE]\n@NESTED\n[ENDIF]\n@OUTER\n[ENDIF]\n@AFTER\n",
            false
        ),
        ["MAIN", "NESTED", "AFTER"]
    );
    // A `[SKIPSTART]` is the one thing that does keep it shut.
    assert_eq!(
        function_names(
            "@MAIN\n[SKIPSTART]\n[IF TRUE]\n@NESTED\n[ENDIF]\n[SKIPEND]\n@AFTER\n",
            false
        ),
        ["MAIN", "AFTER"]
    );
    assert_eq!(
        function_names(
            "@MAIN\n[IF TRUE]\n[IF UNDEFINED]\n@NESTED\n[ENDIF]\n@OUTER\n[ENDIF]\n@AFTER\n",
            false
        ),
        ["MAIN", "OUTER", "AFTER"]
    );
}

/// `[SKIPSTART]` disables everything up to `[SKIPEND]`, and nothing — not even
/// `[ENDIF]` — re-enables lines while it is open (`if (skip) Disabled = true;`
/// closes every case of `AddKeyWord`).
#[test]
fn skipstart_wins_over_every_other_directive() {
    assert_eq!(
        function_names(
            "@MAIN\n[SKIPSTART]\n@DROPPED\n[IF TRUE]\n@ALSO_DROPPED\n[ENDIF]\n[SKIPEND]\n@AFTER\n",
            false
        ),
        ["MAIN", "AFTER"]
    );
}

/// `[IF_DEBUG]`/`[IF_NDEBUG]` are the same region, keyed on debug mode
/// (`Disabled = !DebugMode` / `Disabled = DebugMode`).
#[test]
fn if_debug_follows_the_debug_flag() {
    let src = "@MAIN\n[IF_DEBUG]\n@DEBUG_ONLY\n[ELSE]\n@RELEASE_ONLY\n[ENDIF]\n";
    assert_eq!(function_names(src, false), ["MAIN", "RELEASE_ONLY"]);
    assert_eq!(function_names(src, true), ["MAIN", "DEBUG_ONLY"]);

    let src = "@MAIN\n[IF_NDEBUG]\n@RELEASE_ONLY\n[ENDIF]\n";
    assert_eq!(function_names(src, false), ["MAIN", "RELEASE_ONLY"]);
    assert_eq!(function_names(src, true), ["MAIN"]);
}

// ---------------------------------------------------------------------------
// `;!;` and `;#;`
// ---------------------------------------------------------------------------

/// `;!;` is unconditional: the marker is dropped and what follows is code,
/// whether it opens the line or sits mid-line
/// (`Sub/LexicalAnalyzer.cs:753-765`).
#[test]
fn bang_marker_is_always_code() {
    assert_eq!(body_len("@MAIN\n;!;PRINTL kept\nPRINTL after\n", false), 2);
    assert_eq!(body_len("@MAIN\nPRINTL a ;!; + 1\n", false), 1);
    assert_eq!(
        function_names("@MAIN\n;!;@SECOND\nPRINTL x\n", false),
        ["MAIN", "SECOND"]
    );
}

/// `;#;` is code only in debug mode; otherwise the `;` starts an ordinary
/// comment and the rest of the line disappears.
#[test]
fn hash_marker_follows_the_debug_flag() {
    let src = "@MAIN\nPRINTL a\n;#;PRINTL debug\n";
    assert_eq!(body_len(src, false), 1);
    assert_eq!(body_len(src, true), 2);
}

// ---------------------------------------------------------------------------
// `[…]` diagnostics
// ---------------------------------------------------------------------------

/// `対応する[IF]のない[ENDIF]です` — the pop happens before the check and is
/// not undone (`GameProc/ErbLoader.cs:262-273`).
#[test]
fn stray_endif_warns_on_its_own_line() {
    assert_eq!(
        warnings("@MAIN\nPRINTL a\n[ENDIF]\nPRINTL b\n"),
        [("대응하는 [IF]가 없는 [ENDIF]입니다".to_string(), 3)]
    );
}

/// `[SKIPSTART]と対応しない[SKIPEND]です` (`:250-259`).
#[test]
fn stray_skipend_warns_on_its_own_line() {
    assert_eq!(
        warnings("@MAIN\n[SKIPEND]\n"),
        [("[SKIPSTART]와 대응하지 않는 [SKIPEND]입니다".to_string(), 2)]
    );
}

/// `[SKIPSTART]が重複して使用されています` (`:160-163`): the second one is
/// ignored, so the first `[SKIPEND]` still closes the region.
#[test]
fn duplicate_skipstart_warns_once() {
    assert_eq!(
        warnings("@MAIN\n[SKIPSTART]\ndropped\n[SKIPSTART]\n[SKIPEND]\n@AFTER\n"),
        [("[SKIPSTART]가 중복되어 사용됐습니다".to_string(), 4)]
    );
}

/// `[{0}]がありません` — `PPState.FileEnd` (`:282-296`) reports one open
/// region, whatever the depth. DELIBERATE: Emuera attributes it to line `-1`
/// of the file (`:436`), erars to the end of the source, because its warning
/// channel is a byte span.
#[test]
fn an_unterminated_region_is_reported_at_end_of_file() {
    assert_eq!(
        warnings("@MAIN\n[SKIPSTART]\ndropped\n"),
        [("[SKIPEND]가 없습니다".to_string(), 4)]
    );
    // `ELSEIF` is rewritten to `ENDIF`: the source is missing the closer, not
    // another branch.
    assert_eq!(
        warnings("@MAIN\n[IF TRUE]\nPRINTL a\n"),
        [("[ENDIF]가 없습니다".to_string(), 4)]
    );
    // Two open regions, one warning.
    assert_eq!(
        warnings("@MAIN\n[IF TRUE]\n[IF TRUE]\nPRINTL a\n"),
        [("[ENDIF]가 없습니다".to_string(), 5)]
    );
}

/// `"{0}"に引数がありません` (`:189-192`) and `"{0}"に余分な引数があります`
/// (`:156-159`), both with the bare token.
#[test]
fn missing_and_extra_directive_arguments() {
    assert_eq!(
        warnings("@MAIN\n[IF]\nPRINTL a\n"),
        [("\"IF\"에 인수가 없습니다".to_string(), 2)]
    );
    assert_eq!(
        warnings("@MAIN\n[SKIPSTART TRUE]\nPRINTL a\n"),
        [("\"SKIPSTART\"에 여분의 인수가 있습니다".to_string(), 2)]
    );
    // `[ELSE]` takes none, and the extra argument stops it closing the branch.
    assert_eq!(
        warnings("@MAIN\n[IF TRUE]\n@THEN\n[ELSE TRUE]\n@ELSE\n[ENDIF]\n"),
        [("\"ELSE\"에 여분의 인수가 있습니다".to_string(), 4)]
    );
}

/// `不適切な[ELSE]です` (`:222-227`): no region is open, so there is nothing
/// for it to close.
#[test]
fn else_without_if_is_invalid() {
    assert_eq!(
        warnings("@MAIN\n[ELSE]\nPRINTL a\n"),
        [("부적절한 [ELSE]입니다".to_string(), 2)]
    );
    assert_eq!(
        warnings("@MAIN\n[ELSEIF TRUE]\nPRINTL a\n"),
        [("부적절한 [ELSEIF]입니다".to_string(), 2)]
    );
}

/// `[{0}]の後ろは無視されます。` (`GameProc/ErbLoader.cs:337-341`): the
/// end-of-line test after the `]` is strict, so even a trailing comment is
/// text after the directive.
#[test]
fn text_after_the_bracket_warns() {
    assert_eq!(
        warnings("@MAIN\n[IF TRUE] junk\n@INSIDE\n[ENDIF]\n"),
        [("[IF] 뒤는 무시됩니다".to_string(), 2)]
    );
    assert_eq!(
        warnings("@MAIN\n[IF TRUE];comment\n@INSIDE\n[ENDIF]\n"),
        [("[IF] 뒤는 무시됩니다".to_string(), 2)]
    );
}

/// `認識できないプリプロセッサです` (`:274-277`). Emuera's switch is over the
/// token as written, so a lowercase directive is not one.
#[test]
fn an_unknown_or_lowercase_directive_is_unrecognised() {
    assert_eq!(
        warnings("@MAIN\n[iflol]\nPRINTL a\n"),
        [("인식할 수 없는 전처리기입니다".to_string(), 2)]
    );
    assert_eq!(
        warnings("@MAIN\n[endif]\nPRINTL a\n"),
        [("인식할 수 없는 전처리기입니다".to_string(), 2)]
    );
}

/// `[]の使い方が不正です` (`:330-334`). The keyword still runs afterwards, so
/// `[IF TRUE` opens a region — and leaves it open at end of file.
#[test]
fn a_missing_bracket_is_reported_but_still_read() {
    assert_eq!(
        warnings("@MAIN\n[IF TRUE\n@INSIDE\n"),
        [
            ("[]의 사용법이 잘못됐습니다".to_string(), 2),
            ("[ENDIF]가 없습니다".to_string(), 4),
        ]
    );
}

/// An ERH is `#` lines and nothing else
/// (`GameProc/HeaderFileLoader.cs:96-103`), so the `[…]` preprocessor is not
/// read there at all.
#[test]
fn a_bracket_line_in_a_header_is_an_error() {
    let ctx = ctx(false);
    let mut info = HeaderInfo::default();
    let err = info.merge_header("[IF TRUE]\n#DEFINE X 1\n[ENDIF]\n").unwrap_err();
    assert_eq!(err.0, "헤더에 #으로 시작하지 않는 행이 있습니다");
    drop(ctx);
}

// ---------------------------------------------------------------------------
// line numbers
// ---------------------------------------------------------------------------

/// Every physical line of a skipped region is counted
/// (`Sub/EraStreamReader.ReadEnabledLine`), so a diagnostic after one lands on
/// its real line.
#[test]
fn a_skipped_region_does_not_shift_later_lines() {
    let src = "@MAIN\n[SKIPSTART]\na\n\nb\n\n\nc\n[SKIPEND]\n[ENDIF]\n";
    assert_eq!(
        warnings(src),
        [("대응하는 [IF]가 없는 [ENDIF]입니다".to_string(), 10)]
    );
}

// ---------------------------------------------------------------------------
// `#` directives
// ---------------------------------------------------------------------------

/// `#LOCALSIZE` is ignored on an event function, with
/// `イベント関数では#{0}による{1}のサイズ指定は無視されます`
/// (`GameProc/LogicalLineParser.cs:207-212`).
#[test]
fn localsize_on_an_event_function_is_ignored() {
    assert_eq!(
        warnings("@EVENTFIRST\n#LOCALSIZE 100\nPRINTL a\n"),
        [(
            "이벤트 함수에서는 #LOCALSIZE에 의한 LOCAL 크기 지정이 무시됩니다".to_string(),
            2
        )]
    );
    assert_eq!(
        warnings("@EVENTFIRST\n#LOCALSSIZE 100\nPRINTL a\n"),
        [(
            "이벤트 함수에서는 #LOCALSSIZE에 의한 LOCALS 크기 지정이 무시됩니다".to_string(),
            2
        )]
    );
}

/// `#{0}に0以下の値({1})が与えられました` (`:225-229`) and
/// `#{0}に大きすぎる値({1})が与えられました` (`:230-234`): the directive is
/// dropped and the `!VariableSize.csv` default stands.
#[test]
fn localsize_out_of_range_is_ignored() {
    assert_eq!(
        warnings("@MAIN\n#LOCALSIZE 0\nPRINTL a\n"),
        [("#LOCALSIZE에 0 이하의 값(0)이 지정됐습니다. 설정은 무시됩니다".to_string(), 2)]
    );
    assert_eq!(
        warnings("@MAIN\n#LOCALSIZE -1\nPRINTL a\n"),
        [("#LOCALSIZE에 0 이하의 값(-1)이 지정됐습니다. 설정은 무시됩니다".to_string(), 2)]
    );
    assert_eq!(
        warnings("@MAIN\n#LOCALSIZE 2147483647\nPRINTL a\n"),
        [(
            "#LOCALSIZE에 너무 큰 값(2147483647)이 지정됐습니다. 설정은 무시됩니다".to_string(),
            2
        )]
    );
}

/// `この関数にはすでに#LOCALSIZEが定義されています。（以前の定義は無視されます）`
/// (`:219-224`): the last one wins.
#[test]
fn duplicate_localsize_warns_and_the_last_wins() {
    assert_eq!(
        warnings("@MAIN\n#LOCALSIZE 10\n#LOCALSIZE 20\nPRINTL a\n"),
        [(
            "이 함수에는 이미 #LOCALSIZE 정의가 있습니다(이전 정의는 무시됩니다)".to_string(),
            3
        )]
    );
}

/// `#{0}の後に有効な数値が指定されていません` (`:202-206`, `:213-218`) is
/// level 2, and the function still compiles.
#[test]
fn localsize_without_a_constant_is_an_error() {
    for src in [
        "@MAIN\n#LOCALSIZE\nPRINTL a\n",
        "@MAIN\n#LOCALSIZE LOCAL\nPRINTL a\n",
    ] {
        let (warnings, errors) = diagnostics(src, false);
        assert_eq!(warnings, Vec::new());
        assert_eq!(
            errors,
            [("#LOCALSIZE 뒤에 유효한 수치가 지정되지 않았습니다".to_string(), 2)]
        );
    }
}

/// `#PRI`/`#LATER`/`#SINGLE`/`#ONLY` only mean anything on an event function
/// (`:36-144`).
#[test]
fn event_flags_outside_an_event_function_warn() {
    assert_eq!(
        warnings("@MAIN\n#PRI\nPRINTL a\n"),
        [("이벤트 함수 이외에서 #PRI 지정은 동작하지 않습니다".to_string(), 2)]
    );
    assert_eq!(
        warnings("@MAIN\n#ONLY\nPRINTL a\n"),
        [("이벤트 함수 이외에서 #ONLY 지정은 동작하지 않습니다".to_string(), 2)]
    );
    assert_eq!(
        warnings("@MAIN\n#FUNCTION\n#SINGLE\nPRINTL a\n"),
        [("식중 함수에서 #SINGLE 지정은 동작하지 않습니다".to_string(), 3)]
    );
}

/// A repeated flag warns and changes nothing (`:41-44`, `:64-67`, `:89-92`,
/// `:114-117`).
#[test]
fn duplicate_event_flags_warn() {
    assert_eq!(
        warnings("@EVENTFIRST\n#PRI\n#PRI\nPRINTL a\n"),
        [("#PRI 지정이 중복됐습니다".to_string(), 3)]
    );
    assert_eq!(
        warnings("@EVENTFIRST\n#ONLY\n#ONLY\nPRINTL a\n"),
        [("#ONLY 지정이 중복됐습니다".to_string(), 3)]
    );
}

/// `#PRIと#LATERが重複して使われています(この関数は2度呼ばれます)` (`:70-74`).
#[test]
fn pri_with_later_warns() {
    assert_eq!(
        warnings("@EVENTFIRST\n#PRI\n#LATER\nPRINTL a\n"),
        [(
            "#PRI와 #LATER가 중복 지정됐습니다(Emuera에서는 이 함수가 두 번 호출됩니다)"
                .to_string(),
            3
        )]
    );
}

/// `#ONLY` clears the flags declared before it (`:135-143`) and the flags
/// declared after it are dead (`:52-56`, `:75-79`, `:100-104`).
#[test]
fn only_replaces_the_other_flags() {
    assert_eq!(
        warnings("@EVENTFIRST\n#PRI\n#ONLY\nPRINTL a\n"),
        [("이 이벤트 함수의 #PRI 선언은 무시됩니다".to_string(), 3)]
    );
    assert_eq!(
        warnings("@EVENTFIRST\n#ONLY\n#PRI\nPRINTL a\n"),
        [(
            "#ONLY가 지정된 이벤트 함수에서 #PRI 지정은 동작하지 않습니다".to_string(),
            3
        )]
    );
}

/// `#FUNCTION` twice is level 1 and the line is dropped (`:156-160`); the
/// other one is level 2 (`:161-165`). Either way `is_function` and
/// `is_functions` can never both be set.
#[test]
fn function_declared_twice() {
    assert_eq!(
        warnings("@MAIN\n#FUNCTION\n#FUNCTION\nPRINTL a\n"),
        [(
            "함수 \"MAIN\"에는 이미 #FUNCTION 선언이 있습니다(이 행은 무시됩니다)".to_string(),
            3
        )]
    );

    let (warnings, errors) = diagnostics("@MAIN\n#FUNCTION\n#FUNCTIONS\nPRINTL a\n", false);
    assert_eq!(warnings, Vec::new());
    assert_eq!(
        errors,
        [("함수 \"MAIN\"에는 이미 #FUNCTION 선언이 있습니다".to_string(), 3)]
    );
}

/// `システム関数に#{0}が指定されています` (`:167-171`), level 2: an event
/// function is never an expression function.
#[test]
fn function_on_an_event_function_is_an_error() {
    let (warnings, errors) = diagnostics("@EVENTFIRST\n#FUNCTION\nPRINTL a\n", false);
    assert_eq!(warnings, Vec::new());
    assert_eq!(
        errors,
        [("시스템 함수에 #FUNCTION 지정이 있습니다".to_string(), 2)]
    );
}

/// A flag rejected outside an event function is never *set*: every guard in
/// `#SINGLE`/`#LATER`/`#PRI`/`#ONLY` `break`s before the assignment
/// (`LogicalLineParser.cs:42-46`, `:65-69`, `:90-94`, `:115-119`). So the
/// clearing block `#FUNCTION` carries for them (`:178-197`) has nothing to
/// clear here — and, since a label that *can* hold a flag is an event
/// function and `#FUNCTION` rejects those at `:167-171` first, that block is
/// unreachable in Emuera too. One warning, on the `#LATER` line.
#[test]
fn a_flag_rejected_outside_an_event_function_is_never_set() {
    assert_eq!(
        warnings("@MAIN\n#LATER\n#FUNCTION\nPRINTL a\n"),
        [("이벤트 함수 이외에서 #LATER 지정은 동작하지 않습니다".to_string(), 2)]
    );
}
