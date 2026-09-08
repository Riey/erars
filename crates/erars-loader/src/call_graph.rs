//! Emuera's load-time call graph (`GameProc/ErbLoader.cs:604-756`, `checkScript`)
//! and the two warning keys it feeds.
//!
//! Emuera walks the function list breadth-first from the labels it gave
//! `Depth = 0` — event functions, its system-label set, and every
//! `#FUNCTION`/`#FUNCTIONS` expression function
//! (`GameProc/LogicalLineParser.cs:172`, `:360-365`) — following the constant
//! targets of `CALL`/`JUMP`-family statements and raising `Depth` as it goes
//! (`GameProc/Function/Instraction.Child.cs:2276-2278`). Three outputs come
//! out of that walk: which functions were reached (`usedLabelCount`, the third
//! figure of the `ロード時にレポートを表示する` report,
//! `GameProc/ErbLoader.cs:753`), which constant targets name nothing
//! (`関数が見つからない警告の扱い`), and which functions nothing reached
//! (`関数が呼び出されなかった警告の扱い`).

use erars_compiler::{CallSite, DisplayWarningFlag, EraConfig};
use erars_ast::StrKey;
use hashbrown::{HashMap, HashSet};

/// One registered function, as the graph sees it.
pub struct GraphNode<'a> {
    pub name: StrKey,
    pub file_path: StrKey,
    /// A `Depth = 0` label: an entry point the engine may call without any
    /// `CALL` naming it.
    pub is_root: bool,
    pub calls: &'a [CallSite],
}

/// What the walk produced. Diagnostics are `(level, file, line, message)` in
/// the order Emuera queues them: the not-found warnings of the reachable set
/// first, then the not-called warnings, then the two ignored-count summaries
/// (`GameProc/ErbLoader.cs:747-752`).
#[derive(Default)]
pub struct CallAnalysis {
    /// `usedLabelCount`: functions the walk reached, roots included.
    pub called_count: usize,
    /// Emuera's `useCallForm`: some call target is computed, so any function
    /// may be called and the uncalled check is abandoned entirely
    /// (`GameProc/ErbLoader.cs:667-676`).
    pub dynamic_target: bool,
    pub diagnostics: Vec<(u8, StrKey, u32, String)>,
}

/// Emuera's system-label set (`GameData/IdentifierDictionary.cs:65-107`): a
/// label the engine itself calls, so it is a graph root even though no script
/// line names it.
pub fn is_system_label_name(name: &str) -> bool {
    if matches!(
        name,
        "EVENTFIRST"
            | "EVENTTRAIN"
            | "EVENTSHOP"
            | "EVENTBUY"
            | "EVENTCOM"
            | "EVENTTURNEND"
            | "EVENTCOMEND"
            | "EVENTEND"
            | "SHOW_STATUS"
            | "SHOW_USERCOM"
            | "USERCOM"
            | "SOURCE_CHECK"
            | "CALLTRAINEND"
            | "SHOW_JUEL"
            | "SHOW_ABLUP_SELECT"
            | "USERABLUP"
            | "SHOW_SHOP"
            | "SAVEINFO"
            | "USERSHOP"
            | "EVENTLOAD"
            | "TITLE_LOADGAME"
            | "SYSTEM_AUTOSAVE"
            | "SYSTEM_TITLE"
            | "SYSTEM_LOADEND"
    ) {
        return true;
    }

    // `^COM[0-9]+$`, `^COM_ABLE[0-9]+$`, `^ABLUP[0-9]+$`
    // (`GameData/IdentifierDictionary.cs:43-45`) — the train commands and
    // ability-up handlers the engine dispatches by number.
    let numbered = |rest: &str| !rest.is_empty() && rest.bytes().all(|b| b.is_ascii_digit());

    if let Some(rest) = name.strip_prefix("COM_ABLE") {
        return numbered(rest);
    }
    if let Some(rest) = name.strip_prefix("COM") {
        return numbered(rest);
    }
    if let Some(rest) = name.strip_prefix("ABLUP") {
        return numbered(rest);
    }

    false
}

/// One `DisplayWarningFlag` decision. `ONCE` reports the first occurrence per
/// source file and counts the rest (`GameProc/ErbLoader.cs:687-704` for the
/// not-called side, `:802-818` for the not-found side); `IGNORE` counts every
/// one. `LATER` differs between the two keys and is handled by the callers.
fn report_once_per_file(seen: &mut HashSet<StrKey>, file: StrKey) -> bool {
    seen.insert(file)
}

/// Walks the graph. `defined` answers whether a name is a registered function,
/// which is what decides a not-found warning: it includes event functions and
/// anything registered by `イベント関数のCALLを許可する`, exactly like
/// `CalledFunction.CallFunction` (`Instraction.Child.cs:2269`).
pub fn analyze(nodes: &[GraphNode<'_>], config: &EraConfig) -> CallAnalysis {
    let min_level = config.display_warning_level;
    let not_found_flag = config.function_not_found_warning;
    let not_called_flag = config.function_not_called_warning;

    let mut out = CallAnalysis::default();

    // Several nodes may share a name — event functions are registered as a
    // collection, and `CALL` reaches the first of them. Reachability is per
    // name, as Emuera's `labelDic` lookup is.
    let mut by_name: HashMap<StrKey, Vec<usize>> = HashMap::with_capacity(nodes.len());
    for (idx, node) in nodes.iter().enumerate() {
        by_name.entry(node.name).or_default().push(idx);
    }

    let mut reached = vec![false; nodes.len()];
    let mut queue = Vec::new();

    for (idx, node) in nodes.iter().enumerate() {
        if node.is_root {
            reached[idx] = true;
            queue.push(idx);
        }
    }

    // Breadth-first, exactly Emuera's depth loop: a function is analysed once,
    // when it is first reached.
    let mut cursor = 0;
    while cursor < queue.len() {
        let idx = queue[cursor];
        cursor += 1;

        for call in nodes[idx].calls {
            let Some(target) = call.target else {
                out.dynamic_target = true;
                continue;
            };

            match by_name.get(&target) {
                Some(targets) => {
                    for &t in targets {
                        if !reached[t] {
                            reached[t] = true;
                            queue.push(t);
                        }
                    }
                }
                None => (),
            }
        }
    }

    out.called_count = queue.len();

    // Not-found warnings. Emuera raises them from the analysis pass, so a
    // function that was never analysed never produces any: the reachable set
    // always, the rest only when `呼び出されなかった関数を無視する` is off, or
    // when a computed target made everything reachable
    // (`GameProc/ErbLoader.cs:667-676`, `:704-706`).
    let analyse_unreached = out.dynamic_target || !config.ignore_uncalled_function;
    let mut not_found_seen = HashSet::new();
    let mut ignored_not_found = 0usize;

    for (idx, node) in nodes.iter().enumerate() {
        if !reached[idx] && !analyse_unreached {
            continue;
        }

        for call in node.calls {
            let Some(target) = call.target else { continue };
            // `!func.Function.IsTry()`: a `TRY*` form is allowed to miss
            // (`Instraction.Child.cs:2269-2273`).
            if call.is_try || by_name.contains_key(&target) {
                continue;
            }

            // `printFunctionNotFoundWarning` drops the warning before it
            // counts it when the level is below the configured one
            // (`GameProc/ErbLoader.cs:792-793`).
            if 2 < min_level {
                continue;
            }

            let report = match not_found_flag {
                DisplayWarningFlag::Ignore => false,
                DisplayWarningFlag::Once => {
                    report_once_per_file(&mut not_found_seen, node.file_path)
                }
                // `LATER` is not a case of `printFunctionNotFoundWarning`'s
                // chain (`GameProc/ErbLoader.cs:797-818`), so `ignore` stays
                // false and the warning is reported — every warning is queued
                // and flushed at the end of the load anyway
                // (`GameData/ParserMediator.cs:143-159`,
                // `GameProc/ErbLoader.cs:752`), which is what makes `LATER`
                // and `DISPLAY` indistinguishable on this key.
                DisplayWarningFlag::Later | DisplayWarningFlag::Display => true,
            };

            if !report {
                ignored_not_found += 1;
                continue;
            }

            out.diagnostics.push((
                2,
                node.file_path,
                call.line,
                format!("지정된 함수명 \"@{target}\"은 존재하지 않습니다"),
            ));
        }
    }

    // Not-called warnings, skipped wholesale when a computed target was seen.
    let mut ignored_not_called = 0usize;

    if !out.dynamic_target {
        // `IGNORE` and `LATER` both suppress the per-function warning
        // (`ignoreAll`, `GameProc/ErbLoader.cs:658-665`); `LATER` still gets
        // the summary count below, which is the whole difference between them.
        let ignore_all = matches!(
            not_called_flag,
            DisplayWarningFlag::Ignore | DisplayWarningFlag::Later
        );
        let mut not_called_seen = HashSet::new();

        for (idx, node) in nodes.iter().enumerate() {
            if reached[idx] {
                continue;
            }

            let ignore = ignore_all
                || match not_called_flag {
                    DisplayWarningFlag::Once => {
                        !report_once_per_file(&mut not_called_seen, node.file_path)
                    }
                    _ => false,
                };

            if ignore || 1 < min_level {
                ignored_not_called += 1;
                continue;
            }

            out.diagnostics.push((
                1,
                node.file_path,
                0,
                format!("함수 @{}은 정의되어 있지만 한 번도 호출되지 않습니다", node.name),
            ));
        }
    }

    // The two summary lines. Both are gated on the *not-called* flag in the
    // source, including the not-found one — `GameProc/ErbLoader.cs:747-750`
    // tests `notCalledWarning` twice. Mirrored deliberately: it is how real
    // Emuera behaves, and a game that sets only `関数が見つからない警告の扱い`
    // to `IGNORE` therefore still gets the level-2 count line.
    if ignored_not_called > 0 && min_level <= 1 && not_called_flag != DisplayWarningFlag::Ignore {
        out.diagnostics.push((
            1,
            StrKey::new(""),
            0,
            format!("경고Lv1: 정의된 함수가 한 번도 호출되지 않은 것에 관한 경고를 {ignored_not_called}건 무시했습니다"),
        ));
    }
    if ignored_not_found > 0 && min_level <= 2 && not_called_flag != DisplayWarningFlag::Ignore {
        out.diagnostics.push((
            2,
            StrKey::new(""),
            0,
            format!("경고Lv2: 정의되지 않은 함수를 호출한 것에 관한 경고를 {ignored_not_found}건 무시했습니다"),
        ));
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use erars_compiler::CallSite;

    fn key(s: &str) -> StrKey {
        erars_ast::init_interner();
        StrKey::new(s)
    }

    fn call(target: Option<&str>, is_try: bool) -> CallSite {
        CallSite {
            target: target.map(key),
            line: 7,
            is_try,
        }
    }

    /// `SYSTEM_TITLE` -> `A` -> `B`; `C` is defined and never called; `A` also
    /// calls the undefined `MISSING`.
    fn fixture() -> (Vec<CallSite>, Vec<CallSite>) {
        (vec![call(Some("A"), false)], vec![call(Some("B"), false), call(Some("MISSING"), false)])
    }

    fn nodes<'a>(
        root: &'a [CallSite],
        a: &'a [CallSite],
    ) -> Vec<GraphNode<'a>> {
        vec![
            GraphNode {
                name: key("SYSTEM_TITLE"),
                file_path: key("MAIN.ERB"),
                is_root: true,
                calls: root,
            },
            GraphNode {
                name: key("A"),
                file_path: key("MAIN.ERB"),
                is_root: false,
                calls: a,
            },
            GraphNode {
                name: key("B"),
                file_path: key("MAIN.ERB"),
                is_root: false,
                calls: &[],
            },
            GraphNode {
                name: key("C"),
                file_path: key("OTHER.ERB"),
                is_root: false,
                calls: &[],
            },
        ]
    }

    /// Both keys default to `IGNORE` (`Config/ConfigData.cs:77-78`), so the
    /// walk reports nothing at all — and the reachability count is still
    /// correct, because `display_report` consumes it independently.
    #[test]
    fn defaults_report_nothing_but_still_count() {
        let (root, a) = fixture();
        let analysis = analyze(&nodes(&root, &a), &EraConfig::default());

        assert_eq!(analysis.called_count, 3, "SYSTEM_TITLE, A, B");
        assert!(!analysis.dynamic_target);
        assert!(analysis.diagnostics.is_empty(), "{:?}", analysis.diagnostics);
    }

    /// `関数が見つからない警告の扱い:DISPLAY` reports the constant target that
    /// names nothing (`GameProc/ErbLoader.cs:1466`, level 2).
    #[test]
    fn not_found_display_reports_the_missing_target() {
        let (root, a) = fixture();
        let mut config = EraConfig::default();
        config.function_not_found_warning = DisplayWarningFlag::Display;

        let analysis = analyze(&nodes(&root, &a), &config);
        let found: Vec<_> =
            analysis.diagnostics.iter().filter(|(l, ..)| *l == 2).collect();

        assert_eq!(found.len(), 1, "{:?}", analysis.diagnostics);
        assert!(found[0].3.contains("@MISSING"), "{:?}", found[0]);
        assert_eq!(found[0].2, 7, "the call site's line");
    }

    /// `LATER` is not a branch of `printFunctionNotFoundWarning`'s chain
    /// (`GameProc/ErbLoader.cs:797-818`), so on this key it behaves as
    /// `DISPLAY`.
    #[test]
    fn not_found_later_behaves_as_display() {
        let (root, a) = fixture();
        let mut config = EraConfig::default();
        config.function_not_found_warning = DisplayWarningFlag::Later;

        let analysis = analyze(&nodes(&root, &a), &config);
        assert_eq!(analysis.diagnostics.iter().filter(|(l, ..)| *l == 2).count(), 1);
    }

    /// `ONCE` reports the first occurrence per source file and counts the rest
    /// (`GameProc/ErbLoader.cs:802-818`).
    #[test]
    fn not_found_once_reports_one_per_file() {
        let root = vec![call(Some("A"), false)];
        let a = vec![
            call(Some("MISSING1"), false),
            call(Some("MISSING2"), false),
        ];
        let mut config = EraConfig::default();
        config.function_not_found_warning = DisplayWarningFlag::Once;
        // Otherwise the ignored-count summary is suppressed, which is the
        // `notCalledWarning`-gated oddity at `:747-750`.
        config.function_not_called_warning = DisplayWarningFlag::Later;

        let analysis = analyze(&nodes(&root, &a), &config);
        let messages: Vec<&str> = analysis.diagnostics.iter().map(|d| d.3.as_str()).collect();

        assert_eq!(
            messages.iter().filter(|m| m.contains("존재하지 않습니다")).count(),
            1,
            "{messages:?}"
        );
        assert!(
            messages.iter().any(|m| m.contains("경고Lv2") && m.contains("1건")),
            "{messages:?}"
        );
    }

    /// A `TRY*` call is allowed to miss (`Instraction.Child.cs:2269-2273`).
    #[test]
    fn try_call_never_warns_about_a_missing_target() {
        let root = vec![call(Some("A"), false)];
        let a = vec![call(Some("MISSING"), true)];
        let mut config = EraConfig::default();
        config.function_not_found_warning = DisplayWarningFlag::Display;

        let analysis = analyze(&nodes(&root, &a), &config);
        assert!(analysis.diagnostics.is_empty(), "{:?}", analysis.diagnostics);
    }

    /// `関数が呼び出されなかった警告の扱い:DISPLAY` names the defined-but-unreached
    /// function (`GameProc/ErbLoader.cs:706`, level 1).
    #[test]
    fn not_called_display_names_the_dead_function() {
        let (root, a) = fixture();
        let mut config = EraConfig::default();
        config.function_not_called_warning = DisplayWarningFlag::Display;

        let analysis = analyze(&nodes(&root, &a), &config);
        let called: Vec<_> = analysis.diagnostics.iter().filter(|(l, ..)| *l == 1).collect();

        assert_eq!(called.len(), 1, "{:?}", analysis.diagnostics);
        assert!(called[0].3.contains("@C"), "{:?}", called[0]);
    }

    /// `LATER` suppresses the per-function warning but keeps the summary count
    /// — `ignoreAll` at `:658-665` plus the count line at `:747-748`. That
    /// difference is the entire distinction from `IGNORE`.
    #[test]
    fn not_called_later_only_summarises() {
        let (root, a) = fixture();
        let mut config = EraConfig::default();
        config.function_not_called_warning = DisplayWarningFlag::Later;

        let analysis = analyze(&nodes(&root, &a), &config);
        let messages: Vec<&str> = analysis.diagnostics.iter().map(|d| d.3.as_str()).collect();

        assert!(
            messages.iter().all(|m| !m.contains("@C")),
            "no per-function warning: {messages:?}"
        );
        assert!(
            messages.iter().any(|m| m.contains("경고Lv1") && m.contains("1건")),
            "{messages:?}"
        );
    }

    /// A computed target makes every function reachable, so the uncalled check
    /// is abandoned wholesale (`GameProc/ErbLoader.cs:667-676`).
    #[test]
    fn a_form_target_abandons_the_uncalled_check() {
        let root = vec![call(Some("A"), false), call(None, false)];
        let (_, a) = fixture();
        let mut config = EraConfig::default();
        config.function_not_called_warning = DisplayWarningFlag::Display;

        let analysis = analyze(&nodes(&root, &a), &config);
        assert!(analysis.dynamic_target);
        assert!(
            analysis.diagnostics.iter().all(|d| !d.3.contains("@C")),
            "{:?}",
            analysis.diagnostics
        );
    }

    /// `呼び出されなかった関数を無視する` (default YES): an unreached function is
    /// never analysed, so its own missing targets are never reported. Turning
    /// it off analyses them (`GameProc/ErbLoader.cs:704-706`).
    #[test]
    fn ignore_uncalled_function_gates_warnings_from_dead_code() {
        let root = vec![];
        let dead_call = vec![call(Some("MISSING"), false)];
        let mut nodes = nodes(&root, &[]);
        nodes[3].calls = &dead_call;

        let mut config = EraConfig::default();
        config.function_not_found_warning = DisplayWarningFlag::Display;
        assert!(config.ignore_uncalled_function, "default is YES");

        let analysis = analyze(&nodes, &config);
        assert!(
            analysis.diagnostics.is_empty(),
            "dead code is not analysed: {:?}",
            analysis.diagnostics
        );

        config.ignore_uncalled_function = false;
        let analysis = analyze(&nodes, &config);
        assert_eq!(
            analysis.diagnostics.iter().filter(|(l, ..)| *l == 2).count(),
            1,
            "{:?}",
            analysis.diagnostics
        );
    }

    /// The system-label set is a graph root even with nothing calling it
    /// (`GameData/IdentifierDictionary.cs:65-107`).
    #[test]
    fn system_labels_are_roots() {
        assert!(is_system_label_name("SYSTEM_TITLE"));
        assert!(is_system_label_name("SHOW_STATUS"));
        assert!(is_system_label_name("COM12"));
        assert!(is_system_label_name("COM_ABLE3"));
        assert!(is_system_label_name("ABLUP0"));
        assert!(!is_system_label_name("COM"));
        assert!(!is_system_label_name("COMBAT"));
        assert!(!is_system_label_name("ABLUPX"));
        assert!(!is_system_label_name("MY_FUNC"));
    }
}
