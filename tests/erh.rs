//! Header (`.ERH`) and CSV loading.
//!
//! Emuera loads headers in two passes: pass 1 records every `#DEFINE` and
//! *queues* every `#DIM`, under the comment `#DIMは保留しておいて後でまとめて
//! やる` ("defer `#DIM` and do them all together later"), and pass 2 declares
//! the queue, retrying whatever is still waiting on a constant another queued
//! line has to declare (`GameProc/HeaderFileLoader.cs:37-77`, `:276-364`).

#[allow(dead_code)]
mod test_util;

use erars_ast::StrKey;
use erars_compiler::{HeaderInfo, PendingDim};

fn size_of(header: &HeaderInfo, name: &str) -> Vec<u32> {
    header.global_variables[&StrKey::new(name)].size.clone()
}

#[test]
fn erh_tw() {
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    header.merge_header(include_str!("erh_tests/tw.erh")).unwrap();
    assert!(header.global_variables[&StrKey::new("LAST_SAVE")].is_global);
}

#[test]
fn a_dim_size_may_name_a_constant_declared_further_down() {
    // The deferral is the whole point: `#DIM BAR, FOO` is queued, fails on the
    // first pass because `FOO` is not declared yet, and settles on the second
    // (`HeaderFileLoader.cs:341-348`).
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    header
        .merge_header("#DIM BAR, FOO\n#DIM CONST FOO = 10\n")
        .unwrap();
    assert_eq!(size_of(&header, "BAR"), [10]);
}

#[test]
fn a_dim_size_may_name_a_constant_from_another_file() {
    // Pass 1 runs over every header file before pass 2 declares anything, so
    // the direction the files sort in does not matter.
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    let mut pending = Vec::new();
    header
        .merge_header_defines(0, "#DIM EARLY, LATE_CONST\n", &mut pending)
        .unwrap();
    header
        .merge_header_defines(1, "#DIM CONST LATE_CONST = 7\n", &mut pending)
        .unwrap();
    assert!(header.resolve_pending_dims(pending).is_empty());
    assert_eq!(size_of(&header, "EARLY"), [7]);
}

#[test]
fn an_unresolvable_dim_size_is_reported_against_its_own_file() {
    // Retrying stops as soon as a whole pass settles nothing
    // (`HeaderFileLoader.cs:361-362`); what is left is diagnosed, not
    // panicked on, and each diagnostic keeps the file it came from.
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    let mut pending = Vec::new();
    header
        .merge_header_defines(0, "#DIM FINE, 4\n", &mut pending)
        .unwrap();
    header
        .merge_header_defines(3, "#DIM BROKEN, NEVER_DECLARED\n", &mut pending)
        .unwrap();

    let errors = header.resolve_pending_dims(pending);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(errors[0].0, 3);
    // The good line in the other file still declared.
    assert_eq!(size_of(&header, "FINE"), [4]);
}

#[test]
fn a_dim_size_may_be_a_varsize_call() {
    // `VariableSize.csv` is read before the headers, so `VARSIZE` is a
    // constant by the time a `#DIM` line asks for it. It resizes a built-in
    // global, which is why this starts from the engine's own variable table.
    erars_ast::init_interner();
    let mut header = test_util::header_with_globals();
    header.merge_variable_size_csv("ITEMNAME,500\n").unwrap();
    header.merge_header("#DIM ITEM_TAG, VARSIZE(\"ITEMNAME\")\n").unwrap();
    assert_eq!(size_of(&header, "ITEM_TAG"), [500]);
}

#[test]
fn a_pending_dim_carries_its_own_file_index() {
    // `PendingDim` is public because the loader queues across files itself;
    // its `file` field is what turns a pass-2 failure back into a diagnostic
    // against the right path.
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    let mut pending: Vec<PendingDim> = Vec::new();
    header
        .merge_header_defines(9, "#DIMS TEXT, 3\n", &mut pending)
        .unwrap();
    assert_eq!(pending.len(), 1);
    assert_eq!(pending[0].file, 9);
    assert!(pending[0].is_str);
}

#[test]
fn define_bodies_are_renamed_before_they_are_stored() {
    // Emuera reads ERH through `EramakerFile`, whose `ReadLine` applies
    // `_Rename.csv` before anything else sees the text
    // (`GameProc/HeaderFileLoader.cs:86`). The corpus depends on it: a macro
    // body is only a readable variable reference after the `[[…]]` splice.
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    // `_Rename.csv` is written value-first, key-second
    // (`GameData/ParserMediator.cs:72-73`).
    header.merge_rename_csv("3,REQ:킨키\n").unwrap();
    header
        .merge_header("#DEFINE FLAG_REQ 依頼フラグ:[[REQ:킨키]]:0\n")
        .unwrap();
    assert_eq!(header.macros["FLAG_REQ"], "依頼フラグ:3:0");
}

/// A parser over a header that declares one macro.
fn macro_ctx(define: &str) -> erars_compiler::ParserContext<'static> {
    erars_ast::init_interner();
    let mut header = test_util::header_with_globals();
    header.merge_name_csv("FLAG", "1,플래그\n").unwrap();
    header.merge_header(define).unwrap();
    erars_compiler::ParserContext::new(std::sync::Arc::new(header), StrKey::new("macro.erb"))
}

#[test]
fn a_macro_expands_where_a_variable_is_read() {
    // Emuera stores a `#DEFINE` body as text and splices it in when the
    // identifier is reached, so a macro may stand for a whole variable
    // reference, index and all.
    let ctx = macro_ctx("#DEFINE MY_FLAG FLAG:플래그\n");
    let body = ctx.parse_body_str("LOCAL = MY_FLAG\n").unwrap();
    let stmt = format!("{:?}", body[0].0);
    assert!(stmt.contains("FLAG"), "{stmt}");
    assert!(stmt.contains("Int(1)"), "{stmt}");
}

#[test]
fn a_macro_expands_on_an_assignment_target() {
    // The left hand side goes through the same `variable()` parser, which is
    // where the expansion happens, so a macro works as an assignment target
    // as well as in an expression.
    let ctx = macro_ctx("#DEFINE MY_FLAG FLAG:플래그\n");
    let body = ctx.parse_body_str("MY_FLAG = 7\n").unwrap();
    match &body[0].0 {
        erars_ast::Stmt::Assign(var, None, rhs) => {
            assert_eq!(var.var.resolve(), "FLAG");
            assert_eq!(var.args, vec![erars_ast::Expr::Int(1)]);
            assert_eq!(*rhs, erars_ast::Expr::Int(7));
        }
        other => panic!("expected an assignment, got {other:?}"),
    }
}

#[test]
fn a_gamebase_title_key_that_is_not_the_katakana_one_is_ignored() {
    // Emuera's GAMEBASE switch has no `default:` arm
    // (`GameData/GameBase.cs:114-173`), so an unknown key is dropped without a
    // diagnostic. eraMegaten's `Data/CSV/GameBase.csv:3` is the Korean
    // `타이틀`, which real Emuera therefore ignores, leaving `ScriptTitle`
    // empty — refusing the file would refuse a game its target engine loads.
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    header
        .merge_gamebase_csv("타이틀,ShinEraTenseiP\nウィンドウタイトル,ShinEraTenseiP 0.5.9\n")
        .unwrap();
    assert_eq!(header.gamebase.title, "");
    assert_eq!(header.gamebase.window_title, "ShinEraTenseiP 0.5.9");
}

#[test]
fn a_missing_window_title_falls_back_the_way_emuera_does() {
    // `GameData/GameBase.cs:184-190`: the bare `"Emuera"` for a titleless
    // game, otherwise the title plus `ScriptVersionText`, which prints the
    // version integer split at the thousands digit (`:31-44`).
    erars_ast::init_interner();

    let mut titled = HeaderInfo::default();
    titled
        .merge_gamebase_csv("タイトル,Test\nバージョン,1230\n")
        .unwrap();
    assert_eq!(titled.gamebase.window_title, "Test 1.23");

    let mut odd_version = HeaderInfo::default();
    odd_version
        .merge_gamebase_csv("タイトル,Test\nバージョン,1234\n")
        .unwrap();
    assert_eq!(odd_version.gamebase.window_title, "Test 1.234");

    let mut untitled = HeaderInfo::default();
    untitled.merge_gamebase_csv("作者,nobody\n").unwrap();
    assert_eq!(untitled.gamebase.window_title, "Emuera");
}

#[test]
fn chara_csv_keys_are_accepted_in_english_and_in_any_case() {
    // Emuera uppercases the key before dispatching (`ConstantData.cs:1518`)
    // and gives every fixed name an English alias beside the Japanese one
    // (`:1519-1607`); `番号`/`NO` is compared case-insensitively at
    // `:1408-1409`. eraMegaten's 2_323 chara files are written in English.
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    header.merge_name_csv("TALENT", "3,소질명\n").unwrap();
    header
        .merge_chara_csv(
            "no,4\n\
             Name,ナナ\n\
             callname,나나\n\
             NICKNAME,꼬마\n\
             mastername,주인님\n\
             BASE,0,120\n\
             abl,1,3\n\
             TALENT,소질명\n\
             exp,2,5\n\
             mark,1,2\n\
             relation,4,80\n\
             EQUIP,3\n\
             juel,1,900\n\
             CFLAG,2,11\n\
             CSTR,1,메모\n\
             このキーは存在しない,1\n",
        )
        .unwrap();

    let chara = &header.character_templates[&4];
    assert_eq!(chara.no, 4);
    assert_eq!(chara.name, "ナナ");
    assert_eq!(chara.call_name, "나나");
    assert_eq!(chara.nick_name, "꼬마");
    assert_eq!(chara.master_name, "주인님");
    assert_eq!(chara.base[&0], 120);
    assert_eq!(chara.abl[&1], 3);
    // A name from the matching name CSV resolves to its index, and `素質`
    // is a flag: the value is the stamp, not the column.
    assert_eq!(chara.talent[&3], 1);
    assert_eq!(chara.exp[&2], 5);
    assert_eq!(chara.mark[&1], 2);
    assert_eq!(chara.relation[&4], 80);
    // `装着物`/`EQUIP` names one index and no value, so it stamps 1.
    assert_eq!(chara.equip[&3], 1);
    assert_eq!(chara.juel[&1], 900);
    assert_eq!(chara.cflag[&2], 11);
    assert_eq!(chara.cstr[&1], "메모");
}

#[test]
fn a_nonzero_flag_zero_makes_a_chara_an_sp_template() {
    // `CharacterTemplate.SetSpFlag`: `フラグ,0` decides which table the
    // template lands in.
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    header.merge_chara_csv("NO,1\nCFLAG,0,1\n").unwrap();
    header.merge_chara_csv("NO,2\nCFLAG,0,0\n").unwrap();
    assert!(header.character_sp_templates.contains_key(&1));
    assert!(header.character_templates.contains_key(&2));
}
