use erars_ast::StrKey;
use erars_compiler::HeaderInfo;

#[test]
fn erh_tw() {
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    header.merge_header(include_str!("erh_tests/tw.erh")).unwrap();
    assert!(header.global_variables[&StrKey::new("LAST_SAVE")].is_global);
}
