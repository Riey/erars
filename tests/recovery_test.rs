//! Load-time error recovery.
//!
//! Emuera never throws a whole ERB away over one bad line: `ErbLoader` records
//! the message, replaces the line with an `InvalidLine` and carries on with the
//! next one (`GameProc/ErbLoader.cs:403-407`, `:423-427`,
//! `GameProc/LogicalLine.cs:74-85`). `tests/run_tests.rs` cannot see any of
//! this — it drives `parse_program_str`, which stops at the first error — so
//! these tests call `parse_and_compile` the way the loader does.

mod test_util;

use erars_ast::BuiltinCommand;
use erars_compiler::{Bump, CompiledErb, Instruction};

const PATH: &str = "tests/recovery_test.erb";

/// Compiles one ERB the way `erars-loader`'s `compile_one` does.
#[track_caller]
fn compile(src: &str) -> CompiledErb {
    erars_ast::init_interner();
    let ctx = test_util::get_ctx(PATH);
    ctx.parse_and_compile(&mut ctx.preprocessor(src), &mut Bump::new())
    .unwrap_or_else(|(err, span)| panic!("{PATH} should still load: {err} at {span:?}"))
}

/// The one kind of failure that poisons the whole file.
#[track_caller]
fn compile_fails(src: &str) -> String {
    erars_ast::init_interner();
    let ctx = test_util::get_ctx(PATH);
    match ctx.parse_and_compile(&mut ctx.preprocessor(src), &mut Bump::new()) {
        Ok(erb) => panic!(
            "expected the file to be rejected, got {} function(s)",
            erb.functions.len()
        ),
        Err((err, _)) => err,
    }
}

fn names(erb: &CompiledErb) -> Vec<String> {
    erb.functions
        .iter()
        .map(|f| f.header.name.resolve().to_string())
        .collect()
}

fn throws(body: &[Instruction]) -> usize {
    body.iter()
        .filter(|inst| inst.as_builtin_command() == Some(BuiltinCommand::Throw))
        .count()
}

#[test]
fn a_bad_line_keeps_the_rest_of_its_function() {
    let erb = compile(
        "\
@GOOD
LOCAL = 1
LOCAL = ((
LOCAL = 2
",
    );

    assert_eq!(names(&erb), ["GOOD"]);
    assert_eq!(erb.errors.len(), 1, "{:?}", erb.errors);
    // The unreadable line becomes the `THROW` that stands in for Emuera's
    // `InvalidLine`, and it is the only one: `LOCAL = 2` compiled normally.
    assert_eq!(throws(&erb.functions[0].body), 1);
    assert!(erb.warnings.is_empty(), "{:?}", erb.warnings);
}

#[test]
fn each_bad_line_is_reported_once() {
    let erb = compile(
        "\
@GOOD
LOCAL = ((
LOCAL = 1
LOCAL = ))
",
    );

    assert_eq!(names(&erb), ["GOOD"]);
    assert_eq!(erb.errors.len(), 2, "{:?}", erb.errors);
    assert_eq!(throws(&erb.functions[0].body), 2);
}

#[test]
fn a_bad_line_inside_a_block_drops_only_its_function() {
    // A block opener parses its own body, so by the time the inner line fails
    // the following lines are gone and there is no safe place to resume. Only
    // this function is skipped; the file's other functions still register.
    let erb = compile(
        "\
@FIRST
LOCAL = 1

@SECOND
IF 1
	LOCAL = ((
ENDIF

@THIRD
LOCAL = 3
",
    );

    assert_eq!(names(&erb), ["FIRST", "THIRD"]);
    assert_eq!(erb.errors.len(), 1, "{:?}", erb.errors);
}

#[test]
fn a_bad_declaration_drops_only_its_function() {
    // A `#DIM` erars cannot read means the function's local does not exist,
    // so there is nothing left to resume with. Emuera instead keeps the
    // function and turns every line that mentions the name into an
    // `InvalidLine`; either way the file's other functions survive and the
    // line is reported. See §5 of
    // `docs/research/2026-09-03-emuera-command-gap.md`.
    let erb = compile(
        "\
@FIRST
LOCAL = 1

@SECOND
#DIM ((
LOCAL = 2

@THIRD
LOCAL = 3
",
    );

    assert_eq!(names(&erb), ["FIRST", "THIRD"]);
    assert_eq!(erb.errors.len(), 1, "{:?}", erb.errors);
}

#[test]
fn continue_outside_a_loop_is_a_warning() {
    // `ErbLoader.cs:1041-1058` reports this through `ParserMediator.Warn`,
    // which sets `line.IsError` (`GameData/ParserMediator.cs:118-131`) but
    // never touches the `noError` flag the game-start refusal is keyed to
    // (`GameProc/Process.SystemProc.cs:173-186`). So the function still
    // registers and the game still starts.
    for src in [
        "@F\nCONTINUE\n",
        "@F\nBREAK\n",
        "@F\nIF 1\n\tCONTINUE\nENDIF\n",
    ] {
        let erb = compile(src);
        assert_eq!(names(&erb), ["F"], "{src:?}");
        assert!(erb.errors.is_empty(), "{src:?}: {:?}", erb.errors);
        assert_eq!(erb.warnings.len(), 1, "{src:?}: {:?}", erb.warnings);
    }
}

#[test]
fn a_warning_reports_the_line_it_is_on() {
    let src = "@F\nLOCAL = 1\nCONTINUE\n";
    let erb = compile(src);
    let (_, span, level) = &erb.warnings[0];
    assert_eq!(&src[span.clone()], "CONTINUE");
    // A line-compiler warning is Emuera's level 2 (`GameProc/ErbLoader.cs:1041-1058`),
    // so `表示する最低警告レベル:2` still shows it.
    assert_eq!(*level, 2);
}

#[test]
fn an_unreadable_label_rejects_the_file() {
    // `InvalidLabelLine` is the one failure that sets `noError` in Emuera
    // (`GameProc/ErbLoader.cs:366`): nothing after it can be attributed to a
    // function, so the file is not usable.
    let err = compile_fails(
        "\
@GOOD
LOCAL = 1

@((
LOCAL = 2
",
    );
    assert!(!err.is_empty());
}

#[test]
fn a_file_that_does_not_start_with_a_function_is_rejected() {
    let err = compile_fails("LOCAL = 1\n");
    assert_eq!(err, "First line should be function line");
}

#[test]
fn an_empty_file_compiles_to_nothing() {
    let erb = compile("");
    assert!(erb.functions.is_empty());
    assert!(erb.errors.is_empty());
    assert!(erb.warnings.is_empty());
}

#[test]
fn a_constant_argument_index_is_folded() {
    // An argument's index has to be a plain number by the time the VM sees it,
    // and the corpus writes it as an expression over `#DIM CONST` values.
    // Emuera `Restructure`s the term for the same reason, so erars folds it
    // while the header's constants are in scope.
    let erb = compile("@F, ARG:(1 + 1)\nLOCAL = 1\n");
    assert_eq!(names(&erb), ["F"]);
    assert!(erb.errors.is_empty(), "{:?}", erb.errors);
    let arg = &erb.functions[0].header.args[0].0;
    assert_eq!(arg.args, vec![erars_ast::Expr::Int(2)]);
}

#[test]
fn an_argument_index_that_is_not_constant_rejects_the_file() {
    // Nothing after an unreadable label can be attributed to a function, and
    // Emuera treats a `FunctionLabelLine` it cannot restructure the same way:
    // `InvalidLabelLine` sets `noError` (`GameProc/ErbLoader.cs:366`).
    let err = compile_fails("@F, ARG:LOCAL\nLOCAL = 1\n");
    assert!(!err.is_empty());
}
