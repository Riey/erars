//! `times_not_rigorous_calculation` (emuera.config `TIMESの計算をeramakerにあわせる`,
//! Emuera `Config.TimesNotRigorousCalculation`, `ConfigData.cs:111`):
//!
//! - `false` (the default): `TIMES`'s integer operand is multiplied through
//!   a 96-bit-integer `decimal`, which holds any `i64` exactly, so only the
//!   already-`f32`-rounded factor contributes rounding
//!   (`GameProc/Function/Instraction.Child.cs:906-917`).
//! - `true`: both operands are cast to `double` before multiplying
//!   (`:901-904`), so an operand above 2^53 silently loses its low bits
//!   before the factor is ever applied - the same class of defect
//!   `docs/research/2026-09-06-language-feature-work.md` §5.1 fixed for
//!   erars's *unconditional* pre-existing implementation (which cast to
//!   `f32`, losing bits above 2^24). That fix left erars's `TIMES` matching
//!   Emuera's non-default `true` behaviour unconditionally; this test
//!   covers wiring the config key so `false` (the default) is reachable.
//!
//! `LOCAL = 2^53 + 1` (9007199254740993) is the smallest integer an `f64`
//! cannot represent exactly - `(LOCAL as f64) as i64` alone already rounds
//! it down to 9007199254740992, before any multiply. `TIMES LOCAL, 1.0`
//! must therefore return `LOCAL` unchanged when the config is rigorous, and
//! the rounded-down neighbour when it is not.

use std::sync::Arc;

use erars_ast::StrKey;
use erars_compiler::{compile, EraConfig, HeaderInfo, ParserContext};
use erars_ui::VirtualConsole;
use erars_vm::{console_config, FunctionDic, NullSystemFunctions, TerminalVm, VmContext};

const SCRIPT: &str = concat!(
    "@SYSTEM_TITLE\n",
    "LOCAL = 9007199254740993\n",
    "TIMES LOCAL, 1.0\n",
    "PRINTFORML {LOCAL}\n",
);

/// Runs `SCRIPT` with the given `times_not_rigorous_calculation` setting and
/// returns the printed post-`TIMES` value of `LOCAL`.
fn run_and_get_local(times_not_rigorous_calculation: bool) -> i64 {
    erars_ast::init_interner();

    let header = Arc::new(HeaderInfo {
        global_variables: serde_yaml::from_str(include_str!(
            "../../erars-loader/src/variable.yaml"
        ))
        .unwrap(),
        ..Default::default()
    });

    let config = EraConfig {
        times_not_rigorous_calculation,
        ..Default::default()
    };
    let mut tx = VirtualConsole::new(&console_config(&config));
    let mut ctx = VmContext::new(
        header.clone(),
        Arc::new(config),
        Box::new(NullSystemFunctions),
        "sav".into(),
        "resources".into(),
    );
    let parser = ParserContext::new(header.clone(), StrKey::new("TIMES_PRECISION.ERB"));
    let mut dic = FunctionDic::new();
    for func in parser.parse_program_str(SCRIPT).unwrap() {
        dic.insert_compiled_func(
            &mut ctx.var,
            &ctx.header_info.default_local_size,
            compile(func).unwrap(),
        );
    }

    let vm = TerminalVm::new(dic, header);
    let ok = vm.start(&mut tx, &mut ctx);
    let lines: Vec<String> = tx.lines.iter().map(ToString::to_string).collect();
    assert!(ok, "VM error:\n{}", lines.join("\n"));
    lines[0].trim().parse().unwrap()
}

/// `erars_ast::init_interner()` is process-global, so both cases must run on
/// the same test thread.
#[test]
fn times_not_rigorous_calculation_controls_integer_precision_past_2_53() {
    const LOCAL: i64 = (1i64 << 53) + 1; // 9007199254740993

    assert_eq!(
        run_and_get_local(false),
        LOCAL,
        "the default (rigorous) path must preserve the integer operand exactly through TIMES"
    );
    assert_eq!(
        run_and_get_local(true),
        LOCAL - 1,
        "the non-rigorous (double) path must lose the low bit, matching real Emuera's own \
         TimesNotRigorousCalculation:true behaviour"
    );
}
