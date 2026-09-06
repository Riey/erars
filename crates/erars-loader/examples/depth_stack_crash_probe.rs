//! Empirically finds where deserializing a deeply-nested `Expr` chain
//! actually overflows the stack, on a thread sized exactly like
//! `erars-stdio`'s real runtime thread (`main.rs`'s
//! `.stack_size(8 * 1024 * 1024)`), so a `set_max_depth` cap can be chosen
//! with a measured, not assumed, safety margin below the real crash point.
//!
//! Deliberately a single-depth-per-process probe: a stack overflow is a
//! fatal signal, not a catchable `Result`, so each candidate depth is run
//! as its own process (driven by a bisecting shell loop) rather than
//! looping depths in one process, where a crash would take down every
//! later candidate along with it.
//!
//! `cargo run --release -p erars-loader --example depth_stack_crash_probe -- <depth>`
//! exits 0 and prints `SURVIVED depth=<n>` if deserialization completes;
//! a stack overflow kills the process with SIGSEGV/SIGABRT before it can
//! print or exit normally, which the driving shell observes directly.

use erars_ast::{BinaryOperator, Expr, StrKey, VariableInfo};
use erars_compiler::HeaderInfo;
use hashbrown::HashMap;

fn build_chain(depth: usize) -> Expr {
    let mut e = Expr::Int(1);
    for _ in 0..depth {
        e = Expr::BinopExpr(Box::new(e), BinaryOperator::Add, Box::new(Expr::Int(1)));
    }
    e
}

fn main() {
    let depth: usize = std::env::args()
        .nth(1)
        .expect("usage: depth_stack_crash_probe <depth>")
        .parse()
        .expect("depth must be a number");

    // Build and serialize on the main thread (whatever stack size the OS
    // gave the process); only deserialization runs on the 8 MiB thread
    // under test, matching erars-stdio's real runtime thread exactly.
    erars_ast::init_interner();
    let mut header = HeaderInfo::default();
    header.global_variables.insert(
        StrKey::new("PROBE"),
        VariableInfo {
            init: Some(vec![build_chain(depth)].into_boxed_slice()),
            ..Default::default()
        },
    );
    let local_infos: HashMap<StrKey, Vec<(StrKey, VariableInfo)>> = HashMap::new();
    let mut buf = Vec::new();
    rmp_serde::encode::write(&mut buf, &(&header, &local_infos)).expect("encode");

    let handle = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .name("erars-runtime".into())
        .spawn(move || {
            let mut slice: &[u8] = &buf;
            let mut de = rmp_serde::Deserializer::new(&mut slice);
            de.set_max_depth(usize::MAX);
            let _result: (HeaderInfo, HashMap<StrKey, Vec<(StrKey, VariableInfo)>>) =
                serde::Deserialize::deserialize(&mut de).expect("decode");
        })
        .unwrap();
    handle.join().unwrap();
    println!("SURVIVED depth={depth}");
}
