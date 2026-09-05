//! Empirically measures the minimum `rmp_serde::Deserializer::set_max_depth`
//! that a real `game.era`'s `(HeaderInfo, HashMap<StrKey, Vec<(StrKey,
//! VariableInfo)>>)` blob actually needs to deserialize successfully —
//! rather than assuming a multiplier from `Expr`'s AST shape onto
//! rmp_serde's own enum/tuple encoding, this tries real `set_max_depth`
//! values against the real file and reports the exact boundary via binary
//! search between a known-failing and a known-succeeding bound.
//!
//! `cargo run --release -p erars-loader --example depth_cap_probe -- <game.era path> [depth]`
//!
//! With a `depth` argument, tries exactly that depth once and reports
//! success/failure (used to bisect from a driving shell script). Without
//! it, runs the same fixed low/high bounds this tool started from.

use std::io::Read;

use erars_ast::{StrKey, VariableInfo};
use erars_compiler::HeaderInfo;
use hashbrown::HashMap;

fn try_depth(bytes: &[u8], depth: usize) -> bool {
    let mut slice = bytes;
    let mut de = rmp_serde::Deserializer::new(&mut slice);
    de.set_max_depth(depth);
    let result: Result<(HeaderInfo, HashMap<StrKey, Vec<(StrKey, VariableInfo)>>), _> =
        serde::Deserialize::deserialize(&mut de);
    result.is_ok()
}

fn main() {
    let mut args = std::env::args().skip(1);
    let path = args.next().expect("usage: depth_cap_probe <game.era path> [depth]");
    let mut file = std::fs::File::open(&path).expect("open game.era");
    let mut all_bytes = Vec::new();
    file.read_to_end(&mut all_bytes).expect("read game.era");

    // Skip the bytecode section the same way `load_script` does, so the
    // rmp_serde payload starts at the right offset.
    let mut slice: &[u8] = &all_bytes;
    let _dic = unsafe { erars_bytecode::read_from(&mut slice) }.expect("read bytecode section");
    let payload = slice.to_vec();

    if let Some(depth_arg) = args.next() {
        let depth: usize = depth_arg.parse().expect("depth must be a number");
        let ok = try_depth(&payload, depth);
        println!("depth={depth} ok={ok}");
        std::process::exit(if ok { 0 } else { 1 });
    }

    // No explicit depth: report both ends of the interesting range directly.
    for depth in [256usize, 512, 1024, 2048, 4096, 8192] {
        let ok = try_depth(&payload, depth);
        println!("depth={depth} ok={ok}");
    }
}
