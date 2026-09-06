//! Measures the actual `Expr` nesting depth of every `VariableInfo::init`
//! initialiser in a compiled corpus — both `HeaderInfo::global_variables`
//! and every function's local variable table (`ctx.var.local_infos()`),
//! exactly the two pieces `save_script`/`load_script` serialize together
//! through `rmp_serde`. Read-only: uses the real `run_script` compile path
//! (never `--save`), so it never writes anything into the corpus directory.
//!
//! `cargo run --release -p erars-loader --features multithread --example expr_depth_probe -- <game dir>`

use erars_ast::{Expr, VariableInfo};
use erars_loader::{load_config, run_script};
use erars_vm::NullSystemFunctions;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// One level per `Expr` node that itself contains another `Expr`. Matches
/// the AST's own recursive shape (`erars-ast/src/ast.rs`'s `Expr` enum) —
/// this is an AST-level depth, a direct, honest proxy for the question
/// "how deeply nested is the worst initialiser", not a claim about exactly
/// how many `rmp_serde` container levels each `Expr` level costs (that
/// mapping depends on rmp_serde's own enum/tuple representation, measured
/// separately and empirically in the sibling `depth_cap_probe` tool).
fn expr_depth(e: &Expr) -> usize {
    match e {
        Expr::String(_) | Expr::Int(_) | Expr::FormText(_) | Expr::Var(_) | Expr::IncOpExpr { .. } => 1,
        Expr::BuiltinVar(_, args) => 1 + args.iter().map(expr_depth).max().unwrap_or(0),
        Expr::Method(_, args) | Expr::BuiltinMethod(_, args) => {
            1 + args
                .iter()
                .filter_map(|a| a.as_ref())
                .map(expr_depth)
                .max()
                .unwrap_or(0)
        }
        Expr::UnaryopExpr(inner, _) => 1 + expr_depth(inner),
        Expr::BinopExpr(a, _, b) => 1 + expr_depth(a).max(expr_depth(b)),
        Expr::CondExpr(a, b, c) => 1 + expr_depth(a).max(expr_depth(b)).max(expr_depth(c)),
    }
}

fn var_info_depth(name: &str, info: &VariableInfo) -> Option<(String, usize)> {
    let init = info.init.as_ref()?;
    let depth = init.iter().map(expr_depth).max()?;
    Some((name.to_string(), depth))
}

fn main() {
    let target = std::env::args().nth(1).expect("usage: expr_depth_probe <game dir>");
    let config = load_config(&target);
    let (_vm, ctx, _console) = run_script(
        &target,
        Box::new(NullSystemFunctions),
        config,
        false,
        false,
        false,
    )
    .expect("compile failed");

    let mut worst: Vec<(String, usize)> = Vec::new();

    for (key, info) in ctx.header_info.global_variables.iter() {
        if let Some(row) = var_info_depth(key.resolve(), info) {
            worst.push(row);
        }
    }

    for (func_name, vars) in ctx.var.local_infos() {
        for (var_name, info) in vars {
            if let Some((_, depth)) = var_info_depth(var_name.resolve(), info) {
                worst.push((format!("{}::{}", func_name.resolve(), var_name.resolve()), depth));
            }
        }
    }

    worst.sort_by(|a, b| b.1.cmp(&a.1));
    println!("Top 10 deepest VariableInfo::init Expr trees ({} total with an init):", worst.len());
    for (name, depth) in worst.iter().take(10) {
        println!("  {name}: depth {depth}");
    }
    if let Some((_, max_depth)) = worst.first() {
        println!("MAX_EXPR_DEPTH={max_depth}");
    } else {
        println!("MAX_EXPR_DEPTH=0");
    }
}
