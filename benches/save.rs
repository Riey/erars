use std::{sync::Arc, time::Instant};

use criterion::*;
use erars_compiler::HeaderInfo;
use erars_vm::{NullSystemFunctions, VmContext};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn save(c: &mut Criterion) {
    erars_ast::init_interner();
    let mut ctx = VmContext::new(
        Arc::new(HeaderInfo {
            global_variables: serde_yaml::from_str(include_str!(
                "../crates/erars-loader/src/variable.yaml"
            ))
            .unwrap(),
            ..Default::default()
        }),
        Default::default(),
        Box::new(NullSystemFunctions),
    );
    ctx.var.add_chara();
    c.bench_function("save_local", |b| {
        b.iter(|| ctx.var.get_serializable(&ctx.header_info, String::new()));
    });
    c.bench_function("save_global", |b| {
        b.iter(|| ctx.var.get_global_serializable(&ctx.header_info));
    });
    c.bench_function("load_local", |b| {
        b.iter_custom(|iters| {
            let mut var = ctx.var.clone();
            let ser = var.get_serializable(&ctx.header_info, String::new());
            let start = Instant::now();
            for _ in 0..iters {
                black_box(var.load_serializable(ser.clone(), &ctx.header_info).unwrap());
            }
            start.elapsed()
        });
    });
}

criterion_group!(save_benches, save);

criterion_main!(save_benches);
