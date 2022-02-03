use criterion::*;
use erars::{compiler::compile, erars_compiler::parse_program, function::FunctionDic};

fn parse_new_small(c: &mut Criterion) {
    c.bench_function("new_small 5", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(5);
        b.iter(|| parse_program(&code));
    });
    c.bench_function("new_small 500", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(500);
        b.iter(|| parse_program(&code));
    });
}

fn parse_small(c: &mut Criterion) {
    c.bench_function("small 5", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(5);
        b.iter(|| compile(&code, &mut FunctionDic::new()).unwrap());
    });
    c.bench_function("small 500", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(500);
        b.iter(|| compile(&code, &mut FunctionDic::new()).unwrap());
    });
}

criterion_group!(parse_benches, parse_small, parse_new_small);
criterion_main!(parse_benches);
