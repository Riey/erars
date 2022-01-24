use criterion::*;
use erars::{compiler::compile, function::FunctionDic};

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

criterion_group!(parse_benches, parse_small);
criterion_main!(parse_benches);
