use criterion::*;
use erars::compiler::compile;

fn parse_small(c: &mut Criterion) {
    c.bench_function("small 5", |b| {
        let code = "PRINTL Hello, world!\n".repeat(5);
        b.iter(|| compile(&code).unwrap());
    });
    c.bench_function("small 500", |b| {
        let code = "PRINTL Hello, world!\n".repeat(500);
        b.iter(|| compile(&code).unwrap());
    });
}

criterion_group!(parse_benches, parse_small);
criterion_main!(parse_benches);
