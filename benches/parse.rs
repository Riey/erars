use criterion::*;
use erars::erars_compiler::parse_program;

fn parse_small(c: &mut Criterion) {
    c.bench_function("new_small 5", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(5);
        b.iter(|| parse_program(&code));
    });
    c.bench_function("new_small 500", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(500);
        b.iter(|| parse_program(&code));
    });
}

criterion_group!(parse_benches, parse_small);
criterion_main!(parse_benches);
