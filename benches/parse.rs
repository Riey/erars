use criterion::*;
use erars::erars_compiler::parse_program;
use erars_compiler::VariableInterner;

fn parse_small(c: &mut Criterion) {
    let var = VariableInterner::with_default_variables();
    c.bench_function("small 5", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(5);
        b.iter(|| parse_program(&code, &var).unwrap());
    });
    c.bench_function("small 500", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(500);
        b.iter(|| parse_program(&code, &var).unwrap());
    });
    c.bench_function("small 5000", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(5000);
        b.iter(|| parse_program(&code, &var).unwrap());
    });
    c.bench_function("small 50000", |b| {
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(50000);
        b.iter(|| parse_program(&code, &var).unwrap());
    });
}

criterion_group!(parse_benches, parse_small);
criterion_main!(parse_benches);
