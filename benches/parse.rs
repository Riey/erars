use criterion::*;
use erars::erars_compiler::{compile, parse_program};
use erars_compiler::VariableDic;

fn parse_small(c: &mut Criterion) {
    c.bench_function("small 5", |b| {
        let var = VariableDic::default();
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(5);
        b.iter(|| parse_program(&code, &var).unwrap());
    });
    c.bench_function("small 500", |b| {
        let var = VariableDic::default();
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(500);
        b.iter(|| parse_program(&code, &var).unwrap());
    });
    c.bench_function("small 5000", |b| {
        let var = VariableDic::default();
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(5000);
        b.iter(|| parse_program(&code, &var).unwrap());
    });
    c.bench_function("small 50000", |b| {
        let var = VariableDic::default();
        let code = "@FUNC\nPRINTL Hello, world!\n".repeat(50000);
        b.iter(|| parse_program(&code, &var).unwrap());
    });
}

fn parse_real(c: &mut Criterion) {
    c.bench_function("parse title", |b| {
        let var = VariableDic::default();
        b.iter(|| parse_program(include_str!("../ERB/TITLE.ERB"), &var));
    });

    c.bench_function("parse system", |b| {
        let var = VariableDic::default();
        b.iter(|| parse_program(include_str!("../ERB/SYSTEM.ERB"), &var));
    });
}

fn compile_real(c: &mut Criterion) {
    let mut var = VariableDic::default();

    c.bench_function("compile title", |b| {
        b.iter(|| {
            parse_program(include_str!("../ERB/TITLE.ERB"), &mut var)
                .unwrap()
                .into_iter()
                .map(|f| compile(f, &var))
                .collect::<Result<Vec<_>, _>>()
        });
    });

    c.bench_function("compile system", |b| {
        b.iter(|| {
            parse_program(include_str!("../ERB/SYSTEM.ERB"), &mut var)
                .unwrap()
                .into_iter()
                .map(|f| compile(f, &var))
                .collect::<Result<Vec<_>, _>>()
        });
    });
}

criterion_group!(parse_benches, parse_small, parse_real);
criterion_group!(compile_benches, compile_real);

criterion_main!(parse_benches, compile_benches);
