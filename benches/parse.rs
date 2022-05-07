use criterion::*;
use erars::erars_compiler::{compile, parse_program};
use erars_compiler::VariableInterner;

fn gen_bench(count: usize) -> String {
    let mut ret = String::from("@FUNC\n");

    for _ in 0..count {
        ret.push_str("PRINTL Hello, world!\n");
    }

    ret
}

fn parse_small(c: &mut Criterion) {
    let mut var = VariableInterner::with_default_variables();
    c.bench_function("small 5000", |b| {
        let code = gen_bench(5000);
        b.iter(|| parse_program(&code, &mut var).unwrap());
    });
    c.bench_function("small 50000", |b| {
        let code = gen_bench(50000);
        b.iter(|| parse_program(&code, &mut var).unwrap());
    });
}

fn parse_real(c: &mut Criterion) {
    let mut var = VariableInterner::with_default_variables();

    c.bench_function("parse title", |b| {
        b.iter(|| parse_program(include_str!("../ERB/TITLE.ERB"), &mut var));
    });

    c.bench_function("parse system", |b| {
        b.iter(|| parse_program(include_str!("../ERB/SYSTEM.ERB"), &mut var));
    });
}

fn compile_real(c: &mut Criterion) {
    let mut var = VariableInterner::with_default_variables();

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
