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

fn parse_long(c: &mut Criterion) {
    let mut code = String::with_capacity(100_000 * 30);
    code.push_str("@FUNC\n");
    for _ in 0..33333 {
        code.push_str("IF A:1");
        code.push_str("A:1 = A:2");
        code.push_str("ENDIF");
    }
    c.bench_function("long 100000", |b| {
        b.iter(|| compile(&code, &mut FunctionDic::new()).unwrap());
    });
}

criterion_group!(parse_benches, parse_small, parse_long);
criterion_main!(parse_benches);
