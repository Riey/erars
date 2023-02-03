use std::sync::Arc;

use criterion::*;
use erars_ast::StrKey;
use erars_compiler::{compile, HeaderInfo, ParserContext};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn gen_bench(count: usize) -> String {
    let mut ret = String::from("@FUNC\n");

    for _ in 0..count {
        ret.push_str("PRINTL Hello, world!\n");
    }

    ret
}

fn make_ctx() -> ParserContext<'static> {
    erars_ast::init_interner();

    ParserContext::new(
        Arc::new(HeaderInfo {
            global_variables: serde_yaml::from_str(include_str!(
                "../crates/erars-loader/src/variable.yaml"
            ))
            .unwrap(),
            ..Default::default()
        }),
        StrKey::new("bench.ERB"),
    )
}

fn parse_small(c: &mut Criterion) {
    let ctx = make_ctx();
    c.bench_function("small 5000", |b| {
        let code = gen_bench(5000);
        b.iter(|| ctx.parse_program_str(&code).unwrap());
    });
    c.bench_function("small 50000", |b| {
        let code = gen_bench(50000);
        b.iter(|| ctx.parse_program_str(&code).unwrap());
    });
}

fn parse_real(c: &mut Criterion) {
    let ctx = make_ctx();

    c.bench_function("parse title", |b| {
        b.iter(|| ctx.parse_program_str(include_str!("../ERB/TITLE.ERB")).unwrap());
    });

    c.bench_function("parse system", |b| {
        b.iter(|| ctx.parse_program_str(include_str!("../ERB/SYSTEM.ERB")).unwrap());
    });
}

fn compile_real(c: &mut Criterion) {
    let ctx = make_ctx();

    c.bench_function("compile title", |b| {
        b.iter(|| {
            ctx.parse_program_str(include_str!("../ERB/TITLE.ERB"))
                .unwrap()
                .into_iter()
                .map(|f| compile(f))
                .collect::<Result<Vec<_>, _>>()
        });
    });

    c.bench_function("compile system", |b| {
        b.iter(|| {
            ctx.parse_program_str(include_str!("../ERB/SYSTEM.ERB"))
                .unwrap()
                .into_iter()
                .map(|f| compile(f))
                .collect::<Result<Vec<_>, _>>()
        });
    });
}

criterion_group!(parse_benches, parse_small, parse_real);
criterion_group!(compile_benches, compile_real);

criterion_main!(parse_benches, compile_benches);
