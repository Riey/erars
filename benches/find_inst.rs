use criterion::{criterion_group, criterion_main, Criterion};
use erars_lexer::{InstructionCode, IntoEnumIterator as _};
use rand::{distributions::Alphanumeric, Rng};
use regex_automata::dfa::{dense, regex, Automaton};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn find(c: &mut Criterion) {
    let rand_string: String = rand::thread_rng()
        .sample_iter(&Alphanumeric)
        .take(1000)
        .map(char::from)
        .collect();

    let normal_patterns = InstructionCode::iter().map(|c| format!("{c}")).collect::<Vec<_>>();
    let patterns = InstructionCode::iter()
        .map(|c| format!("^{c}(?-u:\\b)"))
        .collect::<Vec<_>>();
    let syntax = regex_automata::SyntaxConfig::new().case_insensitive(true).utf8(false);
    let re = regex::Builder::new().syntax(syntax).build_many(&patterns).unwrap();
    let dense_dfa = dense::Builder::new().syntax(syntax).build_many(&patterns).unwrap();
    let normal_dense_dfa = dense::Builder::new()
        .syntax(syntax)
        .build_many(&normal_patterns)
        .unwrap();
    let sparse_dfa = dense_dfa.to_sparse().unwrap();

    c.bench_function("find_inst with for loop", |b| {
        b.iter(|| {
            for inst in InstructionCode::iter() {
                let s = <&str>::from(inst);
                if rand_string[..s.len()].eq_ignore_ascii_case(s) {
                    return true;
                }
            }

            false
        });
    });

    c.bench_function("find_inst with regex-automata", |b| {
        b.iter(|| re.find_earliest(rand_string.as_bytes()));
    });

    c.bench_function("find_inst with dense dfa", |b| {
        b.iter(|| dense_dfa.find_earliest_fwd(rand_string.as_bytes()));
    });

    c.bench_function("find_inst with normal dense dfa", |b| {
        b.iter(|| normal_dense_dfa.find_earliest_fwd(rand_string.as_bytes()));
    });

    c.bench_function("find_inst with sparse dfa", |b| {
        b.iter(|| sparse_dfa.find_earliest_fwd(rand_string.as_bytes()));
    });
}

criterion_group!(find_inst_benches, find);

criterion_main!(find_inst_benches);
