default: (build-release)

build:
    cargo build --all

build-release:
    cargo build --release --all

test:
    cargo test --all

test-update:
    K9_UPDATE_SNAPSHOTS=1 cargo test --all

run-ym-http:
    cargo run --release --bin erars-http -- --port=8000 ../eraTHYMKR

run-ym:
    cargo run --release --bin erars-stdio -- ../eraTHYMKR

run-ym-log:
    cargo run --release --bin erars-stdio -- --log-level=trace ../eraTHYMKR

run-ym-toriko:
    cargo run --release --bin erars-stdio -- --use-input=toriko.ron ../eraTHYMKR

gen-test name:
    echo "@SYSTEM_TITLE" > tests/run_tests/basic/{{name}}.erb
    echo "[]" > tests/run_tests/basic/{{name}}.ron
