default: (build-release)

build:
    cargo build

build-release:
    cargo build --release

test:
    cargo test

test-update:
    K9_UPDATE_SNAPSHOTS=1 cargo test

run-ym-http:
    cargo run --release -- --port=8000 ../eraTHYMKR

run-ym:
    cargo run --release -- ../eraTHYMKR

run-ym-log:
    cargo run --release -- --log-level=trace ../eraTHYMKR

run-ym-toriko:
    cargo run --release -- --use-input=toriko.ron ../eraTHYMKR

gen-test name:
    echo "@SYSTEM_TITLE" > tests/run_tests/basic/{{name}}.erb
    echo "[]" > tests/run_tests/basic/{{name}}.ron
