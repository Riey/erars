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

# Headless GPU alignment tests (no display server needed)
test-align:
    cargo test -p erars-renderer headless -- --nocapture

# Render a game's first screen to a PPM headlessly (no display), e.g. over SSH.
# Usage: just headless-shot /path/to/game /tmp/out.ppm
headless-shot game="." out="/tmp/erars-shot.ppm":
    cargo run -p erars-renderer -- --quite --headless-shot {{out}} {{game}}

gen-test name:
    echo "@SYSTEM_TITLE" > tests/run_tests/basic/{{name}}.erb
    touch tests/run_tests/basic/{{name}}.out

gen-specific-test on name:
    echo "@SYSTEM_TITLE" > tests/run_tests/{{on}}/{{name}}.erb
    touch tests/run_tests/{{on}}/{{name}}.out
