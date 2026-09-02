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

# Layout goldens + GPU pixel tests + the tui fixture game (no display server needed).
# ERARS_REQUIRE_GPU=1 turns "no adapter" skips into failures; K9_UPDATE_SNAPSHOTS=1 refreshes goldens.
test-align:
    cargo test -p erars-renderer --lib -- layout:: text:: headless:: draw:: raster:: --nocapture
    cargo test -p erars-renderer --test tui -- --nocapture

# Render a game's first screen to a PNG headlessly (no display), e.g. over SSH.
# Usage: just headless-shot /path/to/game /tmp/out.png
headless-shot game="." out="/tmp/erars-shot.png":
    cargo run -p erars-renderer -- --quite --headless-shot {{out}} {{game}}

gen-test name:
    echo "@SYSTEM_TITLE" > tests/run_tests/basic/{{name}}.erb
    touch tests/run_tests/basic/{{name}}.out

gen-specific-test on name:
    echo "@SYSTEM_TITLE" > tests/run_tests/{{on}}/{{name}}.erb
    touch tests/run_tests/{{on}}/{{name}}.out
