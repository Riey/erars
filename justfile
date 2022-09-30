push-program:
    nix build --json \
        | jq -r '.[].outputs | to_entries[].value' \
        | cachix push erars

push-win-program:
    nix build .#packages.x86_64-linux.erarsWin --json \
        | jq -r '.[].outputs | to_entries[].value' \
        | cachix push erars

build:
    cargo build

build-release:
    cargo build --release

test:
    cargo test

test-update:
    K9_UPDATE_SNAPSHOTS=1 cargo test

run-ym:
    cargo run --release -- --use-input=toriko.ron ../eraTHYMKR

run-ym-log:
    cargo run --release -- --use-input=toriko.ron --log-level=trace ../eraTHYMKR
