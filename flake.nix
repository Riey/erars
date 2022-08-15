{
    description = "erars";

    inputs = {
        nixpkgs.url = github:NixOS/nixpkgs;
        flake-utils.url = github:numtide/flake-utils;
    };

    outputs = { self, nixpkgs, flake-utils }:
        flake-utils.lib.eachDefaultSystem
            (system:
                let
                    pkgs = nixpkgs.legacyPackages.${system};
                    deps = with pkgs; [
                    ];
                in
                {
                    devShell = pkgs.mkShell {
                        name = "erars-shell";
                        buildInputs = deps;
                        nativeBuildInputs = with pkgs; [
                            pkg-config
                            rustfmt
                            rustc
                            cargo
                        ];
                        RUST_BACKTRACE=1;
                        RUST_TEST_THREADS=1;
                    };
                }
            );
}
