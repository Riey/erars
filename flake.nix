{
    description = "erars";

    inputs = {
        nixpkgs.url = github:NixOS/nixpkgs/ee3be6b246f4e8cad9f161b6c46eec4ddeaa6b43;
        flake-utils.url = github:numtide/flake-utils;
        filter.url = github:numtide/nix-filter;
    };

    outputs = { self, nixpkgs, flake-utils, filter }:
        flake-utils.lib.eachDefaultSystem
            (system:
                let
                    pkgs = import nixpkgs {
                        system = system;
                        overlays = [ filter.overlays.default ];
                    };
                    crossPkgs = pkgs.pkgsCross.mingwW64;

                    buildErars = (pkgs:
                        pkgs.rustPlatform.buildRustPackage rec {
                            pname = "erars";
                            version = "0.1.0";

                            src = pkgs.nix-filter {
                                root = ./.;
                                include = [
                                    ".cargo"
                                    "CSV"
                                    "ERB"
                                    "benches"
                                    "examples"
                                    "erars-ast"
                                    "erars-compiler"
                                    "erars-lexer"
                                    "src"
                                    "tests"
                                    "Cargo.toml"
                                    "Cargo.lock"
                                ];
                            };

                            cargoLock = {
                                lockFile = ./Cargo.lock;
                            };

                            meta = with pkgs.lib; {
                                description = "ERA emulator in Rust";
                                homepage = "https://github.com/Riey/erars";
                                license = licenses.gpl3Plus;
                                platforms = platforms.all;
                                maintainers = with maintainers; [ riey ];
                            };
                        }
                    );
                in rec
                {
                    devShell = pkgs.mkShell rec {
                        name = "erars-shell";
                        nativeBuildInputs = with pkgs; [
                            pkg-config
                            cmake
                            rustfmt
                            rustc
                            cargo
                            just
                        ];
                        buildInputs = with pkgs; [
                            fontconfig
                            xorg.libX11
                            xorg.libXcursor
                            xorg.libXrandr
                            xorg.libXi
                        ];
                        LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}:/run/opengl-driver/lib";
                        RUST_BACKTRACE=1;
                    };

                    packages = flake-utils.lib.flattenTree {
                        erars = buildErars pkgs;
                        erarsWin = buildErars crossPkgs;
                    };

                    defaultPackage = packages.erars;
                }
            );
}
