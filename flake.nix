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
                        libGL
                        xorg.libX11
                        xorg.libXcursor
                        xorg.libXrandr
                        xorg.libXxf86vm
                        xorg.libXi
                        xorg.libxcb
                        wayland
                        libxkbcommon
                        vulkan-loader
                        vulkan-validation-layers
                        freetype
                        fontconfig
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
                    };
                }
            );
}
