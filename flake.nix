{
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
        flake-utils.url = "github:numtide/flake-utils";
        rust-overlay = {
            url = "github:oxalica/rust-overlay";
            inputs.nixpkgs.follows = "nixpkgs";
            inputs.flake-utils.follows = "flake-utils";
        };
    };

    outputs = inputs@{ self, nixpkgs, flake-utils, rust-overlay, ... }: 
    flake-utils.lib.eachDefaultSystem (system:
    let
        pkgs = nixpkgs.legacyPackages.${system}.extend rust-overlay.overlays.default;
    in {
        devShells.default = pkgs.mkShell {
            packages = with pkgs; [ 
                gcc
                ghc
                go
                rust-bin.stable.latest.default
            ];
        };
    });
}
