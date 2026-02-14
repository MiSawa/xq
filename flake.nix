{
  description = "Pure rust implementation of jq";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    systems.url = "github:nix-systems/default";
    fenix = {
      url = "github:nix-community/fenix/monthly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [ inputs.devshell.flakeModule ];
      perSystem =
        {
          system,
          pkgs,
          ...
        }:
        let
          toolchain = pkgs.fenix.complete.toolchain;
          minimalToolchain = pkgs.fenix.complete.withComponents [
            "rustc"
            "cargo"
          ];
        in
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.fenix.overlays.default
            ];
          };
          packages.default =
            (pkgs.makeRustPlatform {
              cargo = minimalToolchain;
              rustc = minimalToolchain;
            }).buildRustPackage
              {
                pname = "xq";
                version = (builtins.fromTOML (builtins.readFile ./Cargo.toml)).package.version;
                src = ./.;
                cargoLock.lockFile = ./Cargo.lock;
              };
          devshells.default = {
            commands = [
              {
                name = "release";
                category = "development";
                help = "release to crates.io";
                command = ''nix shell 'nixpkgs#cargo-release' --command 'cargo' 'release' "$@"'';
              }
              {
                name = "generate-license";
                category = "development";
                help = "generate list of licenses using cargo-about";
                command = "nix shell 'nixpkgs#cargo-about' --command 'cargo' 'about' 'generate' 'about.hbs'";
              }
            ];
            packages = [
              pkgs.clang
              toolchain
            ];
          };
        };
    };
}
