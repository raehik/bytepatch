{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs:
  inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, config, ... }: {
        packages.default = self'.packages.ghc96-bytepatch;
        devShells.default = self'.devShells.ghc96;
        haskellProjects.ghc96-base = import ./haskell-flake-ghc96.nix pkgs;
        haskellProjects.ghc96 = {
          # 2023-11-14: GHC 9.6 base package set is borked
          # PR: https://github.com/NixOS/nixpkgs/pull/267477
          basePackages = config.haskellProjects.ghc96-base.outputs.finalPackages;
          devShell.mkShellArgs.name = "ghc96-bytepatch";
        };
      };
    };
}
