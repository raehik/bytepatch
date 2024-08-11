{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    streampatch.url   = "github:raehik/streampatch";
    streampatch.flake = false;
  };
  outputs = inputs:
  inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = inputs.nixpkgs.lib.systems.flakeExposed;
    imports = [ inputs.haskell-flake.flakeModule ];
    perSystem = { self', pkgs, config, ... }: {
      packages.default = self'.packages.ghc96-bytepatch;
      devShells.default = self'.devShells.ghc96;
      haskellProjects.ghc96 = {
        basePackages = pkgs.haskell.packages.ghc96;
        packages.streampatch.source = inputs.streampatch;
        devShell.mkShellArgs.name = "ghc96-bytepatch";
        settings = {
          # unbreak pending https://github.com/NixOS/nixpkgs/pull/333364
          bytezap.broken = false;
        };
      };
    };
  };
}
