{
  inputs = {
    #nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    #flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          #haskellPackages = pkgs.haskell.packages.ghc925;
          packages = {
            bytepatch.root = ./.;
          };
          # buildTools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          overrides = self: super: {
            # 2023-01-11 raehik: nixpkgs binrep broken
            # pending PR: https://github.com/NixOS/nixpkgs/pull/210212
            binrep = pkgs.haskell.lib.overrideCabal super.binrep (oa: {
              version = "0.3.1";
              sha256 = "lliwSmiIpql9GhXRnnjP9YrWQweVESzomCzG/lT+TQU=";
              revision = "1";
              editedCabalFile = "17l5x2vpdwdp6x14n1wayh6751cpsxsywj205n94khnm1cgcfp1a";
              broken = false;
            });
          };
          # hlintCheck.enable = true;
          # hlsCheck.enable = true;
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.bytepatch;
      };
    };
}
