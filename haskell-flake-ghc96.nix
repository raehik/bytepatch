pkgs:

{
  # disable local project options (always do this for package sets)
  defaults.packages = {};
  devShell.enable = false;
  autoWire = [];

  # 2023-11-14: GHC 9.6 base package set is borked
  # PR: https://github.com/NixOS/nixpkgs/pull/267477
  basePackages = pkgs.haskell.packages.ghc96.override {
    overrides = self: super: {
      fgl = self.fgl_5_8_2_0;
      th-desugar = self.th-desugar_1_16;
    };
  };

  packages = {
    # singletons-{th,base} only have 3.0 and 3.3 (GHC 9.8), need 3.2 for GHC 9.6
    singletons-th.source   = "3.2";
    singletons-base.source = "3.2";
    # but now we have to re-override th-desugar bceause singletons-x 3.2 uses
    # th-desugar 1.15...
    th-desugar.source = "1.15";
  };
}
