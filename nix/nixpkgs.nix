{ sources ? import ./sources.nix }:

let
  overlay = _: pkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore {};

    # Haskell overrides
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        # External overrides
        ghc-typelits-extra =
          self.callCabal2nix "ghc-typelits-extra" sources.ghc-typelits-extra {};

        # Internal overrides
        clash-lib = import ../clash-lib {};
        clash-ghc = import ../clash-ghc {};
        clash-prelude = import ../clash-prelude {};
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
