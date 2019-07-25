{ sources ? import ./sources.nix }:

let
  overlay = _: nixpkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (nixpkgs) lib; };

    # Haskell overrides
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        # External overrides
        ghc-typelits-extra =
          self.callCabal2nix "ghc-typelits-extra" sources.ghc-typelits-extra {};

        type-errors =
          self.callCabal2nix "type-errors" sources.type-errors {};

        first-class-families =
          self.callCabal2nix "first-class-families" sources.first-class-families {};

        # Internal overrides
        clash-lib = import ../clash-lib { inherit nixpkgs; };
        clash-ghc = import ../clash-ghc { inherit nixpkgs; };
        clash-prelude = import ../clash-prelude { inherit nixpkgs; };
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
