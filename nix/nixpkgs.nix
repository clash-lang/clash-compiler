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

        ghc-typelits-knownnat =
         self.callCabal2nix "ghc-typelits-knownnat" sources.ghc-typelits-knownnat {};

        ghc-typelits-extra =
         self.callCabal2nix "ghc-typelits-extra" sources.ghc-typelits-extra {};

        # Internal overrides
        clash-lib = import ../clash-lib { inherit nixpkgs; };
        clash-ghc = import ../clash-ghc { inherit nixpkgs; };
        clash-prelude = import ../clash-prelude { inherit nixpkgs; };
        clash-cores = import ../clash-cores { inherit nixpkgs; };
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
