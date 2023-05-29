{ pkgs
, ghc-tcplugins-extra
, ghc-typelits-extra
, ghc-typelits-knownnat
, ghc-typelits-natnormalise
}:
next: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken;
in {
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak prev.brick_0_70_1;

  # Marked as broken in nixpkgs, since it needs on a newer hashable than the
  # .cabal file currently uploaded to hackage.
  concurrent-supply = doJailbreak (markUnbroken prev.concurrent-supply);

  # Use a newer version than the default in nixpkgs.
  doctest-parallel = next.doctest-parallel_0_3_0_1;

  # Use a special version we fetch from flake inputs.
  ghc-typelits-extra =
    prev.callCabal2nix "ghc-typelits-extra" "${ghc-typelits-extra}" {};

  # Use a special version we fetch from flake inputs.
  ghc-tcplugins-extra =
    prev.callCabal2nix
      "ghc-tcplugins-extra"
      "${ghc-tcplugins-extra}"
      {};

  # Use a special version we fetch from flake inputs.
  ghc-typelits-knownnat =
    prev.callCabal2nix
      "ghc-typelits-knownnat"
      "${ghc-typelits-knownnat}"
      {};

  # Use a special version we fetch from flake inputs.
  ghc-typelits-natnormalise =
    prev.callCabal2nix
      "ghc-typelits-natnormalise"
      "${ghc-typelits-natnormalise}"
      {};

  # Use a branch with changes to support GHC 9.6.1.
  hint =
    prev.hint.overrideAttrs(_: {
      src =
        pkgs.fetchFromGitHub {
          owner = "haskell-hint";
          repo = "hint";
          rev = "7803c34c8ae1d83c0f7c13fe6b30fcb3abd0ac51";
          hash = "sha256-ZFusrioxjDmWnDktD1evu7EjPG6brYpmmcaE2NWQKGA=";
        };
    });

  # Latest hackage revision is not yet in nixpkgs.
  prettyprinter-interp = doJailbreak prev.prettyprinter-interp;

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  # Use a newer version than the default in nixpkgs.
  singletons = next.singletons_3_0_2;

  # We want a version that matches with singletons-th, but the tests in here
  # are also a bit flaky since GHC 9.6 isn't officially supported.
  singletons-base = dontCheck prev.singletons-base_3_2;

  # Use a newer version than the default in nixpkgs.
  singletons-th = next.singletons-th_3_2;

  # Needs a newer text than the .cabal file currently uploaded to hackage.
  string-qq = doJailbreak prev.string-qq;

  # Needs a newer version than the default in nixpkgs.
  th-desugar = next.th-desugar_1_15;

  # Needs a newer base than the .cabal file currently uploaded to hackage.
  vector-binary-instances = doJailbreak prev.vector-binary-instances;

  # Use a newer version than the default in nixpkgs.
  vty = next.vty_5_38;
}
