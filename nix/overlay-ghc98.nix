{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken overrideCabal;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak(prev.callHackage "brick" "0.70.1" { });

  # brick 0.70.1 requires vty < 6.0.
  vty = doJailbreak (prev.callHackage "vty" "5.39" { });

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  # singletons-base/th 3.3 is the last version working on GHC 9.8
  singletons-th = prev.callHackage "singletons-th" "3.3" { };
  singletons-base = prev.callHackage "singletons-base" "3.3" { };

  # singletons-th 3.3 requires th-desugar 1.16
  th-desugar = prev.callHackage "th-desugar" "1.16" { };

  # This version of tasty isn't available in the nix ghc98 package set
  tasty = prev.callHackageDirect {
    pkg = "tasty";
    ver = "1.5.4";
    sha256 = "sha256-C6VyZuM+rcqllVlhk52snAKpw3sqrrzncz8Da1yE03Q=";
  } {};

  # Criterion test fails with tasty 1.5.4
  criterion = dontCheck prev.criterion;

  # Broken on GHC 9.8.4 see clash-ffi cabal file for details
  clash-ffi = overrideCabal prev.clash-ffi (drv: {
    broken = true;
  });

  # Randomly GHC panics with heap overflows during testing
  row-types = dontCheck prev.row-types;
}
