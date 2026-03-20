{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak markUnbroken dontCheck;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak(prev.callHackage "brick" "0.70.1" { });

<<<<<<< HEAD
  # brick 0.70.1 requires vty < 6.0.
  vty = prev.callHackage "vty" "5.39" { };

  # Marked as broken in nixpkgs, since it needs a newer hashable than the
  # .cabal file currently uploaded to hackage.
  concurrent-supply = doJailbreak (markUnbroken prev.concurrent-supply);

  # Use a branch with changes to support GHC 9.6.1.
  hint =
    prev.hint.overrideAttrs (_: {
      src =
        pkgs.fetchFromGitHub {
          owner = "haskell-hint";
          repo = "hint";
          rev = "7803c34c8ae1d83c0f7c13fe6b30fcb3abd0ac51";
          hash = "sha256-ZFusrioxjDmWnDktD1evu7EjPG6brYpmmcaE2NWQKGA=";
        };
    });
||||||| parent of 4e6fc5c5 (Update flake (#3112))
  vty = prev.callHackage "vty" "5.39" { };

  # Marked as broken in nixpkgs, since it needs on a newer hashable than the
  # .cabal file currently uploaded to hackage.
  concurrent-supply = doJailbreak (markUnbroken prev.concurrent-supply);

  # Use a branch with changes to support GHC 9.6.1.
  hint =
    prev.hint.overrideAttrs (_: {
      src =
        pkgs.fetchFromGitHub {
          owner = "haskell-hint";
          repo = "hint";
          rev = "7803c34c8ae1d83c0f7c13fe6b30fcb3abd0ac51";
          hash = "sha256-ZFusrioxjDmWnDktD1evu7EjPG6brYpmmcaE2NWQKGA=";
        };
    });
=======
  # brick 0.70.1 requires vty < 6.0.
  vty = doJailbreak (prev.callHackage "vty" "5.39" { });
>>>>>>> 4e6fc5c5 (Update flake (#3112))

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  derive-storable-plugin = markUnbroken prev.derive-storable-plugin;

  # singletons-base/th 3.2 is the last version working on GHC 9.6
  singletons-th = prev.callHackage "singletons-th" "3.2" { };
  # Some tests fail, seems to be golden tests which might fail when output
  # changes?
  singletons-base = dontCheck (prev.callHackage "singletons-base" "3.2" { });

  # singletons-th 3.2 requires th-desugar 1.15
  th-desugar = prev.callHackage "th-desugar" "1.15" { };

  # th-desugar 1.15 requires th-abstraction 0.6
  th-abstraction = prev.callHackage "th-abstraction" "0.6.0.0" { };
}
