{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak markUnbroken dontCheck;
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
