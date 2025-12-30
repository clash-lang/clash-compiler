{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak markUnbroken;
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
}
