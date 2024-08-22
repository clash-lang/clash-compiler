{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak prev.brick_0_70_1;

  # Marked as broken in nixpkgs for this version of GHC.
  docopt = markUnbroken prev.docopt;

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  singletons = prev.callHackage "singletons" "3.0" { };

  # The versions on nixpkgs are too new for GHC 9.0.2, which doesn't have
  # type level `Char` literals.
  singletons-th = prev.callHackage "singletons-th" "3.0" {
    inherit (final) th-desugar;
  };

  # We can't use newer than 1.12 here: we need singletons 3.x (due to the cabal
  # file of `clash-testsuite`) but the changed `DConP` constructor in 1.13
  # stops `singletons-th` from building.
  th-desugar = prev.callHackage "th-desugar" "1.12" { };
}
