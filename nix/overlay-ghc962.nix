{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak prev.brick_0_70_1;


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

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  # We want a version that matches with singletons-th, but the tests in here
  # are also a bit flaky since GHC 9.6 isn't officially supported.
  singletons-base = dontCheck prev.singletons-base_3_2;

  # Use a newer version than the default in nixpkgs.
  singletons-th = prev.singletons-th_3_2;

  # Needs a newer text than the .cabal file currently uploaded to hackage.
  string-qq = doJailbreak prev.string-qq;

  # Needs a newer version than the default in nixpkgs.
  th-desugar = prev.th-desugar_1_15;

  # Needs a newer base than the .cabal file currently uploaded to hackage.
  vector-binary-instances = doJailbreak prev.vector-binary-instances;

  # Use an older version than the default in nixpkgs.
  th-abstraction = prev.th-abstraction_0_5_0_0;

  # type-errors 0.2.0.2 is bounded on doctest >=0.16.0.1 && <0.22
  doctest = prev.callHackage "doctest" "0.21.1" { };

  # We need a new tasty-flaky. The one from Hackage doesn't build for some weird
  # reason..
  tasty-flaky = prev.callCabal2nix "tasty-flaky" (pkgs.fetchFromGitHub {
    owner = "LaurentRDC";
    repo  = "tasty-flaky";
    rev = "fc31a9d622c1eb60030a50152258a9bef785e365";
    sha256 = "sha256-irLM3aVMxpBgsM72ArulMXcoLY2glalVkG//Lrj2JBI=";
  }) {};
}
