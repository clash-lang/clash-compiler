{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak markUnbroken dontCheck;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak prev.brick_0_70_1;

  # brick 0.70.1 requires vty < 6.0.
  vty = doJailbreak (prev.callHackage "vty" "5.39" { });

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  # Requires some old versions of libraries, but still works.
  derive-storable-plugin = doJailbreak prev.derive-storable-plugin;

  # Marken as broken, but compiles anyway.
  hedgehog-fakedata = doJailbreak (markUnbroken prev.hedgehog-fakedata);

  # We need a new tasty-flaky. The one from Hackage doesn't build for some weird
  # reason..
  tasty-flaky = prev.callCabal2nix "tasty-flaky" (pkgs.fetchFromGitHub {
    owner = "LaurentRDC";
    repo  = "tasty-flaky";
    rev = "fc31a9d622c1eb60030a50152258a9bef785e365";
    sha256 = "sha256-irLM3aVMxpBgsM72ArulMXcoLY2glalVkG//Lrj2JBI=";
  }) {};

  # This version of tasty isn't available in the nix ghc98 package set
  tasty = prev.callHackageDirect {
    pkg = "tasty";
    ver = "1.5.3";
    sha256 = "sha256-Ogd8J4aHNeL+xmcRWuJeGBNaePyLs5yo1IoMzvWrVPY=";
  } {};

  # The tests (not the package itself!) require a tasty <1.5, which won't work as we pull in
  # tasty 1.5.3. Solution: don't test!
  time-compat = dontCheck prev.time-compat;
}
