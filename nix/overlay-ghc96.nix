{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak prev.brick_0_70_1;

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

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  # Needs a newer text than the .cabal file currently uploaded to hackage.
  string-qq = doJailbreak prev.string-qq;

  # Needs a newer base than the .cabal file currently uploaded to hackage.
  vector-binary-instances = doJailbreak prev.vector-binary-instances;

  # type-errors 0.2.0.2 is bounded on doctest >=0.16.0.1 && <0.22
  doctest = prev.callHackage "doctest" "0.21.1" { };

  # Marken as broken, but compiles anyway.
  hedgehog-fakedata = doJailbreak (markUnbroken prev.hedgehog-fakedata);
}
