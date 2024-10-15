{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak markUnbroken;
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
}
