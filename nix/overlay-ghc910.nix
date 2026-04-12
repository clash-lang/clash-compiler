{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken;
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

  # An ipv6 test fails on CI. Most likely due to missing kernel support?
  network  = dontCheck prev.network;

  # Marked broken and bounds exclude newer GHC. But seems to work fine.
  derive-storable-plugin = doJailbreak (markUnbroken prev.derive-storable-plugin);

  # This version of tasty isn't available in the nix ghc910 package set
  tasty = prev.callHackageDirect {
    pkg = "tasty";
    ver = "1.5.4";
    sha256 = "sha256-C6VyZuM+rcqllVlhk52snAKpw3sqrrzncz8Da1yE03Q=";
  } {};

  # Criterion test fails with tasty 1.5.4
  criterion = dontCheck prev.criterion;

  # Randomly GHC panics with heap overflows during testing
  row-types = dontCheck prev.row-types;
}
