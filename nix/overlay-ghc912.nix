{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) appendPatches doJailbreak dontCheck markUnbroken;
  disableLibProfiling = drv: pkgs.haskell.lib.overrideCabal drv (_: {
    enableLibraryProfiling = false;
  });
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak (prev.callHackage "brick" "0.70.1" { });

  # brick 0.70.1 requires vty < 6.0.
  vty = doJailbreak (prev.callHackage "vty" "5.39" { });

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  # An ipv6 test fails on CI. Most likely due to missing kernel support?
  network = dontCheck prev.network;

  # Marked broken and bounds exclude newer GHC. But seems to work fine.
  derive-storable-plugin = doJailbreak (markUnbroken prev.derive-storable-plugin);

  # The nixpkgs singletons stack for this package set still pulls a th-desugar
  # release with pre-9.12 template-haskell bounds.
  singletons-base = dontCheck (prev.callHackage "singletons-base" "3.5" { });
  singletons-base-code-generator = markUnbroken prev.singletons-base-code-generator;
  singletons-th = prev.callHackage "singletons-th" "3.5" { };
  th-desugar = prev.callHackage "th-desugar" "1.18" { };

  # Keep tasty aligned with the other supported package sets.
  tasty = prev.callHackageDirect {
    pkg = "tasty";
    ver = "1.5.4";
    sha256 = "sha256-C6VyZuM+rcqllVlhk52snAKpw3sqrrzncz8Da1yE03Q=";
  } { };

  # Criterion test fails with tasty 1.5.4
  criterion = dontCheck prev.criterion;

  # GHC 9.12.4 currently hits a compiler panic in the HLS stack when nixpkgs
  # asks for profiling artifacts.
  ghcide = disableLibProfiling prev.ghcide;
  hiedb = disableLibProfiling prev.hiedb;
  hls-graph = disableLibProfiling prev.hls-graph;
  hls-plugin-api = disableLibProfiling prev.hls-plugin-api;
  hls-test-utils = disableLibProfiling prev.hls-test-utils;
  haskell-language-server = disableLibProfiling prev.haskell-language-server;

  # Upstream nixpkgs has this fix but they have not landed in a release yet
  fourmolu = appendPatches prev.fourmolu [
    (pkgs.fetchpatch {
      name = "fourmolu-absolute-build-tool-paths.patch";
      url = "https://github.com/fourmolu/fourmolu/commit/9217bc926ab80d20b815f0486be2184db07df4fc.patch";
      hash = "sha256-ANzuKy5WfWCGZ7HFVBpTtuyUqzFfef/xR/v1KiyJEX4=";
    })
  ];

  # Randomly GHC panics with heap overflows during testing
  row-types = dontCheck prev.row-types;

  # Randomly fails a single test -- cannot reproduce locally
  attoparsec = dontCheck prev.attoparsec;
}
