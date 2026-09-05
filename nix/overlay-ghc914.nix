{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak (prev.callHackage "brick" "0.70.1" { });

  # Tooling stack still excludes containers 0.8 here.
  config-ini = doJailbreak prev.config-ini;

  # HLS/tooling stack still excludes base 4.22 here.
  dec = doJailbreak prev.dec;

  # HLS stack still excludes base 4.22 here.
  clay = doJailbreak prev.clay;

  # HLS stack still excludes template-haskell 2.24 here.
  constraints-extras = doJailbreak prev.constraints-extras;

  # HLS stack still excludes base 4.22 here.
  hie-compat = doJailbreak prev.hie-compat;

  # Tooling stack still excludes newer containers/time here.
  rebase = doJailbreak prev.rebase;

  # Terminal tooling stack still excludes containers 0.8 here.
  string-random = doJailbreak prev.string-random;

  # Benchmark/tooling stack still excludes newer containers/time here.
  turtle = doJailbreak prev.turtle;

  # HLS stack tests rely on hidden containers internals, but the library builds.
  enummapset = dontCheck prev.enummapset;

  # The default versions in nixpkgs predate GHC 9.14: singletons-base 3.4's
  # custom Setup does not compile against the Cabal library shipped with GHC
  # 9.14. These releases list GHC 9.14.1 as tested.
  singletons-th = prev.singletons-th_3_5_1;
  # Its test suite needs singletons-base-code-generator, which nixpkgs marks
  # broken.
  singletons-base = dontCheck prev.singletons-base_3_5_1;
  th-desugar = prev.th-desugar_1_19;

  # Tooling stack still excludes base 4.22 and newer tagged here.
  binary-instances = doJailbreak prev.binary-instances;

  # Newer Hackage release builds with newer containers.
  algebraic-graphs = prev.callHackageDirect {
    pkg = "algebraic-graphs";
    ver = "0.8";
    sha256 = "0qig4y9ki1qmvklkdmm07i6wjqqmh7b2fpy6xjsc0d5anm5n8icn";
  } { };

  # brick 0.70.1 requires vty < 6.0.
  vty = doJailbreak (prev.callHackage "vty" "5.39" { });

  # Marked as broken in nixpkgs, since it specifies much older dependencies
  # than the defaults in nixpkgs.
  rewrite-inspector = doJailbreak (markUnbroken prev.rewrite-inspector);

  # Marked broken and bounds exclude newer GHC. But seems to work fine.
  derive-storable-plugin = doJailbreak (markUnbroken prev.derive-storable-plugin);

  # HLS stack inspection tests fail on GHC 9.14, but the library builds.
  generic-lens = dontCheck prev.generic-lens;

  # Bounds exclude base 4.22, but it is needed through the HLS stack.
  ghc-trace-events = doJailbreak prev.ghc-trace-events;

  # Library builds, but unit tests regress on GHC 9.14.
  ghc-typelits-natnormalise = dontCheck prev.ghc-typelits-natnormalise;

  # clash-lib still reaches an older containers upper bound here.
  ordered-containers = doJailbreak prev.ordered-containers;

  # clash-lib test tooling still excludes newer template-haskell.
  string-interpolate = doJailbreak prev.string-interpolate;
}
