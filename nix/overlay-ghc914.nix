{ pkgs }:
final: prev:
let
  inherit (pkgs.haskell.lib) doJailbreak dontCheck markUnbroken;
in
{
  # Use an older version than the default in nixpkgs. Since rewrite-inspector
  # is basically abandonware it catches fire with brick 1.0+.
  brick = doJailbreak (prev.callHackage "brick" "0.70.1" { });

  # clash-lib pulls trifecta, which still has an older blaze-markup bound.
  blaze-markup = doJailbreak prev.blaze-markup;

  # blaze-html also still excludes containers-0.8.
  blaze-html = doJailbreak prev.blaze-html;

  # Older package in the tree still excludes base 4.22.
  boring = doJailbreak prev.boring;

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

  # HLS stack still excludes newer base/template-haskell here.
  singletons-th = doJailbreak prev.singletons-th;

  # Benchmark/tooling stack still excludes newer base/template-haskell here.
  singletons-base = doJailbreak prev.singletons-base;

  # Benchmark stack still excludes base 4.22 here.
  binary-orphans = doJailbreak prev.binary-orphans;

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

  # The local package test suites pull in older test tooling that is not yet
  # ported to GHC 9.14, but the libraries themselves build.
  clash-prelude = dontCheck prev.clash-prelude;
  clash-lib = dontCheck prev.clash-lib;

  # Newer Hackage release adds support for newer GHC API versions.
  ghc-exactprint = prev.callHackageDirect {
    pkg = "ghc-exactprint";
    ver = "1.14.0.0";
    sha256 = "08aa89b9rxxbpi6pbcg95f26gvlvlil17lij0jml78nwjax1sca1";
  } { };

  # Newer Hackage release supports GHC 9.14.
  doctest-parallel = prev.callHackageDirect {
    pkg = "doctest-parallel";
    ver = "0.4.1";
    sha256 = "0qm496lsrhs71w1pqc1llf9qs0ip2a4vg64nrscqgrpw55kdh800";
  } { };

  # clash-prelude test dependencies still exclude newer template-haskell and time.
  hedgehog = doJailbreak prev.hedgehog;

  # nixpkgs still has hint-0.9.0.8, but Hackage has a newer GHC 9.14-capable release.
  hint = dontCheck (prev.callHackageDirect {
    pkg = "hint";
    ver = "0.9.0.9";
    sha256 = "sha256-9OubbfO88yD8OIaDdQmzDWG2eGksgb71UI61ja+nx94=";
  } { });

  # clash-lib still reaches an older containers upper bound here.
  ordered-containers = doJailbreak prev.ordered-containers;

  # clash-lib test tooling still excludes newer template-haskell.
  string-interpolate = doJailbreak prev.string-interpolate;

  # clash-prelude still reaches an older base upper bound here.
  lifted-async = doJailbreak prev.lifted-async;

  # Benchmark/HLS stack still excludes template-haskell 2.24.
  microlens-th = doJailbreak prev.microlens-th;

  # HLS stack still excludes base 4.22 and containers 0.8 here.
  microstache = doJailbreak prev.microstache;

  # HLS stack still excludes template-haskell 2.24 here.
  th-desugar = doJailbreak prev.th-desugar;

  # Hackage revision 2 carries the newer bounds and lists GHC 9.14.1 as tested.
  recursion-schemes = prev.callHackageDirect {
    pkg = "recursion-schemes";
    ver = "5.2.3";
    sha256 = "sha256-SJx1lPWbIct/M6L0luDsRagYsGulojMfLUrNl75OW2I=";
    rev = {
      revision = "2";
      sha256 = "0ppyrsm3vnn1lkfan11583rqn26vgzaihbsqa80s969fdf97zb6m";
    };
  } { };

}
