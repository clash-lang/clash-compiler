# This overlay turns a Clash package set into one that is built against
# dependency versions that our version bounds allow, but that are newer than
# what nixpkgs provides. CI builds and tests this package set for the most
# recent GHC only, making sure our upper bounds keep being tested after they
# are bumped. See 'packages_common' in '.github/workflows/on-push.yml'.
#
# In Cabal terms, this overlay is roughly the equivalent of a
# 'cabal.project.local' with:
#
#   constraints: tagged == 0.9
#   allow-newer: *:tagged
#
# To test a new upper bound, add the package to 'newer' and pin its version
# below. Remove it again once nixpkgs ships that version by default.

{ pkgs }:
final: prev:
let
  inherit (pkgs) lib;

  # Packages for which we ignore the bounds of third-party packages, similar to
  # Cabal's 'allow-newer'. Many of our (transitive) dependencies don't allow the
  # newest versions of these packages yet. The bounds of our own packages are
  # still checked: this package set should fail to build if our bounds exclude
  # the versions pinned below.
  newer = [ "tagged" ];

  # Removes the version bounds on packages in 'newer' from the Cabal file of a
  # package. We can't use 'doJailbreak', as it ignores bounds in conditional
  # blocks, and neither can we use '--allow-newer', as only cabal-install
  # supports it. We only patch packages that directly depend on one of 'newer',
  # so we don't needlessly rebuild packages that the pins below don't affect.
  allowNewer = args:
    let
      deps = lib.concatMap (attr: args.${attr} or [ ]) [
        "setupHaskellDepends"
        "libraryHaskellDepends"
        "executableHaskellDepends"
        "testHaskellDepends"
        "benchmarkHaskellDepends"
      ];
      depNames = map (dep: dep.pname or null) deps;
      relaxed = lib.filter (name: lib.elem name depNames) newer;
    in
    lib.optionalString (relaxed != [ ] && !lib.hasPrefix "clash-" args.pname) ''
      echo "Removing version bounds on: ${lib.concatStringsSep " " relaxed}"
      sed -i -E \
        's/(^|[[:space:],:])(${lib.concatStringsSep "|" relaxed})[[:space:]]*(\^>=|>=|<=|==|<|>)[^,]*/\1\2/g' \
        *.cabal
    '';
in
{
  # Note that 'postPatch' is set unconditionally. Deciding whether to set it at
  # all would force the dependencies of a package while evaluating the package
  # itself, which loops on packages with cyclic test dependencies.
  mkDerivation = args: prev.mkDerivation (args // {
    postPatch = (args.postPatch or "") + allowNewer args;
  });

  tagged = prev.callHackageDirect {
    pkg = "tagged";
    ver = "0.9";
    sha256 = "0vn8fkcibz0dn4ifcf3xyk6xrhiif632740zgjq9hjx9dr2h9gfz";
  } { };
}
