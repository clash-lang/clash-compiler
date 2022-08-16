#!/bin/bash
set -xueo pipefail

# Generate source distributions for all our packages
# TODO: `sdist clash-cosim` only works _after_ building it
cabal v2-sdist \
    clash-cores \
    clash-ghc \
    clash-lib \
    clash-lib-hedgehog \
    clash-prelude \
    clash-prelude-hedgehog

# test that we can create a build plan with the index-state in cabal.project
set +u
if [[ "$GHC_HEAD" != "yes" ]]; then
  mv cabal.project.local cabal.project.local.disabled
  [[ ! -f cabal.project.freeze ]] || mv cabal.project.freeze cabal.project.freeze.disabled
  cabal v2-build --dry-run all > /dev/null || (echo Maybe the index-state should be updated?; false)
  mv cabal.project.local.disabled cabal.project.local
  [[ ! -f cabal.project.freeze.disabled ]] || mv cabal.project.freeze.disabled cabal.project.freeze
fi
set -u

# Build compiler _libraries_, but not the executable. This makes sure the executable
# is being built by the job that is going to use it too. In turn, this prevents
# weird errors where executables cannot find some shared libraries.
cabal v2-build \
  lib:clash-prelude \
  lib:clash-prelude-hedgehog \
  lib:clash-lib \
  lib:clash-lib-hedgehog \
  lib:clash-ghc \
  lib:clash-cores \
  lib:clash-testsuite

# Make sure all dependencies of all packages in the clash-compiler project are
# compiled. This makes sure no future CI job is going to build it. Furthermore,
# this command makes sure we transfer all the right dependencies as artifacts -
# as they will end up in Cabal's 'plan.json'.
cabal v2-build all --only-dependencies
