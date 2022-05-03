#!/bin/bash
set -xueo pipefail

# Generate source distributions for all our packages
# TODO: `sdist clash-cosim` only works _after_ building it
cabal v2-sdist \
    clash-cores \
    clash-ffi \
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

# Build with installed constraints for packages in global-db
echo cabal v2-build $(ghc-pkg list --global --simple-output --names-only | sed 's/\([a-zA-Z0-9-]\{1,\}\) */--constraint="\1 installed" /g') all | sh

# Build with default constraints
cabal v2-build all --write-ghc-environment-files=always
