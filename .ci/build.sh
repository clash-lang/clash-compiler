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

# Build with default constraints
cabal v2-build all --write-ghc-environment-files=always

# Put all the test binaries in a predictable location
TESTS="
clash-cores:doctests
clash-cores:unittests
clash-cosim:test
clash-lib:doctests
clash-lib:unittests
clash-prelude:doctests
clash-prelude:unittests
clash-testsuite:clash-testsuite
"
mkdir bin
for TEST in $TESTS; do
  ln -s "$(realpath --relative-to=bin "$(cabal list-bin $TEST)")" bin/$TEST
done

# TODO: remove this and put it back into tests when
# https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12264#note_602406
# is fixed
set +u
if [[ "$SKIP_CLASH_FFI_EXAMPLE" != "yes" ]]; then
  ln -s "$(realpath --relative-to=bin "$(cabal list-bin clash-ffi:ffi-interface-tests)")" bin/clash-ffi:ffi-interface-tests
fi
set -u
