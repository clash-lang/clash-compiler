#!/bin/bash
set -xueo pipefail

# TODO: make sdist work on all, it currently fails for clash-cosim
cabal new-sdist clash-prelude clash-lib clash-ghc

# test that we can create a build plan with the index-state in cabal.project
mv cabal.project.local cabal.project.local.disabled
cabal new-build --dry-run all > /dev/null || (echo Maybe the index-state should be updated?; false)
mv cabal.project.local.disabled cabal.project.local

if [[ "$RUN_BUILD_ALL" = "yes" ]]; then
  # Build with installed constraints for packages in global-db
  echo cabal new-build $(ghc-pkg list --global --simple-output --names-only | sed 's/\([a-zA-Z0-9-]\{1,\}\) */--constraint="\1 installed" /g') all | sh

  # Build with default constraints
  cabal new-build all
fi

if [[ "$RUN_HADDOCK" = "yes" ]]; then
  # Check that documentation was generated successfully
  if [[ "$GHC" = "ghc-8.6.2" ]]; then
    haddock_pkgs="clash-lib clash-cosim"
  else
    haddock_pkgs="clash-lib clash-cosim clash-prelude"
  fi

  for pkg in ${haddock_pkgs}; do
    cabal new-haddock ${pkg}
  done
fi
