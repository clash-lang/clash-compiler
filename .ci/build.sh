#!/bin/bash
set -xeo pipefail

# TODO: make sdist work on all, it currently fails for clash-cosim
cabal new-sdist clash-prelude clash-lib clash-ghc

cabal new-build all

# Build with installed constraints for packages in global-db
echo cabal new-build $(ghc-pkg list --global --simple-output --names-only | sed 's/\([a-zA-Z0-9-]\{1,\}\) */--constraint="\1 installed" /g') all | sh

# Check that documentation was generated succesfully
if [ "$GHC" = "ghc-8.6.2" ]; then
  haddock_pkgs="clash-lib clash-cosim"
else
  haddock_pkgs="clash-lib clash-cosim clash-prelude"
fi

for pkg in ${haddock_pkgs}; do
  if [ ! -e dist-newstyle/build/*/ghc-*/${pkg}-*/doc/html/${pkg}/index.html ]; then
    echo "Haddock generation failed for package ${pkg}"
    exit 1
  fi
done
