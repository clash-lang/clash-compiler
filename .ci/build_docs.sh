#!/bin/bash
set -xueo pipefail

# Check that documentation was generated successfully
if [[ "$GHC_VERSION" = "8.6.5" ]]; then
  haddock_pkgs="clash-lib clash-cosim"
else
  haddock_pkgs="clash-lib clash-cosim clash-prelude"
fi

for pkg in ${haddock_pkgs}; do
  cabal v2-haddock ${pkg} --enable-documentation
done
