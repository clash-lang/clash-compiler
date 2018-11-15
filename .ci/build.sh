#!/bin/bash
set -xeo pipefail

# run new-update first to generate the cabal config file that we can then modify
cabal new-update

echo "store-dir: ${PWD}/cabal-store" >> ${HOME}/.cabal/config

cabal new-build all -j$THREADS

# Build with installed constraints for packages in global-db
echo cabal new-build $(ghc-pkg list --global --simple-output --names-only | sed 's/\([a-zA-Z0-9-]\{1,\}\) */--constraint="\1 installed" /g') all -j$THREADS | sh
