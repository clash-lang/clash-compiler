#!/bin/bash
set -xeo pipefail

# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0
until cabal new-update || [ $NEXT_WAIT_TIME -eq 5 ]; do
   sleep $(( NEXT_WAIT_TIME++ ))
done

echo "store-dir: ${PWD}/cabal-store" >> ${HOME}/.cabal/config

cabal new-build all -j$THREADS

# Build with installed constraints for packages in global-db
echo cabal new-build $(ghc-pkg list --global --simple-output --names-only | sed 's/\([a-zA-Z0-9-]\{1,\}\) */--constraint="\1 installed" /g') all -j$THREADS | sh
