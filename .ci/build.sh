#!/bin/bash
set -xeo pipefail

# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0
until cabal new-update || [ $NEXT_WAIT_TIME -eq 5 ]; do
   sleep $(( NEXT_WAIT_TIME++ ))
done

sed -i "s/-- ghc-options:/ghc-options: -j$THREADS/g" ${HOME}/.cabal/config
sed -i "s/^[- ]*jobs:.*/jobs: $CABAL_JOBS/g" ${HOME}/.cabal/config
echo "store-dir: ${PWD}/cabal-store" >> ${HOME}/.cabal/config

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
