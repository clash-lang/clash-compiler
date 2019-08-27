#!/bin/bash
set -xeo pipefail

apt-get update -q

if [[ "$GHC" = ghc-head ]]; then
  CABAL="cabal-install-head"
elif [[ "$GHC" = ghc-8.8.* ]]; then
  CABAL="cabal-install-3.0"
else
  # GHC <= 8.6
  CABAL="cabal-install-2.4"
fi

apt-get install -yq $CABAL $GHC
cabal --version
ghc --version
cp .ci/cabal.project.local .

if [[ "$MULTIPLE_HIDDEN" == "yes" ]]; then
  sed -i 's/flags: +doctests/flags: +doctests +multiple-hidden/g' cabal.project.local
elif [[ "$MULTIPLE_HIDDEN" == "no" ]]; then
  sed -i 's/flags: +doctests/flags: +doctests -multiple-hidden/g' cabal.project.local
fi

cat cabal.project.local

# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0
until cabal new-update || [ $NEXT_WAIT_TIME -eq 5 ]; do
   sleep $(( NEXT_WAIT_TIME++ ))
done

sed -i "s/-- ghc-options:/ghc-options: -j$THREADS/g" ${HOME}/.cabal/config
sed -i "s/^[- ]*jobs:.*/jobs: $CABAL_JOBS/g" ${HOME}/.cabal/config
echo "store-dir: ${PWD}/cabal-store" >> ${HOME}/.cabal/config
