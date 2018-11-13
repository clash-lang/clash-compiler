#!/bin/bash
set -xeo pipefail
apt-get update -q
apt-get install -yq cabal-install-head $GHC
cabal --version
ghc --version
cp .ci/cabal.project.local .

# see https://github.com/haskell/haddock/issues/900
if [ "$GHC" == ghc-8.6.2 ]; then
echo "Haddock and doctests are broken on GHC 8.6, disabling"
cp .ci/cabal.project.local-8.6 cabal.project.local
fi
