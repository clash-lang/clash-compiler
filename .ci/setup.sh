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
