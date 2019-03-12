#!/bin/bash
set -xeo pipefail

apt-get update -q

CABAL="cabal-install-2.4"
if [ "$GHC" = "ghc-head" ]; then
  CABAL="cabal-install-head"
fi

apt-get install -yq $CABAL $GHC
cabal --version
ghc --version
cp .ci/cabal.project.local .
