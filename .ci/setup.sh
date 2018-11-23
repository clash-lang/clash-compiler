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

case "$GHC" in
  ghc-8.6* )
    # see https://github.com/haskell/haddock/issues/900
    echo "Haddock and doctests are broken on GHC 8.6, disabling"
    cp .ci/cabal.project.local-8.6 cabal.project.local
    ;;
  ghc-head )
    # singletons is broken on head: https://github.com/goldfirere/singletons/issues/357
    # This constraint on a future version makes the ghc-head build fail fast
    echo 'constraints: singletons > 2.5.1' >> cabal.project.local
  ;;
esac
