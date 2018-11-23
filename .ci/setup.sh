#!/bin/bash
set -xeo pipefail

# Do NOT run apt get update, as deb packages are being cached in the Docker
# image. Running update might cause the testsuite to try and download updated
# versions of the packages. Instead, the docker image should be periodically
# updated.
#apt-get update -q

apt-get install -yq cabal-install-head $GHC
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
