#!/bin/bash
set -xeo pipefail

# Create a ghc environment file needed for the doctests
# On cabal>=2.4.1.0 and ghc<8.4.4 this isn't done automaticly
cabal --write-ghc-environment-files=always new-build all

cabal new-test clash-cosim clash-prelude
cabal new-run -- clash-testsuite -j$THREADS --hide-successes
