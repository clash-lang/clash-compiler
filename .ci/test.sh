#!/bin/bash
set -xeo pipefail
cabal new-test clash-cosim clash-prelude
cabal new-run -- clash-testsuite -j$THREADS
