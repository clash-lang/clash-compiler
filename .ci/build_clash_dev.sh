#!/bin/bash
set -xeou pipefail

# Make sure all our deps are build, and listed in the .ghc.environment file
cabal --write-ghc-environment-files=always v2-build --only-dependencies clash-lib

# Check that clash-dev works
echo "" > clash-dev.result
echo 'main >> writeFile "clash-dev.result" "success"' | ./clash-dev
if [[ "$(cat clash-dev.result)" != "success" ]]; then
  echo "clash-dev test failed"
  exit 1
fi
