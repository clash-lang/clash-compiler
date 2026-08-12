#!/usr/bin/env bash
set -xou pipefail

# Make sure everything is up-to-date & installing missing dependencies
sudo apt-get update
sudo apt-get install libtinfo-dev -y

GIT_ROOT=$(git rev-parse --show-toplevel)
CI_DIR=$GIT_ROOT/.ci
cd CI_DIR

# Get the proper amount of cores we can use and configure ghc to use them
THREADS=$($CI_DIR/effective_cpus.sh)
sed "7s/-j4/-j$THREADS/g" cabal.project.local
cp $CI_DIR/cabal.project.local $GIT_ROOT

echo "--- cabal.project.local ---"
cat $GIT_ROOT/cabal.project.local

echo "--- thread count ---"
echo "Using $THREADS threads"
