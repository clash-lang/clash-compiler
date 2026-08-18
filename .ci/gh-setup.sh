#!/usr/bin/env bash
set -xou pipefail

# Make sure everything is up-to-date & installing missing dependencies
sudo apt-get update || exit $?
sudo apt-get install libtinfo-dev -y || exit $?

GIT_ROOT=$(git rev-parse --show-toplevel)
[[ $? -ne 0 ]] && exit 1
CI_DIR=$GIT_ROOT/.ci

# Get the proper amount of cores we can use and configure ghc to use them
THREADS=$($CI_DIR/effective_cpus.sh)
[[ $? -ne 0 ]] && exit 1
sed <$CI_DIR/cabal.project.local.in >$GIT_ROOT/cabal.project.local "
    s/__THREADS__/$THREADS/" || exit $?

echo "--- cabal.project.local ---"
cat $GIT_ROOT/cabal.project.local || exit $?

echo "--- thread count ---"
echo "Using $THREADS threads"
