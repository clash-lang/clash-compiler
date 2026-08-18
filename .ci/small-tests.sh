#!/usr/bin/env bash
set -xou pipefail

GIT_ROOT=$(git rev-parse --show-toplevel)
[[ $? -ne 0 ]] && exit 1
CI_DIR=$GIT_ROOT/.ci

if [ ! -f $GIT_ROOT/cabal.project.local ]; then
    echo "Requires gh-setup.sh to be invoked first, refusing to do that manually as to not create unintended effects"
    exit 1;
fi

THREADS=$($CI_DIR/effective_cpus.sh)
[[ $? -ne 0 ]] && exit 1

# Check for EOL whitespace
grep -E ' $' -n -r . --include=*.{hs,hs-boot,sh} --exclude-dir=dist-newstyle
if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi

# Check whether version numbers in
# clash-{prelude{,-hedgehog},lib{,-hedgehog},ghc} are the same
cabal_files="$GIT_ROOT/clash-prelude/clash-prelude.cabal $GIT_ROOT/clash-prelude-hedgehog/clash-prelude-hedgehog.cabal $GIT_ROOT/clash-lib/clash-lib.cabal $GIT_ROOT/clash-lib-hedgehog/clash-lib-hedgehog.cabal $GIT_ROOT/clash-ghc/clash-ghc.cabal"
versions=$(grep "^[vV]ersion" $cabal_files | grep -Eo '[0-9]+(\.[0-9]+)+')
[[ $? -ne 0 ]] && exit 1

if [[ $(echo $versions | tr ' ' '\n' | wc -l) == 5 ]]; then
    if [[ $(echo $versions | tr ' ' '\n' | uniq | wc -l) != 1 ]]; then
        echo "Expected all distributions to have the same version number. Found: $versions"
        exit 1;
    fi
else
    echo "Expected to find version number in all distributions. Found: $versions";
    exit 1;
fi

#Test if cabal can generate a build plan using the index state as specified in cabal.project
# We should not have cabal.project.local in place for this
mv $GIT_ROOT/cabal.project.local $GIT_ROOT/cabal.project.local.disabled || exit $?
cabal v2-build -j$THREADS --dry-run all > /dev/null || { echo "Maybe state index should be updated?"; exit 1; }
mv $GIT_ROOT/cabal.project.local.disabled $GIT_ROOT/cabal.project.local || exit $?
