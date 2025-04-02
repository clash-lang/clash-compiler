#!/bin/bash
set -xou pipefail

grep -E ' $' -n -r . --include=*.{hs,hs-boot,sh} --exclude-dir=dist-newstyle
if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi

set -e

# Check whether version numbers in
# clash-{prelude{,-hedgehog},lib{,-hedgehog},ghc} are the same
cabal_files="clash-prelude/clash-prelude.cabal clash-prelude-hedgehog/clash-prelude-hedgehog.cabal clash-lib/clash-lib.cabal clash-lib-hedgehog/clash-lib-hedgehog.cabal clash-ghc/clash-ghc.cabal"
versions=$(grep "^[vV]ersion" $cabal_files | grep -Eo '[0-9]+(\.[0-9]+)+')

if [[ $(echo $versions | tr ' ' '\n' | wc -l) == 5 ]]; then
    if [[ $(echo $versions | tr ' ' '\n' | uniq | wc -l) != 1 ]]; then
        echo "Expected all distributions to have the same version number. Found: $versions"
        exit 1;
    fi
else
    echo "Expected to find version number in all distributions. Found: $versions";
    exit 1;
fi

# You'd think comparing v${version} with ${CI_COMMIT_TAG} would do the
# trick, but no..
CI_COMMIT_TAG=${CI_COMMIT_TAG:-}
version=$(echo $versions | tr ' ' '\n' | head -n 1)
tag_version=${CI_COMMIT_TAG:1:${#CI_COMMIT_TAG}-1}  # Strip first character (v0.99 -> 0.99)

# `tag_version` is set when a tag has been created on GitHub. We use this to
# trigger a release pipeline (release to Hackage).
if [[ ${tag_version} != "" ]]; then

    if [[ ${version} != ${tag_version} ]]; then
        if [[ "${CI_COMMIT_TAG:0:1}" == "v" ]]; then
            echo "Tag name and distribution's release number should match:"
            echo "  Tag version:          ${CI_COMMIT_TAG}"
            echo "  Distribution version: v${version}"
            exit 1;
        else
            echo "\$CI_COMMIT_TAG should start with a 'v'. Found: ${CI_COMMIT_TAG}"
            exit 1;
        fi
    fi
fi

# Print out versions for debugging purposes
cabal --version
ghc --version

# This might happen during tags on GitLab CI
CI_COMMIT_BRANCH=${CI_COMMIT_BRANCH:-no_branch_set_by_ci}

cabal v2-update | tee cabal_update_output

# File may exist as part of a dist.tar.zst
if [ ! -f cabal.project.local ]; then
  cp .ci/cabal.project.local .

  set +u
  if [[ "$WORKAROUND_GHC_MMAP_CRASH" == "yes" ]]; then
    sed -i 's/-workaround-ghc-mmap-crash/+workaround-ghc-mmap-crash/g' cabal.project.local
  fi

  if [[ "$GHC_HEAD" == "yes" ]]; then
    cat .ci/cabal.project.local.append-HEAD >> cabal.project.local
  fi
  set -u

  # Fix index-state to prevent rebuilds if Hackage changes between build -> test.
  # Note we can't simply set it to a timestamp of "now", as Cabal will error out
  # when its index state is older than what's mentioned in cabal.project(.local).
  most_recent_index_state=$(grep "The index-state is set to" cabal_update_output | grep -E -o '[^ ]+Z.$' | tr -d .)
  sed -i "s/HEAD/${most_recent_index_state}/g" cabal.project.local
fi

cat cabal.project.local

rm -f ${HOME}/.cabal/config
cabal user-config init
sed -i "s/-- ghc-options:/ghc-options: -j$THREADS/g" ${HOME}/.cabal/config
set +u
if [[ "$WORKAROUND_GHC_MMAP_CRASH" == "yes" ]]; then
  sed -i "s/ghc-options:/ghc-options: +RTS -xm20000000 -RTS -with-rtsopts=-xm20000000/g" ${HOME}/.cabal/config
fi
set -u
sed -i "s/^[- ]*jobs:.*/jobs: $CABAL_JOBS/g" ${HOME}/.cabal/config
sed -i "/remote-repo-cache:.*/d" ${HOME}/.cabal/config
cat ${HOME}/.cabal/config

set +u
