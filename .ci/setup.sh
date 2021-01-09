#!/bin/bash
set -xou pipefail

grep -E ' $' -n -r . --include=*.{hs,hs-boot,sh} --exclude-dir=dist-newstyle
if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi

set -e

# Check whether version numbers in snap / clash-{prelude,lib,ghc} are the same
cabal_files="clash-prelude/clash-prelude.cabal clash-lib/clash-lib.cabal clash-ghc/clash-ghc.cabal clash-cores/clash-cores.cabal"
snapcraft_file=".ci/bindist/linux/snap/snap/snapcraft.yaml"
versions=$(grep "^[vV]ersion" $cabal_files $snapcraft_file | grep -Eo '[0-9]+(\.[0-9]+)+')

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

if [[ ${tag_version} != "" && ${version} != ${tag_version} ]]; then
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

# GHC files are compressed when building docker image to save space
cd /root/.ghcup/ghc
time pixz -d ../${GHC_VERSION}.tpxz
time tar -xf ../${GHC_VERSION}.tar
ghcup set ghc ${GHC_VERSION}
ghcup set cabal ${CABAL_VERSION}
cd -

cabal --version
ghc --version

# File may exist as part of a dist.tar.zst
if [ ! -f cabal.project.local ]; then
  cp .ci/cabal.project.local .

  MULTIPLE_HIDDEN=${MULTIPLE_HIDDEN:-yes}
  if [[ "$MULTIPLE_HIDDEN" == "yes" ]]; then
    sed -i 's/flags: +doctests/flags: +doctests +multiple-hidden/g' cabal.project.local
  elif [[ "$MULTIPLE_HIDDEN" == "no" ]]; then
    sed -i 's/flags: +doctests/flags: +doctests -multiple-hidden/g' cabal.project.local
  fi

  if [[ "$CI_COMMIT_BRANCH" =~ "^partial-evaluator-" ]]; then
    sed -i 's/-experimental-evaluator/+experimental-evaluator/g' cabal.project.local
  fi

  if [[ "$GHC_VERSION" == 9.*.*.* ]]; then
    echo "
    repository head.hackage.ghc.haskell.org
    url: https://ghc.gitlab.haskell.org/head.hackage/
    secure: True
    key-threshold: 3
    root-keys:
        7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
        26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
        f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
    " >> cabal.project.local
  fi

  # Fix index-state to prevent rebuilds if Hackage changes between build -> test.
  sed -i "s/HEAD/$(date -u +%FT%TZ)/g" cabal.project.local
fi

cat cabal.project.local

rm -f ${HOME}/.cabal/config
cabal user-config init
sed -i "s/-- ghc-options:/ghc-options: -j$THREADS/g" ${HOME}/.cabal/config
sed -i "s/^[- ]*jobs:.*/jobs: $CABAL_JOBS/g" ${HOME}/.cabal/config
sed -i "/remote-repo-cache:.*/d" ${HOME}/.cabal/config
cat ${HOME}/.cabal/config

set +u

# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0
until cabal new-update || [ $NEXT_WAIT_TIME -eq 5 ]; do
  sleep $(( NEXT_WAIT_TIME++ ))
done
