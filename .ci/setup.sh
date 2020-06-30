#!/bin/bash
set -xou pipefail

egrep ' $' -n -r . --include=*.{hs,hs-boot,sh} --exclude-dir=dist-newstyle
if [[ $? == 0 ]]; then
    echo "EOL whitespace detected. See ^"
    exit 1;
fi

set -e

# Check whether version numbers in snap / clash-{prelude,lib,ghc} are the same
cabal_files="clash-prelude/clash-prelude.cabal clash-lib/clash-lib.cabal clash-ghc/clash-ghc.cabal clash-cores/clash-cores.cabal"
snapcraft_file="bindist/linux/snap/snap/snapcraft.yaml"
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

# Here to test whether all these variables are set
echo "RUN_HADDOCK=${RUN_HADDOCK}"
echo "RUN_LIBTESTS=${RUN_LIBTESTS}"
echo "RUN_TESTSUITE=${RUN_TESTSUITE}"
echo "RUN_CLASHDEV=${RUN_CLASHDEV}"
echo "RUN_BUILD_ALL=${RUN_BUILD_ALL}"

apt-get update -q

if [[ "$GHC" = ghc-head ]]; then
  CABAL="cabal-head"
elif [[ "$GHC" = ghc-8.4.* ]]; then
  CABAL="cabal-2.4"
elif [[ "$GHC" = ghc-8.6.* || "$GHC" = ghc-8.8.* ]]; then
  CABAL="cabal-3.0"
else
  # GHC >= 8.10
  CABAL="cabal-3.2"
fi

update-alternatives --set opt-ghc /opt/ghc/bin/${GHC}
update-alternatives --set opt-cabal /opt/cabal/bin/${CABAL}

cabal --version
ghc --version
cp .ci/cabal.project.local .

if [[ "$MULTIPLE_HIDDEN" == "yes" ]]; then
  sed -i 's/flags: +doctests/flags: +doctests +multiple-hidden/g' cabal.project.local
elif [[ "$MULTIPLE_HIDDEN" == "no" ]]; then
  sed -i 's/flags: +doctests/flags: +doctests -multiple-hidden/g' cabal.project.local
fi

if [[ "$GHC" == "ghc-head" ]]; then
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

cat cabal.project.local

cabal user-config init
sed -i "s/-- ghc-options:/ghc-options: -j$THREADS/g" ${HOME}/.cabal/config
sed -i "s/^[- ]*jobs:.*/jobs: $CABAL_JOBS/g" ${HOME}/.cabal/config
echo "store-dir: ${PWD}/cabal-store" >> ${HOME}/.cabal/config
sed -i "/remote-repo-cache:.*/d" ${HOME}/.cabal/config
echo "remote-repo-cache: ${PWD}/cabal-packages" >> ${HOME}/.cabal/config
cat ${HOME}/.cabal/config

# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0
until cabal new-update || [ $NEXT_WAIT_TIME -eq 5 ]; do
   sleep $(( NEXT_WAIT_TIME++ ))
done
