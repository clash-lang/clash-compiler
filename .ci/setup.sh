#!/bin/bash
set -xeou pipefail

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

# run new-update first to generate the cabal config file that we can then modify
# retry 5 times, as hackage servers are not perfectly reliable
NEXT_WAIT_TIME=0
until cabal new-update || [ $NEXT_WAIT_TIME -eq 5 ]; do
   sleep $(( NEXT_WAIT_TIME++ ))
done

sed -i "s/-- ghc-options:/ghc-options: -j$THREADS/g" ${HOME}/.cabal/config
sed -i "s/^[- ]*jobs:.*/jobs: $CABAL_JOBS/g" ${HOME}/.cabal/config
echo "store-dir: ${PWD}/cabal-store" >> ${HOME}/.cabal/config
