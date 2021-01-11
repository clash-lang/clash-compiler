#!/bin/bash
set -xueo pipefail

# Check that documentation was generated successfully
if [[ "$GHC_VERSION" = "8.6.5" ]]; then
  haddock_pkgs="clash-lib clash-cosim"
else
  haddock_pkgs="clash-lib clash-cosim clash-prelude"
fi

# Cache dependencies
cabal v2-build all --enable-documentation

mkdir -p hadocs

for pkg in ${haddock_pkgs}; do
  cabal v2-haddock ${pkg} --enable-documentation |& tee haddock_log

  set +e

  # TODO: We're not yet ready for the check below :-)
  # if grep -q "Missing documentation" haddock_log; then
  #   echo -e "\e[1m\e[31mMissing documentation! Scroll up for full log.\e[0m"
  #   grep --color=always -n -C 5 "Missing documentation" haddock_log
  #   exit 1
  # fi

  # if grep -q "If you qualify the identifier, haddock can try to link it anyway" haddock_log; then
  #   echo -e "\e[1m\e[31mIdentifier out of scope! Scroll up for full log.\e[0m"
  #   grep --color=always -n -C 5 "If you qualify the identifier, haddock can try to link it anyway" haddock_log
  #   exit 1
  # fi

  # Copy documention to hadocs/
  ln -s "$(dirname "$(tail -n1 haddock_log)")" hadocs/${pkg}
done
