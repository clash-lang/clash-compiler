#!/bin/bash
set -xueo pipefail

# Check that documentation was generated successfully
if [[ "$GHC_VERSION" = "8.6.5" ]]; then
  haddock_pkgs="clash-lib clash-lib-hedgehog clash-cosim"
else
  haddock_pkgs="clash-prelude clash-prelude-hedgehog clash-lib clash-lib-hedgehog clash-cosim clash-cores"
fi

mkdir -p hadocs

for pkg in ${haddock_pkgs}; do
  set -e

  # Cache dependencies
  cabal v2-build ${pkg} -O0 --enable-documentation --only-dependencies

  # HaskellPrelude yields warnings we cannot fix
  cabal v2-haddock ${pkg} -O0 --enable-documentation \
    |& grep -v "Clash.HaskellPrelude" \
    |  tee haddock_log

  set +e

  # TODO: Enable check commented below. It will check whether _all_ exported
  #       constructs are properly documented. However, we're not even close to
  #       full coverage, so I'm leaving it disabled until we've got a week to
  #       commit.

  # if grep -q "Missing documentation" haddock_log; then
  #   echo -e "\e[1m\e[31mMissing documentation! Scroll up for full log.\e[0m"
  #   grep --color=always -n -C 5 "Missing documentation" haddock_log
  #   exit 1
  # fi

  if [[ $pkg == "clash-prelude" ]]; then
    # TODO: Currently just checking clash-prelude; other libraries still fail
    # these checks.

    out_of_scope_warn="If you qualify the identifier, haddock can try to link it anyway"
    if grep -q "${out_of_scope_warn}" haddock_log; then
      echo -e "\e[1m\e[31mIdentifier out of scope in ${pkg}! Scroll up for full log.\e[0m"
      grep --color=always -n -C 5 "${out_of_scope_warn}" haddock_log
      exit 1
    fi

    link_dest_warn="could not find link destinations for:"
    if grep -q "${link_dest_warn}" haddock_log; then
      echo -e "\e[1m\e[31mCould not find link destination in ${pkg}! Scroll up for full log.\e[0m"
      grep --color=always -n -C 5 "${link_dest_warn}" haddock_log
      exit 1
    fi

    ambiguous_warn="You may be able to disambiguate the identifier by"
    if grep -q "${ambiguous_warn}" haddock_log; then
      echo -e "\e[1m\e[31mAmbiguous identifier found in ${pkg}! Scroll up for full log.\e[0m"
      grep --color=always -n -C 5 "${ambiguous_warn}" haddock_log
      exit 1
    fi
  fi

  # Copy documention to hadocs/
  ln -s "$(dirname "$(tail -n1 haddock_log)")" hadocs/${pkg}
done
