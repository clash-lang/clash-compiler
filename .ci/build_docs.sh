#!/bin/bash
set -xueo pipefail

# Check that documentation was generated successfully
haddock_pkgs="clash-prelude clash-prelude-hedgehog clash-lib clash-lib-hedgehog clash-cosim"

mkdir -p hadocs

for pkg in ${haddock_pkgs}; do
  set -e

  # Cache dependencies
  cabal v2-build ${pkg} -O0 --enable-documentation --only-dependencies

  # The preludes yield link destination warnings we cannot fix. Maybe fixed by:
  # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14447?
  #
  # Some other modules complain about ambiguous identifiers, but they're
  # complaining about identifiers in temporary files, i.e. some Haddock
  # implementation detail.
  cabal v2-haddock ${pkg} -O0 --enable-documentation \
    |& grep -v "Warning: Clash.HaskellPrelude: could not find link destinations for" \
    |& grep -v "Warning: Clash.Explicit.Prelude.Safe: could not find link destinations for" \
    |& grep -v "Warning: Clash.Prelude.Safe: could not find link destinations for" \
    |& grep -v "Warning: Clash.Explicit.Prelude: could not find link destinations for" \
    |& grep -v "Warning: Clash.Prelude: could not find link destinations for" \
    |& grep -v "Warning: 'SNat' is ambiguous. It is defined" \
    |& grep -v "Warning: 'head' is ambiguous. It is defined" \
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

  if [[ "${pkg}" != "clash-lib" && "${pkg}" != "clash-lib-hedgehog" ]]; then
    # TODO: Currently not checking `clash-lib` as it still fails these checks.
    #       `clash-lib-hedgehog` is blocked on
    #       https://github.com/clash-lang/clash-compiler/issues/2462

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

    ambiguous_warn="^Warning: '.*' is ambiguous. It is defined"
    if grep -E -q "${ambiguous_warn}" haddock_log; then
      echo -e "\e[1m\e[31mAmbiguous identifier found in ${pkg}! Scroll up for full log.\e[0m"
      grep --color=always -n -C 5 "${ambiguous_warn}" haddock_log
      exit 1
    fi
  fi

  # Copy documention to hadocs/
  ln -s "$(dirname "$(tail -n1 haddock_log)")" hadocs/${pkg}
done
