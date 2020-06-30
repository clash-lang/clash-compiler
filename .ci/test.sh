#!/bin/bash
set -xou pipefail

# Check that clash-dev works
if [[ "$RUN_CLASHDEV" = "yes" ]]; then
  cabal --write-ghc-environment-files=always new-build clash-prelude
  echo "" > clash-dev.result
  echo 'genVHDL "./examples/FIR.hs" >> writeFile "clash-dev.result" "success"' | ./clash-dev
  if [[ "$(cat clash-dev.result)" != "success" ]]; then
    echo "clash-dev test failed"
    exit 1
  fi
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

# Run unittests, doctests
if [[ "$RUN_LIBTESTS" = "yes" ]]; then
  # Create a ghc environment file needed for the doctests
  # On cabal>=2.4.1.0 and ghc<8.4.4 this isn't done automatically
  cabal --write-ghc-environment-files=always new-build all
  cabal new-test clash-cores clash-cosim clash-prelude clash-lib
fi

# Run HDL generation / simulation
if [[ "$RUN_TESTSUITE" = "yes" ]]; then
  cabal new-run -- clash-testsuite -j$THREADS --hide-successes -p "/.VHDL./ || /.Verilog./"
fi
