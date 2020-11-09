#!/bin/bash
set -xeou pipefail

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
