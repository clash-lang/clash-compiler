#!/bin/sh

# This is just a minimalistic script for demonstrating the process of
# running the clash-testbench example using the Icarus Verilog VVP
# runtime engine. The script is not designed to work in any possible
# system environment and may not work immediately for you. It is
# intended to serve as an easy starter instead. Adapt it to your needs
# if it's not working out-of-the-box for you.

###############################

# Adjust these variables if the tools are not in your PATH already

# Cabal
# https://www.haskell.org/cabal
CABAL=cabal
# Clash
# https://github.com/clash-lang/clash-compiler
CLASH="${CABAL} run clash --"
# Icarus Verilog VVP runtime engine
# http://iverilog.icarus.com
IVERILOG=iverilog
VVP=vvp

###############################

${CABAL} build clash-testbench-example || exit $?
${CLASH} --verilog Calculator.hs || exit $?
${CLASH} --verilog Register.hs || exit $?
${CLASH} --verilog RegisterFail.hs || exit $?
${IVERILOG} verilog/Register.topEntity/topEntity.v -o Register.vvp \
  || exit $?
echo ""
echo "Running Icarus Verilog VVP runtime engine:"
echo ""
${VVP} -Mlib -mlibsimulate-ffi Register.vvp
