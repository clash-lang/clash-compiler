#!/bin/sh

# This is just a minimalistic script for demonstrating the process of
# running the clash-ffi example using the ModelSim / QuestaSim runtime
# engine. The script is not designed to work in any possible system
# environment and may not work immediately for you. It is intended to
# serve as an easy starter instead. Adapt it to your needs if it's not
# working out-of-the-box for you.

###############################

# Adjust these variables if the tools are not in your PATH already

# Cabal
# https://www.haskell.org/cabal
CABAL=cabal
# Clash
# https://github.com/clash-lang/clash-compiler
CLASH="${CABAL} run clash --"
# ModelSim / QuestaSim binaries
VLIB=vlib
VLOG=vlog
VSIM=vsim
# Clash examples folder
# https://github.com/clash-lang/clash-compiler/tree/master/examples
EXAMPLES=../../examples

###############################

${CABAL} build clash-ffi-example || exit $?
${CLASH} --verilog -i${EXAMPLES} ${EXAMPLES}/Calculator.hs || exit $?
${VLIB} work || exit $?
${VLOG} verilog/Calculator.topEntity/topEntity.v || exit $?
echo ""
echo "Running Simulation Runtime Engine:"
echo ""
${VSIM} -no_autoacc -c -do "onfinish exit; run -all" topEntity -pli lib/libclash-ffi-example.vpl
