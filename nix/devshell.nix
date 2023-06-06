{ pkgs }:
compilerVersion:
let
  clashPkgs = pkgs."clashPackages-${compilerVersion}";
in
pkgs.mkShell {
  inputsFrom = [
    clashPkgs.clash-benchmark.env
    clashPkgs.clash-cores.env
    clashPkgs.clash-cosim.env
    clashPkgs.clash-ffi.env
    clashPkgs.clash-ghc.env
    clashPkgs.clash-lib.env
    clashPkgs.clash-lib-hedgehog.env
    clashPkgs.clash-prelude.env
    clashPkgs.clash-prelude-hedgehog.env
    clashPkgs.clash-profiling.env
    clashPkgs.clash-profiling-prepare.env
    clashPkgs.clash-term.env
    clashPkgs.clash-testsuite.env
  ];

  buildInputs = [
    clashPkgs.cabal-install
    clashPkgs.haskell-language-server

    pkgs.ghdl-llvm
    pkgs.nixpkgs-fmt
    pkgs.symbiyosys
    pkgs.verilator
    pkgs.verilog
    pkgs.yosys
  ];
}
