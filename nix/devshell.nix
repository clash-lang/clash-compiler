{ pkgs }:
compilerVersion:
let
  clashPkgs = pkgs."clashPackages-${compilerVersion}";
in
pkgs.mkShell {
  inputsFrom = [
    clashPkgs.clash-benchmark.env
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

    # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
    pkgs.bashInteractive

    pkgs.nixpkgs-fmt
    pkgs.sby
    pkgs.verilator
    pkgs.iverilog
    pkgs.yosys
  ] ++
  # depends on gnat14 which doesn't work ATM on aarch64:
  # https://github.com/NixOS/nixpkgs/issues/469109
  pkgs.lib.optional (!pkgs.stdenv.hostPlatform.isAarch64) [pkgs.ghdl-llvm];
}
