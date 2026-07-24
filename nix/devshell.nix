{ pkgs }:
compilerVersion:
let
  clashPkgs = pkgs."clashPackages-${compilerVersion}";
in
clashPkgs.shellFor {
  # shellFor combines the dependencies of these packages while filtering the
  # packages themselves from the resulting environment. This lets Cabal build
  # the packages from the working tree instead of requiring their Nix
  # derivations to be built before entering the development shell.
  packages = p: [
    p.clash-benchmark
    p.clash-ghc
    p.clash-lib
    p.clash-lib-hedgehog
    p.clash-prelude
    p.clash-prelude-hedgehog
    p.clash-profiling
    p.clash-profiling-prepare
    p.clash-term
    p.clash-testsuite
  ];

  buildInputs = [
    clashPkgs.cabal-install
    clashPkgs.haskell-language-server

    # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
    pkgs.bashInteractive

    pkgs.ghdl-llvm
    pkgs.nixpkgs-fmt
    pkgs.sby
    pkgs.verilator
    pkgs.iverilog
    pkgs.yosys
  ];
}
