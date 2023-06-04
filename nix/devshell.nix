{ pkgs }:
compilerVersion:
let
  hsPkgs = pkgs.haskell.packages.${compilerVersion};
in
pkgs.mkShell {
  buildInputs = [
    hsPkgs.cabal-install
    hsPkgs.haskell-language-server

    pkgs.ghdl-llvm
    pkgs.symbiyosys
    pkgs.verilator
    pkgs.verilog
    pkgs.yosys
  ];

  passthru = pkgs;
}
