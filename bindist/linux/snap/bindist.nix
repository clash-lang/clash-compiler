{ nixpkgs ? import ../../../nix/nixpkgs.nix {} }:

with nixpkgs;

buildEnv {
  name = "clash-compiler-bindist";
  paths = [
    bash
    pkgs.binutils
    pkgs.haskellPackages.cabal-install
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [clash-prelude clash-lib]))
    pkgs.haskellPackages.clash-ghc
  ];
}
