{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs.pkgs.haskellPackages;

{ inherit clash-ghc clash-lib clash-prelude clash-cores; }
