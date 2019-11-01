{ nixpkgs ? import ../nix/nixpkgs.nix {} }:

with nixpkgs.pkgs;
with gitignore;

haskell.lib.enableSharedExecutables
  (haskellPackages.callCabal2nix "clash-ghc" (gitignoreSource ./.) {})
