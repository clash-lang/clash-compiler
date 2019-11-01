{ nixpkgs ? import ../nix/nixpkgs.nix {} }:

with nixpkgs.pkgs;
with gitignore;

haskell.lib.overrideCabal
  (haskellPackages.callCabal2nix "clash-ghc" (gitignoreSource ./.) {})
  (_: {
    enableSharedExecutables = true;
  })
