{ nixpkgs ? import ../nix/nixpkgs.nix {} }:

with nixpkgs.pkgs;
with gitignore;

# We disable tests as doctest-parallel doesn't play nice with nix
haskell.lib.dontCheck

  (haskellPackages.callCabal2nix "clash-lib" (gitignoreSource ./.) {})
