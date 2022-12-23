{ nixpkgs ? import ../nix/nixpkgs.nix {} }:

with nixpkgs.pkgs;
with gitignore;

# We disable haddock as it doesn't play nice with nix
# (issue with ghc plugins)
haskell.lib.dontHaddock
  (haskellPackages.callCabal2nix "clash-cores" (gitignoreSource ./.) {})
