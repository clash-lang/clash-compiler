{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs;

mkShell {
  name = "clash-compiler-shell";
  buildInputs = [
    # For dependency management
    niv
  ];
}
