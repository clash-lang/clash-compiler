{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs;

let clash = pkgs.haskellPackages.ghcWithPackages (pkgs: [ pkgs.clash-ghc ]);
in
mkShell {
  name = "clash-compiler-shell";
  buildInputs = [
    # For nix dependency management
    niv

    # For quick clash experimentation
    clash
  ];
}
