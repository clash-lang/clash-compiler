{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs;

mkShell {
  name = "clash-compiler-shell";
  buildInputs = [
    # For nix dependency management
    niv

    # For quick clash experimentation
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
      clash-ghc

      ghc-typelits-extra
      ghc-typelits-knownnat
      ghc-typelits-natnormalise
    ])
    )
  ];
}
