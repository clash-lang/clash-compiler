{ nixpkgs ? import ../../../../nix/nixpkgs.nix {} }:

with nixpkgs;

let
  nix-user-chroot = stdenv.lib.makeOverridable stdenv.mkDerivation {
    name = "nix-user-chroot-2c52b5f";
    src = ./nix-user-chroot;

    makeFlags = [];

    # hack to use when /nix/store is not available
    postFixup = ''
      exe=$out/bin/nix-user-chroot
      patchelf \
        --set-interpreter .$(patchelf --print-interpreter $exe) \
        --set-rpath $(patchelf --print-rpath $exe | sed 's|/nix/store/|./nix/store/|g') \
        $exe
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin/
      cp nix-user-chroot $out/bin/nix-user-chroot
      runHook postInstall
    '';
  };
in buildEnv {
  name = "clash-compiler-bindist";
  paths = [
    bash
    nix-user-chroot
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [clash-prelude clash-lib]))
    pkgs.haskellPackages.clash-ghc
  ];
}
