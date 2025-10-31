# This overlay contains everything which is non-specific to the version of GHC
# used. Since it is applied to nixpkgs, we also import and apply the more
# specific overlay for the version of GHC in `compilerVersion`.
#
# We should only need to edit this file directly if we want to add additional
# packages to the environment, or need to change the configuration of the
# different packages maintained by QBayLogic.

{ ghc-tcplugins-extra
, ghc-typelits-extra
, ghc-typelits-knownnat
, ghc-typelits-natnormalise
}:
compilerVersion:
final: prev:
let
  # An overlay with the things we need to change for the specified GHC version.
  # The overlays are named without the GHC minor version, so we need to strip the last character from the version
  strippedMinorVersion = builtins.substring 0 (builtins.stringLength compilerVersion - 1) compilerVersion;
  ghcOverlay = import (./. + "/overlay-${strippedMinorVersion}.nix") {
    pkgs = prev;
  };

  # An overlay with the packages we pull in as inputs to this flake.
  #
  # This is mostly intended for packages developed by QBayLogic which are
  # standalone repositories, e.g. the typechecker plugins needed for Clash.
  haskellExternalPackages =
    hfinal: hprev: {
      ghc-tcplugins-extra =
        hprev.callCabal2nix
          "ghc-tcplugins-extra"
          "${ghc-tcplugins-extra}"
          { };

      ghc-typelits-extra =
        hprev.callCabal2nix
          "ghc-typelits-extra"
          "${ghc-typelits-extra}"
          { };

      ghc-typelits-knownnat =
        hprev.callCabal2nix
          "ghc-typelits-knownnat"
          "${ghc-typelits-knownnat}"
          { };

      ghc-typelits-natnormalise =
        hprev.callCabal2nix
          "ghc-typelits-natnormalise"
          "${ghc-typelits-natnormalise}"
          { };
    };

  # An overlay with the packages in this repository.
  haskellInternalPackages =
    hfinal: hprev: {
      clash-benchmark =
        let
          unmodified = hprev.callCabal2nix "clash-benchmark" ../benchmark {
            inherit (hfinal) clash-ghc clash-lib clash-prelude;
          };
        in
        unmodified.overrideAttrs (old: {
          buildInputs = (old.buildInputs or [ ]) ++ [
            prev.makeWrapper
          ];

          postInstall = (old.postInstall or "") + ''
            wrapProgram $out/bin/clash-benchmark-concurrency \
              --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
              --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:"

            wrapProgram $out/bin/clash-benchmark-normalization \
              --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
              --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:"
          '';
        });

      clash-cores =
        hprev.callCabal2nixWithOptions "clash-cores" ../clash-cores "--flag nix" {
          inherit (hfinal) clash-prelude;
        };

      clash-cosim =
        let
          unmodified =
            hprev.callCabal2nix "clash-cosim" ../clash-cosim {
              inherit (hfinal) clash-prelude;
            };
        in
        unmodified.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
            prev.verilog
          ];
        });

      clash-ffi =
        hprev.callCabal2nix "clash-ffi" ../clash-ffi {
          inherit (hfinal) clash-prelude;
        };

      clash-ghc =
        let
          unmodified =
            hprev.callCabal2nixWithOptions
              "clash-ghc"
              ../clash-ghc
              "--flag workaround-ghc-mmap-crash" {
              inherit (hfinal) clash-lib clash-prelude;
            };
        in
        prev.haskell.lib.enableSharedExecutables
          (unmodified.overrideAttrs (old: {
            buildInputs = (old.buildInputs or [ ]) ++ [
              prev.makeWrapper
            ];

            postInstall = (old.postInstall or "") + ''
              wrapProgram $out/bin/clash \
                --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
                --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:"

              wrapProgram $out/bin/clashi \
                --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
                --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:"
            '';
          }));

      clash-lib =
        hprev.callCabal2nix "clash-lib" ../clash-lib {
          inherit (hfinal) clash-prelude;
        };

      clash-lib-hedgehog =
        hprev.callCabal2nix "clash-lib-hedgehog" ../clash-lib-hedgehog {
          inherit (hfinal) clash-lib;
        };

      clash-prelude =
        hprev.callCabal2nixWithOptions
          "clash-prelude"
          ../clash-prelude
          "--flag workaround-ghc-mmap-crash"
          { };

      clash-prelude-hedgehog =
        hprev.callCabal2nix "clash-prelude-hedgehog" ../clash-prelude-hedgehog {
          inherit (hfinal) clash-prelude;
        };

      clash-profiling =
        hprev.callCabal2nix "clash-profiling" ../benchmark/profiling/run {
          inherit (hfinal)
            clash-benchmark
            clash-ghc
            clash-lib
            clash-prelude
            clash-profiling-prepare;
        };

      clash-profiling-prepare =
        let
          unmodified =
            hprev.callCabal2nix "clash-profiling-prepare" ../benchmark/profiling/prepare {
              inherit (hfinal) clash-benchmark clash-lib clash-prelude;
            };
        in
        unmodified.overrideAttrs (old: {
          buildInputs = (old.buildInputs or [ ]) ++ [
            prev.makeWrapper
          ];

          postInstall = (old.postInstall or "") + ''
            wrapProgram $out/bin/clash-profile-netlist-prepare \
              --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
              --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:"

            wrapProgram $out/bin/clash-profile-normalization-prepare \
              --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
              --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:"
          '';
        });

      clash-term =
        hprev.callCabal2nix "clash-term" ../clash-term {
          inherit (hfinal) clash-lib;
        };

      clash-testsuite =
        let
          unmodified =
            hprev.callCabal2nixWithOptions
              "clash-testsuite"
              ../tests
              "--flag workaround-ghc-mmap-crash" {
              inherit (hfinal) clash-ghc clash-lib clash-prelude;
            };
        in
        unmodified.overrideAttrs (old: {
          buildInputs = (old.buildInputs or [ ]) ++ [
            prev.makeWrapper
          ];

          postInstall = (old.postInstall or "") + ''
            wrapProgram $out/bin/clash-testsuite \
              --add-flags "--no-modelsim --no-vivado" \
              --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
              --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:" \
              --prefix PATH : ${prev.lib.makeBinPath [
                prev.gcc
                prev.ghdl-llvm
                prev.symbiyosys
                prev.verilator
                prev.verilog
                prev.yosys
              ]} \
              --set LIBRARY_PATH ${prev.lib.makeLibraryPath [
                prev.ghdl-llvm
                prev.zlib.static
              ]}
          '';
        });
    };

  haskellOverlays =
    prev.lib.composeManyExtensions [
      ghcOverlay
      haskellExternalPackages
      haskellInternalPackages
    ];
in
{
  "clashPackages-${compilerVersion}" =
    prev.haskell.packages.${compilerVersion}.extend haskellOverlays;
}
