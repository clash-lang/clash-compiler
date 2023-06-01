{
  description = "A functional hardware description language";

  nixConfig = {
    extra-substituters = "https://clash-lang.cachix.org";
    extra-trusted-public-keys = "https://clash-lang.cachix.org-1:/2N1uka38B/heaOAC+Ztd/EWLmF0RLfizWgC5tamCBg=";
    extra-experimental-features = "nix-command flakes";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    # Explicitly add these repositories, since in development we always want
    # to be able to bump to the bleeding edge of these packages without needing
    # to mess around with the overlays.

    ghc-tcplugins-extra = {
      url = "github:clash-lang/ghc-tcplugins-extra";
      flake = false;
    };

    ghc-typelits-extra = {
      url = "github:clash-lang/ghc-typelits-extra";
      flake = false;
    };

    ghc-typelits-knownnat = {
      url = "github:clash-lang/ghc-typelits-knownnat";
      flake = false;
    };

    ghc-typelits-natnormalise = {
      url = "github:clash-lang/ghc-typelits-natnormalise";
      flake = false;
    };
  };

  outputs = args@{ self, nixpkgs, flake-utils, ... }:
  flake-utils.lib.eachDefaultSystem (system:
    let
      # Try to avoid changing this to `import nixpkgs { inherit system; };`
      # unless you really need to apply overlays to nixpkgs. Using
      # `legacyPackages` means we don't need to evaluate nixpkgs again.
      pkgs = nixpkgs.legacyPackages.${system};

      ghcPkgs =
        let
          # To change the version of GHC used, you need to change this string,
          # and create an overlay in the ./nix directory if one does not exist
          # for that version of GHC. Nothing else below this line should need
          # to be changed day-to-day.
          ghc = "ghc96";
          overlayPath = ./. + "/nix/overlay-${ghc}.nix";

          overlay =
            if builtins.pathExists overlayPath
              then import overlayPath {
                  inherit pkgs;
                  inherit (args)
                    ghc-tcplugins-extra
                    ghc-typelits-extra
                    ghc-typelits-knownnat
                    ghc-typelits-natnormalise;
                }
              else throw "No overlay for ${ghc} found at ${overlayPath}";
        in
        pkgs.haskell.packages.${ghc}.extend(overlay);

      # Each package here corresponds to a cabal package in the repository.
      # Where dependencies of packages are also from this repository, we
      # override the default derivation (from nixpkgs) and substitute it for
      # the version build from source here.

      clash-benchmark =
        ghcPkgs.callCabal2nix "clash-benchmark" ./benchmark {
          inherit (self.packages.${system})
            clash-ghc
            clash-lib
            clash-prelude;
        };

      clash-cores =
        ghcPkgs.callCabal2nixWithOptions "clash-cores" ./clash-cores "--flag nix" {
          inherit (self.packages.${system})
            clash-lib
            clash-prelude
            clash-prelude-hedgehog;
        };

      clash-cosim =
        ghcPkgs.callCabal2nix "clash-cosim" ./clash-cosim {
          inherit (self.packages.${system}) clash-prelude;
        };

      clash-ffi =
        ghcPkgs.callCabal2nix "clash-ffi" ./clash-ffi {
          inherit (self.packages.${system}) clash-prelude;
        };

      clash-ghc =
        ghcPkgs.callCabal2nixWithOptions "clash-ghc" ./clash-ghc "--flag use-ghc-paths" {
          inherit (self.packages.${system}) clash-lib clash-prelude;
        };

      clash-lib =
        ghcPkgs.callCabal2nix "clash-lib" ./clash-lib {
          inherit (self.packages.${system}) clash-prelude;
        };

      clash-lib-hedgehog =
        ghcPkgs.callCabal2nix "clash-lib-hedgehog" ./clash-lib-hedgehog {
          inherit (self.packages.${system}) clash-lib;
        };

      clash-prelude =
        ghcPkgs.callCabal2nix "clash-prelude" ./clash-prelude {};

      clash-prelude-hedgehog =
        ghcPkgs.callCabal2nix "clash-prelude-hedgehog" ./clash-prelude-hedgehog {
          inherit (self.packages.${system}) clash-prelude;
        };

      clash-profiling =
        ghcPkgs.callCabal2nix "clash-profiling" ./benchmark/profiling/run {
          inherit (self.packages.${system})
            clash-benchmark
            clash-ghc
            clash-lib
            clash-prelude
            clash-profiling-prepare;
        };

      clash-profiling-prepare =
        ghcPkgs.callCabal2nix "clash-profiling-prepare" ./benchmark/profiling/prepare {
          inherit (self.packages.${system})
            clash-benchmark
            clash-lib
            clash-prelude;
          };

      clash-term =
        ghcPkgs.callCabal2nix "clash-term" ./clash-term {
          inherit (self.packages.${system}) clash-lib;
        };

      clash-testsuite =
        ghcPkgs.callCabal2nix "clash-testsuite" ./tests {
          inherit (self.packages.${system})
            clash-cores
            clash-ghc
            clash-lib
            clash-prelude;
        };
    in
    {
      packages = {
        # These packages need no extra configuration other than that already
        # provided by cabal2nix.
        inherit
          clash-cores
          clash-ffi
          clash-lib
          clash-lib-hedgehog
          clash-prelude
          clash-prelude-hedgehog
          clash-profiling
          clash-term;

        clash-benchmark =
          clash-benchmark.overrideAttrs(old: {
            buildInputs = (old.buildInputs or []) ++ [
              pkgs.makeWrapper
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

        # We need to include iverilog in the build environment for clash-cosim,
        # since it checks the executable is in scope in its Setup.hs.
        clash-cosim =
          clash-cosim.overrideAttrs(old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
              pkgs.verilog
            ];
          });

        # The performance of clash-ghc when linked statically is abysmally
        # worse than dynamically. On my machine, building `examples/ALU.hs` in
        # `clash-testsuite` is around 60s when static and under a second when
        # dynamic..
        clash-ghc =
          pkgs.haskell.lib.enableSharedExecutables
            (clash-ghc.overrideAttrs(old: {
              buildInputs = (old.buildInputs or []) ++ [
                pkgs.makeWrapper
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

        # We need to override the environment of clash-profiling-prepare to
        # include the GHC and packages from the build environment, otherwise
        # we will not be able to use the resulting executable.
        clash-profiling-prepare =
          clash-profiling-prepare.overrideAttrs(old: {
            buildInputs = (old.buildInputs or []) ++ [
              pkgs.makeWrapper
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

        # We need to override the environment of clash-testsuite to include the
        # GHC and packages from the build environment, and any vendor tooling
        # which clash-testsuite needs at runtime, otherwise we will not be
        # able to use the resulting executable.
        clash-testsuite =
          clash-testsuite.overrideAttrs(old: {
            buildInputs = (old.buildInputs or []) ++ [
              pkgs.makeWrapper
            ];

            postInstall = (old.postInstall or "") + ''
              wrapProgram $out/bin/clash-testsuite \
                --add-flags "--no-modelsim --no-vivado" \
                --prefix PATH : ${dirOf "${old.passthru.env.NIX_GHC}"} \
                --set GHC_PACKAGE_PATH "${old.passthru.env.NIX_GHC_LIBDIR}/package.conf.d:" \
                --prefix PATH : ${pkgs.lib.makeBinPath [
                  pkgs.gcc
                  pkgs.ghdl-llvm
                  pkgs.symbiyosys
                  pkgs.verilator
                  pkgs.verilog
                  pkgs.yosys
                ]} \
                --set LIBRARY_PATH ${pkgs.lib.makeLibraryPath [
                  pkgs.ghdl-llvm
                  pkgs.zlib.static
                ]}
            '';
          });

        # When not specified, i.e. when just running `nix build .`, we default
        # to building `clash-ghc`, since this is usually what we care about.
        default = self.packages.${system}.clash-ghc;
      };

      apps = {
        # Executables listed here can be run using the `nix run` command, which
        # is more convenient than finding / digging around in the store path of
        # the output from `nix build`.

        clash = {
          type = "app";
          program = "${self.packages.${system}.clash-ghc}/bin/clash";
        };

        clashi = {
          type = "app";
          program = "${self.packages.${system}.clash-ghc}/bin/clashi";
        };

        clash-benchmark-normalization = {
          type = "app";
          program = "${self.packages.${system}.clash-benchmark}/bin/clash-benchmark-normalization";
        };

        clash-benchmark-concurrency = {
          type = "app";
          program = "${self.packages.${system}.clash-benchmark}/bin/clash-benchmark-concurrency";
        };

        clash-profile-netlist-prepare = {
          type = "app";
          program = "${self.packages.${system}.clash-profiling-prepare}/bin/clash-profile-netlist-prepare";
        };

        clash-profile-netlist-run = {
          type = "app";
          program = "${self.packages.${system}.clash-profiling}/bin/clash-profile-netlist-run";
        };

        clash-profile-normalization-prepare = {
          type = "app";
          program = "${self.packages.${system}.clash-profiling-prepare}/bin/clash-profile-normalization-prepare";
        };

        clash-profile-normalization-run = {
          type = "app";
          program = "${self.packages.${system}.clash-profiling}/bin/clash-profile-normalization-run";
        };

        clash-term = {
          type = "app";
          program = "${self.packages.${system}.clash-term}/bin/clash-term";
        };

        clash-testsuite = {
          type = "app";
          program = "${self.packages.${system}.clash-testsuite}/bin/clash-testsuite";
        };

        default = self.apps.${system}.clashi;
      };

      devShells = {
        # We only provide one devshell (accessible via `nix develop`) which
        # provides an environment capable of building this entire repository,
        # running `clash-testsuite`, and simulating designs using free tooling.

        default =
          pkgs.mkShell {
            # The devshell contains all the inputs for these derivations, i.e.
            # the necessary environment needed to build all of these packages.
            # We could make this list shorter by only keeping the roots of the
            # dependency graph of these projects, but it's much more reliable
            # to just list everything explicitly.
            inputsFrom = [
                clash-benchmark.env
                clash-cores.env
                clash-cosim.env
                clash-ffi.env
                clash-ghc.env
                clash-lib.env
                clash-lib-hedgehog.env
                clash-prelude.env
                clash-prelude-hedgehog.env
                clash-profiling.env
                clash-profiling-prepare.env
                clash-testsuite.env
              ];

            # As well as the dependencies needed to build all packages, we
            # provide common tooling that might be wanted while developing
            # the clash compiler.
            buildInputs = [
              ghcPkgs.cabal-install
              ghcPkgs.haskell-language-server

              pkgs.ghdl-llvm
              pkgs.symbiyosys
              pkgs.verilator
              pkgs.verilog
              pkgs.yosys
            ];
          };
      };
    }
  );
}
