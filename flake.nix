{
  description = "A functional hardware description language";

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
      pkgs = nixpkgs.legacyPackages.${system};

      ghcPkgs =
        let
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
    in
    {
      packages = {
        clash-cores =
          ghcPkgs.callCabal2nixWithOptions "clash-cores" ./clash-cores "--flag nix" {
            inherit (self.packages.${system})
              clash-lib
              clash-prelude
              clash-prelude-hedgehog;
          };

        clash-cosim =
          (ghcPkgs.callCabal2nix "clash-cosim" ./clash-cosim {
            inherit (self.packages.${system}) clash-prelude;
          }).overrideAttrs(old: {
            # Ensure iverilog is in the path, since Setup.hs checks for vvp.
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
              pkgs.verilog
            ];
          });

        clash-ffi =
          ghcPkgs.callCabal2nix "clash-ffi" ./clash-ffi {
            inherit (self.packages.${system}) clash-prelude;
          };

        clash-ghc =
          ghcPkgs.callCabal2nix "clash-ghc" ./clash-ghc {
            inherit (self.packages.${system}) clash-prelude clash-lib;
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

        default = self.packages.${system}.clash-ghc;
      };

      apps = {
        clash = {
          type = "app";
          program = "${self.packages.${system}.clash-ghc}/bin/clash";
        };

        clashi = {
          type = "app";
          program = "${self.packages.${system}.clash-ghc}/bin/clashi";
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
        default =
          pkgs.mkShell {
            inputsFrom = [
              self.packages.${system}.clash-cores.env
              self.packages.${system}.clash-cosim.env
              self.packages.${system}.clash-ffi.env
              self.packages.${system}.clash-ghc.env
              self.packages.${system}.clash-lib.env
              self.packages.${system}.clash-lib-hedgehog.env
              self.packages.${system}.clash-prelude.env
              self.packages.${system}.clash-prelude-hedgehog.env
            ];

            buildInputs = [
              ghcPkgs.cabal-install
              ghcPkgs.haskell-language-server

              pkgs.cabal2nix
              pkgs.ghdl-llvm
              pkgs.symbiyosys
              pkgs.verilog
            ];
          };
      };
    }
  );
}
