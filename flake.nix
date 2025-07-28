{
  description = "A functional hardware description language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

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
    let
      # The versions of GHC that we want to be able to build / develop against
      # within the nix environment. Since nix is lazy, only derivations for
      # versions of GHC which are used are actually evaluated.
      ghcVersions = [ "ghc964" "ghc982" "ghc9101" ];

      # We pick a single version of GHC to use by default within nix. This is
      # probably cleaner than always having N copies of each package / app and
      # being forced to refer to them by their GHC version.
      defaultGhcVersion = "ghc9101";

      # Overlays are not per-system, so let's only compute them once.
      # For each version of GHC we produce a `pkgs.clashPackages-ghcVER`, e.g.
      # `pkgs.clashPackages-ghc962`.
      overlays =
        let
          makeOverlay =
            import (./. + "/nix/overlay.nix") {
              inherit (args)
                ghc-tcplugins-extra
                ghc-typelits-extra
                ghc-typelits-knownnat
                ghc-typelits-natnormalise;
            };

          clashOverlays =
            nixpkgs.lib.attrsets.genAttrs ghcVersions makeOverlay;
        in
        clashOverlays // { default = clashOverlays.${defaultGhcVersion}; };
    in
    # Overlays are specified here, since they are not a per-system attribute of
      # a flake (unlike packages, apps, devShells etc.) which are system-specific.
    { inherit overlays; } //
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Our final nixpkgs has an overlay applied for each supported version of
        # GHC for the repository. We can extract all the flake outputs from this
        # beefed-up nixpkgs.
        pkgs =
          let
            overlay =
              nixpkgs.lib.composeManyExtensions
                (nixpkgs.lib.attrsets.attrValues
                  # The default overlay is just a copy of the overlay for the
                  # default version of GHC. We don't need to apply it twice.
                  (builtins.removeAttrs overlays [ "default" ]));
          in
          nixpkgs.legacyPackages.${system}.extend overlay;
      in
      assert pkgs.lib.asserts.assertOneOf "defaultGhcVersion" defaultGhcVersion ghcVersions;
      {
        # Export the variable so that other flakes can use it.
        ghcVersion = defaultGhcVersion;

        packages = {
          inherit (pkgs."clashPackages-${defaultGhcVersion}")
            clash-benchmark
            clash-cosim
            clash-ffi
            clash-ghc
            clash-lib
            clash-lib-hedgehog
            clash-prelude
            clash-prelude-hedgehog
            clash-profiling
            clash-profiling-prepare
            clash-term
            clash-testsuite;

          default =
            pkgs."clashPackages-${defaultGhcVersion}".clash-ghc;
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

        devShells =
          let
            makeDevShell = import ./nix/devshell.nix {
              inherit pkgs;
            };

            clashDevShells =
              pkgs.lib.attrsets.genAttrs ghcVersions makeDevShell;
          in
          clashDevShells // { default = clashDevShells.${defaultGhcVersion}; };
      }
    );
}
