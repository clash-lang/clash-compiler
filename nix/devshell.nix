{ pkgs, qlog, tilia-src }:
compilerVersion:
let
  clashPkgs = pkgs."clashPackages-${compilerVersion}";
  tilia = pkgs.haskell.lib.justStaticExecutables (
    # The upstream tests download formatting corpora and need network access.
    pkgs.haskell.lib.dontCheck (pkgs.haskellPackages.callCabal2nix
      "tilia"
      tilia-src
      {
        ghc-lib-parser = pkgs.haskellPackages.ghc-lib-parser_9_14_1_20251220;
      })
  );
  haskellLanguageServer =
    if compilerVersion == "ghc9141" then
      [ ]
    else
      [ clashPkgs.haskell-language-server ];
in
clashPkgs.shellFor {
  # shellFor combines the dependencies of these packages while filtering the
  # packages themselves from the resulting environment. This lets Cabal build
  # the packages from the working tree instead of requiring their Nix
  # derivations to be built before entering the development shell.
  packages = p: [
    p.clash-benchmark
    p.clash-ghc
    p.clash-lib
    p.clash-lib-hedgehog
    p.clash-prelude
    p.clash-prelude-hedgehog
    p.clash-profiling
    p.clash-profiling-prepare
    p.clash-term
    p.clash-testsuite
  ];

  buildInputs = [
    pkgs.cabal-install
    tilia

    # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
    pkgs.bashInteractive

    pkgs.ghdl-clash
    pkgs.nixpkgs-fmt
    pkgs.sby
    pkgs.verilator
    # Optional: clash-testsuite uses ccache, when present, to cache Verilator's
    # runtime objects across tests.
    pkgs.ccache
    pkgs.iverilog
    pkgs.yosys

    # Tool used to manage the changelog, see 'changelog/README.md'.
    qlog
  ] ++ haskellLanguageServer;
}
