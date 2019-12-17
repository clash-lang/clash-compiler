{}:

let
  ## First off, we have to configure the right nixpkgs
  ## version so all builds are consistent.
  nixpkgs =
    let
      bootstrap = import <nixpkgs> {};
      conf      = with builtins; fromJSON (readFile ./nixpkgs.json);
      src       = bootstrap.fetchFromGitHub {
        owner = "nixos";
        repo  = "nixpkgs";
        inherit (conf) rev sha256;
      };
    in import src {};

  ## Tex is distributed as a multitude of small packages. Pick
  ## the right ones for latex builds.
  texpkgs = with nixpkgs; texlive.combine {
    inherit (texlive)
      scheme-small algorithms
      latexmk collection-latexextra
      helvetic courier;
  };
in

nixpkgs.stdenv.mkDerivation {
  name = "clash-docs";

  shellHook = with nixpkgs; ''
    # This makes it easier to update nixpkgs.json and use git
    export GIT_SSL_CAINFO=${cacert}/etc/ssl/certs/ca-bundle.crt
  '';

  buildInputs = with nixpkgs;
    [ # tools
      ncurses git cacert
      nix-repl nix-prefetch-git

      # sphinx
      pythonPackages.sphinx
      pythonPackages.pygments

      # latex
      texpkgs
    ];
}

