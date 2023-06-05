# Clash Nix Infrastructure

Clash provides a flake-based Nix infrastructure for developing. This is split
across a few files:

    * `flake.nix`

      The main entry to the infrastructure is the `flake.nix`, which defines
      the Nix flake for the repository. This specifies all of the inputs (i.e.
      external repositories and nixpkgs) and outputs (packages, overlays, the
      development environment) for working with Clash.

    * `nix/overlay.nix`

      This file defines an overlay for defining `pkgs.clashPackages-ghcXXX`
      for a particular version of GHC. This overlay contains specific changes
      needed for the GHC version used, and makes all external packages (e.g. the
      typechecker plugins) and internal packages accessible.

    * `nix/overlay-ghcXXX.nix`

      These files provide the changes needed for a specific version of GHC.
      When adding support for a new version of GHC this is all that needs to
      be changed.

    * `nix/devshell.nix`

      This file defines a development environment which contains everything
      needed to build the packages in this repository, and standalone tooling
      which is often useful during development, such as vendor tools.

To change the default version of GHC used to build packages and provide the
development shell, simply change the `defaultGhcVersion` in `flake.nix`. It
must be a member of the `ghcVersions` list, and a specific overlay for that
version of GHC must exist in `nix/`.

## Recommended Cabal Setup

When using `cabal`, it will attempt to download any missing dependencies from
`hackage` when configuring. When changing a `.cabal` file, it is necessary to
be careful that the package is fetched / built with Nix first to avoid this.

This foot-gun can be lessened by adding `offline: True` to the global
configuration for Cabal, so an error will be shown if the dependency has not
already been made available via Nix (i.e. with `nix build`).

## Recommended Nix Setup

To make the environment more user-friendly, there are some recommendations
about how to setup your system. In your `nix.conf` the following should be
included:

```
experimental-features = nix-command flakes
substituters = [...] https://clash-lang.cachix.org
trusted-public-keys = [...] clash-lang.cachix.org-1:/2N1uka38B/heaOAC+Ztd/EWLmF0RLfizWgC5tamCBg=
```

This configuration enables the use of Nix flakes and the `nix` CLI commands,
which are used in this repository without needing to explicitly enable the
features on the command line.

For `substituters` and `trusted-public-keys`, we recommend you include the
`clash-lang` cache on Cachix. Enabling this cache will mean fewer derivations
need to be built locally.

## Using `direnv`

To enable automatic entry and exit of the development environment, install
[`direnv`](https://direnv.net/) and [`nix-direnv`](https://github.com/nix-community/nix-direnv),
then add an `.envrc` file similar to this:

```
use flake
watch_file nix/*
```

Additional arguments that Nix would normally expect can be passed after
`use flake`, e.g. `use flake --cores=$(nproc)`. This file is a normal shell
script and supports arbitrary additional commands to be included.

The environment can then be enabled by executing `direnv allow`.
