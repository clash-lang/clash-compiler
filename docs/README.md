# Clash documentation

[![Documentation Status][badge-link]][badge-img]

[badge-img]: https://clash-lang.readthedocs.io/en/latest/?badge=latest
[badge-link]: http://readthedocs.org/projects/clash-lang/badge/?version=latest

Documentation for [Clash](http://clash-lang.org), using Sphinx and
[readthedocs.io](https://readthedocs.io)

Builds for all versions are available [here][http-docs]. PDF, HTML and Epub
downloads are available on the [RtD downloads page][rtd].

[http-docs]: https://clash-lang.readthedocs.io
[rtd]: https://readthedocs.org/projects/clash-lang/downloads

## Current versions

The format is **repository branch**: `readthedocs documentation branch`

 - **master**: `latest` - The latest Clash documentation for the `master`
   branches is always available on the `latest` RtD build branch.
 - **1.0**: `1.0` - The Clash 1.0.x documentation for the `1.0` branches
   are available in the `1.0` RtD build branch (**TODO FIXME**: NIH!)

## How to use this repository

There are a few pointers on how to use this repository, if you're hacking on it.

### Local builds

Local testing can easily and quickly be done using [Nix]:

[Nix]: https://nixos.org/nix

```bash
$ cd clash-docs/
$ nix-shell --pure
[nix-shell:...]$ make help
[nix-shell:...]$ make html
[nix-shell:...]$ make latexpdf # TODO FIXME: nix deps
[nix-shell:...]$ make epub     # TODO FIXME: nix deps
```

The results are in `_build/html`, which can be viewed locally. (**TODO FIXME**:
provide `sphinx-autobuild` for easier automatic hacking.)

Repository pushes are automatically built by RtD, and the results are available
on the corresponding branches on https://clash-lang.readthedocs.io (see "Current
versions" above). RtD provides builds for all of the above formats.

### Updating `nixpkgs`

`clash-docs` uses a fixed version of `nixpkgs` that allows atomic upgrades, and
ensures everyone doing builds locally uses the same version and configuration
for all documentation. (While this isn't true of https://readthedocs.io, it's
still *very* useful.)

The version of nixpkgs used is specified in `nixpkgs.json`. To update this file,
you can use `nix-prefetch-git` which is available under `nix-shell`:

```bash
$ nix-shell --pure
[nix-shell:...]$ nix-prefetch-git https://github.com/nixos/nixpkgs.git > nixpkgs.json
```

This will atomically upgrade all packages upon the next invocation of
`nix-shell`.

# License

BSD2. See `LICENSE` for details.

