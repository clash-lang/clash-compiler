# Clash - A functional hardware description language

[![Pipeline status](https://gitlab.com/clash-lang/clash-compiler/badges/master/pipeline.svg)](https://gitlab.com/clash-lang/clash-compiler/commits/master)
[![Hackage](https://img.shields.io/hackage/v/clash-ghc.svg)](https://hackage.haskell.org/package/clash-ghc)
[![Documentation Status](https://readthedocs.org/projects/clash-lang/badge/?version=latest)](https://clash-lang.readthedocs.io/en/latest/?badge=latest)

Clash is a functional hardware description language that borrows both
its syntax and semantics from the functional programming language
Haskell. The Clash compiler transforms these high-level descriptions to
low-level synthesizable VHDL, Verilog, or SystemVerilog.

Features of Clash:

  * Strongly typed, but with a very high degree of type inference, enabling both
    safe and fast prototyping using concise descriptions.

  * Interactive REPL: load your designs in an interpreter and easily test all
    your component without needing to setup a test bench.

  * Higher-order functions, with type inference, result in designs that are
    fully parametric by default.

  * Synchronous sequential circuit design based on streams of values, called
    `Signal`s, lead to natural descriptions of feedback loops.

  * Support for multiple clock domains, with type safe clock domain crossing.

# Open-source community
Clash benefits from an active community. Whether you need a question answered or
want to contribute to open-source features, browse the features below to make
the most of Clash.

- [Discourse: long form discussions and questions](https://clash-lang.discourse.group/)
- [Discord: short form discussions and community chat room](https://discord.gg/rebGq25FB4)
- [Slack: short form discussions and questions](https://functionalprogramming.slack.com/archives/CPGMJFF50)
  (Invite yourself at [fpslack.com](https://fpslack.com))
- [Github: issue tracker](https://github.com/clash-lang/clash-compiler/issues)

# Get Clash
Check out [clash-lang.org/install](https://clash-lang.org/install/) to install the latest stable release of Clash, or to setup a Clash project.

# Get Clash from source
Get the source code using [Git](https://git-scm.com/book/en/v2/Getting-Started-What-is-Git%3F) and enter the cloned directory:

```bash
git clone git@github.com:clash-lang/clash-compiler.git

# Alternatively, if you haven't setup SSH keys with GitHub:
# git clone https://github.com/clash-lang/clash-compiler.git

cd clash-compiler
```

To check out a released version, use:

```
git checkout v1.2.3
```

To checkout a release _branch_ use:

```
git checkout 1.2
```

Note that release branches might contain non-released patches.

## GHC compatibility
|      | Linux                           | Windows                         | macOS                           | Clash (released) | Clash (development version)
|------|---------------------------------|---------------------------------|---------------------------------|------------------|------------------------------
| 8.6  | &#x2714;&#xfe0f;                | &#x2714;&#xfe0f;                | &#x2714;&#xfe0f;                | 1.0 - 1.8        | &#x274c;
| 8.8  | &#x2714;&#xfe0f;                | &#x274c;                        | &#x2714;&#xfe0f;                | 1.0 - 1.8        | &#x274c;
| 8.10 | &#x2714;&#xfe0f;                | &#x2714;&#xfe0f;                | &#x274c;                        | 1.2 - 1.8        | &#x2714;&#xfe0f;
| 9.0  | &#x2714;&#xfe0f;                | &#x2714;&#xfe0f;<sup>2</sup>    | &#x2714;&#xfe0f;                | 1.4 - 1.8        | &#x2714;&#xfe0f;
| 9.2  | &#x26a0;&#xfe0f;<sup>1</sup>    | &#x26a0;&#xfe0f;<sup>1</sup>    | &#x26a0;&#xfe0f;<sup>1</sup>    | 1.8              | &#x26a0;&#xfe0f;<sup>1</sup>
| 9.4  | &#x26a0;&#xfe0f;<sup>3</sup> ️   | &#x26a0;&#xfe0f;<sup>3</sup>    | ️&#x26a0;&#xfe0f;<sup>3</sup> ️   | 1.8              | &#x2714;&#xfe0f;
| 9.6  | &#x2714;&#xfe0f;<sup>3</sup>    | &#x2714;&#xfe0f;<sup>3,4</sup>  | &#x2714;&#xfe0f;<sup>3</sup>    | 1.8              | &#x2714;&#xfe0f;
| 9.8  | &#x2714;&#xfe0f;                | &#x2714;&#xfe0f;<sup>4</sup>    | &#x2714;&#xfe0f;                | 1.8              | ️&#x2714;&#xfe0f;
| 9.10 | &#x2714;&#xfe0f;                | &#x2714;&#xfe0f;<sup>4</sup>    | &#x2714;&#xfe0f;                | 1.8              | ️&#x2714;&#xfe0f;


<sup>1</sup> GHC 9.2 contains a regression, rendering Clash error messages indecipherable. This change was reverted in 9.4.

<sup>2</sup> GHC 9.0.2 on Windows fails to compile `clash-cores`. We therefore don't run the Clash test suite on CI for this combination.

<sup>3</sup> Clash starts extremely slowly when compiled with 9.4.8 up to and including 9.6.2. Consider downgrading to 9.4.7 or upgrading to 9.6.3 and up.

<sup>4</sup> Clash starts extremely slowly on Windows with GHC 9.6 through 9.10, consider downgrading to 9.4.7

## Cabal
To use Cabal you need both Cabal and GHC installed on your system. We recommend using [ghcup](https://www.haskell.org/ghcup/). For more information, see [https://www.haskell.org/downloads/](https://www.haskell.org/downloads/).

To run `clash` use:

```
cabal v2-run -- clash
```

If this fails, make sure you've got an up-to-date package index:

```
cabal update
```

## Stack
[Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) and run:

```
stack run -- clash
```
## Nix
Or [use Nix](https://nixos.org/nix/download.html) to get a shell with the `clash` and `clashi` binaries on your PATH:

```bash
# Start the default dev shell
nix develop .

# Start a dev shell with a specific GHC version
nix develop .#ghc9101
```

You will need a modern version of nix with support for the new-style `nix`
command and flakes (2.4 or newer). Support for these must still be manually
enabled, this can be done by setting

```
experimental-features = nix-command flakes
```

in your `nix.conf`.

To automatically enter and exit the `nix` environment on directory change, you
can install [`direnv`](https://direnv.net/) and
[`nix-direnv`](https://github.com/nix-community/nix-direnv) and write the
following to a `.envrc` file in the root of this repository:

```
use flake

watch_file nix/*
```

Upon adding or changing this file you must `direnv allow` in order for the file
to be automatically loaded / reloaded on project changes.

Individual packages / applications can also be built or run using the `nix
build` and `nix run` commands, i.e.

```
nix build .#clash-ghc
nix run .#clashi
```

# Related libraries and initiatives

* [Clashilator](https://github.com/gergoerdi/clashilator): tooling to integrate Clash with Verilator. Enables fast, multithreaded simulation by compiling Clash designs to C++.
* [Clash Protocols](https://github.com/clash-lang/clash-protocols): experimental library for writing Clash circuits with bidirectional communication - such as AXI or Avalon.
* [Clash Starters](https://github.com/clash-lang/clash-starters): starter projects to quickly get you up and running.
* [Clash WaveDrom](https://github.com/expipiplus1/clash-wavedrom): generate wave diagrams from Clash using [WaveDrom](https://wavedrom.com/)

# Projects built with Clash

* [Contranomy](https://github.com/christiaanb/contranomy): a RISCV implementation verified using the [RISC-V Formal Verification Framework](https://github.com/SymbioticEDA/riscv-formal).
* [Space Invaders](https://github.com/gergoerdi/clash-spaceinvaders): a Clash implementation of the 1978 Space Invaders arcade machine by Taito.
