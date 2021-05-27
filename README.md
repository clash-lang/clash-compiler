# Clash - A functional hardware description language

[![Pipeline status](https://gitlab.com/clash-lang/clash-compiler/badges/master/pipeline.svg)](https://gitlab.com/clash-lang/clash-compiler/commits/master)
[![Hackage](https://img.shields.io/hackage/v/clash-ghc.svg)](https://hackage.haskell.org/package/clash-ghc)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/clash-ghc.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=QBayLogic)
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

# Community
**Mailing list**: for updates and questions join the mailing list clash-language+subscribe@googlegroups.com or read the [forum](https://groups.google.com/d/forum/clash-language)

**Slack**: [functionalprogramming.slack.com#clash](https://functionalprogramming.slack.com/archives/CPGMJFF50) (Invite yourself at [fpchat-invite.herokuapp.com](http://fpchat-invite.herokuapp.com)).

**IRC**: [libera.chat#clash (webchat access)](https://web.libera.chat/#clash) (or ircs://irc.libera.chat:6697/clash if you want to use a regular IRC client)

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

## Cabal
To use Cabal you need both Cabal and GHC installed on your system. For Linux and MacOS users we recommend using [ghcup](https://www.haskell.org/ghcup/). Windows users are recommended to use the [Haskell Platform](https://www.haskell.org/platform/windows.html).

To run `clash` use:

```
cabal v2-run --write-ghc-environment-files=always -- clash
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
nix-shell
```

# Related libraries and initiatives

* [Clash Protocols](https://gitlab.com/clash-lang/clash-protocols): experimental library for writing Clash circuits with bidirectional communication - such as AXI or Avalon.
* [Clash Starters](https://github.com/clash-lang/clash-starters): starter projects to quickly get you up and running.

# Projects built with Clash

* [Contranomy](https://github.com/christiaanb/contranomy): a RISCV implementation verified using the [RISC-V Formal Verification Framework](https://github.com/SymbioticEDA/riscv-formal).
* [Space Invaders](https://github.com/gergoerdi/clash-spaceinvaders): a Clash implementation of the 1978 Space Invaders arcade machine by Taito.
