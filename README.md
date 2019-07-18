# Clash - A functional hardware description language

[![Build Status](https://travis-ci.org/clash-lang/clash-compiler.svg?branch=master)](https://travis-ci.org/clash-lang/clash-compiler)
[![Hackage](https://img.shields.io/hackage/v/clash-ghc.svg)](https://hackage.haskell.org/package/clash-ghc)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/clash-ghc.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=exact%3Aclash-ghc)

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

# Support
For updates and questions join the mailing list clash-language+subscribe@googlegroups.com or read the [forum](https://groups.google.com/d/forum/clash-language)

# Using Clash from source
Clone Clash from github using `git` and enter the cloned directory:

```bash
git clone https://github.com/clash-lang/clash-compiler.git
cd clash-compiler
```

Use one of the build tools below to get Clash up and running.

## Cabal
Install Cabal >= 3.0 and GHC >= 8.4. Even though GHC 8.6 is supported, we currently recommend running 8.4 as the former contains some known bugs concerning documentation generation. If you're using Ubuntu, add [HVR's PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc) and install them using APT:

```bash
sudo add-apt-repository -u ppa:hvr/ghc
sudo apt install ghc-8.4.4 cabal-install-3.0
```

Add `/opt/ghc/bin` [to your PATH](https://askubuntu.com/questions/60218/how-to-add-a-directory-to-the-path). Finally, run Clash using `cabal`:

```bash
cabal run --write-ghc-environment-files=always -- clash
```

## Stack

You can use [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) to build and run Clash too:

```bash
stack run -- clash
```

## Nix

Or [use Nix](https://nixos.org/nix/download.html) to get a shell with the `clash` and `clashi` binaries on your PATH:

```bash
nix-shell
```
