# CλaSH - A functional hardware description language

[![Build Status](https://travis-ci.org/clash-lang/clash-prelude.svg?branch=master)](https://travis-ci.org/clash-lang/clash-prelude)
[![Hackage](https://img.shields.io/hackage/v/clash-prelude.svg)](https://hackage.haskell.org/package/clash-prelude)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/clash-prelude.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=exact%3Aclash-prelude)

__WARNING__
Only works with GHC-8.2+
([GHC 8.2.1](https://www.haskell.org/ghc/download_ghc_8_2_1),
 [GHC 8.2.2](https://www.haskell.org/ghc/download_ghc_8_2_2),
 [GHC 8.4.1](https://www.haskell.org/ghc/download_ghc_8_4_1),
 [GHC 8.4.2](https://www.haskell.org/ghc/download_ghc_8_4_2),
 [GHC 8.4.3](https://www.haskell.org/ghc/download_ghc_8_4_3))!

CλaSH (pronounced ‘clash’) is a functional hardware description language that
borrows both its syntax and semantics from the functional programming language
Haskell. The CλaSH compiler transforms these high-level descriptions to
low-level synthesizable VHDL, Verilog, or SystemVerilog.

Features of CλaSH:

  * Strongly typed, yet with a very high degree of type inference, enabling both
    safe and fast prototyping using concise descriptions.

  * Interactive REPL: load your designs in an interpreter and easily test all
    your component without needing to setup a test bench.

  * Higher-order functions, with type inference, result in designs that are
    fully parametric by default.

  * Synchronous sequential circuit design based on streams of values, called
    `Signal`s, lead to natural descriptions of feedback loops.

  * Support for multiple clock domains, with type safe clock domain crossing.

# Support
For updates and questions join the mailing list
clash-language+subscribe@googlegroups.com or read the
[forum](https://groups.google.com/d/forum/clash-language)
