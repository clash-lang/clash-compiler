# `clash-ghc` - Haskell/GHC front-end for the Clash compiler

  * See the LICENSE file for license and copyright details
  * Contains code from the [GHC compiler](http://haskell.org/ghc), see the
    LICENSE_GHC file for license and copyright details pertaining to that code.

# Clash - A functional hardware description language
Clash is a functional hardware description language that borrows both
its syntax and semantics from the functional programming language
Haskell. The Clash compiler transforms these high-level descriptions to
low-level synthesizable VHDL, Verilog, or SystemVerilog.

Features of Clash:

  * Strongly typed (like VHDL), yet with a very high degree of type inference,
    enabling both safe and fast prototying using consise descriptions (like
    Verilog).

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
