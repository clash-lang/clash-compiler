# `clash-lib` - Clash compiler, as a library

  * See the LICENSE file for license and copyright details

# Clash - A functional hardware description language
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
