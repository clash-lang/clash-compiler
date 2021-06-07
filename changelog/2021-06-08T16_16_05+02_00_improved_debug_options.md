CHANGED: Clash now supports more expressive debug options at the command line [#1800](https://github.com/clash-lang/clash-compiler/issues/1800).

With the old `DebugLevel` type for setting debug options, it was not possible to set certain debug options without implying others, i.e. counting transformations was not possible without also printing at least the final normalized core for a term. It is now possible to set options individually with new flags:

  * -fclash-debug-invariants to check invariants and print warnings / errors
  * -fclash-debug-info to choose how much information to show about individual transformations
  * -fclash-debug-count-transformations to print a tally of each transformation applied

The old -fclash-debug flag is still available for backwards compatibility, and each `DebugLevel` is now a synonym for setting these options together.

