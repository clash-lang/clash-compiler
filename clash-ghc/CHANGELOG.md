# Changelog for the [`clash-ghc`](http://hackage.haskell.org/package/clash-ghc) package

## 0.5.11 *August 2nd 2015*
* New features:
  * Re-enable GHC's strictness analysis pass, which improves dead-code removal, which hopefully leads to smaller circuits.

## 0.5.10 *July 9th 2015*
* New features:
  * Use new VHDL backend which outputs VHDL-93 instead of VHDL-2002: generated VHDL is now accepted by a larger number of tools.
  * Treat all so-called bottom values (`error "FOO"`, `let x = x in x`, etc.) occuring in installed libraries as `undefined`.
    Before, there were (very) rare situations where we couldn't find the expressions belonging to a function and demanded a BlackBox, even though we knew the expression would be a bottom value.
    Now, we stop demanding a BlackBox for such a function and simply treat it as `undefined`, thus allowing a greater range of circuit descriptions that we can compile.

## 0.5.9 *June 26th 2015*
* New features:
  * Use new verilog backend which outputs Verilog-2001 instead of Verilog-2005: generated Verilog is now accepted by Altera/Quartus

* Fixes bugs:
  * `--systemverilog` switch incorrectly generates verilog code instead of systemverilog code

## 0.5.8 *June 25th 2015*
* New features:
  * Support for copying string literals from Haskell to generated code
  * Support `clash-prelude-0.9`
  * Size at below which functions are always inlined is configurable, run with `-clash-inline-below=N` to set the size limit to `N`

## 0.5.7 *June 3rd 2015*
* New features:
  * New Verilog backend, run `:verilog` in interactive mode, or `--verilog` for batch mode
  * Generated component names are prefixed by the name of the module containing the `topEntity`

## 0.5.6 *May 18th 2015*
* New features:
  * Inlining limit is configurable, run with `-clash-inline-limit=N` to set the inlining limit to `N`
  * Specialisation limit is configurable, run with `clash-spec-limit=N` to set the inline limit to `N`
  * Debug level is configurable, run with `-clash-debug <LEVEL>` where `<LEVEL>` can be: `DebugNone, DebugFinal, DebugName, DebugApplied, DebugAll`. Be default, `clash` runs with `DebugNone`.

* Fixes bugs:
  * Extend evaluator for `GHC.Integer.Type.minusInteger` and `CLaSH.Promoted.Nat.SNat`.

## 0.5.5 *May 5th 2015*
* New features:
  * `TopEntity` wrappers are now specified as `ANN` annotation pragmas [#42](https://github.com/clash-lang/clash-compiler/issues/42)

## 0.5.4 *May 1st 2015*
* New features:
  * Generate wrappers around `topEntity` that have constant names and types

## 0.5.3 *April 24th 2015*
* Fixes bugs:
  * Fix bug where not enough array type definitions were created by the VHDL backend

## 0.5.2 *April 21st 2015*
* Use latest ghc-typelits-natnormalise

## 0.5.1 *April 20th 2015*
* New features:
  * GHC 7.10 support
  * Update to clash-prelude 0.7.2
  * Use http://hackage.haskell.org/package/ghc-typelits-natnormalise typechecker plugin for better type-level natural number handling

## 0.5 *March 11th 2015*
* New features:
  * SystemVerilog backend. [#45](https://github.com/clash-lang/clash-compiler/issues/45)

## 0.4.1 *February 4th 2015*
* Include bug fixes from clash-lib 0.4.1

## 0.4 *November 17th 2014*
* New features:
  * Support for clash-prelude 0.6

* Fixes bugs:
  * clash-ghc ignores "-package-db" flag [#35](https://github.com/christiaanb/clash2/issues/35)

## 0.3.3 *August 12th 2014*
* Fixes bugs:
  * Compile with GHC 7.8.3 [#31](https://github.com/christiaanb/clash2/issues/31)

## 0.3.2 *June 5th 2014*

* Fixes bugs:
  * Type synonym improperly expanded [#17](https://github.com/christiaanb/clash2/issues/17)
  * BlackBox for `Signed` `maxBound` and `minBound` generate incorrect VHDL. [#19](https://github.com/christiaanb/clash2/issues/19)
  * Generate failure code in the VHDL for recSelError [#23](https://github.com/christiaanb/clash2/issues/23)

## 0.3.1 *May 15th 2014*

* New features:
  * Hardcode `fromInteger` for `Signed` and `Unsigned` [#9](https://github.com/christiaanb/clash2/issues/9)
  * Better blackbox operation for vindex [#12](https://github.com/christiaanb/clash2/issues/12)
  * Replace VHDL default hole by error hole [#13](https://github.com/christiaanb/clash2/issues/13)

* Fixes bugs:
  * Update GHC2Core.hs [#1](https://github.com/christiaanb/clash2/issues/1)
  * primitives (clash.sized.vector) [#2](https://github.com/christiaanb/clash2/issues/2)
  * Type families are not expanded [#3](https://github.com/christiaanb/clash2/issues/3)
  * Incorrect vhdl generation for default value in blackbox [#6](https://github.com/christiaanb/clash2/issues/6)
  * Missing begin keyword in Signed/Unsigned JSON files [#16](https://github.com/christiaanb/clash2/issues/16)
