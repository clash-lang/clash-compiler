# Changelog for the [`clash-ghc`](http://hackage.haskell.org/package/clash-ghc) package

## 0.6.9
* Fixes bugs:
  * `case undefined of ...` should reduce to `undefined` [#116](https://github.com/clash-lang/clash-compiler/issues/109)
  * BlockRAM elements must be bit vectors [#113](https://github.com/clash-lang/clash-compiler/issues/113)

## 0.6.8 *January 13th 2015*
* New features:
  * Support for Haskell's: `Char`, `Int8`, `Int16`, `Int32`, `Int64`, `Word`, `Word8`, `Word16`, `Word32`, `Word64`.
  * Int/Word/Integer bitwidth for generated HDL is configurable using the `-clash-intwidth=N` flag, where `N` can be either 32 or 64.
* Fixes bugs:
  * Cannot reduce `case error ... of ...` to `error ...` [#109](https://github.com/clash-lang/clash-compiler/issues/109)

## 0.6.7 *December 21st 2015*
* Support for `unbound-generics-0.3`
* New features:
  * Only look for 'topEntity' in the root module. [#22](https://github.com/clash-lang/clash-compiler/issues/22)
* Fixes bugs:
  * Unhelpful error message when GHC is not in PATH [#104](https://github.com/clash-lang/clash-compiler/issues/104)

## 0.6.6 *December 11th 2015*
* New features:
  * Remove all existing HDL files before generating new ones. This can be disabled by the `-clash-noclean` flag. [#96](https://github.com/clash-lang/clash-compiler/issues/96)
  * Support for `clash-prelude` 0.10.4

## 0.6.5 *November 17th 2015*
* Fixes bugs:
  * Integer literals used as arguments not always properly annotated with their type.
  * Verilog: Name collision in verilog code [#93](https://github.com/clash-lang/clash-compiler/issues/93)
  * (System)Verilog: Integer literals missing "32'sd" prefix when used in assignments.
  * VHDL: Integer literals should only be capped to 32-bit when used in assignments.
  * Verilog: HO-primitives incorrect for nested vectors.

## 0.6.4 *November 12th 2015*
* Fixes bugs:
  * Reversing alternatives is not meaning preserving for literal patterns [#91](https://github.com/clash-lang/clash-compiler/issues/91)
  * DEC: root of the case-tree must contain at least 2 alternatives [#92](https://github.com/clash-lang/clash-compiler/issues/92)
  * Do not generate overlapping literal patterns in VHDL [#91](https://github.com/clash-lang/clash-compiler/issues/91)

## 0.6.3 *October 24th 2015*
* New features:
  * Improve DEC transformation: consider alternatives before the subject when checking for disjoint expressions.
* Fixes bugs:
  * DEC: don't generate single-branch case-expressions [#90](https://github.com/clash-lang/clash-compiler/issues/90)

## 0.6.2 *October 21st 2015*
* New features:
  * Support `clash-prelude` 0.10.2

* Fixes bugs:
  * CLaSH interpreter was reading '.ghci' file instead of '.clashi' file [#87](https://github.com/clash-lang/clash-compiler/issues/87)
  * DEC: Subject and alternatives are not disjoint [#88](https://github.com/clash-lang/clash-compiler/issues/88)

## 0.6.1 *October 16th 2015*
* New features:
  * Support for `clash-prelude` 0.10.1
  * Transformation that lifts applications of the same function out of alternatives of case-statements. e.g.

    ```haskell
    case x of
      A -> f 3 y
      B -> f x x
      C -> h x
    ```

    is transformed into:

    ```haskell
    let f_arg0 = case x of {A -> 3; B -> x}
        f_arg1 = case x of {A -> y; B -> x}
        f_out  = f f_arg0 f_arg1
    in  case x of
          A -> f_out
          B -> f_out
          C -> h x
    ```

* Fixes bugs:
  * clash won't run when not compiled with usual ghc [#82](https://github.com/clash-lang/clash-compiler/issues/82)
  * Fail to generate VHDL with blockRamFile' in clash-ghc 0.6 [#85](https://github.com/clash-lang/clash-compiler/issues/85)
  * Case-statements acting like normal decoder circuits are erroneously synthesised to priority decoder circuits.

## 0.6 *October 3rd 2015*
* New features:
  * Support `clash-prelude-0.10`
  * Pattern matching on `CLaSH.Sized.Vector`'s `:>` is now supported
  * Unroll "definitions" of the following primitives: `fold`, `dfold`, `foldr`

## 0.5.15 *September 21st 2015*
* New features:
  * Report simulation time in (System)Verilog assert messages

* Fixes bugs:
  * Performance bug: top-level definitions of type "Signal" erroneously inlined.
  * Fix Index maxBound [#79](https://github.com/clash-lang/clash-compiler/pull/79)

## 0.5.14 *September 14th 2015*
* New features:
  * Completely unroll "definitions" of some higher-order primitives with non-representable argument or result vectors:
    It is now possible to translate e.g. `f xs ys = zipWith ($) (map (+) xs) ys :: Vec 4 Int -> Vec 4 Int -> Vec 4 Int`

* Fixes bugs:
  * Converting Bool to Unsigned generates broken VHDL [#77](https://github.com/clash-lang/clash-compiler/issues/77)
  * `topLet` transformation erroneously not performed in a top-down traversal
  * Specialisation limit unchecked on types and constants
  * Vector of functions cannot be translated [#25](https://github.com/clash-lang/clash-compiler/issues/25 )
  * CLaSH fails to generate VHDL when map is applied [#78](https://github.com/clash-lang/clash-compiler/issues/78)

## 0.5.13 *September 8th 2015*
* Fixes bugs:
  * Cannot translate GHC `MachLabel` literal
  * Maybe (Index n) not translatable to VHDL [#75](https://github.com/clash-lang/clash-compiler/issues/75)

## 0.5.12 *September 7th 2015*
* New features:
  * Modest compilation time speed-up. Compilation time of the [I2C](https://github.com/clash-lang/clash-compiler/tree/master/examples/i2c) module on my machine went down from 43s to 24s, and maximum memory usage went down from 840 MB to 700 MB.

* Fixes bugs:
  * Bug in VHDL ROM generation [#69](https://github.com/clash-lang/clash-compiler/issues/69)
  * Clash running out of memory on Simple-ish project [#70](https://github.com/clash-lang/clash-compiler/issues/70)
  * Fix asyncRom VHDL primitive [#71](https://github.com/clash-lang/clash-compiler/pull/71)
  * Fix primitive for CLaSH.Sized.Internal.Signed.size# [#72](https://github.com/clash-lang/clash-compiler/pull/72)
  * rem and quot on Signed are broken [#73](https://github.com/clash-lang/clash-compiler/issues/73)

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
