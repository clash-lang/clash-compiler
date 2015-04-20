# Changelog for the [`clash-ghc`](http://hackage.haskell.org/package/clash-ghc) package

## 0.5.1 *April 20th 2015*
* New features
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
