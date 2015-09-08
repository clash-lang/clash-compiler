# Changelog for the [`clash-vhdl`](http://hackage.haskell.org/package/clash-vhdl) package

## 0.5.10 *September 8th 2015*
* Fixes bugs:
  * Maybe (Index n) not translatable to VHDL [#75](https://github.com/clash-lang/clash-compiler/issues/75)

## 0.5.9 *September 7th 2015*
* Fixes bugs:
  * Bug in VHDL ROM generation [#69](https://github.com/clash-lang/clash-compiler/issues/69)
  * Fix asyncRom VHDL primitive [#71](https://github.com/clash-lang/clash-compiler/pull/71)
  * Fix primitive for CLaSH.Sized.Internal.Signed.size# [#72](https://github.com/clash-lang/clash-compiler/pull/72)
  * rem and quot on Signed are broken [#73](https://github.com/clash-lang/clash-compiler/issues/73)

## 0.5.8 *July 9th 2015*
* New features:
  * Generate VHDL-93 instead of VHDL-2002, the VHDL-93 standard is supported by a larger range of tools

## 0.5.7.1 *June 26th 2015*
* Support for `genStmt` backend method

## 0.5.7 *June 25th 2015*
* New features:
  * Support `clash-prelude-0.9`

* Fixes bug:
  * Can not operate "shiftR" on Int [#63](https://github.com/clash-lang/clash-compiler/issues/63)
  * Fail to generate verilog when using "quot" and "div" on Index [#64](https://github.com/clash-lang/clash-compiler/issues/64)

## 0.5.6 *June 5th 2015*
* Fixes bugs:
  * Incorrect extraction of `Bool` value out of a Sum-of-Product type

## 0.5.5 *June 3rd 2015*
* New features:
  * Compile against `clash-lib-0.5.6`
  * Generated component names are prefixed by the name of the module containing the `topEntity`

## 0.5.4 *May 10th 2015*
* New features:
  * Make VHDL 'assert' primitive GHDL friendly
  * Generate smarter labels for `register` and `blockRam` blackboxes to make finding longest paths easier

* Fixes bugs:
  * Incorrect primitives for BitVector `minBound` and `maxBound`

## 0.5.3 *May 1st 2015*
* New features:
  * Support wrapper generation

* Fixes bugs:
  * Incorrect primitives for BitVector `minBound` and `maxBound`

## 0.5.2 *April 24th 2015*
* Fixes bugs:
  * Fix bug where not enough array type definitions were created

## 0.5.1 *April 20th 2015*
* Update to clash-prelude 0.7.2

## 0.5 *March 11th 2015*
* Initial release
