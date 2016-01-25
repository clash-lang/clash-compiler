# Changelog for the [`clash-systemverilog`](http://hackage.haskell.org/package/clash-systemverilog) package

## 0.6.5
* New features:
  * Support clash-lib-0.6.9
* Fixes bugs:
  * BlockRAM elements must be bit vectors [#113](https://github.com/clash-lang/clash-compiler/issues/113)

## 0.6.4 *January 13th 2015*
* New features:
  * Support for Haskell's: `Char`, `Int8`, `Int16`, `Int32`, `Int64`, `Word`, `Word8`, `Word16`, `Word32`, `Word64`.
  * Int/Word/Integer bitwidth for generated SystemVerilog is configurable using the `-clash-intwidth=N` flag, where `N` can be either 32 or 64.

## 0.6.3 *November 17th 2015*
* Fixes bugs:
  * Integer literals missing "32'sd" prefix when used in assignments

## 0.6.2 *October 21st 2015*
* New features:
  * Support `clash-prelude` 0.10.2

## 0.6.1 *October 16th 2015*
* New features:
  * Support for `clash-prelude` 0.10.1

## 0.6 *October 3rd 2015*
* New features:
  * Support `clash-prelude-0.10`

## 0.5.10 *September 21st 2015*
* New features:
  * Report simulation time in assert messages

## 0.5.9 *September 14 2015*
* Support for clash-lib-0.5.12

## 0.5.8 *September 7th 2015*
* Fixes bugs:
  * Fix primitive for CLaSH.Sized.Internal.Signed.size# [#72](https://github.com/clash-lang/clash-compiler/pull/72)
  * rem and quot on Signed are broken [#73](https://github.com/clash-lang/clash-compiler/issues/73)

## 0.5.7 *June 26th 2015*
* Fixes bug:
  * Incorrect primitive for `CLaSH.Prelude.Testbench.assert'`
  * Incorrect primitive for `CLaSH.Sized.Vec.index_int`
  * Sometimes created incorrect nested `generate` statements

## 0.5.6 *June 25th 2015*
* New features:
  * Support `clash-prelude-0.9`

* Fixes bug:
  * Can not operate "shiftR" on Int [#63](https://github.com/clash-lang/clash-compiler/issues/63)
  * Fail to generate verilog when using "quot" and "div" on Index [#64](https://github.com/clash-lang/clash-compiler/issues/64)

## 0.5.5 *June 3rd 2015*
* New features:
  * Compile against `clash-lib-0.5.6`
  * Generated component names are prefixed by the name of the module containing the `topEntity`

## 0.5.4 *May 10th 2015*
* New features:
  * Generate smarter labels for `register` and `blockRam` blackboxes to make finding longest paths easier

## 0.5.3 *May 5th 2015*
* Fixes bugs:
  * Incorrect implementation of rotateL and rotateR blackbox for BitVector

## 0.5.2 *May 1st 2015*
* New features:
  * Support wrapper generation

## 0.5.1 *April 20th 2015*
* Update to clash-prelude 0.7.2

## 0.5 *March 11th 2015*
* Initial release
