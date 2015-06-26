# Changelog for the [`clash-systemverilog`](http://hackage.haskell.org/package/clash-systemverilog) package

## 0.5.7
* Fixes bug:
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
