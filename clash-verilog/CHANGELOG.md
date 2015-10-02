# Changelog for the [`clash-systemverilog`](http://hackage.haskell.org/package/clash-systemverilog) package

## 0.6
* New features:
  * Support `clash-prelude-0.10`

## 0.5.10 *September 21st 2015*
* New features:
  * Report simulation time in assert messages

## 0.5.9 *September 14th 2015*
* Support for clash-lib-0.5.12

## 0.5.8 *September 7th 2015*
* Fixes bugs:
  * Fix primitive for CLaSH.Sized.Internal.Signed.size# [#72](https://github.com/clash-lang/clash-compiler/pull/72)
  * rem and quot on Signed are broken [#73](https://github.com/clash-lang/clash-compiler/issues/73)

## 0.5.7 *June 26th 2015*
* New features:
  * Generate Verilog-2001 instead of Verilog-2005: generated Verilog is now accepted by Altera/Quartus

## 0.5.6 *June 25th 2015*
* New features:
  * Support `clash-prelude-0.9`

* Fixes bug:
  * Can not operate "shiftR" on Int [#63](https://github.com/clash-lang/clash-compiler/issues/63)
  * Fail to generate verilog when using "quot" and "div" on Index [#64](https://github.com/clash-lang/clash-compiler/issues/64)

## 0.5.5 *June 3rd 2015*
* Initial release
