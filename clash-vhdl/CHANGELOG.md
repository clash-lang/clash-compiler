# Changelog for the [`clash-vhdl`](http://hackage.haskell.org/package/clash-vhdl) package

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
