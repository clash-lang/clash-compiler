# Changelog for [`clash-prelude` package](http://hackage.haskell.org/package/clash-prelude)

## 0.7.1 *March 25th 2015*
* Fixes bugs:
  * Fix laziness bug in Vector.(!!) and Vector.replace

## 0.7 *March 13th 2015*
* New features:
  * Switch types of 'bundle' and 'bundle'', and 'unbundle' and 'unbundle''.
  * Rename all explicitly clocked versions of Signal functions, to the primed
    name of the implicitly clocked Signal functions. E.g. 'cregister' is now
    called 'register'' (where the implicitly clocked function is callled 'register')
  * Add new instances for 'DSignal'
  * Add experimental 'antiDelay' function for 'DSignal'
  * Generalize lifted functions over Signals (e.g. (.==.))

* Fixes bugs:
  * Faster versions of Vector.(!!) and Vector.replace

## 0.6.0.1 *November 17th 2014*
* Fixes bugs:
  * Add missing 'CLaSH.Sized.BitVector' module to .cabal file.

## 0.6 *November 17th 2014*

* New features:
  * Add `Fractional` instance for `Fixed` [#9](https://github.com/christiaanb/clash-prelude/issues/9)
  * Make indexing/subscript of `Vec` ascending [#4](https://github.com/christiaanb/clash-prelude/issues/4)
  * Add separate `BitVector` type, which has a descending index.
  * Add bit indexing operators, including the index/subscript operator `(!)`.
  * Add bit reduction operators: `reduceOr`, `reduceAnd`, `reduceOr`.
  * Rename `BitVector` class to `BitPack` with `pack` and `unpack` class methods.
  * Rename `Pack` class to `Bundle` with `bundle` and `unbundle` class methods.
  * Strip all `Vec` functions from their `v` prefix, i.e. `vmap` -> `map`.
  * Rename `Vec` indexing operator from `(!)` to `(!!)`.
  * Combine `Add` and `Mult` class into `ExtendingNum` class.
  * Add extend and truncate methods to the `Resize` class.
  * Add `SaturatingNum` class with saturating numeric operators.
  * Add multitude of lifted `Signal` operators, i.e. `(.==.) :: Eq a => Signal a -> Signal a -> Signal Bool`
  * Add `CLaSH.Signal.Delayed` with functions and data types for delay-annotated signals to support safe synchronisation.
  * Add `CLASH.Prelude.DataFlow` with functions and data types to create self-synchronising circuits based on data-flow principles.

* Fixes bugs:
  * Remove deprecated 'Arrow' instance for and related functions for `Comp` [#5](https://github.com/christiaanb/clash-prelude/issues/5)

## 0.5.1 *June 5th 2014*

* New features:
  * Add `Default` instance for `Vec` [#2](https://github.com/christiaanb/clash-prelude/issues/2)
  * Instantiation for `blockRam` [#3](https://github.com/christiaanb/clash-prelude/issues/2)

* Fixes bugs:
  * Fixed error on documentation of fLit in Fixed.hs [#6](https://github.com/christiaanb/clash-prelude/issues/6)
  * Non-translatable `Enum` function interfere with `sassert` compilation [#7](https://github.com/christiaanb/clash-prelude/issues/7)
  * Substituted the word 'list' into 'vector' in some places in the documentation. [#8](https://github.com/christiaanb/clash-prelude/issues/8)
  * mark vselectI INLINEABLE [#10](https://github.com/christiaanb/clash-prelude/issues/10)

## 0.5 *April 3rd 2014*
  * Add explicitly clocked synchronous signals for multi-clock circuits

## 0.4.1 *March 27th 2014*
  * Add saturation to fixed-point operators
  * Finalize most documentation

## 0.4 *March 20th 2014*
  * Add fixed-point integers
  * Extend documentation
  * 'bit' and 'testBit' functions give run-time errors on out-of-bound positions

## 0.3 *March 14th 2014*
  * Add Documentation
  * Easy SNat literals for 0..1024, e.g. d4 = snat :: SNat 4
  * Fix blockRamPow2

## 0.2 *March 5th 2014*
  * Initial release
