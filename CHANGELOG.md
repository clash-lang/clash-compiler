# Changelog for [`clash-prelude` package](http://hackage.haskell.org/package/clash-prelude)

## 0.10.6 *February 10th 2016*
* Fixes bugs:
  * `CLaSH.Prelude.DataFlow.parNDF` is not lazy enough

## 0.10.5 *January 13th 2016*
* New features:
  * Add `readNew` to `CLaSH.Prelude.BlockRam`: create a read-after-write blockRAM from a read-before-write blockRAM.
  * `popCount` functions for `BitVector`, `Signed`, and `Unsigned` are now synthesisable.
  * Add `parNDF` to `CLaSH.Prelude.DataFlow`: compose _N_ dataflow circuits in parallel.
  * Add and instance `Vec n a` for `LockStep` in `CLaSH.Prelude.DataFlow`: have _N_ dataflow circuits operate in lock-step.

## 0.10.4 *December 11th 2015*
* New features:
  * Add `pureDF` to `CLaSH.Prelude.DataFlow`: lift combinational circuits to `DataFlow` circuits.
  * Add `fifoDF` to `CLaSH.Prelude.DataFlow`: a simple FIFO buffer adhering to the `DataFlow` protocol.
  * `loopDF` no longer uses the `lockStep` and `stepLock` automatically, and now includes a FIFO buffer on the feedback path.
  * Add `loopDF_nobuf` to `CLaSH.Prelude.DataFlow`: a version of `loopDF` with no FIFO buffer on the feedback path.
  * Add `boolToBV` to `CLaSH.CLass.BitPack`: convert `Bool`eans to `n`-bit `BitVector`s.
  * `ClockSource` in `CLaSH.Annotations.TopEntity` can now have multiple clock inputs [#33](https://github.com/clash-lang/clash-prelude/issues/33)
* Bug fixes:
  * `asyncRomFile` reads file multiple times.

## 0.10.3 *October 24th 2015*
* Disable CPR analysis (See https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c):
  * GHC < 7.11: In all modules using `-fcpr-off`
  * GHC >= 7.11: In `CLaSH.Signal.Internal` and `CLaSH.Prelude.RAM` using `-fno-cpr-anal`

## 0.10.2 *October 21st 2015*
* New features:
  * `ExtendingNum`, `BitPack`, and `Resize` instance for `Index`
  * Add `bv2i`: convert `BitVector n` to `Index (2^n)`
  * Export type-level operations from [ghc-typelits-extra](http://hackage.haskell.org/package/ghc-typelits-extra)

## 0.10.1 *October 16th 2015*
* New features:
  * The `f` in `dfold p f`, now has an `SNat l` instead of a `Proxy l` as its first argument.
  * Add `bv2v` and `v2bv` functions that convert between `Vec n Bit` and `BitVector n`.
  * Add `smap`: apply a function to every element of a vector and the element's position (as an 'SNat' value) in the vector.

## 0.10 *October 3rd 2015*
* New features:
  * The Vec constructor `:>` is now an explicitly bidirectional pattern synonym (the actual constructor has been renamed to Cons).
    As a result, pattern matching on `:>` is now synthesisable by the CLaSH compiler.
  * The function `<:` is replaced by the the explicitly bidirectional pattern synonym `:<`.
    This allows you to pattern match on: "all but the last element of a vector" and "the last element" of the vector.
    Because it is a bidirectional pattern, `:<` can also be used as an expression that appends an element to the tail of a vector.
  * Add a `transpose` function in `CLaSH.Sized.Vector`.
  * Add `stencil1d` and `stensil2d` stencil computation functions in `CLaSH.Sized.Vector`.
  * Add `permute`, `backpermute`, `scatter`, and `gather` permutation functions in `CLaSH.Sized.Vector`.
  * Add specialised permutation functions `interleave`, `rotateLeft`, and `rotateRight` in `CLaSH.Sized.Vector`.
  * `sscanl` and `sscanr` in `CLaSH.Sized.Vector` are renamed to `postscanl` and postscanr` to be more in line with existing Haskell packages such as `vector` and `accelerate`.
  * The `Foldable` and `Traversable` instances of `Vec` now only works for non-empty vectors.
  * Where possible, members of the `Foldable` instance of `Vec` are described in terms of `fold`, which builds a tree (of depth `log(n)`) of computations, instead of `foldr` which had depth `n` computations.
    This reduces the critical path length in your circuits when using these functions.
  * `maxIndex` and `length` in `CLaSH.Sized.Vector` return an `Int` instead of an `Integer`.
  * Add functions that involve an index into a vector to the `CLaSH.Sized.Vector` module: `indices`, `indicesI`, `imap`, `izipWith`, `ifoldr`, `ifoldl`, `findIndex`, `elemIndex`.
  * `CLaSH.Sized.Vector`'s `fold`, `dfold`, `vfold`, and `toList` are now synthesisable by the CLaSH compiler.

## 0.9.3 *September 21st 2015*
* Fixes bugs:
  * Cannot build against singletons-0.2
  * Numerous documentation fixes

## 0.9.2 *August 2nd 2015*
* Disable strictness analysis in `CLaSH.Signal.Internal`, this allows turning on strictness analysis in the GHC front-end of the CLaSH compiler.

## 0.9.1 *June 26th 2015*
* Updated documentation on data-file support on Altera/Quartus

## 0.9 *June 25th 2015*
* New features:
  * Add operations on singleton natural numbers: `addSNat`, `subSNat`, `mulSNat`, and `powSNat`.
  * Add asynchronous RAM functions in `CLaSH.Prelude.RAM`, which have an asynchronous/combinational read port.
  * Add ROM functions in modules `CLaSH.Prelude.ROM` and `CLaSH.Prelude.ROM.File`, where the latter module contains functions that instantiate a ROM from the content specified in an external data-file.
  * Add BlockRam functions, in the `CLaSH.Prelude.BlockRam.File` module, whose content can be initialised with the content specified in an external data-file.
  * `assert` now takes an extra `String` argument so you can distinguish one `assert` from the others.
  Additionally, `assert'` is added which takes an additional `SClock` argument.
  This is needed, because `assert` now reports the clock cycle, and clock domain, when an assertion fails.
  * `defClkAltera` and `defClkXilinx` are replaced by, `altpll` and `alteraPll` for Altera clock sources, and `clockWizard` for Xilinx clock sources.
  These names correspond to the names of the generator utilities in Quartus and ISE/Vivado.
  * Add [Safe](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe-haskell.html) versions of the prelude modules: `CLaSH.Prelude.Safe` and `CLaSH.Prelude.Explicit.Safe`
  * Add synchronizers in the `CLaSH.Prelude.Synchronizer` module

## 0.8 *June 3rd 2015*
* New features:
  * Make the (Bit)Vector argument the _last_ argument for the following functions: `slice`, `setSlice`, `replaceBit`, `replace`. The signatures for the above functions are now:

    ```
    slice      :: BitPack a => SNat m -> SNat n -> a -> BitVector (m + 1 - n)
    setSlice   :: BitPack a => SNat m -> SNat n -> BitVector (m + 1 - n) -> a -> a
    replaceBit :: Integral i => i -> Bit -> a -> a
    replace    :: Integral i => i -> a -> Vec n a -> Vec n a
    ```

    This allows for easier chaining, e.g.:

    ```
    replaceBit 0 1 $
    repleceBit 4 0 $
    replaceBit 6 1 bv
    ```
  * Until version 0.7.5, given `x :: Vec 8 Bit` and `y :: BitVector 8`, it used to be `last x == msb y`.
    This is quite confusing when printing converted values.
    Until version 0.7.5 we would get:

    ```
    > 0x0F :: BitVector 8
    0000_1111
    > unpack 0x0F :: Vec 8 Bit
    <1,1,1,1,0,0,0,0>
    ```

    As of version 0.8, we have `head x == msb y`:

    ```
    > 0x0F :: BitVector 8
    0000_1111
    > unpack 0x0F :: Vec 8 Bit
    <0,0,0,0,1,1,1,1>
    ```

    So converting for `Vec`tors of `Bit`s to `BitVector`s is no longer _index_-preserving, but it is _order_-preserving.
  * Add [QuickCheck](http://hackage.haskell.org/package/QuickCheck) `Arbitary` and `CoArbitary` instances for all data types
  * Add [lens](http://hackage.haskell.org/package/lens) `Ixed` instances for `BitVector`, `Signed`, `Unsigned`, and `Vec`

## 0.7.5 **May 7th 2015**
* New features:
  * Moore machine combinators

## 0.7.4 **May 5th 2015*
* New features:
  * Add `TopEntity` annotations

## 0.7.3 *April 22nd 2015*
* New features:
  * Add the vector functions: `zip3`, `unzip3`, and `zipWith3`
  * Use version 0.2 of the [`ghc-typelits-natnormalise` package](http://hackage.haskell.org/package/ghc-typelits-natnormalise)

## 0.7.2 *April 20th 2015*
* New features:
  * Support for GHC 7.10 => only works with GHC 7.10 and higher
  * Use http://hackage.haskell.org/package/ghc-typelits-natnormalise typechecker plugin for better type-level natural number handling

## 0.7.1 *March 25th 2015*
* Fixes bugs:
  * Fix laziness bug in Vector.(!!) and Vector.replace

## 0.7 *March 13th 2015*
* New features:
  * Switch types of `bundle` and `bundle'`, and `unbundle` and `unbundle'`.
  * Rename all explicitly clocked versions of Signal functions, to the primed
    name of the implicitly clocked Signal functions. E.g. `cregister` is now
    called `register'` (where the implicitly clocked function is callled `register`)
  * Add new instances for `DSignal`
  * Add experimental `antiDelay` function for `DSignal`
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
