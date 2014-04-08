{- | A little tutorial to
-}
module CLaSH.Tutorial (
  -- * Introduction
  -- $introduction

  -- * MAC Example
  -- $mac_example

  -- * Conclusion
  -- $conclusion

  -- * Haskell stuff that the CλaSH compiler does not support
  -- $unsupported
  )
where

{- $introduction
-}

{- $mac_example
-}

{- $conclusion
-}

{- $unsupported
Here is a list of Haskell features which the CλaSH compiler cannot synthesize
to VHDL (for now):

* __Recursive functions__:

  Although it seems rather bad that a compiler for a
  functional language does not support recursion, this bug/feature of the
  CλaSH compiler is amortized by the builtin knowledge of all the functions
  listed in "CLaSH.Sized.Vector". And as you saw in this tutorial, the
  higher-order functions of "CLaSH.Sized.Vector" can cope with many of the
  recursive design patterns found in circuit design.

  Also note that although recursive functions are not supported, recursively
  (tying-the-knot) defined values are supported (as long as these values do not
  have a function type). An example is the following function that performs
  one iteration of bubble sort:

  @
  sortVL xs = vmap fst sorted <: (snd (vlast sorted))
   where
     lefts  = vhead xs :> vmap snd (vinit sorted)
     rights = vtail xs
     sorted = vzipWith compareSwapL (lazyV lefts) rights
  @

  Where we can clearly see that 'lefts' and 'sorted' are defined in terms of
  each other.

* __GADT Pattern Matching__:
-}
