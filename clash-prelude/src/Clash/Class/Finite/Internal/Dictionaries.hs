{-|
Copyright  :  (C) 2024-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Class.Finite.Internal.Dictionaries where

import Data.Constraint (Dict(..))
import GHC.TypeNats
  ( Nat, type (^), type (<=), type (*), type (+)
#if !MIN_VERSION_base(4,16,0)
  , type (-)
#endif
  )
#if !MIN_VERSION_base(4,16,0)
import GHC.TypeLits.Extra (CLog)
#endif
import Unsafe.Coerce (unsafeCoerce)

-- | Evidence that exponentiation can never return a zero result,
-- except the base is zero.
powPositiveIfPositiveBase ::
  forall (n :: Nat) (m :: Nat).
  1 <= n => Dict (1 <= n^m)
powPositiveIfPositiveBase = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | Evidence that exponentiation not returning a zero result is
-- a proof of the base being greater than zero.
powPositiveImpliesPositiveBase ::
  forall (n :: Nat) (m :: Nat).
  1 <= n^m => Dict (1 <= n)
powPositiveImpliesPositiveBase = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | Evidence that any multiplicaton resulting in a positive number
-- must have two positive operands.
mulPositiveImpliesPositiveOperands ::
  forall (n :: Nat) (m :: Nat).
  1 <= n * m => Dict (1 <= n, 1 <= m)
mulPositiveImpliesPositiveOperands =
  unsafeCoerce (Dict :: Dict (0 <= 0, 0 <= 0))

-- | Evidence that zero is the only natural number that is less or
-- equal than zero, also in the scope of addition.
zeroLeAdd ::
  forall (n :: Nat) (m :: Nat).
  n + m <= m => Dict (n ~ 0)
zeroLeAdd = unsafeCoerce (Dict :: Dict (0 ~ 0))

-- | Evidence that exponentiation with a fixed exponent perserves
-- monotonicity.
powMonotone1 ::
  forall (a :: Nat) (b :: Nat) (n :: Nat).
  a <= b => Dict (a^n <= b^n)
powMonotone1 = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | Evidence that we can use the exponentiation laws to rewrite the
-- term as stated below.
powLawsRewrite ::
  forall (a :: Nat) (n :: Nat).
  Dict ((a^(2^(n + 1))) ~ ((a^(2^n)) * (a^(2^n))))
powLawsRewrite = unsafeCoerce (Dict :: Dict (0 ~ 0))

#if !MIN_VERSION_base(4,16,0)
-- | Evidence that exponentiation and clog are dual to each other.
pow2CLogDual ::
  forall (n :: Nat).
  Dict (CLog 2 (2^n) ~ n)
pow2CLogDual = unsafeCoerce (Dict :: Dict (0 ~ 0))

-- | Evidence that substraction and addition of the same nat cancels
-- each other in a greater or equal than one equation.
leqOnePlusMinus ::
  forall (a :: Nat) (b :: Nat).
  (a <= b, 1 <= b) => Dict (1 <= a + (b - a))
leqOnePlusMinus = unsafeCoerce (Dict :: Dict (0 <= 0))
#endif
