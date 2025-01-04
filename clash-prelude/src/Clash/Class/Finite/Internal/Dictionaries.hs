{-|
Copyright  :  (C) 2024-2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

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

-- | Evidence that exponentiation can never return a zero result.
powGTZero ::
  forall (n :: Nat) (m :: Nat).
  Dict (1 <= n^m)
powGTZero = unsafeCoerce (Dict :: Dict (0 <= 0))

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
powCLogDual ::
  forall (a :: Nat) (n :: Nat).
  Dict (CLog a (a^n) ~ n)
powCLogDual = unsafeCoerce (Dict :: Dict (0 ~ 0))

-- | Evidence that substraction and addition of the same nat cancels
-- each other in a greater or equal than one equation.
leqOnePlusMinus ::
  forall (a :: Nat) (b :: Nat).
  (a <= b, 1 <= b) => Dict (1 <= a + (b - a))
leqOnePlusMinus = unsafeCoerce (Dict :: Dict (0 <= 0))
#endif
