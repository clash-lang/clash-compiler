{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeFamilies     #-}

{-# LANGUAGE Trustworthy      #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Class.Resize where

import GHC.Exts                 (Constraint)
import GHC.TypeLits             (Nat, type (+))
import Data.Singletons.Prelude  (TyFun,type (@@))

-- | Coerce a value to be represented by a different number of bits
class Resize (f :: Nat -> *) where
  -- | Constraint on 'resize'
  type ResizeC f :: TyFun Nat (TyFun Nat Constraint -> *) -> *
  -- | A sign-preserving resize operation
  --
  -- * For signed datatypes: Increasing the size of the number replicates the
  -- sign bit to the left. Truncating a number to length L keeps the sign bit
  -- and the rightmost L-1 bits.
  --
  -- * For unsigned datatypes: Increasing the size of the number extends with
  -- zeros to the left. Truncating a number of length N to a length L just
  -- removes the left (most significant) N-L bits.
  resize :: ResizeC f @@ a @@ b => f a -> f b
  -- | Constraint on 'extend', 'zeroExtend', and 'signExtend'
  type ExtendC f :: TyFun Nat (TyFun Nat Constraint -> *) -> *
  -- | Perform a 'zeroExtend' for unsigned datatypes, and 'signExtend' for a
  -- signed datatypes
  extend :: ExtendC f @@ a @@ b => f a -> f (b + a)
  -- | Add extra zero bits in front of the MSB
  zeroExtend :: ExtendC f @@ a @@ b => f a -> f (b + a)
  -- | Add extra sign bits in front of the MSB
  signExtend :: ExtendC f @@ a @@ b => f a -> f (b + a)
  -- | Constraint on 'truncateB'
  type TruncateC f :: TyFun Nat (TyFun Nat Constraint -> *) -> *
  -- | Remove bits from the MSB
  truncateB :: TruncateC f @@ a @@ b => f (a + b) -> f a
