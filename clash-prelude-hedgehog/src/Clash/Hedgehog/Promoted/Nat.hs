{-|
Copyright   : (C) 2026, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Random generation of SNat.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Clash.Hedgehog.Promoted.Nat where

import Clash.Prelude
import Hedgehog (Gen)
import qualified Data.String.Interpolate as I
import qualified GHC.TypeNats as TypeNats
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- | Like @SomeSNat@, but with bounds.
data SomeBoundedSNat lower upperInclusive where
  SomeBoundedSNat
    :: forall n lower upperInclusive
     . (lower <= n, n <= upperInclusive)
    => SNat n
    -> SomeBoundedSNat lower upperInclusive

instance Show (SomeBoundedSNat lower upperInclusive) where
  show (SomeBoundedSNat sn@SNat) = show ''SomeBoundedSNat <> " " <> show sn

{- | Generate a v'SomeBoundedSNat' between the given bounds (inclusive).
Uses 'Hedgehog.Range.linear' and shrinks to @lower@.

For example, to bring into scope the constraints @(KnownNat n, 1 <= n, n <= 8)@:

> SomeBoundedSNat (SNat :: SNat n) <- forAll (genSomeBoundedSNat @1 @8)
-}
genSomeBoundedSNat
  :: (KnownNat lower, KnownNat upperInclusive, lower <= upperInclusive)
  => Gen (SomeBoundedSNat lower upperInclusive)
genSomeBoundedSNat = genSomeBoundedSNat# (\l -> Gen.integral . Range.linear l)

-- | Like 'genSomeBoundedSNat' but does /not/ shrink.
genSomeBoundedSNat_
  :: (KnownNat lower, KnownNat upperInclusive, lower <= upperInclusive)
  => Gen (SomeBoundedSNat lower upperInclusive)
genSomeBoundedSNat_ = genSomeBoundedSNat# (\l -> Gen.integral_ . Range.linear l)

{- | Generate a v'SomeBoundedSNat' between the given bounds (inclusive).
To do so, uses a given term level generator.

__NB__: Make sure the given generator respects the bounds that it is passed at
the term level.
-}
genSomeBoundedSNat#
  :: forall lower upperInclusive
   . (KnownNat lower, KnownNat upperInclusive, lower <= upperInclusive)
  => (Natural -> Natural -> Gen Natural)
  -> Gen (SomeBoundedSNat lower upperInclusive)
genSomeBoundedSNat# gen = do
  n <- gen (natToNatural @lower) (natToNatural @upperInclusive)
  case TypeNats.someNatVal n of
    SomeNat p ->
      case (SNat @lower `compareSNat` sn, sn `compareSNat` SNat @upperInclusive) of
        (SNatLE, SNatLE) -> pure (SomeBoundedSNat sn)
        _ ->
          error
            [I.__i|
            genSomeBoundedSNat: internal error: generated number out of bounds

            lower bound:             #{natToNatural @lower}
            upper bound (inclusive): #{natToNatural @upperInclusive}
            generated number:        #{n}
            |]
       where
        sn = snatProxy p
