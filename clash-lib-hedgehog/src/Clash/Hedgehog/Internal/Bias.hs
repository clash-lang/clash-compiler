{-|
Copyright   : (C) 2021, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V. <devops@qbaylogic.com>

Bias for influencing generator choice.
-}

{-# LANGUAGE CPP #-}

module Clash.Hedgehog.Internal.Bias
  ( Bias(..)
  ) where

import Clash.Core.TyCon
import Clash.Core.TysPrim

-- | Determine the bias of an item. This is used to set the weight of that item
-- so we can sample using the 'Hedgehog.Gen.frequency' generator instead of
-- 'Hedgehog.Gen.element' or 'Hedgehog.Gen.choice'.
--
-- Where might you want to introduce such a bias? If there is a collection of
-- elements where there is a likeliness that real code would use certain values
-- more or less, we want to be able to capture this. An obvious example of this
-- is the @TyConMap@, where without it every constructor would have an even
-- weighting, when in reality some (like @Void#@ or @Addr#@ are much less
-- likely to appear in code written by a Clash user).
--
class Bias a where
  biasOf :: a -> Int

-- Remember, the bias we pick here does not necessarily matter. Only
-- constructors with the correct shape will ever be considered.
--
-- TODO These biases are only very loosely based in reality, and could be
-- completely useless at generating the kinds / types we want to see.
instance Bias TyCon where
  biasOf tc@PrimTyCon{}
    | nm == liftedTypeKindTyConName  = biasBy 3  -- Type
    | nm == typeNatKindTyConName     = biasBy 2  -- Nat
    | nm == typeSymbolKindTyConName  = biasBy 1  -- Symbol

    | nm == integerPrimTyConName     = biasBy 5  -- Integer, Natural, Int#, Word#
    | nm == naturalPrimTyConName     = biasBy 5
    | nm == intPrimTyConName         = biasBy 5
    | nm == wordPrimTyConName        = biasBy 5
    | nm == int64PrimTyConName       = biasBy 4  -- Int64#, Word64#
    | nm == word64PrimTyConName      = biasBy 4
    | nm == floatPrimTyConName       = biasBy 3  -- Float#, Double#
    | nm == doublePrimTyConName      = biasBy 3
    | nm == charPrimTyConName        = biasBy 2  -- Char#, ByteArray#, Addr#
    | nm == byteArrayPrimTyConName   = biasBy 2
    | nm == stringPrimTyConName      = biasBy 2

    | otherwise                      = baseBias  -- Anything else is base
   where
    baseBias = 10
    nm       = tyConName tc

    biasBy :: Int -> Int
    biasBy n = baseBias ^ n

  biasOf AlgTyCon{}         = 20 ^ (4 :: Int)
  biasOf PromotedDataCon{}  = 20 ^ (3 :: Int)
  biasOf FunTyCon{}         = 20 ^ (3 :: Int)
