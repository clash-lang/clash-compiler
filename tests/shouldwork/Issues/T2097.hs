{-# LANGUAGE DeriveGeneric #-}

module T2097 where

import Clash.Prelude

topEntity :: MyFloat
topEntity = -1.0

data MyFloat = MyFloat {
  negative :: Bool,
  exponent :: Unsigned 8,
  mantissa :: UFixed 0 23
} deriving (Eq, Generic, BitPack)

instance Num MyFloat where
  (-) a b = bitCoerce @Float (bitCoerce a - bitCoerce b)
  fromInteger = bitCoerce @Float . fromInteger

instance Fractional MyFloat where
  fromRational = bitCoerce @Float . fromRational

