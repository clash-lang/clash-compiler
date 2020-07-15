{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Bit
  ( bitPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)

import Clash.Sized.Internal.BitVector

import Clash.GHC.PartialEval.Internal

bitPrims :: HashMap Text PrimImpl
bitPrims = HashMap.fromList
  [ ("Clash.Sized.Internal.BitVector.Bit", liftBinary Bit)
  , ("Clash.Sized.Internal.BitVector.high", liftBit high)
  , ("Clash.Sized.Internal.BitVector.low", liftBit low)
  , ("Clash.Sized.Internal.BitVector.eq##", liftBinary eq##)
  , ("Clash.Sized.Internal.BitVector.neq##", liftBinary neq##)
  , ("Clash.Sized.Internal.BitVector.lt##", liftBinary lt##)
  , ("Clash.Sized.Internal.BitVector.ge##", liftBinary ge##)
  , ("Clash.Sized.Internal.BitVector.gt##", liftBinary gt##)
  , ("Clash.Sized.Internal.BitVector.le##", liftBinary le##)
  , ("Clash.Sized.Internal.BitVector.fromInteger##", liftId)
  , ("Clash.Sized.Internal.BitVector.and##", liftBinary and##)
  , ("Clash.Sized.Internal.BitVector.or##", liftBinary or##)
  , ("Clash.Sized.Internal.BitVector.xor##", liftBinary xor##)
  , ("Clash.Sized.Internal.BitVector.complement##", liftUnary complement##)
  ]

liftBit :: Bit -> PrimImpl
liftBit b _e p args =
  resultType p args >>= toValue b

