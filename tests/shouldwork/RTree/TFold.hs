{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}
module TFold where

import CLaSH.Prelude
import GHC.TypeLits.Extra

import Data.Proxy
import Data.Singletons.Prelude

data IIndex (f :: TyFun Nat *) :: *
type instance Apply IIndex l = Index ((2^l)+1)

popCountT = tdfold (Proxy :: Proxy IIndex) fromIntegral (const plus)

popCount = popCountT . v2t . bv2v

topEntity :: BitVector 16 -> Index 17
topEntity = TFold.popCount
