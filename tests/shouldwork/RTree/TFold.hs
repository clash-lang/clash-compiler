{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}
module TFold where

import Clash.Prelude
import GHC.TypeLits.Extra
import Data.Kind (Type)

import Data.Proxy
import Data.Singletons hiding (type (+))

data IIndex (f :: TyFun Nat Type) :: Type
type instance Apply IIndex l = Index ((2^l)+1)

popCountT = tdfold (Proxy :: Proxy IIndex) fromIntegral (const add)

popCount = popCountT . v2t . bv2v

topEntity :: BitVector 16 -> Index 17
topEntity = TFold.popCount
