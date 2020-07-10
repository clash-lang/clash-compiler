{-# LANGUAGE CPP, ScopedTypeVariables, UndecidableInstances #-}
module TFold where

import Clash.Prelude
import GHC.TypeLits.Extra
import Data.Kind (Type)

import Data.Proxy
#if MIN_VERSION_singletons(2,4,0)
import Data.Singletons.Prelude hiding (type (+))
#else
import Data.Singletons.Prelude
#endif

data IIndex (f :: TyFun Nat Type) :: Type
type instance Apply IIndex l = SatIndex 'SatError ((2^l)+1)

popCountT = tdfold (Proxy :: Proxy IIndex) fromIntegral (const add)

popCount = popCountT . v2t . bv2v

topEntity :: BitVector 16 -> SatIndex 'SatError 17
topEntity = TFold.popCount
