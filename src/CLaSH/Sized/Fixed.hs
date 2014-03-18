{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module CLaSH.Sized.Fixed where

import Data.Bits
import CLaSH.Promoted.Nat
import CLaSH.Sized.Signed
import GHC.TypeLits
import Data.Function

newtype SFixed (s :: Nat) (f :: Nat) = SF { unSF :: Signed s }
  deriving Eq

instance (KnownNat s, KnownNat f) => Show (SFixed s f) where
  show (SF sf) = show dec ++ (tail (show frac))
    where
      nF   = fromInteger $ fromSNat (snat :: SNat f)
      dec  = sf `shiftR` nF
      frac = on (/) fromIntegral (toInteger sf .&. ((2 ^ nF) - 1)) (2 ^ nF)

instance (KnownNat f, KnownNat s) => Num (SFixed s f) where
  (+)         = (SF .) . on (+) unSF
  (*)         = ((SF . (`shiftR` nF)) .) . on (*) unSF
    where
      nF  = fromInteger $ fromSNat (snat :: SNat f)
  (-)         = (SF .) . on (-) unSF
  negate      = SF . negate . unSF
  abs         = SF . abs . unSF
  signum      = SF . signum . unSF
  fromInteger i = SF (fromInteger i `shiftL` nF)
    where
      nF  = fromInteger $ fromSNat (snat :: SNat f)
