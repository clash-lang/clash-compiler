{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module CLaSH.Sized.Fixed
  ( -- * Signed fixed point
    SFixed, sf, unSF
    -- * Unsigned fixed point
  , UFixed, uf, unUF
  )
where

import Control.Arrow
import Data.Bits
import Data.List
import Data.Maybe
import Data.Ratio
import GHC.TypeLits
import Data.Function

import CLaSH.Class.BitVector
import CLaSH.Class.Num
import CLaSH.Signal
import CLaSH.Sized.Signed
import CLaSH.Sized.Unsigned

-- | Fixed point signed integer with @i@ integer bits and @f@ fractional bits
--
-- For now, overflow behaviour for the 'Num' functions is wrap-around, not saturate
newtype SFixed (i :: Nat) (f :: Nat) = SF { unSF :: Signed   (i + f) }
  deriving (Eq,Ord)

-- | Fixed point unsigned integer with @i@ integer bits and @f@ fractional bits
--
-- For now, overflow behaviour for the 'Num' functions is wrap-around, not saturate
newtype UFixed (i :: Nat) (f :: Nat) = UF { unUF :: Unsigned (i + f) }
  deriving (Eq,Ord)

fracShift :: KnownNat n => proxy n -> Int
fracShift = fromInteger . natVal

sf :: Signed (i + f) -> SFixed i f
sf = SF

uf :: Unsigned (i + f) -> UFixed i f
uf = UF

showFixed :: (Bits a, KnownNat n, Show a, Integral a) =>
             (proxy n -> a) -> proxy n -> [Char]
showFixed unRep f = show i ++ "." ++ (uncurry pad . second (show . numerator) .
                                      fromJust . find ((==1) . denominator . snd) .
                                      iterate (succ *** (*10)) . (,) 0 $ (nom % denom))
    where
      pad n str = replicate (n - length str) '0' ++ str
      nF        = fracShift f
      rep       = unRep f
      i         = rep `shiftR` nF
      nom       = toInteger rep .&. ((2 ^ nF) - 1)
      denom     = 2 ^ nF

instance (KnownNat (i + f), KnownNat f) => Show (SFixed i f) where
  show = showFixed unSF

instance (KnownNat (i + f), KnownNat f) => Show (UFixed i f) where
  show = showFixed unUF

multFixedS :: (KnownNat ((i + f) + (i + f)), KnownNat (i + f), KnownNat f)
           => SFixed i f -> SFixed i f -> SFixed i f
multFixedS (SF a) (SF b) = res
  where
    resM = mult a b
    resS = resM `shiftR` (fracShift res)
    res  = SF (resizeS resS)

multFixedU :: (KnownNat ((i + f) + (i + f)), KnownNat (i + f), KnownNat f)
           => UFixed i f -> UFixed i f -> UFixed i f
multFixedU (UF a) (UF b) = res
  where
    resM = mult a b
    resS = resM `shiftR` (fracShift res)
    res  = UF (resizeU resS)

fixedFromInteger :: (Bits a, KnownNat n, Num a)
                 => (a -> proxy n) -> Integer -> proxy n
fixedFromInteger toF i = res
  where
    res = toF (fromInteger i `shiftL` fracShift res)

instance (KnownNat ((i + f) + (i + f)), KnownNat (i + f), KnownNat f) => Num (SFixed i f) where
  (+)           = (SF .) . on (+) unSF
  (*)           = multFixedS
  (-)           = (SF .) . on (-) unSF
  negate        = SF . negate . unSF
  abs           = SF . abs . unSF
  signum        = SF . signum . unSF
  fromInteger   = fixedFromInteger SF

instance (KnownNat ((i + f) + (i + f)), KnownNat (i + f), KnownNat f) => Num (UFixed i f) where
  (+)           = (UF .) . on (+) unUF
  (*)           = multFixedU
  (-)           = (UF .) . on (-) unUF
  negate        = UF . negate . unUF
  abs           = UF . abs . unUF
  signum        = UF . signum . unUF
  fromInteger   = fixedFromInteger UF

instance BitVector (SFixed i f) where
  type BitSize (SFixed i f) = i + f
  toBV   (SF s) = toBV s
  fromBV bv     = SF (fromBV bv)

instance BitVector (UFixed i f) where
  type BitSize (UFixed i f) = i + f
  toBV   (UF s) = toBV s
  fromBV bv     = UF (fromBV bv)

instance Pack (SFixed i f) where
  type SignalP (SFixed i f) = Signal (SFixed i f)
  pack   = id
  unpack = id

instance Pack (UFixed i f) where
  type SignalP (UFixed i f) = Signal (UFixed i f)
  pack   = id
  unpack = id
