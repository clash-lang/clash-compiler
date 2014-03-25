{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module CLaSH.Sized.Fixed
  ( -- * Signed fixed point
    SFixed, sf, unSF
    -- * Unsigned fixed point
  , UFixed, uf, unUF
    -- * Fixed point wrapper
  , Fixed (..), fracShift, resizeF
    -- * Proxy
  , asFracProxy, asRepProxy
  )
where

import Control.Arrow
import Data.Bits
import Data.Default
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Ratio
import Data.Typeable
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Syntax(Lift(..))

import CLaSH.Bit
import CLaSH.Class.BitVector
import CLaSH.Class.Num
import CLaSH.Promoted.Nat
import CLaSH.Promoted.Ord
import CLaSH.Signal
import CLaSH.Sized.Signed
import CLaSH.Sized.Unsigned
import CLaSH.Sized.Vector

newtype Fixed (frac :: Nat) (rep :: Nat -> *) (size :: Nat) = Fixed { unFixed :: rep size }
  deriving (Eq,Ord)

type SFixed int frac = Fixed frac Signed   (int + frac)
type UFixed int frac = Fixed frac Unsigned (int + frac)

sf :: SNat frac -> Signed (int + frac) -> SFixed int frac
sf _ fRep = Fixed fRep

unSF :: SFixed int frac -> Signed (int + frac)
unSF (Fixed fRep) = fRep

uf :: SNat frac -> Unsigned (int + frac) -> UFixed int frac
uf _ fRep = Fixed fRep

unUF :: UFixed int frac -> Unsigned (int + frac)
unUF (Fixed fRep) = fRep

asFracProxy :: Fixed frac rep size -> Proxy frac
asFracProxy _ = Proxy

asRepProxy :: Fixed frac rep size -> Proxy rep
asRepProxy _ = Proxy

fracShift :: KnownNat frac => Fixed frac rep size -> Int
fracShift f = fromInteger (natVal (asFracProxy f))

instance ( Show (rep size), Bits (rep size), KnownNat frac
         , Integral (rep size)
         ) => Show (Fixed frac rep size) where
  show f@(Fixed fRep) = i ++ "." ++ (uncurry pad . second (show . numerator) .
                                          fromJust . find ((==1) . denominator . snd) .
                                          iterate (succ *** (*10)) . (,) 0 $ (nom % denom))
    where
      pad n str = replicate (n - length str) '0' ++ str

      nF        = fracShift f
      fRepI     = toInteger fRep
      fRepI_abs = abs fRepI
      i         = if fRepI < 0 then '-' : show (fRepI_abs `shiftR` nF)
                               else show (fRepI `shiftR` nF)
      nom       = if fRepI < 0 then fRepI_abs .&. ((2 ^ nF) - 1)
                               else fRepI .&. ((2 ^ nF) - 1)
      denom     = 2 ^ nF

instance ( Mult (rep size1) (rep size2)
         , MResult (rep size1) (rep size2) ~ rep (size1 + size2)
         ) => Mult (Fixed frac1 rep size1) (Fixed frac2 rep size2) where
  type MResult (Fixed frac1 rep size1) (Fixed frac2 rep size2) = Fixed (frac1 + frac2) rep (size1 + size2)
  mult (Fixed fRep1) (Fixed fRep2) = Fixed (mult fRep1 fRep2)

instance ( Resize rep
         , Bits (rep ((Max size1 size2) + 1)), Bits (rep size1), Bits (rep size2)
         , KnownNat ((Max size1 size2) + 1), KnownNat size1, KnownNat size2
         , KnownNat (Max frac1 frac2), KnownNat frac1, KnownNat frac2
         , Ord (rep size1), Ord (rep size2)
         , Num (rep ((Max size1 size2) + 1)), Num (rep size1), Num (rep size2)
         , Bounded (rep ((Max size1 size2) + 1)), Bounded (rep size1), Bounded (rep size2)
         ) => Add (Fixed frac1 rep size1) (Fixed frac2 rep size2) where
  type AResult (Fixed frac1 rep size1) (Fixed frac2 rep size2) = Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
  plus f1 f2 = let (Fixed f1R) = resizeF f1 :: Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
                   (Fixed f2R) = resizeF f2 :: Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
               in  Fixed (f1R + f2R)
  minus f1 f2 = let (Fixed f1R) = resizeF f1 :: Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
                    (Fixed f2R) = resizeF f2 :: Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
                in  Fixed (f1R - f2R)

-- | The operators of this instance saturate on overflow, and use truncation for rounding
instance ( MResult (rep size1) (rep size1) ~ rep (size1 + size1)
         , Resize rep, Mult (rep size1) (rep size1)
         , KnownNat frac, KnownNat (size1 + size1), KnownNat size1, KnownNat (size1 + 1)
         , Bits (rep (size1 + size1)), Bits (rep size1), Bits (rep (size1 + 1)), KnownNat (frac + frac)
         , Ord (rep (size1 + size1)), Num (rep (size1 + size1))
         , Bounded (rep (size1 + size1)), Bounded (rep size1)
         , BitSize (rep (size1 + 1)) ~ (size1 + 1), BitSize (rep size1) ~ size1
         , BitVector (rep (size1 + 1)), BitVector (rep size1)
         , Num (rep size1), Num (rep (size1 + 1))
         , size1 ~ (size + 1)
         ) => Num (Fixed frac rep size1) where
  (Fixed a) + (Fixed b) = Fixed (satN1 (resize a + resize b))
  (Fixed a) * (Fixed b) = resizeF (Fixed (a `mult` b) :: Fixed (frac + frac) rep (size1 + size1))
  (Fixed a) - (Fixed b) = Fixed (satN1 (resize a - resize b))
  negate (Fixed a)      = Fixed (satN1 (negate (resize a)))
  abs (Fixed a)         = Fixed (satN1 (abs (resize a)))
  signum (Fixed a)      = Fixed (signum a)
  fromInteger i         = let fSH = fromInteger (natVal (Proxy :: Proxy frac))
                              res = Fixed (fromInteger i `shiftL` fSH)
                          in  res

instance (BitVector (rep size)) => BitVector (Fixed frac rep size) where
  type BitSize (Fixed frac rep size) = BitSize (rep size)
  toBV (Fixed fRep) = toBV fRep
  fromBV bv         = Fixed (fromBV bv)

instance Pack (Fixed frac rep size) where
  type SignalP (Fixed frac rep size) = Signal (Fixed frac rep size)
  pack   = id
  unpack = id

instance (Lift (rep size), KnownNat frac, KnownNat size, Typeable rep) =>
         Lift (Fixed frac rep size) where
  lift f@(Fixed fRep) = sigE [| Fixed fRep |] (decFixed (natVal (asFracProxy f)) (typeRep (asRepProxy f)) (natVal f))

decFixed :: Integer -> TypeRep -> Integer -> TypeQ
decFixed f r s = do
  foldl appT (conT ''Fixed) [litT (numTyLit f), conT (mkName (show r)), litT (numTyLit s)]

instance Default (rep size) => Default (Fixed frac rep size) where
  def = Fixed def

instance Bounded (rep size) => Bounded (Fixed frac rep size) where
  minBound = Fixed minBound
  maxBound = Fixed maxBound

-- | Saturating resize operation, truncates for rounding
resizeF :: forall frac1 frac2 rep size1 size2 .
           ( Bounded (rep size2), Eq (rep size1), Ord (rep size1)
           , Num (rep size1), Bits (rep size1), Resize rep
           , KnownNat size2, KnownNat size1, Bits (rep size2)
           , KnownNat frac2, KnownNat frac1, Bounded (rep size1))
        => Fixed frac1 rep size1
        -> Fixed frac2 rep size2
resizeF (Fixed fRep) = res
  where
    argSZ = natVal (Proxy :: Proxy size1)
    resSZ = natVal (Proxy :: Proxy size2)

    argFracSZ = fromInteger (natVal (Proxy :: Proxy frac1))
    resFracSZ = fromInteger (natVal (Proxy :: Proxy frac2))

    trunc = if argFracSZ <= resFracSZ
              then (resize fRep) `shiftL` (resFracSZ - argFracSZ)
              else (resize fRep) `shiftR` (argFracSZ - resFracSZ)

    sat   = if argSZ <= resSZ
              then trunc
              else let fMax = maxBound
                       fMin = minBound
                       mask = complement (resize fMax) :: rep size1
                   in if fRep >= 0
                         then if (fRep .&. mask) == 0
                                    then trunc
                                    else fMax
                         else if (fRep .&. mask) == mask
                                    then trunc
                                    else fMin

    res = Fixed sat

-- | Cheaper saturation when reducing the size of the number by only 1 bit
satN1 :: ( BitVector (rep (n + 1 + 1)), BitVector (rep (n + 1))
         , BitSize (rep (n + 1 + 1)) ~ (n + 1 + 1)
         , BitSize (rep (n + 1)) ~ (n + 1)
         , KnownNat (n + 1 + 1), KnownNat (n + 1)
         , Bounded (rep (n + 1))
         , Bits (rep (n + 1 + 1))
         )
      => rep (n + 1 + 1)
      -> rep (n + 1)
satN1 rep = if isSigned rep
              then case (c,sn) of
                     (L,H) -> maxBound
                     (H,L) -> minBound
                     _     -> fromBV s
              else case c of
                     H -> maxBound
                     _ -> fromBV s
  where
    repBV = toBV rep
    c     = vhead repBV
    s     = vtail repBV
    sn    = vhead s
