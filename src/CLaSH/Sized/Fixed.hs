{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fexpose-all-unfoldings -fno-strictness #-}

-- | Fixed point numbers
--
-- * The 'Num' operators for the given types saturate on overflow,
--   and use truncation as the rounding method.
--
-- * Use @$$('fLit' d)@ to create 'Fixed' point number literals.
-- * Use <#constraintsynonyms Constraint synonyms> when writing type signatures
--   for polymorphic functions that use 'Fixed' point numbers.
--
-- BEWARE: rounding by truncation introduces a sign bias!
--
-- * Truncation for positive numbers effectively results in: round towards zero.
-- * Truncation for negative numbers effectively results in: round towards -infinity.
module CLaSH.Sized.Fixed
  ( -- * 'SFixed': 'Signed' 'Fixed' point numbers
    SFixed, sf, unSF
    -- * 'UFixed': 'Unsigned' 'Fixed' point numbers
  , UFixed, uf, unUF
    -- * 'Fixed' point literals
  , fLit
    -- * 'Fixed' point wrapper
  , Fixed (..), resizeF, fracShift, satN2
    -- * Constraint synonyms
    -- $constraintsynonyms

    -- ** Constraint synonyms for 'SFixed'
  , NumSFixed, AddSFixed, MultSFixed, ResizeSFC
    -- ** Constraint synonyms for 'UFixed'
  , NumUFixed, AddUFixed, MultUFixed, ResizeUFC
    -- ** Constraint synonyms for 'Fixed' wrapper
  , NumFixed, AddFixed, MultFixed, ResizeFC, SatN2C
    -- ** Constraint synonyms for 'Signed' and 'Unsigned'
  , SatN2SC, SatN2UC
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
import CLaSH.Sized.Signed
import CLaSH.Sized.Unsigned
import CLaSH.Sized.Vector

-- | 'Fixed'-point number
--
-- Where:
--
-- * @frac@ denotes the position of the virtual @point@ counting from the LSB
--
-- * @rep@ is the underlying representation
--
-- * @size@ is the number of bits used to represent the number
--
-- The 'Num' operators for this type saturate on overflow,
-- and use truncation as the rounding method.
newtype Fixed (frac :: Nat) (rep :: Nat -> *) (size :: Nat) = Fixed { unFixed :: rep size }
  deriving (Eq,Ord)

-- | Signed 'Fixed'-point number, with @int@ integer bits (including sign-bit)
-- and @frac@ fractional bits.
--
-- * The range 'SFixed' @int@ @frac@ numbers is: [-(2^(@int@ -1)) .. 2^(@int@-1) - 2^-@frac@ ]
-- * The resolution of 'SFixed' @int@ @frac@ numbers is: 2^@frac@
-- * The 'Num' operators for this type saturate on overflow,
--   and use truncation as the rounding method.
--
-- >>>  maxBound :: SFixed 3 4
-- 3.9375
-- >>> minBound :: SFixed 3 4
-- -4.0
-- >>> (1 :: SFixed 3 4) + (2 :: SFixed 3 4)
-- 3.0
-- >>> (2 :: SFixed 3 4) + (3 :: SFixed 3 4)
-- 3.9375
-- >>> (-2 :: SFixed 3 4) + (-3 :: SFixed 3 4)
-- -4.0
-- >>> ($$(fLit 1.375) :: SFixed 3 4) * ($$(fLit -0.8125) :: SFixed 3 4)
-- -1.125
-- >>> ($$(fLit 1.375) :: SFixed 3 4) `mult` ($$(fLit -0.8125) :: SFixed 3 4) :: SFixed 6 8
-- -1.1171875
-- >>> (2 :: SFixed 3 4) `plus` (3 :: SFixed 3 4) :: SFixed 4 4
-- 5.0
-- >>> (-2 :: SFixed 3 4) `plus` (-3 :: SFixed 3 4) :: SFixed 4 4
-- -5.0
type SFixed int frac = Fixed frac Signed (int + frac)

-- | Unsigned 'Fixed'-point number, with @int@ integer bits and @frac@ fractional bits
--
-- * The range 'UFixed' @int@ @frac@ numbers is: [0 .. 2^@int@ - 2^-@frac@ ]
-- * The resolution of 'UFixed' @int@ @frac@ numbers is: 2^@frac@
-- * The 'Num' operators for this type saturate on overflow,
--   and use truncation as the rounding method.
--
-- >>> maxBound :: UFixed 3 4
-- 7.9375
-- >>> minBound :: UFixed 3 4
-- 0.0
-- >>> (1 :: UFixed 3 4) + (2 :: UFixed 3 4)
-- 3.0
-- >>> (2 :: UFixed 3 4) + (6 :: UFixed 3 4)
-- 7.9375
-- >>> (1 :: UFixed 3 4) - (3 :: UFixed 3 4)
-- 0.0
-- >>> ($$(fLit 1.375) :: UFixed 3 4) * ($$(fLit 0.8125) :: UFixed 3 4)
-- 1.0625
-- >>> ($$(fLit 1.375) :: UFixed 3 4) `mult` ($$(fLit 0.8125) :: UFixed 3 4) :: UFixed 6 8
-- 1.1171875
-- >>> (2 :: UFixed 3 4) `plus` (6 :: UFixed 3 4) :: UFixed 4 4
-- 8.0
--
-- However, 'minus' does not saturate to 'minBound' on underflow:
--
-- >>> (1 :: UFixed 3 4) `minus` (3 :: UFixed 3 4) :: UFixed 4 4
-- 14.0
type UFixed int frac = Fixed frac Unsigned (int + frac)

-- | Treat a 'Signed' integer as a @Signed@ 'Fixed'-@point@ integer
--
-- >>> sf d4 (-22 :: Signed 7)
-- -1.375
sf :: SNat frac           -- ^ Position of the virtual @point@
   -> Signed (int + frac) -- ^ The 'Signed' integer
   -> SFixed int frac
sf _ fRep = Fixed fRep

-- | See the underlying representation of a Signed Fixed-point integer
unSF :: SFixed int frac
     -> Signed (int + frac)
unSF (Fixed fRep) = fRep

-- | Treat an 'Unsigned' integer as a @Unsigned@ 'Fixed'-@point@ number
--
-- >>> uf d4 (92 :: Unsigned 7)
-- 5.75
uf :: SNat frac             -- ^ Position of the virtual @point@
   -> Unsigned (int + frac) -- ^ The 'Unsigned' integer
   -> UFixed int frac
uf _ fRep = Fixed fRep

-- | See the underlying representation of an Unsigned Fixed-point integer
unUF :: UFixed int frac
     -> Unsigned (int + frac)
unUF (Fixed fRep) = fRep

asFracProxy :: Fixed frac rep size -> Proxy frac
asFracProxy _ = Proxy

asRepProxy :: Fixed frac rep size -> Proxy rep
asRepProxy _ = Proxy

-- | Get the position of the virtual @point@ of a 'Fixed'-@point@ number
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

{- $constraintsynonyms #constraintsynonyms#
Writing polymorphic functions over fixed point numbers can be a potentially
verbose due to the many class constraints induced by the functions and operators
of this module.

Writing a simple multiply-and-accumulate function can already give rise to many
lines of constraints:

@
mac :: ( 1 <= (int + frac), (((int + frac) + 1) + 1) ~ ((int + frac) + 2)
       , KnownNat (frac + frac), KnownNat ((int + frac) + (int + frac))
       , KnownNat ((int + frac) + 2), KnownNat (int + frac), KnownNat frac
       )
    => SFixed int frac
    -> SFixed int frac
    -> SFixed int frac
    -> SFixed int frac
mac s x y = s + (x * y)
@

But with constraint synonyms, you can write the type signature like this:

@
mac :: NumSFixed int frac
    => SFixed int frac
    -> SFixed int frac
    -> SFixed int frac
    -> SFixed int frac
mac s x y = s + (x * y)
@

Where 'NumSFixed' refers to the @Constraints@ needed by the operators of
the 'Num' class for the 'SFixed' datatype.
-}

-- | Constraint for the 'Mult' instance of 'Fixed'
type MultFixed rep (frac1 :: Nat) (frac2 :: Nat) (size1 :: Nat) (size2 :: Nat)
  = ( Mult    (rep size1) (rep size2)
    , MResult (rep size1) (rep size2) ~ rep (size1 + size2)
    )

-- | Constraint for the 'Mult' instance of 'SFixed'
type MultSFixed int1 frac1 int2 frac2 = MultFixed Signed frac1 frac2 (int1 + frac1) (int2 + frac2)

-- | Constraint for the 'Mult' instance of 'UFixed'
type MultUFixed int1 frac1 int2 frac2 = MultFixed Unsigned frac1 frac2 (int1 + frac1) (int2 + frac2)

-- | When used in a polymorphic setting, use the following <CLaSH-Sized-Fixed.html#constraintsynonyms Constraint synonyms>
-- for less verbose type signatures:
--
-- * @'MultFixed' rep frac1 frac2 size1 size2@ for: 'Fixed'
-- * @'MultSFixed' int1 frac1 int2 frac2@      for: 'SFixed'
-- * @'MultUFixed' int1 frac1 int2 frac2@      for: 'UFixed'
instance MultFixed rep frac1 frac2 size1 size2 => Mult (Fixed frac1 rep size1) (Fixed frac2 rep size2) where
  type MResult (Fixed frac1 rep size1) (Fixed frac2 rep size2) = Fixed (frac1 + frac2) rep (size1 + size2)
  mult (Fixed fRep1) (Fixed fRep2) = Fixed (mult fRep1 fRep2)

-- | Constraint for the 'Add' instance of 'Fixed'
type AddFixed rep (frac1 :: Nat) (frac2 :: Nat) (size1 :: Nat) (size2 :: Nat)
  = ( ResizeFC rep frac1 (Max frac1 frac2) size1 ((Max size1 size2) + 1)
    , ResizeFC rep frac2 (Max frac1 frac2) size2 ((Max size1 size2) + 1)
    , Num (rep (Max size1 size2 + 1))
    )

-- | Constraint for the 'Add' instance of 'SFixed'
type AddSFixed int1 frac1 int2 frac2 = AddFixed Signed frac1 frac2 (int1 + frac1) (int2 + frac2)

-- | Constraint for the 'Add' instance of 'UFixed'
type AddUFixed int1 frac1 int2 frac2 = AddFixed Unsigned frac1 frac2 (int1 + frac1) (int2 + frac2)

-- | When used in a polymorphic setting, use the following <CLaSH-Sized-Fixed.html#constraintsynonyms Constraint synonyms>
-- for less verbose type signatures:
--
-- * @'AddFixed'  rep frac1 frac2 size1 size2@ for: 'Fixed'
-- * @'AddSFixed' int1 frac1 int2 frac2@       for: 'SFixed'
-- * @'AddUFixed' int1 frac1 int2 frac2@       for: 'UFixed'
instance AddFixed rep frac1 frac2 size1 size2 => Add (Fixed frac1 rep size1) (Fixed frac2 rep size2) where
  type AResult (Fixed frac1 rep size1) (Fixed frac2 rep size2) = Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
  plus f1 f2  = let (Fixed f1R) = resizeF f1 :: Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
                    (Fixed f2R) = resizeF f2 :: Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
                in  Fixed (f1R + f2R)
  minus f1 f2 = let (Fixed f1R) = resizeF f1 :: Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
                    (Fixed f2R) = resizeF f2 :: Fixed (Max frac1 frac2) rep ((Max size1 size2) + 1)
                in  Fixed (f1R - f2R)

-- | Constraint for the 'Num' instance of 'Fixed'
type NumFixed (frac :: Nat) rep (size :: Nat)
  = ( SatN2C   rep size
    , ResizeFC rep (frac + frac) frac (size + size) size
    , Num      (rep size)
    , Num      (rep (size + 2))
    , Mult     (rep size) (rep size)
    , MResult  (rep size) (rep size) ~ rep (size + size)
    )

-- | Constraint for the 'Num' instance of 'SFixed'
type NumSFixed int frac = ( 1 <= (int + frac), (((int + frac) + 1) + 1) ~ ((int + frac) + 2)
                          , KnownNat (frac + frac), KnownNat ((int + frac) + (int + frac))
                          , KnownNat ((int + frac) + 2), KnownNat (int + frac), KnownNat frac
                          )
-- | Constraint for the 'Num' instance of 'UFixed'
type NumUFixed int frac = ( 1 <= (int + frac), (((int + frac) + 1) + 1) ~ ((int + frac) + 2)
                          , KnownNat (frac + frac), KnownNat ((int + frac) + (int + frac))
                          , KnownNat ((int + frac) + 2), KnownNat (int + frac), KnownNat frac
                          )

-- | The operators of this instance saturate on overflow, and use truncation as the rounding method.
--
-- When used in a polymorphic setting, use the following <CLaSH-Sized-Fixed.html#constraintsynonyms Constraint synonyms>
-- for less verbose type signatures:
--
-- * @'NumFixed' frac rep size@ for: @'Fixed' frac rep size@
-- * @'NumSFixed' int frac@     for: @'SFixed' int frac@
-- * @'NumUFixed' int frac@     for: @'UFixed' int frac@
instance (NumFixed frac rep size) => Num (Fixed frac rep size) where
  (Fixed a) + (Fixed b) = Fixed (satN2 (resize a + resize b))
  (Fixed a) * (Fixed b) = resizeF (Fixed (a `mult` b) :: Fixed (frac + frac) rep (size + size))
  (Fixed a) - (Fixed b) = Fixed (satN2 (resize a - resize b))
  negate (Fixed a)      = Fixed (satN2 (negate (resize a)))
  abs (Fixed a)         = Fixed (satN2 (abs (resize a)))
  signum (Fixed a)      = Fixed (signum a)
  fromInteger i         = let fSH = fromInteger (natVal (Proxy :: Proxy frac))
                              res = Fixed (fromInteger i `shiftL` fSH)
                          in  res

instance (BitVector (rep size)) => BitVector (Fixed frac rep size) where
  type BitSize (Fixed frac rep size) = BitSize (rep size)
  toBV (Fixed fRep) = toBV fRep
  fromBV bv         = Fixed (fromBV bv)

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

-- | Constraint for the 'resizeF' function
type ResizeFC rep frac1 frac2 size1 size2
  = ( Bounded (rep size2), Eq (rep size1), Ord (rep size1)
    , Num (rep size1), Bits (rep size1), Resize rep
    , KnownNat size2, KnownNat size1, Bits (rep size2)
    , KnownNat frac2, KnownNat frac1, Bounded (rep size1)
    )

-- | Constraint for the 'resizeF' function, specialized for 'SFixed'
type ResizeSFC int1 frac1 int2 frac2 = (KnownNat (int2 + frac2), KnownNat (int1 + frac1), KnownNat frac1, KnownNat frac2)

-- | Constraint for the 'resizeF' function, specialized for 'UFixed'
type ResizeUFC int1 frac1 int2 frac2 = (KnownNat (int2 + frac2), KnownNat (int1 + frac1), KnownNat frac1, KnownNat frac2)

-- | Saturating resize operation, truncates for rounding
--
-- >>> $$(fLit 0.8125) :: SFixed 3 4
-- 0.8125
-- >>> resizeF ($$(fLit 0.8125) :: SFixed 3 4) :: SFixed 2 3
-- 0.75
-- >>> $$(fLit 3.4) :: SFixed 3 4
-- 3.375
-- >>> resizeF ($$(fLit 3.4) :: SFixed 3 4) :: SFixed 2 3
-- 1.875
-- >>> maxBound :: SFixed 2 3
-- 1.875
--
-- When used in a polymorphic setting, use the following <#constraintsynonyms Constraint synonyms>
-- for less verbose type signatures:
--
-- * @'ResizeFC'  rep frac1 frac2 size1 size2@ for: @'Fixed' frac1 rep size1 -> 'Fixed' frac2 rep size2@
-- * @'ResizeSFC' int1 frac1 int2 frac2@       for: @'SFixed' int1 frac1 -> 'SFixed' int2 frac2@
-- * @'ResizeUFC' int1 frac1 int2 frac2@       for: @'UFixed' int1 frac1 -> 'UFixed' int2 frac2@
resizeF :: forall frac1 frac2 rep size1 size2 .
           ResizeFC rep frac1 frac2 size1 size2
        => Fixed frac1 rep size1
        -> Fixed frac2 rep size2
resizeF (Fixed fRep) = Fixed sat
  where
    argSZ = natVal (Proxy :: Proxy size1)
    resSZ = natVal (Proxy :: Proxy size2)

    argFracSZ = fromInteger (natVal (Proxy :: Proxy frac1))
    resFracSZ = fromInteger (natVal (Proxy :: Proxy frac2))

    -- All size and frac comparisons and related if-then-else statements should
    -- be optimized away by the compiler
    sat = if argSZ <= resSZ
            -- if the argument is smaller than the result, resize before shift
            then if argFracSZ <= resFracSZ
                    then resize fRep `shiftL` (resFracSZ - argFracSZ)
                    else resize fRep `shiftR` (argFracSZ - resFracSZ)
            -- if the argument is bigger than the result, shift before resize
            else let fMax = maxBound
                     fMin = minBound
                     mask = complement (resize fMax) :: rep size1
                 in if argFracSZ <= resFracSZ
                       then let shiftedL         = fRep `shiftL` (resFracSZ - argFracSZ)
                                shiftedL_masked  = shiftedL .&. mask
                                shiftedL_resized = resize shiftedL
                            in if fRep >= 0
                                  then if shiftedL_masked == 0
                                          then shiftedL_resized
                                          else fMax
                                  else if shiftedL_masked == mask
                                          then shiftedL_resized
                                          else fMin
                       else let shiftedR         = fRep `shiftR` (argFracSZ - resFracSZ)
                                shiftedR_masked  = shiftedR .&. mask
                                shiftedR_resized = resize shiftedR
                            in if fRep >= 0
                                  then if shiftedR_masked == 0
                                          then shiftedR_resized
                                          else fMax
                                  else if shiftedR_masked == mask
                                          then shiftedR_resized
                                          else fMin

-- | Constraint for the 'satN2' function
type SatN2C rep n
  = ( 1 <= n
    , ((n + 1) + 1) ~ (n + 2)
    , BitVector (rep n)
    , BitVector (rep (n + 2))
    , BitSize   (rep n) ~ n
    , BitSize   (rep (n + 2)) ~ (n + 2)
    , KnownNat  n
    , KnownNat  (n + 2)
    , Bounded   (rep n)
    , Bits      (rep (n + 2))
    )

-- | Constraint for the 'satN2' function, specialized for 'Signed'
type SatN2SC n = (1 <= n, ((n + 1) + 1) ~ (n + 2), KnownNat n, KnownNat (n + 2))

-- | Constraint for the 'satN2' function, specialized for 'Unsigned'
type SatN2UC n = (1 <= n, ((n + 1) + 1) ~ (n + 2), KnownNat n, KnownNat (n + 2))

-- | Resize an (N+2)-bits number to an N-bits number, saturates to
-- 'minBound' or 'maxBound' when the argument does not fit within
-- the representations bounds of the result.
--
-- Uses cheaper saturation than 'resizeF', which is made possible by knowing
-- that we only reduce the size by 2 bits.
--
-- >>> (2 :: Unsigned 2) + (3 :: Unsigned 2)
-- 1
-- >>> satN2 (resize (2 :: Unsigned 2) + resize (3 :: Unsigned 2)) :: Unsigned 2
-- 3
-- >>> satN2 (resize (1 :: Unsigned 2) + resize (1 :: Unsigned 2)) :: Unsigned 2
-- 2
-- >>> (2 :: Unsigned 2) - (3 :: Unsigned 2)
-- 3
-- >>> satN2 (resize (2 :: Unsigned 2) - resize (3 :: Unsigned 2)) :: Unsigned 2
-- 0
-- >>> (2 :: Signed 3) + (3 :: Signed 3)
-- -3
-- >>> satN2 (resize (2 :: Signed 3) + resize (3 :: Signed 3)) :: Signed 3
-- 3
--
-- When used in a polymorphic setting, use the following <#constraintsynonyms Constraint synonyms>
-- for less verbose type signatures:
--
-- * 'SatN2C'  for: @rep (n+2) -> rep n@
-- * 'SatN2SC' for: @'Signed' (n+2) -> 'Signed' n@
-- * 'SatN2UC' for: @'Unsigned' (n+2) -> 'Unsigned' n@
satN2 :: SatN2C rep n
      => rep (n + 2)
      -> rep n
satN2 rep = if isSigned rep
              then case (cS,sn) of
                     (L,H) -> maxBound
                     (H,L) -> minBound
                     _     -> fromBV s
              else case (cS,cU) of
                     (H,H) -> minBound
                     (L,H) -> maxBound
                     _     -> fromBV s
  where
    repBV = toBV rep
    cS    = vhead repBV
    cU    = vhead (vtail repBV)
    s     = vtail (vtail repBV)
    sn    = vhead' s

-- | Convert, at compile-time, a 'Double' literal to a 'Fixed'-point literal.
-- The conversion saturates on overflow, and uses truncation as its rounding method.
--
-- So when you type:
--
-- > n = $$(fLit 2.2867) :: SFixed 4 4
--
-- The compiler sees:
--
-- > n = Fixed (fromInteger 45) :: SFixed 4 4
--
-- Upon evaluation you see that the value is rounded / truncated in accordance
-- to the fixed point representation:
--
-- >>> n
-- 2.8125
fLit :: forall frac rep size .
        (KnownNat frac, Num (rep size), Bounded (rep size), Integral (rep size))
     => Double
     -> Q (TExp (Fixed frac rep size))
fLit a = [|| Fixed (fromInteger sat) ||]
  where
    rMax      = toInteger (maxBound :: rep size)
    rMin      = toInteger (minBound :: rep size)
    sat       = if truncated > rMax
                   then rMax
                   else if truncated < rMin
                           then rMin
                           else truncated
    truncated = truncate shifted :: Integer
    shifted   = a * (2 ^ (natVal (Proxy :: Proxy frac)))
