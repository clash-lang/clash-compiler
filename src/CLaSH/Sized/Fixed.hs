{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
-- * Truncation for negative numbers effectively results in: round towards
--   -infinity.
module CLaSH.Sized.Fixed
  ( -- * 'SFixed': 'Signed' 'Fixed' point numbers
    SFixed, sf, unSF
    -- * 'UFixed': 'Unsigned' 'Fixed' point numbers
  , UFixed, uf, unUF
      -- * Division
  , divide
    -- * 'Fixed' point literals
  , fLit
    -- * 'Fixed' point wrapper
  , Fixed (..), resizeF, fracShift
    -- * Constraint synonyms
    -- $constraintsynonyms

    -- ** Constraint synonyms for 'SFixed'
  , NumSFixedC, AddSFixedC, MultSFixedC, FracSFixedC, ResizeSFC, DivideSC
    -- ** Constraint synonyms for 'UFixed'
  , NumUFixedC, AddUFixedC, MultUFixedC, FracUFixedC, ResizeUFC, DivideUC
    -- ** Constraint synonyms for 'Fixed' wrapper
  , NumFixedC, AddFixedC, MultFixedC, FracFixedC, ResizeFC, DivideC
    -- * Proxy
  , asRepProxy, asIntProxy
  )
where

import Control.Arrow              ((***), second)
import Data.Bits                  (Bits (..))
import Data.Default               (Default (..))
import Data.List                  (find)
import Data.Maybe                 (fromJust)
import Data.Proxy                 (Proxy (..))
import Data.Ratio                 ((%), denominator, numerator)
import Data.Typeable              (Typeable, TypeRep, typeRep)
import GHC.TypeLits               (KnownNat, Nat, type (+), natVal)
import Language.Haskell.TH        (Q, TExp, TypeQ, appT, conT, litT, mkName,
                                   numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))

import CLaSH.Class.BitConvert     (BitConvert (..))
import CLaSH.Class.Num            (Mult (..), Add (..), SaturatingNum (..),
                                   SaturationMode (..), boundedPlus, boundedMin,
                                   boundedMult)
import CLaSH.Class.Resize         (Resize (..))
import CLaSH.Promoted.Nat         (SNat)
import CLaSH.Promoted.Ord         (Max)
import CLaSH.Sized.Signed         (Signed)
import CLaSH.Sized.Unsigned       (Unsigned)

-- | 'Fixed'-point number
--
-- Where:
--
-- * @rep@ is the underlying representation
--
-- * @int@ is the number of bits used to represent the integer part
--
-- * @frac@ is the number of bits used to represent the fractional part
--
-- The 'Num' operators for this type saturate to 'maxBound' on overflow and
-- 'minBound' on underflow, and use truncation as the rounding method.
newtype Fixed (rep :: Nat -> *) (int :: Nat) (frac :: Nat) =
  Fixed { unFixed :: rep (int + frac) }

deriving instance Eq (rep (int + frac))   => Eq (Fixed rep int frac)
deriving instance Ord (rep (int + frac))  => Ord (Fixed rep int frac)

-- | Instance functions do not saturate.
-- Meaning that \"@`'shiftL'` 1 == 'satMult' 'SatWrap' 2'@\""
deriving instance Bits (rep (int + frac)) => Bits (Fixed rep int frac)

-- | Signed 'Fixed'-point number, with @int@ integer bits (including sign-bit)
-- and @frac@ fractional bits.
--
-- * The range 'SFixed' @int@ @frac@ numbers is: [-(2^(@int@ -1)) ..
-- 2^(@int@-1) - 2^-@frac@ ]
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
type SFixed = Fixed Signed

-- | Unsigned 'Fixed'-point number, with @int@ integer bits and @frac@
-- fractional bits
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
type UFixed = Fixed Unsigned

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

asRepProxy :: Fixed rep int frac -> Proxy rep
asRepProxy _ = Proxy

asIntProxy :: Fixed rep int frac -> Proxy int
asIntProxy _ = Proxy

-- | Get the position of the virtual @point@ of a 'Fixed'-@point@ number
fracShift :: KnownNat frac => Fixed rep int frac -> Int
fracShift fx = fromInteger (natVal fx)

instance ( size ~ (int + frac), Show (rep size), Bits (rep size), KnownNat frac
         , Integral (rep size)
         ) => Show (Fixed rep int frac) where
  show f@(Fixed fRep) =
      i ++ "." ++ (uncurry pad . second (show . numerator) .
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
mac :: ( KnownNat frac
       , KnownNat (frac + frac)
       , KnownNat (int + frac)
       , KnownNat (1 + (int + frac))
       , KnownNat ((int + frac) + (int + frac))
       , ((int + int) + (frac + frac)) ~ ((int + frac) + (int + frac))
       )
    => SFixed int frac
    -> SFixed int frac
    -> SFixed int frac
    -> SFixed int frac
mac s x y = s + (x * y)
@

But with constraint synonyms, you can write the type signature like this:

@
mac :: NumSFixedC int frac
    => SFixed int frac
    -> SFixed int frac
    -> SFixed int frac
    -> SFixed int frac
mac s x y = s + (x * y)
@

Where 'NumSFixedC' refers to the @Constraints@ needed by the operators of
the 'Num' class for the 'SFixed' datatype.
-}

-- | Constraint for the 'Mult' instance of 'Fixed'
type MultFixedC rep (int1 :: Nat) (frac1 :: Nat) (int2 :: Nat) (frac2 :: Nat)
  = ( Mult    (rep (int1 + frac1)) (rep (int2 + frac2))
    , MResult (rep (int1 + frac1)) (rep (int2 + frac2)) ~
              rep ((int1 + int2) + (frac1 + frac2))
    )

-- | Constraint for the 'Mult' instance of 'SFixed'
type MultSFixedC int1 frac1 int2 frac2
  = ( KnownNat ((int1 + int2) + (frac1 + frac2))
    , ((int1 + frac1) + (int2 + frac2)) ~ ((int1 + int2) + (frac1 + frac2))
    )

-- | Constraint for the 'Mult' instance of 'UFixed'
type MultUFixedC int1 frac1 int2 frac2 =
     MultSFixedC int1 frac1 int2 frac2

-- | When used in a polymorphic setting, use the following
-- <CLaSH-Sized-Fixed.html#constraintsynonyms Constraint synonyms> for less
-- verbose type signatures:
--
-- * @'MultFixedC' rep frac1 frac2 size1 size2@ for: 'Fixed'
-- * @'MultSFixedC' int1 frac1 int2 frac2@      for: 'SFixed'
-- * @'MultUFixedC' int1 frac1 int2 frac2@      for: 'UFixed'
instance MultFixedC rep int1 frac1 int2 frac2 =>
  Mult (Fixed rep int1 frac1) (Fixed rep int2 frac2) where
  type MResult (Fixed rep int1 frac1) (Fixed rep int2 frac2) =
               Fixed rep (int1 + int2) (frac1 + frac2)
  mult (Fixed fRep1) (Fixed fRep2) = Fixed (mult fRep1 fRep2)

-- | Constraint for the 'Add' instance of 'Fixed'
type AddFixedC rep int1 frac1 int2 frac2
  = ( ResizeFC rep int1 frac1 (Max int1 int2 + 1) (Max frac1 frac2)
    , ResizeFC rep int2 frac2 (Max int1 int2 + 1) (Max frac1 frac2)
    , Bounded  (rep ((Max int1 int2 + 1) + Max frac1 frac2))
    , Num      (rep ((Max int1 int2 + 1) + Max frac1 frac2))
    )

-- | Constraint for the 'Add' instance of 'SFixed'
type AddSFixedC int1 frac1 int2 frac2
  = ( KnownNat frac1
    , KnownNat frac2
    , KnownNat (int1 + frac1)
    , KnownNat (int2 + frac2)
    , KnownNat (Max frac1 frac2)
    , KnownNat ((Max int1 int2 + 1) + Max frac1 frac2)
    )

-- -- | Constraint for the 'Add' instance of 'UFixed'
type AddUFixedC int1 frac1 int2 frac2 =
     AddSFixedC int1 frac1 int2 frac2

-- | When used in a polymorphic setting, use the following
-- <CLaSH-Sized-Fixed.html#constraintsynonyms Constraint synonyms> for less
-- verbose type signatures:
--
-- * @'AddFixedC'  rep frac1 frac2 size1 size2@ for: 'Fixed'
-- * @'AddSFixedC' int1 frac1 int2 frac2@       for: 'SFixed'
-- * @'AddUFixedC' int1 frac1 int2 frac2@       for: 'UFixed'
instance AddFixedC rep int1 frac1 int2 frac2 =>
  Add (Fixed rep int1 frac1) (Fixed rep int2 frac2) where
  type AResult (Fixed rep int1 frac1) (Fixed rep int2 frac2) =
               Fixed rep (Max int1 int2 + 1) (Max frac1 frac2)
  plus f1 f2  =
    let (Fixed f1R) = resizeF f1 :: Fixed rep (Max int1 int2 + 1) (Max frac1 frac2)
        (Fixed f2R) = resizeF f2 :: Fixed rep (Max int1 int2 + 1) (Max frac1 frac2)
    in  Fixed (f1R + f2R)
  minus f1 f2 =
    let (Fixed f1R) = resizeF f1 :: Fixed rep (Max int1 int2 + 1) (Max frac1 frac2)
        (Fixed f2R) = resizeF f2 :: Fixed rep (Max int1 int2 + 1) (Max frac1 frac2)
    in  Fixed (f1R - f2R)

-- | Constraint for the 'Num' instance of 'Fixed'
type NumFixedC rep int frac
  = ( SaturatingNum (rep (int + frac))
    , Mult (rep (int + frac)) (rep (int + frac))
    , ResizeFC rep (int + int) (frac + frac) int frac
    , MResult (rep (int + frac)) (rep (int + frac)) ~
              rep ((int + int) + (frac + frac))
    )

-- | Constraint for the 'Num' instance of 'SFixed'
type NumSFixedC int frac =
  ( KnownNat frac
  , KnownNat (frac + frac)
  , KnownNat (int + frac)
  , KnownNat (1 + (int + frac))
  , KnownNat ((int + frac) + (int + frac))
  , ((int + int) + (frac + frac)) ~ ((int + frac) + (int + frac))
  )
-- | Constraint for the 'Num' instance of 'UFixed'
type NumUFixedC int frac =
  ( KnownNat frac
  , KnownNat (frac + frac)
  , KnownNat (int + frac)
  , KnownNat ((int + frac) + 1)
  , KnownNat ((int + frac) + (int + frac))
  , ((int + int) + (frac + frac)) ~ ((int + frac) + (int + frac))
  )

-- | The operators of this instance saturate on overflow, and use truncation as
-- the rounding method.
--
-- When used in a polymorphic setting, use the following
-- <CLaSH-Sized-Fixed.html#constraintsynonyms Constraint synonyms> for less
-- verbose type signatures:
--
-- * @'NumFixedC' frac rep size@ for: @'Fixed' frac rep size@
-- * @'NumSFixedC' int frac@     for: @'SFixed' int frac@
-- * @'NumUFixedC' int frac@     for: @'UFixed' int frac@
instance (NumFixedC rep int frac) => Num (Fixed rep int frac) where
  (+)              = boundedPlus
  (*)              = boundedMult
  (-)              = boundedMin
  negate (Fixed a) = Fixed (negate a)
  abs    (Fixed a) = Fixed (abs a)
  signum (Fixed a) = Fixed (signum a)
  fromInteger i    = let fSH = fromInteger (natVal (Proxy :: Proxy frac))
                         res = Fixed (fromInteger i `shiftL` fSH)
                     in  res

instance (BitConvert (rep (int + frac))) => BitConvert (Fixed rep int frac) where
  type BitSize (Fixed rep int frac) = BitSize (rep (int + frac))
  pack   (Fixed fRep) = pack fRep
  unpack bv           = Fixed (unpack bv)

instance (Lift (rep (int + frac)), KnownNat frac, KnownNat int, Typeable rep) =>
  Lift (Fixed rep int frac) where
  lift f@(Fixed fRep) = sigE [| Fixed fRep |]
                          (decFixed (typeRep (asRepProxy f))
                                    (natVal (asIntProxy f))
                                    (natVal f))

decFixed :: TypeRep -> Integer -> Integer -> TypeQ
decFixed r i f = do
  foldl appT (conT ''Fixed) [ conT (mkName (show r))
                            , litT (numTyLit i)
                            , litT (numTyLit f)
                            ]

instance Default (rep (int + frac)) => Default (Fixed rep int frac) where
  def = Fixed def

instance Bounded (rep (int + frac)) => Bounded (Fixed rep int frac) where
  minBound = Fixed minBound
  maxBound = Fixed maxBound

-- | Constraint for the 'resizeF' function
type ResizeFC rep int1 frac1 int2 frac2
  = ( Resize   rep
    , Ord      (rep (int1 + frac1))
    , Num      (rep (int1 + frac1))
    , Bits     (rep (int1 + frac1))
    , Bits     (rep (int2 + frac2))
    , KnownNat frac1
    , KnownNat frac2
    , KnownNat (int1 + frac1)
    , KnownNat (int2 + frac2)
    )

-- | Constraint for the 'resizeF' function, specialized for 'SFixed'
type ResizeSFC int1 frac1 int2 frac2
  = ( KnownNat frac1
    , KnownNat frac2
    , KnownNat (int1 + frac1)
    , KnownNat (int2 + frac2)
    )

-- | Constraint for the 'resizeF' function, specialized for 'UFixed'
type ResizeUFC int1 frac1 int2 frac2 =
     ResizeSFC int1 frac1 int2 frac2

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
-- When used in a polymorphic setting, use the following
-- <#constraintsynonyms Constraint synonyms> for less verbose type signatures:
--
-- * @'ResizeFC' rep int1 frac1 int2 frac2@ for:
--   @'Fixed' rep int1 frac1 -> 'Fixed' rep int2 frac2@
--
-- * @'ResizeSFC' int1 frac1 int2 frac2@ for:
--   @'SFixed' int1 frac1 -> 'SFixed' int2 frac2@
--
-- * @'ResizeUFC' rep int1 frac1 int2 frac2@ for:
--   @'UFixed' int1 frac1 -> 'UFixed' int2 frac2@
resizeF ::(ResizeFC rep int1 frac1 int2 frac2, Bounded (rep (int2 + frac2)))
        => Fixed rep int1 frac1
        -> Fixed rep int2 frac2
resizeF = resizeF' False minBound maxBound

resizeF' :: forall rep int1 frac1 int2 frac2 . ResizeFC rep int1 frac1 int2 frac2
         => Bool               -- ^ Wrap
         -> rep (int2 + frac2) -- ^ minBound
         -> rep (int2 + frac2) -- ^ maxBound
         -> Fixed rep int1 frac1
         -> Fixed rep int2 frac2
resizeF' doWrap fMin fMax (Fixed fRep) = Fixed sat
  where
    argSZ = natVal (Proxy :: Proxy (int1 + frac1))
    resSZ = natVal (Proxy :: Proxy (int2 + frac2))

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
            else let mask = complement (resize fMax) :: rep (int1 + frac1)
                 in if argFracSZ <= resFracSZ
                       then let shiftedL         = fRep `shiftL`
                                                   (resFracSZ - argFracSZ)
                                shiftedL_masked  = shiftedL .&. mask
                                shiftedL_resized = resize shiftedL
                            in if doWrap then shiftedL_resized else if fRep >= 0
                                  then if shiftedL_masked == 0
                                          then shiftedL_resized
                                          else fMax
                                  else if shiftedL_masked == mask
                                          then shiftedL_resized
                                          else fMin
                       else let shiftedR         = fRep `shiftR`
                                                   (argFracSZ - resFracSZ)
                                shiftedR_masked  = shiftedR .&. mask
                                shiftedR_resized = resize shiftedR
                            in if doWrap then shiftedR_resized else if fRep >= 0
                                  then if shiftedR_masked == 0
                                          then shiftedR_resized
                                          else fMax
                                  else if shiftedR_masked == mask
                                          then shiftedR_resized
                                          else fMin

-- | Convert, at compile-time, a 'Double' literal to a 'Fixed'-point literal.
-- The conversion saturates on overflow, and uses truncation as its rounding
-- method.
--
-- So when you type:
--
-- > n = $$(fLit 2.8672) :: SFixed 4 4
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
fLit :: forall rep int frac size .
        ( size ~ (int + frac), KnownNat frac, Num (rep size), Bounded (rep size)
        , Integral (rep size))
     => Double
     -> Q (TExp (Fixed rep int frac))
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


instance NumFixedC rep int frac => SaturatingNum (Fixed rep int frac) where
  satPlus w (Fixed a) (Fixed b) = Fixed (satPlus w a b)
  satMin  w (Fixed a) (Fixed b) = Fixed (satMin w a b)
  satMult w (Fixed a) (Fixed b) = case w of
      SatWrap      -> resizeF' True 0 0 res
      SatBound     -> resizeF' False minBound maxBound res
      SatZero      -> resizeF' False 0 0 res
      SatSymmetric -> resizeF' False fMinSym maxBound res
    where
      res     = Fixed (a `mult` b) :: Fixed rep (int + int) (frac + frac)
      fMinSym = if isSigned a
                   then 0
                   else minBound + 1

-- | Constraint for the 'divide' function
type DivideC rep int1 frac1 int2 frac2
  = ( Resize   rep
    , Integral (rep (((int1 + frac2) + 1) + (int2 + frac1)))
    , Bits     (rep (((int1 + frac2) + 1) + (int2 + frac1)))
    , KnownNat int2
    , KnownNat frac2
    , KnownNat (int1 + frac1)
    , KnownNat (int2 + frac2)
    , KnownNat ((int1 + frac2 + 1) + (int2 + frac1))
    )

-- | Constraint for the 'divide' function, specialized for 'SFixed'
type DivideSC int1 frac1 int2 frac2
  = ( KnownNat int2
    , KnownNat frac2
    , KnownNat (int1 + frac1)
    , KnownNat (int2 + frac2)
    , KnownNat ((int1 + frac2 + 1) + (int2 + frac1))
    )

-- | Constraint for the 'divide' function, specialized for 'UFixed'
type DivideUC int1 frac1 int2 frac2 =
     DivideSC int1 frac1 int2 frac2

-- | Fixed point division
--
-- When used in a polymorphic setting, use the following
-- <#constraintsynonyms Constraint synonyms> for less verbose type signatures:
--
-- * @'DivideC' rep int1 frac1 int2 frac2@ for:
--   @'Fixed' rep int1 frac1 -> 'Fixed' rep int2 frac2 -> 'Fixed' rep (int1 + frac2 + 1) (int2 + frac1)@
--
-- * @'DivideSC' rep int1 frac1 int2 frac2@ for:
--   @'SFixed' int1 frac1 -> 'SFixed' int2 frac2 -> 'SFixed' (int1 + frac2 + 1) (int2 + frac1)@
--
-- * @'DivideUC' rep int1 frac1 int2 frac2@ for:
--   @'UFixed' int1 frac1 -> 'UFixed' int2 frac2 -> 'UFixed' (int1 + frac2 + 1) (int2 + frac1)@
divide :: DivideC rep int1 frac1 int2 frac2
       => Fixed rep int1 frac1
       -> Fixed rep int2 frac2
       -> Fixed rep (int1 + frac2 + 1) (int2 + frac1)
divide (Fixed fr1) fx2@(Fixed fr2) = Fixed res
  where
    int2  = fromInteger (natVal (asIntProxy fx2))
    frac2 = fromInteger (natVal fx2)
    fr1'  = resize fr1
    fr2'  = resize fr2
    fr1SH = shiftL fr1' ((int2 + frac2))
    res   = fr1SH `div` fr2'

-- | Constraint for the 'Fractional' instance of 'Fixed'
type FracFixedC rep int frac
  = ( NumFixedC rep int frac
    , DivideC   rep int frac int frac
    , Integral  (rep (int + frac))
    )

-- | Constraint for the 'Fractional' instance of 'SFixed'
type FracSFixedC int frac
  = ( NumSFixedC int frac
    , KnownNat int
    , KnownNat ((int + frac + 1) + (int + frac))
    )

-- | Constraint for the 'Fractional' instance of 'UFixed'
type FracUFixedC int frac
  = FracSFixedC int frac

-- | The operators of this instance saturate on overflow, and use truncation as
-- the rounding method.
--
-- When used in a polymorphic setting, use the following
-- <CLaSH-Sized-Fixed.html#constraintsynonyms Constraint synonyms> for less
-- verbose type signatures:
--
-- * @'FracFixedC' frac rep size@ for: @'Fixed' frac rep size@
-- * @'FracSFixedC' int frac@     for: @'SFixed' int frac@
-- * @'FracUFixedC' int frac@     for: @'UFixed' int frac@
instance (FracFixedC rep int frac) => Fractional (Fixed rep int frac) where
  f1 / f2        = resizeF (divide f1 f2)
  recip fx       = resizeF (divide (1 :: Fixed rep int frac) fx)
  fromRational r = res
    where
      res  = Fixed (fromInteger sat)
      sat  = if res' > rMax
                then rMax
                else if res' < rMin then rMin else res'

      rMax = toInteger (maxBound :: rep (int + frac))
      rMin = toInteger (minBound :: rep (int + frac))
      res' = n `div` d

      frac = fromInteger (natVal res)
      n    = numerator   r `shiftL` (2 * frac)
      d    = denominator r `shiftL` frac
