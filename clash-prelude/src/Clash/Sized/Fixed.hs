{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Fixed point numbers

* The 'Num' operators for the given types saturate on overflow,
  and use truncation as the rounding method.
* 'Fixed' has an instance for 'Fractional' meaning you use fractional
  literals @(3.75 :: 'SFixed' 4 18)@.
* Both integer literals and fractional literals are clipped to 'minBound' and
  'maxBound'. __NB__ Needs the `-XNegativeLiterals` language extension to work
  for signed numbers.
* There is no 'Floating' instance for 'Fixed', but you can use @$$('fLit' d)@
  to create 'Fixed' point literal from 'Double' constant at compile-time.
* Use <#constraintsynonyms Constraint synonyms> when writing type signatures
  for polymorphic functions that use 'Fixed' point numbers.

BEWARE: rounding by truncation can introduce errors larger than naively assumed;
e.g. for /Fixed 16 1/, rounding by truncation turns the real number 4.99 to 4.5,
not 5.0, i.e. an error or 0.49 instead of 0.01

BEWARE: rounding by truncation introduces a sign bias!

* Truncation for positive numbers effectively results in: round towards zero.
* Truncation for negative numbers effectively results in: round towards -infinity.

== Reasoning about precision
Givens the real numbers /A/ and /B/, and the corresponding fixed point numbers
/FA+-da/ and /FB+db/, where /da/ and /db/ denote the (potential) error introduced
by truncation w.r.t. the original /A/ and /B/, the arithmetic operators on fixed
point numbers have the following error propagation properties:

* Addition: /da + db/
* Subtraction: /da - db/
* Multiplication: /FA*db + FB*da + da*db/
* Division: /(FA+da)\/(FB+db) - FA\/FB/

=== Additional error from truncation

Given:

>>> 4.13 :: UFixed 16 3
4.125
>>> 20.9 :: UFixed 16 3
20.875

The expected error that we would get from multiplication is:
/20.875*0.005 + 4.125*0.025 + 0.025*0.005 = 0.207625/

>>> 4.13 * 20.9 :: Double
86.317
>>> (4.13 :: UFixed 16 3) `mul` (20.9 :: UFixed 16 3) :: UFixed 32 6
86.109375
>>> 86.109375 + 0.207625 :: Double
86.317

However the /0.109375/ is smaller than /2^-3/, so the regular multiplication
operator that uses truncation introduces an additional error of /0.109375/:

>>> (4.13 :: UFixed 16 3) * (20.9 :: UFixed 16 3) :: UFixed 16 3
86.0

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Sized.Fixed
  ( -- * 'SFixed': 'Signed' 'Fixed' point numbers
    SFixed, sf, unSF
    -- * 'UFixed': 'Unsigned' 'Fixed' point numbers
  , UFixed, uf, unUF
      -- * Division
  , divide
    -- * Compile-time 'Double' conversion
  , fLit
    -- * Run-time 'Double' conversion (not synthesizable)
  , fLitR
    -- * 'Fixed' point wrapper
  , Fixed (..), resizeF, fracShift
    -- * Constraint synonyms #constraintsynonyms#
    -- $constraintsynonyms

    -- ** Constraint synonyms for 'SFixed'
  , NumSFixedC, ENumSFixedC, FracSFixedC, ResizeSFC, DivideSC
    -- ** Constraint synonyms for 'UFixed'
  , NumUFixedC, ENumUFixedC, FracUFixedC, ResizeUFC, DivideUC
    -- ** Constraint synonyms for 'Fixed' wrapper
  , NumFixedC, ENumFixedC, FracFixedC, ResizeFC, DivideC
    -- * Proxy
  , asRepProxy, asIntProxy
  )
where

import Control.DeepSeq            (NFData)
import Control.Arrow              ((***), second)
import Data.Bits                  (Bits (..), FiniteBits)
import Data.Data                  (Data)
import Data.Default.Class         (Default (..))
import Data.Either                (isLeft)
import Data.Kind                  (Type)
import Text.Read                  (Read(..))
import Data.List                  (find)
import Data.Proxy                 (Proxy (..))
import Data.Ratio                 ((%), denominator, numerator)
import Data.Typeable              (Typeable, TypeRep, typeRep)
import GHC.TypeLits               (KnownNat, Nat, type (+), natVal)
import GHC.TypeLits.Extra         (Max)
import Language.Haskell.TH        (Q, TExp, TypeQ, appT, conT, litT, mkName,
                                   numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift(..))
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Compat
#endif
import Test.QuickCheck            (Arbitrary, CoArbitrary)

import Clash.Class.BitPack        (BitPack (..))
import Clash.Class.Num            (ExtendingNum (..), SaturatingNum (..),
                                   SaturationMode (..), boundedAdd, boundedSub,
                                   boundedMul)
import Clash.Class.Resize         (Resize (..))
import Clash.Promoted.Nat         (SNat)
import Clash.Prelude.BitIndex     (msb, split)
import Clash.Prelude.BitReduction (reduceAnd, reduceOr)
import Clash.Sized.BitVector      (BitVector, (++#))
import Clash.Sized.Signed         (Signed)
import Clash.Sized.Unsigned       (Unsigned)
import Clash.XException
  (ShowX (..), NFDataX (..), isX, errorX, showsPrecXWith, fromJustX)

{- $setup
>>> :set -XDataKinds
>>> :set -XTemplateHaskell
>>> import Clash.Prelude
>>> let n = $$(fLit pi) :: SFixed 4 4
-}

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
newtype Fixed (rep :: Nat -> Type) (int :: Nat) (frac :: Nat) =
  Fixed { unFixed :: rep (int + frac) }

deriving instance NFData (rep (int + frac)) => NFData (Fixed rep int frac)
deriving instance (Typeable rep, Typeable int, Typeable frac
                  , Data (rep (int + frac))) => Data (Fixed rep int frac)
deriving instance Eq (rep (int + frac))      => Eq (Fixed rep int frac)
deriving instance Ord (rep (int + frac))     => Ord (Fixed rep int frac)
deriving instance Enum (rep (int + frac))    => Enum (Fixed rep int frac)
deriving instance Bounded (rep (int + frac)) => Bounded (Fixed rep int frac)
deriving instance Default (rep (int + frac)) => Default (Fixed rep int frac)
deriving instance Arbitrary (rep (int + frac)) => Arbitrary (Fixed rep int frac)
deriving instance CoArbitrary (rep (int + frac)) => CoArbitrary (Fixed rep int frac)
deriving instance FiniteBits (rep (int + frac)) => FiniteBits (Fixed rep int frac)

-- | Instance functions do not saturate.
-- Meaning that \"@\`shiftL\` 1 == 'satMul' 'SatWrap' 2'@\"
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
-- >>> read (show (maxBound :: SFixed 3 4)) :: SFixed 3 4
-- 3.9375
-- >>> 1 + 2 :: SFixed 3 4
-- 3.0
-- >>> 2 + 3 :: SFixed 3 4
-- 3.9375
-- >>> (-2) + (-3) :: SFixed 3 4
-- -4.0
-- >>> 1.375 * (-0.8125) :: SFixed 3 4
-- -1.125
-- >>> (1.375 :: SFixed 3 4) `mul` (-0.8125 :: SFixed 3 4) :: SFixed 6 8
-- -1.1171875
-- >>> (2 :: SFixed 3 4) `add` (3 :: SFixed 3 4) :: SFixed 4 4
-- 5.0
-- >>> (-2 :: SFixed 3 4) `add` (-3 :: SFixed 3 4) :: SFixed 4 4
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
-- >>> 1 + 2 :: UFixed 3 4
-- 3.0
-- >>> 2 + 6 :: UFixed 3 4
-- 7.9375
-- >>> 1 - 3 :: UFixed 3 4
-- 0.0
-- >>> 1.375 * 0.8125 :: UFixed 3 4
-- 1.0625
-- >>> (1.375 :: UFixed 3 4) `mul` (0.8125 :: UFixed 3 4) :: UFixed 6 8
-- 1.1171875
-- >>> (2 :: UFixed 3 4) `add` (6 :: UFixed 3 4) :: UFixed 4 4
-- 8.0
--
-- However, 'sub' does not saturate to 'minBound' on underflow:
--
-- >>> (1 :: UFixed 3 4) `sub` (3 :: UFixed 3 4) :: UFixed 4 4
-- 14.0
type UFixed = Fixed Unsigned

{-# INLINE sf #-}
-- | Treat a 'Signed' integer as a @Signed@ 'Fixed'-@point@ integer
--
-- >>> sf d4 (-22 :: Signed 7)
-- -1.375
sf
  :: SNat frac
  -- ^ Position of the virtual @point@
  -> Signed (int + frac)
  -- ^ The 'Signed' integer
  -> SFixed int frac
sf _ fRep = Fixed fRep

{-# INLINE unSF #-}
-- | See the underlying representation of a Signed Fixed-point integer
unSF :: SFixed int frac
     -> Signed (int + frac)
unSF (Fixed fRep) = fRep

{-# INLINE uf #-}
-- | Treat an 'Unsigned' integer as a @Unsigned@ 'Fixed'-@point@ number
--
-- >>> uf d4 (92 :: Unsigned 7)
-- 5.75
uf
  :: SNat frac
  -- ^ Position of the virtual @point@
  -> Unsigned (int + frac)
  -- ^ The 'Unsigned' integer
  -> UFixed int frac
uf _ fRep = Fixed fRep

{-# INLINE unUF #-}
-- | See the underlying representation of an Unsigned Fixed-point integer
unUF :: UFixed int frac
     -> Unsigned (int + frac)
unUF (Fixed fRep) = fRep

{-# INLINE asRepProxy #-}
-- | 'Fixed' as a 'Proxy' for it's representation type @rep@
asRepProxy :: Fixed rep int frac -> Proxy rep
asRepProxy _ = Proxy

{-# INLINE asIntProxy #-}
-- | 'Fixed' as a 'Proxy' for the number of integer bits @int@
asIntProxy :: Fixed rep int frac -> Proxy int
asIntProxy _ = Proxy

-- | Get the position of the virtual @point@ of a 'Fixed'-@point@ number
fracShift :: KnownNat frac => Fixed rep int frac -> Int
fracShift fx = fromInteger (natVal fx)

instance ( size ~ (int + frac), KnownNat frac, Integral (rep size)
         ) => Show (Fixed rep int frac) where
  show f@(Fixed fRep) =
      i ++ "." ++ (uncurry pad . second (show . numerator) .
                   fromJustX . find ((==1) . denominator . snd) .
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

instance ( size ~ (int + frac), KnownNat frac, Integral (rep size)
         ) => ShowX (Fixed rep int frac) where
  showsPrecX = showsPrecXWith showsPrec

instance NFDataX (rep (int + frac)) => NFDataX (Fixed rep int frac) where
  deepErrorX = Fixed . errorX
  rnfX f@(~(Fixed x)) = if isLeft (isX f) then () else rnfX x
  hasUndefined f@(~(Fixed x)) = if isLeft (isX f) then True else hasUndefined x
  ensureSpine ~(Fixed x) = Fixed x

-- | None of the 'Read' class' methods are synthesizable.
instance (size ~ (int + frac), KnownNat frac, Bounded (rep size), Integral (rep size))
      => Read (Fixed rep int frac) where
  readPrec = fLitR <$> readPrec

{- $constraintsynonyms
Writing polymorphic functions over fixed point numbers can be a potentially
verbose due to the many class constraints induced by the functions and operators
of this module.

Writing a simple multiply-and-accumulate function can already give rise to many
lines of constraints:

@
mac :: ( 'GHC.TypeLits.KnownNat' frac
       , 'GHC.TypeLits.KnownNat' (frac + frac)
       , 'GHC.TypeLits.KnownNat' (int + frac)
       , 'GHC.TypeLits.KnownNat' (1 + (int + frac))
       , 'GHC.TypeLits.KnownNat' ((int + frac) + (int + frac))
       , ((int + int) + (frac + frac)) ~ ((int + frac) + (int + frac))
       )
    => 'SFixed' int frac
    -> 'SFixed' int frac
    -> 'SFixed' int frac
    -> 'SFixed' int frac
mac s x y = s + (x * y)
@

But with constraint synonyms, you can write the type signature like this:

@
mac1 :: 'NumSFixedC' int frac
    => 'SFixed' int frac
    -> 'SFixed' int frac
    -> 'SFixed' int frac
    -> 'SFixed' int frac
mac1 s x y = s + (x * y)
@

Where 'NumSFixedC' refers to the @Constraints@ needed by the operators of
the 'Num' class for the 'SFixed' datatype.

Although the number of constraints for the @mac@ function defined earlier might
be considered small, here is an \"this way lies madness\" example where you
really want to use constraint kinds:

@
mac2 :: ( 'GHC.TypeLits.KnownNat' frac1
        , 'GHC.TypeLits.KnownNat' frac2
        , 'GHC.TypeLits.KnownNat' frac3
        , 'GHC.TypeLits.KnownNat' (Max frac1 frac2)
        , 'GHC.TypeLits.KnownNat' (int1 + frac1)
        , 'GHC.TypeLits.KnownNat' (int2 + frac2)
        , 'GHC.TypeLits.KnownNat' (int3 + frac3)
        , 'GHC.TypeLits.KnownNat' (frac1 + frac2)
        , 'GHC.TypeLits.KnownNat' (Max (frac1 + frac2) frac3)
        , 'GHC.TypeLits.KnownNat' (((int1 + int2) + (frac1 + frac2)) + (int3 + frac3))
        , 'GHC.TypeLits.KnownNat' ((int1 + int2) + (frac1 + frac2))
        , 'GHC.TypeLits.KnownNat' (1 + Max (int1 + frac1) (int2 + frac2))
        , 'GHC.TypeLits.KnownNat' (1 + Max (int1 + int2) int3 + Max (frac1 + frac2) frac3)
        , 'GHC.TypeLits.KnownNat' ((1 + Max int1 int2) + Max frac1 frac2)
        , 'GHC.TypeLits.KnownNat' ((1 + Max ((int1 + int2) + (frac1 + frac2)) (int3 + frac3)))
        , ((int1 + frac1) + (int2 + frac2)) ~ ((int1 + int2) + (frac1 + frac2))
        , (((int1 + int2) + int3) + ((frac1 + frac2) + frac3)) ~ (((int1 + int2) + (frac1 + frac2)) + (int3 + frac3))
        )
     => 'SFixed' int1 frac1
     -> 'SFixed' int2 frac2
     -> 'SFixed' int3 frac3
     -> 'SFixed' (1 + Max (int1 + int2) int3) (Max (frac1 + frac2) frac3)
mac2 x y s = (x \`mul\` y) \`add\` s
@

Which, with the proper constraint kinds can be reduced to:

@
mac3 :: ( 'ENumSFixedC' int1 frac1 int2 frac2
        , 'ENumSFixedC' (int1 + int2) (frac1 + frac2) int3 frac3
        )
     => 'SFixed' int1 frac1
     -> 'SFixed' int2 frac2
     -> 'SFixed' int3 frac3
     -> 'SFixed' (1 + Max (int1 + int2) int3) (Max (frac1 + frac2) frac3)
mac3 x y s = (x \`mul\` y) \`add\` s
@
-}

-- | Constraint for the 'ExtendingNum' instance of 'Fixed'
type ENumFixedC rep int1 frac1 int2 frac2
  = ( Bounded  (rep ((1 + Max int1 int2) + Max frac1 frac2))
    , Num      (rep ((1 + Max int1 int2) + Max frac1 frac2))
    , Bits     (rep ((1 + Max int1 int2) + Max frac1 frac2))
    , ExtendingNum (rep (int1 + frac1)) (rep (int2 + frac2))
    , MResult (rep (int1 + frac1)) (rep (int2 + frac2)) ~
              rep ((int1 + int2) + (frac1 + frac2))
    , KnownNat int1
    , KnownNat int2
    , KnownNat frac1
    , KnownNat frac2
    , Resize   rep
    )

-- | Constraint for the 'ExtendingNum' instance of 'SFixed'
type ENumSFixedC int1 frac1 int2 frac2
  = ( KnownNat (int2 + frac2)
    , KnownNat (1 + Max int1 int2 + Max frac1 frac2)
    , KnownNat (Max frac1 frac2)
    , KnownNat (1 + Max int1 int2)
    , KnownNat (int1 + frac1)
    , KnownNat frac2
    , KnownNat int2
    , KnownNat frac1
    , KnownNat int1
    )

-- | Constraint for the 'ExtendingNum' instance of 'UFixed'
type ENumUFixedC int1 frac1 int2 frac2 =
     ENumSFixedC int1 frac1 int2 frac2

-- | When used in a polymorphic setting, use the following
-- <Clash-Sized-Fixed.html#constraintsynonyms Constraint synonyms> for less
-- verbose type signatures:
--
-- * @'ENumFixedC'  rep frac1 frac2 size1 size2@ for: 'Fixed'
-- * @'ENumSFixedC' int1 frac1 int2 frac2@       for: 'SFixed'
-- * @'ENumUFixedC' int1 frac1 int2 frac2@       for: 'UFixed'
instance ENumFixedC rep int1 frac1 int2 frac2 =>
  ExtendingNum (Fixed rep int1 frac1) (Fixed rep int2 frac2) where
  type AResult (Fixed rep int1 frac1) (Fixed rep int2 frac2) =
               Fixed rep (1 + Max int1 int2) (Max frac1 frac2)
  add (Fixed f1) (Fixed f2) =
    let sh1 = fromInteger (natVal (Proxy @(Max frac1 frac2)) - natVal (Proxy @frac1)) :: Int
        f1R = shiftL (resize f1) sh1 :: rep ((1 + Max int1 int2) + (Max frac1 frac2))
        sh2 = fromInteger (natVal (Proxy @(Max frac1 frac2)) - natVal (Proxy @frac2)) :: Int
        f2R = shiftL (resize f2) sh2 :: rep ((1 + Max int1 int2) + (Max frac1 frac2))
    in  Fixed (f1R + f2R)
  sub (Fixed f1) (Fixed f2) =
    let sh1 = fromInteger (natVal (Proxy @(Max frac1 frac2)) - natVal (Proxy @frac1)) :: Int
        f1R = shiftL (resize f1) sh1 :: rep ((1 + Max int1 int2) + (Max frac1 frac2))
        sh2 = fromInteger (natVal (Proxy @(Max frac1 frac2)) - natVal (Proxy @frac2)) :: Int
        f2R = shiftL (resize f2) sh2 :: rep ((1 + Max int1 int2) + (Max frac1 frac2))
    in  Fixed (f1R - f2R)
  type MResult (Fixed rep int1 frac1) (Fixed rep int2 frac2) =
               Fixed rep (int1 + int2) (frac1 + frac2)
  mul (Fixed fRep1) (Fixed fRep2) = Fixed (mul fRep1 fRep2)

-- | Constraint for the 'Num' instance of 'Fixed'
type NumFixedC rep int frac
  = ( SaturatingNum (rep (int + frac))
    , ExtendingNum (rep (int + frac)) (rep (int + frac))
    , MResult (rep (int + frac)) (rep (int + frac)) ~
              rep ((int + int) + (frac + frac))
    , BitSize (rep ((int + int) + (frac + frac))) ~
              (int + ((int + frac) + frac))
    , BitPack (rep ((int + int) + (frac + frac)))
    , Bits    (rep ((int + int) + (frac + frac)))
    , BitPack (rep (int + frac))
    , Enum    (rep (int + frac))
    , Bits    (rep (int + frac))
    , Ord     (rep (int + frac))
    , Integral (rep (int + frac))
    , Resize  rep
    , KnownNat int
    , KnownNat frac
    )

-- | Constraint for the 'Num' instance of 'SFixed'
type NumSFixedC int frac =
  ( KnownNat ((int + int) + (frac + frac))
  , KnownNat (frac + frac)
  , KnownNat (int + int)
  , KnownNat (int + frac)
  , KnownNat frac
  , KnownNat int
  )

-- | Constraint for the 'Num' instance of 'UFixed'
type NumUFixedC int frac =
     NumSFixedC int frac

-- | The operators of this instance saturate on overflow, and use truncation as
-- the rounding method.
--
-- When used in a polymorphic setting, use the following
-- <Clash-Sized-Fixed.html#constraintsynonyms Constraint synonyms> for less
-- verbose type signatures:
--
-- * @'NumFixedC' frac rep size@ for: @'Fixed' frac rep size@
-- * @'NumSFixedC' int frac@     for: @'SFixed' int frac@
-- * @'NumUFixedC' int frac@     for: @'UFixed' int frac@
instance (NumFixedC rep int frac) => Num (Fixed rep int frac) where
  (+)              = boundedAdd
  (*)              = boundedMul
  (-)              = boundedSub
  negate           = boundedSub (Fixed 0)
  abs    (Fixed a) = Fixed (abs a)
  signum (Fixed a)
    | a == 0       = 0
    | a <  0       = -1
    | otherwise    = 1
  fromInteger i    = let fSH = fromInteger (natVal (Proxy @frac))
                         res = i `shiftL` fSH
                         rMax = toInteger (maxBound :: rep (int + frac))
                         rMin = toInteger (minBound :: rep (int + frac))
                         sat | res > rMax = rMax
                             | res < rMin = rMin
                             | otherwise  = res
                     in  Fixed (fromInteger sat)

instance (BitPack (rep (int + frac))) => BitPack (Fixed rep int frac) where
  type BitSize (Fixed rep int frac) = BitSize (rep (int + frac))
  pack   (Fixed fRep) = pack fRep
  unpack bv           = Fixed (unpack bv)

instance (Lift (rep (int + frac)), KnownNat frac, KnownNat int, Typeable rep) =>
  Lift (Fixed rep int frac) where
  lift f@(Fixed fRep) = sigE [| Fixed fRep |]
                          (decFixed (typeRep (asRepProxy f))
                                    (natVal (asIntProxy f))
                                    (natVal f))
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

decFixed :: TypeRep -> Integer -> Integer -> TypeQ
decFixed r i f = do
  foldl appT (conT ''Fixed) [ conT (mkName (show r))
                            , litT (numTyLit i)
                            , litT (numTyLit f)
                            ]

-- | Constraint for the 'resizeF' function
type ResizeFC rep int1 frac1 int2 frac2
  = ( Resize   rep
    , Ord      (rep (int1 + frac1))
    , Num      (rep (int1 + frac1))
    , Bits     (rep (int1 + frac1))
    , Bits     (rep (int2 + frac2))
    , Bounded  (rep (int2 + frac2))
    , KnownNat int1
    , KnownNat frac1
    , KnownNat int2
    , KnownNat frac2
    )

-- | Constraint for the 'resizeF' function, specialized for 'SFixed'
type ResizeSFC int1 frac1 int2 frac2
  = ( KnownNat int1
    , KnownNat frac1
    , KnownNat int2
    , KnownNat frac2
    , KnownNat (int2 + frac2)
    , KnownNat (int1 + frac1)
    )

-- | Constraint for the 'resizeF' function, specialized for 'UFixed'
type ResizeUFC int1 frac1 int2 frac2 =
     ResizeSFC int1 frac1 int2 frac2

{-# INLINE resizeF #-}
-- | Saturating resize operation, truncates for rounding
--
-- >>> 0.8125 :: SFixed 3 4
-- 0.8125
-- >>> resizeF (0.8125 :: SFixed 3 4) :: SFixed 2 3
-- 0.75
-- >>> 3.4 :: SFixed 3 4
-- 3.375
-- >>> resizeF (3.4 :: SFixed 3 4) :: SFixed 2 3
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
resizeF
  :: forall rep int1 frac1 int2 frac2
   . ResizeFC rep int1 frac1 int2 frac2
  => Fixed rep int1 frac1
  -> Fixed rep int2 frac2
resizeF (Fixed fRep) = Fixed sat
  where
    fMin  = minBound :: rep (int2 + frac2)
    fMax  = maxBound :: rep (int2 + frac2)
    argSZ = natVal (Proxy @(int1 + frac1))
    resSZ = natVal (Proxy @(int2 + frac2))

    argFracSZ = fromInteger (natVal (Proxy @frac1))
    resFracSZ = fromInteger (natVal (Proxy @frac2))

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
                            in if fRep >= 0
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
                            in if fRep >= 0
                                  then if shiftedR_masked == 0
                                          then shiftedR_resized
                                          else fMax
                                  else if shiftedR_masked == mask
                                          then shiftedR_resized
                                          else fMin

-- | Convert, at compile-time, a 'Double' /constant/ to a 'Fixed'-point /literal/.
-- The conversion saturates on overflow, and uses truncation as its rounding
-- method.
--
-- So when you type:
--
-- @
-- n = $$('fLit' pi) :: 'SFixed' 4 4
-- @
--
-- The compiler sees:
--
-- @
-- n = 'Fixed' (fromInteger 50) :: 'SFixed' 4 4
-- @
--
-- Upon evaluation you see that the value is rounded / truncated in accordance
-- to the fixed point representation:
--
-- >>> n
-- 3.125
--
-- Further examples:
--
-- >>> sin 0.5 :: Double
-- 0.479425538604203
-- >>> $$(fLit (sin 0.5)) :: SFixed 1 8
-- 0.4765625
-- >>> atan 0.2 :: Double
-- 0.19739555984988078
-- >>> $$(fLit (atan 0.2)) :: SFixed 1 8
-- 0.1953125
-- >>> $$(fLit (atan 0.2)) :: SFixed 1 20
-- 0.19739532470703125
fLit
  :: forall rep int frac size
   . ( size ~ (int + frac)
     , KnownNat frac
     , Bounded (rep size)
     , Integral (rep size) )
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
    shifted   = a * (2 ^ (natVal (Proxy @frac)))

-- | Convert, at run-time, a 'Double' to a 'Fixed'-point.
--
-- __NB__: this functions is /not/ synthesizable
--
-- = Creating data-files #creatingdatafiles#
--
-- An example usage of this function is for example to convert a data file
-- containing 'Double's to a data file with ASCI-encoded binary numbers to be
-- used by a synthesizable function like 'Clash.Prelude.ROM.File.asyncRomFile'.
-- For example, given a file @Data.txt@ containing:
--
-- @
-- 1.2 2.0 3.0 4.0
-- -1.0 -2.0 -3.5 -4.0
-- @
--
-- which we want to put in a ROM, interpreting them as @8.8@ signed fixed point
-- numbers. What we do is that we first create a conversion utility,
-- @createRomFile@, which uses 'fLitR':
--
-- @createRomFile.hs@:
--
-- @
-- module Main where
--
-- import Clash.Prelude
-- import System.Environment
-- import qualified Data.List as L
--
-- createRomFile
--   :: KnownNat n
--   => (Double -> BitVector n)
--   -> FilePath
--   -> FilePath
--   -> IO ()
-- createRomFile convert fileR fileW = do
--   f <- readFile fileR
--   let ds :: [Double]
--       ds = L.concat . (L.map . L.map) read . L.map words $ lines f
--       bvs = L.map (filter (/= '_') . show . convert) ds
--   writeFile fileW (unlines bvs)
--
-- toSFixed8_8 :: Double -> SFixed 8 8
-- toSFixed8_8 = 'fLitR'
--
-- main :: IO ()
-- main = do
--   [fileR,fileW] <- getArgs
--   createRomFile ('pack' . toSFixed8_8) fileR fileW
-- @
--
-- We then compile this to an executable:
--
-- @
-- \$ clash --make createRomFile.hs
-- @
--
-- We can then use this utility to convert our @Data.txt@ file which contains
-- 'Double's to a @Data.bin@ file which will containing the desired ASCI-encoded
-- binary data:
--
-- @
-- \$ ./createRomFile \"Data.txt\" \"Data.bin\"
-- @
--
-- Which results in a @Data.bin@ file containing:
--
-- @
-- 0000000100110011
-- 0000001000000000
-- 0000001100000000
-- 0000010000000000
-- 1111111100000000
-- 1111111000000000
-- 1111110010000000
-- 1111110000000000
-- @
--
-- We can then use this @Data.bin@ file in for our ROM:
--
-- @
-- romF :: Unsigned 3 -> Unsigned 3 -> SFixed 8 8
-- romF rowAddr colAddr = 'unpack'
--                      $ 'Clash.Prelude.ROM.File.asyncRomFile' d8 "Data.bin" ((rowAddr * 4) + colAddr)
-- @
--
-- And see that it works as expected:
--
-- @
-- __>>> romF 1 2__
-- -3.5
-- __>>> romF 0 0__
-- 1.19921875
-- @
--
-- == Using Template Haskell
--
-- For those of us who like to live on the edge, another option is to convert
-- our @Data.txt@ at compile-time using
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell Template Haskell>.
-- For this we first create a module @CreateRomFileTH.hs@:
--
-- @
-- module CreateRomFileTH (romDataFromFile) where
--
-- import Clash.Prelude
-- import qualified Data.List        as L
-- import Language.Haskell.TH        (ExpQ, litE, stringL)
-- import Language.Haskell.TH.Syntax (qRunIO)
--
-- createRomFile :: KnownNat n => (Double -> BitVector n)
--               -> FilePath -> FilePath -> IO ()
-- createRomFile convert fileR fileW = do
--   f <- readFile fileR
--   let ds :: [Double]
--       ds = L.concat . (L.map . L.map) read . L.map words $ lines f
--       bvs = L.map (filter (/= '_') . show . convert) ds
--   writeFile fileW (unlines bvs)
--
-- romDataFromFile :: KnownNat n => (Double -> BitVector n) -> String -> ExpQ
-- romDataFromFile convert fileR = do
--   let fileW = fileR L.++ ".bin"
--   bvF <- qRunIO (createRomFile convert fileR fileW)
--   litE (stringL fileW)
-- @
--
-- Instead of first converting @Data.txt@ to @Data.bin@, we will now use the
-- @romDataFromFile@ function to convert @Data.txt@ to a new file in the proper
-- format at compile-time of our new @romF'@ function:
--
-- @
-- import Clash.Prelude
-- import CreateRomFileTH
--
-- toSFixed8_8 :: Double -> SFixed 8 8
-- toSFixed8_8 = 'fLitR'
--
-- romF' :: Unsigned 3 -> Unsigned 3 -> SFixed 8 8
-- romF' rowAddr colAddr = unpack $
--   asyncRomFile d8
--                $(romDataFromFile (pack . toSFixed8_8) "Data.txt") -- Template Haskell splice
--                ((rowAddr * 4) + colAddr)
-- @
--
-- And see that it works just like the @romF@ function from earlier:
--
-- @
-- __>>> romF' 1 2__
-- -3.5
-- __>>> romF' 0 0__
-- 1.19921875
-- @
fLitR
  :: forall rep int frac size
   . ( size ~ (int + frac)
     , KnownNat frac
     , Bounded (rep size)
     , Integral (rep size))
  => Double
  -> Fixed rep int frac
fLitR a = Fixed (fromInteger sat)
  where
    rMax      = toInteger (maxBound :: rep size)
    rMin      = toInteger (minBound :: rep size)
    sat       = if truncated > rMax
                   then rMax
                   else if truncated < rMin
                           then rMin
                           else truncated
    truncated = truncate shifted :: Integer
    shifted   = a * (2 ^ (natVal (Proxy @frac)))

instance NumFixedC rep int frac => SaturatingNum (Fixed rep int frac) where
  satAdd w (Fixed a) (Fixed b) = Fixed (satAdd w a b)
  satSub  w (Fixed a) (Fixed b) = Fixed (satSub w a b)

  satMul SatWrap (Fixed a) (Fixed b) =
    let res  = a `mul` b
        sh   = fromInteger (natVal (Proxy @frac))
        res' = shiftR res sh
    in  Fixed (resize res')

  satMul SatBound (Fixed a) (Fixed b) =
    let res     = a `mul` b
        sh      = fromInteger (natVal (Proxy @frac))
        (rL,rR) = split res :: (BitVector int, BitVector (int + frac + frac))
    in  case isSigned a of
          True  -> let overflow = complement (reduceOr (pack (msb rR) ++# pack rL)) .|.
                                             reduceAnd (pack (msb rR) ++# pack rL)
                   in  case overflow of
                         1 -> unpack (resize (shiftR rR sh))
                         _ -> case msb rL of
                                0 -> maxBound
                                _ -> minBound
          False -> case rL of
                     0 -> unpack (resize (shiftR rR sh))
                     _ -> maxBound

  satMul SatZero (Fixed a) (Fixed b) =
    let res     = a `mul` b
        sh      = fromInteger (natVal (Proxy @frac))
        (rL,rR) = split res :: (BitVector int, BitVector (int + frac + frac))
    in  case isSigned a of
          True  -> let overflow = complement (reduceOr (pack (msb rR) ++# pack rL)) .|.
                                             reduceAnd (pack (msb rR) ++# pack rL)
                   in  case overflow of
                         1 -> unpack (resize (shiftR rR sh))
                         _ -> 0
          False -> case rL of
                     0 -> unpack (resize (shiftR rR sh))
                     _ -> 0

  satMul SatSymmetric (Fixed a) (Fixed b) =
    let res     = a `mul` b
        sh      = fromInteger (natVal (Proxy @frac))
        (rL,rR) = split res :: (BitVector int, BitVector (int + frac + frac))
    in  case isSigned a of
          True  -> let overflow = complement (reduceOr (pack (msb rR) ++# pack rL)) .|.
                                             reduceAnd (pack (msb rR) ++# pack rL)
                   in  case overflow of
                         1 -> unpack (resize (shiftR rR sh))
                         _ -> case msb rL of
                                0 -> maxBound
                                _ -> succ minBound
          False -> case rL of
                     0 -> unpack (resize (shiftR rR sh))
                     _ -> maxBound

-- | Constraint for the 'divide' function
type DivideC rep int1 frac1 int2 frac2
  = ( Resize   rep
    , Integral (rep (((int1 + frac2) + 1) + (int2 + frac1)))
    , Bits     (rep (((int1 + frac2) + 1) + (int2 + frac1)))
    , KnownNat int1
    , KnownNat frac1
    , KnownNat int2
    , KnownNat frac2
    )

-- | Constraint for the 'divide' function, specialized for 'SFixed'
type DivideSC int1 frac1 int2 frac2
  = ( KnownNat (((int1 + frac2) + 1) + (int2 + frac1))
    , KnownNat frac2
    , KnownNat int2
    , KnownNat frac1
    , KnownNat int1
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
divide
  :: DivideC rep int1 frac1 int2 frac2
  => Fixed rep int1 frac1
  -> Fixed rep int2 frac2
  -> Fixed rep (int1 + frac2 + 1) (int2 + frac1)
divide (Fixed fr1) fx2@(Fixed fr2) =
  let int2  = fromInteger (natVal (asIntProxy fx2))
      frac2 = fromInteger (natVal fx2)
      fr1'  = resize fr1
      fr2'  = resize fr2
      fr1SH = shiftL fr1' ((int2 + frac2))
      res   = fr1SH `quot` fr2'
  in  Fixed res

-- | Constraint for the 'Fractional' instance of 'Fixed'
type FracFixedC rep int frac
  = ( NumFixedC rep int frac
    , DivideC   rep int frac int frac
    , Integral  (rep (int + frac))
    , KnownNat  int
    , KnownNat  frac
    )

-- | Constraint for the 'Fractional' instance of 'SFixed'
type FracSFixedC int frac
  = ( NumSFixedC int frac
    , KnownNat ((int + frac + 1) + (int + frac))
    )

-- | Constraint for the 'Fractional' instance of 'UFixed'
type FracUFixedC int frac
  = FracSFixedC int frac

-- | The operators of this instance saturate on overflow, and use truncation as
-- the rounding method.
--
-- When used in a polymorphic setting, use the following
-- <Clash-Sized-Fixed.html#constraintsynonyms Constraint synonyms> for less
-- verbose type signatures:
--
-- * @'FracFixedC' frac rep size@ for: @'Fixed' frac rep size@
-- * @'FracSFixedC' int frac@     for: @'SFixed' int frac@
-- * @'FracUFixedC' int frac@     for: @'UFixed' int frac@
instance FracFixedC rep int frac => Fractional (Fixed rep int frac) where
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

instance (NumFixedC rep int frac, Integral (rep (int + frac))) =>
         Real (Fixed rep int frac) where
  toRational f@(Fixed fRep) = nom % denom
   where
     nF        = fracShift f
     denom     = 1 `shiftL` nF
     nom       = toInteger fRep

instance (FracFixedC rep int frac, NumFixedC rep int frac, Integral (rep (int + frac))) =>
         RealFrac (Fixed rep int frac) where
  properFraction f@(Fixed fRep) = (fromIntegral whole, fract)
    where
      whole = (fRep `shiftR` fracShift f) + offset
      fract = Fixed $ fRep - (whole `shiftL` fracShift f)
      offset = if f < 0 then 1 else 0
