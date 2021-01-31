{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016     , Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Promoted.Nat
  ( -- * Singleton natural numbers
    -- ** Data type
    SNat (..)
    -- ** Construction
  , snatProxy
  , withSNat
    -- ** Conversion
  , snatToInteger, snatToNatural, snatToNum
    -- ** Conversion (ambiguous types)
  , natToInteger, natToNatural, natToNum
    -- ** Arithmetic
  , addSNat, mulSNat, powSNat, minSNat, maxSNat, succSNat
    -- *** Partial
  , subSNat, divSNat, modSNat, flogBaseSNat, clogBaseSNat, logBaseSNat, predSNat
    -- *** Specialised
  , pow2SNat
    -- *** Comparison
  , SNatLE (..), compareSNat
    -- * Unary/Peano-encoded natural numbers
    -- ** Data type
  , UNat (..)
    -- ** Construction
  , toUNat
    -- ** Conversion
  , fromUNat
    -- ** Arithmetic
  , addUNat, mulUNat, powUNat
    -- *** Partial
  , predUNat, subUNat
    -- * Base-2 encoded natural numbers
    -- ** Data type
  , BNat (..)
    -- ** Construction
  , toBNat
    -- ** Conversion
  , fromBNat
    -- ** Pretty printing base-2 encoded natural numbers
  , showBNat
    -- ** Arithmetic
  , succBNat, addBNat, mulBNat, powBNat
    -- *** Partial
  , predBNat, div2BNat, div2Sub1BNat, log2BNat
    -- ** Normalisation
  , stripZeros
    -- * Constraints on natural numbers
  , leToPlus
  , leToPlusKN
  )
where

import Data.Kind          (Type)
import GHC.Show           (appPrec)
import GHC.TypeLits       (KnownNat, Nat, type (+), type (-), type (*),
                           type (^), type (<=), natVal)
import GHC.TypeLits.Extra (CLog, FLog, Div, Log, Mod, Min, Max)
import GHC.Natural        (naturalFromInteger)
import Language.Haskell.TH (appT, conT, litT, numTyLit, sigE)
import Language.Haskell.TH.Syntax (Lift (..))
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Compat
#endif
import Numeric.Natural    (Natural)
import Unsafe.Coerce      (unsafeCoerce)
import Clash.XException   (ShowX (..), showsPrecXWith)

{- $setup
>>> :set -XBinaryLiterals
>>> import Clash.Promoted.Nat.Literals (d789)
-}

-- | Singleton value for a type-level natural number @n@
--
-- * "Clash.Promoted.Nat.Literals" contains a list of predefined 'SNat' literals
-- * "Clash.Promoted.Nat.TH" has functions to easily create large ranges of new
--   'SNat' literals
data SNat (n :: Nat) where
  SNat :: KnownNat n => SNat n

instance Lift (SNat n) where
  lift s = sigE [| SNat |]
                (appT (conT ''SNat) (litT $ numTyLit (snatToInteger s)))
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedFromUntyped
#endif

-- | Create an @`SNat` n@ from a proxy for /n/
snatProxy :: KnownNat n => proxy n -> SNat n
snatProxy _ = SNat

instance Show (SNat n) where
  showsPrec d p@SNat | n <= 1024 = showChar 'd' . shows n
                     | otherwise = showParen (d > appPrec) $
                                     showString "SNat @" . shows n
   where
    n = snatToInteger p

instance ShowX (SNat n) where
  showsPrecX = showsPrecXWith showsPrec

{-# INLINE withSNat #-}
-- | Supply a function with a singleton natural @n@ according to the context
withSNat :: KnownNat n => (SNat n -> a) -> a
withSNat f = f SNat

-- | Same as 'snatToInteger' and 'GHC.TypeLits.natVal', but doesn't take term
-- arguments. Example usage:
--
-- >>> natToInteger @5
-- 5
natToInteger :: forall n . KnownNat n => Integer
natToInteger = snatToInteger (SNat @n)
{-# INLINE natToInteger #-}

-- | Reify the type-level 'Nat' @n@ to it's term-level 'Integer' representation.
snatToInteger :: SNat n -> Integer
snatToInteger p@SNat = natVal p
{-# INLINE snatToInteger #-}

-- | Same as 'snatToNatural' and 'GHC.TypeNats.natVal', but doesn't take term
-- arguments. Example usage:
--
-- >>> natToNatural @5
-- 5
natToNatural :: forall n . KnownNat n => Natural
natToNatural = snatToNatural (SNat @n)
{-# INLINE natToNatural #-}

-- | Reify the type-level 'Nat' @n@ to it's term-level 'Natural'.
snatToNatural :: SNat n -> Natural
snatToNatural = naturalFromInteger . snatToInteger
{-# INLINE snatToNatural #-}

-- | Same as 'snatToNum', but doesn't take term arguments. Example usage:
--
-- >>> natToNum @5 @Int
-- 5
natToNum :: forall n a . (Num a, KnownNat n) => a
natToNum = snatToNum (SNat @n)
{-# INLINE natToNum #-}

-- | Reify the type-level 'Nat' @n@ to it's term-level 'Num'ber.
snatToNum :: forall a n . Num a => SNat n -> a
snatToNum p@SNat = fromInteger (snatToInteger p)
{-# INLINE snatToNum #-}

-- | Unary representation of a type-level natural
--
-- __NB__: Not synthesizable
data UNat :: Nat -> Type where
  UZero :: UNat 0
  USucc :: UNat n -> UNat (n + 1)

instance KnownNat n => Show (UNat n) where
  show x = 'u':show (natVal x)

instance KnownNat n => ShowX (UNat n) where
  showsPrecX = showsPrecXWith showsPrec

-- | Convert a singleton natural number to its unary representation
--
-- __NB__: Not synthesizable
toUNat :: forall n . SNat n -> UNat n
toUNat p@SNat = fromI @n (snatToInteger p)
  where
    fromI :: forall m . Integer -> UNat m
    fromI 0 = unsafeCoerce @(UNat 0) @(UNat m) UZero
    fromI n = unsafeCoerce @(UNat ((m-1)+1)) @(UNat m) (USucc (fromI @(m-1) (n - 1)))

-- | Convert a unary-encoded natural number to its singleton representation
--
-- __NB__: Not synthesizable
fromUNat :: UNat n -> SNat n
fromUNat UZero     = SNat :: SNat 0
fromUNat (USucc x) = addSNat (fromUNat x) (SNat :: SNat 1)

-- | Add two unary-encoded natural numbers
--
-- __NB__: Not synthesizable
addUNat :: UNat n -> UNat m -> UNat (n + m)
addUNat UZero     y     = y
addUNat x         UZero = x
addUNat (USucc x) y     = USucc (addUNat x y)

-- | Multiply two unary-encoded natural numbers
--
-- __NB__: Not synthesizable
mulUNat :: UNat n -> UNat m -> UNat (n * m)
mulUNat UZero      _     = UZero
mulUNat _          UZero = UZero
mulUNat (USucc x) y      = addUNat y (mulUNat x y)

-- | Power of two unary-encoded natural numbers
--
-- __NB__: Not synthesizable
powUNat :: UNat n -> UNat m -> UNat (n ^ m)
powUNat _ UZero     = USucc UZero
powUNat x (USucc y) = mulUNat x (powUNat x y)

-- | Predecessor of a unary-encoded natural number
--
-- __NB__: Not synthesizable
predUNat :: UNat (n+1) -> UNat n
predUNat (USucc x) = x
predUNat UZero     =
  error "predUNat: impossible: 0 minus 1, -1 is not a natural number"

-- | Subtract two unary-encoded natural numbers
--
-- __NB__: Not synthesizable
subUNat :: UNat (m+n) -> UNat n -> UNat m
subUNat x         UZero     = x
subUNat (USucc x) (USucc y) = subUNat x y
subUNat UZero     _         = error "subUNat: impossible: 0 + (n + 1) ~ 0"

-- | Predecessor of a singleton natural number
predSNat :: SNat (a+1) -> SNat (a)
predSNat SNat = SNat
{-# INLINE predSNat #-}

-- | Successor of a singleton natural number
succSNat :: SNat a -> SNat (a+1)
succSNat SNat = SNat
{-# INLINE succSNat #-}

-- | Add two singleton natural numbers
addSNat :: SNat a -> SNat b -> SNat (a+b)
addSNat SNat SNat = SNat
{-# INLINE addSNat #-}
infixl 6 `addSNat`

-- | Subtract two singleton natural numbers
subSNat :: SNat (a+b) -> SNat b -> SNat a
subSNat SNat SNat = SNat
{-# INLINE subSNat #-}
infixl 6 `subSNat`

-- | Multiply two singleton natural numbers
mulSNat :: SNat a -> SNat b -> SNat (a*b)
mulSNat SNat SNat = SNat
{-# INLINE mulSNat #-}
infixl 7 `mulSNat`

-- | Power of two singleton natural numbers
powSNat :: SNat a -> SNat b -> SNat (a^b)
powSNat SNat SNat = SNat
{-# NOINLINE powSNat #-}
infixr 8 `powSNat`

-- | Division of two singleton natural numbers
divSNat :: (1 <= b) => SNat a -> SNat b -> SNat (Div a b)
divSNat SNat SNat = SNat
{-# INLINE divSNat #-}
infixl 7 `divSNat`

-- | Modulo of two singleton natural numbers
modSNat :: (1 <= b) => SNat a -> SNat b -> SNat (Mod a b)
modSNat SNat SNat = SNat
{-# INLINE modSNat #-}
infixl 7 `modSNat`

minSNat :: SNat a -> SNat b -> SNat (Min a b)
minSNat SNat SNat = SNat

maxSNat :: SNat a -> SNat b -> SNat (Max a b)
maxSNat SNat SNat = SNat

-- | Floor of the logarithm of a natural number
flogBaseSNat :: (2 <= base, 1 <= x)
             => SNat base -- ^ Base
             -> SNat x
             -> SNat (FLog base x)
flogBaseSNat SNat SNat = SNat
{-# NOINLINE flogBaseSNat #-}

-- | Ceiling of the logarithm of a natural number
clogBaseSNat :: (2 <= base, 1 <= x)
             => SNat base -- ^ Base
             -> SNat x
             -> SNat (CLog base x)
clogBaseSNat SNat SNat = SNat
{-# NOINLINE clogBaseSNat #-}

-- | Exact integer logarithm of a natural number
--
-- __NB__: Only works when the argument is a power of the base
logBaseSNat :: (FLog base x ~ CLog base x)
            => SNat base -- ^ Base
            -> SNat x
            -> SNat (Log base x)
logBaseSNat SNat SNat = SNat
{-# NOINLINE logBaseSNat #-}

-- | Power of two of a singleton natural number
pow2SNat :: SNat a -> SNat (2^a)
pow2SNat SNat = SNat
{-# INLINE pow2SNat #-}

-- | Ordering relation between two Nats
data SNatLE a b where
  SNatLE :: forall a b . a <= b => SNatLE a b
  SNatGT :: forall a b . (b+1) <= a => SNatLE a b

-- | Get an ordering relation between two SNats
compareSNat :: forall a b . SNat a -> SNat b -> SNatLE a b
compareSNat a b =
  if snatToInteger a <= snatToInteger b
     then unsafeCoerce (SNatLE @0 @0)
     else unsafeCoerce (SNatGT @1 @0)

-- | Base-2 encoded natural number
--
--    * __NB__: The LSB is the left/outer-most constructor:
--    * __NB__: Not synthesizable
--
-- >>> B0 (B1 (B1 BT))
-- b6
--
-- == Constructors
--
-- * Starting/Terminating element:
--
--      @
--      __BT__ :: 'BNat' 0
--      @
--
-- * Append a zero (/0/):
--
--      @
--      __B0__ :: 'BNat' n -> 'BNat' (2 'GHC.TypeNats.*' n)
--      @
--
-- * Append a one (/1/):
--
--      @
--      __B1__ :: 'BNat' n -> 'BNat' ((2 'GHC.TypeNats.*' n) 'GHC.TypeNats.+' 1)
--      @
data BNat :: Nat -> Type where
  BT :: BNat 0
  B0 :: BNat n -> BNat (2*n)
  B1 :: BNat n -> BNat ((2*n) + 1)

instance KnownNat n => Show (BNat n) where
  show x = 'b':show (natVal x)

instance KnownNat n => ShowX (BNat n) where
  showsPrecX = showsPrecXWith showsPrec

-- | Show a base-2 encoded natural as a binary literal
--
-- __NB__: The LSB is shown as the right-most bit
--
-- >>> d789
-- d789
-- >>> toBNat d789
-- b789
-- >>> showBNat (toBNat d789)
-- "0b1100010101"
-- >>> 0b1100010101 :: Integer
-- 789
showBNat :: BNat n -> String
showBNat = go []
  where
    go :: String -> BNat m -> String
    go xs BT  = "0b" ++ xs
    go xs (B0 x) = go ('0':xs) x
    go xs (B1 x) = go ('1':xs) x

-- | Convert a singleton natural number to its base-2 representation
--
-- __NB__: Not synthesizable
toBNat :: SNat n -> BNat n
toBNat s@SNat = toBNat' (snatToInteger s)
  where
    toBNat' :: forall m . Integer -> BNat m
    toBNat' 0 = unsafeCoerce BT
    toBNat' n = case n `divMod` 2 of
      (n',1) -> unsafeCoerce (B1 (toBNat' @(Div (m-1) 2) n'))
      (n',_) -> unsafeCoerce (B0 (toBNat' @(Div m 2) n'))

-- | Convert a base-2 encoded natural number to its singleton representation
--
-- __NB__: Not synthesizable
fromBNat :: BNat n -> SNat n
fromBNat BT     = SNat :: SNat 0
fromBNat (B0 x) = mulSNat (SNat :: SNat 2) (fromBNat x)
fromBNat (B1 x) = addSNat (mulSNat (SNat :: SNat 2) (fromBNat x))
                          (SNat :: SNat 1)

-- | Add two base-2 encoded natural numbers
--
-- __NB__: Not synthesizable
addBNat :: BNat n -> BNat m -> BNat (n+m)
addBNat (B0 a) (B0 b) = B0 (addBNat a b)
addBNat (B0 a) (B1 b) = B1 (addBNat a b)
addBNat (B1 a) (B0 b) = B1 (addBNat a b)
addBNat (B1 a) (B1 b) = B0 (succBNat (addBNat a b))
addBNat BT     b      = b
addBNat a      BT     = a

-- | Multiply two base-2 encoded natural numbers
--
-- __NB__: Not synthesizable
mulBNat :: BNat n -> BNat m -> BNat (n*m)
mulBNat BT      _  = BT
mulBNat _       BT = BT
mulBNat (B0 a)  b  = B0 (mulBNat a b)
mulBNat (B1 a)  b  = addBNat (B0 (mulBNat a b)) b

-- | Power of two base-2 encoded natural numbers
--
-- __NB__: Not synthesizable
powBNat :: BNat n -> BNat m -> BNat (n^m)
powBNat _  BT      = B1 BT
powBNat a  (B0 b)  = let z = powBNat a b
                     in  mulBNat z z
powBNat a  (B1 b)  = let z = powBNat a b
                     in  mulBNat a (mulBNat z z)

-- | Successor of a base-2 encoded natural number
--
-- __NB__: Not synthesizable
succBNat :: BNat n -> BNat (n+1)
succBNat BT     = B1 BT
succBNat (B0 a) = B1 a
succBNat (B1 a) = B0 (succBNat a)

-- | Predecessor of a base-2 encoded natural number
--
-- __NB__: Not synthesizable
predBNat :: (1 <= n) => BNat n -> BNat (n-1)
predBNat (B1 a) = case stripZeros a of
  BT -> BT
  a' -> B0 a'
predBNat (B0 x) = B1 (predBNat x)

-- | Divide a base-2 encoded natural number by 2
--
-- __NB__: Not synthesizable
div2BNat :: BNat (2*n) -> BNat n
div2BNat BT     = BT
div2BNat (B0 x) = x
div2BNat (B1 _) = error "div2BNat: impossible: 2*n ~ 2*n+1"

-- | Subtract 1 and divide a base-2 encoded natural number by 2
--
-- __NB__: Not synthesizable
div2Sub1BNat :: BNat (2*n+1) -> BNat n
div2Sub1BNat (B1 x) = x
div2Sub1BNat _      = error "div2Sub1BNat: impossible: 2*n+1 ~ 2*n"

-- | Get the log2 of a base-2 encoded natural number
--
-- __NB__: Not synthesizable
log2BNat :: BNat (2^n) -> BNat n
log2BNat BT = error "log2BNat: log2(0) not defined"
log2BNat (B1 x) = case stripZeros x of
  BT -> BT
  _  -> error "log2BNat: impossible: 2^n ~ 2x+1"
log2BNat (B0 x) = succBNat (log2BNat x)

-- | Strip non-contributing zero's from a base-2 encoded natural number
--
-- >>> B1 (B0 (B0 (B0 BT)))
-- b1
-- >>> showBNat (B1 (B0 (B0 (B0 BT))))
-- "0b0001"
-- >>> showBNat (stripZeros (B1 (B0 (B0 (B0 BT)))))
-- "0b1"
-- >>> stripZeros (B1 (B0 (B0 (B0 BT))))
-- b1
--
-- __NB__: Not synthesizable
stripZeros :: BNat n -> BNat n
stripZeros BT      = BT
stripZeros (B1 x)  = B1 (stripZeros x)
stripZeros (B0 BT) = BT
stripZeros (B0 x)  = case stripZeros x of
  BT -> BT
  k  -> B0 k

-- | Change a function that has an argument with an @(n ~ (k + m))@ constraint to a
-- function with an argument that has an @(k <= n)@ constraint.
--
-- === __Examples__
--
-- Example 1
--
-- @
-- f :: Index (n+1) -> Index (n + 1) -> Bool
--
-- g :: forall n. (1 'GHC.TypeNats.<=' n) => Index n -> Index n -> Bool
-- g a b = 'leToPlus' \@1 \@n (f a b)
-- @
--
-- Example 2
--
-- @
-- head :: Vec (n + 1) a -> a
--
-- head' :: forall n a. (1 'GHC.TypeNats.<=' n) => Vec n a -> a
-- head' = 'leToPlus' @1 @n head
-- @
leToPlus
  :: forall (k :: Nat) (n :: Nat) r
   . ( k <= n
     )
  => (forall m . (n ~ (k + m)) => r)
  -- ^ Context with the @(n ~ (k + m))@ constraint
  -> r
leToPlus r = r @(n - k)
{-# INLINE leToPlus #-}

-- | Same as 'leToPlus' with added 'KnownNat' constraints
leToPlusKN
  :: forall (k :: Nat) (n :: Nat) r
   . ( k <= n
     , KnownNat k
     , KnownNat n
     )
  => (forall m . (n ~ (k + m), KnownNat m) => r)
  -- ^ Context with the @(n ~ (k + m))@ constraint
  -> r
leToPlusKN r = r @(n - k)
{-# INLINE leToPlusKN #-}
