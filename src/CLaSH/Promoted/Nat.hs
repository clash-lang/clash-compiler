{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE TypeOperators  #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Promoted.Nat
  ( -- * Singleton natural numbers
    -- ** Data type
    SNat (..)
    -- ** Construction
  , snatProxy
  , withSNat
  , snat
    -- ** Conversion
  , snatToInteger
    -- ** Arithmetic
  , addSNat, mulSNat, powSNat
    -- *** Partial
  , subSNat, divSNat, logBaseSNat
    -- *** Specialised
  , pow2SNat
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
  )
where

import Data.Reflection        (reifyNat)
import GHC.Integer            (smallInteger)
import GHC.Integer.Logarithms (integerLogBase#)
import GHC.TypeLits           (KnownNat, Nat, type (+), type (-), type (*),
                               type (^), natVal)
import Unsafe.Coerce          (unsafeCoerce)

{- $setup
>>> :set -XBinaryLiterals
>>> import CLaSH.Promoted.Nat.Literals (d789)
-}

-- | Singleton value for a type-level natural number 'n'
--
-- * "CLaSH.Promoted.Nat.Literals" contains a list of predefined 'SNat' literals
-- * "CLaSH.Promoted.Nat.TH" has functions to easily create large ranges of new
--   'SNat' literals
data SNat (n :: Nat) where
  SNat :: KnownNat n => SNat n

-- | Create a singleton literal for a type-level natural number
snat :: KnownNat n => SNat n
snat = SNat
{-# DEPRECATED snat "Use 'SNat' instead of 'snat'" #-}

-- | Create an @`SNat` n@ from a proxy for /n/
snatProxy :: KnownNat n => proxy n -> SNat n
snatProxy _ = SNat

instance Show (SNat n) where
  show p@SNat = 'd' : show (natVal p)

{-# INLINE withSNat #-}
-- | Supply a function with a singleton natural 'n' according to the context
withSNat :: KnownNat n => (SNat n -> a) -> a
withSNat f = f SNat

{-# INLINE snatToInteger #-}
-- | Reify the type-level 'Nat' @n@ to it's term-level 'Integer' representation.
snatToInteger :: SNat n -> Integer
snatToInteger p@SNat = natVal p

-- | Unary representation of a type-level natural
--
-- __NB__: Not synthesisable
data UNat :: Nat -> * where
  UZero :: UNat 0
  USucc :: UNat n -> UNat (n + 1)

instance KnownNat n => Show (UNat n) where
  show x = 'u':show (natVal x)

-- | Convert a singleton natural number to its unary representation
--
-- __NB__: Not synthesisable
toUNat :: SNat n -> UNat n
toUNat p@SNat = fromI (natVal p)
  where
    fromI :: Integer -> UNat m
    fromI 0 = unsafeCoerce UZero
    fromI n = unsafeCoerce (USucc (fromI (n - 1)))

-- | Convert a unary-encoded natural number to its singleton representation
--
-- __NB__: Not synthesisable
fromUNat :: UNat n -> SNat n
fromUNat UZero     = SNat :: SNat 0
fromUNat (USucc x) = addSNat (fromUNat x) (SNat :: SNat 1)

-- | Add two unary-encoded natural numbers
--
-- __NB__: Not synthesisable
addUNat :: UNat n -> UNat m -> UNat (n + m)
addUNat UZero     y     = y
addUNat x         UZero = x
addUNat (USucc x) y     = USucc (addUNat x y)

-- | Multiply two unary-encoded natural numbers
--
-- __NB__: Not synthesisable
mulUNat :: UNat n -> UNat m -> UNat (n * m)
mulUNat UZero      _     = UZero
mulUNat _          UZero = UZero
mulUNat (USucc x) y      = addUNat y (mulUNat x y)

-- | Power of two unary-encoded natural numbers
--
-- __NB__: Not synthesisable
powUNat :: UNat n -> UNat m -> UNat (n ^ m)
powUNat _ UZero     = USucc UZero
powUNat x (USucc y) = mulUNat x (powUNat x y)

-- | Predecessor of a unary-encoded natural number
--
-- __NB__: Not synthesisable
predUNat :: UNat (n+1) -> UNat n
predUNat (USucc x) = x

-- | Subtract two unary-encoded natural numbers
--
-- __NB__: Not synthesisable
subUNat :: UNat (m+n) -> UNat n -> UNat m
subUNat x         UZero     = x
subUNat (USucc x) (USucc y) = subUNat x y
subUNat UZero     _         = error "impossible: 0 + (n + 1) ~ 0"

-- | Add two singleton natural numbers
addSNat :: SNat a -> SNat b -> SNat (a+b)
addSNat SNat SNat = SNat
{-# INLINE addSNat #-}

-- | Subtract two singleton natural numbers
subSNat :: SNat (a+b) -> SNat b -> SNat a
subSNat SNat SNat = SNat
{-# INLINE subSNat #-}

-- | Multiply two singleton natural numbers
mulSNat :: SNat a -> SNat b -> SNat (a*b)
mulSNat SNat SNat = SNat
{-# INLINE mulSNat #-}

-- | Power of two singleton natural numbers
powSNat :: SNat a -> SNat b -> SNat (a^b)
powSNat SNat SNat = SNat
{-# NOINLINE powSNat #-}

-- | Division of two singleton natural numbers
--
-- __NB__: Only works when the dividend is an integer multiple of the
-- divisor.
divSNat :: SNat (a*(b+1)) -> SNat (b+1) -> SNat a
divSNat x y = reifyNat (snatToInteger x `div` snatToInteger y)
            $ \p -> unsafeCoerce (snatProxy p)
{-# NOINLINE divSNat #-}

-- | Logarithm of a natural number
--
-- __NB__: Only works when the argument is a power of the base
logBaseSNat :: SNat (a+2) -- ^ Base
            -> SNat ((a+2)^b)
            -> SNat b
logBaseSNat x y =
  reifyNat (smallInteger (integerLogBase# (snatToInteger x) (snatToInteger y)))
  $ \p -> unsafeCoerce (snatProxy p)
{-# NOINLINE logBaseSNat #-}

-- | Power of two of a singleton natural number
pow2SNat :: SNat a -> SNat (2^a)
pow2SNat SNat = SNat
{-# INLINE pow2SNat #-}

-- | Base-2 encoded natural number
--
--    * __NB__: The LSB is the left/outer-most constructor:
--    * __NB__: Not synthesisable
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
--      __B0__ :: 'BNat' n -> 'BNat' (2 '*' n)
--      @
--
-- * Append a one (/1/):
--
--      @
--      __B1__ :: 'BNat' n -> 'BNat' ((2 '*' n) '+' 1)
--      @
data BNat :: Nat -> * where
  BT :: BNat 0
  B0 :: BNat n -> BNat (2*n)
  B1 :: BNat n -> BNat ((2*n) + 1)

instance KnownNat n => Show (BNat n) where
  show x = 'b':show (natVal x)

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
-- __NB__: Not synthesisable
toBNat :: SNat n -> BNat n
toBNat s@SNat = toBNat' (natVal s)
  where
    toBNat' :: Integer -> BNat m
    toBNat' 0 = unsafeCoerce BT
    toBNat' n = case n `divMod` 2 of
      (n',1) -> unsafeCoerce (B1 (toBNat' n'))
      (n',_) -> unsafeCoerce (B0 (toBNat' n'))

-- | Convert a base-2 encoded natural number to its singleton representation
--
-- __NB__: Not synthesisable
fromBNat :: BNat n -> SNat n
fromBNat BT     = SNat :: SNat 0
fromBNat (B0 x) = mulSNat (SNat :: SNat 2) (fromBNat x)
fromBNat (B1 x) = addSNat (mulSNat (SNat :: SNat 2) (fromBNat x))
                          (SNat :: SNat 1)

-- | Add two base-2 encoded natural numbers
--
-- __NB__: Not synthesisable
addBNat :: BNat n -> BNat m -> BNat (n+m)
addBNat (B0 a) (B0 b) = B0 (addBNat a b)
addBNat (B0 a) (B1 b) = B1 (addBNat a b)
addBNat (B1 a) (B0 b) = B1 (addBNat a b)
addBNat (B1 a) (B1 b) = B0 (succBNat (addBNat a b))
addBNat BT     b      = b
addBNat a      BT     = a

-- | Multiply two base-2 encoded natural numbers
--
-- __NB__: Not synthesisable
mulBNat :: BNat n -> BNat m -> BNat (n*m)
mulBNat BT      _  = BT
mulBNat _       BT = BT
mulBNat (B0 a)  b  = B0 (mulBNat a b)
mulBNat (B1 a)  b  = addBNat (B0 (mulBNat a b)) b

-- | Power of two base-2 encoded natural numbers
--
-- __NB__: Not synthesisable
powBNat :: BNat n -> BNat m -> BNat (n^m)
powBNat _  BT      = B1 BT
powBNat a  (B0 b)  = let z = powBNat a b
                     in  mulBNat z z
powBNat a  (B1 b)  = let z = powBNat a b
                     in  mulBNat a (mulBNat z z)

-- | Successor of a base-2 encoded natural number
--
-- __NB__: Not synthesisable
succBNat :: BNat n -> BNat (n+1)
succBNat BT     = B1 BT
succBNat (B0 a) = B1 a
succBNat (B1 a) = B0 (succBNat a)

-- | Predecessor of a base-2 encoded natural number
--
-- __NB__: Not synthesisable
predBNat :: BNat (n+1) -> (BNat n)
predBNat (B1 a) = case stripZeros a of
  BT -> BT
  a' -> B0 a'
predBNat (B0 x)  = B1 (go x)
  where
    go :: BNat m -> BNat (m-1)
    go (B1 a) = case stripZeros a of
      BT -> BT
      a' -> B0 a'
    go (B0 a)  = B1 (go a)
    go BT      = error "impossible: 0 ~ 0 - 1"

-- | Divide a base-2 encoded natural number by 2
--
-- __NB__: Not synthesisable
div2BNat :: BNat (2*n) -> BNat n
div2BNat BT     = BT
div2BNat (B0 x) = x
div2BNat (B1 _) = error "impossible: 2*n ~ 2*n+1"

-- | Subtract 1 and divide a base-2 encoded natural number by 2
--
-- __NB__: Not synthesisable
div2Sub1BNat :: BNat (2*n+1) -> BNat n
div2Sub1BNat (B1 x) = x
div2Sub1BNat _      = error "impossible: 2*n+1 ~ 2*n"

-- | Get the log2 of a base-2 encoded natural number
--
-- __NB__: Not synthesisable
log2BNat :: BNat (2^n) -> BNat n
log2BNat (B1 x) = case stripZeros x of
  BT -> BT
  _  -> error "impossible: 2^n ~ 2x+1"
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
-- __NB__: Not synthesisable
stripZeros :: BNat n -> BNat n
stripZeros BT      = BT
stripZeros (B1 x)  = B1 (stripZeros x)
stripZeros (B0 BT) = BT
stripZeros (B0 x)  = case stripZeros x of
  BT -> BT
  k  -> B0 k
