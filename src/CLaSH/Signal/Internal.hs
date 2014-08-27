{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module CLaSH.Signal.Internal
  ( -- * Datatypes
    Clock (..)
  , SClock (..)
  , CSignal (..)
    -- * Basic circuits
  , register#
    -- * Type classes
    -- ** 'Eq'-like
  , (==&), (/=&)
    -- ** 'Ord'-like
  , compare1, (<&), (<=&), (>=&), (>&)
    -- ** 'Functor'
  , mapSignal#
    -- ** 'Applicative'
  , signal#
  , appSignal#
    -- ** 'Foldable'
  , foldr#
    -- ** 'Traversable'
  , traverse#
    -- ** 'Enum'-like
  , fromEnum1
    -- ** 'Rational'-like
  , toRational1
    -- ** 'Integral'-like
  , toInteger1
    -- ** 'Bits'-like
  , testBit1
  , popCount1
  , shift1
  , rotate1
  , setBit1
  , clearBit1
  , shiftL1
  , unsafeShiftL1
  , shiftR1
  , unsafeShiftR1
  , rotateL1
  , rotateR1
  )
where

import Control.Applicative        (Applicative (..), (<$>), liftA2, liftA3)
import Data.Bits                  (Bits (..), FiniteBits (..))
import Data.Default               (Default (..))
import Data.Foldable              (Foldable (..))
import Data.Traversable           (Traversable (..))
import GHC.TypeLits               (Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))

import CLaSH.Class.Num            (Add (..), Mult (..), SaturatingNum (..))
import CLaSH.Promoted.Nat         (SNat)
import CLaSH.Promoted.Symbol      (SSymbol)

-- | A clock with a name ('Symbol') and period ('Nat')
data Clock = Clk Symbol Nat

-- | Singleton value for a type-level 'Clock' with the given @name@ and @period@
data SClock (clk :: Clock)
  where
    SClock :: SSymbol name -> SNat period -> SClock (Clk name period)

infixr 5 :-
-- | A synchronized signal with samples of type @a@, explicitly synchronized to
-- a clock @clk@
--
-- __NB__: The constructor, @(':-')@, is __not__ synthesisable.
data CSignal (clk :: Clock) a = a :- CSignal clk a

instance Show a => Show (CSignal clk a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (CSignal clk a) where
  lift ~(x :- _) = [| signal# x |]

instance Default a => Default (CSignal clk a) where
  def = signal# def

instance Functor (CSignal clk) where
  fmap = mapSignal#

{-# NOINLINE mapSignal# #-}
mapSignal# :: (a -> b) -> CSignal clk a -> CSignal clk b
mapSignal# f (a :- as) = f a :- mapSignal# f as

instance Applicative (CSignal clk) where
  pure  = signal#
  (<*>) = appSignal#

{-# NOINLINE signal# #-}
signal# :: a -> CSignal clk a
signal# a = let s = a :- s in s

{-# NOINLINE appSignal# #-}
appSignal# :: CSignal clk (a -> b) -> CSignal clk a -> CSignal clk b
appSignal# (f :- fs) ~(a :- as) = f a :- appSignal# fs as

instance Num a => Num (CSignal clk a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = signal# . fromInteger

-- | __NB__: Not synthesisable
--
-- Is used in conversions to lists
instance Foldable (CSignal clk) where
  foldr = foldr#

{-# NOINLINE foldr# #-}
-- | In \"@'foldr# f z s@\" the @z@ element never used.
--
-- __NB__: Not synthesisable
foldr# :: (a -> b -> b) -> b -> CSignal clk a -> b
foldr# f z (a :- s) = a `f` (foldr# f z s)

instance Traversable (CSignal clk) where
  traverse = traverse#

{-# NOINLINE traverse# #-}
-- | __NB__: Not synthesisable
traverse# :: Applicative f => (a -> f b) -> CSignal clk a -> f (CSignal clk b)
traverse# f (a :- s) = (:-) <$> f a <*> traverse# f s

{-# NOINLINE register# #-}
register# :: SClock clk -> a -> CSignal clk a -> CSignal clk a
register# _ i s = i :- s

instance Bounded a => Bounded (CSignal clk a) where
  minBound = signal# minBound
  maxBound = signal# maxBound

instance Add a b => Add (CSignal clk a) (CSignal clk b) where
  type AResult (CSignal clk a) (CSignal clk b) = CSignal clk (AResult a b)
  plus  = liftA2 plus
  minus = liftA2 minus

instance Mult a b => Mult (CSignal clk a) (CSignal clk b) where
  type MResult (CSignal clk a) (CSignal clk b) = CSignal clk (MResult a b)
  mult = liftA2 mult

instance SaturatingNum a => SaturatingNum (CSignal clk a) where
  satPlus s = liftA2 (satPlus s)
  satMin  s = liftA2 (satMin s)
  satMult s = liftA2 (satMult s)

-- | __WARNING__: ('==') and ('/=') are undefined, use ('==&') and ('/=&')
-- instead
instance Eq (CSignal clk a) where
  (==) = error "(==)' undefined for 'CSignal', use '(==&)' instead"
  (/=) = error "(/=)' undefined for 'CSignal', use '(/=&)' instead"

infix 4 ==&
-- | Version of ('==') that returns a 'CSignal' of 'Bool'
(==&) :: Eq a => CSignal clk a -> CSignal clk a -> CSignal clk Bool
(==&) = liftA2 (==)

infix 4 /=&
-- | Version of ('/=') that returns a 'CSignal' of 'Bool'
(/=&) :: Eq a => CSignal clk a -> CSignal clk a -> CSignal clk Bool
(/=&) = liftA2 (/=)

-- | __WARNING__: 'compare', ('<'), ('>='), ('>'), and ('<=') are
-- undefined, use 'compare1', ('<&'), ('>=&'), ('>&'), and ('<=&') instead
instance Ord a => Ord (CSignal clk a) where
  compare = error "'compare' undefined for 'CSignal', use 'compare1' instead"
  (<)     = error "'(<)' undefined for 'CSignal', use '(<&)' instead"
  (>=)    = error "'(>=)' undefined for 'CSignal', use '(>=&)' instead"
  (>)     = error "'(>)' undefined for 'CSignal', use '(>&)' instead"
  (<=)    = error "'(<=)' undefined for 'CSignal', use '(<=&)' instead"
  max     = liftA2 max
  min     = liftA2 min

-- | Version of 'compare' that returns a 'CSignal' of 'Ordering'
compare1 :: Ord a => CSignal clk a -> CSignal clk a -> CSignal clk Ordering
compare1 = liftA2 compare

infix 4 <&
-- | Version of ('<') that returns a 'CSignal' of 'Bool'
(<&) :: Ord a => CSignal clk a -> CSignal clk a -> CSignal clk Bool
(<&) = liftA2 (<)

infix 4 <=&
-- | Version of ('<=') that returns a 'CSignal' of 'Bool'
(<=&) :: Ord a => CSignal clk a -> CSignal clk a -> CSignal clk Bool
(<=&) = liftA2 (<=)

infix 4 >&
-- | Version of ('>') that returns a 'CSignal' of 'Bool'
(>&) :: Ord a => CSignal clk a -> CSignal clk a -> CSignal clk Bool
(>&) = liftA2 (>)

infix 4 >=&
-- | Version of ('>=') that returns a 'CSignal' of 'Bool'
(>=&) :: Ord a => CSignal clk a -> CSignal clk a -> CSignal clk Bool
(>=&) = liftA2 (>=)

-- | __WARNING__: 'fromEnum' is undefined, use 'fromEnum1' instead
instance Enum a => Enum (CSignal clk a) where
  succ           = fmap succ
  pred           = fmap pred
  toEnum         = signal# . toEnum
  fromEnum       = error "'fromEnum' undefined for 'CSignal', use 'fromEnum1'"
  enumFrom       = sequenceA . fmap enumFrom
  enumFromThen   = (sequenceA .) . liftA2 enumFromThen
  enumFromTo     = (sequenceA .) . liftA2 enumFromTo
  enumFromThenTo = ((sequenceA .) .) . liftA3 enumFromThenTo

-- | Version of 'fromEnum' that returns a 'CSignal' of 'Int'
fromEnum1 :: Enum a => CSignal clk a -> CSignal clk Int
fromEnum1 = fmap fromEnum

-- | __WARNING__: 'toRational' is undefined, use 'toRational1' instead
instance (Num a, Ord a) => Real (CSignal clk a) where
  toRational = error "'toRational' undefined for 'CSignal', use 'toRational1'"

-- | Version of 'toRational' that returns a 'CSignal' of 'Rational'
toRational1 :: Real a => CSignal clk a -> CSignal clk Rational
toRational1 = fmap toRational

-- | __WARNING__: 'toInteger' is undefined, use 'toInteger1' instead
instance Integral a => Integral (CSignal clk a) where
  quot        = liftA2 quot
  rem         = liftA2 rem
  div         = liftA2 div
  mod         = liftA2 mod
  quotRem a b = (quot a b, rem a b)
  divMod a b  = (div a b, mod a b)
  toInteger   = error "'toInteger' undefined for 'CSignal', use 'toInteger1'"

-- | Version of 'toRational' that returns a 'CSignal' of 'Integer'
toInteger1 :: Integral a => CSignal clk a -> CSignal clk Integer
toInteger1 = fmap toInteger

-- | __WARNING__: 'testBit' and 'popCount' are undefined, use 'testBit1' and
-- 'popCount1' instead
instance Bits a => Bits (CSignal clk a) where
  (.&.)            = liftA2 (.&.)
  (.|.)            = liftA2 (.|.)
  xor              = liftA2 xor
  complement       = fmap complement
  shift a i        = fmap (`shift` i) a
  rotate a i       = fmap (`rotate` i) a
  zeroBits         = signal# zeroBits
  bit              = signal# . bit
  setBit a i       = fmap (`setBit` i) a
  clearBit a i     = fmap (`clearBit` i) a
  testBit          = error "'testBit' undefined for 'CSignal', use 'testbit1'"
  bitSizeMaybe _   = bitSizeMaybe (undefined :: a)
  bitSize _        = bitSize (undefined :: a)
  isSigned _       = isSigned (undefined :: a)
  shiftL a i       = fmap (`shiftL` i) a
  unsafeShiftL a i = fmap (`unsafeShiftL` i) a
  shiftR a i       = fmap (`shiftR` i) a
  unsafeShiftR a i = fmap (`unsafeShiftR` i) a
  rotateL a i      = fmap (`rotateL` i) a
  rotateR a i      = fmap (`rotateR` i) a
  popCount         = error "'popCount' undefined for 'CSignal', use 'popCount1'"

instance FiniteBits a => FiniteBits (CSignal clk a) where
  finiteBitSize _ = finiteBitSize (undefined :: a)

-- | Version of 'testBit' that has a 'CSignal' of 'Int' as indexing argument,
-- and a result of 'CSignal' of 'Bool'
testBit1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk Bool
testBit1 = liftA2 testBit

-- | Version of 'popCount' that returns a 'CSignal' of 'Int'
popCount1 :: Bits a => CSignal clk a -> CSignal clk Int
popCount1 = fmap popCount

-- | Version of 'shift' that has a 'CSignal' of 'Int' as indexing argument
shift1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
shift1 = liftA2 shift

-- | Version of 'rotate' that has a 'CSignal' of 'Int' as indexing argument
rotate1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
rotate1 = liftA2 rotate

-- | Version of 'setBit' that has a 'CSignal' of 'Int' as indexing argument
setBit1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
setBit1 = liftA2 setBit

-- | Version of 'clearBit' that has a 'CSignal' of 'Int' as indexing argument
clearBit1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
clearBit1 = liftA2 clearBit

-- | Version of 'shiftL' that has a 'CSignal' of 'Int' as indexing argument
shiftL1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
shiftL1 = liftA2 shiftL

-- | Version of 'unsafeShiftL' that has a 'CSignal' of 'Int' as indexing argument
unsafeShiftL1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
unsafeShiftL1 = liftA2 unsafeShiftL

-- | Version of 'shiftR' that has a 'CSignal' of 'Int' as indexing argument
shiftR1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
shiftR1 = liftA2 shiftR

-- | Version of 'unsafeShiftR' that has a 'CSignal' of 'Int' as indexing argument
unsafeShiftR1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
unsafeShiftR1 = liftA2 unsafeShiftR

-- | Version of 'rotateL' that has a 'CSignal' of 'Int' as indexing argument
rotateL1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
rotateL1 = liftA2 rotateL

-- | Version of 'rotateR' that has a 'CSignal' of 'Int' as indexing argument
rotateR1 :: Bits a => CSignal clk a -> CSignal clk Int -> CSignal clk a
rotateR1 = liftA2 rotateR

instance Fractional a => Fractional (CSignal clk a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = signal# . fromRational
