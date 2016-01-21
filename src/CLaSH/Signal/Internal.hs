{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE Unsafe #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
#if __GLASGOW_HASKELL__ > 711
{-# OPTIONS_GHC -fno-cpr-anal #-}
#endif

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Signal.Internal
  ( -- * Datatypes
    Clock (..)
  , SClock (..)
  , Signal' (..)
    -- * Basic circuits
  , register#
  , regEn#
  , mux
  , signal
    -- * Boolean connectives
  , (.&&.), (.||.), not1
    -- * Simulation functions (not synthesisable)
  , simulate
    -- * List \<-\> Signal conversion (not synthesisable)
  , sample
  , sampleN
  , fromList
    -- * QuickCheck combinators
  , testFor
    -- * Type classes
    -- ** 'Eq'-like
  , (.==.), (./=.)
    -- ** 'Ord'-like
  , compare1, (.<.), (.<=.), (.>=.), (.>.)
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
  -- * EXTREMELY EXPERIMENTAL
  , joinSignal#
  )
where

import Control.Applicative        (liftA2, liftA3)
import Data.Bits                  (Bits (..), FiniteBits (..))
import Data.Default               (Default (..))
import GHC.TypeLits               (Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))
import Test.QuickCheck            (Arbitrary (..), CoArbitrary(..), Property,
                                   property)

import CLaSH.Class.Num            (ExtendingNum (..), SaturatingNum (..))
import CLaSH.Promoted.Nat         (SNat, snatToInteger)
import CLaSH.Promoted.Symbol      (SSymbol, ssymbolToString)

{- $setup
>>> :set -XDataKinds
>>> :set -XMagicHash
>>> import CLaSH.Promoted.Nat
>>> import CLaSH.Promoted.Symbol
>>> type SystemClock = Clk "System" 1000
>>> type Signal a = Signal' SystemClock a
>>> let register = register# (SClock ssymbol snat :: SClock SystemClock)
-}

-- | A clock with a name ('Symbol') and period ('Nat')
data Clock = Clk Symbol Nat

-- | Singleton value for a type-level 'Clock' with the given @name@ and @period@
data SClock (clk :: Clock)
  where
    SClock :: SSymbol name -> SNat period -> SClock ('Clk name period)

instance Show (SClock clk) where
  show (SClock nm r) = ssymbolToString nm ++ show (snatToInteger r)

infixr 5 :-
-- | A synchronized signal with samples of type @a@, explicitly synchronized to
-- a clock @clk@
--
-- __NB__: The constructor, @(':-')@, is __not__ synthesisable.
data Signal' (clk :: Clock) a = a :- Signal' clk a

instance Show a => Show (Signal' clk a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Signal' clk a) where
  lift ~(x :- _) = [| signal# x |]

instance Default a => Default (Signal' clk a) where
  def = signal# def

instance Functor (Signal' clk) where
  fmap = mapSignal#

{-# NOINLINE mapSignal# #-}
mapSignal# :: (a -> b) -> Signal' clk a -> Signal' clk b
mapSignal# f (a :- as) = f a :- mapSignal# f as

instance Applicative (Signal' clk) where
  pure  = signal#
  (<*>) = appSignal#

{-# NOINLINE signal# #-}
signal# :: a -> Signal' clk a
signal# a = let s = a :- s in s

{-# NOINLINE appSignal# #-}
appSignal# :: Signal' clk (a -> b) -> Signal' clk a -> Signal' clk b
appSignal# (f :- fs) ~(a :- as) = f a :- appSignal# fs as

{-# NOINLINE joinSignal# #-}
-- | __WARNING: EXTREMELY EXPERIMENTAL__
--
-- The circuit semantics of this operation are unclear and/or non-existent.
-- There is a good reason there is no 'Monad' instance for 'Signal''.
--
-- Is currently treated as 'id' by the CLaSH compiler.
joinSignal# :: Signal' clk (Signal' clk a) -> Signal' clk a
joinSignal# ~(xs :- xss) = head# xs :- joinSignal# (mapSignal# tail# xss)
  where
    head# (x' :- _ )  = x'
    tail# (_  :- xs') = xs'

instance Num a => Num (Signal' clk a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  negate      = fmap negate
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = signal# . fromInteger

-- | __NB__: Not synthesisable
--
-- __NB__: In \"@'foldr' f z s@\":
--
-- * The function @f@ should be /lazy/ in its second argument.
-- * The @z@ element will never be used.
instance Foldable (Signal' clk) where
  foldr = foldr#

{-# NOINLINE foldr# #-}
-- | __NB__: Not synthesisable
--
-- __NB__: In \"@'foldr#' f z s@\":
--
-- * The function @f@ should be /lazy/ in its second argument.
-- * The @z@ element will never be used.
foldr# :: (a -> b -> b) -> b -> Signal' clk a -> b
foldr# f z (a :- s) = a `f` (foldr# f z s)

instance Traversable (Signal' clk) where
  traverse = traverse#

{-# NOINLINE traverse# #-}
traverse# :: Applicative f => (a -> f b) -> Signal' clk a -> f (Signal' clk b)
traverse# f (a :- s) = (:-) <$> f a <*> traverse# f s

infixr 2 .||.
-- | The above type is a generalisation for:
--
-- @
-- __(.||.)__ :: 'CLaSH.Signal.Signal' 'Bool' -> 'CLaSH.Signal.Signal' 'Bool' -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('||') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

infixr 3 .&&.
-- | The above type is a generalisation for:
--
-- @
-- __(.&&.)__ :: 'CLaSH.Signal.Signal' 'Bool' -> 'CLaSH.Signal.Signal' 'Bool' -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('&&') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.&&.) :: Applicative f => f Bool -> f Bool -> f Bool
(.&&.) = liftA2 (&&)

-- | The above type is a generalisation for:
--
-- @
-- __not1__ :: 'CLaSH.Signal.Signal' 'Bool' -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of 'not' that operates on 'CLaSH.Signal.Signal's of 'Bool'
not1 :: Functor f => f Bool -> f Bool
not1 = fmap not

{-# NOINLINE register# #-}
register# :: SClock clk -> a -> Signal' clk a -> Signal' clk a
register# _ i s = i :- s

{-# NOINLINE regEn# #-}
regEn# :: SClock clk -> a -> Signal' clk Bool -> Signal' clk a -> Signal' clk a
regEn# clk i b s = r
  where
    r  = register# clk i s'
    s' = mux b s r

{-# INLINE mux #-}
-- | The above type is a generalisation for:
--
-- @
-- __mux__ :: 'CLaSH.Signal.Signal' 'Bool' -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a
-- @
--
-- A multiplexer. Given "@'mux' b t f@", output @t@ when @b@ is 'True', and @f@
-- when @b@ is 'False'.
mux :: Applicative f => f Bool -> f a -> f a -> f a
mux = liftA3 (\b t f -> if b then t else f)

{-# INLINE signal #-}
-- | The above type is a generalisation for:
--
-- @
-- __signal__ :: a -> 'CLaSH.Signal.Signal' a
-- @
--
-- Create a constant 'CLaSH.Signal.Signal' from a combinational value
--
-- >>> sampleN 5 (signal 4 :: Signal Int)
-- [4,4,4,4,4]
signal :: Applicative f => a -> f a
signal = pure

instance Bounded a => Bounded (Signal' clk a) where
  minBound = signal# minBound
  maxBound = signal# maxBound

instance ExtendingNum a b => ExtendingNum (Signal' clk a) (Signal' clk b) where
  type AResult (Signal' clk a) (Signal' clk b) = Signal' clk (AResult a b)
  plus  = liftA2 plus
  minus = liftA2 minus
  type MResult (Signal' clk a) (Signal' clk b) = Signal' clk (MResult a b)
  times = liftA2 times

instance SaturatingNum a => SaturatingNum (Signal' clk a) where
  satPlus s = liftA2 (satPlus s)
  satMin  s = liftA2 (satMin s)
  satMult s = liftA2 (satMult s)

-- | __WARNING__: ('==') and ('/=') are undefined, use ('.==.') and ('./=.')
-- instead
instance Eq (Signal' clk a) where
  (==) = error "(==)' undefined for 'Signal'', use '(.==.)' instead"
  (/=) = error "(/=)' undefined for 'Signal'', use '(./=.)' instead"

infix 4 .==.
-- | The above type is a generalisation for:
--
-- @
-- __(.==.)__ :: 'Eq' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('==') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.==.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(.==.) = liftA2 (==)

infix 4 ./=.
-- | The above type is a generalisation for:
--
-- @
-- __(./=.)__ :: 'Eq' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('/=') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(./=.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(./=.) = liftA2 (/=)

-- | __WARNING__: 'compare', ('<'), ('>='), ('>'), and ('<=') are
-- undefined, use 'compare1', ('.<.'), ('.>=.'), ('.>.'), and ('.<=.') instead
instance Ord a => Ord (Signal' clk a) where
  compare = error "'compare' undefined for 'Signal'', use 'compare1' instead"
  (<)     = error "'(<)' undefined for 'Signal'', use '(.<.)' instead"
  (>=)    = error "'(>=)' undefined for 'Signal'', use '(.>=.)' instead"
  (>)     = error "'(>)' undefined for 'Signal'', use '(.>.)' instead"
  (<=)    = error "'(<=)' undefined for 'Signal'', use '(.<=.)' instead"
  max     = liftA2 max
  min     = liftA2 min

-- | The above type is a generalisation for:
--
-- @
-- __compare1__ :: 'Ord' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Ordering'
-- @
--
-- It is a version of 'compare' that returns a 'CLaSH.Signal.Signal' of 'Ordering'
compare1 :: (Ord a, Applicative f) => f a -> f a -> f Ordering
compare1 = liftA2 compare

infix 4 .<.
-- | The above type is a generalisation for:
--
-- @
-- __(.<.)__ :: 'Ord' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('<') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.<.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<.) = liftA2 (<)

infix 4 .<=.
-- | The above type is a generalisation for:
--
-- @
-- __(.<=.)__ :: 'Ord' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('<=') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.<=.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<=.) = liftA2 (<=)

infix 4 .>.
-- | The above type is a generalisation for:
--
-- @
-- __(.>.)__ :: 'Ord' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('>') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.>.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>.) = liftA2 (>)

infix 4 .>=.
-- | The above type is a generalisation for:
--
-- @
-- __(.>=.)__ :: 'Ord' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
--  It is a version of ('>=') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.>=.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>=.) = liftA2 (>=)

-- | __WARNING__: 'fromEnum' is undefined, use 'fromEnum1' instead
instance Enum a => Enum (Signal' clk a) where
  succ           = fmap succ
  pred           = fmap pred
  toEnum         = signal# . toEnum
  fromEnum       = error "'fromEnum' undefined for 'Signal'', use 'fromEnum1'"
  enumFrom       = sequenceA . fmap enumFrom
  enumFromThen   = (sequenceA .) . liftA2 enumFromThen
  enumFromTo     = (sequenceA .) . liftA2 enumFromTo
  enumFromThenTo = ((sequenceA .) .) . liftA3 enumFromThenTo

-- | The above type is a generalisation for:
--
-- @
-- __fromEnum1__ :: 'Enum' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int'
-- @
--
-- It is a version of 'fromEnum' that returns a CLaSH.Signal.Signal' of 'Int'
fromEnum1 :: (Enum a, Functor f) => f a -> f Int
fromEnum1 = fmap fromEnum

-- | __WARNING__: 'toRational' is undefined, use 'toRational1' instead
instance (Num a, Ord a) => Real (Signal' clk a) where
  toRational = error "'toRational' undefined for 'Signal'', use 'toRational1'"

-- | The above type is a generalisation for:
--
-- @
-- __toRational1__ :: 'Real' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Rational'
-- @
--
-- It is a version of 'toRational' that returns a 'CLaSH.Signal.Signal' of 'Rational'
toRational1 :: (Real a, Functor f) => f a -> f Rational
toRational1 = fmap toRational

-- | __WARNING__: 'toInteger' is undefined, use 'toInteger1' instead
instance Integral a => Integral (Signal' clk a) where
  quot        = liftA2 quot
  rem         = liftA2 rem
  div         = liftA2 div
  mod         = liftA2 mod
  quotRem a b = (quot a b, rem a b)
  divMod a b  = (div a b, mod a b)
  toInteger   = error "'toInteger' undefined for 'Signal'', use 'toInteger1'"

-- | The above type is a generalisation for:
--
-- @
-- __toInteger1__ :: 'Integral' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Integer'
-- @
--
-- It is a version of 'toRational' that returns a 'CLaSH.Signal.Signal' of 'Integer'
toInteger1 :: (Integral a, Functor f) => f a -> f Integer
toInteger1 = fmap toInteger

-- | __WARNING__: 'testBit' and 'popCount' are undefined, use 'testBit1' and
-- 'popCount1' instead
instance Bits a => Bits (Signal' clk a) where
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
  testBit          = error "'testBit' undefined for 'Signal'', use 'testbit1'"
  bitSizeMaybe _   = bitSizeMaybe (undefined :: a)
  bitSize _        = maybe 0 id (bitSizeMaybe (undefined :: a))
  isSigned _       = isSigned (undefined :: a)
  shiftL a i       = fmap (`shiftL` i) a
  unsafeShiftL a i = fmap (`unsafeShiftL` i) a
  shiftR a i       = fmap (`shiftR` i) a
  unsafeShiftR a i = fmap (`unsafeShiftR` i) a
  rotateL a i      = fmap (`rotateL` i) a
  rotateR a i      = fmap (`rotateR` i) a
  popCount         = error "'popCount' undefined for 'Signal'', use 'popCount1'"

instance FiniteBits a => FiniteBits (Signal' clk a) where
  finiteBitSize _ = finiteBitSize (undefined :: a)

-- | The above type is a generalisation for:
--
-- @
-- __testBit1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'Bool'
-- @
--
-- It is a version of 'testBit' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing
-- argument, and a result of 'CLaSH.Signal.Signal' of 'Bool'
testBit1 :: (Bits a, Applicative f) => f a -> f Int -> f Bool
testBit1 = liftA2 testBit

-- | The above type is a generalisation for:
--
-- @
-- __popCount1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int'
-- @
--
--  It is a version of 'popCount' that returns a 'CLaSH.Signal.Signal' of 'Int'
popCount1 :: (Bits a, Functor f) => f a -> f Int
popCount1 = fmap popCount

-- | The above type is a generalisation for:
--
-- @
-- __shift1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'shift' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shift1 :: (Bits a, Applicative f) => f a -> f Int -> f a
shift1 = liftA2 shift

-- | The above type is a generalisation for:
--
-- @
-- __rotate1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'rotate' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotate1 :: (Bits a, Applicative f) => f a -> f Int -> f a
rotate1 = liftA2 rotate

-- | The above type is a generalisation for:
--
-- @
-- __setBit1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'setBit' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
setBit1 :: (Bits a, Applicative f) => f a -> f Int -> f a
setBit1 = liftA2 setBit

-- | The above type is a generalisation for:
--
-- @
-- __clearBit1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'clearBit' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
clearBit1 :: (Bits a, Applicative f) => f a -> f Int -> f a
clearBit1 = liftA2 clearBit

-- | The above type is a generalisation for:
--
-- @
-- __shiftL1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'shiftL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shiftL1 :: (Bits a, Applicative f) => f a -> f Int -> f a
shiftL1 = liftA2 shiftL

-- | The above type is a generalisation for:
--
-- @
-- __unsafeShiftL1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'unsafeShiftL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
unsafeShiftL1 :: (Bits a, Applicative f) => f a -> f Int -> f a
unsafeShiftL1 = liftA2 unsafeShiftL

-- | The above type is a generalisation for:
--
-- @
-- __shiftR1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'shiftR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shiftR1 :: (Bits a, Applicative f) => f a -> f Int -> f a
shiftR1 = liftA2 shiftR

-- | The above type is a generalisation for:
--
-- @
-- __unsafeShiftR1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'unsafeShiftR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
unsafeShiftR1 :: (Bits a, Applicative f) => f a -> f Int -> f a
unsafeShiftR1 = liftA2 unsafeShiftR

-- | The above type is a generalisation for:
--
-- @
-- __rotateL1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'rotateL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotateL1 :: (Bits a, Applicative f) => f a -> f Int -> f a
rotateL1 = liftA2 rotateL

-- | The above type is a generalisation for:
--
-- @
-- __rotateR1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'rotateR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotateR1 :: (Bits a, Applicative f) => f a -> f Int -> f a
rotateR1 = liftA2 rotateR

instance Fractional a => Fractional (Signal' clk a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = signal# . fromRational

instance Arbitrary a => Arbitrary (Signal' clk a) where
  arbitrary = liftA2 (:-) arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Signal' clk a) where
  coarbitrary xs gen = do
    n <- arbitrary
    coarbitrary (take (abs n) (sample xs)) gen

-- | The above type is a generalisation for:
--
-- @
-- __testFor__ :: 'Int' -> 'CLaSH.Signal.Signal' Bool -> 'Property'
-- @
--
-- @testFor n s@ tests the signal @s@ for @n@ cycles.
testFor :: Foldable f => Int -> f Bool -> Property
testFor n = property . and . take n . sample

-- * List \<-\> Signal conversion (not synthesisable)

-- | The above type is a generalisation for:
--
-- @
-- __sample__ :: 'CLaSH.Signal.Signal' a -> [a]
-- @
--
-- Get an infinite list of samples from a 'CLaSH.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'CLaSH.Signal.Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesisable
sample :: Foldable f => f a -> [a]
sample = foldr (:) []

-- | The above type is a generalisation for:
--
-- @
-- __sampleN__ :: Int -> 'CLaSH.Signal.Signal' a -> [a]
-- @
--
-- Get a list of @n@ samples from a 'CLaSH.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'CLaSH.Signal.Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
sampleN :: Foldable f => Int -> f a -> [a]
sampleN n = take n . sample

-- | Create a 'CLaSH.Signal.Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
fromList :: [a] -> Signal' clk a
fromList = Prelude.foldr (:-) (error "finite list")

-- * Simulation functions (not synthesisable)

-- | Simulate a (@'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register 8) [1, 2, 3]
-- [8,1,2,3...
--
-- __NB__: This function is not synthesisable
simulate :: (Signal' clk1 a -> Signal' clk2 b) -> [a] -> [b]
simulate f = sample . f . fromList
