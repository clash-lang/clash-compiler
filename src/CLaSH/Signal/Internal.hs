{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Copyright  :  (C) 2013-2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
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
  )
where

import Control.Applicative        (Applicative (..), (<$>), liftA2, liftA3)
import Data.Bits                  (Bits (..), FiniteBits (..))
import Data.Default               (Default (..))
import Data.Foldable              as F (Foldable (..))
import Data.Traversable           (Traversable (..))
import GHC.TypeLits               (Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))

import CLaSH.Class.Num            (ExtendingNum (..), SaturatingNum (..))
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
-- | Version of ('||') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.||.) :: Signal' clk Bool -> Signal' clk Bool -> Signal' clk Bool
(.||.) = liftA2 (||)

infixr 3 .&&.
-- | Version of ('&&') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.&&.) :: Signal' clk Bool -> Signal' clk Bool -> Signal' clk Bool
(.&&.) = liftA2 (&&)

-- | Version of 'not' that operates on 'CLaSH.Signal.Signal's of 'Bool'
not1 :: Signal' clk Bool -> Signal' clk Bool
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
-- | A multiplexer. Given "@'mux' b t f@", output @t@ when @b@ is 'True', and @f@
-- when @b@ is 'False'.
mux :: Signal' clk Bool -> Signal' clk a -> Signal' clk a -> Signal' clk a
mux = liftA3 (\b t f -> if b then t else f)

{-# INLINE signal #-}
-- | Create a constant 'CLaSH.Signal.Signal' from a combinational value
--
-- >>> sample (signal 4)
-- [4, 4, 4, 4, ...
signal :: a -> Signal' clk a
signal = signal#

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
-- | Version of ('==') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.==.) :: Eq a => Signal' clk a -> Signal' clk a -> Signal' clk Bool
(.==.) = liftA2 (==)

infix 4 ./=.
-- | Version of ('/=') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(./=.) :: Eq a => Signal' clk a -> Signal' clk a -> Signal' clk Bool
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

-- | Version of 'compare' that returns a 'CLaSH.Signal.Signal' of 'Ordering'
compare1 :: Ord a => Signal' clk a -> Signal' clk a -> Signal' clk Ordering
compare1 = liftA2 compare

infix 4 .<.
-- | Version of ('<') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.<.) :: Ord a => Signal' clk a -> Signal' clk a -> Signal' clk Bool
(.<.) = liftA2 (<)

infix 4 .<=.
-- | Version of ('<=') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.<=.) :: Ord a => Signal' clk a -> Signal' clk a -> Signal' clk Bool
(.<=.) = liftA2 (<=)

infix 4 .>.
-- | Version of ('>') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.>.) :: Ord a => Signal' clk a -> Signal' clk a -> Signal' clk Bool
(.>.) = liftA2 (>)

infix 4 .>=.
-- | Version of ('>=') that returns a 'CLaSH.Signal.Signal' of 'Bool'
(.>=.) :: Ord a => Signal' clk a -> Signal' clk a -> Signal' clk Bool
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

-- | Version of 'fromEnum' that returns a CLaSH.Signal.Signal' of 'Int'
fromEnum1 :: Enum a => Signal' clk a -> Signal' clk Int
fromEnum1 = fmap fromEnum

-- | __WARNING__: 'toRational' is undefined, use 'toRational1' instead
instance (Num a, Ord a) => Real (Signal' clk a) where
  toRational = error "'toRational' undefined for 'Signal'', use 'toRational1'"

-- | Version of 'toRational' that returns a 'CLaSH.Signal.Signal' of 'Rational'
toRational1 :: Real a => Signal' clk a -> Signal' clk Rational
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

-- | Version of 'toRational' that returns a 'CLaSH.Signal.Signal' of 'Integer'
toInteger1 :: Integral a => Signal' clk a -> Signal' clk Integer
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
  bitSize _        = bitSize (undefined :: a)
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

-- | Version of 'testBit' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing
-- argument, and a result of 'CLaSH.Signal.Signal' of 'Bool'
testBit1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk Bool
testBit1 = liftA2 testBit

-- | Version of 'popCount' that returns a 'CLaSH.Signal.Signal' of 'Int'
popCount1 :: Bits a => Signal' clk a -> Signal' clk Int
popCount1 = fmap popCount

-- | Version of 'shift' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shift1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
shift1 = liftA2 shift

-- | Version of 'rotate' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotate1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
rotate1 = liftA2 rotate

-- | Version of 'setBit' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
setBit1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
setBit1 = liftA2 setBit

-- | Version of 'clearBit' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
clearBit1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
clearBit1 = liftA2 clearBit

-- | Version of 'shiftL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shiftL1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
shiftL1 = liftA2 shiftL

-- | Version of 'unsafeShiftL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
unsafeShiftL1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
unsafeShiftL1 = liftA2 unsafeShiftL

-- | Version of 'shiftR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shiftR1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
shiftR1 = liftA2 shiftR

-- | Version of 'unsafeShiftR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
unsafeShiftR1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
unsafeShiftR1 = liftA2 unsafeShiftR

-- | Version of 'rotateL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotateL1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
rotateL1 = liftA2 rotateL

-- | Version of 'rotateR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotateR1 :: Bits a => Signal' clk a -> Signal' clk Int -> Signal' clk a
rotateR1 = liftA2 rotateR

instance Fractional a => Fractional (Signal' clk a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = signal# . fromRational

-- * List \<-\> Signal conversion (not synthesisable)

-- | Get an infinite list of samples from a 'CLaSH.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'CLaSH.Signal.Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesisable
sample :: Signal' clk a -> [a]
sample = F.foldr (:) []

-- | Get a list of @n@ samples from a 'CLaSH.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'CLaSH.Signal.Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
sampleN :: Int -> Signal' clk a -> [a]
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
-- >>> simulate (register 8) [1, 2, 3, ...
-- [8, 1, 2, 3, ...
--
-- __NB__: This function is not synthesisable
simulate :: (Signal' clk1 a -> Signal' clk2 b) -> [a] -> [b]
simulate f = sample . f . fromList
