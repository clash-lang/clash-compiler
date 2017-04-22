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
{-# OPTIONS_GHC -fno-cpr-anal #-}

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
    -- ** lazy version
  , simulate_lazy
    -- * List \<-\> Signal conversion (not synthesisable)
  , sample
  , sampleN
  , fromList
    -- ** lazy versions
  , sample_lazy
  , sampleN_lazy
  , fromList_lazy
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
import Control.DeepSeq            (NFData, force)
import Control.Exception          (catch, evaluate, throw)
import Data.Bits                  (Bits (..))
import Data.Default               (Default (..))
import GHC.TypeLits               (Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))
import System.IO.Unsafe           (unsafeDupablePerformIO)
import Test.QuickCheck            (Arbitrary (..), CoArbitrary(..), Property,
                                   property)

import CLaSH.Promoted.Nat         (SNat, snatToInteger)
import CLaSH.Promoted.Symbol      (SSymbol, ssymbolToString)
import CLaSH.XException           (XException, errorX, seqX)

{- $setup
>>> :set -XDataKinds
>>> :set -XMagicHash
>>> import CLaSH.Promoted.Nat
>>> import CLaSH.Promoted.Symbol
>>> type SystemClock = Clk "System" 1000
>>> type Signal a = Signal' SystemClock a
>>> let register = register# (SClock SSymbol SNat :: SClock SystemClock)
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
appSignal# (f :- fs) xs@(~(a :- as)) = f a :- (xs `seq` appSignal# fs as) -- See [NOTE: Lazy ap]

{- NOTE: Lazy ap
Signal's ap, i.e (Applicative.<*>), must be lazy in it's second argument:

> appSignal :: Signal' clk (a -> b) -> Signal' clk a -> Signal' clk b
> appSignal (f :- fs) ~(a :- as) = f a :- appSignal fs as

because some feedback loops, such as the loop described in 'system' in the
example at http://hackage.haskell.org/package/clash-prelude-0.10.10/docs/CLaSH-Prelude-BlockRam.html,
will lead to "Exception <<loop>>".

However, this "naive" lazy version is _too_ lazy and induces spaceleaks.
The current version:

> appSignal# :: Signal' clk (a -> b) -> Signal' clk a -> Signal' clk b
> appSignal# (f :- fs) xs@(~(a :- as)) = f a :- (xs `seq` appSignal# fs as)

Is lazy enough to handle the earlier mentioned feedback loops, but doesn't leak
(as much) memory like the "naive" lazy version, because the Signal constructor
of the second argument is evaluated as soon as the tail of the result is evaluated.
-}


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
{-# DEPRECATED not1 "'not1' will be removed in clash-prelude-1.0, use \"fmap not\" instead." #-}

{-# NOINLINE register# #-}
register# :: SClock clk -> a -> Signal' clk a -> Signal' clk a
register# _ i s = i :- s

{-# NOINLINE regEn# #-}
regEn# :: SClock clk -> a -> Signal' clk Bool -> Signal' clk a -> Signal' clk a
regEn# _ = go
  where
    -- In order to produce the first (current) value of the register's output
    -- signal, 'o', we don't need to know the shape of either input (enable or
    -- value-in).  This is important, because both values might be produced from
    -- the output in a feedback loop, so we can't know their shape (pattern
    -- match) them until we have produced output.
    --
    -- Thus, we use lazy pattern matching to delay inspecting the shape of
    -- either argument until output has been produced.
    --
    -- However, both arguments need to be evaluated to WHNF as soon as possible
    -- to avoid a space-leak.  Below, we explicitly reduce the value-in signal
    -- using 'seq' as the tail of our output signal is produced.  On the other
    -- hand, because the value of the tail depends on the value of the enable
    -- signal 'e', it will be forced by the 'if'/'then' statement and we don't
    -- need to 'seq' it explicitly.
    go o ~(e :- es) as@(~(x :- xs)) =
      o `seqX` o :- (as `seq` if e then go x es xs else go o es xs)

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

-- | The above type is a generalisation for:
--
-- @
-- __compare1__ :: 'Ord' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Ordering'
-- @
--
-- It is a version of 'compare' that returns a 'CLaSH.Signal.Signal' of 'Ordering'
compare1 :: (Ord a, Applicative f) => f a -> f a -> f Ordering
compare1 = liftA2 compare
{-# DEPRECATED compare1 "'compare1' will be removed in clash-prelude-1.0, use \"liftA2 compare\" instead." #-}

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

-- | The above type is a generalisation for:
--
-- @
-- __fromEnum1__ :: 'Enum' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int'
-- @
--
-- It is a version of 'fromEnum' that returns a CLaSH.Signal.Signal' of 'Int'
fromEnum1 :: (Enum a, Functor f) => f a -> f Int
fromEnum1 = fmap fromEnum
{-# DEPRECATED fromEnum1 "'fromEnum1' will be removed in clash-prelude-1.0, use \"fmap fromEnum\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __toRational1__ :: 'Real' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Rational'
-- @
--
-- It is a version of 'toRational' that returns a 'CLaSH.Signal.Signal' of 'Rational'
toRational1 :: (Real a, Functor f) => f a -> f Rational
toRational1 = fmap toRational
{-# DEPRECATED toRational1 "'toRational1' will be removed in clash-prelude-1.0, use \"fmap toRational\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __toInteger1__ :: 'Integral' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Integer'
-- @
--
-- It is a version of 'toRational' that returns a 'CLaSH.Signal.Signal' of 'Integer'
toInteger1 :: (Integral a, Functor f) => f a -> f Integer
toInteger1 = fmap toInteger
{-# DEPRECATED toInteger1 "'toInteger1' will be removed in clash-prelude-1.0, use \"fmap toInteger\" instead." #-}

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
{-# DEPRECATED testBit1 "'testBit1' will be removed in clash-prelude-1.0, use \"liftA2 testBit\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __popCount1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int'
-- @
--
--  It is a version of 'popCount' that returns a 'CLaSH.Signal.Signal' of 'Int'
popCount1 :: (Bits a, Functor f) => f a -> f Int
popCount1 = fmap popCount
{-# DEPRECATED popCount1 "'popCount1' will be removed in clash-prelude-1.0, use \"fmap popCount\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __shift1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'shift' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shift1 :: (Bits a, Applicative f) => f a -> f Int -> f a
shift1 = liftA2 shift
{-# DEPRECATED shift1 "'shift1' will be removed in clash-prelude-1.0, use \"liftA2 shift\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __rotate1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'rotate' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotate1 :: (Bits a, Applicative f) => f a -> f Int -> f a
rotate1 = liftA2 rotate
{-# DEPRECATED rotate1 "'rotate1' will be removed in clash-prelude-1.0, use \"liftA2 rotate\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __setBit1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'setBit' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
setBit1 :: (Bits a, Applicative f) => f a -> f Int -> f a
setBit1 = liftA2 setBit
{-# DEPRECATED setBit1 "'setBit1' will be removed in clash-prelude-1.0, use \"liftA2 setBit\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __clearBit1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'clearBit' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
clearBit1 :: (Bits a, Applicative f) => f a -> f Int -> f a
clearBit1 = liftA2 clearBit
{-# DEPRECATED clearBit1 "'clearBit1' will be removed in clash-prelude-1.0, use \"liftA2 clearBit\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __shiftL1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'shiftL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shiftL1 :: (Bits a, Applicative f) => f a -> f Int -> f a
shiftL1 = liftA2 shiftL
{-# DEPRECATED shiftL1 "'shiftL1' will be removed in clash-prelude-1.0, use \"liftA2 shiftL\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __unsafeShiftL1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'unsafeShiftL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
unsafeShiftL1 :: (Bits a, Applicative f) => f a -> f Int -> f a
unsafeShiftL1 = liftA2 unsafeShiftL
{-# DEPRECATED unsafeShiftL1 "'unsafeShiftL1' will be removed in clash-prelude-1.0, use \"liftA2 unsafeShiftL\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __shiftR1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'shiftR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
shiftR1 :: (Bits a, Applicative f) => f a -> f Int -> f a
shiftR1 = liftA2 shiftR
{-# DEPRECATED shiftR1 "'shiftR1' will be removed in clash-prelude-1.0, use \"liftA2 shiftR\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __unsafeShiftR1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'unsafeShiftR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
unsafeShiftR1 :: (Bits a, Applicative f) => f a -> f Int -> f a
unsafeShiftR1 = liftA2 unsafeShiftR
{-# DEPRECATED unsafeShiftR1 "'unsafeShiftR1' will be removed in clash-prelude-1.0, use \"liftA2 unsafeShiftR\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __rotateL1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'rotateL' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotateL1 :: (Bits a, Applicative f) => f a -> f Int -> f a
rotateL1 = liftA2 rotateL
{-# DEPRECATED rotateL1 "'rotateL1' will be removed in clash-prelude-1.0, use \"liftA2 rotateL\" instead." #-}

-- | The above type is a generalisation for:
--
-- @
-- __rotateR1__ :: 'Bits' a => 'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' 'Int' -> 'CLaSH.Signal.Signal' 'a'
-- @
--
-- It is a version of 'rotateR' that has a 'CLaSH.Signal.Signal' of 'Int' as indexing argument
rotateR1 :: (Bits a, Applicative f) => f a -> f Int -> f a
rotateR1 = liftA2 rotateR
{-# DEPRECATED rotateR1 "'rotateR1' will be removed in clash-prelude-1.0, use \"liftA2 rotateR\" instead." #-}

instance Fractional a => Fractional (Signal' clk a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = signal# . fromRational

instance Arbitrary a => Arbitrary (Signal' clk a) where
  arbitrary = liftA2 (:-) arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Signal' clk a) where
  coarbitrary xs gen = do
    n <- arbitrary
    coarbitrary (take (abs n) (sample_lazy xs)) gen

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

-- | A 'force' that lazily returns exceptions
forceNoException :: NFData a => a -> IO a
forceNoException x = catch (evaluate (force x)) (\(e :: XException) -> return (throw e))

headStrictCons :: NFData a => a -> [a] -> [a]
headStrictCons x xs = unsafeDupablePerformIO ((:) <$> forceNoException x <*> pure xs)

headStrictSignal :: NFData a => a -> Signal' clk a -> Signal' clk a
headStrictSignal x xs = unsafeDupablePerformIO ((:-) <$> forceNoException x <*> pure xs)

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
sample :: (Foldable f, NFData a) => f a -> [a]
sample = foldr headStrictCons []

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
sampleN :: (Foldable f, NFData a) => Int -> f a -> [a]
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
fromList :: NFData a => [a] -> Signal' clk a
fromList = Prelude.foldr headStrictSignal (errorX "finite list")

-- * Simulation functions (not synthesisable)

-- | Simulate a (@'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesisable
simulate :: (NFData a, NFData b) => (Signal' clk1 a -> Signal' clk2 b) -> [a] -> [b]
simulate f = sample . f . fromList

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
sample_lazy :: Foldable f => f a -> [a]
sample_lazy = foldr (:) []

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
sampleN_lazy :: Foldable f => Int -> f a -> [a]
sampleN_lazy n = take n . sample_lazy

-- | Create a 'CLaSH.Signal.Signal' from a list
--
-- Every element in the list will correspond to a value of the signal for one
-- clock cycle.
--
-- >>> sampleN 2 (fromList [1,2,3,4,5])
-- [1,2]
--
-- __NB__: This function is not synthesisable
fromList_lazy :: [a] -> Signal' clk a
fromList_lazy = Prelude.foldr (:-) (error "finite list")

-- * Simulation functions (not synthesisable)

-- | Simulate a (@'CLaSH.Signal.Signal' a -> 'CLaSH.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesisable
simulate_lazy :: (Signal' clk1 a -> Signal' clk2 b) -> [a] -> [b]
simulate_lazy f = sample_lazy . f . fromList_lazy
