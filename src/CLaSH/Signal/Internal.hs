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
  , (.&&.), (.||.)
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
  , (.<.), (.<=.), (.>=.), (.>.)
    -- ** 'Functor'
  , mapSignal#
    -- ** 'Applicative'
  , signal#
  , appSignal#
    -- ** 'Foldable'
  , foldr#
    -- ** 'Traversable'
  , traverse#
  -- * EXTREMELY EXPERIMENTAL
  , joinSignal#
  )
where

import Control.Applicative        (liftA2, liftA3)
import Control.DeepSeq            (NFData, force)
import Control.Exception          (catch, evaluate, throw)
import Data.Default               (Default (..))
import GHC.TypeLits               (Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))
import System.IO.Unsafe           (unsafeDupablePerformIO)
import Test.QuickCheck            (Arbitrary (..), CoArbitrary(..), Property,
                                   property)

import CLaSH.Promoted.Nat         (SNat, snatToInteger)
import CLaSH.Promoted.Symbol      (SSymbol, ssymbolToString)
import CLaSH.XException           (XException, errorX)

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
