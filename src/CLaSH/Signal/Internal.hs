{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
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
    Domain (..)
  , Signal (..)
    -- * Clocks and resets
  , Clock (..,Clock)
  , ClockKind (..)
  , clockGate
  , Reset (..)
  , ResetKind (..)
  , unsafeFromAsyncReset
  , unsafeToAsyncReset
  , fromSyncReset
  , toSyncReset
    -- * Basic circuits
  , delay#
  , register#
  , mux
    -- * Testbench functions
  , clockGen
  , asyncResetGen
  , syncResetGen
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
import GHC.Generics               (Generic)
import GHC.Stack                  (HasCallStack, withFrozenCallStack)
import GHC.TypeLits               (KnownNat, KnownSymbol, Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))
import System.IO.Unsafe           (unsafeDupablePerformIO)
import Test.QuickCheck            (Arbitrary (..), CoArbitrary(..), Property,
                                   property)

import CLaSH.Promoted.Nat         (SNat (..), snatToInteger)
import CLaSH.Promoted.Symbol      (SSymbol (..))
import CLaSH.XException           (XException, errorX, seqX)

{- $setup
>>> :set -XDataKinds
>>> :set -XMagicHash
>>> :set -XTypeApplications
>>> import CLaSH.Promoted.Nat
>>> import CLaSH.Promoted.Symbol
>>> type System = Dom "System" 10000
>>> let systemClock = Clock @System (pure True)
>>> let systemReset = Async @System (True :- pure False)
>>> let register = register# systemClock systemReset
-}

-- * Signal

-- | A domain with a name (@Symbol@) and a clock period (@Nat@) in ps
data Domain = Dom { domainName :: Symbol, clockPeriod :: Nat }

infixr 5 :-
-- | A synchronized signal with samples of type @a@, explicitly synchronized to
-- a @domain@
--
-- __NB__: The constructor, @(':-')@, is __not__ synthesisable.
data Signal (domain :: Domain) a = a :- Signal domain a

instance Show a => Show (Signal domain a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Signal domain a) where
  lift ~(x :- _) = [| signal# x |]

instance Default a => Default (Signal domain a) where
  def = signal# def

instance Functor (Signal domain) where
  fmap = mapSignal#

{-# NOINLINE mapSignal# #-}
mapSignal# :: (a -> b) -> Signal domain a -> Signal domain b
mapSignal# f (a :- as) = f a :- mapSignal# f as

instance Applicative (Signal domain) where
  pure  = signal#
  (<*>) = appSignal#

{-# NOINLINE signal# #-}
signal# :: a -> Signal domain a
signal# a = let s = a :- s in s

{-# NOINLINE appSignal# #-}
appSignal# :: Signal domain (a -> b) -> Signal domain a -> Signal domain b
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
joinSignal# :: Signal domain (Signal domain a) -> Signal domain a
joinSignal# ~(xs :- xss) = head# xs :- joinSignal# (mapSignal# tail# xss)
  where
    head# (x' :- _ )  = x'
    tail# (_  :- xs') = xs'

instance Num a => Num (Signal domain a) where
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
instance Foldable (Signal domain) where
  foldr = foldr#

{-# NOINLINE foldr# #-}
-- | __NB__: Not synthesisable
--
-- __NB__: In \"@'foldr#' f z s@\":
--
-- * The function @f@ should be /lazy/ in its second argument.
-- * The @z@ element will never be used.
foldr# :: (a -> b -> b) -> b -> Signal domain a -> b
foldr# f z (a :- s) = a `f` (foldr# f z s)

instance Traversable (Signal domain) where
  traverse = traverse#

{-# NOINLINE traverse# #-}
traverse# :: Applicative f => (a -> f b) -> Signal domain a -> f (Signal domain b)
traverse# f (a :- s) = (:-) <$> f a <*> traverse# f s

-- * Clocks and resets

-- | Distinction between gated and ungated clocks
data ClockKind
  = Source -- ^ A clock signal coming straight from the clock source
  | Gated  -- ^ A clock signal that has been gated
  deriving (Eq,Ord,Show,Generic,NFData)

-- | A clock signal belonging to a @domain@
data Clock (domain :: Domain) (gated :: ClockKind) where
  Clock# :: SSymbol name
         -> SNat period
         -> Signal ('Dom name period) Bool
         -> Clock  ('Dom name period) gated

instance Show (Clock domain gated) where
  show (Clock# nm rt _) = show nm ++ show (snatToInteger rt)

-- | We can only "create" @Source@ clock signals
pattern Clock
  :: forall domain name period
   . (KnownSymbol name, KnownNat period, domain ~ 'Dom name period)
  => ()
  => Signal domain Bool
  -> Clock  domain 'Source
pattern Clock en <- Clock# _nm _rt en
  where
    Clock en = Clock# SSymbol SNat en

-- | Clock gating primitive
clockGate :: Clock domain gated -> Signal domain Bool -> Clock domain 'Gated
clockGate (Clock# nm rt en) en' = Clock# nm rt (en .&&. en')
{-# NOINLINE clockGate #-}

-- | Clock generator, for simulations and test benches.
--
-- To be used like:
--
-- @
-- type DomA = Dom \"A\" 1000
-- clkA = clockGen @DomA
-- @
clockGen
  :: (domain ~ 'Dom nm period, KnownSymbol nm, KnownNat period)
  => Signal domain Bool
  -> Clock domain 'Source
clockGen = Clock
{-# NOINLINE clockGen #-}

-- | Asynchronous reset generator, for simulations and test benches.
--
-- To be used like:
--
-- @
-- type DomA = Dom \"A\" 1000
-- rstA = asyncResetGen @DomA
-- @
asyncResetGen :: Reset domain 'Asynchronous
asyncResetGen = Async (True :- pure False)
{-# NOINLINE asyncResetGen #-}

-- | Synchronous reset generator, for simulations and test benches.
--
-- To be used like:
--
-- @
-- type DomA = Dom \"A\" 1000
-- rstA = syncResetGen @DomA
-- @
syncResetGen :: Reset domain 'Synchronous
syncResetGen = Sync (True :- pure False)
{-# NOINLINE syncResetGen #-}

-- | The \"kind\" of reset
data ResetKind = Synchronous | Asynchronous
  deriving (Eq,Ord,Show,Generic,NFData)

-- | A reset signal belonging to a @domain@
data Reset (domain :: Domain) (synchronous :: ResetKind) where
  Sync  :: Signal domain Bool -> Reset domain 'Synchronous
  Async :: Signal domain Bool -> Reset domain 'Asynchronous

-- | 'unsafeToAsyncReset#' is unsafe because it can introduce:
--
-- * meta-stability
-- * combinational loops
unsafeFromAsyncReset :: Reset domain 'Asynchronous -> Signal domain Bool
unsafeFromAsyncReset (Async r) = r
{-# NOINLINE unsafeFromAsyncReset #-}

-- | 'unsafeToAsyncReset' is unsafe because it can introduce:
--
-- * meta-stability
-- * combinational loops
unsafeToAsyncReset :: Signal domain Bool -> Reset domain 'Asynchronous
unsafeToAsyncReset r = Async r
{-# NOINLINE unsafeToAsyncReset #-}

-- | It is safe to treat synchronous resets as @Bool@ signals
fromSyncReset :: Reset domain 'Synchronous -> Signal domain Bool
fromSyncReset (Sync r) = r
{-# NOINLINE fromSyncReset #-}

-- | it is afe to treat @Bool@ signals as synchronous resets
toSyncReset :: Signal domain Bool -> Reset domain 'Synchronous
toSyncReset r = Sync r
{-# NOINLINE toSyncReset #-}

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

-- [Note: register strictness annotations]
--
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

delay#
  :: HasCallStack
  => Clock  domain gated
  -> Signal domain a
  -> Signal domain a
delay# (Clock# _ _ en) =
    go (withFrozenCallStack (errorX "delay: initial value undefined")) en
  where
    go o (e :- es) as@(~(x :- xs)) =
      -- See [Note: register strictness annotations]
      o `seqX` o :- (as `seq` if e then go x es xs else go o es xs)
{-# NOINLINE delay# #-}

register#
  :: HasCallStack
  => Clock domain gated
  -> Reset domain synchronous
  -> a
  -> Signal domain a
  -> Signal domain a
register# (Clock# _ _ ena) (Sync rst)  i =
    go (withFrozenCallStack (errorX "register: initial value undefined")) rst ena
  where
    go o rt@(~(r :- rs)) ~(e :- es) as@(~(x :- xs)) =
      let o' = if r then i else x
          -- [Note: register strictness annotations]
      in  o `seqX` o :- (rt `seq` as `seq` if e then go o' rs es xs
                                                else go o  rs es xs)

register# (Clock# _ _ ena) (Async rst) i =
  go (withFrozenCallStack (errorX "register: initial value undefined")) rst ena
    where
      go o ~(r :- rs) ~(e :- es) as@(~(x :- xs)) =
        let o' = if r then i else o
            -- [Note: register strictness annotations]
        in  o' `seqX` o' :- (as `seq` if e then go x  rs es xs
                                           else go o' rs es xs)
{-# NOINLINE register# #-}

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

instance Fractional a => Fractional (Signal domain a) where
  (/)          = liftA2 (/)
  recip        = fmap recip
  fromRational = signal# . fromRational

instance Arbitrary a => Arbitrary (Signal domain a) where
  arbitrary = liftA2 (:-) arbitrary arbitrary

instance CoArbitrary a => CoArbitrary (Signal domain a) where
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

headStrictSignal :: NFData a => a -> Signal domain a -> Signal domain a
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
fromList :: NFData a => [a] -> Signal domain a
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
simulate :: (NFData a, NFData b) => (Signal domain1 a -> Signal domain2 b) -> [a] -> [b]
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
fromList_lazy :: [a] -> Signal domain a
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
simulate_lazy :: (Signal domain1 a -> Signal domain2 b) -> [a] -> [b]
simulate_lazy f = sample_lazy . f . fromList_lazy
