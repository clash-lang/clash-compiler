{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2017     , Myrtle Software Ltd, Google Inc.
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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE Unsafe #-}

-- See: https://github.com/clash-lang/clash-compiler/commit/721fcfa9198925661cd836668705f817bddaae3c
-- as to why we need this.
{-# OPTIONS_GHC -fno-cpr-anal #-}

{-# OPTIONS_HADDOCK show-extensions not-home #-}

module Clash.Signal.Internal
  ( -- * Datatypes
    Domain (..)
  , Signal (..)
  , head#
  , tail#
    -- * Clocks
  , Clock (..)
  , ClockKind (..)
  , clockPeriod
  , clockEnable
    -- ** Clock gating
  , clockGate
    -- * Resets
  , Reset (..)
  , ResetKind (..)
  , unsafeFromAsyncReset
  , unsafeToAsyncReset
  , fromSyncReset
  , unsafeToSyncReset
    -- * Basic circuits
  , delay#
  , register#
  , mux
    -- * Simulation and testbench functions
  , clockGen
  , tbClockGen
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

import Type.Reflection            (Typeable)
import Control.Applicative        (liftA2, liftA3)
import Control.DeepSeq            (NFData, force)
import Control.Exception          (catch, evaluate, throw)
import Data.Default.Class         (Default (..))
import GHC.Generics               (Generic)
import GHC.Stack                  (HasCallStack, withFrozenCallStack)
import GHC.TypeLits               (KnownNat, KnownSymbol, Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))
import System.IO.Unsafe           (unsafeDupablePerformIO)
import Test.QuickCheck            (Arbitrary (..), CoArbitrary(..), Property,
                                   property)

import Clash.Promoted.Nat         (SNat (..), snatToInteger, snatToNum)
import Clash.Promoted.Symbol      (SSymbol (..))
import Clash.XException           (Undefined (..), XException, errorX, seqX)

{- $setup
>>> :set -XDataKinds
>>> :set -XMagicHash
>>> :set -XTypeApplications
>>> import Clash.Promoted.Nat
>>> import Clash.XException
>>> type System = Dom "System" 10000
>>> let systemClockGen = clockGen @System
>>> let systemResetGen = asyncResetGen @System
>>> let register = register#
>>> let registerS = register#
>>> let registerA = register#
-}

-- * Signal

-- | A domain with a name (@Symbol@) and a clock period (@Nat@) in /ps/
data Domain = Dom { domainName :: Symbol, clkPeriod :: Nat }
  deriving (Typeable)

infixr 5 :-
{- | CλaSH has synchronous 'Signal's in the form of:

@
'Signal' (domain :: 'Domain') a
@

Where /a/ is the type of the value of the 'Signal', for example /Int/ or /Bool/,
and /domain/ is the /clock-/ (and /reset-/) domain to which the memory elements
manipulating these 'Signal's belong.

The type-parameter, /domain/, is of the kind 'Domain' which has types of the
following shape:

@
data Domain = Dom { domainName :: 'GHC.TypeLits.Symbol', clkPeriod :: 'GHC.TypeLits.Nat' }
@

Where /domainName/ is a type-level string ('GHC.TypeLits.Symbol') representing
the name of the /clock-/ (and /reset-/) domain, and /clkPeriod/ is a type-level
natural number ('GHC.TypeLits.Nat') representing the clock period (in __ps__)
of the clock lines in the /clock-domain/.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that goes any faster!
-}
data Signal (domain :: Domain) a
  -- | The constructor, @(':-')@, is __not__ synthesisable.
  = a :- Signal domain a

head# :: Signal dom a -> a
head# (x' :- _ )  = x'

tail# :: Signal dom a -> Signal dom a
tail# (_  :- xs') = xs'

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
example at http://hackage.haskell.org/package/clash-prelude-0.10.10/docs/Clash-Prelude-BlockRam.html,
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
-- Is currently treated as 'id' by the Clash compiler.
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
  Clock
    :: (domain ~ ('Dom name period))
    => SSymbol name
    -> SNat    period
    -> Clock domain 'Source
  GatedClock
    :: (domain ~ ('Dom name period))
    => SSymbol name
    -> SNat    period
    -> Signal domain Bool
    -> Clock  domain 'Gated

-- | Get the clock period of a 'Clock' (in /ps/) as a 'Num'
clockPeriod
  :: Num a
  => Clock domain gated
  -> a
clockPeriod (Clock _ period)        = snatToNum period
clockPeriod (GatedClock _ period _) = snatToNum period

-- | If the clock is gated, return 'Just' the /enable/ signal, 'Nothing'
-- otherwise
clockEnable
  :: Clock domain gated
  -> Maybe (Signal domain Bool)
clockEnable Clock {}            = Nothing
clockEnable (GatedClock _ _ en) = Just en

instance Show (Clock domain gated) where
  show (Clock      nm period)   = show nm ++ show (snatToInteger period)
  show (GatedClock nm period _) = show nm ++ show (snatToInteger period)

-- | Clock gating primitive
clockGate :: Clock domain gated -> Signal domain Bool -> Clock domain 'Gated
clockGate (Clock nm rt)         en  = GatedClock nm rt en
clockGate (GatedClock nm rt en) en' = GatedClock nm rt (en .&&. en')
{-# NOINLINE clockGate #-}

-- | Clock generator for simulations. Do __not__ use this clock generator for
-- for the /testBench/ function, use 'tbClockGen' instead.
--
-- To be used like:
--
-- @
-- type DomA = Dom \"A\" 1000
-- clkA = clockGen @DomA
-- @
clockGen
  :: (domain ~ 'Dom nm period, KnownSymbol nm, KnownNat period)
  => Clock domain 'Source
clockGen = Clock SSymbol SNat
{-# NOINLINE clockGen #-}

-- | Clock generator to be used in the /testBench/ function.
--
-- To be used like:
--
-- @
-- type DomA = Dom \"A\" 1000
-- clkA en = clockGen @DomA en
-- @
--
-- === __Example__
--
-- @
-- type DomA1 = Dom \"A\" 1 -- fast, twice as fast as slow
-- type DomB2 = Dom \"B\" 2 -- slow
--
-- topEntity
--   :: Clock DomA1 Source
--   -> Reset DomA1 Asynchronous
--   -> Clock DomB2 Source
--   -> Signal DomA1 (Unsigned 8)
--   -> Signal DomB2 (Unsigned 8, Unsigned 8)
-- topEntity clk1 rst1 clk2 i =
--   let h = register clk1 rst1 0 (register clk1 rst1 0 i)
--       l = register clk1 rst1 0 i
--   in  unsafeSynchronizer clk1 clk2 (bundle (h,l))
--
-- testBench
--   :: Signal DomB2 Bool
-- testBench = done
--   where
--     testInput      = stimuliGenerator clkA1 rstA1 $(listToVecTH [1::Unsigned 8,2,3,4,5,6,7,8])
--     expectedOutput = outputVerifier   clkB2 rstB2 $(listToVecTH [(0,0) :: (Unsigned 8, Unsigned 8),(1,2),(3,4),(5,6),(7,8)])
--     done           = expectedOutput (topEntity clkA1 rstA1 clkB2 testInput)
--     done'          = not \<$\> done
--     clkA1          = 'tbClockGen' \@DomA1 (unsafeSynchronizer clkB2 clkA1 done')
--     clkB2          = 'tbClockGen' \@DomB2 done'
--     rstA1          = asyncResetGen \@DomA1
--     rstB2          = asyncResetGen \@DomB2
-- @
tbClockGen
  :: (domain ~ 'Dom nm period, KnownSymbol nm, KnownNat period)
  => Signal domain Bool
  -> Clock domain 'Source
tbClockGen _ = Clock SSymbol SNat
{-# NOINLINE tbClockGen #-}

-- | Asynchronous reset generator, for simulations and the /testBench/ function.
--
-- To be used like:
--
-- @
-- type DomA = Dom \"A\" 1000
-- rstA = asyncResetGen @DomA
-- @
--
-- __NB__: Can only be used for components with an /active-high/ reset
-- port, which all __clash-prelude__ components are.
--
-- === __Example__
--
-- @
-- type Dom2 = Dom "dom" 2
-- type Dom7 = Dom "dom" 7
-- type Dom9 = Dom "dom" 9
--
-- topEntity
--   :: Clock Dom2 Source
--   -> Clock Dom7 Source
--   -> Clock Dom9 Source
--   -> Signal Dom7 Integer
--   -> Signal Dom9 Integer
-- topEntity clk2 clk7 clk9 i = delay clk9 (unsafeSynchronizer clk2 clk9 (delay clk2 (unsafeSynchronizer clk7 clk2 (delay clk7 i))))
-- {-# NOINLINE topEntity #-}
--
-- testBench
--   :: Signal Dom9 Bool
-- testBench = done
--   where
--     testInput      = stimuliGenerator clk7 rst7 $(listToVecTH [(1::Integer)..10])
--     expectedOutput = outputVerifier   clk9 rst9
--                         ((undefined :> undefined :> Nil) ++ $(listToVecTH ([2,3,4,5,7,8,9,10]::[Integer])))
--     done           = expectedOutput (topEntity clk2 clk7 clk9 testInput)
--     done'          = not \<$\> done
--     clk2           = tbClockGen \@Dom2 (unsafeSynchronizer clk9 clk2 done')
--     clk7           = tbClockGen \@Dom7 (unsafeSynchronizer clk9 clk7 done')
--     clk9           = tbClockGen \@Dom9 done'
--     rst7           = 'asyncResetGen' \@Dom7
--     rst9           = 'asyncResetGen' \@Dom9
-- @
asyncResetGen :: Reset domain 'Asynchronous
asyncResetGen = Async (True :- pure False)
{-# NOINLINE asyncResetGen #-}

-- | Synchronous reset generator, for simulations and the /testBench/ function.
--
-- To be used like:
--
-- @
-- type DomA = Dom \"A\" 1000
-- rstA = syncResetGen @DomA
-- @
--
-- __NB__: Can only be used for components with an /active-high/ reset
-- port, which all __clash-prelude__ components are.
syncResetGen :: ( domain ~ 'Dom n clkPeriod
                , KnownNat clkPeriod )
             => Reset domain 'Synchronous
syncResetGen = Sync (True :- pure False)
{-# NOINLINE syncResetGen #-}

-- | The \"kind\" of reset
--
-- Given a situation where a reset is asserted, and then de-asserted at the
-- active flank of the clock, we can observe the difference between a
-- synchronous reset and an asynchronous reset:
--
-- === Synchronous reset
--
-- > registerS
-- >   :: Clock domain gated -> Reset domain Synchronous
-- >   -> Signal domain Int -> Signal domain Int
-- > registerS = register
--
-- >>> printX (sampleN 4 (registerS (clockGen @System) (syncResetGen @System) 0 (fromList [1,2,3])))
-- [X,0,2,3]
--
-- === Asynchronous reset
--
-- > registerA
-- >   :: Clock domain gated -> Reset domain Asynchronous
-- >   -> Signal domain Int -> Signal domain Int
-- > registerA = register
--
-- >>> sampleN 4 (registerA (clockGen @System) (asyncResetGen @System) 0 (fromList [1,2,3]))
-- [0,1,2,3]
data ResetKind
  = Synchronous
  -- ^ Components with a synchronous reset port produce the reset value when:
  --
  --     * The reset is asserted during the active flank of the clock to which
  --       the component is synchronized.
  | Asynchronous
  -- ^ Components with an asynchronous reset port produce the reset value when:
  --
  --     * Immediately when the reset is asserted.
  deriving (Eq,Ord,Show,Generic,NFData)

-- | A reset signal belonging to a @domain@.
--
-- The underlying representation of resets is 'Bool'. Note that all components
-- in the __clash-prelude__ package have an /active-high/ reset port, i.e., the
-- component is reset when the reset port is 'True'.
data Reset (domain :: Domain) (synchronous :: ResetKind) where
  Sync  :: Signal domain Bool -> Reset domain 'Synchronous
  Async :: Signal domain Bool -> Reset domain 'Asynchronous

-- | 'unsafeFromAsyncReset' is unsafe because it can introduce:
--
-- * <Clash-Explicit-Signal.html#metastability meta-stability>
unsafeFromAsyncReset :: Reset domain 'Asynchronous -> Signal domain Bool
unsafeFromAsyncReset (Async r) = r
{-# NOINLINE unsafeFromAsyncReset #-}

-- | 'unsafeToAsyncReset' is unsafe because it can introduce:
--
-- * combinational loops
--
-- === __Example__
--
-- @
-- resetSynchronizer
--   :: Clock domain gated
--   -> Reset domain 'Asynchronous
--   -> Reset domain 'Asynchronous
-- resetSynchronizer clk rst  =
--   let r1 = register clk rst True (pure False)
--       r2 = register clk rst True r1
--   in  'unsafeToAsyncReset' r2
-- @
unsafeToAsyncReset :: Signal domain Bool -> Reset domain 'Asynchronous
unsafeToAsyncReset r = Async r
{-# NOINLINE unsafeToAsyncReset #-}

-- | It is safe to treat synchronous resets as @Bool@ signals
fromSyncReset :: Reset domain 'Synchronous -> Signal domain Bool
fromSyncReset (Sync r) = r
{-# NOINLINE fromSyncReset #-}

-- | 'unsafeToSyncReset' is unsafe because:
--
-- * It can lead to <Clash-Explicit-Signal.html#metastability meta-stability>
-- issues in the presence of asynchronous resets.
unsafeToSyncReset :: Signal domain Bool -> Reset domain 'Synchronous
unsafeToSyncReset r = Sync r
{-# NOINLINE unsafeToSyncReset #-}

infixr 2 .||.
-- | The above type is a generalisation for:
--
-- @
-- __(.||.)__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('||') that returns a 'Clash.Signal.Signal' of 'Bool'
(.||.) :: Applicative f => f Bool -> f Bool -> f Bool
(.||.) = liftA2 (||)

infixr 3 .&&.
-- | The above type is a generalisation for:
--
-- @
-- __(.&&.)__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('&&') that returns a 'Clash.Signal.Signal' of 'Bool'
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
  :: (HasCallStack, Undefined a)
  => Clock  domain gated
  -> Signal domain a
  -> Signal domain a
delay# Clock {} =
  \s -> withFrozenCallStack (deepErrorX "delay: initial value undefined") :- s

delay# (GatedClock _ _ en) =
    go (withFrozenCallStack (deepErrorX "delay: initial value undefined")) en
  where
    go o (e :- es) as@(~(x :- xs)) =
      -- See [Note: register strictness annotations]
      o `seqX` o :- (as `seq` if e then go x es xs else go o es xs)
{-# NOINLINE delay# #-}

register#
  :: (HasCallStack, Undefined a)
  => Clock domain gated
  -> Reset domain synchronous
  -> a
  -> Signal domain a
  -> Signal domain a
register# Clock {} (Sync rst) i =
    go (withFrozenCallStack (deepErrorX "register: initial value undefined")) rst
  where
    go o rt@(~(r :- rs)) as@(~(x :- xs)) =
      let o' = if r then i else x
          -- [Note: register strictness annotations]
      in  o `seqX` o :- (rt `seq` as `seq` go o' rs xs)

register# Clock {} (Async rst) i =
    go (withFrozenCallStack (deepErrorX "register: initial value undefined")) rst
  where
    go o (r :- rs) as@(~(x :- xs)) =
      let o' = if r then i else o
          -- [Note: register strictness annotations]
      in  o' `seqX` o' :- (as `seq` go x rs xs)

register# (GatedClock _ _ ena) (Sync rst)  i =
    go (withFrozenCallStack (deepErrorX "register: initial value undefined")) rst ena
  where
    go o rt@(~(r :- rs)) enas@(~(e :- es)) as@(~(x :- xs)) =
      let oE = if e then x else o
          oR = if r then i else oE
          -- [Note: register strictness annotations]
      in  o `seqX` o :- (rt `seq` enas `seq` as `seq` go oR rs es xs)

register# (GatedClock _ _ ena) (Async rst) i =
    go (withFrozenCallStack (deepErrorX "register: initial value undefined")) rst ena
  where
    go o (r :- rs) enas@(~(e :- es)) as@(~(x :- xs)) =
      let oR = if r then i else o
          oE = if e then x else oR
          -- [Note: register strictness annotations]
      in  oR `seqX` oR :- (as `seq` enas `seq` go oE rs es xs)
{-# NOINLINE register# #-}

{-# INLINE mux #-}
-- | The above type is a generalisation for:
--
-- @
-- __mux__ :: 'Clash.Signal.Signal' 'Bool' -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a
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
-- __(.==.)__ :: 'Eq' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('==') that returns a 'Clash.Signal.Signal' of 'Bool'
(.==.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(.==.) = liftA2 (==)

infix 4 ./=.
-- | The above type is a generalisation for:
--
-- @
-- __(./=.)__ :: 'Eq' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('/=') that returns a 'Clash.Signal.Signal' of 'Bool'
(./=.) :: (Eq a, Applicative f) => f a -> f a -> f Bool
(./=.) = liftA2 (/=)

infix 4 .<.
-- | The above type is a generalisation for:
--
-- @
-- __(.<.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('<') that returns a 'Clash.Signal.Signal' of 'Bool'
(.<.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<.) = liftA2 (<)

infix 4 .<=.
-- | The above type is a generalisation for:
--
-- @
-- __(.<=.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('<=') that returns a 'Clash.Signal.Signal' of 'Bool'
(.<=.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.<=.) = liftA2 (<=)

infix 4 .>.
-- | The above type is a generalisation for:
--
-- @
-- __(.>.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
-- It is a version of ('>') that returns a 'Clash.Signal.Signal' of 'Bool'
(.>.) :: (Ord a, Applicative f) => f a -> f a -> f Bool
(.>.) = liftA2 (>)

infix 4 .>=.
-- | The above type is a generalisation for:
--
-- @
-- __(.>=.)__ :: 'Ord' a => 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' 'Bool'
-- @
--
--  It is a version of ('>=') that returns a 'Clash.Signal.Signal' of 'Bool'
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
-- __testFor__ :: 'Int' -> 'Clash.Signal.Signal' Bool -> 'Property'
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
-- __sample__ :: 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
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
-- __sampleN__ :: Int -> 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get a list of @n@ samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
sampleN :: (Foldable f, NFData a) => Int -> f a -> [a]
sampleN n = take n . sample

-- | Create a 'Clash.Signal.Signal' from a list
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

-- | Simulate a (@'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register systemClockGen systemResetGen 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesisable
simulate :: (NFData a, NFData b) => (Signal domain1 a -> Signal domain2 b) -> [a] -> [b]
simulate f = sample . f . fromList

-- | The above type is a generalisation for:
--
-- @
-- __sample__ :: 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
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
-- __sampleN__ :: Int -> 'Clash.Signal.Signal' a -> [a]
-- @
--
-- Get a list of @n@ samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Clash.Signal.Signal'
-- at consecutive clock cycles
--
-- > sampleN 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesisable
sampleN_lazy :: Foldable f => Int -> f a -> [a]
sampleN_lazy n = take n . sample_lazy

-- | Create a 'Clash.Signal.Signal' from a list
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

-- | Simulate a (@'Clash.Signal.Signal' a -> 'Clash.Signal.Signal' b@) function
-- given a list of samples of type @a@
--
-- >>> simulate (register systemClockGen systemResetGen 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesisable
simulate_lazy :: (Signal domain1 a -> Signal domain2 b) -> [a] -> [b]
simulate_lazy f = sample_lazy . f . fromList_lazy
