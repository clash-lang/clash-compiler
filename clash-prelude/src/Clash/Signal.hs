{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2019, Myrtle Software Ltd,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Clash has synchronous 'Signal's in the form of:

@
'Signal' (dom :: 'Domain') a
@

Where /a/ is the type of the value of the 'Signal', for example /Int/ or /Bool/,
and /dom/ is the /clock-/ (and /reset-/) domain to which the memory elements
manipulating these 'Signal's belong.

The type-parameter, /dom/, is of the kind 'Domain' - a simple string. That
string refers to a single /synthesis domain/. A synthesis domain describes the
behavior of certain aspects of memory elements in it. More specifically, a
domain looks like:

@
'DomainConfiguration'
  { _name :: 'Domain'
  -- ^ Domain name
  , _period :: 'Nat'
  -- ^ Clock period in /ps/
  , _edge :: 'ActiveEdge'
  -- ^ Active edge of the clock
  , _reset :: 'ResetKind'
  -- ^ Whether resets are synchronous (edge-sensitive) or asynchronous (level-sensitive)
  , _init :: 'InitBehavior'
  -- ^ Whether the initial (or "power up") value of memory elements is
  -- unknown/undefined, or configurable to a specific value
  , _polarity :: ResetPolarity
  -- ^ Whether resets are active high or active low
  }
@

Check the documentation of each of the types to see the various options Clash
provides. In order to specify a domain, an instance of 'KnownDomain' should be
made. Clash provides an implementation 'System' with some common options
chosen:

@
instance KnownDomain "System" ('DomainConfiguration "System" 10000 'Rising 'Asynchronous 'Defined 'ActiveHigh) where
  knownDomain = SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh
@

In words, "System" is a synthesis domain with a clock running with a period
of 10000 /ps/. Memory elements respond to the rising edge of the clock,
asynchronously to changes in their resets, and have defined power up values
if applicable.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that goes any faster!
* __NB__: Whether 'System' has good defaults depends on your target platform.
Check out 'IntelSystem' and 'XilinxSystem' too!
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Signal
  ( -- * Synchronous signals
    Signal
  , BiSignalIn
  , BiSignalOut
  , BiSignalDefault(..)
    -- * Domain
  , Domain
  , KnownDomain(..)
  , ActiveEdge(..)
  , SActiveEdge(..)
  , InitBehavior(..)
  , SInitBehavior(..)
  , ResetKind(..)
  , SResetKind(..)
  , ResetPolarity(..)
  , DomainConfiguration(..)
  , SDomainConfiguration(..)
    -- ** Default domains
  , System
  , XilinxSystem
  , IntelSystem
  , vSystem
  , vIntelSystem
  , vXilinxSystem
    -- ** Domain utilities
  , VDomainConfiguration(..)
  , vDomain
  , createDomain
  , knownVDomain
    -- * Clock
  , Clock
    -- ** Synchronization primitive
  , unsafeSynchronizer
    -- * Reset
  , Reset(..)
  , unsafeToReset
  , unsafeFromReset
  , unsafeToHighPolarity
  , unsafeToLowPolarity
  , unsafeFromHighPolarity
  , unsafeFromLowPolarity
  , E.resetSynchronizer
    -- ** Enabling
  , Enable(..)
  , toEnable
  , fromEnable
  , S.enableGen
    -- * Hidden clocks and resets
    -- $hiddenclockandreset

    -- ** Hidden clock
  , HiddenClock
  , hideClock
  , exposeClock
  , withClock
  , hasClock
    -- ** Hidden reset
  , HiddenReset
  , hideReset
  , exposeReset
  , withReset
  , hasReset
    -- ** Hidden enable
  , HiddenEnable
  , hideEnable
  , exposeEnable
  , withEnable
  , hasEnable
    -- ** Hidden clock, reset, and enable
  , HiddenClockResetEnable
  , hideClockResetEnable
  , exposeClockResetEnable
  , withClockResetEnable
  , SystemClockResetEnable
    -- * Basic circuit functions
  , dflipflop
  , delay
  , delayMaybe
  , delayEn
  , register
  , regMaybe
  , regEn
  , mux
    -- * Simulation and testbench functions
  , clockGen
  , tbClockGen
  , tbEnableGen
  , resetGen
  , systemClockGen
  , tbSystemClockGen
  , systemResetGen
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Product/Signal isomorphism
  , Bundle(..)
    -- * Simulation functions (not synthesizable)
  , simulate
  , simulateB
    -- ** lazy versions
  , simulate_lazy
  , simulateB_lazy
    -- * List \<-\> Signal conversion (not synthesizable)
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
    -- * Bisignal functions
  , veryUnsafeToBiSignalIn
  , readFromBiSignal
  , writeToBiSignal
  , mergeBiSignalOuts
  )
where

import           GHC.TypeLits
  (KnownNat, KnownSymbol, AppendSymbol, Symbol)
import           Data.Bits             (Bits) -- Haddock only
import           Data.Maybe            (isJust, fromJust)
import           Prelude
import           Test.QuickCheck       (Property, property)

import qualified Clash.Explicit.Signal as E
import           Clash.Explicit.Signal
  (System, resetSynchronizer, systemClockGen, systemResetGen, tbSystemClockGen)
import qualified Clash.Explicit.Signal as S
import           Clash.Hidden
import           Clash.Promoted.Nat    (SNat (..))
import           Clash.Promoted.Symbol (SSymbol (..))
import           Clash.Signal.Bundle   (Bundle (..))
import           Clash.Signal.BiSignal --(BisignalIn, BisignalOut, )
import           Clash.Signal.Internal hiding
  (sample, sample_lazy, sampleN, sampleN_lazy, simulate, simulate_lazy, testFor)
import qualified Clash.Signal.Internal as S
import           Clash.XException      (Undefined)

{- $setup
>>> :set -XFlexibleContexts -XTypeApplications
>>> import Clash.XException (printX)
>>> import Control.Applicative (liftA2)
>>> let oscillate = register False (not <$> oscillate)
>>> let count = regEn 0 oscillate (count + 1)
>>> :{
sometimes1 = s where
  s = register Nothing (switch <$> s)
  switch Nothing = Just 1
  switch _       = Nothing
:}

>>> :{
countSometimes = s where
  s     = regMaybe 0 (plusM (pure <$> s) sometimes1)
  plusM = liftA2 (liftA2 (+))
:}

-}

-- * Hidden clock and reset arguments

{- $hiddenclockandreset #hiddenclockandreset#
Clocks and resets are by default implicitly routed to their components. You can
see from the type of a component whether it has hidden clock or reset
arguments:

It has a hidden clock when it has a:

@
f :: 'HiddenClock' dom => ...
@

Constraint.

Or it has a hidden reset when it has a:

@
g :: 'HiddenReset' dom polarity => ...
@

Constraint.

Or it has both a hidden clock argument and a hidden reset argument when it
has a:

@
h :: 'HiddenClockReset' dom  => ..
@

Constraint.

Given a component with an explicit clock and reset arguments, you can turn them
into hidden arguments using 'hideClock' and 'hideReset'. So given a:

@
f :: Clock dom -> Reset dom -> Signal dom a -> ...
@

You hide the clock and reset arguments by:

@
-- g :: 'HiddenClockReset' dom  => Signal dom a -> ...
g = 'hideClockReset' f
@

Or, alternatively, by:

@
-- h :: HiddenClockResetEnable dom conf => Signal dom a -> ...
h = f 'hasClock' 'hasReset'
@

=== Assigning explicit clock and reset arguments to hidden clocks and resets

Given a component:

@
f :: HiddenClockResetEnable dom conf
  => Signal dom Int
  -> Signal dom Int
@

which has hidden clock and routed reset arguments, we expose those hidden
arguments so that we can explicitly apply them:

@
-- g :: Clock dom -> Reset dom -> Signal dom Int -> Signal dom Int
g = 'exposeClockResetEnable' f
@

or, alternatively, by:

@
-- h :: Clock dom -> Reset dom -> Signal dom Int -> Signal dom Int
h clk rst = withClock clk rst f
@

Similarly, there are 'exposeClock' and 'exposeReset' to connect just expose
the hidden clock or the hidden reset argument.

You will need to explicitly apply clocks and resets when you want to use
components such as PPLs and 'resetSynchronizer':

@
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
  -> Signal System Int
topEntity clk rst =
  let (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' (SSymbol \@\"altpll50\") clk rst
      rstSync            = 'resetSynchronizer' pllOut ('unsafeToAsyncReset' pllStable)
  in  'exposeClockResetEnable' f pllOut rstSync
@

or, using the alternative method:

@
topEntity2
  :: Clock System
  -> Reset System
  -> Signal System Int
  -> Signal System Int
topEntity2 clk rst =
  let (pllOut,pllStable) = 'Clash.Intel.ClockGen.altpll' (SSymbol \@\"altpll50\") clk rst
      rstSync            = 'resetSynchronizer' pllOut ('unsafeToAsyncReset' pllStable)
  in  'withClockReset' pllOut rstSync f
@

-}

type HiddenClockName dom = AppendSymbol dom "_clk"
type HiddenResetName dom = AppendSymbol dom "_rst"
type HiddenEnableName dom = AppendSymbol dom "_en"

-- | A /constraint/ that indicates the component has a hidden 'Clock'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenClock dom conf =
  ( Hidden (HiddenClockName dom) (Clock dom)
  , KnownDomain dom conf )

-- | A /constraint/ that indicates the component needs a 'Reset'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenReset dom conf =
  ( Hidden (HiddenResetName dom) (Reset dom)
  , KnownDomain dom conf )

-- | A /constraint/ that indicates the component needs a 'Enable'
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenEnable dom conf =
  ( Hidden (HiddenEnableName dom) (Enable dom)
  , KnownDomain dom conf )

-- | A /constraint/ that indicates the component needs a 'Clock', a 'Reset',
-- and an 'Enable' belonging to the same dom.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type HiddenClockResetEnable dom conf =
  ( HiddenClock dom conf
  , HiddenReset dom conf
  , HiddenEnable dom conf
  )

-- | A /constraint/ that indicates the component needs a 'Clock', a 'Reset',
-- and an 'Enable' belonging to the 'System' dom.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
type SystemClockResetEnable =
  ( Hidden (HiddenClockName System) (Clock System)
  , Hidden (HiddenResetName System) (Reset System)
  , Hidden (HiddenEnableName System) (Enable System)
  )

-- | Expose the hidden 'Clock' argument of a component, so it can be applied
-- explicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
exposeClock
  :: forall dom conf  r
   . (HiddenClock dom conf => r)
  -- ^ The component with a hidden clock
  -> (KnownDomain dom conf => Clock dom -> r)
  -- ^ The component with its clock argument exposed
exposeClock = \f clk -> expose @(HiddenClockName dom) f clk
{-# INLINE exposeClock #-}

-- | Hide the 'Clock' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideClock
  :: forall dom conf  r
   . HiddenClock dom conf
  => (Clock dom -> r)
  -- ^ Function whose clock argument you want to hide
  -> r
hideClock = \f -> f (fromLabel @(HiddenClockName dom))
{-# INLINE hideClock #-}

-- | Connect an explicit 'Clock' to a function with a hidden 'Clock' argument.
--
-- @
-- withClock = 'flip' exposeClock
-- @
withClock
  :: forall dom conf  r
   . KnownDomain dom conf
  => Clock dom
  -- ^ The 'Clock' we want to connect
  -> (HiddenClock dom conf => r)
  -- ^ The function with a hidden 'Clock' argument
  -> r
withClock = \clk f -> expose @(HiddenClockName dom) f clk
{-# INLINE withClock #-}

-- | Connect a hidden 'Clock' to an argument where a normal 'Clock' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasClock
  :: forall dom conf
   . HiddenClock dom conf
  => Clock dom
hasClock = fromLabel @(HiddenClockName dom)
{-# INLINE hasClock #-}

-- | Expose the hidden 'Reset' argument of a component, so it can be applied
-- explicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
exposeReset
  :: forall dom conf r
   . (HiddenReset dom conf => r)
  -- ^ The component with a hidden reset
  -> (KnownDomain dom conf => Reset dom -> r)
  -- ^ The component with its reset argument exposed
exposeReset = \f rst -> expose @(HiddenResetName dom) f rst
{-# INLINE exposeReset #-}

-- | Hide the 'Reset' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideReset
  :: forall dom conf r
   . HiddenReset dom conf
  => (Reset dom -> r)
  -- ^ Component whose reset argument you want to hide
  -> r
hideReset = \f -> f (fromLabel @(HiddenResetName dom))
{-# INLINE hideReset #-}

-- | Connect an explicit 'Reset' to a function with a hidden 'Reset' argument.
--
-- @
-- withReset = 'flip' exposeReset
-- @
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
withReset
  :: forall dom conf r
   . KnownDomain dom conf
  => Reset dom
  -- ^ The 'Reset' we want to connect
  -> (HiddenReset dom conf => r)
  -- ^ The function with a hidden 'Reset' argument
  -> r
withReset = \rst f -> expose @(HiddenResetName dom) f rst
{-# INLINE withReset #-}

-- | Connect a hidden 'Reset' to an argument where a normal 'Reset' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasReset
  :: forall dom conf
   . HiddenReset dom conf
  => Reset dom
hasReset = fromLabel @(HiddenResetName dom)
{-# INLINE hasReset #-}

-- | Expose the hidden 'Enable' argument of a component, so it can be applied
-- explicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
exposeEnable
  :: forall dom conf r
   . (HiddenEnable dom conf => r)
  -- ^ The component with a hidden reset
  -> (KnownDomain dom conf => Enable dom -> r)
  -- ^ The component with its reset argument exposed
exposeEnable = \f rst -> expose @(HiddenEnableName dom) f rst
{-# INLINE exposeEnable #-}

-- | Hide the 'Enable' argument of a component, so it can be routed implicitly.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideEnable
  :: forall dom conf r
   . HiddenEnable dom conf
  => (Enable dom -> r)
  -- ^ Component whose reset argument you want to hide
  -> r
hideEnable = \f -> f (fromLabel @(HiddenEnableName dom))
{-# INLINE hideEnable #-}

-- | Connect an explicit 'Enable' to a function with a hidden 'Enable' argument.
--
-- @
-- withEnable = 'flip' exposeEnable
-- @
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
withEnable
  :: forall dom conf r
   . KnownDomain dom conf
  => Enable dom
  -- ^ The 'Enable' we want to connect
  -> (HiddenEnable dom conf => r)
  -- ^ The function with a hidden 'Enable' argument
  -> r
withEnable = \rst f -> expose @(HiddenEnableName dom) f rst
{-# INLINE withEnable #-}

-- | Connect a hidden 'Enable' to an argument where a normal 'Enable' argument
-- was expected.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hasEnable
  :: forall dom conf
   . HiddenEnable dom conf
  => Enable dom
hasEnable = fromLabel @(HiddenEnableName dom)
{-# INLINE hasEnable #-}


-- | Expose the hidden 'Clock', 'Reset', and 'Enable' arguments of a component,
-- so they can be applied explicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
--
-- === __Example__
--
-- @
-- topEntity :: Vec 2 (Vec 3 (Unsigned 8)) -> Vec 6 (Unsigned 8)
-- topEntity = concat
--
-- testBench :: Signal System Bool
-- testBench = done
--   where
--     testInput      = pure ((1 :> 2 :> 3 :> Nil) :> (4 :> 5 :> 6 :> Nil) :> Nil)
--     expectedOutput = outputVerifier ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = exposeClockResetEnable (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = tbSystemClockGen (not <\$\> done)
--     rst            = systemResetGen
-- @
exposeClockResetEnable
  :: forall dom conf r
   . (HiddenClockResetEnable dom conf => r)
  -- ^ The component with hidden clock, reset, and enable arguments
  -> (KnownDomain dom conf => Clock dom -> Reset dom -> Enable dom -> r)
  -- ^ The component with its clock, reset, and enable arguments exposed
exposeClockResetEnable =
  \f clk rst en ->
    expose
      @(HiddenEnableName dom)
      ( expose
          @(HiddenResetName dom)
          ( expose
              @(HiddenClockName dom)
              f
              clk )
          rst
      )
      en
{-# INLINE exposeClockResetEnable #-}

-- -- | Hide the 'Clock' and 'Reset' arguments of a component, so they can be
-- -- routed implicitly
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
hideClockResetEnable
  :: forall dom conf r
   . HiddenClockResetEnable dom conf
  => (KnownDomain dom conf => Clock dom -> Reset dom -> Enable dom -> r)
  -- ^ Component whose clock, reset, and enable argument you want to hide
  -> r
hideClockResetEnable =
  \f ->
    f
      (fromLabel @(HiddenClockName dom))
      (fromLabel @(HiddenResetName dom))
      (fromLabel @(HiddenEnableName dom))
{-# INLINE hideClockResetEnable #-}

-- | Connect an explicit 'Clock' and 'Reset' to a function with a hidden
-- 'Clock' and 'Reset' argument. In order for this function to work as
-- expected, you need to explicitly relate the /dom/ in the arguments with
-- the result. You can do this either by supplying a type signature:
--
-- @
-- h :: Clock System -> Reset System -> Signal System Int
-- h = withClockReset clk rst f
-- @
--
-- Or by type applying both sides:
--
-- @
-- g = withClockReset @System clk rst (f @System)
-- @
--
-- If omitted, GHC / Clash will fail to infer the type you might expect. This
-- is a known limitation in GHC / Haskell for now. Monomorphism isn't
-- a requirement. I.e., the following is fine:
--
-- @
-- i :: Clock dom -> Reset dom -> Signal dom Int
-- i = withClockReset clk rst f
-- @
--
-- as well as
--
-- @
-- j = withClockReset @dom clk rst (f @dom)
-- @
--
-- .. as they both explicitly "connect" the /dom/ in the arguments and result.
--
-- <Clash-Signal.html#hiddenclockandreset Click here to read more about hidden clocks, resets, and enables>
withClockResetEnable
  :: forall dom r conf
   . KnownDomain dom conf
  => Clock dom
  -- ^ The 'Clock' we want to connect
  -> Reset dom
  -- ^ The 'Reset' we want to connect
  -> Enable dom
  -- ^ The 'Enable' we want to connect
  -> (HiddenClockResetEnable dom conf => r)
  -- ^ The function with a hidden 'Clock', hidden 'Reset', and hidden
  -- 'Enable' argument
  -> r
withClockResetEnable =
  \clk rst en f ->
    expose
      @(HiddenEnableName dom)
      ( expose
          @(HiddenResetName dom)
          ( expose
              @(HiddenClockName dom)
              f
              clk)
          rst )
      en
{-# INLINE withClockResetEnable #-}

-- * Basic circuit functions

-- | Special version of 'delay' that doesn't take enable signals of any kind.
-- Initial value will be undefined.
dflipflop
  :: forall dom conf a
   . ( HiddenClock dom conf
     , Undefined a )
  => Signal dom a
  -> Signal dom a
dflipflop =
  E.dflipflop (fromLabel @(HiddenClockName dom))
{-# INLINE dflipflop #-}

-- | 'delay' @s@ delays the values in 'Signal' @s@ for once cycle, the value
-- at time 0 is /dflt/.
--
-- >>> sampleN @System 3 (delay 0 (fromList [1,2,3,4]))
-- [0,1,2]
delay
  :: forall dom conf  a
   . ( Undefined a
     , HiddenClock dom conf
     , HiddenEnable dom conf )
  => a
  -- ^ Initial value
  -> Signal dom a
  -- ^ Signal to delay
  -> Signal dom a
delay dflt i =
  delay#
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenEnableName dom))
    dflt
    i
{-# INLINE delay #-}

-- | Version of 'delay' that only updates when its second argument is a 'Just'
-- value.
--
-- >>> let input = fromList [Just 1, Just 2, Nothing, Nothing, Just 5, Just 6, Just (7::Int)]
-- >>> sampleN @System 7 (delayMaybe 0 input)
-- [0,1,2,2,2,5,6]
delayMaybe
  :: forall dom conf  a
   . ( Undefined a
     , HiddenClock dom conf
     , HiddenEnable dom conf )
  => a
  -- ^ Initial value
  -> Signal dom (Maybe a)
  -> Signal dom a
delayMaybe dflt i =
  E.delayMaybe
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenEnableName dom))
    dflt
    i
{-# INLINE delayMaybe #-}

-- | Version of 'delay' that only updates when its second argument is asserted.
--
-- >>> let input = fromList [1,2,3,4,5,6,7::Int]
-- >>> let enable = fromList [True,True,False,False,True,True,True]
-- >>> sampleN @System 7 (delayEn 0 enable input)
-- [0,1,2,2,2,5,6]
delayEn
  :: forall dom conf  a
   . ( Undefined a
     , HiddenClock dom conf
     , HiddenEnable dom conf )
  => a
  -- ^ Initial value
  -> Signal dom Bool
  -- ^ Enable
  -> Signal dom a
  -> Signal dom a
delayEn dflt en i =
  E.delayEn
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenEnableName dom))
    dflt
    en
    i
{-# INLINE delayEn #-}

-- | 'register' @i s@ delays the values in 'Signal' @s@ for one cycle, and sets
-- the value at time 0 to @i@
--
-- >>> sampleN @System 5 (register 8 (fromList [1,1,2,3,4]))
-- [8,8,1,2,3]
register
  :: forall dom conf a
   . ( HiddenClockResetEnable dom conf
     , Undefined a )
  => a
  -- ^ Reset value
  --
  -- 'register' outputs the reset value when the reset value is active
  -> Signal dom a
  -> Signal dom a
register i s =
  E.register
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenResetName dom))
    (fromLabel @(HiddenEnableName dom))
    i
    s
{-# INLINE register #-}
infixr 3 `register`

-- | Version of 'register' that only updates its content when its second
-- argument is a 'Just' value. So given:
--
-- @
-- sometimes1 = s where
--   s = 'register' Nothing (switch '<$>' s)
--
--   switch Nothing = Just 1
--   switch _       = Nothing
--
-- countSometimes = s where
--   s     = 'regMaybe' 0 (plusM ('pure' '<$>' s) sometimes1)
--   plusM = 'liftA2' (liftA2 (+))
-- @
--
-- We get:
--
-- >>> sampleN @System 9 sometimes1
-- [Nothing,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1]
-- >>> sampleN @System 9 countSometimes
-- [0,0,0,1,1,2,2,3,3]
regMaybe
  :: forall dom conf a
   . ( HiddenClockResetEnable dom conf
     , Undefined a )
  => a
  -- ^ Reset value. 'regMaybe' outputs the reset value when the reset is active.
  -> Signal dom (Maybe a)
  -> Signal dom a
regMaybe initial iM =
  E.regMaybe
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenResetName dom))
    (fromLabel @(HiddenEnableName dom))
    initial
    iM
{-# INLINE regMaybe #-}
infixr 3 `regMaybe`

-- | Version of 'register' that only updates its content when its second argument
-- is asserted. So given:
--
-- @
-- oscillate = 'register' False ('not' '<$>' oscillate)
-- count     = 'regEn' 0 oscillate (count + 1)
-- @
--
-- We get:
--
-- >>> sampleN @System 9 oscillate
-- [False,False,True,False,True,False,True,False,True]
-- >>> sampleN @System 9 count
-- [0,0,0,1,1,2,2,3,3]
regEn
  :: forall dom conf a
   . ( HiddenClockResetEnable dom conf
     , Undefined a )
  => a
  -- ^ Reset value
  --
  -- 'regEn' outputs the reset value when the reset value is active
  -> Signal dom Bool
  -> Signal dom a
  -> Signal dom a
regEn initial en i =
  E.regEn
    (fromLabel @(HiddenClockName dom))
    (fromLabel @(HiddenResetName dom))
    (fromLabel @(HiddenEnableName dom))
    initial
    en
    i
{-# INLINE regEn #-}

-- * Signal -> List conversion

-- | Get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesizable
sample
  :: forall dom conf a
   . ( KnownDomain dom conf
     , Undefined a )
  => (HiddenClockResetEnable dom conf => Signal dom a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sample s =
  case knownDomain @dom of
    SDomainConfiguration dom _ _ _ _ _ ->
      let clk = Clock dom in
      let rst = Reset dom (True :- pure False) in
      let en = S.enableGen in
      S.sample (exposeClockResetEnable s clk rst en)
{-# NOINLINE sample #-}

-- | Get a list of /n/ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sampleN @System 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesizable
sampleN
  :: forall dom conf a
   . ( KnownDomain dom conf
     , Undefined a )
  => Int
  -- ^ The number of samples we want to see
  -> (HiddenClockResetEnable dom conf => Signal dom a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sampleN n s =
  case knownDomain @dom of
    SDomainConfiguration dom _ _ _ _ _ ->
      let clk = Clock dom in
      let rst = Reset dom (True :- pure False) in
      let en = S.enableGen in
      S.sampleN n (exposeClockResetEnable s clk rst en)
{-# NOINLINE sampleN #-}

-- | /Lazily/ get an infinite list of samples from a 'Clash.Signal.Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sample s == [s0, s1, s2, s3, ...
--
-- __NB__: This function is not synthesizable
sample_lazy
  :: forall dom conf a
   . KnownDomain dom conf
  => (HiddenClockResetEnable dom conf => Signal dom a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sample_lazy s =
  case knownDomain @dom of
    SDomainConfiguration dom _ _ _ _ _ ->
      let clk = Clock dom in
      let rst = Reset dom (True :- pure False) in
      let en = S.enableGen in
      S.sample_lazy (exposeClockResetEnable s clk rst en)
{-# NOINLINE sample_lazy #-}

-- | Lazily get a list of /n/ samples from a 'Signal'
--
-- The elements in the list correspond to the values of the 'Signal'
-- at consecutive clock cycles
--
-- > sampleN @System 3 s == [s0, s1, s2]
--
-- __NB__: This function is not synthesizable
sampleN_lazy
  :: forall dom conf a
   . KnownDomain dom conf
  => Int
  -> (HiddenClockResetEnable dom conf => Signal dom a)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
sampleN_lazy n s =
  case knownDomain @dom of
    SDomainConfiguration dom _ _ _ _ _ ->
      let clk = Clock dom in
      let rst = Reset dom (True :- pure False) in
      let en = S.enableGen in
      S.sampleN_lazy n (exposeClockResetEnable s clk rst en)
{-# NOINLINE sampleN_lazy #-}

-- * Simulation functions

-- | Simulate a (@'Signal' a -> 'Signal' b@) function given a list of samples
-- of type /a/
--
-- >>> simulate @System (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- Where 'System' denotes the /domain/ to simulate on.
--
-- __NB__: This function is not synthesizable
simulate
  :: forall dom conf a b
   . ( KnownDomain dom conf
     , Undefined a
     , Undefined b )
  => (HiddenClockResetEnable dom conf =>
      Signal dom a -> Signal dom b)
  -- ^ 'Signal' we want to sample, whose source potentially has a hidden clock
  -- (and reset)
  -> [a]
  -> [b]
simulate f =
  case knownDomain @dom of
    SDomainConfiguration dom _ _ _ _ _ ->
      let clk = Clock dom in
      let rst = Reset dom (True :- pure False) in
      let en = S.enableGen in
      -- TODO: Explain why we skip the the first value here
      tail . S.simulate (exposeClockResetEnable f clk rst en) . dup1
{-# NOINLINE simulate #-}

-- | /Lazily/ simulate a (@'Signal' a -> 'Signal' b@) function given a list of
-- samples of type /a/
--
-- >>> simulate @System (register 8) [1, 2, 3]
-- [8,1,2,3...
-- ...
--
-- __NB__: This function is not synthesizable
simulate_lazy
  :: forall dom conf a b
   . KnownDomain dom conf
  => (HiddenClockResetEnable dom conf =>
      Signal dom a -> Signal dom b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulate_lazy f =
  case knownDomain @dom of
    SDomainConfiguration dom _ _ _ _ _ ->
      let clk = Clock dom in
      let rst = Reset dom (True :- pure False) in
      let en = S.enableGen in
      tail . S.simulate_lazy (exposeClockResetEnable f clk rst en) . dup1
{-# NOINLINE simulate_lazy #-}

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type @a@
--
-- >>> simulateB @System (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB
  :: forall dom conf a b
   . ( KnownDomain dom conf
     , Bundle a
     , Bundle b
     , Undefined a
     , Undefined b )
  => (HiddenClockResetEnable dom conf =>
      Unbundled dom a -> Unbundled dom b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulateB f =
  case knownDomain @dom of
    SDomainConfiguration dom _ _ _ _ _ ->
      let clk = Clock dom in
      let rst = Reset dom (True :- pure False) in
      let en = S.enableGen in
      tail . S.simulateB (exposeClockResetEnable f clk rst en) . dup1
{-# NOINLINE simulateB #-}

-- | /Lazily/ simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a
-- list of samples of type @a@
--
-- >>> simulateB @System (unbundle . register (8,8) . bundle) [(1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB_lazy
  :: forall dom conf a b
   . ( KnownDomain dom conf
     , Bundle a
     , Bundle b )
  => (HiddenClockResetEnable dom conf =>
      Unbundled dom a -> Unbundled dom b)
  -- ^ Function we want to simulate, whose components potentially have a hidden
  -- clock (and reset)
  -> [a]
  -> [b]
simulateB_lazy f =
  case knownDomain @dom of
    SDomainConfiguration dom _ _ _ _ _ ->
      let clk = Clock dom in
      let rst = Reset dom (True :- pure False) in
      let en = S.enableGen in
      tail . S.simulateB_lazy (exposeClockResetEnable f clk rst en) . dup1
{-# NOINLINE simulateB_lazy #-}

dup1 :: [a] -> [a]
dup1 (x:xs) = x:x:xs
dup1 _      = error "empty list"

-- * QuickCheck combinators

-- |  @testFor n s@ tests the signal /s/ for /n/ cycles.
testFor
  :: KnownDomain dom conf
  => Int
  -- ^ The number of cycles we want to test for
  -> (HiddenClockResetEnable dom conf => Signal dom Bool)
  -- ^ 'Signal' we want to evaluate, whose source potentially has a hidden clock
  -- (and reset)
  -> Property
testFor n s = property (and (Clash.Signal.sampleN n s))

-- ** Synchronization primitive
-- | Implicit version of 'Clash.Explicit.Signal.unsafeSynchronizer'.
unsafeSynchronizer
  :: forall dom1 dom2 conf1 conf2 a
   . ( HiddenClock dom1 conf1
     , HiddenClock dom2 conf2 )
  => Signal dom1 a
  -> Signal dom2 a
unsafeSynchronizer =
  hideClock (hideClock S.unsafeSynchronizer)
