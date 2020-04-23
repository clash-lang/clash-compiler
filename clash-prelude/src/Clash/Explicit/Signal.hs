{-|
Copyright  :  (C) 2013-2016, University of Twente,
                  2016-2019, Myrtle Software,
                  2017     , Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Clash has synchronous 'Signal's in the form of:

@
'Signal' (dom :: 'GHC.TypeLits.Symbol') a
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
  { _name:: 'GHC.TypeLits.Symbol'
  -- ^ Domain name
  , _period :: 'GHC.TypeLits.Nat'
  -- ^ Clock period in /ps/
  , _edge :: 'ActiveEdge'
  -- ^ Active edge of the clock
  , _reset :: 'ResetKind'
  -- ^ Whether resets are synchronous (edge-sensitive) or asynchronous (level-sensitive)
  , _init :: 'InitBehavior'
  -- ^ Whether the initial (or "power up") value of memory elements is
  -- unknown/undefined, or configurable to a specific value
  , _polarity :: 'ResetPolarity'
  -- ^ Whether resets are active high or active low
  }
@

Check the documentation of each of the types to see the various options Clash
provides. In order to specify a domain, an instance of 'KnownDomain' should be
made. Clash provides a standard implementation, called 'System', that is
configured as follows:

@
instance KnownDomain "System" where
  type KnownConf "System" = 'DomainConfiguration "System" 10000 'Rising 'Asynchronous 'Defined 'ActiveHigh
  knownDomain = 'SDomainConfiguration' SSymbol SNat 'SRising' 'SAsynchronous' 'SDefined' 'SActiveHigh'
@

In words, \"System\" is a synthesis domain with a clock running with a period
of 10000 /ps/ (100 MHz). Memory elements update their state on the rising edge
of the clock, can be reset asynchronously with regards to the clock, and have
defined power up values if applicable.

In order to create a new domain, you don't have to instantiate it explicitly.
Instead, you can have 'createDomain' create a domain for you. You can also use
the same function to subclass existing domains.

* __NB__: \"Bad things\"™  happen when you actually use a clock period of @0@,
so do __not__ do that!
* __NB__: You should be judicious using a clock with period of @1@ as you can
never create a clock that goes any faster!
* __NB__: For the best compatibility make sure your period is divisible by 2,
because some VHDL simulators don't support fractions of picoseconds.
* __NB__: Whether 'System' has good defaults depends on your target platform.
Check out 'IntelSystem' and 'XilinxSystem' too!

=== Explicit clocks and resets, and meta-stability #metastability#

When using multiple clocks and/or reset lines there are ways to accidentally
introduce situations that are prone to
<https://en.wikipedia.org/wiki/Metastability_in_electronics metastability>.
These bugs are incredibly hard to debug as they often cannot be simulated, so
it's best to prevent them in the first place. This section outlines the
situations in which metastability arises and how to prevent it.

Two types of resets exist: synchronous and asynchronous resets. These reset
types are encoded in a synthesis domain. For the following examples we assume
the following exist:

@
'DomainConfiguration' \"SyncExample\" _period _edge 'Synchronous' _init
'DomainConfiguration' \"AsyncExample\" _period _edge 'Asynchronous' _init
@

See the previous section on how to use domains.

We now go over the clock and reset line combinations and explain when they
can potentially introduce situations prone to meta-stability:

    *   /Reset situation 1/:

        @
        f :: 'Reset' \"SyncExample\" -> 'Reset' \"SyncExample\" -> ..
        f x y = ..
        @

        There are no problems here, because although /x/ and /y/ can have
        different values, components to these reset lines are reset
        /synchronously/, and there is no metastability situation.

    *   /Reset situation 2/:

        @
        g :: 'Reset' \"AsyncExample\" -> 'Reset' \"AsyncExample\" -> ..
        g x y = ..
        @

        This situation can be prone to metastability, because although /x/ and
        /y/ belong to the same /domain/ according to their domain, there is no
        guarantee that they actually originate from the same source. This means
        that one component can enter its reset state asynchronously to another
        component, inducing metastability in the other component.

    *   /Clock situation/:

        @
        k :: 'Clock' dom -> 'Clock' dom -> ..
        k x y = ..
        @

        The situation above is potentially prone to metastability, because
        even though /x/ and /y/ belong to the same /domain/ according to their
        domain, there is no guarantee that they actually originate from the same
        source. They could hence be connected to completely unrelated clock
        sources, and components can then induce metastable states in others.

-}

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.Signal
  ( -- * Synchronous signal
    Signal
  , BiSignalIn
  , BiSignalOut
  , BiSignalDefault(..)
    -- * Domain
  , Domain
  , KnownDomain(..)
  , KnownConfiguration
  , ActiveEdge(..)
  , SActiveEdge(..)
  , InitBehavior(..)
  , SInitBehavior(..)
  , ResetKind(..)
  , SResetKind(..)
  , ResetPolarity(..)
  , SResetPolarity(..)
  , DomainConfiguration(..)
  , SDomainConfiguration(..)
  -- ** Configuration type families
  , DomainPeriod
  , DomainActiveEdge
  , DomainResetKind
  , DomainInitBehavior
  , DomainResetPolarity
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
  , clockPeriod
  , activeEdge
  , resetKind
  , initBehavior
  , resetPolarity
    -- ** Enabling
  , Enable
  , toEnable
  , fromEnable
  , enableGen
    -- * Clock
  , Clock
  , freqCalc  -- DEPRECATED
  , periodToHz
  , hzToPeriod
    -- ** Synchronization primitive
  , unsafeSynchronizer
  , veryUnsafeSynchronizer
    -- * Reset
  , Reset
  , unsafeToReset
  , unsafeFromReset
  , unsafeToHighPolarity
  , unsafeToLowPolarity
  , unsafeFromHighPolarity
  , unsafeFromLowPolarity
  , convertReset
  , resetSynchronizer
  , holdReset
    -- * Basic circuit functions
  , enable
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
  , resetGen
  , resetGenN
  , systemClockGen
  , systemResetGen
    -- * Boolean connectives
  , (.&&.), (.||.)
    -- * Product/Signal isomorphism
  , Bundle(..)
  , EmptyTuple(..)
  , TaggedEmptyTuple(..)
    -- * Simulation functions (not synthesizable)
  , simulate
  , simulateB
  , simulateWithReset
  , simulateWithResetN
    -- ** lazy versions
  , simulate_lazy
  , simulateB_lazy
    -- * List \<-\> Signal conversion (not synthesizable)
  , sample
  , sampleN
  , sampleWithReset
  , sampleWithResetN
  , fromList
  , fromListWithReset
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

import           Data.Maybe                     (isJust)
import           GHC.TypeLits                   (type (+), type (<=))

import           Clash.Annotations.Primitive    (hasBlackBox)
import           Clash.Class.Num                (satSucc, SaturationMode(SatBound))
import           Clash.Promoted.Nat             (SNat(..), snatToNum)
import           Clash.Signal.Bundle
  (Bundle (..), EmptyTuple(..), TaggedEmptyTuple(..), vecBundle#)
import           Clash.Signal.BiSignal
import           Clash.Signal.Internal
import           Clash.Signal.Internal.Ambiguous
  (knownVDomain, clockPeriod, activeEdge, resetKind, initBehavior, resetPolarity)
import           Clash.Sized.Index              (Index)
import qualified Clash.Sized.Vector
import           Clash.XException               (NFDataX, deepErrorX, fromJustX)

{- $setup
>>> :set -XDataKinds -XTypeApplications -XFlexibleInstances -XMultiParamTypeClasses -XTypeFamilies
>>> :set -fno-warn-deprecations
>>> :m -Clash.Prelude
>>> :m -Clash.Prelude.Safe
>>> :m -Clash.Explicit.Prelude.Safe
>>> :m -Clash.Signal
>>> import Clash.Explicit.Prelude
>>> import Clash.Promoted.Nat (SNat(..))
>>> import qualified Data.List as L
>>> :{
instance KnownDomain "Dom2" where
  type KnownConf "Dom2" = 'DomainConfiguration "Dom2" 2 'Rising 'Asynchronous 'Defined 'ActiveHigh
  knownDomain = SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh
:}

>>> :{
instance KnownDomain "Dom7" where
  type KnownConf "Dom7" = 'DomainConfiguration "Dom7" 7 'Rising 'Asynchronous 'Defined 'ActiveHigh
  knownDomain = SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh
:}

>>> type Dom2 = "Dom2"
>>> type Dom7 = "Dom7"
>>> let clk2 = clockGen @Dom2
>>> let clk7 = clockGen @Dom7
>>> let en2 = enableGen @Dom2
>>> let en7 = enableGen @Dom7
>>> let oversampling clkA clkB enA enB dflt = delay clkB enB dflt . unsafeSynchronizer clkA clkB . delay clkA enA dflt
>>> let almostId clkA clkB enA enB dflt = delay clkB enB dflt . unsafeSynchronizer clkA clkB . delay clkA enA dflt . unsafeSynchronizer clkB clkA . delay clkB enB dflt
>>> let oscillate clk rst en = let s = register clk rst en False (not <$> s) in s
>>> let count clk rst en = let s = regEn clk rst en 0 (oscillate clk rst en) (s + 1) in s
>>> :{
sometimes1 clk rst en = s where
  s = register clk rst en Nothing (switch <$> s)
  switch Nothing = Just 1
  switch _       = Nothing
:}

>>> :{
countSometimes clk rst en = s where
  s = regMaybe clk rst en 0 (plusM (pure <$> s) (sometimes1 clk rst en))
  plusM = liftA2 (liftA2 (+))
:}

-}

-- **Clock
-- | Clock generator for the 'System' clock domain.
--
-- __NB__: should only be used for simulation, and __not__ for the /testBench/
-- function. For the /testBench/ function, used 'tbSystemClockGen'
systemClockGen
  :: Clock System
systemClockGen = clockGen

-- | Reset generator for the 'System' clock domain.
--
-- __NB__: should only be used for simulation or the \testBench\ function.
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
--     expectedOutput = outputVerifier' ((1:>2:>3:>4:>5:>6:>Nil):>Nil)
--     done           = exposeClockResetEnable (expectedOutput (topEntity <$> testInput)) clk rst
--     clk            = tbSystemClockGen (not <\$\> done)
--     rst            = 'systemResetGen'
-- @
systemResetGen ::Reset System
systemResetGen = resetGen

-- | Normally, asynchronous resets can be both asynchronously asserted and
-- de-asserted. Asynchronous de-assertion can induce meta-stability in the
-- component which is being reset. To ensure this doesn't happen,
-- 'resetSynchronizer' ensures that de-assertion of a reset happens
-- synchronously. Assertion of the reset remains asynchronous.
--
-- Note that asynchronous assertion does not induce meta-stability in the
-- component whose reset is asserted. However, when a component \"A\" in another
-- clock or reset domain depends on the value of a component \"B\" being
-- reset, then asynchronous assertion of the reset of component \"B"\ can induce
-- meta-stability in component \"A\". To prevent this from happening you need
-- to use a proper synchronizer, for example one of the synchronizers in
-- "Clash.Explicit.Synchronizer"
--
-- === __Example__
--
-- @
-- topEntity
--   :: Clock  System
--   -> Reset  System
--   -> Signal System Bit
--   -> Signal System (BitVector 8)
-- topEntity clk rst key1 =
--     let  (pllOut,pllStable) = altpll (SSymbol @"altpll50") clk rst
--          rstSync            = 'resetSynchronizer' pllOut (unsafeToHighPolarity pllStable)
--     in   exposeClockResetEnable leds pllOut rstSync
--   where
--     key1R  = isRising 1 key1
--     leds   = mealy blinkerT (1, False, 0) key1R
-- @
resetSynchronizer
  :: forall dom
   . KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Reset dom
resetSynchronizer clk rst en =
  case resetKind @dom of
    SAsynchronous ->
      let isActiveHigh = case resetPolarity @dom of { SActiveHigh -> True; _ -> False }
          r1 = register clk rst en isActiveHigh (pure (not isActiveHigh))
          r2 = register clk rst en isActiveHigh r1
       in unsafeToReset r2
    SSynchronous ->
      -- Reset is already synchronous, nothing to do!
      rst

-- | Calculate the period, in __ps__, given a frequency in __Hz__
--
-- i.e. to calculate the clock period for a circuit to run at 240 MHz we get
--
-- >>> freqCalc 240e6
-- 4167
--
-- __NB__: This function is /not/ synthesizable
freqCalc :: Double -> Integer
freqCalc = toInteger . hzToPeriod
{-# DEPRECATED freqCalc "Use 'hzToPeriod' instead." #-}

-- ** Synchronization primitive
-- | The 'unsafeSynchronizer' function is a primitive that must be used to
-- connect one clock domain to the other, and will be synthesized to a (bundle
-- of) wire(s) in the eventual circuit. This function should only be used as
-- part of a proper synchronization component, such as the following dual
-- flip-flop synchronizer:
--
-- @
-- dualFlipFlop
--   :: Clock domA
--   -> Clock domB
--   -> Enable domA
--   -> Enable domB
--   -> Bit
--   -> Signal domA Bit
--   -> Signal domB Bit
-- dualFlipFlop clkA clkB enA enB dflt =
--   'delay' clkB enB dflt . 'delay' clkB enB dflt . 'unsafeSynchronizer' clkA clkB
-- @
--
-- The 'unsafeSynchronizer' works in such a way that, given 2 clocks:
--
-- @
-- createDomain vSystem{vName=\"Dom7\", vPeriod=7}
--
-- clk7 :: 'Clock' Dom7
-- clk7 = 'clockGen'
--
-- en7 :: 'Enable' Dom7
-- en7 = 'enableGen'
-- @
--
-- and
--
-- @
-- createDomain vSystem{vName=\"Dom2\", vPeriod=2}
--
-- clk2 :: 'Clock' Dom2
-- clk2 = 'clockGen'
--
-- en2 :: 'Enable' Dom2
-- en2 = 'enableGen'
-- @
--
-- Oversampling followed by compression is the identity function plus 2 initial
-- values:
--
-- @
-- 'delay' clkB enB dflt $
-- 'unsafeSynchronizer' clkA clkB $
-- 'delay' clkA enA dflt $
-- 'unsafeSynchronizer' clkB clkA $
-- 'delay' clkB enB s
--
-- ==
--
-- dflt :- dflt :- s
-- @
--
-- Something we can easily observe:
--
-- @
-- oversampling clkA clkB enA enB dflt =
--   'delay' clkB enB dflt
--     . 'unsafeSynchronizer' clkA clkB
--     . 'delay' clkA enA dflt
-- almostId clkA clkB enA enB dflt =
--   'delay' clkB enB dflt
--     . 'unsafeSynchronizer' clkA clkB
--     . 'delay' clkA enA dflt
--     . 'unsafeSynchronizer' clkB clkA
--     . 'delay' clkB enB dflt
-- @
--
-- >>> sampleN 37 (oversampling clk7 clk2 en7 en2 0 (fromList [(1::Int)..10]))
-- [0,0,1,1,1,2,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,6,7,7,7,8,8,8,8,9,9,9,10,10,10,10]
-- >>> sampleN 12 (almostId clk2 clk7 en2 en7 0 (fromList [(1::Int)..10]))
-- [0,0,1,2,3,4,5,6,7,8,9,10]
unsafeSynchronizer
  :: forall dom1 dom2 a
   . ( KnownDomain dom1
     , KnownDomain dom2 )
  => Clock dom1
  -- ^ 'Clock' of the incoming signal
  -> Clock dom2
  -- ^ 'Clock' of the outgoing signal
  -> Signal dom1 a
  -> Signal dom2 a
unsafeSynchronizer _clk1 _clk2 =
  veryUnsafeSynchronizer
    (snatToNum (clockPeriod @dom1))
    (snatToNum (clockPeriod @dom2))
{-# INLINE unsafeSynchronizer #-}

-- | Same as 'unsafeSynchronizer', but with manually supplied clock periods.
--
-- Note: this unsafeSynchronizer is defined to be consistent with the vhdl and verilog
-- implementations however as only synchronous signals are represented in Clash this
-- cannot be done precisely and can lead to odd behaviour. For example,
-- @
-- sample $ unsafeSynchronizer @Dom2 @Dom7 . unsafeSynchronizer @Dom7 @Dom2 $ fromList [0..10]
-- > [0,4,4,4,7,7,7,7,11,11,11..
-- @
-- is quite different from the identity,
-- @
-- sample $ fromList [0..10]
-- > [0,1,2,3,4,5,6,7,8,9,10..
-- @
-- with values appearing from the "future".
veryUnsafeSynchronizer
  :: Int
  -- ^ Period of clock belonging to 'dom1'
  -> Int
  -- ^ Period of clock belonging to 'dom2'
  -> Signal dom1 a
  -> Signal dom2 a
veryUnsafeSynchronizer t1 t2
  -- this case is just an optimisation for when the periods are the same
  | t1 == t2 = same

  | otherwise = go 0

  where
  same :: Signal dom1 a -> Signal dom2 a
  same (s :- ss) = s :- same ss

  go :: Int -> Signal dom1 a -> Signal dom2 a
  go relativeTime (a :- s)
    | relativeTime <= 0 = a :- go (relativeTime + t2) (a :- s)
    | otherwise = go (relativeTime - t1) s
{-# NOINLINE veryUnsafeSynchronizer #-}
{-# ANN veryUnsafeSynchronizer hasBlackBox #-}

-- * Basic circuit functions

-- | Merge enable signal with signal of bools by applying the boolean AND
-- operation.
enable
  :: Enable dom
  -> Signal dom Bool
  -> Enable dom
enable e0 e1 =
  toEnable (fromEnable e0 .&&. e1)

-- | Special version of 'delay' that doesn't take enable signals of any kind.
-- Initial value will be undefined.
dflipflop
  :: ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -> Signal dom a
  -> Signal dom a
dflipflop clk i =
  delay
    clk
    (toEnable (pure True))
    (deepErrorX "First value of dflipflop undefined")
    i
{-# INLINE dflipflop #-}

-- | \"@'delay' clk s@\" delays the values in 'Signal' /s/ for once cycle, the
-- value at time 0 is /dflt/.
--
-- >>> sampleN 3 (delay systemClockGen enableGen 0 (fromList [1,2,3,4]))
-- [0,1,2]
delay
  :: ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -- ^ Clock
  -> Enable dom
  -- ^ Global enable
  -> a
  -- ^ Initial value
  -> Signal dom a
  -> Signal dom a
delay = delay#
{-# INLINE delay #-}

-- | Version of 'delay' that only updates when its third argument is a 'Just'
-- value.
--
-- >>> let input = fromList [Just 1, Just 2, Nothing, Nothing, Just 5, Just 6, Just (7::Int)]
-- >>> sampleN 7 (delayMaybe systemClockGen enableGen 0 input)
-- [0,1,2,2,2,5,6]
delayMaybe
  :: ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -- ^ Clock
  -> Enable dom
  -- ^ Global enable
  -> a
  -- ^ Initial value
  -> Signal dom (Maybe a)
  -> Signal dom a
delayMaybe clk gen dflt i =
  delayEn clk gen dflt (isJust <$> i) (fromJustX <$> i)
{-# INLINE delayMaybe #-}

-- | Version of 'delay' that only updates when its third argument is asserted.
--
-- >>> let input = fromList [1,2,3,4,5,6,7::Int]
-- >>> let enable = fromList [True,True,False,False,True,True,True]
-- >>> sampleN 7 (delayEn systemClockGen enableGen 0 enable input)
-- [0,1,2,2,2,5,6]
delayEn
  :: ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -- ^ Clock
  -> Enable dom
  -- ^ Global enable
  -> a
  -- ^ Initial value
  -> Signal dom Bool
  -- ^ Enable
  -> Signal dom a
  -> Signal dom a
delayEn clk gen dflt en i =
  delay clk (enable gen en) dflt i
{-# INLINE delayEn #-}

-- | \"@'register' clk rst i s@\" delays the values in 'Signal' /s/ for one
-- cycle, and sets the value to @i@ the moment the reset becomes 'False'.
--
-- >>> sampleN 5 (register systemClockGen resetGen enableGen 8 (fromList [1,1,2,3,4]))
-- [8,8,1,2,3]
register
  :: ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -- ^ clock
  -> Reset dom
  -- ^ Reset, 'register' outputs the reset value when the reset is active
  -> Enable dom
  -- ^ Global enable
  -> a
  -- ^ Reset value. If the domain has initial values enabled, the reset value
  -- will also be the initial value.
  -> Signal dom a
  -> Signal dom a
register clk rst gen initial i =
  register# clk rst gen initial initial i
{-# INLINE register #-}

-- | Version of 'register' that only updates its content when its fourth
-- argument is a 'Just' value. So given:
--
-- @
-- sometimes1 clk rst en = s where
--   s = 'register' clk rst en Nothing (switch '<$>' s)
--
--   switch Nothing = Just 1
--   switch _       = Nothing
--
-- countSometimes clk rst en = s where
--   s     = 'regMaybe' clk rst en 0 (plusM ('pure' '<$>' s) (sometimes1 clk rst en))
--   plusM = liftA2 (liftA2 (+))
-- @
--
-- We get:
--
-- >>> sampleN 9 (sometimes1 systemClockGen resetGen enableGen)
-- [Nothing,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1,Nothing,Just 1]
-- >>> sampleN 9 (count systemClockGen resetGen enableGen)
-- [0,0,0,1,1,2,2,3,3]
regMaybe
  :: ( KnownDomain dom
     , NFDataX a )
  => Clock dom
  -- ^ Clock
  -> Reset dom
  -- ^ Reset, 'regMaybe' outputs the reset value when the reset value is active
  -> Enable dom
  -- ^ Global enable
  -> a
  -- ^ Reset value. If the domain has initial values enabled, the reset value
  -- will also be the initial value.
  -> Signal dom (Maybe a)
  -> Signal dom a
regMaybe clk rst en initial iM =
  register clk rst (enable en (fmap isJust iM)) initial (fmap fromJustX iM)
{-# INLINE regMaybe #-}

-- | Version of 'register' that only updates its content when its fourth
-- argument is asserted. So given:
--
-- @
-- oscillate clk rst en = let s = 'register' clk rst en False (not \<$\> s) in s
-- count clk rst en     = let s = 'regEn clk rst en 0 (oscillate clk rst en) (s + 1) in s
-- @
--
-- We get:
--
-- >>> sampleN 9 (oscillate systemClockGen resetGen enableGen)
-- [False,False,True,False,True,False,True,False,True]
-- >>> sampleN 9 (count systemClockGen resetGen enableGen)
-- [0,0,0,1,1,2,2,3,3]
regEn
  :: ( KnownDomain dom
     , NFDataX a
     )
  => Clock dom
  -- ^ Clock
  -> Reset dom
  -- ^ Reset, 'regEn' outputs the reset value when the reset value is active
  -> Enable dom
  -- ^ Global enable
  -> a
  -- ^ Reset value. If the domain has initial values enabled, the reset value
  -- will also be the initial value.
  -> Signal dom Bool
  -- ^ Enable signal
  -> Signal dom a
  -> Signal dom a
regEn clk rst gen initial en i =
  register clk rst (enable gen en) initial i
{-# INLINE regEn #-}

-- * Simulation functions

-- | Same as 'simulate', but with the reset line asserted for /n/ cycles. Similar
-- to 'simulate', 'simulateWithReset' will drop the output values produced while
-- the reset is asserted. While the reset is asserted, the first value from
-- @[a]@ is fed to the circuit.
simulateWithReset
  :: forall dom a b m
   . ( KnownDomain dom
     , NFDataX a
     , NFDataX b
     , 1 <= m )
  => SNat m
  -- ^ Number of cycles to assert the reset
  -> a
  -- ^ Reset value
  -> ( KnownDomain dom
    => Clock dom
    -> Reset dom
    -> Enable dom
    -> Signal dom a
    -> Signal dom b )
  -- ^ Circuit to simulate
  -> [a]
  -> [b]
simulateWithReset m resetVal f as =
  drop (snatToNum m) out
 where
  inp = replicate (snatToNum m) resetVal ++ as
  rst = resetGenN @dom m
  clk = clockGen
  en  = enableGen
  out = simulate (f clk rst en) inp
{-# NOINLINE simulateWithReset #-}

-- | Same as 'simulateWithReset', but only sample the first /Int/ output values.
simulateWithResetN
  :: ( KnownDomain dom
     , NFDataX a
     , NFDataX b
     , 1 <= m )
  => SNat m
  -- ^ Number of cycles to assert the reset
  -> a
  -- ^ Reset value
  -> Int
  -- ^ Number of cycles to simulate (excluding cycle spent in reset)
  -> ( KnownDomain dom
    => Clock dom
    -> Reset dom
    -> Enable dom
    -> Signal dom a
    -> Signal dom b )
  -- ^ Circuit to simulate
  -> [a]
  -> [b]
simulateWithResetN nReset resetVal nSamples f as =
  take nSamples (simulateWithReset nReset resetVal f as)
{-# INLINE simulateWithResetN #-}

-- | Simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a list of
-- samples of type /a/
--
-- >>> simulateB (unbundle . register systemClockGen resetGen enableGen (8,8) . bundle) [(1,1), (1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB
  :: (Bundle a, Bundle b, NFDataX a, NFDataX b)
  => (Unbundled dom1 a -> Unbundled dom2 b)
  -- ^ The function we want to simulate
  -> [a]
  -- ^ Input samples
  -> [b]
simulateB f = simulate (bundle . f . unbundle)

-- | /Lazily/ simulate a (@'Unbundled' a -> 'Unbundled' b@) function given a
-- list of samples of type /a/
--
-- >>> simulateB (unbundle . register systemClockGen resetGen enableGen (8,8) . bundle) [(1,1), (1,1), (2,2), (3,3)] :: [(Int,Int)]
-- [(8,8),(8,8),(1,1),(2,2),(3,3)...
-- ...
--
-- __NB__: This function is not synthesizable
simulateB_lazy
  :: (Bundle a, Bundle b)
  => (Unbundled dom1 a -> Unbundled dom2 b)
  -- ^ The function we want to simulate
  -> [a]
  -- ^ Input samples
  -> [b]
simulateB_lazy f = simulate_lazy (bundle . f . unbundle)


-- | Hold reset for a number of cycles relative to an incoming reset signal.
--
-- Example:
--
-- >>> let sampleWithReset = sampleN 8 . unsafeToHighPolarity
-- >>> sampleWithReset (holdReset @System clockGen enableGen (SNat @2) (resetGenN (SNat @3)))
-- [True,True,True,True,True,False,False,False]
--
-- 'holdReset' holds the reset for an additional 2 clock cycles for a total
-- of 5 clock cycles where the reset is asserted. 'holdReset' also works on
-- intermediate assertions of the reset signal:
--
-- >>> let rst = fromList [True, False, False, False, True, False, False, False]
-- >>> sampleWithReset (holdReset @System clockGen enableGen (SNat @2) (unsafeFromHighPolarity rst))
-- [True,True,True,False,True,True,True,False]
--
holdReset
  :: forall dom n
   . KnownDomain dom
  => Clock dom
  -> Enable dom
  -- ^ Global enable
  -> SNat n
  -- ^ Hold for /n/ cycles, counting from the moment the incoming reset
  -- signal becomes deasserted.
  -> Reset dom
  -- ^ Reset to extend
  -> Reset dom
holdReset clk en SNat rst =
  unsafeFromHighPolarity ((/=maxBound) <$> counter)
 where
  counter :: Signal dom (Index (n+1))
  counter = register clk rst en 0 (satSucc SatBound <$> counter)

-- | Like 'fromList', but resets on reset and has a defined reset value.
--
-- >>> let rst = unsafeFromHighPolarity (fromList [True, True, False, False, True, False])
-- >>> let res = fromListWithReset @System rst Nothing [Just 'a', Just 'b', Just 'c']
-- >>> sampleN 6 res
-- [Nothing,Nothing,Just 'a',Just 'b',Nothing,Just 'a']
--
-- __NB__: This function is not synthesizable
fromListWithReset
  :: forall dom a
   . (KnownDomain dom, NFDataX a)
  => Reset dom
  -> a
  -> [a]
  -> Signal dom a
fromListWithReset rst resetValue vals =
  go (unsafeToHighPolarity rst) vals
 where
  go (r :- rs) _ | r = resetValue :- go rs vals
  go (_ :- rs) [] = deepErrorX "fromListWithReset: input ran out" :- go rs []
  go (_ :- rs) (a : as) = a :- go rs as

-- | Convert between different types of reset, adding a synchronizer in case
-- it needs to convert from an asynchronous to a synchronous reset.
convertReset
  :: forall domA domB
   . ( KnownDomain domA
     , KnownDomain domB
     )
  => Clock domA
  -> Clock domB
  -> Reset domA
  -> Reset domB
convertReset clkA clkB (unsafeToHighPolarity -> rstA0) =
  unsafeFromHighPolarity rstA2
 where
  rstA1 = unsafeSynchronizer clkA clkB rstA0
  rstA2 =
    case (resetKind @domA, resetKind @domB) of
      (SSynchronous,  SSynchronous)  -> rstA1
      (SAsynchronous, SAsynchronous) -> rstA1
      (SSynchronous,  SAsynchronous) -> rstA1
      (SAsynchronous, SSynchronous) ->
        delay clkB enableGen True $
          delay clkB enableGen True rstA1

-- | Get a list of samples from a 'Signal', while asserting the reset line
-- for /n/ clock cycles. 'sampleWithReset' does not return the first /n/ cycles,
-- i.e., when the reset is asserted.
--
-- __NB__: This function is not synthesizable
sampleWithReset
  :: forall dom a m
   . ( KnownDomain dom
     , NFDataX a
     , 1 <= m )
  => SNat m
  -- ^ Number of cycles to assert the reset
  -> (KnownDomain dom
      => Clock dom
      -> Reset dom
      -> Enable dom
      -> Signal dom a)
  -- ^ 'Signal' to sample
  -> [a]
sampleWithReset nReset f0 =
  let f1 = f0 clockGen (resetGenN @dom nReset) enableGen in
  drop (snatToNum nReset) (sample f1)
{-# NOINLINE sampleWithReset #-}

-- | Get a fine list of /m/ samples from a 'Signal', while asserting the reset line
-- for /n/ clock cycles. 'sampleWithReset' does not return the first /n/ cycles,
-- i.e., while the reset is asserted.
--
-- __NB__: This function is not synthesizable
sampleWithResetN
  :: forall dom a m
   . ( KnownDomain dom
     , NFDataX a
     , 1 <= m )
  => SNat m
  -- ^ Number of cycles to assert the reset
  -> Int
  -- ^ Number of samples to produce
  -> (KnownDomain dom
      => Clock dom
      -> Reset dom
      -> Enable dom
      -> Signal dom a)
  -- ^ 'Signal' to sample
  -> [a]
sampleWithResetN nReset nSamples f =
  take nSamples (sampleWithReset nReset f)

{-# RULES "sequenceAVecSignal" Clash.Sized.Vector.traverse# (\x -> x) = vecBundle# #-}
