{-|
Copyright  :  (C) 2020-2023, QBayLogic B.V.,
                  2022-2023, Google LLC
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities to deal with resets.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Explicit.Reset
  ( -- Defined in this module
    resetSynchronizer
  , resetGlitchFilter
  , resetGlitchFilterWithReset
  , unsafeResetGlitchFilter
  , holdReset
  , convertReset
  , noReset
  , andReset, unsafeAndReset
  , orReset, unsafeOrReset

    -- Re-exports
  , Reset
  , resetGen
  , resetGenN
  , resetKind
  , systemResetGen
  , unsafeToReset
  , unsafeFromReset
  , unsafeToActiveHigh
  , unsafeToActiveLow
  , unsafeFromActiveHigh
  , unsafeFromActiveLow

  -- * Deprecated
  , unsafeFromHighPolarity
  , unsafeFromLowPolarity
  , unsafeToHighPolarity
  , unsafeToLowPolarity
  ) where

import           Data.Type.Equality ((:~:)(Refl))
import           Data.Typeable (Typeable)

import           Clash.Class.Num (satSucc, SaturationMode(SatBound))
import           Clash.Explicit.Signal
import           Clash.Explicit.Synchronizer (dualFlipFlopSynchronizer)
import           Clash.Promoted.Nat
import           Clash.Signal.Internal hiding
  (unsafeToReset, unsafeFromHighPolarity, unsafeFromActiveHigh, unsafeFromLowPolarity, unsafeFromActiveLow)
import qualified Clash.Signal.Internal as Internal
import           Clash.Sized.Index (Index)

import           GHC.Stack (HasCallStack)
import           GHC.TypeLits (type (+), type (<=))

{- $setup
>>> import Clash.Explicit.Prelude
-}

-- | A reset that is never asserted
noReset :: forall dom . KnownDomain dom => Reset dom
noReset = unsafeFromActiveHigh (knownDomain @dom) (pure False)

-- | Output reset will be asserted when either one of the input resets is
-- asserted
orReset ::
  forall dom .
  HasSynchronousReset dom =>
  Reset dom ->
  Reset dom ->
  Reset dom
orReset = unsafeOrReset

-- | Output reset will be asserted when either one of the input resets is
-- asserted. This function is considered unsafe because it can be used on
-- domains with components with asynchronous resets, where use of this function
-- can introduce glitches triggering a reset.
unsafeOrReset :: forall dom. Reset dom -> Reset dom -> Reset dom
unsafeOrReset r@(unsafeFromReset -> rst0) (unsafeFromReset -> rst1) =
  Internal.unsafeToReset (resetConf r) $
    case getResetPolarity (resetConf r) of
      SActiveHigh -> rst0 .||. rst1
      SActiveLow  -> rst0 .&&. rst1

-- | Output reset will be asserted when both input resets are asserted
andReset ::
  forall dom .
  HasSynchronousReset dom =>
  Reset dom ->
  Reset dom ->
  Reset dom
andReset = unsafeAndReset

-- | Output reset will be asserted when both input resets are asserted. This
-- function is considered unsafe because it can be used on domains with
-- components with asynchronous resets, where use of this function can introduce
-- glitches triggering a reset.
unsafeAndReset :: forall dom. Reset dom -> Reset dom -> Reset dom
unsafeAndReset r@(unsafeFromReset -> rst0) (unsafeFromReset -> rst1) =
  Internal.unsafeToReset (resetConf r) $
    case getResetPolarity (resetConf r) of
      SActiveHigh -> rst0 .&&. rst1
      SActiveLow  -> rst0 .||. rst1

-- | The resetSynchronizer will synchronize an incoming reset according to
-- whether the domain is synchronous or asynchronous.
--
-- For asynchronous resets this synchronizer ensures the reset will only
-- be de-asserted synchronously but it can still be asserted asynchronously.
-- The reset assert is immediate, but reset de-assertion is delayed by two
-- cycles.
--
-- Normally, asynchronous resets can be both asynchronously asserted and
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
-- "Clash.Explicit.Synchronizer".
--
-- For synchronous resets this function ensures that the reset is asserted and
-- de-asserted synchronously. Both the assertion and de-assertion of the reset
-- are delayed by two cycles.
--
-- === __Example 1__
-- The circuit below detects a rising bit (i.e., a transition from 0 to 1) in a
-- given argument. It takes a reset that is not synchronized to any of the other
-- incoming signals and synchronizes it using 'resetSynchronizer'.
--
-- @
-- topEntity
--   :: Clock  System
--   -> Reset  System
--   -> Signal System Bit
--   -> Signal System (BitVector 8)
-- topEntity clk asyncRst key1 =
--   withClockResetEnable clk rst enableGen leds
--  where
--   rst   = 'resetSynchronizer' clk asyncRst
--   key1R = isRising 1 key1
--   leds  = mealy blinkerT (1, False, 0) key1R
-- @
--
-- === __Example 2__
-- Similar to /Example 1/ this circuit detects a rising bit (i.e., a transition
-- from 0 to 1) in a given argument. It takes a clock that is not stable yet and
-- a reset signal that is not synchronized to any other signals. It stabilizes
-- the clock and then synchronizes the reset signal.
--
--
-- Note that the function 'Clash.Intel.ClockGen.altpllSync' provides this
-- functionality in a convenient form, obviating the need for
-- @resetSynchronizer@ for this use case.
--
-- @
-- topEntity
--   :: Clock  System
--   -> Reset  System
--   -> Signal System Bit
--   -> Signal System (BitVector 8)
-- topEntity clk rst key1 =
--     let  (pllOut,pllStable) = unsafeAltpll clk rst
--          rstSync            = 'resetSynchronizer' pllOut (unsafeFromActiveLow pllStable)
--     in   exposeClockResetEnable leds pllOut rstSync enableGen
--   where
--     key1R  = isRising 1 key1
--     leds   = mealy blinkerT (1, False, 0) key1R
-- @
--
-- === __Implementation details__
-- 'resetSynchronizer' implements the following circuit for asynchronous domains:
--
-- >                                   rst
-- >   --------------------------------------+
-- >                       |                 |
-- >                  +----v----+       +----v----+
-- >     deasserted   |         |       |         |
-- >   --------------->         +------->         +-------->
-- >                  |         |       |         |
-- >              +---|>        |   +---|>        |
-- >              |   |         |   |   |         |
-- >              |   +---------+   |   +---------+
-- >      clk     |                 |
-- >   -----------------------------+
--
-- This corresponds to figure 3d at <https://www.embedded.com/asynchronous-reset-synchronization-and-distribution-challenges-and-solutions/>
--
-- For synchronous domains two sequential dflipflops are used:
--
-- >                  +---------+       +---------+
-- >     rst          |         |       |         |
-- >   --------------->         +------->         +-------->
-- >                  |         |       |         |
-- >              +---|>        |   +---|>        |
-- >              |   |         |   |   |         |
-- >              |   +---------+   |   +---------+
-- >      clk     |                 |
-- >   -----------------------------+
--
resetSynchronizer
  :: forall dom
   . Clock dom
  -> Reset dom
  -> Reset dom
resetSynchronizer clk rst = rstOut
 where
  isActiveHigh = case getResetPolarity (resetConf rst) of { SActiveHigh -> True; _ -> False }
  rstOut =
    case getResetKind (resetConf rst) of
      SAsynchronous -> Internal.unsafeToReset (resetConf rst)
                         $ register clk rst enableGen isActiveHigh
                         $ register clk rst enableGen isActiveHigh
                         $ pure (not isActiveHigh)
      SSynchronous  -> Internal.unsafeToReset (resetConf rst)
                         $ delay clk enableGen isActiveHigh
                         $ delay clk enableGen isActiveHigh
                         $ unsafeFromReset rst

-- | Filter glitches from reset signals by only triggering a reset after it has
-- been asserted for /glitchlessPeriod/ cycles. Similarly, it will stay
-- asserted until a /glitchlessPeriod/ number of deasserted cycles have been
-- observed.
--
-- This circuit can only be used on platforms supporting initial values. This
-- restriction can be worked around by using 'unsafeResetGlitchFilter' but this
-- is not recommended.
--
-- On platforms without initial values, you should instead use
-- 'resetGlitchFilterWithReset' with an additional power-on reset, or
-- 'holdReset' if filtering is only needed on deassertion.
--
-- At power-on, the reset will be asserted. If the filtered reset input remains
-- unasserted, the output reset will deassert after /glitchlessPeriod/ clock
-- cycles.
--
-- If @resetGlitchFilter@ is used in a domain with asynchronous resets
-- ('Asynchronous'), @resetGlitchFilter@ will first synchronize the reset input
-- with 'dualFlipFlopSynchronizer'.
--
-- === __Example 1__
-- >>> let sampleResetN n = sampleN n . unsafeToActiveHigh
-- >>> let resetFromList = unsafeFromActiveHigh . fromList
-- >>> let rst = resetFromList [True, True, False, False, True, False, False, True, True, False, True, True]
-- >>> sampleResetN 12 (resetGlitchFilter d2 (clockGen @XilinxSystem) rst)
-- [True,True,True,True,False,False,False,False,False,True,True,True]
resetGlitchFilter
  :: forall dom glitchlessPeriod
   . ( HasCallStack
     , HasDefinedInitialValues dom
     , 1 <= glitchlessPeriod
     )
  => SNat glitchlessPeriod
  -- ^ Consider a reset signal to be properly asserted after having seen the
  -- reset asserted for /glitchlessPeriod/ cycles straight.
  -> Clock dom
  -> Reset dom
  -> Reset dom
resetGlitchFilter = unsafeResetGlitchFilter
{-# INLINE resetGlitchFilter #-}

-- | Filter glitches from reset signals by only triggering a reset after it has
-- been asserted for /glitchlessPeriod/ cycles. Similarly, it will stay
-- asserted until a /glitchlessPeriod/ number of deasserted cycles have been
-- observed.
--
-- On platforms without initial values ('Unknown'), 'resetGlitchFilter' cannot
-- be used and you should use 'resetGlitchFilterWithReset' with an additional
-- power-on reset, or 'holdReset' if filtering is only needed on deassertion.
--
-- @unsafeResetGlitchFilter@ allows breaking the requirement of initial values,
-- but by doing so it is possible that the design starts up with a period of up
-- to /2 * glitchlessPeriod/ clock cycles where the reset output is unasserted
-- (or longer in the case of glitches on the filtered reset input). This can
-- cause a number of problems. The outputs\/tri-states of the design might
-- output random things, including coherent but incorrect streams of data. This
-- might have grave repercussions in the design's environment (sending network
-- packets, overwriting non-volatile memory, in extreme cases destroying
-- controlled equipment or causing harm to living beings, ...).
--
-- Without initial values, the synthesized result of @unsafeResetGlitchFilter@
-- eventually correctly outputs a filtered version of the reset input. However,
-- in simulation, it will indefinitely output an undefined value. This happens
-- both in Clash simulation and in HDL simulation. Therefore, simulation should
-- not include the @unsafeResetGlitchFilter@.
--
-- If @unsafeResetGlitchFilter@ is used in a domain with asynchronous resets
-- ('Asynchronous'), @unsafeResetGlitchFilter@ will first synchronize the reset
-- input with 'dualFlipFlopSynchronizer'.
unsafeResetGlitchFilter
  :: forall dom glitchlessPeriod
   . ( HasCallStack
     , 1 <= glitchlessPeriod
     )
  => SNat glitchlessPeriod
  -- ^ Consider a reset signal to be properly asserted after having seen the
  -- reset asserted for /glitchlessPeriod/ cycles straight.
  -> Clock dom
  -> Reset dom
  -> Reset dom
unsafeResetGlitchFilter glitchlessPeriod clk =
  resetGlitchFilter# glitchlessPeriod reg dffSync
 where
  reg = delay clk enableGen
  dffSync = dualFlipFlopSynchronizer clk clk (Internal.unsafeFromActiveHigh (clockConf clk) (pure False)) enableGen
{-# INLINE unsafeResetGlitchFilter #-}

-- | Filter glitches from reset signals by only triggering a reset after it has
-- been asserted for /glitchlessPeriod/ cycles. Similarly, it will stay
-- asserted until a /glitchlessPeriod/ number of deasserted cycles have been
-- observed.
--
-- Compared to 'resetGlitchFilter', this function adds an additional power-on
-- reset input. As soon as the power-on reset asserts, the reset output will
-- assert, and after the power-on reset deasserts, the reset output will stay
-- asserted for another /glitchlessPeriod/ clock cycles. This is identical
-- behavior to 'holdReset' where it concerns the power-on reset, and differs
-- from the filtered reset, which will only cause an assertion after
-- /glitchlessPeriod/ cycles.
--
-- If @resetGlitchFilterWithReset@ is used in a domain with asynchronous resets
-- ('Asynchronous'), @resetGlitchFilterWithReset@ will first synchronize the
-- reset input with 'dualFlipFlopSynchronizer'.
resetGlitchFilterWithReset
  :: forall dom glitchlessPeriod
   . ( HasCallStack
     , 1 <= glitchlessPeriod
     )
  => SNat glitchlessPeriod
  -- ^ Consider a reset signal to be properly asserted after having seen the
  -- reset asserted for /glitchlessPeriod/ cycles straight.
  -> Clock dom
  -> Reset dom
  -- ^ The power-on reset for the glitch filter itself
  -> Reset dom
  -- ^ The reset that will be filtered
  -> Reset dom
resetGlitchFilterWithReset glitchlessPeriod clk ownRst =
  resetGlitchFilter# glitchlessPeriod reg dffSync
 where
  reg = register clk ownRst enableGen
  dffSync = dualFlipFlopSynchronizer clk clk ownRst enableGen
{-# INLINE resetGlitchFilterWithReset #-}

resetGlitchFilter#
  :: forall dom glitchlessPeriod state
   . ( HasCallStack
     , 1 <= glitchlessPeriod
     , state ~ (Bool, Index glitchlessPeriod)
     )
  => SNat glitchlessPeriod
  -> (   state
      -> Signal dom state
      -> Signal dom state
     )
  -> (   Bool
      -> Signal dom Bool
      -> Signal dom Bool
     )
  -> Reset dom
  -> Reset dom
resetGlitchFilter# SNat reg dffSync rstIn0 =
  let s' = go <$> s <*> rstIn2
      s  = reg (asserted, 0) s'
  in Internal.unsafeToReset (resetConf rstIn0) $ fst <$> s
 where
  rstIn1 = unsafeFromReset rstIn0
  rstIn2 =
    case getResetKind (resetConf rstIn0) of
      SAsynchronous -> dffSync asserted rstIn1
      SSynchronous -> rstIn1

  go :: state -> Bool -> state
  go (state, count) reset
    | reset == state    = (state,     0)
    | count == maxBound = (not state, 0)
    | otherwise         = (state,     count + 1)

  asserted :: Bool
  asserted =
    case getResetPolarity (resetConf rstIn0) of
      SActiveHigh -> True
      SActiveLow -> False

-- | Hold reset for a number of cycles relative to an incoming reset signal.
--
-- Example:
--
-- >>> let sampleWithReset = sampleN 8 . unsafeToActiveHigh
-- >>> sampleWithReset (holdReset @System clockGen enableGen (SNat @2) (resetGenN (SNat @3)))
-- [True,True,True,True,True,False,False,False]
--
-- 'holdReset' holds the reset for an additional 2 clock cycles for a total
-- of 5 clock cycles where the reset is asserted. 'holdReset' also works on
-- intermediate assertions of the reset signal:
--
-- >>> let rst = fromList [True, False, False, False, True, False, False, False]
-- >>> sampleWithReset (holdReset @System clockGen enableGen (SNat @2) (unsafeFromActiveHigh rst))
-- [True,True,True,False,True,True,True,False]
--
holdReset
  :: forall dom n
   . Clock dom
  -> Enable dom
  -- ^ Global enable
  -> SNat n
  -- ^ Hold for /n/ cycles, counting from the moment the incoming reset
  -- signal becomes deasserted.
  -> Reset dom
  -- ^ Reset to extend
  -> Reset dom
holdReset clk en SNat rst =
  Internal.unsafeFromActiveHigh (resetConf rst) ((/=maxBound) <$> counter)
 where
  counter :: Signal dom (Index (n+1))
  counter = register clk rst en 0 (satSucc SatBound <$> counter)

-- | Convert between different types of reset, adding a synchronizer when
-- the domains are not the same. See 'resetSynchronizer' for further details
-- about reset synchronization.
--
-- If @domA@ has 'Synchronous' resets, a flip-flop is inserted in @domA@ to
-- filter glitches. This adds one @domA@ clock cycle delay.
convertReset
  :: forall domA domB
   . Clock domA
  -> Clock domB
  -> Reset domA
  -> Reset domB
convertReset clkA clkB rstA0 = rstB1
 where
  rstA1 = unsafeFromReset rstA0
  rstA2 =
    case (getResetPolarity (clockConf clkA), getResetPolarity (clockConf clkB)) of
      (SActiveLow, SActiveLow)   -> rstA1
      (SActiveHigh, SActiveHigh) -> rstA1
      _                          -> not <$> rstA1
  rstA3 =
    case getResetKind (clockConf clkA) of
      SSynchronous -> delay clkA enableGen assertedA rstA2
      _            -> rstA2
  rstB0 = Internal.unsafeToReset (clockConf clkB) $ unsafeSynchronizer clkA clkB rstA3
  rstB1 =
    case sameDomain (clockConf clkA) (clockConf clkB) of
      Just Refl -> rstA0
      Nothing   -> resetSynchronizer clkB rstB0
  assertedA :: Bool
  assertedA =
    case getResetPolarity (clockConf clkA) of
      SActiveHigh -> True
      SActiveLow  -> False
