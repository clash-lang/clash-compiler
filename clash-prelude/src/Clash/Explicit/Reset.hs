{-|
Copyright  :  (C) 2020-2021, QBayLogic B.V.
                  2022     , Google LLC
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Utilities to deal with resets.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Explicit.Reset
  ( -- Defined in this module
    resetSynchronizer
  , resetGlitchFilter
  , holdReset
  , convertReset

    -- Reexports
  , Reset
  , resetGen
  , resetGenN
  , resetKind
  , systemResetGen
  , unsafeToReset
  , unsafeFromReset
  , unsafeToHighPolarity
  , unsafeToLowPolarity
  , unsafeFromHighPolarity
  , unsafeFromLowPolarity
  ) where

import           Data.Type.Equality ((:~:)(Refl))

import           Clash.Class.Num (satSucc, SaturationMode(SatBound))
import           Clash.Explicit.Moore
import           Clash.Explicit.Signal
import           Clash.Promoted.Nat
import           Clash.Signal.Internal
import           Clash.Sized.Index (Index)

import           GHC.Stack (HasCallStack)
import           GHC.TypeLits (type (+))

{- $setup
>>> import Clash.Explicit.Prelude
-}

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
-- a reset singal that is not synchronized to any other signals. It stabalizes
-- the clock and then synchronizes the reset signal.
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
--     in   exposeClockResetEnable leds pllOut rstSync enableGen
--   where
--     key1R  = isRising 1 key1
--     leds   = mealy blinkerT (1, False, 0) key1R
-- @
--
-- === __Implementation details__
-- 'resetSynchronizer' implements the following circuit for asynchronous domains:
--
-- @
--                                   rst
--   --------------------------------------+
--                       |                 |
--                  +----v----+       +----v----+
--     deasserted   |         |       |         |
--   --------------->         +------->         +-------->
--                  |         |       |         |
--              +---|>        |   +---|>        |
--              |   |         |   |   |         |
--              |   +---------+   |   +---------+
--      clk     |                 |
--   -----------------------------+
-- @
--
-- This corresponds to figure 3d at <https://www.embedded.com/asynchronous-reset-synchronization-and-distribution-challenges-and-solutions/>
--
-- For synchronous domains two sequential dflipflops are used:
--
-- @
--                  +---------+       +---------+
--     rst          |         |       |         |
--   --------------->         +------->         +-------->
--                  |         |       |         |
--              +---|>        |   +---|>        |
--              |   |         |   |   |         |
--              |   +---------+   |   +---------+
--      clk     |                 |
--   -----------------------------+
-- @
--
resetSynchronizer
  :: forall dom
   . KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Reset dom
resetSynchronizer clk rst = rstOut
 where
  isActiveHigh = case resetPolarity @dom of { SActiveHigh -> True; _ -> False }
  rstOut =
    case (resetKind @dom) of
      SAsynchronous -> unsafeToReset
                         $ register clk rst enableGen isActiveHigh
                         $ register clk rst enableGen isActiveHigh
                         $ pure (not isActiveHigh)
      SSynchronous  -> unsafeToReset
                         $ delay clk enableGen isActiveHigh
                         $ delay clk enableGen isActiveHigh
                         $ unsafeFromReset rst
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE resetSynchronizer #-} -- Give reset synchronizer its own HDL file

-- | Filter glitches from reset signals by only triggering a reset after it has
-- been asserted for /glitchlessPeriod/ cycles. Similarly, it will stay
-- asserted until a /glitchlessPeriod/ number of deasserted cycles have been
-- observed.
--
-- This circuit can only be used on platforms supporting initial values. Platforms
-- not supporting this should consider using a power-on-reset and a circuit
-- designed with this environment in mind.
--
-- === __Example 1__
-- >>> let sampleResetN n = sampleN n . unsafeToHighPolarity
-- >>> let resetFromList = unsafeFromHighPolarity . fromList
-- >>> let rst = resetFromList [True, True, False, False, True, False, False, True, True, False, True, True]
-- >>> sampleResetN 12 (resetGlitchFilter d2 systemClockGen rst)
-- [True,True,True,True,False,False,False,False,False,True,True,True]
--
resetGlitchFilter
  :: forall dom glitchlessPeriod n
   . ( HasCallStack
     , KnownDomain dom
     , glitchlessPeriod ~ (n + 1) )
  => SNat glitchlessPeriod
  -- ^ Consider a reset signal to be properly asserted after having seen the
  -- reset asserted for /glitchlessPeriod/ cycles straight.
  -> Clock dom
  -> Reset dom
  -> Reset dom
resetGlitchFilter SNat clk rst =
  case initBehavior @dom of
    SDefined ->
      unsafeToReset $
        moore clk noReset enableGen go fst (asserted, 0) (unsafeFromReset rst)
    SUnknown ->
      error "'resetGlitchFilter' only supports domains with defined initial values."
 where
  go :: state ~ (Bool, Index glitchlessPeriod) => state -> Bool -> state
  go (state, count) reset
    | reset == state    = (state,     0)
    | count == maxBound = (not state, 0)
    | otherwise         = (state,     count + 1)

  noReset :: Reset dom
  noReset = unsafeToReset (pure (not asserted))

  asserted :: Bool
  asserted =
    case resetPolarity @dom of
      SActiveHigh -> True
      SActiveLow -> False
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE resetGlitchFilter #-} -- Give reset glitch filter its own HDL file

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

-- | Convert between different types of reset, adding a synchronizer when
-- the domains are not the same. See 'resetSynchronizer' for further details
-- about reset synchronization.
convertReset
  :: forall domA domB
   . ( KnownDomain domA
     , KnownDomain domB
     )
  => Clock domA
  -> Clock domB
  -> Reset domA
  -> Reset domB
convertReset clkA clkB rstA0 = rstA2
 where
  rstA1 = unsafeToReset (unsafeSynchronizer clkA clkB (unsafeFromReset rstA0))
  rstA2 =
    case (sameDomain @domA @domB) of
      Just Refl -> rstA0
      Nothing   -> resetSynchronizer clkB rstA1
