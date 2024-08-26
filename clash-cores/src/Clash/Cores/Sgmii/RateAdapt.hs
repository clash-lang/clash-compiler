{-# LANGUAGE CPP #-}

{- |
  Copyright   :  (C) 2024, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Functions for the rate adaptation blocks that are required for lower bit
  rates than 1000 Mbps
-}
module Clash.Cores.Sgmii.RateAdapt
  ( rateAdaptRx
  , rateAdaptTx
  )
where

import Clash.Cores.Sgmii.Common
import Clash.Prelude

-- | State transition function for the receive rate adaption function
rateAdaptRxT ::
  -- | Current state
  Index 100 ->
  -- | Input value
  (LinkSpeed, Maybe a) ->
  -- | New state and output value
  (Index 100, Maybe a)
rateAdaptRxT n (_, Nothing) = (n, Nothing)
rateAdaptRxT n (linkSpeed, Just a)
  | n == 0 = (n', Just a)
  | otherwise = (n', Nothing)
 where
  n' = if n == repeatN then 0 else n + 1
  repeatN = case linkSpeed of
    Speed1000 -> 0
    Speed100 -> 9
    Speed10 -> 99

-- | Rate adaption function that takes an input and only outputs this input once
--   per N cycles based on the current link speed
rateAdaptRx ::
  (HiddenClockResetEnable dom) =>
  -- | Link speed reported by the PHY
  Signal dom LinkSpeed ->
  -- | Input value
  Signal dom (Maybe a) ->
  -- | Output value
  Signal dom (Maybe a)
rateAdaptRx linkSpeed a = mealyB rateAdaptRxT 0 (linkSpeed, a)

{-# CLASH_OPAQUE rateAdaptRx #-}

-- | State transition function for the transmit rate adaption function
rateAdaptTxT ::
  -- | Current state
  Index 100 ->
  -- | Input value
  (LinkSpeed, Maybe a) ->
  -- | New state and output value
  (Index 100, (Bool, Maybe a))
rateAdaptTxT n (linkSpeed, a) = (n', (ready, a))
 where
  n' = if ready then 0 else n + 1
  ready = n == repeatN
  repeatN = case linkSpeed of
    Speed1000 -> 0
    Speed100 -> 9
    Speed10 -> 99

-- | Rate adaption function that passes an input to the output, and accepts new
--   inputs based on the link speed reported by the PHY.

--   Remarks:
--   - The input is __only__ allowed to be changed the sample after the first
--     element in the output tuple, @READY@, has been asserted.
rateAdaptTx ::
  (HiddenClockResetEnable dom) =>
  -- | Link speed reported by the PHY
  Signal dom LinkSpeed ->
  -- | Input value from the outside world
  Signal dom (Maybe a) ->
  -- | Output tuple containing the request for a new value ('Bool') and the
  --   (possibly replicated) output value
  (Signal dom Bool, Signal dom (Maybe a))
rateAdaptTx linkSpeed bv = mealyB rateAdaptTxT 0 (linkSpeed, bv)

{-# CLASH_OPAQUE rateAdaptTx #-}
