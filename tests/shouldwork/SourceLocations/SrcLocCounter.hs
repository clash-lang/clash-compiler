{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module SrcLocCounter where

import Clash.Prelude

-- | Simple counter: increments every cycle
counter
  :: HiddenClockResetEnable dom
  => Signal dom (Bool, (Unsigned 8))
counter = bundle (c .==. 0, c)
  where
    c = regEn 0 (pure True) (c + 1)

