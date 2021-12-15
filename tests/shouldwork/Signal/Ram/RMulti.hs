-- An asyncRam with different read and write clocks
--
-- Test reading with a fast clock. Memory is initialized through the write port,
-- but after that the write side is dormant.
module RMulti where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import qualified Prelude as P

createDomain vSystem{vName="P10", vPeriod=10000}
createDomain vSystem{vName="P20", vPeriod=20000}

ram
  :: Clock P20
  -> Clock P10
  -> Signal P10 (Unsigned 1)
  -> Signal P20 (Maybe (Unsigned 1, Unsigned 2))
  -> Signal P10 (Unsigned 2)
ram wClk rClk = asyncRamPow2 wClk rClk enableGen

tbOutput
  :: Clock P20
  -> Clock P10
  -> Signal P10 (Unsigned 2)
tbOutput wClk rClk = output
 where
  wrM = stimuliGenerator wClk wNoReset $
          Just (0, 1) :> Just (1,2) :> Nothing :> Nil
  rd = delay rClk enableGen 0 $ rd + 1
  output = ignoreFor rClk rNoReset enableGen d2 0 $ ram wClk rClk rd wrM
  wNoReset = unsafeFromHighPolarity @P20 (pure False)
  rNoReset = unsafeFromHighPolarity @P10 (pure False)

tb
  :: ( KnownNat n
     , 1 <= n)
  => Vec n (Unsigned 2)
  -> Signal P10 Bool
tb expectedSamples = done
 where
  output = tbOutput wClk rClk
  expectedOutput = outputVerifier' rClk noReset expectedSamples
  done = expectedOutput output
  (rClk, wClk) = biTbClockGen (not <$> done) :: (Clock P10, Clock P20)
  noReset = unsafeFromHighPolarity @P10 (pure False)
{-# INLINE tb #-}
