-- An asyncRam with different read and write clocks
--
-- Test reading with a fast clock. Memory is initialized through the write port,
-- but after that the write side is dormant.

{-# OPTIONS_GHC -Wno-orphans #-}

module RMulti where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createDomain vSystem{vName="P10", vPeriod=10000}
createDomain vSystem{vName="P20", vPeriod=20000}

type Ram =
     Clock P20
  -> Clock P10
  -> Signal P10 (Unsigned 1)
  -> Signal P20 (Maybe (Unsigned 1, Unsigned 2))
  -> Signal P10 (Unsigned 2)

ram
  :: Ram
ram wClk rClk = asyncRamPow2 wClk rClk enableGen

tbOutput
  :: Ram
  -> Clock P20
  -> Clock P10
  -> Signal P10 (Unsigned 2)
tbOutput top wClk rClk = output
 where
  wrM = stimuliGenerator wClk noReset $
          Just (0, 1) :> Just (1,2) :> Nothing :> Nil
  rd = delay rClk enableGen 0 $ rd + 1
  output = ignoreFor rClk noReset enableGen d2 0 $ top wClk rClk rd wrM
{-# INLINE tbOutput #-}

tb
  :: ( KnownNat n
     , 1 <= n)
  => Ram
  -> Vec n (Unsigned 2)
  -> Signal P10 Bool
tb top expectedSamples = done
 where
  output = tbOutput top wClk rClk
  expectedOutput = outputVerifier' rClk noReset expectedSamples
  done = expectedOutput output
  (rClk, wClk) = biTbClockGen (not <$> done) :: (Clock P10, Clock P20)
{-# INLINE tb #-}
