{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Blob where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createMemBlob @4 "content" Nothing [4 .. 7]

topEntity
  :: Clock System
  -> Enable System
  -> Signal System (Unsigned 2)
  -> Signal System (Maybe (Unsigned 2, Unsigned 4))
  -> Signal System (Unsigned 4, Unsigned 4)
topEntity clk en rd wrM =
  let ram en0 = unpack <$> blockRamBlob clk en0 content rd wrM0
      wrM0 = fmap (fmap (\(wr, din) -> (wr, pack din))) wrM
  in bundle (ram enableGen, ram en)
{-# NOINLINE topEntity #-}

samples :: Vec _ (Unsigned 2, Maybe (Unsigned 2, Unsigned 4), Unsigned 4)
samples =
  -- rd  wrM          out

     -- Read initial contents
     (0, Nothing     , 15)
  :> (1, Nothing     ,  4)
  :> (2, Nothing     ,  5)
     -- Write and read back
  :> (3, Just (0,  8),  6)
  :> (0, Just (1,  9),  7)
  :> (1, Just (2, 10),  8)
  :> (2, Just (3, 11),  9)
  :> (3, Nothing     , 10)
  :> (3, Nothing     , 11)
  :> Nil

testBench :: Signal System Bool
testBench = done
  where
    (rd, wrM, expect) = unzip3 samples
    rdInput = stimuliGenerator clk rst rd
    wrMInput = stimuliGenerator clk rst wrM
    expectedOutput =
      outputVerifier' clk rst $ zip expect expect
    done = expectedOutput $ ignoreFor clk rst en d1 (15, 15) $
             topEntity clk en rdInput wrMInput
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen
{-# NOINLINE testBench #-}
