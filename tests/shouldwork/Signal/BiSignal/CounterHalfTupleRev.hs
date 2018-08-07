{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module CounterHalfTupleRev where

import Data.Int
import Debug.Trace
import qualified Prelude as P
import Clash.Explicit.Prelude
import Clash.Signal hiding (sampleN)

-- | Alternatively read / increment+write
counter :: (Bool, Int)
        -- ^ Internal flip + previous read
        -> Int
        -- ^ Int from inout
        -> ((Bool, Int), Maybe Int)
counter (write, prevread) i = ((write', prevread'), output)
  where
    output    = if write then Just (succ prevread) else Nothing
    prevread' = if write then prevread else i
    write' = not write
{-# NOINLINE counter #-}

-- | Write on odd cyles
f :: Clock System Source
  -> Reset System Asynchronous
  -> BiSignalIn 'Undefined System (BitSize Int)
  -> ( BiSignalOut 'Undefined System (BitSize Int)
     , Signal System Int
     )
f clk rst s = (writeToBiSignal s (mealy clk rst counter (False, 0) (readFromBiSignal s)), 6)
{-# NOINLINE f #-}

-- | Write on even cyles
g :: Clock System Source
  -> Reset System Asynchronous
  -> BiSignalIn 'Undefined System (BitSize Int)
  -> ( BiSignalOut 'Undefined System (BitSize Int)
     , Signal System Int
     )
g clk rst s = (writeToBiSignal s (mealy clk rst counter (True, 0) (readFromBiSignal s)), 7)
{-# NOINLINE g #-}


topEntity :: Clock System Source
          -> Reset System Asynchronous
          -> ( Signal System Int
             , Signal System Int
             , Signal System Int
             )
topEntity clk rst = ( readFromBiSignal bus'
                    , fInt
                    , gInt
                    )
  where
    (fBus, fInt) = f clk rst bus'
    (gBus, gInt) = g clk rst bus'

    bus  = mergeBiSignalOuts (fBus :> gBus :> Nil)
    bus' = veryUnsafeToBiSignalIn bus
{-# NOINLINE topEntity #-}


main :: IO()
main = do
  --let ints = sampleN 10 (topEntity systemClock systemReset)
  let done = sampleN 10 testBench
  putStrLn $ showX $ done


testBench :: Signal System Bool
testBench = done
  where
    clock          = tbSystemClockGen (not <$> done)
    (a, _b, _c)    = topEntity clock systemResetGen
    done           = expectedOutput a
    expectedOutput = outputVerifier clock systemResetGen (1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil)
