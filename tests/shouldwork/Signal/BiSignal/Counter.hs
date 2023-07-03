{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Counter where

import Data.Int
import Debug.Trace
import qualified Prelude as P
import Clash.Explicit.Prelude hiding (even, odd)
import Clash.Explicit.Testbench
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

-- | Write on odd cyles
odd :: Clock System
  -> Reset System
  -> Enable System
  -> BiSignalIn  'Floating System (BitSize Int)
  -> BiSignalOut 'Floating System (BitSize Int)
odd clk rst en s = writeToBiSignal s (mealy clk rst en counter (False, 0) (readFromBiSignal s))

-- | Write on even cyles
even :: Clock System
  -> Reset System
  -> Enable System
  -> BiSignalIn  'Floating System (BitSize Int)
  -> BiSignalOut 'Floating System (BitSize Int)
even clk rst en s = writeToBiSignal s (mealy clk rst en counter (True, 0) (readFromBiSignal s))


-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}
topEntity :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Int
topEntity clk rst en = readFromBiSignal bus'
  where
    bus  = mergeBiSignalOuts $ odd clk rst en bus' :> even clk rst en bus' :> Nil
    bus' = veryUnsafeToBiSignalIn bus

main :: IO()
main = do
  let done = sampleN 10 testBench
  putStrLn $ showX $ done


testBench :: Signal System Bool
testBench = done
  where
    clock          = tbSystemClockGen (not <$> done)
    done           = expectedOutput (topEntity clock systemResetGen enableGen)
    expectedOutput = outputVerifier' clock systemResetGen (1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil)
