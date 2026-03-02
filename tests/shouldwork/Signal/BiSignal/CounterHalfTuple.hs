{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module CounterHalfTuple where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Signal hiding (sampleN)
import Data.Int
import Debug.Trace
import qualified Prelude as P

-- | Alternatively read / increment+write
counter ::
  -- | Internal flip + previous read
  (Bool, Int) ->
  -- | Int from inout
  Int ->
  ((Bool, Int), Maybe Int)
counter (write, prevread) i = ((write', prevread'), output)
 where
  output = if write then Just (succ prevread) else Nothing
  prevread' = if write then prevread else i
  write' = not write
{-# OPAQUE counter #-}

-- | Write on odd cyles
f ::
  Clock System ->
  Reset System ->
  Enable System ->
  BiSignalIn 'Floating System (BitSize Int) ->
  ( Signal System Int
  , BiSignalOut 'Floating System (BitSize Int)
  )
f clk rst en s = (6, writeToBiSignal s (mealy clk rst en counter (False, 0) (readFromBiSignal s)))
{-# OPAQUE f #-}

-- | Write on even cyles
g ::
  Clock System ->
  Reset System ->
  Enable System ->
  BiSignalIn 'Floating System (BitSize Int) ->
  ( Signal System Int
  , BiSignalOut 'Floating System (BitSize Int)
  )
g clk rst en s = (7, writeToBiSignal s (mealy clk rst en counter (True, 0) (readFromBiSignal s)))
{-# OPAQUE g #-}

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  ( Signal System Int
  , Signal System Int
  , Signal System Int
  )
topEntity clk rst en =
  (readFromBiSignal bus', fInt, gInt)
 where
  (fInt, fBus) = f clk rst en bus'
  (gInt, gBus) = g clk rst en bus'

  bus = mergeBiSignalOuts (fBus :> gBus :> Nil)
  bus' = veryUnsafeToBiSignalIn bus
{-# OPAQUE topEntity #-}

main :: IO ()
main = do
  -- let ints = sampleN 10 (topEntity systemClock systemReset)
  let done = sampleN 10 testBench
  putStrLn $ showX $ done

testBench :: Signal System Bool
testBench = done
 where
  clock = tbSystemClockGen (not <$> done)
  (a, _b, _c) = topEntity clock systemResetGen enableGen
  done = expectedOutput a
  expectedOutput = outputVerifier' clock systemResetGen (1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil)
