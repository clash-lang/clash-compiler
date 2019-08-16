module Map where

import Clash.Prelude
import Clash.Explicit.Testbench

go
  :: SystemClockResetEnable
  => ()
  -> Signal System Bool
  -> Signal System Bool
go _ = mealy update initialState
  where
    initialState :: (Bool, SatIndex 'SatError 5)
    initialState = (False, minBound)

    update (c, index) i = ((c, index), c `xor` i)

topEntity
  :: SystemClockResetEnable
  => Signal System (Vec 4 Bool)
  -> Signal System (Vec 4 Bool)
topEntity =
  bundle . fmap (go ()) . unbundle
{-# NOINLINE topEntity #-}


testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((True :> True :> False :> False :> Nil) :> Nil)
    expectedOutput = outputVerifier' clk rst ((True :> True :> False :> False :>Nil):>Nil)
    done           = expectedOutput (exposeClockResetEnable topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
