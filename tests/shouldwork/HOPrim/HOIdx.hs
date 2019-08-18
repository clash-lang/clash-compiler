module HOIdx where

import Clash.Prelude
import Clash.Explicit.Testbench

hoIdx ks i a = (ks !! i) a

f g h = hoIdx (g :> h :> Nil)

work :: Int -> Int -> Int -> Int -> Int
work a b = f (+a) (*b)

topEntity :: (Int,Int,Int,Int) -> Int
topEntity (a,b,idx,x) = work a b idx x

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((1,2,0,3):>(1,2,1,3):>Nil)
    expectedOutput = outputVerifier' clk rst (4:>6:>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
