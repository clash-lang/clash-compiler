module T1556 where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Clash.Explicit.Testbench

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> Signal System (Unsigned 8)
topEntity clk rst en = o2
    where
        o1 = withClockResetEnable clk rst enMerged $ register 1 $ succ <$> o1
        o2 = CEP.register clk rst enMerged 0 o1
        enMerged = CEP.andEnable en (pure True)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
    where
        o = topEntity clk rst en
        expectedOutput
            = outputVerifier' clk rst (0 :> 1 :> 2 :> Nil)
        done = expectedOutput o
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen
        en = tbEnableGen
