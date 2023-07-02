{-# LANGUAGE CPP #-}

module MatrixVect where

import Clash.Prelude
import Clash.Explicit.Testbench
import qualified Data.List as L

row1 = 1 :> 2 :> 3 :> Nil
row2 = 4 :> 5 :> 6 :> Nil
row3 = 7 :> 8 :> 9 :> Nil

matrix = row1 :> row2 :> row3 :> Nil
vector = 2 :> 3 :> 4 :> Nil

dotProduct xs ys = foldr (+) 0 (zipWith (*) xs ys)
matrixVector m v = map (`dotProduct` v) m

topEntity :: Vec 3 (Signed 16) -> Vec 3 (Signed 16)
topEntity = matrixVector matrix
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((2 :> 3 :> 4 :> Nil) :> Nil)
    expectedOutput = outputVerifier' clk rst ((20 :> 47 :> 74 :> Nil) :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
