{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}
module DivZero where

import Clash.Prelude
import Clash.Explicit.Testbench
import GHC.Natural
import qualified Data.List as L

type T8 = (BitVector 8, Unsigned 8, Signed 8, Index 8, Int, Word, Integer)

topEntity :: T8 -> T8
topEntity (b,u,s,x,i,w,nX) =
  ( 4 `quot` b
  , 4 `quot` u
  , 4 `quot` s
  , 4 `quot` x
  , 4 `quot` i
  , 4 `quot` w
  , 4 `quot` nX)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

t8ToEnum :: T8 -> (BitVector 8, Unsigned 8, Signed 8, Index 8, Int, Word, Int)
t8ToEnum (b,u,s,x,i,w,nX) = (b,u,s,x,i,w, fromEnum nX)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (0, 0, 0, 0, 0, 0, 0)
    expectedOutput = outputVerifierBitVector' clk rst
                       ($(bLit (L.replicate (3 * 8 + 3 + 3 * 64) '.')) :> Nil)
    done           = expectedOutput (pack . t8ToEnum . topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
