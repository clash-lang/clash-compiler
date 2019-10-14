{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TypeInType            #-}

module Scaffold where

import qualified Prelude as P

import Clash.Prelude hiding (Text)
import Clash.Explicit.Testbench

import           Clash.Primitives.Scaffold



multiplyPrim
  :: Signal System (BitVector 8)
  -> Signal System (BitVector 8)
  -> Signal System (BitVector 8)
multiplyPrim a b =
  a * b
{-# NOINLINE multiplyPrim #-}
{-# ANN multiplyPrim
  (Synthesize
    { t_name     = "mult_prim"
    , t_inputs   = [ PortName "lhs", PortName "rhs" ]
    , t_output   = PortName "result"
    }) #-}

makeScaffold "multiply" "mult_prim"
  [ ]
  [ [ In "lhs" 8
    , In "rhs" 8
    , Out "result" 8
    ]
  ]

topEntity
  :: Signal System (BitVector 8)
  -> Signal System (BitVector 8)
topEntity a = _result $ multiply (MultiplyI a a)
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  done = outputVerifier' clk aclr (4:>9:>16:>64:>81:>100:>Nil) res
  res  = topEntity inp
  inp  = stimuliGenerator clk aclr (2:>3:>4:>8:>9:>10:>Nil)
  clk  = tbSystemClockGen (not <$> done)
  aclr = systemResetGen
