{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

module Constrained where

import Clash.Prelude
import Clash.Explicit.Testbench

import Data.Bits (complement)
import Data.Kind (Type)

data Bus n :: Type where
  Bus :: forall n
       . n <= 10
      => BitVector n
      -> Bus n

instance KnownNat n => Eq (Bus n) where
  Bus a == Bus b = a == b

instance KnownNat n => ShowX (Bus n) where
  showsPrecX _ _ = undefined


complementBus
  :: KnownNat n
  => Bus n
  -> Bus n
complementBus (Bus bv) = Bus (complement bv)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE complementBus #-}

topEntity :: Bus 5 -> Bus 5
topEntity = complementBus
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (Bus 1 :> Bus 2 :> Bus 3 :> Nil)
    expectedOutput = outputVerifier' clk rst (Bus 30 :> Bus 29 :> Bus 28 :> Nil)

    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
