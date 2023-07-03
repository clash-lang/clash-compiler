{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module T2117 where

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.Annotations.Primitive
import Data.String.Interpolate (__i)

undefBV
  :: Signal System Bool
undefBV = testUndefined @(BitVector 8) (deepErrorX "undefined value")
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE undefBV #-}

undefTup
  :: Signal System Bool
undefTup = testUndefined @(Bit, Bit) (deepErrorX "undefined value")
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE undefTup #-}

partialDefTup
  :: Signal System Bool
partialDefTup = testDefined @(Bit, Bit) (errorX "undefined value", 0)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE partialDefTup #-}

testBenchG
  :: Signal System Bool
  -> Signal System Bool
testBenchG f = done
 where
  done = outputVerifier' clk rst (True :> Nil) f
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
{-# INLINE testBenchG #-}

testBenchUndefBV :: Signal System Bool
testBenchUndefBV = testBenchG undefBV
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBenchUndefBV #-}
{-# ANN testBenchUndefBV (TestBench 'undefBV) #-}

testBenchUndefTup :: Signal System Bool
testBenchUndefTup = testBenchG undefTup
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBenchUndefTup #-}
{-# ANN testBenchUndefTup (TestBench 'undefTup) #-}

testBenchPartialDefTup :: Signal System Bool
testBenchPartialDefTup = testBenchG partialDefTup
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBenchPartialDefTup #-}
{-# ANN testBenchPartialDefTup (TestBench 'partialDefTup) #-}

-- Only call with XException-argument if you want the model to match the HDL.
testUndefined
  :: NFDataX a
  => a
  -> Signal System Bool
testUndefined !_ = pure True
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testUndefined #-}
{-# ANN testUndefined (InlinePrimitive [VHDL] [__i|
  [ { "BlackBox" :
      { "name"      : "T2117.testUndefined"
      , "kind"      : "Expression"
      , "template"  : "~IF ~ISUNDEFINED[1] ~THEN true ~ELSE false ~FI"
      }
    }
  ]
  |]) #-}

-- Only call with an argument that has bits defined if you want the model to
-- match the HDL.
testDefined
  :: NFDataX a
  => a
  -> Signal System Bool
testDefined !_ = pure True
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testDefined #-}
{-# ANN testDefined (InlinePrimitive [VHDL] [__i|
  [ { "BlackBox" :
      { "name"      : "T2117.testDefined"
      , "kind"      : "Expression"
      , "template"  : "~IF ~ISUNDEFINED[1] ~THEN false ~ELSE true ~FI"
      }
    }
  ]
  |]) #-}
