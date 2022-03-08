{-# LANGUAGE BangPatterns #-}
module T2117 where

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.Annotations.Primitive
import Data.String.Interpolate.IsString (i)
import Data.String.Interpolate.Util (unindent)

undefBV
  :: Signal System Bool
undefBV = testUndefined @(BitVector 8) (deepErrorX "undefined value")
{-# NOINLINE undefBV #-}

undefTup
  :: Signal System Bool
undefTup = testUndefined @(Bit, Bit) (deepErrorX "undefined value")
{-# NOINLINE undefTup #-}

partialDefTup
  :: Signal System Bool
partialDefTup = testDefined @(Bit, Bit) (errorX "undefined value", 0)
{-# NOINLINE partialDefTup #-}

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
{-# NOINLINE testBenchUndefBV #-}
{-# ANN testBenchUndefBV (TestBench 'undefBV) #-}

testBenchUndefTup :: Signal System Bool
testBenchUndefTup = testBenchG undefTup
{-# NOINLINE testBenchUndefTup #-}
{-# ANN testBenchUndefTup (TestBench 'undefTup) #-}

testBenchPartialDefTup :: Signal System Bool
testBenchPartialDefTup = testBenchG partialDefTup
{-# NOINLINE testBenchPartialDefTup #-}
{-# ANN testBenchPartialDefTup (TestBench 'partialDefTup) #-}

-- Only call with XException-argument if you want the model to match the HDL.
testUndefined
  :: NFDataX a
  => a
  -> Signal System Bool
testUndefined !_ = pure True
{-# NOINLINE testUndefined #-}
{-# ANN testUndefined (InlinePrimitive [VHDL] $ unindent [i|
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
{-# NOINLINE testDefined #-}
{-# ANN testDefined (InlinePrimitive [VHDL] $ unindent [i|
  [ { "BlackBox" :
      { "name"      : "T2117.testDefined"
      , "kind"      : "Expression"
      , "template"  : "~IF ~ISUNDEFINED[1] ~THEN false ~ELSE true ~FI"
      }
    }
  ]
  |]) #-}
