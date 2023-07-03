{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module T1786 where

import           Clash.Prelude

import           Clash.Explicit.Testbench
import           Clash.Annotations.Primitive

import           Data.String.Interpolate (__i)

testEnable :: Signal System Bool
testEnable = testAlwaysEnabled enableGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testEnable #-}

-- Only call with always-enabled Enable if you want the model to match the HDL.
testAlwaysEnabled
  :: Enable System
  -> Signal System Bool
testAlwaysEnabled !_ = pure True
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testAlwaysEnabled #-}
{-# ANN testAlwaysEnabled (InlinePrimitive [VHDL] [__i|
  [ { "BlackBox" :
      { "name"      : "T1786.testAlwaysEnabled"
      , "kind"      : "Expression"
      , "template"  : "~IF ~ISACTIVEENABLE[0] ~THEN false ~ELSE true ~FI"
      }
    }
  ]
  |]) #-}

testEnableTB :: Signal System Bool
testEnableTB = done
 where
  done = outputVerifier' clk rst (True :> Nil) testEnable
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testEnableTB #-}
{-# ANN testEnableTB (TestBench 'testEnable) #-}

testBool :: Signal System Bool
testBool = testAlwaysEnabledBool (pure True)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBool #-}

-- Only call with always-true input if you want the model to match the HDL.
testAlwaysEnabledBool
  :: Signal System Bool
  -> Signal System Bool
testAlwaysEnabledBool !_ = pure True
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testAlwaysEnabledBool #-}
{-# ANN testAlwaysEnabledBool (InlinePrimitive [VHDL] [__i|
  [ { "BlackBox" :
      { "name"      : "T1786.testAlwaysEnabledBool"
      , "kind"      : "Expression"
      , "template"  : "~IF ~ISACTIVEENABLE[0] ~THEN false ~ELSE true ~FI"
      }
    }
  ]
  |]) #-}

testBoolTB :: Signal System Bool

testBoolTB = done
 where
  done = outputVerifier' clk rst (True :> Nil) testBool
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBoolTB #-}
{-# ANN testBoolTB (TestBench 'testBool) #-}
