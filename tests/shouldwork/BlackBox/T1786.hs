{-# LANGUAGE BangPatterns #-}
module T1786 where

import           Clash.Prelude

import           Clash.Explicit.Testbench
import           Clash.Annotations.Primitive

import           Data.String.Interpolate (__i)

testEnable :: Signal System Bool
testEnable = testAlwaysEnabled enableGen
{-# NOINLINE testEnable #-}

-- Only call with always-enabled Enable if you want the model to match the HDL.
testAlwaysEnabled
  :: Enable System
  -> Signal System Bool
testAlwaysEnabled !_ = pure True
{-# NOINLINE testAlwaysEnabled #-}
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
{-# NOINLINE testEnableTB #-}
{-# ANN testEnableTB (TestBench 'testEnable) #-}

testBool :: Signal System Bool
testBool = testAlwaysEnabledBool (pure True)
{-# NOINLINE testBool #-}

-- Only call with always-true input if you want the model to match the HDL.
testAlwaysEnabledBool
  :: Signal System Bool
  -> Signal System Bool
testAlwaysEnabledBool !_ = pure True
{-# NOINLINE testAlwaysEnabledBool #-}
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
{-# NOINLINE testBoolTB #-}
{-# ANN testBoolTB (TestBench 'testBool) #-}
