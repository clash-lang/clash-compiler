{-# LANGUAGE TemplateHaskell #-}

module THCorePluginRegistration where

import Clash.Prelude
import Clash.Explicit.Testbench
import Language.Haskell.TH.Syntax (lift)
import THCorePluginHelper (pluginValue)

topEntity :: Signal System Int
topEntity = pure $(
  if pluginValue == 42 then
    lift pluginValue
  else
    fail "Core plugin registered through TH did not rewrite the helper")

testBench :: Signal System Bool
testBench = done
 where
  done = outputVerifier' clk rst (42 :> Nil) topEntity
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
