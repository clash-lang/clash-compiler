{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module AsyncBlob where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

createMemBlob "content" Nothing [1 :: Unsigned 8 .. 16]

topEntity
  :: Signal System (Unsigned 4)
  -> Signal System (Unsigned 8)
topEntity = fmap (unpack . asyncRomBlobPow2 content)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput = register clk rst en 0 (testInput + 1)
    expectedOutput =
      outputVerifier' clk rst $ $(listToVecTH $ [1 :: Unsigned 8 .. 8])
    done = expectedOutput $ topEntity testInput
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
