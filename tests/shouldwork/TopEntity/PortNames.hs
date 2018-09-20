module PortNames where

import qualified Prelude as P
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Regex.PCRE ((=~))

import Clash.Prelude
import Clash.Explicit.Testbench

{-# ANN topEntity
  (Synthesize
    { t_name     = "PortNames_topEntity"
    , t_inputs   = [
        ]
    , t_output   = PortProduct "" [
            PortName "result",
            PortName "out2222"
        ]
    }) #-}
topEntity :: (Signal System Bool, Signal System Bool)
topEntity = (pure True, pure False)

-- Simulation test
{-# ANN testBench
  (Synthesize
    { t_name     = "PortNames_testBench"
    , t_inputs   = [ ]
    , t_output   = PortName "result"
    }) #-}
testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst ((True, False) :> (True, False) :> Nil)
    done           = expectedOutput (bundle topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

-- File content test
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | haystack =~ needle = return ()
  | otherwise = P.error $ P.concat [ "Expected:\n\n  ", needle
                                   , "\n\nIn:\n\n", haystack ]

mainVerilog :: IO ()
mainVerilog = do
  [modDir, topFile] <- getArgs
  content <- readFile "verilog/PortNames/PortNames_topEntity/PortNames_topEntity.v"

  assertIn "out2222" content
