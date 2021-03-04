module PortNamesWithUnit where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Explicit.Testbench

{-# ANN topEntity
  (Synthesize
    { t_name     = "PortNamesWithUnit_topEntity"
    , t_inputs   = [
        ]
    , t_output   = PortProduct "top" [
            PortName "zero",
            PortProduct "sub" [
              PortName "one",
              PortName "two",
              PortName "three"
            ]
        ]
    }) #-}
topEntity :: (Signal System Int, (Signal System (), Signal System Int, Signal System Int))
topEntity = (pure 0, (pure (), pure 2, pure 3))

-- Simulation test
{-# ANN testBench
  (Synthesize
    { t_name     = "PortNamesWithUnit_testBench"
    , t_inputs   = [ ]
    , t_output   = PortName "result"
    }) #-}
testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst ((0, ((), 2, 3)) :> (0, ((), 2, 3)) :> Nil)
    done           = expectedOutput (bundle $ bundle <$> topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen

-- File content test
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | needle `isInfixOf` haystack =
      P.error $ P.concat [ "Did not expect:\n\n  ", needle
                         , "\n\nIn:\n\n", haystack ]
  | otherwise = return ()

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "PortNamesWithUnit_topEntity.v")

  assertIn    "top_zero" content
  assertNotIn "top_sub_one" content
  assertIn    "top_sub_two" content
  assertIn    "top_sub_three" content
