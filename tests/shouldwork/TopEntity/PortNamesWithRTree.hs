module PortNamesWithRTree where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Explicit.Testbench

data A = A Int Int deriving (Eq, Generic, ShowX)
data B = B Int Int Int deriving (Eq, Generic, ShowX)

{-# ANN topEntity
  (Synthesize
    { t_name     = "PortNamesWithRTree_topEntity"
    , t_inputs   = [
        ]
    , t_output   = PortProduct "tupje" [
            PortName "zero",
            PortProduct "treetje" [
              PortProduct "elmje1" [
                PortName "one",
                PortName "two"
              ],
              PortProduct "elmje2" [
                PortProduct "A" [
                  PortName "ein",
                  PortName "zwei"
                ],
                PortProduct "B" [
                  PortName "ein"
                ]
              ]
            ]
        ]
    }) #-}
topEntity :: Signal System (Int, RTree 2 (A, B))
topEntity = pure (0, trepeat (A 1 2, B 3 4 5))

-- Simulation test
{-# ANN testBench
  (Synthesize
    { t_name     = "PortNamesWithRTree_testBench"
    , t_inputs   = [ ]
    , t_output   = PortName "result"
    }) #-}
testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst ((0, trepeat (A 1 2, B 3 4 5)) :> Nil)
    done           = expectedOutput topEntity
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
  content <- readFile (topDir </> show 'topEntity </> "PortNamesWithRTree_topEntity.v")

  assertIn    "tupje_zero" content
  assertIn    "tupje_treetje_elmje1_one" content
  assertIn    "tupje_treetje_elmje1_two" content
  assertIn    "tupje_treetje_elmje2_A_ein" content
  assertIn    "tupje_treetje_elmje2_A_zwei" content
  assertIn    "tupje_treetje_elmje2_B_ein" content
  assertNotIn "tupje_treetje_elmje2_B_0" content
  assertIn    "tupje_treetje_elmje2_B_1" content
  assertIn    "tupje_treetje_elmje2_B_2" content
  assertIn    "tupje_treetje_2_0_0" content
  assertIn    "tupje_treetje_2_0_1" content
  assertIn    "tupje_treetje_2_1_0" content
  assertIn    "tupje_treetje_2_1_1" content
  assertIn    "tupje_treetje_2_1_2" content
  assertIn    "tupje_treetje_3_0_0" content
  assertIn    "tupje_treetje_3_0_1" content
  assertIn    "tupje_treetje_3_1_0" content
  assertIn    "tupje_treetje_3_1_1" content
  assertIn    "tupje_treetje_3_1_2" content
