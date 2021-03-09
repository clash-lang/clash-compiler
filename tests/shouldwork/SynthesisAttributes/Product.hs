{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Product where

import qualified Prelude as P
import Prelude ((++))

import Clash.Prelude hiding (assert, (++))
import Clash.Prelude.Testbench
import Clash.Annotations.SynthesisAttributes

import Data.List (isInfixOf)
import Data.String (IsString)
import System.Environment (getArgs)
import System.FilePath ((</>))

import qualified Data.Text as T
import qualified Data.Text.IO as T

--------------- Logic -------------------
mac
  :: SystemClockResetEnable
  => Signal System (Signed 9, Signed 9)
  -> Signal System (Signed 9)
mac xy = mealy macT 0 xy
  where
    macT acc (x,y) = (acc',o)
      where
        acc' = acc + x * y
        o    = acc

topEntity
  :: SystemClockResetEnable
  => Signal System ( Signed 9 `Annotate` 'StringAttr "top" "input1"
                   , Signed 9 `Annotate` 'StringAttr "top" "input2"
                   )
  -> Signal System ( Signed 9 `Annotate` 'StringAttr "top" "output1"
                   , Signed 9 `Annotate` 'StringAttr "top" "output2"
                   )
topEntity xy = bundle (s, s)
  where
    s = mac xy
{-# NOINLINE topEntity #-}


--------------- Actual tests for generated HDL -------------------
assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

-- VHDL test
mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  assertIn "attribute top : string;" content
  assertIn " : signal is \"input1\"" content
  assertIn " : signal is \"input2\"" content
  assertIn " : signal is \"output1\"" content
  assertIn " : signal is \"output2\"" content

-- Verilog test
mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")

  assertIn "(* top = \"input1\" *) input" content
  assertIn "(* top = \"input2\" *) input" content
  assertIn "(* top = \"output1\" *) output" content
  assertIn "(* top = \"output2\" *) output" content

-- Verilog and SystemVerilog should share annotation syntax
mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.sv")

  assertIn "(* top = \"input1\" *) input" content
  assertIn "(* top = \"input2\" *) input" content
  assertIn "(* top = \"output1\" *) output" content
  assertIn "(* top = \"output2\" *) output" content


-- Simulation tests
testBench :: Signal System Bool
testBench = done'
  where
    testInput :: _ => _
    testInput      = stimuliGenerator $(listToVecTH [ (1, 1) :: (Signed 9, Signed 9)
                                                    , (2, 2)
                                                    , (3, 3)
                                                    , (4, 4)
                                                    ])

    expectedOutput a = outputVerifier' $(listToVecTH [ (0, 0) :: (Signed 9, Signed 9)
                                                  , (1, 1)
                                                  , (5, 5)
                                                  , (14, 14)
                                                  ]) a

    done :: _ => _
    done           = expectedOutput (topEntity testInput)
    done'          = withClockResetEnable (tbSystemClockGen (not <$> done')) systemResetGen enableGen done
