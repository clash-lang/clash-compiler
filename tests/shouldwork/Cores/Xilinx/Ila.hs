{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Ila where

import Clash.Explicit.Prelude

import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.FilePath.Glob
import qualified Language.Haskell.TH as TH

import Clash.Annotations.TH
import Clash.Cores.Xilinx.Ila
import Clash.Explicit.Testbench

type Dom = XilinxSystem

top :: "result" ::: Unsigned 8
top = 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top #-}
makeTopEntity 'top

oneCounter :: IlaConfig 1 -> Clock Dom -> Signal Dom ()
oneCounter config clk = setName @"one_counter_ila" $ ila @Dom config clk counter
 where
  counter :: Signal Dom (Unsigned 64)
  counter = register clk noReset enableGen 0 (counter + 1)

threeCounters :: IlaConfig 3 -> Clock Dom -> Signal Dom ()
threeCounters config clk =
  setName @"three_counters_ila" $
    ila @Dom config clk counter0 counter1 counter2
 where
  counter0 :: Signal Dom (Unsigned 64)
  counter0 = register clk noReset enableGen 0 (counter0 + 1)

  counter1 :: Signal Dom (Unsigned 64)
  counter1 = register clk noReset enableGen 0 (counter1 + 2)

  counter2 :: Signal Dom (Unsigned 64)
  counter2 = register clk noReset enableGen 0 (counter2 + 3)

testWithDefaultsOne :: Clock Dom -> Signal Dom ()
testWithDefaultsOne = oneCounter (ilaConfig ("foo" :> Nil))
{-# ANN testWithDefaultsOne (TestBench 'top) #-}
{-# ANN testWithDefaultsOne (defSyn "testWithDefaultsOne") #-}

testWithDefaultsThree :: Clock Dom -> Signal Dom ()
testWithDefaultsThree = threeCounters (ilaConfig ("foo" :> "bar" :> "ipsum" :> Nil))
{-# ANN testWithDefaultsThree (TestBench 'top) #-}
{-# ANN testWithDefaultsThree (defSyn "testWithDefaultsThree") #-}

testWithLefts :: Clock Dom -> Signal Dom ()
testWithLefts = threeCounters $
  (ilaConfig ("foo" :> "bar" :> "ipsum" :> Nil))
    { comparators = Left 3
    , probeTypes = Left Data
    , depth = D2048
    , captureControl = False
    , stages = 5
    }
{-# ANN testWithLefts (TestBench 'top) #-}
{-# ANN testWithLefts (defSyn "testWithLefts") #-}

testWithRights :: Clock Dom -> Signal Dom ()
testWithRights = threeCounters $
  (ilaConfig ("foo" :> "bar" :> "ipsum" :> Nil))
    { comparators = Right (4 :> 5 :> 6 :> Nil)
    , probeTypes = Right (DataAndTrigger :> Data :> Trigger :> Nil)
    , depth = D1024
    , captureControl = True
    , stages = 3
    }
{-# ANN testWithRights (TestBench 'top) #-}
{-# ANN testWithRights (defSyn "testWithRights") #-}

testWithRightsSameCu :: Clock Dom -> Signal Dom ()
testWithRightsSameCu = threeCounters $
  (ilaConfig ("foo" :> "bar" :> "ipsum" :> Nil))
    { comparators = Right (4 :> 4 :> 4 :> Nil)
    , probeTypes = Right (Trigger :> Data :> DataAndTrigger :> Nil)
    , depth = D4096
    , captureControl = True
    , stages = 1
    , advancedTriggers = True
    }
{-# ANN testWithRightsSameCu (TestBench 'top) #-}
{-# ANN testWithRightsSameCu (defSyn "testWithRightsSameCu") #-}

mainVHDL :: IO ()
mainVHDL = do
  [dir] <- getArgs

  -- TCL content check:
  main

  -- HDL content check:
  let hdlDir = dir </> show 'testWithDefaultsOne
  [path] <- glob (hdlDir </> "Ila_testWithDefaultsOne_oneCounter*.vhdl")
  contents <- readFile path
  assertIn contents "attribute KEEP of foo : signal is \"true\";" -- signal name
  assertIn contents "one_counter_ila : testWithDefaultsOne_ila"   -- instantiation label

mainVerilog :: IO ()
mainVerilog = main

mainSystemVerilog :: IO ()
mainSystemVerilog = main

getTcl :: TH.Name -> IO String
getTcl nm = do
  [dir] <- getArgs
  let topDir = dir </> show nm
  [tclFileName] <- filter (".tcl" `isSuffixOf`) <$> listDirectory topDir
  let tclPath = topDir </> tclFileName
  readFile tclPath

assertIn :: String -> String -> IO ()
assertIn haystack needle
  | needle `isInfixOf` haystack = return ()
  | otherwise = error $ mconcat [ "Expected:\n\n  ", needle
                                , "\n\nIn:\n\n", haystack ]

main :: IO ()
main = do
  tcl <- getTcl 'testWithDefaultsOne
  assertIn tcl "C_NUM_OF_PROBES 1"
  assertIn tcl "C_INPUT_PIPE_STAGES 0"
  assertIn tcl "C_DATA_DEPTH 4096"
  assertIn tcl "ALL_PROBE_SAME_MU true"
  assertIn tcl "C_EN_STRG_QUAL 1"
  assertIn tcl "C_TRIGIN_EN false"
  assertIn tcl "ALL_PROBE_SAME_MU_CNT 2"
  assertIn tcl "C_PROBE0_WIDTH 64"
  assertIn tcl "C_PROBE0_TYPE 0"
  assertIn tcl "C_PROBE0_MU_CNT 2"
  assertIn tcl "C_ADV_TRIGGER false"

  tcl <- getTcl 'testWithDefaultsThree
  assertIn tcl "C_NUM_OF_PROBES 3"
  assertIn tcl "C_INPUT_PIPE_STAGES 0"
  assertIn tcl "C_DATA_DEPTH 4096"
  assertIn tcl "ALL_PROBE_SAME_MU true"
  assertIn tcl "C_EN_STRG_QUAL 1"
  assertIn tcl "C_TRIGIN_EN false"
  assertIn tcl "ALL_PROBE_SAME_MU_CNT 2"
  assertIn tcl "C_PROBE0_WIDTH 64"
  assertIn tcl "C_PROBE0_TYPE 0"
  assertIn tcl "C_PROBE0_MU_CNT 2"
  assertIn tcl "C_PROBE1_WIDTH 64"
  assertIn tcl "C_PROBE1_TYPE 0"
  assertIn tcl "C_PROBE1_MU_CNT 2"
  assertIn tcl "C_PROBE2_WIDTH 64"
  assertIn tcl "C_PROBE2_TYPE 0"
  assertIn tcl "C_PROBE2_MU_CNT 2"
  assertIn tcl "C_ADV_TRIGGER false"

  tcl <- getTcl 'testWithLefts
  assertIn tcl "C_NUM_OF_PROBES 3"
  assertIn tcl "C_INPUT_PIPE_STAGES 5"
  assertIn tcl "C_DATA_DEPTH 2048"
  assertIn tcl "ALL_PROBE_SAME_MU true"
  assertIn tcl "C_EN_STRG_QUAL 0"
  assertIn tcl "C_TRIGIN_EN false"
  assertIn tcl "ALL_PROBE_SAME_MU_CNT 3"
  assertIn tcl "C_PROBE0_WIDTH 64"
  assertIn tcl "C_PROBE0_TYPE 1"
  assertIn tcl "C_PROBE0_MU_CNT 3"
  assertIn tcl "C_PROBE1_WIDTH 64"
  assertIn tcl "C_PROBE1_TYPE 1"
  assertIn tcl "C_PROBE1_MU_CNT 3"
  assertIn tcl "C_PROBE2_WIDTH 64"
  assertIn tcl "C_PROBE2_TYPE 1"
  assertIn tcl "C_PROBE2_MU_CNT 3"
  assertIn tcl "C_ADV_TRIGGER false"

  tcl <- getTcl 'testWithRights
  assertIn tcl "C_NUM_OF_PROBES 3"
  assertIn tcl "C_INPUT_PIPE_STAGES 3"
  assertIn tcl "C_DATA_DEPTH 1024"
  assertIn tcl "ALL_PROBE_SAME_MU false"
  assertIn tcl "C_EN_STRG_QUAL 1"
  assertIn tcl "C_TRIGIN_EN false"
  assertIn tcl "C_PROBE0_WIDTH 64"
  assertIn tcl "C_PROBE0_TYPE 0"
  assertIn tcl "C_PROBE0_MU_CNT 4"
  assertIn tcl "C_PROBE1_WIDTH 64"
  assertIn tcl "C_PROBE1_TYPE 1"
  assertIn tcl "C_PROBE1_MU_CNT 5"
  assertIn tcl "C_PROBE2_WIDTH 64"
  assertIn tcl "C_PROBE2_TYPE 2"
  assertIn tcl "C_PROBE2_MU_CNT 6"
  assertIn tcl "C_ADV_TRIGGER false"

  tcl <- getTcl 'testWithRightsSameCu
  assertIn tcl "C_NUM_OF_PROBES 3"
  assertIn tcl "C_INPUT_PIPE_STAGES 1"
  assertIn tcl "C_DATA_DEPTH 4096"
  assertIn tcl "ALL_PROBE_SAME_MU true"
  assertIn tcl "C_EN_STRG_QUAL 1"
  assertIn tcl "C_TRIGIN_EN false"
  assertIn tcl "ALL_PROBE_SAME_MU_CNT 4"
  assertIn tcl "C_PROBE0_WIDTH 64"
  assertIn tcl "C_PROBE0_TYPE 2"
  assertIn tcl "C_PROBE0_MU_CNT 4"
  assertIn tcl "C_PROBE1_WIDTH 64"
  assertIn tcl "C_PROBE1_TYPE 1"
  assertIn tcl "C_PROBE1_MU_CNT 4"
  assertIn tcl "C_PROBE2_WIDTH 64"
  assertIn tcl "C_PROBE2_TYPE 0"
  assertIn tcl "C_PROBE2_MU_CNT 4"
  assertIn tcl "C_ADV_TRIGGER true"
