{-# LANGUAGE CPP #-}

-- | With @-fclash-split-registers@, registers of product types are split into a
-- register per field. Checks that the field registers are named after the
-- original register and the fields, and that splitting doesn't change the
-- behavior of the circuit.
module SplitRegister where

import qualified Prelude as P
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Annotations.SynthesisAttributes

data Pair = Pair { left :: Unsigned 4, right :: Unsigned 4 }
  deriving (Generic, NFDataX, BitPack, Eq, Show, ShowX)

type Nested = ((Unsigned 4, Bool), Pair)

topEntity ::
  Clock System -> Reset System -> Enable System ->
  Signal System (Unsigned 4) ->
  Signal System (Nested, Pair, Pair, (Bool, Bool), Pair)
topEntity clk rst en x = bundle (nested, named, delayed, annotated, separate)
 where
  -- Nested products get a register per leaf
  nested = register clk rst en ((0, False), Pair 1 2) (nextNested <$> x <*> nested)
  nextNested a ((b, c), Pair d e) = ((a + b, not c), Pair (d + e) (e - a))

  -- Registers named with setName get the field name as a suffix
  named = setName @"named" (register clk rst en (Pair 3 4) (swap' <$> named <*> x))
  swap' (Pair a b) y = Pair (b + y) a

  -- No reset
  delayed = delay clk en (Pair 5 6) (Pair <$> x <*> fmap left named)

  -- Synthesis attributes are copied to all field registers
  annotated =
    setName @"annotated"
      (annotateReg (Attr "KEEP" :> Nil)
        (delay clk en (False, True) (bundle (fmap (`testBit` 0) x, fmap (`testBit` 1) x))))

  -- Registers in components that aren't inlined are split too
  separate = component clk rst en x

component ::
  Clock System -> Reset System -> Enable System ->
  Signal System (Unsigned 4) -> Signal System Pair
component clk rst en x = s
 where
  s = register clk rst en (Pair 7 8) (Pair <$> x <*> fmap left s)
{-# OPAQUE component #-}

testBench :: Signal System Bool
testBench = done
 where
  -- The output verifier checks the first value both during and right after
  -- reset. Keep the circuit disabled until then, so its output doesn't change
  -- in between: the delays don't have a reset.
  (testInput, testEnable) = unbundle $ stimuliGenerator clk rst
    (   (0, False) :> (1, True) :> (2, True) :> (3, True) :> (5, True)
     :> (8, True) :> (13, True) :> (4, True) :> (9, True) :> (0, True)
     :> (15, True) :> (7, True) :> (6, True) :> Nil)
  expectedOutput = outputVerifier' clk rst
    (1221679480 :> 1221679480 :> 54849719831 :> 108403135777 :>
     227399751474 :> 394577142355 :> 127710931077 :> 5532500696 :>
     163093224525 :> 452382053012 :> 474946207753 :> 428889854960 :>
     126684626815 :> 314160557415 :> Nil)
  done = expectedOutput (pack <$> topEntity clk rst (toEnable testEnable) testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ P.concat [ "Expected:\n\n  ", needle
                                   , "\n\nIn:\n\n", haystack ]

assertNotIn :: String -> String -> IO ()
assertNotIn needle haystack
  | needle `isInfixOf` haystack = P.error $ P.concat [ "Did not expect:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]
  | otherwise = return ()

readHdl :: FilePath -> IO String
readHdl file = do
  [topDir] <- getArgs
  readFile (topDir </> show 'topEntity </> file)

mainVerilog :: IO ()
mainVerilog = do
  content <- readHdl "topEntity.v"
  assertIn "reg [3:0] nested_sel0_sel0" content
  assertIn "reg  nested_sel0_sel1" content
  assertIn "reg [3:0] nested_sel1_left" content
  assertIn "reg [3:0] nested_sel1_right" content
  assertIn "reg [3:0] named_left" content
  assertIn "reg [3:0] named_right" content
  assertIn "(* KEEP *) reg  annotated_sel0" content
  assertIn "(* KEEP *) reg  annotated_sel1" content
  -- No registers for whole products
  assertNotIn "reg [12:0]" content
  assertNotIn "reg [7:0]" content
  assertNotIn "reg [1:0]" content

  comp <- readHdl "component.v"
  assertIn "reg [3:0] s_left" comp
  assertIn "reg [3:0] s_right" comp

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  content <- readHdl "topEntity.sv"
  assertIn "logic [3:0] nested_sel1_left" content
  assertIn "logic [3:0] named_right" content
  assertIn "(* KEEP *) logic annotated_sel0" content

mainVHDL :: IO ()
mainVHDL = do
  content <- readHdl "topEntity.vhdl"
  assertIn "signal nested_sel1_left" content
  assertIn "signal named_right" content
  assertIn "attribute KEEP of annotated_sel0 : signal is true;" content
