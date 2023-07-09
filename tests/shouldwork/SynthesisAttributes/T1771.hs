module T1771 where

import qualified Prelude as P

import qualified Data.List as List
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

-- Keeping a single annotation in a type synonym should not prevent it from
-- being rendered, or throw an error during compilation.
type AttrSingle x = 'StringAttr "foo" x

-- Keeping multiple annotations in a type synonym should not prevent them from
-- being rendered, or throw an error during compilation.
type AttrMulti = '[ AttrSingle "BB", 'BoolAttr "bar" 'True ]

-- Hiding the Annotate synonym in another type synonym should not prevent it
-- from being rendered, or throw an error during compilation.
type PinC = Annotate (BitVector 1) '[ AttrSingle "CC" ]

{-# ANN topEntity (Synthesize
      { t_name   = "topEntity"
      , t_inputs = [PortName "pin_a", PortName "pin_b", PortName "pin_c"]
      , t_output = PortName "res"
      }) #-}
topEntity
  :: BitVector 1 `Annotate` AttrSingle "AA"
  -> BitVector 1 `Annotate` AttrMulti
  -> PinC
  -> BitVector 1
topEntity = \x y -> const const x y

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | List.isInfixOf needle haystack = pure ()
  | otherwise = P.error $ mconcat
      [ "Expected:\n\n  ", needle, "\n\nIn:\n\n", haystack]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.vhdl")

  assertIn "attribute foo of pin_a : signal is \"AA\";" content
  assertIn "attribute foo of pin_b : signal is \"BB\";" content
  assertIn "attribute bar of pin_b : signal is true;" content
  assertIn "attribute foo of pin_c : signal is \"CC\";" content

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.v")

  assertIn "(* foo = \"AA\" *) input wire [0:0] pin_a" content
  assertIn "(* foo = \"BB\", bar = 1 *) input wire [0:0] pin_b" content
  assertIn "(* foo = \"CC\" *) input wire [0:0] pin_c" content

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content <- readFile (topDir </> show 'topEntity </> "topEntity.sv")

  assertIn "(* foo = \"AA\" *) input wire logic [0:0] pin_a" content
  assertIn "(* foo = \"BB\", bar = 1 *) input wire logic [0:0] pin_b" content
  assertIn "(* foo = \"CC\" *) input wire logic [0:0] pin_c" content
