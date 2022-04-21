-- Test that enable lines are treated specially by Clash, meaning they have
-- the type "main_types.en_system" in the generated HDL.
--
module T1171 where

import Prelude as P

import Data.List
import System.Environment
import System.FilePath

import Clash.Prelude

{-# ANN f (Synthesize
      { t_name   = "f"
      , t_inputs =
          [ PortProduct ""
              [ PortName "clk"
              , PortName "rst"
              , PortName "en"
              ]
          , PortName "bv"
          ]
      , t_output = PortName "res"
      })
  #-}
f
  :: (HiddenClockResetEnable System)
  => Signal System (BitVector 4)
  -> Signal System Bool
f = fmap (\x -> msb x == lsb x)

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise = P.error $ mconcat [ "Expected:\n\n  ", needle
                                  , "\n\nIn:\n\n", haystack ]

mainVHDL :: IO ()
mainVHDL = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'f </> "f.vhdl")

  assertIn "en  : in f_types.en_System;" content

mainVerilog :: IO ()
mainVerilog = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'f </> "f.v")

  assertIn "input  en // enable" content

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topDir] <- getArgs
  content  <- readFile (topDir </> show 'f </> "f.sv")

  assertIn "input wire logic en  // enable" content
