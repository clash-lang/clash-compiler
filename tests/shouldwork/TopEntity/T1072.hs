module T1072 where

import Clash.Explicit.Prelude
import Data.List
import System.Environment
import System.FilePath
import qualified Prelude as P

topEntity2
  :: (Clock System, Reset System)
  -> (Enable System, Signal System Int)
  -> Signal System Int
topEntity2 (clk, rst) (en, a) = register clk rst en 0 a
{-# NOINLINE topEntity2 #-}

{-# ANN topEntity
  (Synthesize
    { t_name   = "top"
    , t_inputs = [ PortProduct "" [ PortName "theClock", PortName "theReset"]
                 , PortProduct "" [PortName "theEnable", PortName "theA" ] ]
    , t_output = PortName "theResult" }
  )#-}
topEntity = topEntity2

assertIn :: String -> String -> IO ()
assertIn needle haystack
  | needle `isInfixOf` haystack = return ()
  | otherwise                   = P.error $ P.concat [ "Expected:\n\n  ", needle
                                                     , "\n\nIn:\n\n", haystack ]

-- VHDL test
main :: FilePath -> IO ()
main topFile = do
  content <- readFile topFile

  assertIn " theClock" content
  assertIn " theReset" content
  assertIn " theEnable" content
  assertIn " theA" content
  assertIn " theResult" content

mainVHDL :: IO ()
mainVHDL = do
  [topFile] <- getArgs
  main (replaceFileName topFile "top/top.vhdl")

mainVerilog :: IO ()
mainVerilog = do
  [topFile] <- getArgs
  main (replaceFileName topFile "top/top.v")

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  [topFile] <- getArgs
  main (replaceFileName topFile "top/top.sv")
