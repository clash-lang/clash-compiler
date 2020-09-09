{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module T1506B where

import Clash.Prelude

import qualified T1506A
import Control.Monad (when)
import qualified Clash.Util.Interpolate as I
import qualified Data.List as L
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory)

topEntity = T1506A.topEntity

countLinesContaining :: String -> String -> Int
countLinesContaining needle haystack = L.length $ L.filter (needle `L.isInfixOf`) $ lines haystack

mainHDL :: String -> IO ()
mainHDL topFile = do
  [topDir] <- getArgs
  content <- readFile (takeDirectory topDir </> topFile)
  let
    xRstCount = countLinesContaining "xRst" content
    iRstCount = countLinesContaining "iRst" content
    xRstCountExp = 7
    iRstCountExp = 13

  when (xRstCountExp /= xRstCount || iRstCountExp /= iRstCount) $ error $ [I.i|
    Found #{xRstCount} xRst and #{iRstCount} iRst occurrences, but expected
    #{xRstCountExp} and #{iRstCountExp} instead in #{topFile}.
  |]

mainSystemVerilog, mainVerilog, mainVHDL :: IO ()
mainSystemVerilog = mainHDL "topEntity_0.sv"
mainVerilog       = mainHDL "topEntity_0.v"
mainVHDL          = mainHDL "topentity_0.vhdl"
