module ClockConfiguration where

import Clash.Explicit.Prelude
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

createDomain vSystem{vName="Fast", vPeriod=5000.5, vActiveEdge=Falling}

topEntity :: Clock Fast -> Bool -> Bool
topEntity _ = not
{-# ANN topEntity (Synthesize "topEntity" [PortName "clk", PortName "x"] (PortName "result")) #-}

mainVerilog :: IO ()
mainVerilog = do
  [root] <- getArgs
  sdc <- readFile (root </> show 'topEntity </> "topEntity.sdc")
  unless ("-period 5.000500 -waveform {0.000 2.500250}" `isInfixOf` sdc) $
    fail "Clock constraint lost its fractional period"
