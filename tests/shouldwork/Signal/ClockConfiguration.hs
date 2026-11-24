module ClockConfiguration where

import Clash.Explicit.Prelude
import Clash.Annotations.Primitive (Primitive(..), HDL(Verilog))
import Clash.Signal.Internal (ClockN)
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Environment (getArgs)
import System.FilePath ((</>))

createDomain vSystem{vName="Fast", vPeriod=5000.5, vActiveEdge=Falling}

topEntity :: Clock Fast -> Bool -> Bool
topEntity _ = not
{-# ANN topEntity (Synthesize "topEntity" [PortName "clk", PortName "x"] (PortName "result")) #-}

-- A ClockN port must still be an HDL wire after the change to its Haskell representation.
negativePin :: ClockN Fast -> Bool
negativePin _ = False
{-# OPAQUE negativePin #-}
{-# ANN negativePin (InlineYamlPrimitive [Verilog]
  "BlackBox:\n  name: ClockConfiguration.negativePin\n  kind: Expression\n  template: ~ARG[0]") #-}

clockPort :: ClockN Fast -> Bool
clockPort = negativePin
{-# ANN clockPort (Synthesize "clockPort" [PortName "clkN"] (PortName "result")) #-}

mainVerilog :: IO ()
mainVerilog = do
  [root] <- getArgs
  sdc <- readFile (root </> show 'topEntity </> "topEntity.sdc")
  unless ("-period 5.000500 -waveform {0.000 2.500250}" `isInfixOf` sdc) $
    fail "Clock constraint lost its fractional period"
  hdl <- readFile (root </> show 'clockPort </> "clockPort.v")
  unless ("assign result = clkN;" `isInfixOf` hdl) (fail "Missing negative clock wire")
