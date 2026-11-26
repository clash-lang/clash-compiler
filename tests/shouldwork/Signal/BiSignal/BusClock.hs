module BusClock where

import Clash.Annotations.Primitive (HDL(Verilog), Primitive(InlineYamlPrimitive))
import qualified Clash.Driver.Manifest as Manifest
import Clash.Explicit.Prelude
import Control.Monad (unless)
import Data.List (isInfixOf)
import qualified Prelude as P
import System.Environment (getArgs)
import System.FilePath ((</>))

createClockDomain vSystem{vName="Bus", vPeriod=250000}
data Unclocked

-- The buffer exposes the external pins to the receiving register. Its HDL
-- has no Bus clock or dictionary, so only the inout port supplies its period.
inputBuffer :: BiSignalIn 'Floating Bus 8 -> Signal System (BitVector 8)
inputBuffer bus = unsafeSynchronizer (clockGen @Bus) systemClockGen (readFromBiSignal bus)
{-# OPAQUE inputBuffer #-}
{-# ANN inputBuffer (InlineYamlPrimitive [Verilog]
  "BlackBox:\n  name: BusClock.inputBuffer\n  kind: Expression\n  template: ~ARG[0]") #-}

topEntity
  :: BiSignalIn 'Floating Bus 8
  -> BiSignalIn 'Floating Unclocked 1
  -> Signal System (BitVector 8)
topEntity bus _ = delay systemClockGen enableGen 0 (inputBuffer bus)
{-# ANN topEntity (Synthesize "busCapture" [PortName "bus", PortName "spare"] (PortName "result")) #-}

mainVerilog :: IO ()
mainVerilog = do
  [root] <- getArgs
  let dir = root </> show 'topEntity
  hdl <- readFile (dir </> "busCapture.v")
  unless ("inout wire [7:0] bus" `isInfixOf` hdl) (fail "Missing bidirectional bus port")
  Just manifest <- Manifest.readManifest (dir </> Manifest.manifestFilename)
  unless (P.any ((== 250000) . vPeriod) (Manifest.domains manifest)) $
    fail "Missing clock configuration for the bidirectional bus"
