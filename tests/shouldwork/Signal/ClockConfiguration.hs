module ClockConfiguration where

import Clash.Explicit.Prelude
import Clash.Annotations.Primitive (Primitive(..), HDL(Verilog))
import Clash.Signal.Internal (ClockN)
import qualified Clash.Driver.Manifest as Manifest
import Control.Monad (unless)
import Data.List (isInfixOf)
import qualified Prelude as P
import System.Environment (getArgs)
import System.FilePath ((</>))

createDomain vSystem{vName="Fast", vPeriod=5000.5, vActiveEdge=Falling}

data Tagged a
instance KnownClockDomain a => KnownDomain (Tagged a) where
  type ClockDomainOf (Tagged a) = a

createDomain vSystem{vName="SignalOnly", vPeriod=27123}
data Wire
instance KnownDomain Wire where
  type ClockDomainOf Wire = SignalOnly
data Unclocked

topEntity
  :: Clock (Tagged Fast) -> Signal (Tagged Fast) Bool
  -> Signal Wire Bool -> Signal Unclocked Bool
  -> (Signal (Tagged Fast) Bool, Signal Wire Bool, Signal Unclocked Bool)
topEntity clk x wire spare = (delay clk enableGen False x, wire, spare)
{-# ANN topEntity (defSyn "topEntity") #-}

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
  let dir = root </> show 'topEntity
      check file fragment = do
        contents <- readFile file
        unless (fragment `isInfixOf` contents) $
          fail ("Missing " <> show fragment <> " in " <> file)
  check (dir </> "topEntity.v") "negedge clk"
  check (dir </> "topEntity.sdc") "-period 5.000500 -waveform {0.000 2.500250}"
  check (root </> show 'clockPort </> "clockPort.v") "assign result = clkN;"
  Just manifest <- Manifest.readManifest (dir </> Manifest.manifestFilename)
  unless (P.any ((== 27123) . vPeriod) (Manifest.domains manifest)) $
    fail "Missing configuration for the signal-only clock"
