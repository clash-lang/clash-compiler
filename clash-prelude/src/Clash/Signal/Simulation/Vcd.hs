

module Clash.Simulation.Vcd (vcdText, writeVcd) where

import Clash.Simulation


-- Zero-width signals are dropped from the VCD

-- | Create a VCD file for the given traces and simulation configuration.
vcdText ::
  Simulation ->
  Either String Text
vcdText = ...

-- | Create a VCD file for the given traces and simulation configuration,
-- and write it to a file.
-- Errors if the VCD generation fails
writeVcd ::
  FilePath ->
  Simulation ->
  IO ()
writeVcd file sim = do
  text <- assertRight $ vcdText sim
  writeFile file text
