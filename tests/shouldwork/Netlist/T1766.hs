module T1766 where

import qualified Prelude

import Data.List (nub)
import Data.Maybe (mapMaybe)

import Clash.Class.Counter
import Clash.Explicit.Prelude

import Clash.Backend
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

brightness :: Unsigned 3 -> Unsigned 3 -> Unsigned 4
brightness epoch led =
  maxBound - satMul SatBound distanceToEpoch 4
 where
  distanceToEpoch = distance (extend epoch) (extend led)
  distance a b = if a > b then a - b else b - a

knightrider
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (BitVector 8)
knightrider clk rst ena = pack <$> leds
 where
  leds = mealy clk rst ena modulateVec 0 (slope <$> ledPos)
  modulateVec counter thresholds = (counter + 1, map (counter <) thresholds)
  slope epoch = map (brightness epoch) (iterateI (+1) 0)

  initial :: (Unsigned 3, Index 10)
  initial = (0, 0)

  (ledPos, delayCount) =
      unbundle
    $ register clk rst ena initial
    $ fmap countSucc
    $ bundle (ledPos, delayCount)

topEntity :: Clock System -> Signal System Bool -> Signal System (BitVector 8)
topEntity clk rstBool = knightrider clk rst enableGen
 where
  rst = unsafeFromLowPolarity rstBool

testPath :: FilePath
testPath = "tests/shouldwork/Netlist/T1766.hs"

assertNoDuplicateSignals :: Component -> IO ()
assertNoDuplicateSignals (Component nm inps outs ds) =
  let names = mapMaybe netName ds
      signals = Prelude.length names
      unique = Prelude.length (nub names)
   in if signals == unique
        then pure ()
        else error ("Signals: " <> show signals <> ", unique names: " <> show unique)
 where
  netName (NetDecl _ _ i _ _) = Just i
  netName _ = Nothing

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertNoDuplicateSignals . snd) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ (assertNoDuplicateSignals . snd) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog id testPath
  mapM_ (assertNoDuplicateSignals . snd) netlist
