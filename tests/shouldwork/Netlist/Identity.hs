{-# LANGUAGE QuasiQuotes #-}
module Identity where

import Prelude as P

import Clash.Prelude
import Clash.Netlist.Types
import qualified Clash.Util.Interpolate as I
import qualified Clash.Netlist.Id as Id

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

topEntity :: Signal System Int -> Signal System Int
topEntity = id

testPath :: FilePath
testPath = "tests/shouldwork/Netlist/Identity.hs"

assertAssignsInOut :: Component -> IO ()
assertAssignsInOut (Component _ [i] [o] ds) =
  case ds of
    [Assignment oName _ (Identifier iName Nothing)]
      | Id.toText iName == Id.toText (fst i)
      , Id.toText oName == Id.toText ((\(_,(n,_),_) -> n) o)
      -> return ()
      | otherwise -> P.error [I.i|
          Incorrect input/output names:

           oName: #{oName}

           o: #{o}

           iName: #{iName}

           i: #{i}
        |]

    _ -> P.error "Identity circuit performs more than just one assignment"

assertAssignsInOut _ = error "Unexpected number of inputs and outputs"

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertAssignsInOut . snd) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ (assertAssignsInOut . snd) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog id testPath
  mapM_ (assertAssignsInOut . snd) netlist
