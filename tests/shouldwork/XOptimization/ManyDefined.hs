module ManyDefined where

import qualified Data.List as List
import           Test.Tasty.Clash
import           Test.Tasty.Clash.NetlistTest

import           Clash.Prelude
import           Clash.Driver.Types
import           Clash.Netlist.Types

data MaybeIntBool
  = IntVal Int
  | BoolVal Bool
  | NotAThing

topEntity :: MaybeIntBool -> Int
topEntity m = case m of
  IntVal i  -> i
  BoolVal b -> if b then 1 else 0
  NotAThing -> errorX "NotAThing is not a thing"

assertOneAltPerDefined :: Component -> IO ()
assertOneAltPerDefined c =
  case findCondAssign (declarations c) of
    Just (CondAssignment _ _ _ _ xs)
      | List.length xs == 2 -> return ()
      | otherwise -> error $ "Expected 2 alternatives, found " <> show (List.length xs)

    _ -> error "Expected conditional assignment in netlist"
 where
  findCondAssign = List.find isCondAssign

  isCondAssign CondAssignment{} = True
  isCondAssign _                = False

testPath :: FilePath
testPath = "tests/shouldwork/XOptimization/ManyDefined.hs"

enableXOpt :: ClashOpts -> ClashOpts
enableXOpt c = c { _opt_aggressiveXOpt = True }

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL enableXOpt testPath
  mapM_ (assertOneAltPerDefined . snd) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog enableXOpt testPath
  mapM_ (assertOneAltPerDefined . snd) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog enableXOpt testPath
  mapM_ (assertOneAltPerDefined . snd) netlist
