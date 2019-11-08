module OneDefinedLitPat where

import Clash.Prelude
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

topEntity :: Unsigned 8 -> Unsigned 8
topEntity m = case m of
    0 -> 1
    _ -> errorX "Value is not 0"

assertNoMux :: Component -> IO ()
assertNoMux c =
  case go 0 $ declarations c of
    0 -> return ()
    n -> error $ "Expected 0 multiplexers, found " <> show n
 where
  go acc []                    = acc
  go acc (CondAssignment{}:ds) = go (acc + 1) ds
  go acc (_:ds)                = go acc ds

testPath :: FilePath
testPath = "tests/shouldwork/XOptimization/OneDefinedLitPat.hs"

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL [] testPath
  mapM_ (assertNoMux . getComponent) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog [] testPath
  mapM_ (assertNoMux . getComponent) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog [] testPath
  mapM_ (assertNoMux . getComponent) netlist

