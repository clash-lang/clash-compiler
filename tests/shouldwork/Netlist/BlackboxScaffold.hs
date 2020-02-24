{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module BlackboxScaffold where

import Prelude as P

import Clash.Magic
import Clash.Prelude
import Clash.Netlist.Types hiding (pattern Clock)

import Clash.Primitives.Scaffold

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

makeScaffold "testFunction" "testPrimitive"
  [ StringParameter "testStr" "val"
  , IntegerParameter "testInteger" (0 :: Integer)
  , BoolParameter "testBool" True
  , BitVectorParameter "testBitVector" (0 :: BitVector 8)
  ]
  [ [ Clock "d1clk1" Out
    , Clock "d1clk2" In
    , Port  "d1i1" 1 In
    , Port  "d1o1" 2 Out
    ]
  , [ Clock "d2clk1" Out
    , Clock "d2clk2" In
    , Port  "d2i1" 1 In
    , Port  "d2i2" 1 In
    , Port  "d2o1" 2 Out
  ] ]

topEntity :: BitVector 1 -> BitVector 1
topEntity !n = _d2i2 o
  where
    i = (testPrimitiveI @System @System clockGen clockGen) { _d1i1 = n }
    o = testFunction i

testPath :: FilePath
testPath = "tests/shouldwork/Netlist/BlackboxScaffold.hs"

isPrimInst (InstDecl Entity Nothing "testPrimitive" _ _ _) = True
isPrimInst _ = False

assertNumPrimInsts :: Component -> IO ()
assertNumPrimInsts (Component nm inps outs ds) =
  case nm of
    "testPrimitive" -> pure ()
    "topentity"
        | nPrimInsts == 1 -> pure ()
        | otherwise ->
            error ( "Found " <> show nPrimInsts <> " instances of testPrimitive in topEntity. "
                 <> "Expected 1.")
    _ -> error ("Unexpected component: " <> show nm)
 where
  primInsts = filter isPrimInst ds
  nPrimInsts = P.length primInsts

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertNumPrimInsts . getComponent) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ (assertNumPrimInsts . getComponent) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog id testPath
  mapM_ (assertNumPrimInsts . getComponent) netlist

