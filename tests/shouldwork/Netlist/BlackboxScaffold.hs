{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NoDeDup where

import Prelude as P

import Clash.Magic
import Clash.Prelude
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

makeScaffold "testFunction" "testPrimitive"
  [ StringParameter "testStr" "val"
  , IntegerParameter "testInteger" (0 :: Integer)
  , BoolParameter "testBool" True
  , BitVectorParameter "testBitVector" (0 :: BitVector 8)
  ]
  [ [ ClkOut "d1clk1"
    , ClkIn  "d1clk2"
    , In     "d1i1" 1
    , Out    "d1o1" 2
    ]
  , [ ClkOut "d2clk1"
    , ClkIn  "d2clk2"
    , In     "d2i1" 1
    , In     "d2i2" 1
    , Out    "d2o1" 2
  ] ]

topEntity :: Int -> Int
topEntity n abcd = testFunction

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
  primInst = filter isPrimInst ds
  nPrimInsts = P.length primInsts

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertNumPrimInsts . getComponent) netlist
