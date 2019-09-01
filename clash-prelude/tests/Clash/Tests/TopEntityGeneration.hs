{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Clash.Tests.TopEntityGeneration where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Prelude hiding (undefined)
import Clash.Annotations.TH

data Unnamed = Unnamed Int
data Simple = Simple ("simple1" ::: Int) ("simple2" ::: Bool)
data Named
  = Named
  { name1 :: "named1" ::: BitVector 3
  , name2 :: "named2" ::: BitVector 5
  }
data Embedded
  = Embedded
  { name3 :: "embedded1" ::: Simple
  , name4 :: "embedded2" ::: Bool
  }

topEntity1 :: "in1" ::: Signal System Int
           -> "in2" ::: Signal System Bool
           -> "out" ::: Signal System Int
topEntity1 = undefined
makeTopEntity 'topEntity1

expectedTopEntity1 :: TopEntity
expectedTopEntity1 = 
 Synthesize "topEntity1"
    [PortName "in1", PortName "in2"]
    (PortName "out")

topEntity2 :: "int"      ::: Signal System Int
           -> "tuple"    ::: ( "tup1" ::: Signal System (BitVector 7)
                             , "tup2" ::: Signal System (BitVector 9))
           -> "simple"   ::: Signal System Simple
           -> "named"    ::: Signal System Named
           -> "embedded" ::: Signal System Embedded
           -> "out"      ::: Signal System Bool
topEntity2 = undefined
makeTopEntity 'topEntity2

expectedTopEntity2 :: TopEntity
expectedTopEntity2 =
  Synthesize "topEntity2"
    [ PortName "int"
    , PortProduct "tuple" [PortName "tup1",PortName "tup2"]
    , PortProduct "simple" [PortName "simple1",PortName "simple2"]
    , PortProduct "named" [PortName "named1",PortName "named2"]
    , PortProduct "embedded"
      [ PortProduct "embedded1"
        [ PortName "simple1"
        , PortName "simple2"]
      , PortName "embedded2"]
    ]
    (PortName "out")

topEntity3 :: "tup1" ::: Signal System (Int, Bool)
           -> "tup2" ::: (Signal System Int, Signal System Bool)
           -> "tup3" ::: Signal System ("int":::Int, "bool":::Bool)
           -> "tup4" ::: ("int":::Signal System Int, "bool":::Signal System Bool)
           -> "custom" ::: Signal System Named
           -> "outTup" ::: Signal System ("outint":::Int, "outbool":::Bool)
topEntity3 = undefined
makeTopEntity 'topEntity3

expectedTopEntity3 :: TopEntity
expectedTopEntity3 =
 Synthesize "topEntity3"
    [ PortName "tup1"
    , PortName "tup2"
    , PortProduct "tup3" [PortName "int",PortName "bool"]
    , PortProduct "tup4" [PortName "int",PortName "bool"]
    , PortProduct "custom" [PortName "named1",PortName "named2"]
    ]
    (PortProduct "outTup" [PortName "outint",PortName "outbool"])

tests :: TestTree
tests =
  testGroup
    "TopEntityGeneration"
    [ testCase "topEntity1" $
      $(buildTopEntity Nothing 'topEntity1) @?= expectedTopEntity1
    , testCase "topEntity2" $
      $(buildTopEntity Nothing 'topEntity2) @?= expectedTopEntity2
    , testCase "topEntity3" $
      $(buildTopEntity Nothing 'topEntity3) @?= expectedTopEntity3
    ]

