{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module PortNames where

import qualified Prelude as P

import Clash.Prelude
import Clash.Annotations.TH

type Pair a = ("left" ::: a, "right" ::: a)

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
data OneSide
  = OneSide
  { name5 :: "embedded1" ::: Simple
  , name6 :: Bool
  }
newtype Single = Single ("s" ::: Int)

data Gadt x where
  Gadt :: ("gadt" ::: Int) -> Gadt Int

type family CF x y z where
  CF Int Int Int = ("cfiii" ::: Single)
  CF Bool Int Int = ("cfbii" ::: Single)
type family OF x y z
type instance OF Int Int Int = ("ofiii" ::: Single)
type instance OF Bool Int Int = ("ofbii" ::: Single)

data family X x y z
data instance X Int Int Int = X1 ("xiii" ::: Int) ("xiii2" ::: Bool)
newtype instance X Bool Int Int = X2 ("xbii" ::: Int)

data Impossible = L ("left" ::: Int) | R ("right" ::: Bool)

data FailureTy1 = TwoF1 ("one" ::: Int) Int
data SuccessTy = TwoS ("one" ::: Int) Single

data Passthrough a b = Passthrough a b

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

topEntity3 :: "clk" ::: Clock System
           -> "rst" ::: Reset System
           -> "en"  ::: Enable System
           -> "tup1" ::: Signal System (Int, Bool)
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
    [ PortName "clk"
    , PortName "rst"
    , PortName "en"
    , PortName "tup1"
    , PortName "tup2"
    , PortProduct "tup3" [PortName "int",PortName "bool"]
    , PortProduct "tup4" [PortName "int",PortName "bool"]
    , PortProduct "custom" [PortName "named1",PortName "named2"]
    ]
    (PortProduct "outTup" [PortName "outint",PortName "outbool"])

topEntity4 :: Signal System (Gadt Int)
           -> Signal System Single
           -> Signal System (CF Int Int Int)
           -> Signal System (CF Bool Int Int)
           -> Signal System (OF Int Int Int)
           -> Signal System (OF Bool Int Int)
           -> Signal System (X Int Int Int)
           -> Signal System (X Bool Int Int)
           -> Signal System Single
topEntity4 = undefined
makeTopEntity 'topEntity4

expectedTopEntity4 :: TopEntity
expectedTopEntity4 =
  Synthesize "topEntity4"
    [ PortName "gadt"
    , PortName "s"
    , PortProduct "cfiii" [PortName "s"]
    , PortProduct "cfbii" [PortName "s"]
    , PortProduct "ofiii" [PortName "s"]
    , PortProduct "ofbii" [PortName "s"]
    , PortProduct "" [PortName "xiii", PortName "xiii2"]
    , PortName "xbii"
    ]
    (PortName "s")

topEntity5 :: "in1" ::: Signal System SuccessTy
           -> "ab"     ::: Signal System (Passthrough (Passthrough Simple Simple) Simple)
           -> "out" ::: Signal System Int
topEntity5 = undefined
makeTopEntity 'topEntity5

expectedTopEntity5 :: TopEntity
expectedTopEntity5 =
 Synthesize "topEntity5"
    [ PortProduct "in1" [PortName "one", PortName "s"]
    , PortProduct "ab" [PortProduct "" [PortProduct "" [PortName "simple1",PortName "simple2"]
                                       ,PortProduct "" [PortName "simple1",PortName "simple2"]]
                       ,PortProduct "" [PortName "simple1",PortName "simple2"]]
    ]
    (PortName "out")

expectedTopEntity6 :: TopEntity
expectedTopEntity6 =
 Synthesize "topEntity6"
    [ PortProduct "" [ PortName "clk", PortName "rst", PortName "en"]
    , PortProduct "in1" [PortName "one", PortName "s"]]
    (PortName "out")


topEntity7 :: (HiddenClockResetEnable System)
           => "in1" ::: Signal System (Vec 3 Int)
           -> "in2" ::: Signal System (Vec 3 Simple)
           -> "passthrough" ::: Signal System (Passthrough Single Single)
           -> "out" ::: Signal System Int
topEntity7 = undefined
makeTopEntity 'topEntity7

expectedTopEntity7 :: TopEntity
expectedTopEntity7 =
 Synthesize "topEntity7"
    [ PortProduct "" [PortName "clk", PortName "rst", PortName "en"]
    , PortName "in1"
    , PortName "in2"
    , PortProduct "passthrough" [PortName "s", PortName "s"]
    ]
    (PortName "out")

topEntity8 :: (HiddenClockResetEnable System)
           => "pair" ::: Signal System (Pair Bool)
           -> "pair" ::: Signal System (Pair Single)
           -> "out" ::: Signal System Int
topEntity8 = undefined
makeTopEntity 'topEntity8

expectedTopEntity8 :: TopEntity
expectedTopEntity8 =
 Synthesize "topEntity8"
    [ PortProduct "" [PortName "clk", PortName "rst", PortName "en"]
    , PortProduct "pair" [PortName "left", PortName "right"]
    , PortProduct "pair" [PortProduct "left" [PortName "s"], PortProduct "right" [PortName "s"]]
    ]
    (PortName "out")
