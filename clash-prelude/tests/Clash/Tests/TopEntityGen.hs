{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Clash.Tests.TopEntityGen where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.TH

import Clash.Prelude hiding (undefined, Exp)
import Clash.Annotations.TopEntityGen

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
--   ======>
--     {-# ANN topEntity1 ((Synthesize "topEntity1")
--                           [PortName "in1", PortName "in2"])
--                          (PortName "out") #-}
topEntity1Splice :: String
topEntity1Splice = "[PragmaD (AnnP (ValueAnnotation Clash.Tests.TopEntityGen.topEntity1) (AppE (AppE (AppE (ConE Clash.Annotations.TopEntity.Synthesize) (ListE [LitE (CharL 't'),LitE (CharL 'o'),LitE (CharL 'p'),LitE (CharL 'E'),LitE (CharL 'n'),LitE (CharL 't'),LitE (CharL 'i'),LitE (CharL 't'),LitE (CharL 'y'),LitE (CharL '1')])) (ListE [AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 'i'),LitE (CharL 'n'),LitE (CharL '1')]),AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 'i'),LitE (CharL 'n'),LitE (CharL '2')])])) (AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 'o'),LitE (CharL 'u'),LitE (CharL 't')]))))]"

topEntity2 :: "int"      ::: Signal System Int
           -> "tuple"    ::: ("tup1" ::: Signal System (BitVector 7), "tup2" ::: Signal System (BitVector 9))
           -> "simple"   ::: Signal System Simple
           -> "named"    ::: Signal System Named
           -> "embedded" ::: Signal System Embedded
           -> "out"      ::: Signal System Bool
topEntity2 = undefined
makeTopEntity 'topEntity2
--   ======>
--     {-# ANN topEntity2 ((Synthesize "topEntity2")
--                           [PortName "int",
--                            (PortProduct "tuple") [PortName "tup1", PortName "tup2"],
--                            (PortProduct "simple") [PortName "simple1", PortName "simple2"],
--                            (PortProduct "named") [PortName "named1", PortName "named2"],
--                            (PortProduct "embedded")
--                              [(PortProduct "embedded1")
--                                 [PortName "simple1", PortName "simple2"],
--                               PortName "embedded2"]])
--                          (PortName "out") #-}
topEntity2Splice :: String
topEntity2Splice = "[PragmaD (AnnP (ValueAnnotation Clash.Tests.TopEntityGen.topEntity2) (AppE (AppE (AppE (ConE Clash.Annotations.TopEntity.Synthesize) (ListE [LitE (CharL 't'),LitE (CharL 'o'),LitE (CharL 'p'),LitE (CharL 'E'),LitE (CharL 'n'),LitE (CharL 't'),LitE (CharL 'i'),LitE (CharL 't'),LitE (CharL 'y'),LitE (CharL '2')])) (ListE [AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 'i'),LitE (CharL 'n'),LitE (CharL 't')]),AppE (AppE (ConE Clash.Annotations.TopEntity.PortProduct) (ListE [LitE (CharL 't'),LitE (CharL 'u'),LitE (CharL 'p'),LitE (CharL 'l'),LitE (CharL 'e')])) (ListE [AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 't'),LitE (CharL 'u'),LitE (CharL 'p'),LitE (CharL '1')]),AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 't'),LitE (CharL 'u'),LitE (CharL 'p'),LitE (CharL '2')])]),AppE (AppE (ConE Clash.Annotations.TopEntity.PortProduct) (ListE [LitE (CharL 's'),LitE (CharL 'i'),LitE (CharL 'm'),LitE (CharL 'p'),LitE (CharL 'l'),LitE (CharL 'e')])) (ListE [AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 's'),LitE (CharL 'i'),LitE (CharL 'm'),LitE (CharL 'p'),LitE (CharL 'l'),LitE (CharL 'e'),LitE (CharL '1')]),AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 's'),LitE (CharL 'i'),LitE (CharL 'm'),LitE (CharL 'p'),LitE (CharL 'l'),LitE (CharL 'e'),LitE (CharL '2')])]),AppE (AppE (ConE Clash.Annotations.TopEntity.PortProduct) (ListE [LitE (CharL 'n'),LitE (CharL 'a'),LitE (CharL 'm'),LitE (CharL 'e'),LitE (CharL 'd')])) (ListE [AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 'n'),LitE (CharL 'a'),LitE (CharL 'm'),LitE (CharL 'e'),LitE (CharL 'd'),LitE (CharL '1')]),AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 'n'),LitE (CharL 'a'),LitE (CharL 'm'),LitE (CharL 'e'),LitE (CharL 'd'),LitE (CharL '2')])]),AppE (AppE (ConE Clash.Annotations.TopEntity.PortProduct) (ListE [LitE (CharL 'e'),LitE (CharL 'm'),LitE (CharL 'b'),LitE (CharL 'e'),LitE (CharL 'd'),LitE (CharL 'd'),LitE (CharL 'e'),LitE (CharL 'd')])) (ListE [AppE (AppE (ConE Clash.Annotations.TopEntity.PortProduct) (ListE [LitE (CharL 'e'),LitE (CharL 'm'),LitE (CharL 'b'),LitE (CharL 'e'),LitE (CharL 'd'),LitE (CharL 'd'),LitE (CharL 'e'),LitE (CharL 'd'),LitE (CharL '1')])) (ListE [AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 's'),LitE (CharL 'i'),LitE (CharL 'm'),LitE (CharL 'p'),LitE (CharL 'l'),LitE (CharL 'e'),LitE (CharL '1')]),AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 's'),LitE (CharL 'i'),LitE (CharL 'm'),LitE (CharL 'p'),LitE (CharL 'l'),LitE (CharL 'e'),LitE (CharL '2')])]),AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 'e'),LitE (CharL 'm'),LitE (CharL 'b'),LitE (CharL 'e'),LitE (CharL 'd'),LitE (CharL 'd'),LitE (CharL 'e'),LitE (CharL 'd'),LitE (CharL '2')])])])) (AppE (ConE Clash.Annotations.TopEntity.PortName) (ListE [LitE (CharL 'o'),LitE (CharL 'u'),LitE (CharL 't')]))))]"

tests :: TestTree
tests =
  testGroup
    "topEntityGen"
    [ testCase "topEntity1" $
      $(stringE . show =<< makeTopEntity 'topEntity1) @?= topEntity1Splice
    , testCase "topEntity2" $
      $(stringE . show =<< makeTopEntity 'topEntity2) @?= topEntity2Splice
    ]

