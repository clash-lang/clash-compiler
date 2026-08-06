{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}

module T3348 where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive))
import GHC.Magic (lazy)
import Clash.Netlist.Types (BlackBox (BBTemplate))

import qualified Clash.Netlist.BlackBox.Types as BB

class Vio (dom :: Domain) a res | a -> res where
  vioX :: a

instance Vio dom (Signal dom o) o where
  vioX = pure undefined

instance Vio dom a o => Vio dom (Signal dom i -> a) o where
  vioX !_i = vioX @dom @a @o

vioProbe ::
  forall dom a o n m.
  (KnownDomain dom, Vio dom a o) =>
  Vec n String ->
  Vec m String ->
  o ->
  Clock dom ->
  a
vioProbe inputNames outputNames initialOutputProbeValues clk =
  vioProbe# @dom @a @o inputNames outputNames initialOutputProbeValues clk
{-# OPAQUE vioProbe #-}

vioProbe# ::
  forall dom a o n m.
  (KnownDomain dom, Vio dom a o) =>
  Vec n String ->
  Vec m String ->
  o ->
  Clock dom ->
  a
vioProbe# !_inputNames !_outputNames !_initialOutputProbeValues clk =
  lazy clk `seq` vioX @dom @a @o
{-# OPAQUE vioProbe# #-}
{-# ANN vioProbe# (InlineYamlPrimitive [minBound..] $ unlines
  [ "BlackBoxHaskell:"
  , "    name: T3348.vioProbe#"
  , "    templateFunction: T3348.myTF"
  , "    workInfo: Always"
  ]) #-}

myTF :: BB.BlackBoxFunction
myTF _isD _primName _args _ty =
  pure (Right (BB.emptyBlackBoxMeta, BBTemplate [BB.Text "0"]))

topEntity ::
  Clock XilinxSystem ->
  Signal XilinxSystem Bit ->
  Signal XilinxSystem Bool ->
  Signal XilinxSystem (Vec 0 Bool)
topEntity clk i1 i2 =
  vioProbe @XilinxSystem ("a" :> "b" :> Nil) Nil Nil clk i1 i2

