{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.BitPack
  ( bitPackPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Clash.Class.BitPack

import Clash.GHC.PartialEval.Primitive.Strategy

bitPackPrims :: HashMap Text PrimImpl
bitPackPrims = HashMap.fromList
  [ ("Clash.Class.BitPack.packDouble#", liftUnary (pack @Double))
  , ("Clash.Class.BitPack.packFloat#", liftUnary (pack @Float))
  , ("Clash.Class.BitPack.unpackDouble#", liftUnary (unpack @Double))
  , ("Clash.Class.BitPack.unpackFloat#", liftUnary (unpack @Float))
  ]
