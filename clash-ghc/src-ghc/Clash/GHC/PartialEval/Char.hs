{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Char
  ( charPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Prim
import GHC.Types hiding (Type)

import Clash.GHC.PartialEval.Internal

charPrims :: HashMap Text PrimImpl
charPrims = HashMap.fromList
  [ ("GHC.Prim.gtChar#", charComparison gtChar#)
  , ("GHC.Prim.geChar#", charComparison geChar#)
  , ("GHC.Prim.eqChar#", charComparison eqChar#)
  , ("GHC.Prim.neChar#", charComparison neChar#)
  , ("GHC.Prim.ltChar#", charComparison ltChar#)
  , ("GHC.Prim.leChar#", charComparison leChar#)
  , ("GHC.Prim.ord#", primOrd)
  , ("GHC.Types.C#", liftBox)
  ]

primOrd :: PrimImpl
primOrd = liftUnary $ \x ->
  let !(UChar (C# a)) = x in UInt (I# (ord# a))

charComparison :: (Char# -> Char# -> Int#) -> PrimImpl
charComparison f =
  liftBinary $ \x y ->
    let !(UChar (C# a)) = x
        !(UChar (C# b)) = y
     in UInt (I# (f a b))

