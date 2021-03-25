{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.Singletons
  ( singletonsPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Natural (Natural)

import Clash.GHC.PartialEval.Primitive.Strategy

singletonsPrims :: HashMap Text PrimImpl
singletonsPrims = HashMap.fromList
  [ -- GHC 8.4.4, singletons-2.4.1
    ("Data.Singletons.TypeLits.Internal.$s^_f", primF)
    -- GHC 8.6.5, singletons-2.5.1
  , ("Data.Singletons.TypeLits.Internal.$fSingI->^@#@$_f", primF)
    -- GHC 8.8.1, singletons-2.6
  , ("Data.Singletons.TypeLits.Internal.%^_f", primF)
  ]

primF :: PrimImpl
primF =
  liftBinary $ \(x :: Natural) (y :: Natural) -> x ^ y
