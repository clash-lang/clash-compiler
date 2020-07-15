{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Natural
  ( naturalPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Natural
import GHC.Types

import Clash.Core.Evaluator.Models
import Clash.Core.Util (undefinedTm)

import Clash.GHC.PartialEval.Internal

naturalPrims :: HashMap Text PrimImpl
naturalPrims = HashMap.fromList
  [ ("GHC.Natural.naturalToInteger", liftUnary naturalToInteger)
  , ("GHC.Natural.naturalFromInteger", liftUnary naturalFromInteger)
  , ("GHC.Natural.plusNatural", liftBinary plusNatural)
  , ("GHC.Natural.timesNatural", liftBinary timesNatural)
  , ("GHC.Natural.minusNatural", primMinusNatural)
  , ("GHC.Natural.wordToNatural#", primWordToNatural)
  , ("GHC.Natural.gcdNatural", liftBinary gcdNatural)
--, ("GHC.Natural.$wshiftLNatural", _)
  , ("GHC.Natural.NatS#", primNatS)
  ]

primMinusNatural :: PrimImpl
primMinusNatural e p args
  | [Left x, Left y] <- args
  = do a <- fromTermOrValue e x
       b <- fromTermOrValue e y
       resTy <- resultType p args

       if a >= b
         then toValue (minusNatural a b) resTy
         else lift $ evaluateWhnf e (undefinedTm resTy)

  | otherwise
  = empty

primWordToNatural :: PrimImpl
primWordToNatural =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in wordToNatural# a

primNatS :: PrimImpl
primNatS =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in NatS# a

