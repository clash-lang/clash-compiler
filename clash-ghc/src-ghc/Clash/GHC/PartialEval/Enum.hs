{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Enum
  ( enumPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import qualified Data.List as List (find)
import Data.Text (Text)

import Clash.Core.DataCon
import Clash.Core.Term
import Clash.Core.Evaluator.Models

import Clash.GHC.PartialEval.Internal

enumPrims :: HashMap Text PrimImpl
enumPrims = HashMap.fromList
  [ ("GHC.Prim.tagToEnum#", primTagToEnum)
  ]

primTagToEnum :: PrimImpl
primTagToEnum e _ args
  | [Right ty, Left x] <- args
  = do env <- lift getLocalEnv
       dcs <- typeDcs ty
       UInt a <- fromTermOrValue e x

       case List.find (\dc -> dcTag dc == a + 1) dcs of
         Just dc -> pure (VThunk (Data dc) env)
         _ -> empty

  | otherwise
  = empty

