{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2021-2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utility functions used by the normalisation transformations. Those on
  terms are in "Clash.Normalize.Scoped.Util".
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Normalize.Util
 ( isConstantArg
 )
 where

import qualified Control.Lens            as Lens
import qualified Data.HashMap.Strict     as HashMapS
import qualified Data.Map                as Map
import           Data.Text               (Text)

import           Clash.Annotations.Primitive (extractPrim)
import           Clash.Normalize.Types
import           Clash.Primitives.Util   (constantArgs)
import           Clash.Rewrite.Types     (RewriteMonad, extra, primitives)

-- | Determine if argument should reduce to a constant given a primitive and
-- an argument number. Caches results.
isConstantArg
  :: Text
  -- ^ Primitive name
  -> Int
  -- ^ Argument number
  -> RewriteMonad NormalizeState Bool
  -- ^ Yields @DontCare@ for if given primitive name is not found, if the
  -- argument does not exist, or if the argument was not mentioned by the
  -- blackbox.
isConstantArg "Clash.Explicit.SimIO.mealyIO" i = pure (i == 2 || i == 3)
isConstantArg nm i = do
  argMap <- Lens.use (extra.primitiveArgs)
  case Map.lookup nm argMap of
    Nothing -> do
      -- Constant args not yet calculated, or primitive does not exist
      prims <- Lens.view primitives
      case extractPrim =<< HashMapS.lookup nm prims of
        Nothing ->
          -- Primitive does not exist:
          pure False
        Just p -> do
          -- Calculate constant arguments:
          let m = constantArgs nm p
          (extra.primitiveArgs) Lens.%= Map.insert nm m
          pure (i `elem` m)
    Just m ->
      -- Cached version found
      pure (i `elem` m)
