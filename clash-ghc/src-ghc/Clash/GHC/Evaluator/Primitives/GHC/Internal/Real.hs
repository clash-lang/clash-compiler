{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.GHC.Evaluator.Primitives.GHC.Internal.Real
  ( primitives
  ) where

import           Data.Text           (Text)

import           Clash.Core.Evaluator.Types

import Clash.GHC.Evaluator.Primitive.Util

primitives :: [(Text, PrimStep)]
primitives =
  [ ( "GHC.Internal.Real.^_$s$spowImpl2"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int# -> Integer -> Integer
            | [intLiteral -> Just j, integerLiteral -> Just i] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Internal.Real.^_$s$spowImpl"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int -> Integer -> Integer
            | [intLiteral -> Just j, integerLiteral -> Just i] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Internal.Real.$w$spowImpl"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Integer -> Int# -> Integer
            | [integerLiteral -> Just i, intLiteral -> Just j] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  , ( "GHC.Internal.Real.$w$spowImpl1"
    , \tcm isSubj pInfo tys args mach ->
        case mkPrimStepContext tcm isSubj pInfo tys args mach of
          PrimStepContext{..} -- :: Int# -> Int# -> Integer
            | [intLiteral -> Just i, intLiteral -> Just j] <- args
            -> reduce (catchErrorCall (integerToIntLiteral $ i ^ j))
          _ -> Nothing
    )
  ]
