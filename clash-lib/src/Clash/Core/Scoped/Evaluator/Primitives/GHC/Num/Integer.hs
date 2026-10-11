{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Num.Integer" in the scoped evaluator:
  ports of @Clash.GHC.Evaluator.Primitives.GHC.Num.Integer@. See
  "Clash.Core.Scoped.Evaluator.Primitives".
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Num.Integer
  ( primitives
  ) where

import Data.Text (Text)
import Data.Text.Extra (showt)

import Clash.Core.Literal (Literal (..))

import qualified GHC.Num.Integer

import Clash.Core.Scoped.Evaluator (integerLiteral, reduce)
import Clash.Core.Scoped.Evaluator.Primitives.Util (boolToBoolLiteral, pairOf)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.HasType (primInfoType)
import Clash.Core.Scoped.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ (showt 'GHC.Num.Integer.integerAdd, integerOp (+))
  , (showt 'GHC.Num.Integer.integerSub, integerOp (-))
  , (showt 'GHC.Num.Integer.integerMul, integerOp (*))
  , ( showt 'GHC.Num.Integer.integerEq
    , PrimReduction $ \pc -> case pairOf integerLiteral (pcArgs pc) of
        Just (i, j) ->
          reduce pc (boolToBoolLiteral (envTypeEnv (pcEnv pc))
                       (primInfoType (mScope (pcMachine pc)) (pcInfo pc)) (i == j))
        Nothing -> Nothing )
  ]
 where
  integerOp f = PrimReduction $ \pc -> case pairOf integerLiteral (pcArgs pc) of
    Just (i, j) -> reduce pc (Literal (IntegerLiteral (f i j)))
    Nothing -> Nothing
