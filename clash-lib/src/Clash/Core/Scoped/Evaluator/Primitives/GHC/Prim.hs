{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Prim" in the scoped evaluator: ports
  of @Clash.GHC.Evaluator.Primitives.GHC.Prim@. See
  "Clash.Core.Scoped.Evaluator.Primitives".
-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Prim
  ( primitives
  ) where

import Data.Text (Text)
import Data.Text.Extra (showt)

import qualified GHC.Prim

import Clash.Core.Scoped.Evaluator (reduce)
import Clash.Core.Scoped.Evaluator.Primitives.Util
  (boolToIntLiteral, intLiteral, integerToIntLiteral, pairOf)
import Clash.Core.Scoped.Evaluator.Types

primitives :: [(Text, PrimReduction)]
primitives =
  [ (showt '(GHC.Prim.+#), intOp (+))
  , (showt '(GHC.Prim.-#), intOp (-))
  , (showt '(GHC.Prim.*#), intOp (*))
  , ( showt '(GHC.Prim.==#)
    , PrimReduction $ \pc -> case pairOf intLiteral (pcArgs pc) of
        Just (i, j) -> reduce pc (boolToIntLiteral (i == j))
        Nothing -> Nothing )
  ]
 where
  intOp f = PrimReduction $ \pc -> case pairOf intLiteral (pcArgs pc) of
    Just (i, j) -> reduce pc (integerToIntLiteral (f i j))
    Nothing -> Nothing
