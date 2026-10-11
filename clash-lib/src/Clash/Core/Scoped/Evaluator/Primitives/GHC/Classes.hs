{-|
  Copyright   :  (C) 2013-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2022, Google Inc.,
                     2017-2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Reductions of the primitives of "GHC.Classes" in the scoped evaluator:
  ports of @Clash.GHC.Evaluator.Primitives.GHC.Classes@. See
  "Clash.Core.Scoped.Evaluator.Primitives".
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Core.Scoped.Evaluator.Primitives.GHC.Classes
  ( primitives
  ) where

import Data.Text (Text)
import Data.Text.Extra (showt)

import qualified GHC.Classes

import Clash.Core.Scoped.Evaluator (reduceIn, whnf)
import Clash.Core.Scoped.Evaluator.Primitives.Util (boolToBoolLiteral, isFalseDC, isTrueDC)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.HasType (primInfoType)
import Clash.Core.Scoped.Syntax

primitives :: [(Text, PrimReduction)]
primitives =
  [ (showt '(GHC.Classes.&&), PrimReduction (andOr True))
  , (showt '(GHC.Classes.||), PrimReduction (andOr False))
  ]

-- | @(&&)@ and @(||)@, lazy in both arguments: evaluates both with a nested
-- 'whnf'. The nested machines live in extensions of the primitive's scope, so
-- the result continues in the last one, with the original stack sunk into it.
-- See @Clash.GHC.Evaluator.Primitives.GHC.Classes@.
andOr :: Distinct n => Bool -> PrimContext o n -> Maybe (Next o n)
andOr isAnd pc
  | [lArg, rArg] <- pcArgs pc
  = whnf env True m {mStack = [], mTerm = valToTerm lArg} $ \m1 ->
      whnf env True m1 {mStack = [], mTerm = sink (valToTerm rArg)} $ \m2 ->
        let lW = sink (mTerm m1)
            rW = mTerm m2
            m2' = m2 {mStack = sink1 (mStack m)}
            lit = boolToBoolLiteral (envTypeEnv env) (primInfoType (mScope m) (pcInfo pc))
            -- The constructor that decides the result on its own
            absorbing con = if isAnd then isFalseDC con else isTrueDC con
            combine l r = if isAnd then l && r else l || r
        in case (lW, rW) of
             (Data lCon, Data rCon) ->
               Just (Next m2' {mTerm = lit (combine (isTrueDC lCon) (isTrueDC rCon))})
             (Data lCon, _)
               | absorbing lCon -> reduceIn pc m2' (lit (not isAnd))
               | otherwise -> reduceIn pc m2' rW
             (_, Data rCon)
               | absorbing rCon -> reduceIn pc m2' (lit (not isAnd))
               | otherwise -> reduceIn pc m2' lW
             _ -> Nothing
  | otherwise = Nothing
 where
  m = pcMachine pc
  env = pcEnv pc

