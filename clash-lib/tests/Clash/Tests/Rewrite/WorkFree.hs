{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.WorkFree"
-}

module Clash.Tests.Rewrite.WorkFree (tests) where

import Control.Exception (evaluate)
import Control.Monad.Trans.State.Strict (evalState)
import System.Timeout (timeout)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Term (..), mkApps)
import Clash.Core.Type (Type)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (emptyVarEnv)
import Clash.Driver.Types (BindingMap)
import Clash.Rewrite.WorkFree (isWorkFree)

import Test.Clash.Rewrite
  (globalId, intLit, intTy, mkBindingMap, mkTestDataCon, parseTyConTy)

-- | @Strm@, a type with a single constructor 'consDataCon'
streamTy :: Type
streamTy = parseTyConTy "Strm"

-- | @Cons :: Int -> Strm -> Strm@
consDataCon :: DataCon
consDataCon = mkTestDataCon "Cons" 102 [] [] [intTy, streamTy] streamTy

-- | @xs = Cons 1 ys@ and @ys = Cons 2 xs@
xs, ys :: Id
xs = globalId User "xs" 10 streamTy
ys = globalId User "ys" 11 streamTy

mutuallyRecursive :: BindingMap
mutuallyRecursive = mkBindingMap
  [ (xs, mkApps (Data consDataCon) [Left (intLit 1), Left (Var ys)])
  , (ys, mkApps (Data consDataCon) [Left (intLit 2), Left (Var xs)])
  ]

-- | 'isWorkFree' analyses the body of a global binder it encounters, and only
-- checked whether that binder refers to itself directly. For mutually
-- recursive binders, such as
--
-- > xs = Cons 1 ys
-- > ys = Cons 2 xs
--
-- analysing @xs@ analysed @ys@, which analysed @xs@ again, and so on: the
-- result is only cached once the analysis of a binder finishes. A recursive
-- binder is not work free, like a self-recursive one.
workFreeMutualRecursion :: Assertion
workFreeMutualRecursion = do
  result <- timeout 2000000 $ evaluate $
    evalState (isWorkFree id mutuallyRecursive (Var xs)) emptyVarEnv
  assertEqual "isWorkFree of a mutually recursive binder" (Just False) result

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.WorkFree"
  [ testCase "isWorkFree terminates on mutually recursive binders"
      workFreeMutualRecursion
  ]
