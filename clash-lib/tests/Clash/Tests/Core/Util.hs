{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Util"
-}

module Clash.Tests.Core.Util (tests) where

import Control.Monad.Trans.State.Lazy (evalState)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (Name (nameUniq), NameSort (User))
import Clash.Core.Term (Pat (..), Term (..))
import Clash.Core.TyCon (AlgTyConRhs (..), TyCon (..), TyConMap)
import Clash.Core.TysPrim (liftedTypeKind)
import Clash.Core.Util (mkSelectorCase)
import Clash.Unique (Unique)

import qualified Clash.Data.UniqMap as UniqMap

import Test.Clash.Rewrite
  ( assertAlphaEq, assertNoShadowing, inScopeOf, intId, intTy, intTyCon
  , localId, pairDataCon, pairTy, parseTyConName )

-- | A 'TyConMap' with @Int@ and @Pair@, see 'pairDataCon'
pairTcm :: TyConMap
pairTcm = UniqMap.fromList [(nameUniq intNm, intTyCon), (nameUniq pairNm, pairTc)]
 where
  intNm = parseTyConName "Int"
  pairNm = parseTyConName "Pair"
  pairTc = AlgTyCon
    { tyConUniq = nameUniq pairNm
    , tyConName = pairNm
    , tyConKind = liftedTypeKind
    , tyConArity = 0
    , algTcRhs = DataTyCon [pairDataCon]
    , isClassTc = False
    }

-- | 'mkSelectorCase' for the second field of a @Pair@, while the first two
-- uniques it gets, 50 and 51, are in scope. It makes a binder for every field,
-- and one for the selected field. It used to make all of them fresh with
-- respect to the same in-scope set: unique 50 and 51 are both in scope, so
-- both binders for the fields got unique 52, derived from them. The binder for
-- the selected field then got unique 52, from the supply:
--
-- @
-- case p of MkPair wild sel -> sel
-- @
--
-- with @wild@ and @sel@ sharing a unique, after which
-- 'Clash.Normalize.Transformations.caseCon' selected the first field.
mkSelectorCaseFresh :: Assertion
mkSelectorCaseFresh = do
  assertNoShadowing result
  assertAlphaEq expected result
 where
  p = localId User "p" 1 pairTy
  v50 = intId User "v" 50
  v51 = intId User "v" 51
  result =
    evalState
      (mkSelectorCase "mkSelectorCaseFresh" (inScopeOf [p, v50, v51]) pairTcm (Var p) 1 1)
      (50 :: Unique)
  expected =
    Case (Var p) intTy
      [(DataPat pairDataCon [] [wild, sel], Var sel)]
  wild = intId User "wild" 2
  sel = intId User "sel" 3

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Util"
  [ testCase "mkSelectorCase binds every field to a different unique"
      mkSelectorCaseFresh
  ]
