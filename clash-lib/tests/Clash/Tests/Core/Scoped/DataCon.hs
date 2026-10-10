{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for data constructors in scoped Clash Core
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.DataCon (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.DataCon as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.DataCon
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite
  (intId, intTy, localId, mkDataCon, pairDataCon, pairTy, parseTyConTy, tyVar)
import Test.Clash.Scoped (assertScopedAlphaEq, assertScopedTyAlphaEq)

-- | @MkF :: forall a. forall b. a -> b -> F a@, with an existential type
-- variable @b@
fTyCon :: C.Type
fTyCon = parseTyConTy "F"

aTv, bTv :: C.TyVar
aTv = tyVar C.User "a" 50
bTv = tyVar C.User "b" 51

mkF :: C.DataCon
mkF = mkDataCon "MkF" 1003 [aTv] [bTv] [C.VarTy aTv, C.VarTy bTv] (C.AppTy fTyCon (C.VarTy aTv))

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.DataCon"
  [ testCase "dataConSig" $
      case dataConSig mkF of
        DataConSig (TCons ua TNil) (TCons eb TNil) [t1, t2] -> do
          -- The field types refer to the binders of the telescopes
          case (t1, t2) of
            (VarTy _ _ n1, VarTy _ _ n2) -> do
              tyNameRaw n1 @?= tyNameRaw (tyNameOf ua)
              tyNameRaw n2 @?= tyNameRaw (tyNameOf eb)
            _ -> assertFailure "unexpected field types"
        _ -> assertFailure "unexpected signature"

  , testCase "dataConInstArgTys" $ do
      let int = fromClashTypeClosed emptyScope intTy
      case dataConInstArgTys emptyScope pairDataCon [] of
        Just [t1, t2] -> do
          assertScopedTyAlphaEq intTy t1
          assertScopedTyAlphaEq intTy t2
        _ -> assertFailure "unexpected result"
      case dataConInstArgTys emptyScope mkF [int, int] of
        Just [t1, t2] -> do
          assertScopedTyAlphaEq intTy t1
          assertScopedTyAlphaEq intTy t2
        _ -> assertFailure "unexpected result"
      assertBool "too few types" (null (dataConInstArgTys emptyScope mkF [int]))
      assertBool "too many types" (null (dataConInstArgTys emptyScope mkF [int, int, int]))

    -- Instantiating @a := c@, where the free type variable @c@ has the same
    -- name as the existential @b@, must rename @b@.
  , testCase "instantiation renames existentials that would capture" $ do
      -- In the empty scope, a gets name 1 and b gets name 2
      let c = tyVar C.User "c" 2
      withConvEnv [c] [] $ \env -> do
        let r = dataConInstExtTyVars (convScope env) mkF [fromClashType env (C.VarTy c)] $
                  \_ ext fieldTys -> case (ext, fieldTys) of
                    (TCons eb TNil, [VarTy _ _ n1, VarTy _ _ n2]) -> do
                      assertBool "renamed" (tyNameRaw (tyNameOf eb) /= 2)
                      tyNameRaw n1 @?= 2
                      tyNameRaw n2 @?= tyNameRaw (tyNameOf eb)
                    _ -> assertFailure "unexpected result"
        maybe (assertFailure "dataConInstExtTyVars failed") id r

  , testCase "withDataConPat" $ do
      let s = localId C.User "s" 1 pairTy
          names = [C.mkUnsafeSystemName "a" 0, C.mkUnsafeSystemName "b" 0]
      withConvEnv [] [s] $ \env -> case fromClashTerm env (C.Var s) of
        sVar -> do
          let sc = convScope env
              r = withDataConPat sc pairDataCon [] names $ \_ p vars -> case vars of
                [va, _] -> Case sVar (fromClashTypeClosed sc intTy) [Alt p va]
                _ -> error "unexpected fields"
          case r of
            Just tm -> assertScopedAlphaEq
              (C.Case (C.Var s) intTy
                [(C.DataPat pairDataCon [] [intId C.User "a" 2, intId C.User "b" 3], C.Var (intId C.User "a" 2))])
              tm
            Nothing -> assertFailure "withDataConPat failed"
  , testCase "dataConType" $
      assertScopedTyAlphaEq (C.dcType mkF) (dataConType emptyScope mkF)
  ]
